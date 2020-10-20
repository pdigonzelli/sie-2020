/******************************************************/
/* GENERACION DE STOCK_HISTORICO_TAMBORES  DE REMITOS */
/* DANIEL REYNA                                       */
/******************************************************/
define buffer stock for stock_historico_tambores.
define buffer bstock for stock_historico_tambores.


define input parameter piEmpresa as INTEGER NO-UNDO.
define input parameter piSucursal as INTEGER NO-UNDO.
define input parameter piTipoTambor as INTEGER NO-UNDO.
define input parameter piNromov as INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piSucursalOrigen AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piSucursalDestino AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piTamborDesde AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piTamborHasta AS INTEGER NO-UNDO.
define input parameter piTipoMov like tipos_movi.id_tipo_movimiento NO-UNDO.
DEFINE INPUT PARAMETER piFecha AS DATE NO-UNDO.

define vari tid_sucursal   like tambores_industria.id_sucursal.
define vari tid_empresa    like tambores_industria.id_empresa.
define vari tnromov        like tambores_industria.nromov.
define vari tid_tipotambor like tambores_industria.id_tipotambor.
define vari tid_envase     like tambores_industria.id_envase.
define vari tid_calidad    like tambores_industria.id_calidad.



define vari xxx as character.
define vari vdesde as integer.
define vari vhasta as integer.
define vari nomarch as character.
DEFINE VAR vi AS INTEGER NO-UNDO.
DEFINE VAR viDesde AS INTEGER NO-UNDO initial 99999999.
DEFINE VAR viHasta AS INTEGER NO-UNDO.


FIND tipos_movi WHERE tipos_movi.id_tipo_movimiento = piTipoMov NO-LOCK NO-ERROR.

IF NOT AVAILABLE tipos_movi THEN DO:
    RETURN "ATENCION NO EXISTE EL TIPO DE MOVIMIENTO RECIBIDO " + STRING(piTipoMov).
END.    

/*----- fin de control de consistencia -------*/
        
DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
        
        FOR EACH tambores_industria WHERE tambores_industria.id_empresa             = piEmpresa    
                                        AND tambores_industria.id_sucursal          = piSucursal   
                                        AND tambores_industria.id_tipotambor        = piTipoTambor 
                                        AND tambores_industria.nromov               = piNromov
                                        AND tambores_industria.id_tambor            >= piTamborDesde
                                        AND tambores_industria.id_tambor            <= piTamborHasta
                                        AND tambores_industria.id_sucursal_ubicacion = piSucursalDestino 
            BREAK BY tambores_industria.id_articulo.

            vi = vi + 1.
            
            IF tambores_industria.id_tambor <= viDesde  THEN viDesde = tambores_industria.id_tambor.
            IF tambores_industria.id_tambor >= viHasta  THEN vihasta = tambores_industria.id_tambor.

            IF LAST-OF (tambores_industria.id_articulo) THEN
            DO:
                create stock.
                assign
                    stock.id_articulo        = tambores_industria.id_articulo
                    stock.fecha              = piFecha
                    stock.id_tipo_movimiento = piTipoMov
                    stock.id_lote            = tambores_industria.id_lote
                    stock.anio               = tambores_industria.anio
                    stock.tambor_desde       = viDesde
                    stock.tambor_hasta       = viHasta
                    stock.id_suc_origen      = piSucursalOrigen
                    stock.id_suc_des         = piSucursalDestino
                    stock.datos_adicionales  = tipos_movi.descripcion + 
                            string(piEmpresa) + "-" + string(piSucursal) + "-" + string(piTipoTambor)           
                    stock.c_usuario          = userid("userdb")
                    stock.c_fecha            = today
                    stock.c_hora             = string(time,"hh:mm:ss")
                    stock.id_empresa         = tambores_industria.id_empresa
                    stock.id_sucursal        = tambores_industria.id_sucursal
                    stock.id_tipotambor      = tambores_industria.id_tipotambor
                    stock.nromov             = tambores_industria.nromov
                    stock.id_serial          = next-value(serial-stock-tambores) 
                    stock.signo              = if tipos_movi.codigo = "+" 
                                                then "-"
                                                else "+"
                    stock.id_envase         = tambores_industria.id_envase
                    stock.id_calidad        = tambores_industria.id_calidad.                                        
                
                IF tipos_movi.baja = "" THEN
                DO:
                    create bstock.
                    buffer-copy stock except stock.id_serial to bstock.
                    assign
                        bstock.id_serial         = next-value(serial-stock-tambores)
                        bstock.id_suc_des         = piSucursalOrigen
                        bstock.id_suc_origen      = piSucursalDestino
                        bstock.signo              = if tipos_movi.codigo = "+" 
                                                    then "+"
                                                    else "-".

                    vi = 0.
                    viDesde = 99999999.
                    vihasta = 0.
                END.
            END.
        END.
END.

RETURN "".
