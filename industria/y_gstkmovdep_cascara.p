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


FIND tipos_movi WHERE tipos_movi.id_tipo_movimiento = piTipoMov NO-LOCK NO-ERROR.

IF NOT AVAILABLE tipos_movi THEN DO:
    RETURN "ATENCION NO EXISTE EL TIPO DE MOVIMIENTO RECIBIDO " + STRING(piTipoMov).
END.    

/*----- fin de control de consistencia -------*/
        
DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
        
        FIND FIRST lotes_cascara WHERE lotes_cascara.id_empresa           = piEmpresa    
                                   AND lotes_cascara.id_sucursal          = piSucursal   
                                   AND lotes_cascara.id_tipotambor        = piTipoTambor 
                                   AND lotes_cascara.nromov               = piNromov
                                 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE lotes_cascara  THEN DO:
          RETURN "No hay tambores en esta locacion de stock ".
        END.
        
        CREATE stock.
        ASSIGN
            stock.id_articulo        = lotes_cascara.id_articulo
            stock.fecha              = piFecha
            stock.id_tipo_movimiento = piTipoMov
            stock.id_lote            = lotes_cascara.id_lote
            stock.anio               = lotes_cascara.anio
            stock.tambor_desde       = piTamborDesde
            stock.tambor_hasta       = piTamborHasta
            stock.id_suc_origen      = piSucursalOrigen
            stock.id_suc_des         = piSucursalDestino
            stock.datos_adicionales  = tipos_movi.descripcion + string(piEmpresa) + "-" + string(piSucursal) + "-" + string(piTipoTambor)           
            stock.c_usuario          = userid("userdb")
            stock.c_fecha            = today
            stock.c_hora             = string(time,"hh:mm:ss")
            stock.id_empresa         = lotes_cascara.id_empresa
            stock.id_sucursal        = lotes_cascara.id_sucursal
            stock.id_tipotambor      = lotes_cascara.id_tipotambor
            stock.nromov             = lotes_cascara.nromov
            stock.id_serial          = next-value(serial-stock-tambores) 
            stock.signo              = if tipos_movi.codigo = "+" then "-" else "+"
            stock.id_envase          = lotes_cascara.id_envase
            stock.id_calidad         = lotes_cascara.id_calidad.                                        
        
        IF tipos_movi.baja = "" THEN
        DO:
            create bstock.
            buffer-copy stock except stock.id_serial to bstock.
            assign
                bstock.id_serial          = next-value(serial-stock-tambores)
                bstock.id_suc_des         = piSucursalOrigen
                bstock.id_suc_origen      = piSucursalDestino
                bstock.signo              = if tipos_movi.codigo = "+" 
                                            then "+"
                                            else "-".
        END.
END.

RETURN "".
