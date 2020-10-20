/****************************************************************/
/* GENERACION DE STOCK_HISTORICO_TAMBORES  MOVIMIENTOS TAMBORES */
/* DANIEL REYNA                                                 */
/****************************************************************/
define buffer stock for stock_historico_tambores.
define buffer bstock for stock_historico_tambores.


define input parameter x1 as integer.
define input parameter x2 as integer.
define input parameter x3 as integer.
define input parameter x4 as integer.
define input parameter xtipo like tipos_movi.id_tipo_movimiento.
define input parameter xfec as date.
define input parameter xserial as decimal.

define vari tid_sucursal   like tambores_industria.id_sucursal.
define vari tid_empresa    like tambores_industria.id_empresa.
define vari tnromov        like tambores_industria.nromov.
define vari tid_tipotambor like tambores_industria.id_tipotambor.


define vari xxx as character.
define vari vdesde as integer.
define vari vhasta as integer.
define vari nomarch as character.

find tipos_movi where 
    tipos_movi.id_tipo_movimiento = xtipo no-lock no-error.
if not available tipos_movi then do:
    message "ATENCION NO EXISTE EL TIPO DE MOVIMIENTO RECIBIDO " xtipo
    view-as alert-box.
    return "error".
end.    

nomarch = "{1}".
find movimientos_tambores where 
                   movimientos_tambores.id_empresa = x1 and 
                   movimientos_tambores.id_sucursal = x2 and
                   movimientos_tambores.id_tipotambor = x3 and
                   movimientos_tambores.nromov_mov = x4
                   no-lock no-error.
if available movimientos_tambores then do:
    if tipos_movimientos.codigo <> "B" and
       tipos_movimientos.baja   = "" then do:
        /******** Movimiento negativo **************/  
        run genera_movimiento (input "-",
                               input movimientos_tambores.id_suc_origen,
                               input movimientos_tambores.id_suc_des).
        /******** Movimiento positivo **************/  
        run genera_movimiento (input "+",
                               input movimientos_tambores.id_suc_des,
                               input movimientos_tambores.id_suc_origen).
    end.
    else do: 
        if tipos_movimientos.codigo = "B" then do:
            /******** Movimiento positivo **************/  
            run genera_movimiento (input "B",
                                   input movimientos_tambores.id_suc_origen,
                                   input movimientos_tambores.id_suc_des).
        end.
        else do:
            /******** Movimiento con su signo **************/  
            run genera_movimiento (input tipos_movimiento.codigo,
                                   input movimientos_tambores.id_suc_origen,
                                   input movimientos_tambores.id_suc_des).
        
        end.
    end.                                       

end.            



procedure genera_movimiento. 
define input parameter xsigno as character. 
define input parameter vsuco like movimientos_tambores.id_suc_origen.
define input parameter vsucd like movimientos_tambores.id_suc_des.
create stock.
ASSIGN
            stock.id_articulo        = movimientos_tambores.id_articulo
            stock.fecha              = xfec
            stock.id_tipo_movimiento = xtipo
            stock.id_lote            = movimientos_tambores.id_lote
            stock.anio               = movimientos_tambores.anio
            stock.tambor_desde       = movimientos_tambores.tambor_desde
            stock.tambor_hasta       = movimientos_tambores.tambor_hasta
            stock.id_suc_origen      = vsuco
            stock.id_suc_des         = vsucd
            stock.datos_adicionales  = tipos_movi.descripcion
            stock.c_usuario          = userid("userdb") 
            stock.c_fecha            = today
            stock.c_hora             = string(time,"hh:mm:ss")
            stock.id_empresa         = movimientos_tambores.id_empresa
            stock.id_sucursal        = movimientos_tambores.id_sucursal
            stock.id_tipotambor      = movimientos_tambores.id_tipotambor
            stock.nromov             = movimientos_tambores.nromov_mov
            stock.signo              = xsigno.
if xserial = 0 then stock.id_serial = next-value(serial-stock-tambores).
            stock.id_serial = xserial.

end.
