/**************************************************/
/* GENERACION DE STOCK_HISTORICO_TAMBORES         */
/* DANIEL REYNA                                   */
/**************************************************/
define buffer stock for stock_historico_tambores.
define buffer bstock for stock_historico_tambores.


define input parameter x1 as integer.
define input parameter x2 as integer.
define input parameter x3 as integer.
define input parameter x4 as integer.
define input parameter xtipo like tipos_movi.id_tipo_movimiento.

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
/* debe venir el nombre de la cabecera del lote */
case nomarch:
when "items_factura" then do:  /*---- PARA REMITOS -----*/
    find first items_factura
            where items_factura.id_sucursal = x1 and
                  items_factura.id_tipo_movsto = x2 and
                  items_factura.nro = x3
                  no-lock no-error.
    if not available items_factura then do:
        message
         "NO DEBERIA PRODUCIRSE ESTE ERROR" SKIP
                "AVISAR A SISTEMAS"
                view-as alert-box.
        return "error".                
    end.
                      
    find remitos 
            where remitos.id_sucursal = x1 and
                  remitos.id_tipo_movsto = x2 and
                  remitos.nro = x3 no-lock no-error.
    if not available remitos then do:
        message "REMITO INEXISTENTE SE DESHACE LA TRANSACCION"
        view-as alert-box.
        return "error".
    end.

    for each items_factura             
            where items_factura.id_sucursal = x1 and
                  items_factura.id_tipo_movsto = x2 and
                  items_factura.nro = x3 no-lock:
        find first lugar_descarga of remitos no-lock no-error.
        
        /*--- RUTINA DE CONTROL DE CONSISTENCIA DE TAMBORES ----*/
        for each tambores_industria where
           tambores_industria.id_articulo = items_factura.id_articulo and
           tambores_industria.id_lote = 
           integer(substring(items_factura.nro_lote,1,4)) and
           tambores_industria.anio = 
           integer(substring(items_factura.nro_lote,6,2)) + 2000 and
           tambores_industria.id_tambor >= items_factura.desde_lote and
           tambores_industria.id_tambor <= items_factura.hasta_lote no-lock:
           accum tambores_industria.id_tambor (count).
           if tambores_industria.id_sucursal_ubicacion <> 
              if available lugar_descarga then lugar_descarga.id_sucursal 
                                          else 1 then do:
              message 
          "Atencion el lugar de descarga difiere del la ubicacion del tambor "
             lugar_descarga.id_sucursal              
             tambores_industria.id_sucursal_ubicacion
              view-as alert-box.
              return "error".
           end.         
           assign
             tid_sucursal   = tambores_industria.id_sucursal 
             tid_empresa    = tambores_industria.id_empresa
             tid_tipotambor = tambores_industria.id_tipotambor
             tnromov        = tambores_industria.nromov.
        end.       
        if (accum count tambores_industria.id_tambor) <>
           (items_factura.hasta_lote - items_factura.desde_lote + 1) then do:
            message "Atencion la cantidad de tambores informada en el remito"
            (accum count tambores_industria.id_tambor) 
            skip "difiere de la cantidad existente en tambores industria"
            (items_factura.hasta_lote - items_factura.desde_lote + 1)
            view-as alert-box.
            return "error".
        end.            
        /*----- fin de control de consistencia -------*/
       
        create stock.
        assign
            stock.id_articulo        = items_factura.id_articulo
            stock.fecha              = today
            stock.id_tipo_movimiento = xtipo
            stock.id_lote    = integer(substring(items_factura.nro_lote,1,4))
            stock.anio  = integer(substring(items_factura.nro_lote,6,2)) + 2000
            stock.tambor_desde       = items_factura.desde_lote
            stock.tambor_hasta       = items_factura.hasta_lote
            stock.id_suc_origen      = items_factura.id_sucursal
            stock.id_suc_des         = if available lugar_descarga then
                                       lugar_descarga.id_sucursal else 1
            stock.datos_adicionales  = tipos_movi.descripcion            
            stock.c_usuario          = userid("userdb")
            stock.c_fecha            = today
            stock.c_hora             = string(time,"hh:mm:ss")
            stock.id_empresa         = tid_empresa
            stock.id_sucursal        = tid_sucursal
            stock.id_tipotambor      = tid_tipotambor
            stock.nromov             = tnromov
            stock.id_serial          = next-value(serial-stock-tambores) 
            stock.signo              = "-".
        create bstock.
        buffer-copy stock except stock.id_serial to bstock.
        assign
            bstock.id_serial         = next-value(serial-stock-tambores)
            stock.id_suc_des         = items_factura.id_sucursal
            stock.id_suc_origen      = if available lugar_descarga then
                                       lugar_descarga.id_sucursal else 1
            stock.signo              = "+".
    end.
end.

/* when "aux_items_venta" then do:      /* Facturacion */
    find first aux_items_venta where 
                        aux_items_venta.id_punto_venta = x1 and
                        aux_items_venta.nromov = x2
                        no-lock no-error.
end.                            */
/*
when "movimientos_tambores" then do:
   find {1} where 
                   {1}.id_empresa = x1 and 
                   {1}.id_sucursal = x2 and
                   {1}.id_tipotambor = x3 and
                   {1}.nromov_mov = x4
                   no-lock no-error.
    /******** Movimiento negativo **************/  
    run genera_movimiento (input "-",
                           input movimientos_tambores.id_suc_origen,
                           input movimientos_tambores.id_suc_des).
    /******** Movimiento positivo **************/  
    run genera_movimiento (input "+",
                           input movimientos_tambores.id_suc_des,
                           input movimientos_tambores.id_suc_origen).

            
end.
*/
otherwise do:
   find {1} where 
                   {1}.id_empresa = x1 and 
                   {1}.id_sucursal = x2 and
                   {1}.id_tipotambor = x3 and
                   {1}.nromov = x4 
                   no-lock no-error.
    vdesde = 0.
    vhasta = 0.                       
    for each tambores_industria where
            tambores_industria.id_empresa  = {1}.id_empresa and
            tambores_industria.id_sucursal = {1}.id_sucursal and
            tambores_industria.id_tipotambor = {1}.id_tipotambor and
            tambores_industria.nromov = {1}.nromov
            no-lock 
        break by tambores_industria.id_lote:
        if first-of(tambores_industria.id_lote) then 
            assign vdesde = tambores_industria.id_tambor.
        if last-of(tambores_industria.id_lote) then 
            assign vhasta = tambores_industria.id_tambor.
        accum tambores_industria.id_tambor (count).
    end.
/*    display vdesde vhasta (accum count tambores_industria.id_tambor) 
    {1}.id_articulo.
  */  
    create stock.
    ASSIGN
            stock.id_articulo        = {1}.id_articulo
            stock.fecha              = today
            stock.id_tipo_movimiento = xtipo
            stock.id_serial          = next-value(serial-stock-tambores) 
            stock.id_lote            = {1}.id_lote
            stock.anio               = {1}.anio
            stock.tambor_desde       = vdesde
            stock.tambor_hasta       = vhasta
            stock.id_suc_origen      = {1}.id_sucursal
            stock.id_suc_des         = {1}.id_sucursal
            stock.datos_adicionales  = tipos_movi.descripcion
            stock.c_usuario          = userid("userdb")
            stock.c_fecha            = today
            stock.c_hora             = string(time,"hh:mm:ss").
    
    
    

end.                                
end.



procedure genera_movimiento.
define input parameter xsigno as character. 
define input parameter vsuco like movimientos_tambores.id_suc_origen.
define input parameter vsucd like movimientos_tambores.id_suc_des.
create stock.
ASSIGN
            stock.id_articulo        = {1}.id_articulo
            stock.fecha              = today
            stock.id_tipo_movimiento = xtipo
            stock.id_lote            = {1}.id_lote
            stock.anio               = {1}.anio
            stock.tambor_desde       = movimientos_tambores.tambor_desde
            stock.tambor_hasta       = movimientos_tambores.tambor_hasta
            stock.id_suc_origen      = vsuco
            stock.id_suc_des         = vsucd
            stock.datos_adicionales  = tipos_movi.descripcion
            stock.c_usuario          = userid("userdb")
            stock.c_fecha            = today
            stock.c_hora             = string(time,"hh:mm:ss")
            stock.id_empresa         = {1}.id_empresa
            stock.id_sucursal        = {1}.id_sucursal
            stock.id_tipotambor      = {1}.id_tipotambor
            stock.nromov             = {1}.nromov_mov
            stock.id_serial          = next-value(serial-stock-tambores) 
            stock.signo              = xsigno.
end.
