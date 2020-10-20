/******************************************************/
/* GENERACION DE STOCK_HISTORICO_TAMBORES  DE REMITOS */
/* DANIEL REYNA                                       */
/******************************************************/
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
define vari tid_envase     like tambores_industria.id_envase.
define vari tid_calidad    like tambores_industria.id_calidad.



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
            where items_factura.id_sucursal     = x1 and
                  items_factura.id_tipo_movsto  = x2 and
                  items_factura.nro             = x3 and
                  items_factura.item            = x4 no-lock:
        find first lugar_descarga of remitos no-lock no-error.
        
        /*
        /*--- RUTINA DE CONTROL DE CONSISTENCIA DE TAMBORES ----*/
        for each tambores_industria where
           tambores_industria.id_articulo   = items_factura.id_articulo and
           tambores_industria.id_lote       = 
           integer(substring(items_factura.nro_lote,1,4)) and
           tambores_industria.anio          = 
           integer(substring(items_factura.nro_lote,6,2)) + 2000 and
           tambores_industria.id_tambor     >= items_factura.desde_lote and
           tambores_industria.id_tambor     <= items_factura.hasta_lote and
           IF items_factura.id_tipotambor <> 0 THEN tambores_industria.id_tipotambor = items_factura.id_tipotambor ELSE TRUE 
           AND 
           tambores_industria.id_locacion_ubicacion <> 10 no-lock:
           accum tambores_industria.id_tambor (count).
           /* if (remitos.mercado = 1 and
               tambores_industria.id_sucursal_ubicacion <> 85) or
              (remitos.mercado <> 1 and
               tambores_industria.id_sucursal_ubicacion <> 
                  if available lugar_descarga then lugar_descarga.id_sucursal 
                  else 1) then do:
              message 
          "Atencion el lugar de descarga difiere de la ubicacion del tambor "
             lugar_descarga.id_sucursal              
             tambores_industria.id_lote
             tambores_industria.anio
             tambores_industria.id_articulo
             tambores_industria.id_tambor
             tambores_industria.kilos_tambor
             tambores_industria.id_sucursal_ubicacion
             TAMBORES_iNDUSTRIA.ID_TIPOTAMBOR
             tambores_industria.nromov
              view-as alert-box.
              return "error".
           end.       */  
           assign
             tid_sucursal   = tambores_industria.id_sucursal 
             tid_empresa    = tambores_industria.id_empresa
             tid_tipotambor = tambores_industria.id_tipotambor
             tnromov        = tambores_industria.nromov
             tid_envase     = tambores_industria.id_envase
             tid_calidad    = tambores_industria.id_calidad.
        end. 
        
        if (accum count tambores_industria.id_tambor) <>
           (items_factura.hasta_lote - items_factura.desde_lote + 1) then do:
            message "Atencion la cantidad de tambores informada en el remito"
            (accum count tambores_industria.id_tambor) 
            skip "difiere de la cantidad existente en tambores industria"
            (items_factura.hasta_lote - items_factura.desde_lote + 1)
            items_factura.nro_lote items_factura.peso
            view-as alert-box.
            return "error".
        end.
        */
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
            stock.datos_adicionales  = tipos_movi.descripcion + 
                    string(x1) + "-" + string(x2) + "-" + string(x3)           
            stock.c_usuario          = userid("userdb")
            stock.c_fecha            = today
            stock.c_hora             = string(time,"hh:mm:ss")
            stock.id_empresa         = tid_empresa
            stock.id_sucursal        = tid_sucursal
            stock.id_tipotambor      = tid_tipotambor
            stock.nromov             = tnromov
            stock.id_serial          = next-value(serial-stock-tambores) 
            stock.signo              = if tipos_movi.codigo = "+" 
                                        then "-"
                                        else "+"
            stock.id_envase         = tid_envase
            stock.id_calidad        = tid_calidad.                                        
        create bstock.
        buffer-copy stock except stock.id_serial to bstock.
        assign
            bstock.id_serial         = next-value(serial-stock-tambores)
            bstock.id_suc_des         = items_factura.id_sucursal
            bstock.id_suc_origen      = if available lugar_descarga then
                                       lugar_descarga.id_sucursal else 1
            bstock.signo              = if tipos_movi.codigo = "+" 
                                        then "+"
                                        else "-".
end.            
