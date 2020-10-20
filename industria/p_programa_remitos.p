define var v_lote as char.

for each remitos where year(fecha) = 2001 
                   and (id_sucursal = 95
                    or id_sucursal = 96)
                   and id_cliente <> 1.
    for each items_factura of remitos.

        v_lote = entry(1,items_factura.nro_lote,"/").

        if items_factura.id_articulo = 50 or
           items_factura.id_articulo = 51 or
           items_factura.id_articulo = 57 or
           items_factura.id_articulo = 58 then
        do:
            find lotes_aceite where lotes_aceite.id_lote = integer(v_lote) no-lock no-error.
            if available lotes_aceite then
                do:
                    for each tambores_industria of lotes_aceite.
                        assign tambores_industria.id_empresa_ubicacion = 1
                               tambores_industria.id_sucursal_ubicacion = 85
                            /* tambores_industria.id_sucursal_ubicacion = 96
                               tambores_industria.id_locacion_ubicacion = 50 */
                               tambores_industria.id_posicion_ubicacion = 1. 
                    end.
                end.
        end. 
        
        if items_factura.id_articulo = 52 or
           items_factura.id_articulo = 53 or
           items_factura.id_articulo = 71 then
        do:
            find lotes_jugo where lotes_jugo.id_lote = integer(v_lote) no-lock no-error.
            if available lotes_jugo then
                do:
                    for each tambores_industria of lotes_aceite.
                        assign tambores_industria.id_empresa_ubicacion = 1
                               tambores_industria.id_sucursal_ubicacion = 85
                            /* tambores_industria.id_sucursal_ubicacion = 96
                               tambores_industria.id_locacion_ubicacion = 50 */
                               tambores_industria.id_posicion_ubicacion = 1. 
                    end.
                end.
        end. 
        
    end.
end.
