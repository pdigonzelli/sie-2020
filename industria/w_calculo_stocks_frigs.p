DEFINE VAR p_lugdes as integer.
define var v_lote as integer format ">>>9".
define var v_anio_lote as integer format "9999".

p_lugdes = 113.

for each remitos no-lock where remitos.fecha >= DATE("01/05/02")
                           and remitos.fecha <= DATE("25/09/02")
                           and remitos.id_lugdes = p_lugdes 
                           and (remitos.id_sucursal = 95 or remitos.id_sucursal = 96)
                           and remitos.estado = TRUE.
                           
    for each items_factura of remitos no-lock.
        v_lote = integer(substring(items_factura.nro_lote,1,4)).
        v_anio_lote = integer(substring(items_factura.nro_lote,6,2)).
        v_anio_lote = v_anio_lote + 2000.
        
        for each tambores_industria where tambores_industria.id_tambor >= items_factura.desde_lote
                                      and tambores_industria.id_tambor <= items_factura.hasta_lote
                                      and tambores_industria.id_lote = v_lote
                                      and year(tambores_industria.fecha) = v_anio_lote
                                      and tambores_industria.id_articulo = items_factura.id_articulo.
                
                ASSIGN tambores_industria.id_sucursal_ubicacion = 87
                       tambores_industria.id_locacion_ubicacion = 4.
        end.
    end.
end.                           
