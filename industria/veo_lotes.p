FOR each tambores_industria where 
    id_lote     = 0
and anio        = 2001 
and id_articulo = 51
 
by id_tambor.
    display 
    tambores_industria.id_tipotambor    tambores_industria.id_tambor nromov     id_sucursal_ubicacion 
    id_locacion_ubicacion 
    tambores_industria.c_fecha
    tambores_industria.c_hora
    nro_remito.
    find remitos where 
    remitos.id_sucursal = tambores_industria.id_sucursal_remito and
    remitos.id_tipo_movsto = tambores_industria.id_tipo_movsto and
    remitos.nro = tambores_industria.nro_remito
    no-lock no-error.
    if available remitos then do:
    display 
            remitos.id_sucursal
            remitos.id_tipo_movsto
            remitos.nro 
            fecha_proceso 
            remitos.c_hora 
            remitos.c_usuario
    with scrollable.
    end.
end.