for each tambores_industria use-index nromov
    where tambores_industria.id_sucursal_ubicacion = 95 and
          tambores_industria.id_locacion_ubicacion <> 10  and
          tambores_industria.id_lote               = 311  and  
          substring(string(anio,"9999"),3,2) = string(03,"99") and                         tambores_industria.id_articulo           = 53 
      no-lock:
      find productos_terminados where 
        productos_terminados.id_articulo = tambores_industria.id_articulo
        no-lock no-error.
      if  not available productos_terminados then next.
      display 
              tambores_industria.id_tambor (count)
              tambores_industria.id_lote
              tambores_industria.id_articulo
              productos_terminados.descripcion format "x(10)"
                    when available productos_terminados
              tambores_industria.id_sucursal_ubicacion
              tambores_industria.nromov
              tambores_industria.c_fecha
              tambores_industria.c_hora
              tambores_industria.nro_remito
              with scrollable.
              
              
end.