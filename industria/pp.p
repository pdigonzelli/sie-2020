define buffer x for stock_historico_tambores.
define buffer y for tambores_industria.

for each x where id_articulo = 43 and id_tipotambor = 1 and id_lote = 154 and anio = 2003.
    find first tambores_industria where
            tambores_industria.id_sucursal = x.id_sucursal and
            tambores_industria.id_empresa  = x.id_empresa and
            tambores_industria.id_tipotambor = x.id_tipotambor and
            tambores_industria.nromov = x.nromov
            no-lock no-error.
    if available tambores_industria and
       tambores_industria.nromov_destino <> 0 then do:
        find first y  where
            tambores_industria.id_sucursal_destino = y.id_sucursal and
            tambores_industria.id_empresa_destino  = y.id_empresa and
            tambores_industria.id_tipotambor_destino = y.id_tipotambor and
            tambores_industria.nromov_destino = y.nromov
            no-lock no-error.
       
                displa x.id_tipo_movim with scrollable.
                displa y.id_lote y.anio.
    end.
end.
wait-for f4 of current-window.