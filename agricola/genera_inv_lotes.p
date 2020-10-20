for each lotes_plantacion no-lock:
 find first inv_lotes_plantacion where inv_lotes_plantacion.id_proveedor = lotes_plantacion.id_proveedor and
 inv_lotes_plantacion.id_origen = lotes_plantacion.id_origen and
 inv_lotes_plantacion.id_lote = lotes_plantacion.id_lote no-lock no-error.
 if not available inv_lotes_plantacion Then
    do:
    end.
end.
