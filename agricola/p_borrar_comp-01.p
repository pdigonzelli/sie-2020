define input parameter p_reporte as integer.

for each rb_resumen_agricola where id_reporte = p_reporte :
   delete rb_resumen_agricola.
end.

