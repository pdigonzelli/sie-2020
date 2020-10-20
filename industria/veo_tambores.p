define buffer xx for tambores_industria.
for each xx where  
/*  xx.id_lote     = 521 and    
  xx.anio        = 2003 and     */
  xx.id_articulo = 511 and  
/*  id_tipotambor  = 2     and   */
  id_sucursal_ubicacion = 96 /* and
    and  id_sucursal = 95  and  
 id_locacion_ubicacion = 4        */
 by xx.id_tambor.
displ 
 xx.anio
 xx.id_lote
 xx.nromov
 xx.id_sucursal
 xx.id_empresa 
 xx.id_articulo id_sucursal_ubicacion id_locacion_ubicacion id_tambor (count)
 xx.id_tipotambor
 xx.kilos_tambor
 xx.id_calidad
 xx.id_envase
 xx.c_usuario
 xx.c_fecha
 with scrollable.
end.
wait-for f4 of current-window.
pause 0.