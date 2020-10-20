TRIGGER PROCEDURE FOR WRITE OF item_ingreso_lote_ubicacion old iti.

define var hprog as handle no-undo.

find ingreso_lote_ubicacion where ingreso_lote_ubicacion.id_sucursal_ubicacion = 
     item_ingreso_lote_ubicacion.id_sucursal_ubicacion and
     ingreso_lote_ubicacion.nromov = item_ingreso_lote_ubicacion.nromov_ingreso no-lock no-error.

run libLotesUbicacion.p persistent set hprog.

if rowid(iti) <> ? and iti.cantidad <> item_ingreso_lote_ubicacion.cantidad then
do:
   run transferenciaLoteUbicacion in hprog (item_ingreso_lote_ubicacion.id_empresa ,
                                            item_ingreso_lote_ubicacion.id_sucursal ,
                                            item_ingreso_lote_ubicacion.id_tipotambor,
                                            item_ingreso_lote_ubicacion.nromov,
                                            item_ingreso_lote_ubicacion.id_sucursal_ubicacion,
                                            91,
                                            ingreso_lote_ubicacion.fecha,
                                            iti.cantidad).

end.

     
run transferenciaLoteUbicacion in hprog (item_ingreso_lote_ubicacion.id_empresa ,
                                         item_ingreso_lote_ubicacion.id_sucursal ,
                                         item_ingreso_lote_ubicacion.id_tipotambor,
                                         item_ingreso_lote_ubicacion.nromov,
                                         91,
                                         item_ingreso_lote_ubicacion.id_sucursal_ubicacion,                                         
                                         ingreso_lote_ubicacion.fecha,
                                         item_ingreso_lote_ubicacion.cantidad).
                                         
if return-value <> "" then 
do:
    delete procedure hprog.
    undo, leave.                                         
end.    
                                         
delete procedure hprog.