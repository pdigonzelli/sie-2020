TRIGGER PROCEDURE FOR DELETE OF item_ingreso_lote_ubicacion.

define var hprog as handle no-undo.

run libLotesUbicacion.p persistent set hprog.

run transferenciaLoteUbicacion in hprog (item_ingreso_lote_ubicacion.id_empresa ,
                                         item_ingreso_lote_ubicacion.id_sucursal ,
                                         item_ingreso_lote_ubicacion.id_tipotambor,
                                         item_ingreso_lote_ubicacion.nromov,
                                         item_ingreso_lote_ubicacion.id_sucursal_ubicacion,
                                         91,
                                         today,
                                         canti).

if return-value <> "" then 
do:
    delete procedure hprog.
    undo, leave.           
end.

delete procedure hprog.                                  

