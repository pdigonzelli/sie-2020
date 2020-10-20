TRIGGER PROCEDURE FOR REPLICATION-WRITE OF r_subd_ventas_embarque.
DEFINE BUFFER b_vtas FOR subd_vtas.
DEFINE VAR v_cantidad AS INTEGER.

if available r_subd_ventas_embarque Then
  do:
    
    /* Carga permisos de embarques */       
    FIND FIRST b_vtas OF r_subd_ventas_embarque NO-LOCK NO-ERROR.
   /*-- GENERACION DE PE y recalculo automático --*/
    RUN p_devuelve_cant-pe.p (INPUT ROWID(b_vtas), OUTPUT v_cantidad).
    IF v_cantidad > 0 THEN
       RUN p_cargar-pe.p (INPUT ROWID(b_vtas), INPUT v_cantidad).

END.


