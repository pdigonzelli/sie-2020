
    DEF VAR v_finca AS INTEGER.
for each liq_control_tareas no-lock where id_empresa = 101 and 
     /* liq_control_tareas.id_sector = 5 and    */
      liq_control_tareas.fecha >= DATE("01/08/16") 
      and liq_control_tareas.fecha <= date("15/08/16") 
      ,each liq_items_control_tareas of liq_control_tareas 
       WHERE liq_items_control_tareas.id_tarea <> 0 
        AND liq_items_control_tareas.legajo = 10520 no-lock,
       first liq_legajos where liq_legajos.id_empresa_liq =
        liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo  NO-LOCK
       by liq_items_control_tareas.id_empresa by legajo by liq_items_control_tareas.fecha:
      
        find first tareas where tareas.id_tarea = liq_items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
          v_finca = 0.

    IF liq_legajos.toma_ccosto_legajo = NO THEN
    DO:

        find first liq_ccostos where liq_ccostos.id_proveedor = liq_control_tareas.id_proveedor and
        liq_ccostos.id_origen = liq_control_tareas.id_origen AND
        liq_ccostos.id_centro_costo > 100000000  no-lock no-error.
        if available liq_ccostos Then
          v_finca = liq_ccostos.id_centro_costo_liq.
         Else
         DO:
             FIND FIRST liq_centros_costos WHERE 
                 liq_centros_costos.id_empresa = liq_legajo.id_empresa AND
                 liq_centros_costos.id_centro_costo = liq_legajos.id_centro_costo NO-LOCK NO-ERROR.
             IF AVAILABLE liq_centros_costos THEN
             DO:
                 MESSAGE liq_legajo.id_centro_costo VIEW-AS ALERT-BOX.

                 FIND FIRST liq_ccostos WHERE liq_ccostos.id_proveedor = liq_centros_costos.id_proveedor AND
                     liq_ccostos.id_origen = liq_centros_costos.id_origen and
                     liq_ccostos.id_centro_costo > 100000000 NO-LOCK NO-ERROR.
                 IF AVAILABLE liq_ccostos THEN
                     v_finca = liq_ccostos.id_centro_costo_liq.
             END.
         END.
    END.
   ELSE v_finca = liq_legajos.id_ccostos_liq.


END.
   DISPLAY liq_items_control_tareas.fecha 
           liq_control_tareas.id_proveedor
           liq_control_tareas.id_origen
           v_finca FORMAT ">>>>>>>>>>>>>".




END.
