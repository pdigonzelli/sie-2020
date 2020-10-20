FOR EACH liq_items_control_tareas WHERE fecha >= DATE("16/07/2016") 
    AND id_empresa = 101  AND id_sector = 6 AND cant_horas <> 0 NO-LOCK, 
    FIRST tareas OF liq_items_control_tareas NO-LOCK BREAK BY liq_items_control_tareas.id_tarea:
    
    IF LAST-OF(liq_items_control_tareas.id_tarea) THEN
    IF tareas.id_concepto_liq[15] = 0 THEN
        DISPLAY tareas.id_tarea.

    
/*    DISPLAY liq_items_control_tareas.fecha.
    FIND FIRST liq_control_tareas OF liq_items_control_tareas NO-LOCK NO-ERROR.
    IF AVAILABLE liq_control_tareas THEN
        DISPLAY  liq_control_tareas.fecha liq_control_tareas.id_proveedor liq_control_tareas.id_origen
        liq_control_tareas.id_sucursal liq_control_tareas.id_sector 
        liq_control_tareas.id_tipo_planilla
        liq_items_control_tareas.id_tarea 
        liq_items_control_tareas.cant_jornal
         cant_horas cantidad id_unidad_liquidacion.*/
END.
