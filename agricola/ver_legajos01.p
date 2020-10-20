FOR EACH liq_items_control_tareas WHERE fecha >= DATE("16/07/2016") 
    AND id_empresa = 101 AND legajo = 57217 AND id_sector = 6 NO-LOCK BY fecha:
    DISPLAY liq_items_control_tareas.fecha.
    FIND FIRST liq_control_tareas OF liq_items_control_tareas NO-LOCK NO-ERROR.
    IF AVAILABLE liq_control_tareas THEN
        DISPLAY  liq_control_tareas.fecha liq_control_tareas.id_proveedor liq_control_tareas.id_origen
        liq_control_tareas.id_sucursal liq_control_tareas.id_sector 
        liq_control_tareas.id_tipo_planilla
        liq_items_control_tareas.id_tarea 
        liq_items_control_tareas.cant_jornal
         cant_horas cantidad id_unidad_liquidacion.
END.
