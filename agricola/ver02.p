FOR EACH liq_items_control_tareas WHERE fecha >= DATE("01/01/2015")
     AND legajo = 61127 NO-LOCK:
    FIND FIRST liq_control_tareas OF liq_items_control_tareas NO-LOCK NO-ERROR.
    IF AVAILABLE liq_control_tareas THEN 
        DISPLAY liq_control_tareas.fecha liq_control_tareas.id_proveedor liq_control_tareas.id_origen cantidad.
END.
