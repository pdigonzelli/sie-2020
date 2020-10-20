FOR EACH CONTROL_tareas WHERE id_sucursal = 5 AND fecha >= TODAY - 1 :
    DISPLAY nro_planilla fecha.
    FOR EACH items_control_tareas OF CONTROL_tareas:
        DELETE items_control_tareas.
    END.
    DELETE CONTROL_tareas.
END.
