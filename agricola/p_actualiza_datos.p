 DEF INPUT PARAMETER v_sector AS INTEGER.
 DEF INPUT PARAMETER v_sucursal AS INTEGER.
 DEF INPUT PARAMETER v_fecha_desde AS DATE.
 DEF INPUT PARAMETER v_fecha_hasta AS DATE.
 DEF INPUT PARAMETER v_empresa AS INTEGER.
    
FOR EACH CONTROL_tareas WHERE CONTROL_tareas.id_sector = v_sector AND
    CONTROL_tareas.id_sucursal = v_sucursal AND
    CONTROL_tareas.fecha >= v_fecha_desde AND
    CONTROL_tareas.fecha  <= v_fecha_hasta AND
    CONTROL_tareas.id_empresa = v_empresa NO-LOCK:
    FOR EACH items_control_tareas OF CONTROL_tareas :
        assign  cant_jornal = cant_jornal_norm
                cant_horas = (cant_hs_norm + cant_hs_comp + cant_hs_extras).
    END.
END.
