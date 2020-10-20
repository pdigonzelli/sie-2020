DEFINE INPUT PARAMETER p_empresa AS INTEGER.
DEFINE INPUT PARAMETER p_suc AS INTEGER.
DEFINE INPUT PARAMETER p_sector AS INTEGER.
DEFINE INPUT PARAMETER p_tipo_planilla AS INTEGER.
DEFINE INPUT PARAMETER p_nro_planilla AS INTEGER.
DEFINE INPUT PARAMETER p_fecha AS DATE.

DEF BUFFER b_control FOR CONTROL_tarea.
DEF BUFFER b_items FOR items_control_tareas.

FOR EACH b_control WHERE b_control.id_empresa = p_empresa AND
    b_control.id_sucursal = p_suc AND
    b_control.id_sector = p_sector AND
    b_control.id_tipo_planilla = p_tipo_planilla AND
    b_control.nro_planilla = p_nro_planilla AND
    b_control.fecha = p_fecha:
    FOR EACH b_items OF b_control:
        DELETE b_items.
    END.
  DELETE b_control.
END.
