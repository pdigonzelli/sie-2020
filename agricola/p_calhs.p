DEF VAR v_fecha AS DATE.
DEF VAR v_fecha_desde AS DATE.
DEF VAR v_total AS INTEGER.

v_fecha_desde = DATE("01/01/2010").

FOR EACH CONTROL_tareas WHERE id_sucursal = 25 AND fecha >= DATE("01/01/2010") NO-LOCK BY fecha:
    v_fecha = CONTROL_tareas.fecha.
    FOR EACH items_control_tareas OF CONTROL_tareas NO-LOCK:
       RUN devuelve-total-horas (INPUT CONTROL_tareas.nro_planilla,
                                 INPUT CONTROL_tareas.id_empresa,
                                 INPUT items_CONTROL_tareas.legajo,
                                 OUTPUT v_total).

       DISPLAY control_tareas.fecha items_control_tareas.legajo v_total.
   END.
END.




PROCEDURE devuelve-total-horas.
DEF INPUT PARAMETER p_nro AS INTEGER.
DEF INPUT PARAMETER p_empresa AS INTEGER.
DEF INPUT PARAMETER p_legajo AS INTEGER.
DEFINE OUTPUT PARAMETER p_total AS INTEGER.

DEFINE BUFFER b_control FOR CONTROL_tareas.
DEFINE BUFFER b_items FOR items_CONTROL_tareas.

IF v_fecha = v_fecha_desde THEN p_total = 0.

FOR EACH b_items WHERE b_items.fecha >= v_fecha_desde AND
    b_items.fecha <= v_fecha AND b_items.id_empresa = p_empresa AND b_items.legajo = p_legajo NO-LOCK, FIRST b_control OF b_items WHERE
     b_control.nro_planilla <> p_nro NO-LOCK:
    p_total = p_total + (b_items.cant_jornal_norm * 8 +  b_items.cant_hs_norm + b_items.cant_hs_comp).
END.

END PROCEDURE.
