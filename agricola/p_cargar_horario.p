DEF INPUT PARAMETER v_intervalo AS INTEGER.
DEF INPUT PARAMETER xRow AS ROWID NO-UNDO.
DEF INPUT PARAMETER v_inicio AS CHARACTER NO-UNDO.
DEF INPUT PARAMETER v_fin AS CHARACTER NO-UNDO.

DEF BUFFER b_control FOR CONTROL_tareas.
DEF BUFFER b_items FOR items_control_tareas.

FIND FIRST b_control WHERE ROWID(b_control) = xRow NO-LOCK NO-ERROR.
IF NOT AVAILABLE b_control THEN RETURN.

FOR EACH b_items OF b_control:
   CASE v_intervalo:
       WHEN 1 THEN
       DO:
           IF b_items.hora_inicio = "" THEN
               ASSIGN b_items.hora_inicio = v_inicio.
           IF b_items.hora_fin = "" THEN
           ASSIGN b_items.hora_fin = v_fin.
       END.
       WHEN 2 THEN
       DO:
           IF b_items.hora_inicio-1 = "" THEN
               ASSIGN b_items.hora_inicio-1 = v_inicio.
           IF b_items.hora_fin-1 = "" THEN
           ASSIGN b_items.hora_fin-1 = v_fin.
       END.
   END CASE.

END.

