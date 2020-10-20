DEF VAR v_cuenta AS INTEGER.
DEF BUFFER b1 FOR r_cat_pers_tareas.

FOR EACH r_cat_pers_tareas WHERE id_categoria_tarea = 4 NO-LOCK:
   /* DISPLAY r_cat_pers_tareas.   */
    /*CREATE b1.
    BUFFER-COPY r_cat_pers_tareas EXCEPT id_categoria_tarea TO b1.
    ASSIGN b1.id_categoria_tarea = 4.*/

    v_cuenta = v_cuenta + 1.

END.

MESSAGE v_cuenta VIEW-AS ALERT-BOX.
