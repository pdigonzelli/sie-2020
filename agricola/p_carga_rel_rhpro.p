def temp-table t-temp6 
  FIELD id_c1 AS INTEGER
  FIELD id_cconvenio_liq AS INTEGER
  FIELD id_c2 AS INTEGER
  FIELD id_ccategoria_liq AS INTEGER
  FIELD desc-categoria AS CHARACTER
  FIELD id_categoria_tarea AS INTEGER
  FIELD desc-cattarea AS CHARACTER
  FIELD id_codigo_rhpro AS INTEGER
  FIELD id_c3 AS INTEGER
  FIELD desc-codigo AS CHARACTER
  FIELD id_codigo_rhpro_diferencial AS INTEGER
  FIELD id_c4 AS INTEGER
  FIELD desc-coddif AS CHARACTER
  FIELD diferencial AS CHARACTER.

DEF VAR i AS INTEGER.
DEF VAR v-titulo AS CHARACTER. 
DEF VAR v-path AS CHARACTER.
DEF VAR v_inicio AS INTEGER.
DEF VAR v_legajo AS INTEGER.

v-path = "z:\sistemas\sami\util\".


input from VALUE(v-path + "tareas-agricola.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp6.
      import DELIMITER ";" t-temp6.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

/*
FOR EACH t-temp6:
    DISPLAY t-temp6.
END.
*/


FOR EACH t-temp6 WHERE t-temp6.id_cconvenio_liq <> 0 NO-LOCK:
    FIND FIRST r_cat_pers_tareas_rhpro WHERE
        r_cat_pers_tareas_rhpro.id_cconvenio_liq = t-temp6.id_cconvenio_liq AND
        r_cat_pers_tareas_rhpro.id_ccategoria_liq = t-temp6.id_ccategoria_liq AND
        r_cat_pers_tareas_rhpro.id_categoria_tarea = t-temp6.id_categoria_tarea NO-ERROR.
    IF NOT AVAILABLE r_cat_pers_tareas_rhpro THEN
        CREATE r_cat_pers_tareas_rhpro.
    /*BUFFER-COPY t-temp6 TO r_cat_pers_tareas_rhpro.*/
    
    IF t-temp6.diferencial <> "" THEN
    DO:
        DISPLAY t-temp6.diferencial.
        UPDATE r_cat_pers_tareas_rhpro.id_diferencial.
    END.
    
END.


MESSAGE "La importacion desde RHPRO fue realizada" VIEW-AS ALERT-BOX WARNING.
