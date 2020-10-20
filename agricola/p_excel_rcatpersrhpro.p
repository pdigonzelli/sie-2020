def var p_archivo as character.
def var v_grupo as character.
def var v_grupo_consulta as character.
DEF VAR v_categorias_tareas AS CHARACTER.
DEF VAR v_abacus AS CHARACTER.
DEF BUFFER bcon01 FOR liq_conceptos.

p_archivo = "z:\temp\rcatpersrhpro.txt".

output to value(p_archivo).
put "Convenio;Cod Cat;Categoria;Cod Cat Tarea;Categoria Tarea;Cod.RHPRO;Desc.Cod.RHPRO;Cod.Dif.;Desc.Cod.Dif.;Diferencial;Cod.Adic;Desc.Cod.Adic".
put skip.
for each r_cat_pers_tareas_rhpro NO-LOCK BY id_cconvenio_liq BY id_categoria BY id_categoria_tarea:
  find first liq_ccategoriasliq WHERE
       liq_ccategoriasliq.id_categoria = r_cat_pers_tareas_rhpro.id_ccategoria_liq no-lock no-error.
  FIND FIRST categorias_tareas WHERE categorias_tareas.id_categoria = r_cat_pers_tareas_rhpro.id_categoria_tarea NO-LOCK NO-ERROR.
  FIND FIRST liq_conceptos WHERE liq_conceptos.id_concepto = r_cat_pers_tareas_rhpro.id_codigo_rhpro NO-LOCK NO-ERROR.
  FIND FIRST bcon01 WHERE bcon01.id_concepto = r_cat_pers_tareas_rhpro.id_codigo_rhpro_diferencial NO-LOCK NO-ERROR.
  FIND FIRST diferenciales WHERE diferenciales.id_diferencial = r_cat_pers_tareas_rhpro.id_diferencial NO-LOCK NO-ERROR.

 export delimiter ";" 
   r_cat_pers_tareas_rhpro.id_cconvenio 
   r_cat_pers_tareas_rhpro.id_ccategoria_liq 
   (IF AVAILABLE liq_ccategoriasliq THEN liq_ccategoriasliq.descripcion ELSE "")
   r_cat_pers_tareas_rhpro.id_categoria_tarea
   (IF AVAILABLE categorias_tareas THEN categorias_tareas.descripcion ELSE "")
   r_cat_pers_tareas_rhpro.id_codigo_rhpro
   (IF AVAILABLE liq_conceptos THEN liq_conceptos.descripcion ELSE "")
   r_cat_pers_tareas_rhpro.id_codigo_rhpro_diferencial
   (IF AVAILABLE bcon01 THEN bcon01.descripcion ELSE "")
   (IF AVAILABLE diferenciales THEN diferenciales.descripcion ELSE "").

 END.
output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
