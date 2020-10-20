def var p_archivo as character.
def var v_grupo as character.
def var v_grupo_consulta as character.
DEF VAR v_categorias_tareas AS CHARACTER.
DEF VAR v_abacus AS CHARACTER.
DEF BUFFER bcon01 FOR conceptos_abacus.

p_archivo = "z:\temp\rcatperstareas.txt".

output to value(p_archivo).
put "Convenio;Cod Cat;Categoria;Cod Cat Tarea;Categoria Tarea;Cod.Abacus;Desc.Cod.Abacus;Cod.Dif.;Desc.Cod.Dif.;Diferencial;Cod.Adic;Desc.Cod.Adic".
put skip.
for each r_cat_pers_tareas NO-LOCK BY id_convenio BY id_categoria BY id_categoria_tarea:
  find first liq_categorias of r_cat_pers_tareas no-lock no-error.
  FIND FIRST categorias_tareas WHERE categorias_tareas.id_categoria = r_cat_pers_tareas.id_categoria_tarea NO-LOCK NO-ERROR.
  FIND FIRST conceptos_abacus WHERE conceptos_abacus.id_concepto = r_cat_pers_tareas.id_codigo_abacus NO-LOCK NO-ERROR.
  FIND FIRST bcon01 WHERE bcon01.id_concepto = r_cat_pers_tareas.id_codigo_abacus_diferencial NO-LOCK NO-ERROR.
  FIND FIRST diferenciales WHERE diferenciales.id_diferencial = r_cat_pers_tareas.id_diferencial NO-LOCK NO-ERROR.

 export delimiter ";" 
   r_cat_pers_tareas.id_convenio 
   r_cat_pers_tareas.id_categoria 
   (IF AVAILABLE liq_categorias THEN liq_categorias.descripcion ELSE "")
   r_cat_pers_tareas.id_categoria_tarea
   (IF AVAILABLE categorias_tareas THEN categorias_tareas.descripcion ELSE "")
   r_cat_pers_tareas.id_codigo_abacus
   (IF AVAILABLE conceptos_abacus THEN conceptos_abacus.descripcion ELSE "")
   r_cat_pers_tareas.id_codigo_abacus_diferencial
   (IF AVAILABLE bcon01 THEN bcon01.descripcion ELSE "")
   (IF AVAILABLE diferenciales THEN diferenciales.descripcion ELSE "").

 END.
output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
