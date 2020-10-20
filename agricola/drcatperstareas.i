  FIELD id_categoria LIKE r_cat_pers_tareas.id_categoria VALIDATE ~
  FIELD id_categoria_tarea LIKE r_cat_pers_tareas.id_categoria_tarea VALIDATE ~
  FIELD id_codigo_abacus LIKE r_cat_pers_tareas.id_codigo_abacus VALIDATE ~
  FIELD id_codigo_abacus_diferencial LIKE r_cat_pers_tareas.id_codigo_abacus_diferencial VALIDATE  FORMAT ">>>9"~
  FIELD id_convenio LIKE r_cat_pers_tareas.id_convenio VALIDATE ~
  FIELD id_diferencial LIKE r_cat_pers_tareas.id_diferencial VALIDATE ~
  FIELD desc-cat-tarea LIKE categorias_tareas.descripcion VALIDATE ~
  FIELD desc-cat-legajo LIKE liq_categorias.descripcion VALIDATE ~
  FIELD desc-abacus LIKE conceptos_abacus.descripcion VALIDATE 
