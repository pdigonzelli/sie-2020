  FIELD id_categoria_tarea LIKE r_cat_pers_tareas_rhpro.id_categoria_tarea VALIDATE ~
  FIELD id_ccategoria_liq LIKE r_cat_pers_tareas_rhpro.id_ccategoria_liq VALIDATE ~
  FIELD id_cconvenio_liq LIKE r_cat_pers_tareas_rhpro.id_cconvenio_liq VALIDATE ~
  FIELD id_codigo_rhpro LIKE r_cat_pers_tareas_rhpro.id_codigo_rhpro VALIDATE ~
  FIELD id_codigo_rhpro_diferencial LIKE r_cat_pers_tareas_rhpro.id_codigo_rhpro_diferencial VALIDATE  FORMAT ">>>>9"~
  FIELD id_diferencial LIKE r_cat_pers_tareas_rhpro.id_diferencial VALIDATE ~
  FIELD desc-categoria LIKE liq_ccategoriasliq.descripcion VALIDATE  LABEL "Desc.Cat."~
  FIELD desc-concepto LIKE liq_conceptos.descripcion VALIDATE  LABEL "Desc.Concepto"~
  FIELD des-cat-tareas LIKE categorias_tareas.descripcion VALIDATE  LABEL "Cat.Tareas" COLUMN-LABEL "Cat.Tareas"
