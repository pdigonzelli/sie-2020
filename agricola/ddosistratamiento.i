  FIELD id_dosis_tratamiento LIKE dosis_tratamiento.id_dosis_tratamiento VALIDATE ~
  FIELD id_ing_activo_tratamiento LIKE dosis_tratamiento.id_ing_activo_tratamiento VALIDATE ~
  FIELD id_prod_comercial_tratamiento LIKE dosis_tratamiento.id_prod_comercial_tratamiento VALIDATE ~
  FIELD id_tipo_prod_tratamiento LIKE dosis_tratamiento.id_tipo_prod_tratamiento VALIDATE ~
  FIELD valor_dosis LIKE dosis_tratamiento.valor_dosis VALIDATE ~
  FIELD id_medida_dosis LIKE dosis_tratamiento.id_medida_dosis VALIDATE ~
  FIELD desc-tipo LIKE tipo_prod_tratamiento.descripcion VALIDATE ~
  FIELD desc-prod LIKE prod_comercial_tratamiento.descripcion VALIDATE ~
  FIELD desc-ing LIKE ing_activo_tratamiento.descripcion VALIDATE ~
  FIELD desc-dosis LIKE medidas_dosis.descripcion VALIDATE ~
  FIELD desc-tratamiento AS CHARACTER FORMAT "x(60)"~
  FIELD descripcion LIKE dosis_tratamiento.descripcion VALIDATE 
