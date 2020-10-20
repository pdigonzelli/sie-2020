  FIELD id_contrato LIKE items_contratos.id_contrato VALIDATE ~
  FIELD item LIKE items_contratos.item VALIDATE ~
  FIELD anio LIKE items_contratos.anio VALIDATE ~
  FIELD id_tipo_contrato LIKE items_contratos.id_tipo_contrato VALIDATE ~
  FIELD id_articulo LIKE items_contratos.id_articulo VALIDATE ~
  FIELD id_calidad LIKE items_contratos.id_calidad VALIDATE ~
  FIELD id_envase LIKE items_contratos.id_envase VALIDATE ~
  FIELD Calidad LIKE calidades.descripcion VALIDATE  LABEL "Calidad"~
  FIELD Envase LIKE envases_prod.descripcion VALIDATE  LABEL "Envase"~
  FIELD abreviatura LIKE productos_terminados.abreviatura VALIDATE ~
  FIELD semana_entrega LIKE items_contratos.semana_entrega VALIDATE ~
  FIELD anio_semana_entrega LIKE items_contratos.anio_semana_entrega VALIDATE 
