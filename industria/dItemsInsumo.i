  FIELD cantidad LIKE items_insumo.cantidad VALIDATE ~
  FIELD Insumo AS CHARACTER FORMAT "x(30)" LABEL "Insumo"~
  FIELD Unidad AS CHARACTER FORMAT "x(25)" LABEL "Unidad"~
  FIELD id_articulo LIKE items_insumo.id_articulo VALIDATE ~
  FIELD id_calidad LIKE items_insumo.id_calidad VALIDATE ~
  FIELD id_confeccion LIKE items_insumo.id_confeccion VALIDATE ~
  FIELD id_envase LIKE items_insumo.id_envase VALIDATE ~
  FIELD id_insumo LIKE items_insumo.id_insumo VALIDATE ~
  FIELD id_tipo_unidad LIKE items_insumo.id_tipo_unidad VALIDATE ~
  FIELD item LIKE items_insumo.item VALIDATE 
