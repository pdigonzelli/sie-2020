  FIELD anio LIKE r_gastos_items_contrato.anio VALIDATE ~
  FIELD Gasto AS CHARACTER FORMAT "x(25)" LABEL "Gasto"~
  FIELD Moneda AS CHARACTER FORMAT "x(25)" LABEL "Moneda"~
  FIELD Unidad AS CHARACTER FORMAT "x(15)" LABEL "Unidad"~
  FIELD id_contrato LIKE r_gastos_items_contrato.id_contrato VALIDATE ~
  FIELD id_gasto LIKE r_gastos_items_contrato.id_gasto VALIDATE ~
  FIELD id_moneda LIKE r_gastos_items_contrato.id_moneda VALIDATE ~
  FIELD id_tipo_contrato LIKE r_gastos_items_contrato.id_tipo_contrato VALIDATE ~
  FIELD id_tipo_unidad_venta LIKE r_gastos_items_contrato.id_tipo_unidad_venta VALIDATE ~
  FIELD importe LIKE r_gastos_items_contrato.importe VALIDATE ~
  FIELD item LIKE r_gastos_items_contrato.item VALIDATE ~
  FIELD porcentaje LIKE r_gastos_items_contrato.porcentaje VALIDATE 
