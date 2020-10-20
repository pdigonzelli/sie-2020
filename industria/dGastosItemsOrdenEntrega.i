  FIELD id_gasto LIKE gastos_items_orden_entrega.id_gasto VALIDATE ~
  FIELD gasto AS CHARACTER FORMAT "x(30)" LABEL "gasto"~
  FIELD id_orden_entrega LIKE gastos_items_orden_entrega.id_orden_entrega VALIDATE ~
  FIELD importe LIKE gastos_items_orden_entrega.importe VALIDATE ~
  FIELD item_oe LIKE gastos_items_orden_entrega.item_oe VALIDATE 
