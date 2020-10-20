  FIELD id_gasto LIKE gastos_orden_entrega.id_gasto VALIDATE ~
  FIELD Gasto AS CHARACTER FORMAT "x(20)" LABEL "Gasto"~
  FIELD id_orden_entrega LIKE gastos_orden_entrega.id_orden_entrega VALIDATE ~
  FIELD importe LIKE gastos_orden_entrega.importe VALIDATE 
