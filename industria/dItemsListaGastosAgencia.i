  FIELD item LIKE items_lista_gastos_agencia.item VALIDATE ~
  FIELD id_lista LIKE items_lista_gastos_agencia.id_lista VALIDATE ~
  FIELD id_gasto LIKE items_lista_gastos_agencia.id_gasto VALIDATE ~
  FIELD importe LIKE items_lista_gastos_agencia.importe VALIDATE ~
  FIELD Gasto AS CHARACTER FORMAT "x(20)" LABEL "Gasto"
