  FIELD id_gasto LIKE tarifa_depositos.id_gasto VALIDATE ~
  FIELD id_moneda LIKE tarifa_depositos.id_moneda VALIDATE ~
  FIELD id_sucursal_ubicacion LIKE tarifa_depositos.id_sucursal_ubicacion VALIDATE ~
  FIELD id_unidad LIKE tarifa_depositos.id_unidad VALIDATE ~
  FIELD importe LIKE tarifa_depositos.importe VALIDATE ~
  FIELD Deposito AS CHARACTER FORMAT "x(20)" LABEL "Deposito"~
  FIELD Gasto AS CHARACTER FORMAT "x(15)" LABEL "Gasto"~
  FIELD Moneda AS CHARACTER FORMAT "x(15)" LABEL "Moneda"~
  FIELD Unidad AS CHARACTER FORMAT "x(15)" LABEL "Unidad"
