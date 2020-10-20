  FIELD c_fecha LIKE lista_gastos_agencia.c_fecha VALIDATE ~
  FIELD c_hora LIKE lista_gastos_agencia.c_hora VALIDATE ~
  FIELD c_usuario LIKE lista_gastos_agencia.c_usuario VALIDATE ~
  FIELD fecha_desde LIKE lista_gastos_agencia.fecha_desde VALIDATE ~
  FIELD fecha_hasta LIKE lista_gastos_agencia.fecha_hasta VALIDATE ~
  FIELD id_agencia LIKE lista_gastos_agencia.id_agencia VALIDATE ~
  FIELD id_lista LIKE lista_gastos_agencia.id_lista VALIDATE ~
  FIELD nombre LIKE lista_gastos_agencia.nombre VALIDATE ~
  FIELD observaciones LIKE lista_gastos_agencia.observaciones VALIDATE ~
  FIELD vigente LIKE lista_gastos_agencia.vigente VALIDATE ~
  FIELD Agencia AS CHARACTER FORMAT "x(25)" LABEL "Agencia"~
  FIELD id_tipo_lista LIKE lista_gastos_agencia.id_tipo_lista VALIDATE 
