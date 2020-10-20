  FIELD id_contrato LIKE contratos.id_contrato VALIDATE ~
  FIELD contratoCliente AS CHARACTER FORMAT "x(50)"~
  FIELD fecha LIKE contratos.fecha VALIDATE ~
  FIELD anio LIKE contratos.anio VALIDATE ~
  FIELD id_cliente LIKE contratos.id_cliente VALIDATE ~
  FIELD razon_social LIKE clientes.razon_social VALIDATE ~
  FIELD id_tipo_contrato LIKE contratos.id_tipo_contrato VALIDATE ~
  FIELD email_pl LIKE contratos.email_pl VALIDATE ~
  FIELD orden_fabricacion LIKE contratos.orden_fabricacion VALIDATE 
