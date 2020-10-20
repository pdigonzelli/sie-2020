  FIELD descripcion LIKE tipos_contratos.descripcion VALIDATE ~
  FIELD id_contrato LIKE contratos.id_contrato VALIDATE ~
  FIELD orden_fabricacion LIKE contratos.orden_fabricacion VALIDATE ~
  FIELD anio LIKE contratos.anio VALIDATE ~
  FIELD razon_social LIKE clientes.razon_social VALIDATE ~
  FIELD fecha LIKE contratos.fecha VALIDATE ~
  FIELD id_po_cliente1 LIKE contratos.id_po_cliente[1] VALIDATE ~
  FIELD id_tipo_contrato LIKE contratos.id_tipo_contrato VALIDATE 
