  FIELD id_cliente LIKE contratos.id_cliente VALIDATE ~
  FIELD id_broker LIKE contratos.id_broker VALIDATE ~
  FIELD id_cliente_destino LIKE contratos.id_cliente_destino VALIDATE ~
  FIELD id_cliente_documentacion LIKE contratos.id_cliente_documentacion VALIDATE ~
  FIELD id_cliente_final LIKE contratos.id_cliente_final VALIDATE ~
  FIELD id_consignee LIKE contratos.id_consignee VALIDATE ~
  FIELD id_documentacion LIKE contratos.id_documentacion VALIDATE ~
  FIELD id_estado LIKE contratos.id_estado VALIDATE ~
  FIELD id_instrumento_pago LIKE contratos.id_instrumento_pago VALIDATE ~
  FIELD id_notify LIKE contratos.id_notify VALIDATE ~
  FIELD id_po_cliente1 LIKE contratos.id_po_cliente[1] VALIDATE ~
  FIELD anio LIKE items_contratos.anio VALIDATE ~
  FIELD item LIKE items_contratos.item VALIDATE ~
  FIELD id_tipo_contrato LIKE items_contratos.id_tipo_contrato VALIDATE ~
  FIELD id_contrato LIKE items_contratos.id_contrato VALIDATE ~
  FIELD id_articulo LIKE items_contratos.id_articulo VALIDATE ~
  FIELD id_calidad LIKE items_contratos.id_calidad VALIDATE ~
  FIELD id_deposito_final LIKE items_contratos.id_deposito_final VALIDATE ~
  FIELD cantidad LIKE items_contratos.cantidad VALIDATE ~
  FIELD semana_entrega LIKE items_contratos.semana_entrega VALIDATE ~
  FIELD semana_entrega_hasta LIKE items_contratos.semana_entrega_hasta VALIDATE ~
  FIELD nombre LIKE clientes.nombre VALIDATE ~
  FIELD Articulos LIKE productos_terminados.descripcion VALIDATE  LABEL "Articulo"~
  FIELD Calidades LIKE calidades.descripcion VALIDATE  LABEL "Calidad"
