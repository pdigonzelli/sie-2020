  FIELD item LIKE items_contratos.item VALIDATE ~
  FIELD semana_entrega LIKE items_contratos.semana_entrega VALIDATE ~
  FIELD cantidad LIKE items_contratos.cantidad VALIDATE ~
  FIELD id_destino LIKE items_contratos.id_destino VALIDATE ~
  FIELD descripcion LIKE destinos.descripcion VALIDATE ~
  FIELD estado LIKE items_contratos.estado VALIDATE ~
  FIELD pendiente LIKE items_contratos.pendiente VALIDATE ~
  FIELD id_orden_entrega LIKE items_orden_entrega.id_orden_entrega VALIDATE ~
  FIELD item_oe LIKE items_orden_entrega.item_oe VALIDATE ~
  FIELD anio_semana_entrega LIKE items_contratos.anio_semana_entrega VALIDATE 
