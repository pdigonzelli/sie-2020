  FIELD id_orden_entrega LIKE orden_entrega.id_orden_entrega VALIDATE ~
  FIELD fecha LIKE orden_entrega.fecha VALIDATE ~
  FIELD semana_embarque LIKE orden_entrega.semana_embarque VALIDATE ~
  FIELD item_oe LIKE items_orden_entrega.item_oe VALIDATE ~
  FIELD tambores_pedidos LIKE items_orden_entrega.tambores_pedidos VALIDATE  FORMAT ">>>>>>>>>>>9"~
  FIELD id_articulo LIKE items_orden_entrega.id_articulo VALIDATE ~
  FIELD id_contrato LIKE items_orden_entrega.id_contrato VALIDATE ~
  FIELD orden_fabricacion LIKE contratos.orden_fabricacion VALIDATE 
