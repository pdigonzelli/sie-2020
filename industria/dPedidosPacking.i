  FIELD id_empresa LIKE pedidos_packing.id_empresa VALIDATE ~
  FIELD id_orden LIKE pedidos_packing.id_orden VALIDATE ~
  FIELD semana LIKE pedidos_packing.semana VALIDATE ~
  FIELD pallets_hechos LIKE pedidos_packing.pallets_hechos VALIDATE ~
  FIELD total_pallets LIKE pedidos_packing.total_pallets VALIDATE ~
  FIELD id_vapor LIKE pedidos_packing.id_vapor VALIDATE ~
  FIELD id_vapor_usa LIKE pedidos_packing.id_vapor_usa VALIDATE ~
  FIELD id_puerto_ent LIKE pedidos_packing.id_puerto_ent VALIDATE ~
  FIELD id_puerto_sal LIKE pedidos_packing.id_puerto_sal VALIDATE ~
  FIELD id_punto_emisor LIKE pedidos_packing.id_punto_emisor VALIDATE  LABEL "P.Emisor"~
  FIELD id_mercado LIKE pedidos_packing.id_mercado VALIDATE ~
  FIELD id_cliente LIKE pedidos_packing.id_cliente VALIDATE ~
  FIELD id_cliente_remito LIKE pedidos_packing.id_cliente_remito VALIDATE ~
  FIELD en_proceso LIKE pedidos_packing.en_proceso VALIDATE ~
  FIELD estado LIKE pedidos_packing.estado VALIDATE ~
  FIELD fecha LIKE pedidos_packing.fecha VALIDATE ~
  FIELD completo LIKE pedidos_packing.completo VALIDATE ~
  FIELD contramarca LIKE pedidos_packing.contramarca VALIDATE ~
  FIELD ano LIKE pedidos_packing.ano VALIDATE ~
  FIELD puerto_destino LIKE destinos.descripcion VALIDATE  LABEL "Puerto Dest."~
  FIELD puerto_origen LIKE lugar_descarga.descripcion VALIDATE  LABEL "Puerto Orig"~
  FIELD id_cliente-2 LIKE clientes.id_cliente VALIDATE ~
  FIELD nombre LIKE clientes.nombre VALIDATE 
