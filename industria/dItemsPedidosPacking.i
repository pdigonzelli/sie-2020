  FIELD cant_pallets LIKE items_pedidos_packing.cant_pallets VALIDATE  FORMAT ">>>9" LABEL "Pallets"~
  FIELD calibre LIKE items_pedidos_packing.calibre VALIDATE ~
  FIELD contramarca LIKE items_pedidos_packing.contramarca VALIDATE ~
  FIELD en_proceso LIKE items_pedidos_packing.en_proceso VALIDATE ~
  FIELD id_articulo LIKE items_pedidos_packing.id_articulo VALIDATE  FORMAT ">>>>9" LABEL "Articulo"~
  FIELD id_calidad LIKE items_pedidos_packing.id_calidad VALIDATE ~
  FIELD id_caract LIKE items_pedidos_packing.id_caract VALIDATE  LABEL "Caract."~
  FIELD id_categoria LIKE items_pedidos_packing.id_categoria VALIDATE ~
  FIELD id_envase LIKE items_pedidos_packing.id_envase VALIDATE ~
  FIELD id_marca LIKE items_pedidos_packing.id_marca VALIDATE ~
  FIELD id_orden LIKE items_pedidos_packing.id_orden VALIDATE ~
  FIELD id_empresa LIKE items_pedidos_packing.id_empresa VALIDATE ~
  FIELD id_punto_emisor LIKE items_pedidos_packing.id_punto_emisor VALIDATE ~
  FIELD id_tipo_pallet LIKE items_pedidos_packing.id_tipo_pallet VALIDATE ~
  FIELD id_tipo_esquinero LIKE items_pedidos_packing.id_tipo_esquinero VALIDATE ~
  FIELD id_vapor LIKE items_pedidos_packing.id_vapor VALIDATE ~
  FIELD id_variedad LIKE items_pedidos_packing.id_variedad VALIDATE ~
  FIELD item LIKE items_pedidos_packing.item VALIDATE ~
  FIELD semana LIKE items_pedidos_packing.semana VALIDATE ~
  FIELD anio LIKE items_pedidos_packing.anio VALIDATE ~
  FIELD pallets LIKE r_pallets_envases.pallets LABEL "Cajas"
