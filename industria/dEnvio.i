  FIELD anio LIKE envios.anio VALIDATE ~
  FIELD NroLote AS CHARACTER FORMAT "XXXXXXXX/99" LABEL "NroLote"~
  FIELD Envase AS CHARACTER FORMAT "x(25)" LABEL "Envase"~
  FIELD Calidad AS CHARACTER FORMAT "x(20)" LABEL "Calidad"~
  FIELD Contrato AS CHARACTER FORMAT "x(20)" LABEL "Contrato"~
  FIELD Entrega AS CHARACTER FORMAT "x(40)" LABEL "Entrega"~
  FIELD Cliente AS CHARACTER FORMAT "x(25)" LABEL "Cliente"~
  FIELD DestinoFinal AS CHARACTER FORMAT "x(20)" LABEL "DestinoFinal"~
  FIELD Producto AS CHARACTER FORMAT "x(20)" LABEL "Producto"~
  FIELD Carrier AS CHARACTER FORMAT "x(20)" LABEL "Carrier"~
  FIELD Vapor AS CHARACTER FORMAT "x(30)"~
  FIELD c_fecha LIKE envios.c_fecha VALIDATE ~
  FIELD c_hora LIKE envios.c_hora VALIDATE ~
  FIELD c_usuario LIKE envios.c_usuario VALIDATE ~
  FIELD descripcion_fechas_intermedias1 LIKE envios.descripcion_fechas_intermedias[1] VALIDATE ~
  FIELD descripcion_fechas_intermedias2 LIKE envios.descripcion_fechas_intermedias[2] VALIDATE ~
  FIELD descripcion_fechas_intermedias3 LIKE envios.descripcion_fechas_intermedias[3] VALIDATE ~
  FIELD descripcion_fechas_intermedias4 LIKE envios.descripcion_fechas_intermedias[4] VALIDATE ~
  FIELD descripcion_fechas_intermedias5 LIKE envios.descripcion_fechas_intermedias[5] VALIDATE ~
  FIELD origen AS CHARACTER FORMAT "x(40)" LABEL "Origen"~
  FIELD fechas_intermedias1 LIKE envios.fechas_intermedias[1] VALIDATE ~
  FIELD fechas_intermedias2 LIKE envios.fechas_intermedias[2] VALIDATE ~
  FIELD fechas_intermedias3 LIKE envios.fechas_intermedias[3] VALIDATE ~
  FIELD fechas_intermedias4 LIKE envios.fechas_intermedias[4] VALIDATE ~
  FIELD fechas_intermedias5 LIKE envios.fechas_intermedias[5] VALIDATE ~
  FIELD fecha_llegada_destino LIKE envios.fecha_llegada_destino VALIDATE ~
  FIELD fecha_salida_destino LIKE envios.fecha_salida_destino VALIDATE ~
  FIELD fecha_salida_origen LIKE envios.fecha_salida_origen VALIDATE ~
  FIELD id_destino LIKE envios.id_destino VALIDATE ~
  FIELD id_envio LIKE envios.id_envio VALIDATE ~
  FIELD id_origen LIKE envios.id_origen VALIDATE ~
  FIELD id_vapor LIKE envios.id_vapor VALIDATE ~
  FIELD semana LIKE envios.semana VALIDATE ~
  FIELD fecha_custom_entry LIKE envios.fecha_custom_entry VALIDATE ~
  FIELD fecha_real_llegada LIKE envios.fecha_real_llegada VALIDATE ~
  FIELD fecha_release_customer LIKE envios.fecha_release_customer VALIDATE  LABEL "Fecha Released by Customer"~
  FIELD fecha_release_fda LIKE envios.fecha_release_fda VALIDATE  LABEL "Fecha Released by FDA"~
  FIELD observaciones LIKE envios.observaciones VALIDATE ~
  FIELD fecha_fda_notice LIKE envios.fecha_fda_notice VALIDATE  LABEL "Fecha FDA Notice"~
  FIELD nro_fda_notice LIKE envios.nro_fda_notice VALIDATE  LABEL "Nro FDA Notice"~
  FIELD id_orden_entrega LIKE r_envio_oe.id_orden_entrega VALIDATE ~
  FIELD id_cliente LIKE items_orden_entrega.id_cliente VALIDATE ~
  FIELD id_articulo LIKE items_orden_entrega.id_articulo VALIDATE ~
  FIELD item_oe LIKE items_orden_entrega.item_oe VALIDATE ~
  FIELD item LIKE items_orden_entrega.item VALIDATE ~
  FIELD id_contrato LIKE items_orden_entrega.id_contrato VALIDATE ~
  FIELD destino AS CHARACTER FORMAT "x(40)" LABEL "Destino"
