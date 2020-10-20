  FIELD anio LIKE lotes_cascara.anio VALIDATE ~
  FIELD Sucursal AS CHARACTER FORMAT "x(20)" LABEL "Sucursal"~
  FIELD c_fecha LIKE lotes_cascara.c_fecha VALIDATE ~
  FIELD c_hora LIKE lotes_cascara.c_hora VALIDATE ~
  FIELD c_usuario LIKE lotes_cascara.c_usuario VALIDATE ~
  FIELD Fecha LIKE lotes_cascara.Fecha VALIDATE ~
  FIELD Vapor AS CHARACTER FORMAT "x(20)" LABEL "Vapor"~
  FIELD id_articulo LIKE lotes_cascara.id_articulo VALIDATE ~
  FIELD id_calidad LIKE lotes_cascara.id_calidad VALIDATE ~
  FIELD id_empresa LIKE lotes_cascara.id_empresa VALIDATE ~
  FIELD id_envase LIKE lotes_cascara.id_envase VALIDATE ~
  FIELD Cantidad AS INTEGER FORMAT ">,>>9" LABEL "Cantidad"~
  FIELD id_lote LIKE lotes_cascara.id_lote VALIDATE ~
  FIELD id_sucursal LIKE lotes_cascara.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE lotes_cascara.id_tipotambor VALIDATE ~
  FIELD nromov LIKE lotes_cascara.nromov VALIDATE ~
  FIELD observaciones LIKE lotes_cascara.observaciones VALIDATE ~
  FIELD id_orden_entrega LIKE lotes_cascara.id_orden_entrega VALIDATE ~
  FIELD item_oe LIKE lotes_cascara.item_oe VALIDATE ~
  FIELD id_contrato LIKE lotes_cascara.id_contrato VALIDATE ~
  FIELD item_contrato LIKE lotes_cascara.item_contrato VALIDATE ~
  FIELD id_tipo_contrato LIKE lotes_cascara.id_tipo_contrato VALIDATE ~
  FIELD anio_contrato LIKE lotes_cascara.anio_contrato VALIDATE ~
  FIELD control_calidad LIKE lotes_cascara.control_calidad VALIDATE ~
  FIELD usuario_control_calidad LIKE lotes_cascara.usuario_control_calidad VALIDATE ~
  FIELD id_sucursal_remito LIKE lotes_cascara.id_sucursal_remito VALIDATE ~
  FIELD id_tipo_movsto LIKE lotes_cascara.id_tipo_movsto VALIDATE ~
  FIELD nro_remito LIKE lotes_cascara.nro_remito VALIDATE ~
  FIELD id_lote_cliente LIKE lotes_cascara.id_lote_cliente VALIDATE ~
  FIELD anio_cliente LIKE lotes_cascara.anio_cliente VALIDATE ~
  FIELD codigo_lote LIKE lotes_cascara.codigo_lote VALIDATE ~
  FIELD Producto AS CHARACTER FORMAT "x(25)" LABEL "Producto"
