  FIELD anio LIKE permisos_embarque.anio VALIDATE ~
  FIELD fobfactura AS CHARACTER FORMAT "x(50)" LABEL "fobfactura"~
  FIELD descripcion LIKE aduanas.descripcion VALIDATE ~
  FIELD id_permiso_embarque LIKE permisos_embarque.id_permiso_embarque VALIDATE  FORMAT "XXXX 999999 X"~
  FIELD fecha LIKE permisos_embarque.fecha VALIDATE ~
  FIELD importe LIKE permisos_embarque.importe VALIDATE  FORMAT "->,>>>,>>9.99"~
  FIELD id_orden_entrega LIKE permisos_embarque.id_orden_entrega VALIDATE ~
  FIELD item_oe LIKE permisos_embarque.item_oe VALIDATE ~
  FIELD rectificado LIKE permisos_embarque.rectificado VALIDATE ~
  FIELD fecha_oficializacion LIKE permisos_embarque.fecha_oficializacion VALIDATE ~
  FIELD tipo_pe LIKE permisos_embarque.tipo_pe VALIDATE ~
  FIELD banco_interviniente LIKE permisos_embarque.banco_interviniente VALIDATE ~
  FIELD consignacion LIKE permisos_embarque.consignacion VALIDATE ~
  FIELD c_fecha LIKE permisos_embarque.c_fecha VALIDATE ~
  FIELD c_hora LIKE permisos_embarque.c_hora VALIDATE ~
  FIELD c_usuario LIKE permisos_embarque.c_usuario VALIDATE ~
  FIELD estado_afip LIKE permisos_embarque.estado_afip VALIDATE ~
  FIELD estado_bcra LIKE permisos_embarque.estado_bcra VALIDATE ~
  FIELD fecha_concluido LIKE permisos_embarque.fecha_concluido VALIDATE ~
  FIELD fecha_cumplido LIKE permisos_embarque.fecha_cumplido VALIDATE ~
  FIELD fecha_declaracion_venta LIKE permisos_embarque.fecha_declaracion_venta VALIDATE ~
  FIELD fecha_envio_tuc LIKE permisos_embarque.fecha_envio_tuc VALIDATE ~
  FIELD fecha_rec_tuc LIKE permisos_embarque.fecha_rec_tuc VALIDATE ~
  FIELD fecha_venc_derecho LIKE permisos_embarque.fecha_venc_derecho VALIDATE ~
  FIELD id_aduana LIKE permisos_embarque.id_aduana VALIDATE ~
  FIELD id_articulo LIKE permisos_embarque.id_articulo VALIDATE ~
  FIELD id_calidad LIKE permisos_embarque.id_calidad VALIDATE ~
  FIELD id_cliente LIKE permisos_embarque.id_cliente VALIDATE ~
  FIELD id_despachante LIKE permisos_embarque.id_despachante VALIDATE ~
  FIELD id_moneda_origen LIKE permisos_embarque.id_moneda_origen VALIDATE ~
  FIELD id_posicion_arancelaria LIKE permisos_embarque.id_posicion_arancelaria VALIDATE ~
  FIELD id_tipo_articulo LIKE permisos_embarque.id_tipo_articulo VALIDATE ~
  FIELD importe_derecho LIKE permisos_embarque.importe_derecho VALIDATE ~
  FIELD importe_reembolso LIKE permisos_embarque.importe_reembolso VALIDATE ~
  FIELD importe_reintegro LIKE permisos_embarque.importe_reintegro VALIDATE ~
  FIELD nro_remito_envio_tuc LIKE permisos_embarque.nro_remito_envio_tuc VALIDATE ~
  FIELD observaciones LIKE permisos_embarque.observaciones VALIDATE 
