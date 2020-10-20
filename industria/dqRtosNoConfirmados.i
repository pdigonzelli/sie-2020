  FIELD id_sucursal LIKE remitos.id_sucursal VALIDATE ~
  FIELD id_tipo_movsto LIKE remitos.id_tipo_movsto VALIDATE ~
  FIELD nro LIKE remitos.nro VALIDATE ~
  FIELD nro_comprobante LIKE remitos.nro_comprobante VALIDATE ~
  FIELD anio_of LIKE remitos.anio_of VALIDATE ~
  FIELD anio_produccion LIKE remitos.anio_produccion VALIDATE ~
  FIELD bultos LIKE remitos.bultos VALIDATE ~
  FIELD chofer LIKE remitos.chofer VALIDATE ~
  FIELD c_fecha LIKE remitos.c_fecha VALIDATE ~
  FIELD c_hora LIKE remitos.c_hora VALIDATE ~
  FIELD c_usuario LIKE remitos.c_usuario VALIDATE ~
  FIELD estado LIKE remitos.estado VALIDATE ~
  FIELD fecha LIKE remitos.fecha VALIDATE ~
  FIELD fecha_descarga LIKE remitos.fecha_descarga VALIDATE ~
  FIELD fecha_llegada LIKE remitos.fecha_llegada VALIDATE ~
  FIELD fecha_proceso LIKE remitos.fecha_proceso VALIDATE ~
  FIELD fecha_salida LIKE remitos.fecha_salida VALIDATE ~
  FIELD hora_descarga LIKE remitos.hora_descarga VALIDATE ~
  FIELD hora_llegada LIKE remitos.hora_llegada VALIDATE ~
  FIELD hora_salida LIKE remitos.hora_salida VALIDATE ~
  FIELD id_agencia LIKE remitos.id_agencia VALIDATE ~
  FIELD id_ciaseg LIKE remitos.id_ciaseg VALIDATE ~
  FIELD id_cliente LIKE remitos.id_cliente VALIDATE ~
  FIELD id_cliente_remito LIKE remitos.id_cliente_remito VALIDATE ~
  FIELD id_destino LIKE remitos.id_destino VALIDATE ~
  FIELD id_destino_final LIKE remitos.id_destino_final VALIDATE ~
  FIELD id_galpon LIKE remitos.id_galpon VALIDATE ~
  FIELD id_lugdes LIKE remitos.id_lugdes VALIDATE ~
  FIELD id_operacion LIKE remitos.id_operacion VALIDATE ~
  FIELD id_operacion_compra LIKE remitos.id_operacion_compra VALIDATE ~
  FIELD id_orden_entrega LIKE remitos.id_orden_entrega VALIDATE ~
  FIELD id_proveedor LIKE remitos.id_proveedor VALIDATE ~
  FIELD id_punto_venta LIKE remitos.id_punto_venta VALIDATE ~
  FIELD id_tipocomp_compra LIKE remitos.id_tipocomp_compra VALIDATE ~
  FIELD id_vapor LIKE remitos.id_vapor VALIDATE ~
  FIELD impresion LIKE remitos.impresion VALIDATE ~
  FIELD item_oe LIKE remitos.item_oe VALIDATE ~
  FIELD mercado LIKE remitos.mercado VALIDATE ~
  FIELD nromov_compra LIKE remitos.nromov_compra VALIDATE ~
  FIELD nro_contenedor LIKE remitos.nro_contenedor VALIDATE ~
  FIELD nro_iascav LIKE remitos.nro_iascav VALIDATE ~
  FIELD nro_orden_fab LIKE remitos.nro_orden_fab VALIDATE ~
  FIELD nro_ord_carga LIKE remitos.nro_ord_carga VALIDATE ~
  FIELD nro_per_embarque LIKE remitos.nro_per_embarque VALIDATE ~
  FIELD nro_precinto LIKE remitos.nro_precinto VALIDATE ~
  FIELD observaciones LIKE remitos.observaciones VALIDATE ~
  FIELD observ_ordenentrega LIKE remitos.observ_ordenentrega VALIDATE ~
  FIELD orden_embarque LIKE remitos.orden_embarque VALIDATE ~
  FIELD orden_fabricacion LIKE remitos.orden_fabricacion VALIDATE ~
  FIELD Pat_acopla LIKE remitos.Pat_acopla VALIDATE ~
  FIELD pat_chasis LIKE remitos.pat_chasis VALIDATE ~
  FIELD peso LIKE remitos.peso VALIDATE ~
  FIELD Peso_bruto LIKE remitos.Peso_bruto VALIDATE ~
  FIELD Peso_neto LIKE remitos.Peso_neto VALIDATE ~
  FIELD Tara LIKE remitos.Tara VALIDATE ~
  FIELD temp_llegada LIKE remitos.temp_llegada VALIDATE ~
  FIELD temp_salida LIKE remitos.temp_salida VALIDATE ~
  FIELD tipo_remito LIKE remitos.tipo_remito VALIDATE ~
  FIELD Valor_declarado LIKE remitos.Valor_declarado VALIDATE ~
  FIELD LugarDescarga AS CHARACTER FORMAT "x(20)" LABEL "LugarDescarga"~
  FIELD FechaArribo LIKE confirmacion_remito.fecha VALIDATE  FORMAT "99/99/9999" LABEL "FechaArribo"~
  FIELD Destino AS CHARACTER FORMAT "x(15)" LABEL "Destino"