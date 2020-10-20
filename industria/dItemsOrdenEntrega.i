  FIELD anio LIKE items_orden_entrega.anio VALIDATE ~
  FIELD cliente AS CHARACTER FORMAT "x(50)" LABEL "cliente"~
  FIELD cajas_x_pallets LIKE items_orden_entrega.cajas_x_pallets VALIDATE ~
  FIELD cantidad_pallets LIKE items_orden_entrega.cantidad_pallets VALIDATE ~
  FIELD Producto AS CHARACTER FORMAT "x(50)" LABEL "producto"~
  FIELD cantidad_tambores LIKE items_orden_entrega.cantidad_tambores VALIDATE ~
  FIELD cerrado LIKE items_orden_entrega.cerrado VALIDATE ~
  FIELD destino AS CHARACTER FORMAT "x(50)" LABEL "destino"~
  FIELD contenedores LIKE items_orden_entrega.contenedores VALIDATE ~
  FIELD fob_ton LIKE items_orden_entrega.fob_ton VALIDATE ~
  FIELD condVta AS CHARACTER FORMAT "x(15)" LABEL "condVta"~
  FIELD fob_ton_u$s LIKE items_orden_entrega.fob_ton_u$s VALIDATE ~
  FIELD fob_unitario LIKE items_orden_entrega.fob_unitario VALIDATE ~
  FIELD agencia AS CHARACTER FORMAT "x(50)" LABEL "agencia"~
  FIELD id_articulo LIKE items_orden_entrega.id_articulo VALIDATE  FORMAT ">>9"~
  FIELD id_calidad LIKE items_orden_entrega.id_calidad VALIDATE ~
  FIELD despachante AS CHARACTER FORMAT "x(50)" LABEL "despachante"~
  FIELD id_cliente LIKE items_orden_entrega.id_cliente VALIDATE ~
  FIELD id_condicion_venta LIKE items_orden_entrega.id_condicion_venta VALIDATE ~
  FIELD vapor AS CHARACTER FORMAT "x(15)" LABEL "vapor"~
  FIELD id_contrato LIKE items_orden_entrega.id_contrato VALIDATE ~
  FIELD id_envase LIKE items_orden_entrega.id_envase VALIDATE ~
  FIELD id_estado LIKE items_orden_entrega.id_estado VALIDATE ~
  FIELD id_frigorifico LIKE items_orden_entrega.id_frigorifico VALIDATE ~
  FIELD id_instrumento_pago LIKE items_orden_entrega.id_instrumento_pago VALIDATE ~
  FIELD id_marca LIKE items_orden_entrega.id_marca VALIDATE ~
  FIELD id_moneda LIKE items_orden_entrega.id_moneda VALIDATE ~
  FIELD id_moneda_cambio LIKE items_orden_entrega.id_moneda_cambio VALIDATE ~
  FIELD id_orden_entrega LIKE items_orden_entrega.id_orden_entrega VALIDATE ~
  FIELD id_programa_despacho LIKE items_orden_entrega.id_programa_despacho VALIDATE ~
  FIELD id_tipo_contenedor LIKE items_orden_entrega.id_tipo_contenedor VALIDATE ~
  FIELD id_tipo_contrato LIKE items_orden_entrega.id_tipo_contrato VALIDATE ~
  FIELD id_tipo_orden_entrega LIKE items_orden_entrega.id_tipo_orden_entrega VALIDATE ~
  FIELD id_tipo_pallet LIKE items_orden_entrega.id_tipo_pallet VALIDATE ~
  FIELD id_tipo_plazo LIKE items_orden_entrega.id_tipo_plazo VALIDATE ~
  FIELD id_tipo_unidad_venta LIKE items_orden_entrega.id_tipo_unidad_venta VALIDATE ~
  FIELD id_tipo_venta LIKE items_orden_entrega.id_tipo_venta VALIDATE ~
  FIELD importe_comisiones LIKE items_orden_entrega.importe_comisiones VALIDATE ~
  FIELD importe_derechos_exportacion LIKE items_orden_entrega.importe_derechos_exportacion VALIDATE ~
  FIELD importe_factura_dolar LIKE items_orden_entrega.importe_factura_dolar VALIDATE ~
  FIELD importe_fob_dolar LIKE items_orden_entrega.importe_fob_dolar VALIDATE ~
  FIELD importe_origen LIKE items_orden_entrega.importe_origen VALIDATE ~
  FIELD importe_reintegro_fijo LIKE items_orden_entrega.importe_reintegro_fijo VALIDATE ~
  FIELD item LIKE items_orden_entrega.item VALIDATE ~
  FIELD item_oe LIKE items_orden_entrega.item_oe VALIDATE ~
  FIELD item_programa_despacho LIKE items_orden_entrega.item_programa_despacho VALIDATE ~
  FIELD kgs_brutos_tambores LIKE items_orden_entrega.kgs_brutos_tambores VALIDATE ~
  FIELD kgs_netos_tambores LIKE items_orden_entrega.kgs_netos_tambores VALIDATE ~
  FIELD kilos_x_caja LIKE items_orden_entrega.kilos_x_caja VALIDATE ~
  FIELD modo_actualizacion LIKE items_orden_entrega.modo_actualizacion VALIDATE ~
  FIELD pendiente LIKE items_orden_entrega.pendiente VALIDATE ~
  FIELD plazo LIKE items_orden_entrega.plazo VALIDATE ~
  FIELD precio_x_caja LIKE items_orden_entrega.precio_x_caja VALIDATE ~
  FIELD precio_x_galon LIKE items_orden_entrega.precio_x_galon VALIDATE ~
  FIELD semana_entrega LIKE items_orden_entrega.semana_entrega VALIDATE ~
  FIELD tambores_pedidos LIKE items_orden_entrega.tambores_pedidos VALIDATE ~
  FIELD tipo_cambio LIKE items_orden_entrega.tipo_cambio VALIDATE ~
  FIELD total_cajas LIKE items_orden_entrega.total_cajas VALIDATE ~
  FIELD total_factura LIKE items_orden_entrega.total_factura VALIDATE ~
  FIELD valor_aduana_derechos LIKE items_orden_entrega.valor_aduana_derechos VALIDATE ~
  FIELD valor_aduana_reintegro LIKE items_orden_entrega.valor_aduana_reintegro VALIDATE ~
  FIELD x_kilos LIKE items_orden_entrega.x_kilos VALIDATE ~
  FIELD fecha LIKE items_orden_entrega.fecha VALIDATE ~
  FIELD cert_fito LIKE items_orden_entrega.cert_fito VALIDATE ~
  FIELD grados_brix LIKE items_orden_entrega.grados_brix VALIDATE ~
  FIELD observaciones LIKE items_orden_entrega.observaciones VALIDATE ~
  FIELD total_galones LIKE items_orden_entrega.total_galones VALIDATE ~
  FIELD cheque LIKE items_orden_entrega.cheque VALIDATE ~
  FIELD coeficiente LIKE items_orden_entrega.coeficiente VALIDATE ~
  FIELD cotizacion LIKE items_orden_entrega.cotizacion VALIDATE ~
  FIELD c_fecha LIKE items_orden_entrega.c_fecha VALIDATE ~
  FIELD c_hora LIKE items_orden_entrega.c_hora VALIDATE ~
  FIELD c_usuario LIKE items_orden_entrega.c_usuario VALIDATE ~
  FIELD id_empresa_ipp LIKE items_orden_entrega.id_empresa_ipp VALIDATE ~
  FIELD id_orden_ipp LIKE items_orden_entrega.id_orden_ipp VALIDATE ~
  FIELD id_punto_emisor_ipp LIKE items_orden_entrega.id_punto_emisor_ipp VALIDATE ~
  FIELD item_ipp LIKE items_orden_entrega.item_ipp VALIDATE ~
  FIELD PrecioVenta AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "PrecioVenta"~
  FIELD Unidad AS CHARACTER FORMAT "x(8)" LABEL "Unidad"~
  FIELD GastosItem AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "GastosItem"~
  FIELD UniTon AS DECIMAL FORMAT ">>>,>>>,>>9.9999" LABEL "UniTon"~
  FIELD UniKil AS DECIMAL FORMAT ">>>,>>>,>>9.9999" LABEL "UniKil"~
  FIELD UniGal AS DECIMAL FORMAT ">>>,>>>,>>9.9999" LABEL "UniGal"~
  FIELD UniLib AS DECIMAL FORMAT ">>>,>>>,>>9.9999" LABEL "UniLib"~
  FIELD Libras AS DECIMAL FORMAT ">>>,>>>,>>9.9999" LABEL "Libras"~
  FIELD Moneda AS CHARACTER FORMAT "x(20)" LABEL "Moneda"~
  FIELD Estado AS CHARACTER FORMAT "x(20)" LABEL "Estado"
