  FIELD id_contrato LIKE items_contratos.id_contrato VALIDATE ~
  FIELD OrdenFabricacion AS CHARACTER FORMAT "x(5)" LABEL "OrdenFabricacion"~
  FIELD Tipo AS CHARACTER FORMAT "x(15)" LABEL "Tipo"~
  FIELD Calidad AS CHARACTER FORMAT "x(20)" LABEL "Calidad"~
  FIELD Envase AS CHARACTER FORMAT "x(20)" LABEL "Envase"~
  FIELD Articulo AS CHARACTER FORMAT "x(20)" LABEL "Articulo"~
  FIELD item LIKE items_contratos.item VALIDATE ~
  FIELD anio LIKE items_contratos.anio VALIDATE ~
  FIELD id_tipo_contrato LIKE items_contratos.id_tipo_contrato VALIDATE ~
  FIELD id_articulo LIKE items_contratos.id_articulo VALIDATE ~
  FIELD id_calidad LIKE items_contratos.id_calidad VALIDATE ~
  FIELD id_envase LIKE items_contratos.id_envase VALIDATE ~
  FIELD semana_entrega LIKE items_contratos.semana_entrega VALIDATE ~
  FIELD anio_semana_entrega LIKE items_contratos.anio_semana_entrega VALIDATE ~
  FIELD adelanto LIKE items_contratos.adelanto VALIDATE ~
  FIELD arribo_estimado LIKE items_contratos.arribo_estimado VALIDATE ~
  FIELD cajas_pallets LIKE items_contratos.cajas_pallets VALIDATE ~
  FIELD calibres LIKE items_contratos.calibres VALIDATE ~
  FIELD cantidad LIKE items_contratos.cantidad VALIDATE ~
  FIELD cantidad_pallet LIKE items_contratos.cantidad_pallet VALIDATE ~
  FIELD cert_fito LIKE items_contratos.cert_fito VALIDATE ~
  FIELD comision_broker LIKE items_contratos.comision_broker VALIDATE ~
  FIELD comision_broker_viejo LIKE items_contratos.comision_broker_viejo VALIDATE ~
  FIELD comision_otros LIKE items_contratos.comision_otros VALIDATE ~
  FIELD comision_otros_viejo LIKE items_contratos.comision_otros_viejo VALIDATE ~
  FIELD consignacion LIKE items_contratos.consignacion VALIDATE ~
  FIELD contramarca LIKE items_contratos.contramarca VALIDATE ~
  FIELD cotizacion_base LIKE items_contratos.cotizacion_base VALIDATE ~
  FIELD cotizacion_origen LIKE items_contratos.cotizacion_origen VALIDATE ~
  FIELD c_fecha LIKE items_contratos.c_fecha VALIDATE ~
  FIELD c_hora LIKE items_contratos.c_hora VALIDATE ~
  FIELD c_usuario LIKE items_contratos.c_usuario VALIDATE ~
  FIELD descuento LIKE items_contratos.descuento VALIDATE ~
  FIELD destino_final LIKE items_contratos.destino_final VALIDATE ~
  FIELD detalle_clientes LIKE items_contratos.detalle_clientes VALIDATE ~
  FIELD embarque_estimado LIKE items_contratos.embarque_estimado VALIDATE ~
  FIELD estado LIKE items_contratos.estado VALIDATE ~
  FIELD fecha LIKE items_contratos.fecha VALIDATE ~
  FIELD id_articulo_cliente1 LIKE items_contratos.id_articulo_cliente[1] VALIDATE ~
  FIELD id_articulo_cliente2 LIKE items_contratos.id_articulo_cliente[2] VALIDATE ~
  FIELD id_articulo_cliente3 LIKE items_contratos.id_articulo_cliente[3] VALIDATE ~
  FIELD id_caract LIKE items_contratos.id_caract VALIDATE ~
  FIELD id_categoria LIKE items_contratos.id_categoria VALIDATE ~
  FIELD id_clausula LIKE items_contratos.id_clausula VALIDATE ~
  FIELD id_cliente_final LIKE items_contratos.id_cliente_final VALIDATE ~
  FIELD id_cond_pago_cliente LIKE items_contratos.id_cond_pago_cliente VALIDATE ~
  FIELD id_confeccion_final LIKE items_contratos.id_confeccion_final VALIDATE ~
  FIELD id_deposito_final LIKE items_contratos.id_deposito_final VALIDATE ~
  FIELD id_destino LIKE items_contratos.id_destino VALIDATE ~
  FIELD id_empacador LIKE items_contratos.id_empacador VALIDATE ~
  FIELD id_forma_carga LIKE items_contratos.id_forma_carga VALIDATE ~
  FIELD id_from LIKE items_contratos.id_from VALIDATE ~
  FIELD id_instrumento_pago LIKE items_contratos.id_instrumento_pago VALIDATE ~
  FIELD id_marca LIKE items_contratos.id_marca VALIDATE ~
  FIELD id_medida_pallet LIKE items_contratos.id_medida_pallet VALIDATE ~
  FIELD id_moneda_base LIKE items_contratos.id_moneda_base VALIDATE ~
  FIELD id_moneda_local LIKE items_contratos.id_moneda_local VALIDATE ~
  FIELD id_moneda_origen LIKE items_contratos.id_moneda_origen VALIDATE ~
  FIELD id_po_cliente1 LIKE items_contratos.id_po_cliente[1] VALIDATE ~
  FIELD id_po_cliente2 LIKE items_contratos.id_po_cliente[2] VALIDATE ~
  FIELD id_po_cliente3 LIKE items_contratos.id_po_cliente[3] VALIDATE ~
  FIELD id_puerto_ent LIKE items_contratos.id_puerto_ent VALIDATE ~
  FIELD id_puerto_sal LIKE items_contratos.id_puerto_sal VALIDATE ~
  FIELD id_tipo_empaque LIKE items_contratos.id_tipo_empaque VALIDATE ~
  FIELD id_tipo_envase LIKE items_contratos.id_tipo_envase VALIDATE ~
  FIELD id_tipo_esquinero LIKE items_contratos.id_tipo_esquinero VALIDATE ~
  FIELD id_tipo_pallet LIKE items_contratos.id_tipo_pallet VALIDATE ~
  FIELD id_tipo_tratamiento LIKE items_contratos.id_tipo_tratamiento VALIDATE ~
  FIELD id_tipo_unidad_venta LIKE items_contratos.id_tipo_unidad_venta VALIDATE ~
  FIELD id_tipo_unidad_venta_origen LIKE items_contratos.id_tipo_unidad_venta_origen VALIDATE ~
  FIELD id_tipo_venta LIKE items_contratos.id_tipo_venta VALIDATE ~
  FIELD id_vapor LIKE items_contratos.id_vapor VALIDATE ~
  FIELD id_variedad LIKE items_contratos.id_variedad VALIDATE ~
  FIELD interes LIKE items_contratos.interes VALIDATE ~
  FIELD kilos_cajas LIKE items_contratos.kilos_cajas VALIDATE ~
  FIELD marca_tambores LIKE items_contratos.marca_tambores VALIDATE ~
  FIELD numero_release1 LIKE items_contratos.numero_release[1] VALIDATE ~
  FIELD numero_release2 LIKE items_contratos.numero_release[2] VALIDATE ~
  FIELD numero_release3 LIKE items_contratos.numero_release[3] VALIDATE ~
  FIELD obleas LIKE items_contratos.obleas VALIDATE ~
  FIELD observaciones LIKE items_contratos.observaciones VALIDATE ~
  FIELD obs_comerciales LIKE items_contratos.obs_comerciales VALIDATE ~
  FIELD otra_identificacion LIKE items_contratos.otra_identificacion VALIDATE ~
  FIELD papel_sulfito LIKE items_contratos.papel_sulfito VALIDATE ~
  FIELD pendiente LIKE items_contratos.pendiente VALIDATE ~
  FIELD precio_base LIKE items_contratos.precio_base VALIDATE ~
  FIELD precio_base_calculo LIKE items_contratos.precio_base_calculo VALIDATE ~
  FIELD precio_comision LIKE items_contratos.precio_comision VALIDATE ~
  FIELD precio_local LIKE items_contratos.precio_local VALIDATE ~
  FIELD precio_origen LIKE items_contratos.precio_origen VALIDATE ~
  FIELD precio_venta LIKE items_contratos.precio_venta VALIDATE ~
  FIELD reutiliza_caja LIKE items_contratos.reutiliza_caja VALIDATE ~
  FIELD saldo LIKE items_contratos.saldo VALIDATE ~
  FIELD semana_entrega_hasta LIKE items_contratos.semana_entrega_hasta VALIDATE ~
  FIELD Cumplidos AS INTEGER FORMAT ">>>,>>>,>>9" LABEL "Cumplidos"~
  FIELD CondVenta AS CHARACTER FORMAT "x(50)" LABEL "CondVenta"~
  FIELD Moneda AS CHARACTER FORMAT "x(15)" LABEL "Moneda"
