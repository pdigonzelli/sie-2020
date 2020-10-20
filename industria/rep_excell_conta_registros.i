    CREATE re_cial_completo.
    IF AVAILABLE vapores THEN DO:
        ASSIGN re_cial_completo.vapor                = vapores.descripcion.
    END.
    IF AVAILABLE contactos_industria THEN DO:
        ASSIGN re_cial_completo.cliente_final        = contactos_industria.razon_social.
    END.
    IF AVAILABLE tipo_moneda THEN DO:
        ASSIGN re_cial_completo.moneda_venta         = tipo_moneda.descripcion.
    END.
    IF AVAILABLE clausulas THEN DO:
        ASSIGN re_cial_completo.condicion_venta      = clausulas.descripcion.
    END.
    IF AVAILABLE productos_terminados THEN DO:
        ASSIGN re_cial_completo.articulo             = productos_terminados.descripcion.
    END.
    
    IF AVAILABLE tipo_unidad_venta THEN DO:
        ASSIGN re_cial_completo.unidad_venta         = tipo_unidad_venta.descripcion.
    END.

    IF AVAILABLE destinos THEN
    DO:
        ASSIGN re_cial_completo.destino              = destinos.descripcion.
    END.
    
        ASSIGN re_cial_completo.id_contrato          = items_contratos.id_contrato
               re_cial_completo.id_tipo_contrato     = items_contratos.id_tipo_contrato
               re_cial_completo.anio                 = items_contratos.anio_semana_entrega
               re_cial_completo.ITEM                 = items_contratos.ITEM
               re_cial_completo.orden_fabricacion    = contratos.orden_fabricacion
               re_cial_completo.id_vapor             = v_vapor
               re_cial_completo.id_destino           = v_destino
               re_cial_completo.anio_envio           = v_anio_envio
               re_cial_completo.id_articulo          = items_contrato.id_articulo
               re_cial_completo.id_calidad           = items_contratos.id_calidad
               re_cial_completo.calidad              = calidades.descripcion
               re_cial_completo.semana_desde         = items_contratos.semana_entrega
               re_cial_completo.semana_hasta         = items_contratos.semana_entrega_hasta
               re_cial_completo.anio_item_contrato   = items_contratos.anio_semana_entrega
               re_cial_completo.precio_venta           = items_contratos.precio_origen
               re_cial_completo.id_condicion_venta     = items_contratos.id_clausula
               re_cial_completo.fecha_contrato         = contrato.fecha
               re_cial_completo.importe_contrato       = v_importe_contrato
               re_cial_completo.precio_unitario_x_kilo = v_importe_contrato_kilos
               
               re_cial_completo.importe_factura        = v_importe_fac_fob
               re_cial_completo.importe_fac_todos      = v_importe_fac_fob + v_importe_fac_todos
               re_cial_completo.importe_item_factura   = v_importe_item_fac_fob
               re_cial_completo.importe_item_fac_todos = v_importe_item_fac_todos
               re_cial_completo.factura                = v_factura
               re_cial_completo.fecha_factura          = v_fecha_fac
               re_cial_completo.vto_factura            = v_fecha_vto
               re_cial_completo.id_cliente             = contratos.id_cliente
               re_cial_completo.cliente                = clientes.razon_social
               re_cial_completo.id_cliente_final       = contratos.id_cliente_final

               re_cial_completo.orden_entrega          = v_orden_entrega
               
               re_cial_completo.cantidad_contratos     = v_cantidad_contratos

               re_cial_completo.nro_pack_list          = v_nro_pack_list
               re_cial_completo.cantidad_pl            = v_cantidad_pl
               re_cial_completo.cantidad_pl_400        = v_cantidad_pl_400
                               
               re_cial_completo.porc_comision          = v_porc_comision
               re_cial_completo.importe_comision       = v_importe_comision

               re_cial_completo.nro_contenedor         = v_nro_contenedor
               re_cial_completo.cobranzas              = v_cobranzas
               re_cial_completo.fechas_cobranzas       = v_fechas_cobranzas.
               
               IF v_lotes_contrato = "" THEN
                    ASSIGN re_cial_completo.lote       = v_lotes.
               ELSE
                    ASSIGN re_cial_completo.lote       = v_lotes_contrato.

               v_total_cobranzas = 0.
               DO i = 1 TO NUM-ENTRIES(v_cobranzas,";").
                    v_total_cobranzas = v_total_cobranzas + DECIMAL(ENTRY(i,v_cobranzas,";")).
               END.

        ASSIGN re_cial_completo.TOTAL_cobranzas        = v_total_cobranzas
               re_cial_completo.consignacion           = v_consignacion.

        IF v_tiene_lote_anterior THEN
            ASSIGN re_cial_completo.lotes_anio_anterior = TRUE.
        ELSE
            ASSIGN re_cial_completo.lotes_anio_anterior = FALSE.

        
        ASSIGN  re_cial_completo.importe_contrato_fob           = v_importe_contrato_fob
                re_cial_completo.importe_contrato_kilos_400     = v_importe_contrato_kilos_400 
                re_cial_completo.importe_contrato_kilos_fob     = v_importe_contrato_kilos_fob
                re_cial_completo.importe_contrato_kilos_fob_400 = v_importe_contrato_kilos_fob_400
                re_cial_completo.gastos                         = v_importe_contrato - v_importe_contrato_fob.
