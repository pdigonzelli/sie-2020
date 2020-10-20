    CREATE re_cial_completo.
    IF AVAILABLE orden_entrega THEN
    DO:
        ASSIGN re_cial_completo.id_orden_entrega     = orden_entrega.id_orden_entrega
               re_cial_completo.id_vapor             = orden_entrega.id_vapor
               re_cial_completo.id_destino           = orden_entrega.id_destino
               re_cial_completo.destino              = destinos.descripcion
               re_cial_completo.anio_envio           = year(orden_entrega.fecha)
               re_cial_completo.semana_oe            = orden_entrega.semana_entrega
               re_cial_completo.id_estado            = orden_entrega.id_estado
               re_cial_completo.estado               = estados_oe.descripcion.
              /* re_cial_completo.importe_contrato     = orden_entrega.TOTAL_factura */
    END.
    ELSE
    DO:
        ASSIGN re_cial_completo.id_vapor             = items_contratos.id_vapor.
    END.
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
    IF AVAILABLE tipo_unidad_venta THEN DO:
        ASSIGN re_cial_completo.unidad_venta         = tipo_unidad_venta.descripcion.
    END.
        ASSIGN re_cial_completo.id_contrato          = items_contratos.id_contrato
               re_cial_completo.id_tipo_contrato     = items_contratos.id_tipo_contrato
               re_cial_completo.anio                 = items_contratos.anio_semana_entrega
               re_cial_completo.ITEM                 = items_contratos.ITEM
               re_cial_completo.orden_fabricacion    = contratos.orden_fabricacion
               re_cial_completo.id_articulo          = items_contrato.id_articulo
               re_cial_completo.articulo             = productos_terminados.descripcion
               re_cial_completo.id_calidad           = items_contratos.id_calidad
               re_cial_completo.calidad              = calidades.descripcion
               re_cial_completo.semana_desde         = items_contratos.semana_entrega
               re_cial_completo.semana_hasta         = items_contratos.semana_entrega_hasta
               re_cial_completo.anio_item_contrato   = items_contratos.anio_semana_entrega
               re_cial_completo.precio_venta         = items_contratos.precio_origen
               re_cial_completo.id_condicion_venta   = items_contratos.id_clausula
               re_cial_completo.fecha_contrato       = contrato.fecha
               re_cial_completo.importe_contrato     = v_importe_contrato
               
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
               
               re_cial_completo.cantidad_contratos     = v_cantidad_contratos

               re_cial_completo.nro_pack_list          = v_nro_pack_list
               re_cial_completo.cantidad_pl            = v_cantidad_pl
                               
               re_cial_completo.porc_comision          = v_porc_comision
               re_cial_completo.importe_comision       = (v_importe_comision * (v_porc_comision / 100))

               re_cial_completo.lote                   = v_lotes
                                                    
               re_cial_completo.nro_contenedor         = v_nro_contenedor
               re_cial_completo.cobranzas              = v_cobranzas
               re_cial_completo.fechas_cobranzas       = v_fechas_cobranzas.
               
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

        
