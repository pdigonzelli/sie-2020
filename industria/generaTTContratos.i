    CREATE tt-contrato.

    IF AVAILABLE contactos_industria THEN DO:
        ASSIGN tt-contrato.cliente_final        = contactos_industria.razon_social.
    END.
    IF AVAILABLE tipo_moneda THEN DO:
        ASSIGN tt-contrato.moneda_venta         = tipo_moneda.descripcion.
    END.
    IF AVAILABLE clausulas THEN DO:
        ASSIGN tt-contrato.condicion_venta      = clausulas.descripcion.
    END.
    IF AVAILABLE productos_terminados THEN DO:
        ASSIGN tt-contrato.articulo             = productos_terminados.descripcion.
    END.
    
    IF AVAILABLE tipo_unidad_venta THEN DO:
        ASSIGN tt-contrato.unidad_venta         = tipo_unidad_venta.descripcion.
    END.

    IF AVAILABLE destinos THEN 
        ASSIGN tt-contrato.destino    = destinos.descripcion
               tt-contrato.id_destino = destinos.id_destino.
    
    /*IF AVAILABLE zonas_clientes THEN*/

    
        ASSIGN tt-contrato.id_contrato          = items_contratos.id_contrato
               tt-contrato.id_tipo_contrato     = items_contratos.id_tipo_contrato
               tt-contrato.anio                 = items_contratos.anio_semana_entrega
               tt-contrato.ITEM                 = items_contratos.ITEM
               tt-contrato.orden_fabricacion    = contratos.orden_fabricacion
               tt-contrato.id_vapor             = v_vapor
               tt-contrato.anio_envio             = v_anio_envio
               tt-contrato.id_articulo            = items_contrato.id_articulo
               tt-contrato.id_calidad             = items_contratos.id_calidad
               tt-contrato.calidad                = calidades.descripcion
               tt-contrato.semana_desde           = items_contratos.semana_entrega
               tt-contrato.semana_hasta           = items_contratos.semana_entrega_hasta
               tt-contrato.anio_item_contrato     = items_contratos.anio_semana_entrega
               tt-contrato.precio_venta           = items_contratos.precio_origen
               tt-contrato.id_condicion_venta     = items_contratos.id_clausula
               tt-contrato.fecha_contrato         = contrato.fecha
               tt-contrato.importe_contrato       = v_importe_contrato
               tt-contrato.precio_unitario_x_kilo = v_importe_contrato_kilos
               tt-contrato.id_cliente             = contratos.id_cliente
               tt-contrato.cliente                = clientes.razon_social
               tt-contrato.id_cliente_final       = contratos.id_cliente_final
               tt-contrato.orden_entrega          = v_orden_entrega
               tt-contrato.cantidad_contratos     = v_cantidad_contratos
               tt-contrato.porc_comision          = v_porc_comision
               tt-contrato.importe_comision       = v_importe_comision
               tt-contrato.lote                           = v_lotes_contrato
               tt-contrato.importe_contrato_fob           = v_importe_contrato_fob
               tt-contrato.importe_contrato_kilos_400     = v_importe_contrato_kilos_400 
               tt-contrato.importe_contrato_kilos_fob     = v_importe_contrato_kilos_fob
               tt-contrato.importe_contrato_kilos_fob_400 = v_importe_contrato_kilos_fob_400
               tt-contrato.gastos                         = v_importe_contrato - v_importe_contrato_fob
               tt-contrato.coef = v_coef.

               IF v_tiene_lote_anterior THEN
                  ASSIGN tt-contrato.lotes_anio_anterior = TRUE.
               ELSE
                  ASSIGN tt-contrato.lotes_anio_anterior = FALSE.

               /*
               tt-contrato.importe_factura        = v_importe_fac_fob
               tt-contrato.importe_fac_todos      = v_importe_fac_fob + v_importe_fac_todos
               tt-contrato.importe_item_factura   = v_importe_item_fac_fob
               tt-contrato.importe_item_fac_todos = v_importe_item_fac_todos
               tt-contrato.factura                = v_factura
               tt-contrato.fecha_factura          = v_fecha_fac
               tt-contrato.vto_factura            = v_fecha_vto*/

               /*        tt-contrato.id_destino           = v_destino*/

               /*
               tt-contrato.nro_pack_list          = v_nro_pack_list
               tt-contrato.cantidad_pl            = v_cantidad_pl

                               */

               /*tt-contrato.nro_contenedor         = v_nro_contenedor
               tt-contrato.cobranzas              = v_cobranzas
               tt-contrato.cantidad_pl_400        = v_cantidad_pl_400
                              tt-contrato.fechas_cobranzas       = v_fechas_cobranzas.*/
               /*
               v_total_cobranzas = 0.
               DO i = 1 TO NUM-ENTRIES(v_cobranzas,";").
                    v_total_cobranzas = v_total_cobranzas + DECIMAL(ENTRY(i,v_cobranzas,";")).
               END.*/
               /*
               ASSIGN tt-contrato.TOTAL_cobranzas        = v_total_cobranzas
                      tt-contrato.consignacion           = v_consignacion.*/



        
        
