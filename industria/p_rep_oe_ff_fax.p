DEFINE INPUT PARAMETER p_r_oe AS ROWID.
DEFINE VAR v_flete AS DECIMAL.
DEFINE VAR v_seguro AS DECIMAL.
DEFINE VAR v_vs_ddp AS DECIMAL.
DEFINE VAR v_entri AS DECIMAL.
DEFINE VAR i AS INTEGER.
DEFINE VAR vTotalFacturas AS DECIMAL.

FOR EACH rep_oe_fax.
    FOR EACH item_rep_oe_fax.
        DELETE ITEM_rep_oe_fax.
    END.
    DELETE rep_oe_fax.
END.
i = 0.
FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = p_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST agencias     WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
    FIND FIRST tipos_plazo  OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST vapores      OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST destinos     OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST despachantes OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = orden_entrega.id_lugdes NO-LOCK NO-ERROR.
    
    CREATE rep_oe_fax.
    ASSIGN rep_oe_fax.id_orden_entrega = orden_entrega.id_orden_entrega
        rep_oe_fax.semana           = orden_entrega.semana_embarque
        rep_oe_fax.id_agencia       = orden_entrega.id_agencia
        rep_oe_fax.agencia          = IF AVAILABLE agencias THEN agencias.abreviatura ELSE "NONE"
        rep_oe_fax.plazo            = orden_entrega.plazo
        rep_oe_fax.id_tipo_plazo    = orden_entrega.id_tipo_plazo
        rep_oe_fax.tipo_plazo       = IF AVAILABLE tipos_plazo THEN tipos_plazo.descripcion ELSE "NONE"
        rep_oe_fax.fecha_embarque   = orden_entrega.fecha_embarque
        rep_oe_fax.id_vapor         = orden_entrega.id_vapor
        rep_oe_fax.vapor            = IF AVAILABLE vapores THEN vapores.abreviatura ELSE "NONE"
        rep_oe_fax.id_destino       = orden_entrega.id_destino
        rep_oe_fax.destino          = IF AVAILABLE destinos THEN destinos.abreviatura ELSE "NONE"
        rep_oe_fax.id_despachante   = orden_entrega.id_despachante
        rep_oe_fax.despachante      = IF AVAILABLE despachantes THEN despachantes.descripcion ELSE "NONE"
        rep_oe_fax.id_lugar_descarga = orden_entrega.id_lugdes
        rep_oe_fax.lugar_descarga   = IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "NONE"
        rep_oe_fax.observaciones    = orden_entrega.observaciones.
    
    FOR EACH gastos_orden_entrega OF orden_entrega NO-LOCK.
        CASE gastos_orden_entrega.id_gasto:
            WHEN 6 THEN DO: /* DELIVERY */
                ASSIGN rep_oe_fax.delivery = gastos_orden_entrega.importe.
            END.
            WHEN 7 THEN DO: /* REPROCESO */
                ASSIGN rep_oe_fax.reproceso = gastos_orden_entrega.importe.
            END.
            WHEN 9 THEN DO: /* THC */
                ASSIGN rep_oe_fax.thc = gastos_orden_entrega.importe.
            END.
            WHEN 12 THEN DO: /* VARIOS */
                ASSIGN rep_oe_fax.varios = gastos_orden_entrega.importe.    
            END.
            WHEN 13 THEN DO: /* IKS */
                ASSIGN rep_oe_fax.iks = gastos_orden_entrega.importe.    
            END.
            WHEN 14 THEN DO: /* TOLL */
                ASSIGN rep_oe_fax.toll = gastos_orden_entrega.importe.    
            END.
            WHEN 15 THEN DO: /* HANDLING */
                ASSIGN rep_oe_fax.handling = gastos_orden_entrega.importe.    
            END.
            WHEN 16 THEN DO: /* INLAND */
                ASSIGN rep_oe_fax.inland = gastos_orden_entrega.importe.    
            END.
            WHEN 17 THEN DO: /* BUNKER */
                ASSIGN rep_oe_fax.bunker = gastos_orden_entrega.importe.    
            END.
            WHEN 19 THEN DO: /* BL */
                ASSIGN rep_oe_fax.bl = gastos_orden_entrega.importe.    
            END.
            WHEN 20 THEN DO: /* THC DESTINO */
                ASSIGN rep_oe_fax.thc_destino = gastos_orden_entrega.importe.    
            END.
            WHEN 23 THEN DO: /* FINANCING AND STORAGE */
                ASSIGN rep_oe_fax.financing_storage = gastos_orden_entrega.importe.    
            END.
            WHEN 24 THEN DO: /* PALLETIZING */
                ASSIGN rep_oe_fax.palletizing = gastos_orden_entrega.importe.    
            END.
            WHEN 26 THEN DO: /* INBALANCE SURCHARGE */
                ASSIGN rep_oe_fax.inb_sur = gastos_orden_entrega.importe.    
            END.
            WHEN 27 THEN DO: /* DCR */
                ASSIGN rep_oe_fax.dcr = gastos_orden_entrega.importe.    
            END.
            WHEN 28 THEN DO: /* WAREHOUSE */
                ASSIGN rep_oe_fax.warehouse = gastos_orden_entrega.importe.    
            END.
            WHEN 31 THEN DO: /* EBAF */
                ASSIGN rep_oe_fax.ebaf = gastos_orden_entrega.importe.    
            END.
            WHEN 32 THEN DO: /* AMS */
                ASSIGN rep_oe_fax.ams = gastos_orden_entrega.importe.    
            END.
        END CASE.    
    END.
    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK
                                BREAK BY items_orden_entrega.id_envase.
        
        v_flete = 0.
        v_seguro = 0.
        FOR EACH gastos_items_orden_entrega OF items_orden_entrega NO-LOCK.
            CASE gastos_items_orden_entrega.id_gasto:
                WHEN 3 THEN DO: /* DUTY DDP (ENTRY) */
                    v_entri = gastos_items_orden_entrega.importe.
                END.
                WHEN 5 THEN DO: /*FLETE*/
                    v_flete = gastos_items_orden_entrega.importe.    
                END.
                WHEN 11 THEN DO: /*SEGURO*/
                    v_seguro = gastos_items_orden_entrega.importe.    
                END.
            END CASE.
        END.
        FIND FIRST clausulas WHERE clausulas.id_clausula = items_orden_entrega.id_condicion_venta NO-LOCK NO-ERROR.
        FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
        IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:
            FIND FIRST variedades WHERE variedades.id_variedad = items_orden_entrega.id_calidad NO-LOCK NO-ERROR.
        END.
        ELSE DO:
            FIND FIRST calidades OF items_orden_entrega NO-LOCK NO-ERROR.
        END.
        FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST marcas_prod OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST tipo_moneda OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
        IF i = 0 THEN DO:
            FIND FIRST contactos_industria WHERE contactos_industria.id_contacto = contratos.id_notify
                                            NO-LOCK NO-ERROR.
            IF AVAILABLE contactos_industria THEN DO:
                /* OBTENGO LOS DATOS DEL NOTIFY */
                ASSIGN rep_oe_fax.id_contacto_notify     = contactos_industria.id_contacto
                       rep_oe_fax.nombre_notify          = contactos_industria.nombre
                       rep_oe_fax.direccion_notify       = contactos_industria.direccion
                       rep_oe_fax.localidad_notify       = contactos_industria.localidad
                       rep_oe_fax.provincia_notify       = contactos_industria.provincia
                       rep_oe_fax.codigo_postal_notify   = contactos_industria.codigo_postal
                       rep_oe_fax.pais_notify            = contactos_industria.pais
                       rep_oe_fax.contacto_notify        = contactos_industria.contacto
                       rep_oe_fax.tel_notify             = contactos_industria.tel
                       rep_oe_fax.fax_notify             = contactos_industria.fax.
            END.
            FIND FIRST contactos_industria WHERE contactos_industria.id_contacto = contratos.id_consignee
                                            NO-LOCK NO-ERROR.
            IF AVAILABLE contactos_industria THEN DO:
                /* OBTENGO LOS DATOS DEL CONSIGNEE */
                ASSIGN rep_oe_fax.id_contacto_consignee     = contactos_industria.id_contacto
                       rep_oe_fax.nombre_consignee          = contactos_industria.nombre
                       rep_oe_fax.direccion_consignee       = contactos_industria.direccion
                       rep_oe_fax.localidad_consignee       = contactos_industria.localidad
                       rep_oe_fax.provincia_consignee       = contactos_industria.provincia
                       rep_oe_fax.codigo_postal_consignee   = contactos_industria.codigo_postal
                       rep_oe_fax.pais_consignee            = contactos_industria.pais
                       rep_oe_fax.contacto_consignee        = contactos_industria.contacto
                       rep_oe_fax.tel_consignee             = contactos_industria.tel
                       rep_oe_fax.fax_consignee             = contactos_industria.fax.
            END.
            FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
            IF AVAILABLE items_contratos THEN DO:
                FIND FIRST destinos WHERE destinos.id_destino = items_contratos.destino_final NO-LOCK NO-ERROR.

                ASSIGN rep_oe_fax.id_po_cliente         = items_contratos.id_po_cliente[1]
                       rep_oe_fax.id_product_cliente    = items_contratos.id_articulo_cliente[1]
                       rep_oe_fax.id_release_cliente    = items_contratos.numero_release[1]
                       rep_oe_fax.id_destino_final      = items_contratos.destino_final
                       rep_oe_fax.destino_final         = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE".
            END.
            i = 1.
        END.
        FIND FIRST tipos_plazo OF contratos NO-LOCK NO-ERROR.
        FIND FIRST instrumentos_pagos WHERE instrumentos_pagos.id_instrumento_pago = 
                                            contratos.id_instrumento_pago NO-LOCK NO-ERROR.
        
        CREATE ITEM_rep_oe_fax.
        ASSIGN ITEM_rep_oe_fax.id_orden_entrega = items_orden_entrega.id_orden_entrega
            ITEM_rep_oe_fax.ITEM_oe             = items_orden_entrega.ITEM_oe
            ITEM_rep_oe_fax.id_cliente          = items_orden_entrega.id_cliente
            ITEM_rep_oe_fax.cliente             = IF AVAILABLE clientes THEN clientes.nombre ELSE "NONE"
            ITEM_rep_oe_fax.id_contrato         = items_orden_entrega.id_contrato
            ITEM_rep_oe_fax.id_tipo_contrato    = items_orden_entrega.id_tipo_contrato
            ITEM_rep_oe_fax.anio                = items_orden_entrega.anio
            ITEM_rep_oe_fax.ITEM                = items_orden_entrega.ITEM.

        IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:
            ASSIGN ITEM_rep_oe_fax.calidad             = IF AVAILABLE variedades THEN variedades.abreviatura ELSE "NONE"
                   ITEM_rep_oe_fax.id_calidad          = items_orden_entrega.id_calidad.
        END.
        ELSE DO:
            ASSIGN ITEM_rep_oe_fax.calidad             = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "NONE"
                   ITEM_rep_oe_fax.id_calidad          = items_orden_entrega.id_calidad.
        END.
        
        ASSIGN ITEM_rep_oe_fax.id_articulo         = items_orden_entrega.id_articulo
            ITEM_rep_oe_fax.articulo            = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
            ITEM_rep_oe_fax.tambores            = items_orden_entrega.cantidad_tambores
            /*ITEM_rep_oe_fax.orden_fabricacion   = */
            ITEM_rep_oe_fax.kilos_netos         = items_orden_entrega.kgs_netos_tambores
            ITEM_rep_oe_fax.kilos_brutos        = items_orden_entrega.kgs_brutos_tambores
            ITEM_rep_oe_fax.id_clausula         = items_orden_entrega.id_condicion_venta
            ITEM_rep_oe_fax.clausula            = IF AVAILABLE clausulas THEN clausulas.descripcion ELSE "NONE"
            ITEM_rep_oe_fax.id_marca            = items_orden_entrega.id_marca
            ITEM_rep_oe_fax.marca               = IF AVAILABLE marcas_prod THEN marcas_prod.abreviatura ELSE "NONE"
                .
            IF items_orden_entrega.id_tipo_venta = 3 THEN DO:
                ASSIGN ITEM_rep_oe_fax.clausula            = "Consignacion".
            END.
            IF orden_entrega.id_tipo_orden_entrega = 1 THEN DO:
                
                CASE items_orden_entrega.id_tipo_unidad_venta:
                    WHEN 1 THEN /* TONELADAS */ DO:
                        ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.total_factura / 
                                                                    (items_orden_entrega.kgs_netos_tambores / 1000).
                        
                    END.
                    WHEN 2 THEN /* KILOS */ DO:
                        ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.total_factura / 
                                                                     items_orden_entrega.kgs_netos_tambores.
                    END.
                    WHEN 3 THEN /* GALONES PERO ES IGUAL QUE TONELADAS */ DO:
                        ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.total_factura / 
                                                                    (items_orden_entrega.kgs_netos_tambores / 1000).
                    END.
                    WHEN 4 THEN /* LIBRAS PERO ES IGUAL QUE TONELADAS */ DO:
                        ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.total_factura / 
                                                                    (items_orden_entrega.kgs_netos_tambores / 1000).
                    END.
                END CASE.
                
            END.
            ELSE DO:
                /* ACA ENTRA CUANDO ES UNA OE DE FRUTA FRESCA */
                ASSIGN ITEM_rep_oe_fax.precio_unitario_ton = items_orden_entrega.precio_x_caja.
            END.

            vTotalFacturas = vTotalFacturas + items_orden_entrega.total_factura.
            IF LAST-OF(items_orden_entrega.id_envase) THEN DO:
             ASSIGN ITEM_rep_oe_fax.TOTAL_fob               = items_orden_entrega.fob_ton
                    ITEM_rep_oe_fax.flete                   = v_flete
                    ITEM_rep_oe_fax.seguro                  = v_seguro
                    ITEM_rep_oe_fax.vs_ddp                  = v_vs_ddp
                    ITEM_rep_oe_fax.importe_factura         = vTotalFacturas
                    ITEM_rep_oe_fax.precio_x_galon          = items_orden_entrega.total_factura / items_orden_entrega.TOTAL_galones
                    ITEM_rep_oe_fax.TOTAL_galones           = items_orden_entrega.TOTAL_galones
                    ITEM_rep_oe_fax.fob_ton                 = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.fob_unitario ELSE ROUND(items_orden_entrega.fob_unitario,0)
                    ITEM_rep_oe_fax.entri                   = v_entri
                    ITEM_rep_oe_fax.coeficiente             = items_orden_entrega.coeficiente
                    ITEM_rep_oe_fax.valor_aduana_derecho    = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.valor_aduana_derecho ELSE 0
                    ITEM_rep_oe_fax.valor_aduana_reintegro  = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.valor_aduana_reintegro ELSE 0
                    ITEM_rep_oe_fax.derecho                 = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.importe_derecho ELSE 0
                    ITEM_rep_oe_fax.reintegro               = IF items_orden_entrega.id_tipo_venta <> 3 THEN items_orden_entrega.importe_reintegro ELSE 0
                    ITEM_rep_oe_fax.comision                = items_orden_entrega.importe_comision
                    ITEM_rep_oe_fax.cert_fito               = items_orden_entrega.cert_fito
                    ITEM_rep_oe_fax.plazo                   = IF AVAILABLE contratos THEN contratos.plazo ELSE items_orden_entrega.plazo
                    ITEM_rep_oe_fax.id_tipo_plazo           = IF AVAILABLE contratos THEN contratos.id_tipo_plazo ELSE items_orden_entrega.id_tipo_plazo
                    ITEM_rep_oe_fax.tipo_plazo              = IF AVAILABLE tipos_plazo THEN tipos_plazo.descripcion ELSE "NONE"
                    ITEM_rep_oe_fax.id_instrumento_pago     = IF AVAILABLE contratos THEN contratos.id_instrumento_pago ELSE items_orden_entrega.id_instrumento_pago
                    ITEM_rep_oe_fax.instrumento_pago        = IF AVAILABLE instrumentos_pagos THEN instrumentos_pagos.descripcion ELSE "NONE"
                    ITEM_rep_oe_fax.id_tipo_unidad_venta    = items_orden_entrega.id_tipo_unidad_venta
                    ITEM_rep_oe_fax.pallets                 = items_orden_entrega.cantidad_pallets
                    ITEM_rep_oe_fax.cajas_x_pallets         = items_orden_entrega.cajas_x_pallets
                    ITEM_rep_oe_fax.x_kilos                 = items_orden_entrega.x_kilos
                    item_rep_oe_fax.moneda_cambio           = IF AVAILABLE tipo_moneda THEN tipo_moneda.descripcion ELSE "NONE".
        
            FIND FIRST porcentaje_reint_articulo OF items_orden_entrega NO-LOCK NO-ERROR.
            IF AVAILABLE porcentaje_reint_articulo THEN
             ASSIGN ITEM_rep_oe_fax.porc_reintegro      = porcentaje_reint_articulo.porcentaje.
            ELSE
             ASSIGN ITEM_rep_oe_fax.porc_reintegro      = 5.
            END.
        END.
     
END.
