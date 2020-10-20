                
                for each items_orden_entrega of items_contratos
                                             WHERE id_orden_entrega > 1000 no-lock.
                    tiene-oe = TRUE.
                    FIND FIRST orden_entrega OF items_orden_entrega NO-LOCK NO-ERROR.
                    find agencias where agencias.id_agencia = orden_entrega.id_agencia no-lock no-error.
                    find vapores    of orden_entrega no-lock no-error.
                    find destinos   of orden_entrega no-lock no-error.
                    
                    find estados_oe             of orden_entrega NO-LOCK NO-ERROR.
                    find productos_terminados   of items_orden_entrega no-lock no-error.
                    find calidades              of items_orden_entrega no-lock no-error.

                    CREATE oe_packing_contratos_lotes.
                    ASSIGN oe_packing_contratos_lotes.id_orden_entrega   = orden_entrega.id_orden_entrega
                        oe_packing_contratos_lotes.item_oe               = items_orden_entrega.item_oe
                        oe_packing_contratos_lotes.id_vapor              = orden_entrega.id_vapor
                        oe_packing_contratos_lotes.vapor                 = IF AVAILABLE vapores THEN vapores.descripcion ELSE "NULL"
                        oe_packing_contratos_lotes.id_destino            = orden_entrega.id_destino
                        oe_packing_contratos_lotes.destino               = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NULL"
                        oe_packing_contratos_lotes.id_cliente            = items_contratos.id_cliente
                        oe_packing_contratos_lotes.cliente               = IF AVAILABLE clientes THEN clientes.nombre ELSE "NULL"
                        oe_packing_contratos_lotes.id_contrato           = items_orden_entrega.id_contrato
                        oe_packing_contratos_lotes.id_tipo_contrato      = items_orden_entrega.id_tipo_contrato
                        oe_packing_contratos_lotes.anio                  = items_orden_entrega.anio
                        oe_packing_contratos_lotes.ITEM                  = items_orden_entrega.ITEM
                        oe_packing_contratos_lotes.id_calidad            = items_orden_entrega.id_calidad
                        oe_packing_contratos_lotes.calidad               = IF AVAILABLE calidades THEN calidades.descripcion_ingles ELSE "NULL"
                        oe_packing_contratos_lotes.id_articulo           = items_orden_entrega.id_articulo
                        oe_packing_contratos_lotes.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura_ingles ELSE "NULL"
                        oe_packing_contratos_lotes.tambores              = items_orden_entrega.cantidad_tambores
                        oe_packing_contratos_lotes.orden_fabricacion     = string(contratos.orden_fabricacion)
                        oe_packing_contratos_lotes.etd                   = orden_entrega.fecha_embarque
                        oe_packing_contratos_lotes.eta                   = orden_entrega.fecha_arribo
                        oe_packing_contratos_lotes.id_agencia            = orden_entrega.id_agencia
                        oe_packing_contratos_lotes.agencia               = IF AVAILABLE agencias THEN agencias.descripcion ELSE "NULL"
                        oe_packing_contratos_lotes.id_estado_oe          = orden_entrega.id_estado
                        oe_packing_contratos_lotes.estado_oe             = IF AVAILABLE estados_oe THEN estados_oe.descripcion ELSE "NULL"
                        oe_packing_contratos_lotes.semana_desde          = items_contratos.semana_entrega.
                    
                    FOR EACH packing_list WHERE packing_list.id_contrato      = items_contratos.id_contrato
                                            AND packing_list.id_tipo_contrato = items_contratos.id_tipo_contrato
                                            AND packing_list.anio             = items_contratos.anio
                                            AND packing_list.item_contrato    = items_contratos.ITEM
                                            NO-LOCK
                                            BREAK BY packing_list.ITEM_contrato.
                        FOR EACH items_packing_list WHERE items_packing_list.id_sucursal = packing_list.id_sucursal
                                                      AND items_packing_list.id_packing_list = packing_list.id_packing_list
                                                      AND integer(items_packing_list.nro_orden_embarque) = orden_entrega.id_orden_entrega
                                                    NO-LOCK
                                                    BREAK BY items_packing_list.nro_contenedor
                                                          BY id_lote.
                            v_tam_des = v_tam_des + items_packing_list.cantidad.
                            IF LAST-OF(id_lote) THEN
                            DO:
                                CREATE item_oe_packing_contrato_lote.
                                ASSIGN item_oe_packing_contrato_lote.id_orden_entrega   = orden_entrega.id_orden_entrega
                                    item_oe_packing_contrato_lote.item_oe               = items_orden_entrega.item_oe
                                    item_oe_packing_contrato_lote.nro_contenedor        = items_packing_list.nro_contenedor
                                    item_oe_packing_contrato_lote.nro_lote              = items_packing_list.nro_lote
                                    item_oe_packing_contrato_lote.tambores_despachados  = v_tam_des.
                                v_tam_des = 0.
                            END.
                        END.
                    END.
                    
                end.
                IF NOT tiene-oe THEN DO:
                    find destinos of items_contratos no-lock no-error.
                    find productos_terminados of items_contratos no-lock no-error.
                    find calidades of items_contratos no-lock no-error.

                    CREATE oe_packing_contratos_lotes.
                    ASSIGN oe_packing_contratos_lotes.id_destino     = items_contratos.id_destino
                    oe_packing_contratos_lotes.destino               = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NULL"
                    oe_packing_contratos_lotes.id_cliente            = items_contratos.id_cliente
                    oe_packing_contratos_lotes.cliente               = IF AVAILABLE clientes THEN clientes.nombre ELSE "NULL"
                    oe_packing_contratos_lotes.id_contrato           = items_contratos.id_contrato
                    oe_packing_contratos_lotes.id_tipo_contrato      = items_contratos.id_tipo_contrato
                    oe_packing_contratos_lotes.anio                  = items_contratos.anio
                    oe_packing_contratos_lotes.ITEM                  = items_contratos.ITEM
                    oe_packing_contratos_lotes.id_calidad            = items_contratos.id_calidad
                    oe_packing_contratos_lotes.calidad               = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NULL"
                    oe_packing_contratos_lotes.id_articulo           = items_contratos.id_articulo
                    oe_packing_contratos_lotes.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NULL"
                    oe_packing_contratos_lotes.tambores              = items_contratos.cantidad
                    oe_packing_contratos_lotes.orden_fabricacion     = string(contratos.orden_fabricacion)
                    oe_packing_contratos_lotes.semana_desde          = items_contratos.semana_entrega 
                        .
                        
                END.

                FIND contactos_industria WHERE contactos_industria.id_contacto = contratos.id_cliente_destino NO-LOCK NO-ERROR.
                IF AVAILABLE contactos_industria THEN DO:
                    ASSIGN oe_packing_contratos_lotes.id_cliente_final = contratos.id_cliente_destino
                           oe_packing_contratos_lotes.cliente_final    = contactos_industria.nombre.
                END.
    
                FIND contactos_industria WHERE contactos_industria.id_contacto = contratos.id_documentacion NO-LOCK NO-ERROR.
                IF AVAILABLE contactos_industria THEN DO:
                    ASSIGN oe_packing_contratos_lotes.id_documentacion = contratos.id_documentacion
                           oe_packing_contratos_lotes.documentacion    = contactos_industria.nombre.
                END.
                tiene-oe = FALSE.
