DEFINE var r as rowid no-undo.
DEFINE var gall as decimal no-undo.
DEFINE var v_kilos as decimal no-undo.
DEFINE var v_kilos_brutos as decimal no-undo.
DEFINE var v_precio_total as decimal no-undo.
DEFINE var v_cantidad_tambores as integer no-undo.
DEFINE var v_gall_brix like inspecciones_lote.bx_correg no-undo.
DEFINE var v_id_orden_entrega like orden_entrega.id_orden_entrega no-undo.
DEFINE VAR v_item_oe AS INTEGER.
DEFINE var h_con as handle.
DEFINE var r_item_oe as rowid.
DEFINE VAR v_comision AS DECIMAL.
DEFINE VAR v_porc_comision AS DECIMAL.
DEFINE BUFFER bb_item_oe FOR items_orden_entrega.

RUN get-container (output h_con). /* BUSCO EL HANDLE DE LA VENTANA CONTENEDORA DE SMART-OBJECTS */
RUN get-rowid-item-oe in h_con (output r_item_oe). /* BUSCO EL ROWID DE LA TABLA ITEM_ORDEN_ENTREGA */



FIND FIRST bb_item_oe WHERE ROWID(bb_item_oe) = r_item_oe NO-ERROR.
IF AVAILABLE bb_item_oe THEN DO:
    
    FIND items_contratos WHERE items_contratos.id_contrato      = bb_item_oe.id_contrato
                           AND items_contratos.id_tipo_contrato = bb_item_oe.id_tipo_contrato
                           AND items_contratos.anio             = bb_item_oe.anio
                           AND items_contratos.item             = bb_item_oe.item
                           NO-LOCK NO-ERROR.
    IF AVAILABLE items_contratos THEN DO:
           FIND contratos of items_contratos no-lock no-error.
           IF AVAILABLE contratos THEN DO:
               IF bb_item_oe.modo_actualizacion THEN DO:
                   /* MODO AUTOMATICO, ES DECIR TOMA INFORMACION DE TAMBORES RELACIONADOS */
                  v_id_orden_entrega = bb_item_oe.id_orden_entrega.
                  v_item_oe          = bb_item_oe.ITEM_oe.
                  RUN p_calcular-brix_item_oe.p (INPUT v_id_orden_entrega,
                                                 INPUT v_item_oe,
                                                 OUTPUT v_kilos,
                                                 OUTPUT v_kilos_brutos,
                                                 OUTPUT v_cantidad_tambores,
                                                 OUTPUT gall,
                                                 OUTPUT v_gall_brix).
                  
                  general.items_orden_entrega.cantidad_tambores:screen-value in frame F-Main      = string(v_cantidad_tambores).
                  general.items_orden_entrega.kgs_netos_tambores:screen-value in frame F-Main     = string(v_kilos).
                  general.items_orden_entrega.kgs_brutos_tambores:screen-value in frame F-Main    = string(v_kilos_brutos).
    
                  CASE items_contratos.id_tipo_unidad_venta_origen: /*verificar donde puta se carga este dato*/
                    WHEN 1 then /* TONELADAS */ DO:
                        v_precio_total = ((v_kilos / 1000) * items_contratos.precio_origen).
                    END.
                    WHEN 2 then /* KILOS */ DO:
                        v_precio_total = (v_kilos * items_contratos.precio_origen).
                    END.
                    WHEN 3 then /* GALONES */ DO:
                        IF items_contrato.id_articulo = 52 OR
                           items_contrato.id_articulo = 53 OR 
                           items_contrato.id_articulo = 71 THEN DO:       
                            general.items_orden_entrega.grados_brix:screen-value in frame F-Main = string(v_gall_brix).                               
                            general.items_orden_entrega.total_galones:screen-value in frame F-Main          = string(gall).
                        END.
                        v_precio_total = (gall * items_contratos.precio_origen).
                    END.
                    WHEN 4 THEN /* LIBRAS */ DO:
                        v_precio_total = ((v_kilos * 2.20462) * items_contratos.precio_origen).
                    END.
                  END CASE. 
                  /* MESSAGE v_precio_total VIEW-AS ALERT-BOX. */
                  general.items_orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total).
               END.
               ELSE DO:
                   /* MODO MANUAL, TOMA LOS DATOS CARGADOS EN LA OE */
                   v_id_orden_entrega = bb_item_oe.id_orden_entrega.
                   v_item_oe          = bb_item_oe.ITEM_oe.
                   v_cantidad_tambores  = INTEGER(general.items_orden_entrega.cantidad_tambores:screen-value in frame F-Main).
                   v_gall_brix          = DECIMAL(general.items_orden_entrega.grados_brix:screen-value in frame F-Main).

                   FIND FIRST r_productos_calidad_envase WHERE r_productos_calidad_envase.id_articulo = items_contrato.id_articulo
                                                           AND r_productos_calidad_envase.id_calidad = items_contrato.id_calidad
                                                           AND r_productos_calidad_envase.id_envase  = items_contrato.id_envase
                                                            NO-LOCK NO-ERROR.
                   IF AVAILABLE r_productos_calidad_envase THEN DO:
                        v_kilos              = v_cantidad_tambores * r_productos_calidad_envase.Kilos.
                        FIND FIRST envases_prod WHERE envases_prod.id_envase = items_contratos.id_envase
                                                  NO-LOCK NO-ERROR.
                        IF AVAILABLE envases_prod THEN DO:
                            v_kilos_brutos       = v_kilos + (v_cantidad_tambores * envases_prod.tara).
                        END.
                   END.

                   general.items_orden_entrega.kgs_netos_tambores:screen-value in frame F-Main = STRING(v_kilos).
                   general.items_orden_entrega.kgs_brutos_tambores:screen-value in frame F-Main = STRING(v_kilos_brutos).
                   
                   CASE items_contratos.id_tipo_unidad_venta_origen:
                    WHEN 1 THEN /* TONELADAS */ DO:
                        v_precio_total = ((v_kilos / 1000) * items_contrato.precio_origen).
                    END.
                    WHEN 2 THEN /* KILOS */ DO:
                        v_precio_total = (v_kilos * items_contrato.precio_origen).
                    END.
                    WHEN 3 THEN /* GALONES */ DO:
                        v_gall_brix = DECIMAL(general.items_orden_entrega.grados_brix:screen-value in frame F-Main).                               
                        FIND LAST brix WHERE brix.brix <= ROUND(v_gall_brix,1) NO-LOCK NO-ERROR.
                            IF AVAILABLE brix THEN DO:
                                gall = round((v_kilos / brix.pe) / 3.785 , 2).
                                general.items_orden_entrega.total_galones:screen-value in frame F-Main = STRING(gall).
                                v_precio_total = (gall * items_contrato.precio_origen).
                            END.
                    END.
                    WHEN 4 THEN /* LIBRAS */ DO:
                        v_precio_total = ((v_kilos * 2.20462) * items_contrato.precio_origen).
                    END.
                  END case. 
                  general.items_orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total).
                  
               END.       
            END.
            ELSE message "No se encontro el contratos " view-as alert-box.
        END.
        ELSE message "No se encontro el item de contrato " view-as alert-box.

    /* SE CALCULA SIEMPRE LOS GASTOS SIN IMPORTAR EL MODO DE ACTUALIZACION*/
    run calculo-gastos (input bb_item_oe.id_orden_entrega, 
                        INPUT INTEGER(items_orden_entrega.ITEM_oe:SCREEN-VALUE IN FRAME F-Main),
                        input integer(items_orden_entrega.id_condicion_venta:screen-value in frame F-Main)).
    /* LLAMA AL PROGRAMA i_calculo_gastos_item_oe.i */
    
    IF bb_item_oe.id_condicion_venta = 22 THEN DO:
        FIND FIRST gastos_items_orden_entrega OF bb_item_oe
                                              WHERE gastos_items_orden_entrega.id_gasto = 13
                                              NO-LOCK NO-ERROR.
        IF AVAILABLE gastos_items_orden_entrega THEN DO:
            /* MESSAGE v_precio_total gastos_items_orden_entrega.importe VIEW-AS ALERT-BOX. */
            general.items_orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total - gastos_items_orden_entrega.importe).
        END.
    END.
    

    message "Se actualizaron los datos satisfactoriamente!" view-as alert-box.

END.

