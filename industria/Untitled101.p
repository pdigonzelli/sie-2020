DEFINE VAR p_cliente AS INTEGER.
DEFINE VAR p_fecha AS DATE.

DEFINE VAR v_importe_fac AS DECIMAL.
DEFINE VAR v_importe_item_fac AS DECIMAL.
DEFINE VAR v_cantidad_contratos AS DECIMAL.
DEFINE VAR v_importe_comision AS DECIMAL.
DEFINE VAR v_porc_comision AS DECIMAL.
DEFINE VAR v_cantidad_pl AS DECIMAL.
DEFINE VAR v_factura AS CHAR.
DEFINE VAR v_item_fac AS INTEGER.
DEFINE VAR v_lotes AS CHAR.
DEFINE VAR v_nro_pack_list AS CHAR.
DEFINE VAR v_fecha_fac AS DATE.
DEFINE VAR v_fecha_vto AS DATE.
DEFINE VAR gall         AS DECIMAL.
DEFINE VAR gallx        AS DECIMAL.
DEFINE VAR v_kilos_envase AS DECIMAL.
define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.

DEFINE VAR tiene-oe AS LOGICAL INITIAL FALSE.

FOR EACH items_contratos NO-LOCK WHERE id_contrato = "ERJ0014".

    FIND productos_terminados OF items_contratos NO-LOCK NO-ERROR.
    FIND calidades OF items_contratos NO-LOCK NO-ERROR.
    FIND tipo_moneda WHERE tipo_moneda.id_moneda = items_contratos.id_moneda_origen NO-LOCK NO-ERROR.
    FIND tipo_unidad_venta WHERE tipo_unidad_venta.id_tipo_unidad_venta = 
                             items_contratos.id_tipo_unidad_venta_origen NO-LOCK NO-ERROR.

    IF p_cliente > 0 THEN
        FIND contratos WHERE contratos.id_contrato      = items_contratos.id_contrato
                         AND contratos.id_tipo_contrato = items_contratos.id_tipo_contrato
                         AND contratos.anio             = items_contratos.anio
                         AND contratos.id_cliente       = p_cliente NO-LOCK NO-ERROR. 
    ELSE
        FIND contratos OF items_contratos NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN
    DO:
      FIND contactos_industria WHERE contactos_industria.id_contacto = contratos.id_cliente_destino
                               NO-LOCK NO-ERROR.
      /*FIND r_envases_prod OF items_contratos NO-LOCK NO-ERROR.
        IF AVAILABLE r_envases_prod THEN DO:*/
            
      IF items_contratos.id_articulo = 52 OR 
         items_contratos.id_articulo = 53 OR
         items_contratos.id_articulo = 71 THEN DO:
          FIND r_productos_calidad_envase OF items_contratos NO-LOCK NO-ERROR.
          IF AVAILABLE r_productos_calidad_envase THEN DO:
              v_kilos_envase = r_productos_calidad_envase.kilos.
          END.
      END.
      ELSE
      DO:
          FIND r_envases_prod OF items_contratos NO-LOCK NO-ERROR.
          IF AVAILABLE r_envases_prod THEN DO:
              v_kilos_envase = r_envases_prod.kilos.
          END.
      END.    
      
      v_cantidad_contratos = items_contratos.cantidad * v_kilos_envase.
            
            /* LO MISMO PERO PARA CALCULAR LAS COMISIONES */
            case items_contratos.id_tipo_unidad_venta:
                          when 1 then /* TONELADAS SE MANEJA IGUAL QUE KILOS */
                              do:
                                  v_importe_comision = (items_contratos.cantidad * v_kilos_envase * (items_contrato.precio_comision / 1000)).
                              end.
                          when 2 then /* KILOS */
                              do:
                                  v_importe_comision = (items_contratos.cantidad * v_kilos_envase * items_contrato.precio_comision).
                              end.
                          when 3 then /* GALONES */                    
                              do:
                                  v_importe_comision = (items_contratos.cantidad * 53.6 * items_contrato.precio_comision).
                              end.
                          when 4 then /* LIBRAS */
                              do:
                                  v_importe_comision = ((items_contratos.cantidad * v_kilos_envase * 2.20462) * items_contrato.precio_comision).
                              end.
            end case.
            v_porc_comision = items_contratos.comision_broker.
        
        /* END. */
      tiene-oe = FALSE.
      MESSAGE "afuera de OE " items_contratos.id_contrato items_contratos.ITEM VIEW-AS ALERT-BOX.
      FOR EACH orden_entrega OF items_contratos NO-LOCK.

             FIND destinos OF orden_entrega NO-LOCK NO-ERROR.
             FIND estados_oe OF orden_entrega NO-LOCK NO-ERROR.
             v_lotes = "".
        
             v_importe_fac = 0.
             v_cantidad_pl = 0.
             v_nro_pack_list = "".
             v_importe_item_fac = 0.
             FOR EACH packing_list WHERE packing_list.id_contrato      = items_contratos.id_contrato
                                     AND packing_list.id_tipo_contrato = items_contratos.id_tipo_contrato
                                     AND packing_list.anio             = items_contratos.anio
                                     NO-LOCK,
               EACH items_packing_list OF packing_list WHERE items_packing_list.nro_orden_embarque = 
                       orden_entrega.id_orden_entrega NO-LOCK 
                       BREAK BY packing_list.nro_pack_list
                             BY items_packing_list.nro_lote:
                 
                 v_cantidad_pl = v_cantidad_pl + items_packing_list.kilos.
                 
                 IF FIRST-OF(items_packing_list.nro_lote) THEN DO:
                    v_nro_pack_list = v_nro_pack_list + packing_list.nro_pack_list. 
                    FIND FIRST r_items_venta_pack_list WHERE 
                               r_items_venta_pack_list.id_sucursal = items_packing_list.id_sucursal AND
                               r_items_venta_pack_list.id_packing_list = items_packing_list.id_packing_list AND
                               r_items_venta_pack_list.ITEM_pack = items_packing_list.ITEM 
                               NO-LOCK NO-ERROR.
                       FIND FIRST ITEMs_venta OF r_items_venta_pack_list NO-LOCK NO-ERROR.
                       IF AVAILABLE items_venta THEN
                       DO:
                           v_importe_item_fac = v_importe_item_fac + 
                                           ((items_venta.precio_origen + items_venta.gastos) * items_venta.cantidad).
                           
                           FIND FIRST subd_vtas OF items_venta NO-LOCK NO-ERROR.
                           IF AVAILABLE subd_vtas THEN
                               DO:
                                   v_importe_fac = subd_vtas.importe_origen.
                                   v_factura = string(subd_vtas.id_punto_venta,"9999") + STRING(subd_vtas.nro_comp,"99999999").
                                   v_fecha_fac = subd_vtas.fecha_comp.
                                   v_fecha_vto = subd_vtas.vencimiento.
                               END.
                      END.
                   END.
               END.
        
        
             FOR EACH lotes_jugo OF orden_entrega.
               v_lotes = v_lotes + " " + string(lotes_jugo.id_lote) + "/" + STRING(YEAR(lotes_jugo.fecha)).
             END.
             FOR EACH lotes_aceite OF orden_entrega.
               v_lotes = v_lotes + " " + string(lotes_aceite.id_lote) + "/" + STRING(YEAR(lotes_aceite.fecha)).
             END.
             
             MESSAGE items_contratos.id_contrato items_contratos.ITEM VIEW-AS ALERT-BOX.
             FIND clientes OF contratos NO-LOCK NO-ERROR.
             IF AVAILABLE clientes THEN
             DO:
                MESSAGE "adentro de clientes" items_contratos.id_contrato items_contratos.ITEM VIEW-AS ALERT-BOX.
                tiene-oe = TRUE.
             END.
             ELSE
                 DISP contratos.id_contrato.
    
            
      END.
    END.
    IF NOT tiene-oe THEN
      DO:
          v_importe_fac = 0.
          v_cantidad_pl = 0.
          FOR EACH packing_list WHERE packing_list.id_contrato      = items_contratos.id_contrato
                                  AND packing_list.id_tipo_contrato = items_contratos.id_tipo_contrato
                                  AND packing_list.anio             = items_contratos.anio
                                  AND packing_list.item_contrato    = items_contratos.ITEM
                                  NO-LOCK
                                  BREAK BY packing_list.ITEM_contrato.
            v_nro_pack_list = v_nro_pack_list + packing_list.nro_pack_list. 
            FOR EACH items_packing_list OF packing_list NO-LOCK,
                    FIRST r_items_venta_pack_list WHERE 
                            r_items_venta_pack_list.id_sucursal = items_packing_list.id_sucursal AND
                            r_items_venta_pack_list.id_packing_list = items_packing_list.id_packing_list AND
                            r_items_venta_pack_list.ITEM_pack = items_packing_list.ITEM 
                            NO-LOCK BREAK BY r_items_venta_pack_list.ITEM.
                IF LAST-OF(r_items_venta_pack_list.ITEM) THEN
                DO:
                    FIND FIRST ITEMs_venta OF r_items_venta_pack_list NO-LOCK NO-ERROR.
                    IF AVAILABLE items_venta THEN
                    DO:
                        v_importe_item_fac = v_importe_item_fac + 
                                        ((items_venta.precio_origen + items_venta.gastos) * items_venta.cantidad).
                        
                        FIND FIRST subd_vtas OF items_venta NO-LOCK NO-ERROR.
                        IF AVAILABLE subd_vtas THEN
                            DO:
                                v_importe_fac = subd_vtas.importe_neto_origen.
                                v_factura = string(subd_vtas.id_punto_venta,"9999") + STRING(subd_vtas.nro_comp,"99999999").
                                v_fecha_fac = subd_vtas.fecha_comp.
                                v_fecha_vto = subd_vtas.vencimiento.
                                /* MESSAGE v_factura subd_vtas.estado VIEW-AS ALERT-BOX. */
                            END.
                    END.                 
                END.
                v_cantidad_pl = v_cantidad_pl + items_packing_list.kilos.                                          
            
            END.
            
          END.
          FIND clientes OF contratos NO-LOCK NO-ERROR.
          IF AVAILABLE clientes THEN DO:
             MESSAGE "adentro de clientes dos" items_contratos.id_contrato items_contratos.ITEM VIEW-AS ALERT-BOX.
          END.
          ELSE
              DISP contratos.id_contrato.
      END.

END.
