DEFINE INPUT PARAMETER p_cliente AS INTEGER.
DEFINE INPUT PARAMETER p_fecha AS DATE.
define INPUT PARAMETER v_semana_desde as integer.
define INPUT PARAMETER v_anio_desde as integer.
define INPUT PARAMETER v_semana_hasta as integer.
define INPUT PARAMETER v_anio_hasta as integer.

DEFINE VAR v_importe_fac_fob AS DECIMAL.
DEFINE VAR v_importe_fac_todos AS DECIMAL.
DEFINE VAR v_importe_item_fac_fob AS DECIMAL.
DEFINE VAR v_importe_item_fac_todos AS DECIMAL.
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
DEFINE VAR v_nro_contenedor AS CHAR.
DEFINE VAR v_cobranzas AS CHAR.
DEFINE VAR v_fechas_cobranzas AS CHAR.
DEFINE VAR v_total_cobranzas AS DECIMAL.
DEFINE VAR i AS INTEGER.
DEFINE VAR v_consignacion AS LOGICAL FORMAT "SI/NO" INITIAL "NO".
DEFINE VAR v_tiene_lote_anterior AS LOGICAL INITIAL FALSE.
DEFINE VAR v_importe_contrato AS DECIMAL.


DEFINE VAR tiene-oe AS LOGICAL INITIAL FALSE.



FOR EACH contratos WHERE YEAR(contratos.fecha) = v_anio_desde 
                     AND contratos.id_tipo_contrato < 100 NO-LOCK.
    FOR EACH items_contratos OF contratos NO-LOCK.
            FIND productos_terminados OF items_contratos NO-LOCK NO-ERROR.
            FIND calidades OF items_contratos NO-LOCK NO-ERROR.
            FIND tipo_moneda WHERE tipo_moneda.id_moneda = items_contratos.id_moneda_origen NO-LOCK NO-ERROR.
            FIND tipo_unidad_venta WHERE tipo_unidad_venta.id_tipo_unidad_venta = 
                                     items_contratos.id_tipo_unidad_venta_origen NO-LOCK NO-ERROR.
        
            FIND contactos_industria WHERE contactos_industria.id_contacto = contratos.id_cliente_destino
                                     NO-LOCK NO-ERROR.
                    
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
                    
              /* PARA CALCULAR LOS IMPORTE DE CONTRATO */
                    case items_contratos.id_tipo_unidad_venta_origen:
                        when 1 then /* TONELADAS SE MANEJA IGUAL QUE KILOS */
                            do:
                                v_importe_contrato = (((items_contratos.cantidad * v_kilos_envase) / 1000) * items_contrato.precio_origen).
                            end.
                        when 2 then /* KILOS */
                            do:
                                v_importe_contrato = (items_contratos.cantidad * v_kilos_envase * items_contrato.precio_origen).
                            end.
                        when 3 then /* GALONES */                    
                            do:
                                v_importe_contrato = (items_contratos.cantidad * 53.6 * items_contrato.precio_origen).
                            end.
                        when 4 then /* LIBRAS */
                            do:
                                v_importe_contrato = ((items_contratos.cantidad * v_kilos_envase * 2.20462) * items_contrato.precio_origen).
                            end.
                    end case.

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
              FOR EACH orden_entrega OF items_contratos NO-LOCK.
        
                     FIND destinos OF orden_entrega NO-LOCK NO-ERROR.
                     FIND vapores OF orden_entrega NO-LOCK NO-ERROR.
                     FIND estados_oe OF orden_entrega NO-LOCK NO-ERROR.
                     FIND clausulas WHERE orden_entrega.id_condicion_venta = clausulas.id_clausula NO-LOCK NO-ERROR.
                     v_lotes = "".
                
                     v_importe_fac_fob = 0.
                     v_importe_fac_todos = 0.
                     v_cantidad_pl = 0.
                     v_nro_pack_list = "".
                     v_importe_item_fac_fob = 0.
                     v_importe_item_fac_todos = 0.
                     FOR EACH packing_list WHERE packing_list.id_contrato      = items_contratos.id_contrato
                                             AND packing_list.id_tipo_contrato = items_contratos.id_tipo_contrato
                                             AND packing_list.anio             = items_contratos.anio
                                             NO-LOCK,
                       EACH items_packing_list OF packing_list WHERE items_packing_list.nro_orden_embarque = 
                               orden_entrega.id_orden_entrega NO-LOCK 
                               BREAK BY packing_list.nro_pack_list
                                     BY items_packing_list.nro_lote:
                         v_nro_contenedor = v_nro_contenedor + " " + items_packing_list.nro_contenedor.
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
                                   v_importe_item_fac_fob = (v_importe_item_fac_fob + 
                                                             (items_venta.precio_origen * items_venta.cantidad)).
                                   
                                   v_importe_item_fac_todos = v_importe_item_fac_todos + 
                                        ((items_venta.precio_origen + items_venta.gastos) * items_venta.cantidad).
        
                                   FIND FIRST subd_vtas OF items_venta NO-LOCK NO-ERROR.
                                   IF AVAILABLE subd_vtas THEN
                                       DO:
                                           /* v_importe_fac_fob = subd_vtas.importe_origen. */
                                           RUN p_fob_fac.p (INPUT ROWID(subd_vtas), 
                                                            OUTPUT v_importe_fac_fob,
                                                            OUTPUT v_importe_fac_todos ).
                                           RUN getCobranzasIndust.p (INPUT ROWID(subd_vtas), 
                                                                     OUTPUT v_fechas_cobranzas,
                                                                     OUTPUT v_cobranzas).
                                           IF subd_vtas.id_tipo_venta = 3 THEN DO:
                                               v_consignacion = TRUE.
                                           END.
                                           v_factura = string(subd_vtas.id_punto_venta,"9999") + STRING(subd_vtas.nro_comp,"99999999").
                                           v_fecha_fac = subd_vtas.fecha_comp.
                                           v_fecha_vto = subd_vtas.vencimiento.
                                       END.
                              END.
                           END.
                       END.
                
                
                     FOR EACH lotes_jugo OF orden_entrega.
                       IF YEAR(lotes_jugo.fecha) = v_anio_desde - 1 THEN v_tiene_lote_anterior = TRUE.
                       v_lotes = v_lotes + " " + string(lotes_jugo.id_lote) + "/" + STRING(YEAR(lotes_jugo.fecha)).
                     END.
                     FOR EACH lotes_aceite OF orden_entrega.
                       IF YEAR(lotes_aceite.fecha) = v_anio_desde - 1 THEN v_tiene_lote_anterior = TRUE.
                       v_lotes = v_lotes + " " + string(lotes_aceite.id_lote) + "/" + STRING(YEAR(lotes_aceite.fecha)).
                     END.

                     /* 
                       FOR EACH tambores_industria WHERE tambores_industria.id_contrato_of     = items_contratos.id_contrato
                                            AND tambores_industria.id_tipocontrato_of = items_contratos.id_tipo_contrato
                                            AND tambores_industria.anio_of            = items_contratos.anio
                                            AND tambores_industria.item_of            = items_contratos.ITEM
                                            NO-LOCK
                                            BREAK BY tambores_industria.id_lote.

                IF LAST-OF(tambores_industria.id_lote) THEN DO:
                    IF YEAR(tambores_industria.fecha) = v_anio_desde - 1 THEN v_tiene_lote_anterior = TRUE.
                       v_lotes_contrato = v_lotes_contrato + " " + string(tambores_industria.id_lote) + 
                                                             "/" + STRING(YEAR(tambores_industria.fecha)).
                END.
              END.
                     */
                     
                     FIND clientes OF contratos NO-LOCK NO-ERROR.
                     IF AVAILABLE clientes THEN
                     DO:
                        {..\industria\i_calculo_re_cial_completo2.i}
                        tiene-oe = TRUE.
                     END.
                     ELSE
                         DISP contratos.id_contrato.
            
                    
              END.
        
              IF NOT tiene-oe THEN
              DO:
                  v_importe_fac_fob = 0.
                  v_importe_fac_todos = 0.
        
                  v_cantidad_pl = 0.
                  FIND vapores OF items_contratos NO-LOCK NO-ERROR.
                  FIND clausulas WHERE items_contratos.id_clausula = clausulas.id_clausula NO-LOCK NO-ERROR.
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
        
                        v_nro_contenedor = v_nro_contenedor + " " + items_packing_list.nro_contenedor.
                        IF LAST-OF(r_items_venta_pack_list.ITEM) THEN
                        DO:
                            FIND FIRST ITEMs_venta OF r_items_venta_pack_list NO-LOCK NO-ERROR.
                            IF AVAILABLE items_venta THEN
                            DO:
                                v_importe_item_fac_fob = (v_importe_item_fac_fob + 
                                                             (items_venta.precio_origen * items_venta.cantidad)).
                                   
                                   v_importe_item_fac_todos = v_importe_item_fac_todos + 
                                        ((items_venta.precio_origen + items_venta.gastos) * items_venta.cantidad).
        
                                FIND FIRST subd_vtas OF items_venta NO-LOCK NO-ERROR.
                                IF AVAILABLE subd_vtas THEN
                                    DO:
                                        /* v_importe_fac_fob = subd_vtas.importe_origen. */
                                           RUN p_fob_fac.p (INPUT ROWID(subd_vtas), 
                                                            OUTPUT v_importe_fac_fob,
                                                            OUTPUT v_importe_fac_todos ).
        
                                           RUN getCobranzasIndust.p (INPUT ROWID(subd_vtas), 
                                                            OUTPUT v_fechas_cobranzas,
                                                            OUTPUT v_cobranzas).
        
                                           IF subd_vtas.id_tipo_venta = 3 THEN DO:
                                               v_consignacion = TRUE.
                                           END.
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
                      {..\industria\i_calculo_re_cial_completo2.i}
                  END.
                  ELSE
                      DISP contratos.id_contrato.
              END.
               
            v_nro_contenedor = "".
            v_importe_fac_fob = 0.
            v_importe_fac_todos = 0.
            v_cantidad_contratos = 0.
            v_cantidad_pl = 0.
            v_factura = "".
            v_item_fac = 0.
            v_nro_pack_list = "".
            v_importe_item_fac_fob = 0.
            v_importe_item_fac_todos = 0.
            v_fecha_fac = ?.
            v_fecha_vto = ?.
            v_lotes = "".
            gall = 0.
            gallx = 0.
            v_consignacion = FALSE.
            v_cobranzas = "".
            v_fechas_cobranzas = "".
            v_total_cobranzas = 0.
            v_tiene_lote_anterior = FALSE.
            v_importe_contrato = 0.
    END.
END.
