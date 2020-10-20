            FIND productos_terminados OF items_contratos NO-LOCK NO-ERROR.
            FIND calidades OF items_contratos NO-LOCK NO-ERROR.
            FIND destinos OF items_contratos NO-LOCK NO-ERROR.
            FIND zonas_clientes WHERE zonas_clientes.id_zona = destinos.id_zona NO-LOCK NO-ERROR.
            FIND clausulas WHERE items_contratos.id_clausula = clausulas.id_clausula NO-LOCK NO-ERROR.
            
            FIND contratos OF items_contratos NO-LOCK.
            FIND contactos_industria WHERE contactos_industria.id_contacto = contratos.id_cliente_destino
                                     NO-LOCK NO-ERROR.
                    
            FIND r_productos_calidad_envase OF items_contratos NO-LOCK NO-ERROR.
            IF AVAILABLE r_productos_calidad_envase THEN DO:
                    v_kilos_envase = r_productos_calidad_envase.kilos.
            END.
            ELSE v_kilos_envase = 250.
            
            v_cantidad_contratos = items_contratos.cantidad * v_kilos_envase.
              
            IF items_contratos.id_articulo = 52 OR
               items_contratos.id_articulo = 53 THEN DO:
                FIND r_productos_calidad WHERE r_productos_calidad.id_articulo = items_contratos.id_articulo 
                                           AND r_productos_calidad.id_calidad = items_contratos.id_calidad 
                                         NO-LOCK NO-ERROR.  
                v_coef = IF AVAILABLE r_productos_calidad THEN r_productos_calidad.coeficiente ELSE 1.

            END.
            ELSE
                v_coef = 1.
              

            IF v_coef >= 1.25 THEN
                v_pesoref = 260.
            ELSE
                v_pesoref = 250.

            v_cantidad_contratos_400 = items_contratos.cantidad * v_kilos_envase * v_coef.
            
            /*---------- GASTOS DEL CONTRATO --------*/

            FOR EACH r_gastos_items_contrato OF items_contratos NO-LOCK.
                case r_gastos_items_contrato.id_tipo_unidad_venta:
                    when 1 then /* TONELADAS SE MANEJA IGUAL QUE KILOS */
                        do:
                            v_gastos = v_gastos + r_gastos_items_contrato.importe / 1000.
                        end.
                    when 2 then /* KILOS */
                        do:
                            v_gastos = v_gastos + r_gastos_items_contrato.importe.
                        end.
                    when 3 then /* GALONES */                    
                        do:
                            v_gastos = v_gastos + r_gastos_items_contrato.importe * 53.6 / v_pesoref.                                
                        end.
                    when 4 then /* LIBRAS */
                        do:
                            v_gastos = v_gastos + r_gastos_items_contrato.importe * 2.20462.
                        end.
                end case.
            END.
           /*---------- GASTOS DEL CONTRATO --------*/

           /*---------- IMPORTE DE CONTRATO --------*/
            v_precio_fob = items_contratos.precio_origen.

            
                case items_contratos.id_tipo_unidad_venta_origen:
                    when 1 then /* TONELADAS SE MANEJA IGUAL QUE KILOS */
                        do:
                            v_precio = items_contratos.precio_origen / 1000.
                            v_precio_fob = v_precio_fob / 1000.
                        end.
                    when 2 then /* KILOS */
                        do:
                            v_precio = items_contratos.precio_origen.
                        end.
                    when 3 then /* GALONES */                    
                        do:
                            v_precio = items_contratos.precio_origen * 53.6 / v_pesoref.
                            v_precio_fob = v_precio_fob * 53.6  / v_pesoref .
                        end.
                    when 4 then /* LIBRAS */
                        do:
                            v_precio = items_contrato.precio_origen * 2.20462.
                            v_precio_fob = v_precio_fob * 2.20462.
                        end.
                END case.
                                        
                v_precio_fob = v_precio_fob - v_gastos.
           /*---------- IMPORTE DE CONTRATO --------*/

           /*---------- ORDEN DE ENTREGA Y LOTES --------------*/
                v_fob_ton = 0.
                v_lotes_contrato = "".
                v_orden_entrega = "".
                FOR EACH items_orden_entrega OF items_contratos WHERE fob_ton > 0 NO-LOCK.
                    v_fob_ton = v_fob_ton + items_orden_entrega.fob_ton.
                    v_orden_entrega = v_orden_entrega + " " + STRING(items_orden_entrega.id_orden_entrega).

                    FOR EACH lotes_jugo OF items_orden_entrega.
                       IF YEAR(lotes_jugo.fecha) = v_anio_desde - 1 THEN v_tiene_lote_anterior = TRUE.
                       v_lotes_contrato = v_lotes_contrato + " " + string(lotes_jugo.id_lote) + 
                                                             "/" + STRING(YEAR(lotes_jugo.fecha)).
                     END.
                     FOR EACH lotes_aceite OF items_orden_entrega.
                       IF YEAR(lotes_aceite.fecha) = v_anio_desde - 1 THEN v_tiene_lote_anterior = TRUE.
                       v_lotes_contrato = v_lotes_contrato + " " + string(lotes_aceite.id_lote) + 
                                                             "/" + STRING(YEAR(lotes_aceite.fecha)).
                     END.
                END.
            /*---------- ORDEN DE ENTREGA Y LOTES --------------*/

                IF v_fob_ton > 0 THEN v_precio_fob = v_fob_ton / v_cantidad_contratos.
                
            /*---------- COMISIONES -----------------*/
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
                v_importe_comision = v_importe_comision * ( v_porc_comision / 100 ).    

            /*---------- COMISIONES -----------------*/

                /*
                v_importe_fac_fob = 0.
                v_importe_fac_todos = 0.
        
                v_cantidad_pl = 0.
                FIND clausulas WHERE items_contratos.id_clausula = clausulas.id_clausula NO-LOCK NO-ERROR.
                FOR EACH packing_list WHERE packing_list.id_contrato      = items_contratos.id_contrato
                                        AND packing_list.id_tipo_contrato = items_contratos.id_tipo_contrato
                                        AND packing_list.anio             = items_contratos.anio
                                        AND packing_list.item_contrato    = items_contratos.ITEM
                                        NO-LOCK
                                        BREAK BY packing_list.ITEM_contrato.
                  v_nro_pack_list = v_nro_pack_list + packing_list.nro_pack_list. 
                  FIND vapores OF packing_list NO-LOCK NO-ERROR.
                  FIND destinos WHERE destinos.id_destino = packing_list.id_destino_grupo NO-LOCK NO-ERROR.
                    
                  FOR EACH items_packing_list OF packing_list NO-LOCK,
                          FIRST r_items_venta_pack_list WHERE 
                                  r_items_venta_pack_list.id_sucursal = items_packing_list.id_sucursal AND
                                  r_items_venta_pack_list.id_packing_list = items_packing_list.id_packing_list AND
                                  r_items_venta_pack_list.ITEM_pack = items_packing_list.ITEM 
                                  NO-LOCK BREAK BY r_items_venta_pack_list.ITEM.
                        
                      v_lotes = v_lotes + " " + items_packing_list.nro_lote.
        
                      v_nro_contenedor = v_nro_contenedor + " " + items_packing_list.nro_contenedor.
                      IF LAST-OF(r_items_venta_pack_list.ITEM) THEN DO:
                          FIND FIRST ITEMs_venta OF r_items_venta_pack_list NO-LOCK NO-ERROR.
                          IF AVAILABLE items_venta THEN DO:
                                v_importe_item_fac_fob = (v_importe_item_fac_fob + 
                                                         (items_venta.precio_origen * items_venta.cantidad)).
                                   
                                v_importe_item_fac_todos = v_importe_item_fac_todos + 
                                     ((items_venta.precio_origen + items_venta.gastos) * items_venta.cantidad).
        
                              FIND FIRST subd_vtas OF items_venta NO-LOCK NO-ERROR.
                              IF AVAILABLE subd_vtas THEN DO:
                                      
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
                      
                      v_precio_fob = v_importe_item_fac_fob / v_cantidad_contratos.
                      v_cantidad_pl = v_cantidad_pl + items_packing_list.kilos.
                        
                END.  /* END DEL ITEM PACKING LIST */
                
                          
           END.  /* END DEL PACKING LIST */
                  */
           
           v_importe_contrato = v_cantidad_contratos  * v_precio.
           v_importe_contrato_fob = v_cantidad_contratos  * v_precio_fob.
           
           v_importe_contrato_kilos = v_importe_contrato / v_cantidad_contratos.
           v_importe_contrato_kilos_400 = v_importe_contrato / v_cantidad_contratos_400.

           v_importe_contrato_kilos_fob = v_importe_contrato_fob / v_cantidad_contratos.
           v_importe_contrato_kilos_fob_400 = v_importe_contrato_fob / v_cantidad_contratos_400.
           
          FIND clientes OF contratos NO-LOCK NO-ERROR.
          IF AVAILABLE clientes THEN DO:
               /*DISP contratos.id_contrato.*/
              {..\industria\generaTTContratos.i}

          END.
              /*
          ELSE
              DISP contratos.id_contrato.*/
    
       
    
        
   
            v_nro_contenedor = "".
            v_importe_fac_fob = 0.
            v_importe_fac_todos = 0.
            v_cantidad_contratos = 0.
            v_cantidad_contratos_400 = 0.
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
            v_importe_contrato_fob = 0.
            v_importe_contrato_kilos = 0.
            v_importe_contrato_kilos_400 = 0.
            v_importe_contrato_kilos_fob = 0.
            v_importe_contrato_kilos_fob_400 = 0.
            v_gastos                         = 0.
            v_lotes_contrato = "".
            v_coef = 1.
            v_cantidad_pl_400 = 0.
