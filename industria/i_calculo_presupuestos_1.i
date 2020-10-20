    FIND productos_terminados OF items_contratos NO-LOCK NO-ERROR.
    FIND calidades OF items_contratos NO-LOCK NO-ERROR.
    FIND tipos_plazo OF contratos NO-LOCK NO-ERROR.
    FIND instrumentos_pagos WHERE instrumentos_pagos.id_instrumento_pago =
                                  contratos.id_instrumento_pago NO-LOCK NO-ERROR.

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
      
      IF items_contratos.id_articulo = 52 OR 
         items_contratos.id_articulo = 53 OR
         items_contratos.id_articulo = 71 THEN DO:
          FIND r_productos_calidad_envase OF items_contratos NO-LOCK NO-ERROR.
          IF AVAILABLE r_productos_calidad_envase THEN DO:
              v_kilos_envase = r_productos_calidad_envase.kilos.
              v_importe_fob_contrato = ( ( items_contrato.cantidad * v_kilos_envase ) / 1000 ) * items_contrato.precio_base_calculo.
          END.
      END.
      ELSE
      DO:
          FIND r_envases_prod OF items_contratos NO-LOCK NO-ERROR.
          IF AVAILABLE r_envases_prod THEN DO:
              v_kilos_envase = r_envases_prod.kilos.
              v_importe_fob_contrato = ( items_contrato.cantidad * v_kilos_envase ) * items_contrato.precio_base_calculo.
          END.
      END.
      
      v_cantidad_contratos = items_contratos.cantidad * v_kilos_envase.    

      /* PARA CALCULAR LOS PRECIOS DE VENTA DE CONTRATO */
      case items_contratos.id_tipo_unidad_venta_origen:
                    when 1 then /* TONELADAS SE MANEJA IGUAL QUE KILOS */
                        do:
                            v_importe_contrato = (items_contratos.cantidad * v_kilos_envase * (items_contrato.precio_origen / 1000)).
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
      
      tiene-oe = FALSE.
      FOR EACH orden_entrega OF items_contratos NO-LOCK.
          v_importe_fob_pl = orden_entrega.fob_ton.
          
          v_importe_fac = 0.
          v_cantidad_pl = 0.
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
                               END.
                      END.
                   END.
               END.
        
             FIND clientes OF contratos NO-LOCK NO-ERROR.
             IF AVAILABLE clientes THEN
             DO:
                {..\industria\i_calculo_presupuestos_2.i}
                tiene-oe = TRUE.
             END.
             ELSE
                 DISP contratos.id_contrato.
    
            
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
                            END.
                    END.                 
                END.
                v_cantidad_pl = v_cantidad_pl + items_packing_list.kilos.
            END.                                                         
          END.
          FIND clientes OF contratos NO-LOCK NO-ERROR.
          IF AVAILABLE clientes THEN DO:
              {..\industria\i_calculo_presupuestos_2.i}
          END.
      END.
    END.
    v_importe_fob_pl = 0.
    v_importe_fob_contrato = 0.
    v_importe_comision = 0.
    v_importe_contrato = 0.
    v_importe_fac = 0.
    v_cantidad_contratos = 0.
    v_cantidad_pl = 0.
    v_factura = "".
    v_item_fac = 0.
    v_nro_pack_list = "".
    v_importe_item_fac = 0.
    v_fecha_fac = ?.
    v_fecha_vto = ?.
    v_lotes = "".
    gall = 0.
    gallx = 0.
