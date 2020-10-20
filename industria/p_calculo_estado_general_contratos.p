DEFINE INPUT PARAMETER p_cliente AS INTEGER.
DEFINE INPUT PARAMETER p_fecha AS DATE.

DEFINE VAR v_tambores AS INTEGER.
DEFINE VAR v_kilos_tambores AS DECIMAL.
DEFINE VAR v_tam_despachados AS INTEGER.
DEFINE VAR v_kilos_tam_despachados AS DECIMAL.
DEFINE VAR v_tam_stock AS INTEGER.
DEFINE VAR v_kilos_tam_stock AS DECIMAL.
DEFINE VAR v_importe_fac AS DECIMAL.
DEFINE VAR v_importe_cob AS DECIMAL.
DEFINE VAR v_bultos_fac AS INTEGER.
DEFINE VAR v_importe_oe AS DECIMAL.
DEFINE VAR v_factura AS CHAR.
DEFINE VAR v_item_fac AS INTEGER.
DEFINE VAR v_nro_pack_list AS CHAR.
DEFINE VAR gall         AS DECIMAL.
DEFINE VAR gallx        AS DECIMAL.

FOR EACH contratos_prod_fac_cons.
    DELETE contratos_prod_fac_cons.
END.

FOR EACH items_contratos NO-LOCK WHERE items_contratos.id_tipo_contrato < 100
                                  /* AND id_contrato = "BZ0090" */
                                   AND items_contratos.c_fecha >= p_fecha
                         BY items_contratos.anio_semana_entrega
                         BY items_contratos.semana_entrega
                         BY items_contratos.semana_entrega_hasta.
    
    IF p_cliente > 0 THEN
        FIND contratos WHERE contratos.id_contrato      = items_contratos.id_contrato
                         AND contratos.id_tipo_contrato = items_contratos.id_tipo_contrato
                         AND contratos.anio             = items_contratos.anio
                         AND contratos.id_cliente       = p_cliente NO-LOCK NO-ERROR. 
    ELSE
        FIND contratos OF items_contratos NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN
    DO:
      
      FIND FIRST orden_entrega OF items_contratos NO-LOCK NO-ERROR.
      IF AVAILABLE orden_entrega THEN
        DO:
            FOR EACH tambores_industria OF orden_entrega
                                      NO-LOCK
                                      BREAK BY tambores_industria.id_sucursal
                                            BY tambores_industria.id_lote
                                            BY YEAR(tambores_industria.fecha)
                                            BY tambores_industria.id_articulo
                                            BY tambores_industria.id_calidad.
            
                v_tambores          = v_tambores + 1.
                v_kilos_tambores    = v_kilos_tambores + tambores_industria.kilos_tambor.
                IF tambores_industria.id_sucursal_ubicacion <> 85 THEN 
                DO:
                    v_tam_stock          = v_tam_stock + 1.
                    v_kilos_tam_stock    = v_kilos_tam_stock + tambores_industria.kilos_tambor.
                END.
                ELSE DO:
                    v_tam_despachados          = v_tam_despachados + 1.
                    v_kilos_tam_despachados    = v_kilos_tam_despachados + tambores_industria.kilos_tambor.
                END.

                
                case items_contratos.id_tipo_unidad_venta_origen:
                              when 1 then /* TONELADAS */
                                  do:
                                      v_importe_oe = v_importe_oe + ((tambores_industria.kilos_tambor / 1000) * items_contrato.precio_origen).
                                  end.
                              when 2 then /* KILOS */
                                  do:
                                      v_importe_oe = v_importe_oe + (tambores_industria.kilos_tambor * items_contrato.precio_origen).
                                  end.
                              when 3 then /* GALONES */                    
                                  do:
                                      run p_calcula_galones_tambor.p (  input tambores_industria.id_etiqueta, OUTPUT gall ).
                                      v_importe_oe = v_importe_oe + (gall * items_contrato.precio_origen).
                                  end.
                              when 4 then /* LIBRAS */
                                  do:
                                      v_importe_oe = v_importe_oe + ((tambores_industria.kilos_tambor * 2.20462) * items_contrato.precio_origen).
                                  end.
                end case. 
            END.
        END.
        ELSE
        DO:
            FIND r_envases_prod OF items_contratos NO-LOCK NO-ERROR.
            IF AVAILABLE r_envases_prod THEN
            DO:
            case items_contratos.id_tipo_unidad_venta_origen:
                          when 1 then /* TONELADAS */
                              do:
                                  v_importe_oe =  (( items_contratos.cantidad * r_envases_prod.kilos / 1000) * items_contrato.precio_origen).
                              end.
                          when 2 then /* KILOS */
                              do:
                                  v_importe_oe = (items_contratos.cantidad * r_envases_prod.kilos * items_contrato.precio_origen).
                              end.
                          when 3 then /* GALONES */                    
                              do:
                                  v_importe_oe = (items_contratos.cantidad * 53.6 * items_contrato.precio_origen).
                              end.
                          when 4 then /* LIBRAS */
                              do:
                                  v_importe_oe = ((items_contratos.cantidad * r_envases_prod.kilos * 2.20462) * items_contrato.precio_origen).
                              end.
            end case. 
            END.
            /*ELSE MESSAGE "Articulo " items_contratos.id_articulo " Envase " items_contratos.id_envase
                    VIEW-AS ALERT-BOX. */
      END.
      /* MESSAGE " Contrato " contratos.id_contrato items_contratos.ITEM VIEW-AS ALERT-BOX. */
      v_importe_fac = 0.
      v_bultos_fac = 0.
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
                    v_importe_fac = v_importe_fac + 
                                    ((items_venta.precio_origen + items_venta.gastos) * items_venta.cantidad).
                    /*MESSAGE v_importe_fac items_packing_list.ITEM 
                              items_packing_list.id_packing_list nro_pack_list VIEW-AS ALERT-BOX. */
                    FIND FIRST subd_vtas OF items_venta NO-LOCK NO-ERROR.
                    IF AVAILABLE subd_vtas THEN
                        DO:
                            v_factura = string(subd_vtas.id_punto_venta,"9999") + STRING(subd_vtas.nro_comp,"99999999").
                            /* MESSAGE v_factura subd_vtas.estado VIEW-AS ALERT-BOX. */
                        END.
                END.

                 
            END.
            v_bultos_fac = v_bultos_fac + items_packing_list.cantidad.                                          
        END.
        
        IF LAST-OF(packing_list.ITEM_contrato) THEN
        DO:
            FIND FIRST r_items_venta_pack_list OF packing_list NO-LOCK NO-ERROR.
            IF AVAILABLE r_items_venta_pack_list THEN
            DO:
                FOR EACH r_cobranzas_comp WHERE r_cobranzas_comp.id_punto_venta = 
                                                r_items_venta_pack_list.id_punto_venta 
                                            AND r_cobranzas_comp.nromov         = 
                                                r_items_venta_pack_list.nromov
                                          NO-LOCK.
                    v_importe_cob = v_importe_cob + r_cobranzas_comp.importe_pago.
                END.
            END.
        END.
      END.
       
    
        FIND clientes OF contratos NO-LOCK NO-ERROR.
        IF AVAILABLE cliente THEN
        DO:
            {..\industria\i_contratos_prod_fac_cons.i}
        END.
        
    END.



    
    /* MESSAGE "Tambores " v_tambores
            " kilos " v_kilos_tambores
            " Despachados " v_tam_despachados
            " kilos " v_kilos_tam_despachados
            " Stock " v_tam_stock
            " kilos " v_kilos_tam_stock
            " Facturado " v_importe_fac
            " Bultos fac " v_bultos_fac
            " Fac segun OE " v_importe_oe
            " Cobrado " v_importe_cob VIEW-AS ALERT-BOX.
       */
    
    v_tambores = 0.
    v_kilos_tambores = 0.
    v_tam_despachados = 0.
    v_kilos_tam_despachados = 0.
    v_tam_stock = 0.
    v_kilos_tam_stock = 0.
    v_importe_fac = 0.
    v_importe_cob = 0.
    v_importe_oe = 0.
    v_bultos_fac = 0.
    v_factura = "".
    v_item_fac = 0.
    v_nro_pack_list = "".
END.
