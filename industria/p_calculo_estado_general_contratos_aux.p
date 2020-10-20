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

FOR EACH contratos_prod_fac_cons.
    DELETE contratos_prod_fac_cons.
END.

FOR EACH items_contratos NO-LOCK /* WHERE items_contratos.id_contrato = "13242-B"
                                   AND items_contratos.ITEM = 1 */ .
    FIND contratos OF items_contratos.
    FOR EACH orden_entrega OF items_contratos NO-LOCK.
        FOR EACH tambores_industria OF orden_entrega
                                  NO-LOCK
                                  BREAK BY tambores_industria.id_sucursal
                                        BY tambores_industria.id_lote
                                        BY YEAR(tambores_industria.fecha)
                                        BY tambores_industria.id_articulo
                                        BY tambores_industria.id_calidad.
        
            v_tambores          = v_tambores + 1.
            v_kilos_tambores    = v_kilos_tambores + tambores_industria.kilos_tambor.
            IF tambores_industria.id_locacion = 4 THEN 
            DO:
                v_tam_stock          = v_tam_stock + 1.
                v_kilos_tam_stock    = v_kilos_tam_stock + tambores_industria.kilos_tambor.
            END.
            IF tambores_industria.id_locacion = 50 THEN 
            DO:
                v_tam_despachados          = v_tam_despachados + 1.
                v_kilos_tam_despachados    = v_kilos_tam_despachados + tambores_industria.kilos_tambor.
            END.
            
            IF LAST-OF(tambores_industria.id_calidad) THEN
            DO:
               
                   /* v_importe_oe = orden_entrega.total_factura. */
                    FOR EACH r_subd_ventas_oe WHERE r_subd_ventas_oe.nro_orden_embarque = 
                                                    orden_entrega.id_orden_entrega
                                              NO-LOCK.
                        FOR EACH subd_vtas WHERE subd_vtas.id_punto_venta = r_subd_ventas_oe.id_punto_venta 
                                             AND subd_vtas.nromov         = r_subd_ventas_oe.nromov
                                             NO-LOCK.
                            v_importe_fac = v_importe_fac + subd_vtas.importe_origen.
                            FOR EACH aux_subd_ventas OF subd_vtas.
                                v_bultos_fac = v_bultos_fac + aux_subd_ventas.cant_envases.
                            END.
                            
                        END.
                        FOR EACH r_cobranzas_comp WHERE r_cobranzas_comp.id_punto_venta = 
                                                        r_subd_ventas_oe.id_punto_venta 
                                                    AND r_cobranzas_comp.nromov         = 
                                                        r_subd_ventas_oe.nromov
                                                  NO-LOCK.
                            v_importe_cob = v_importe_cob + r_cobranzas_comp.importe_pago.
                            
                        END.
                        
                    END.
                    
                
            END.
        END.
    END.




    {..\industria\i_contratos_prod_fac_cons.i}
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
END.
