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


FOR EACH items_contratos NO-LOCK /* WHERE items_contratos.id_contrato = "TW0150"
                                   AND items_contratos.ITEM = 1 */.
    FIND contratos OF items_contratos.
    FOR EACH tambores_industria WHERE tambores_industria.id_contrato_of     = items_contratos.id_contrato
                                  AND tambores_industria.id_tipocontrato_of = items_contratos.id_tipo_contrato
                                  AND tambores_industria.anio_of            = items_contratos.anio
                                  AND tambores_industria.ITEM_of            = items_contratos.ITEM
                                  NO-LOCK
                                  BREAK BY id_sucursal
                                        BY id_lote
                                        BY YEAR(tambores_industria.fecha)
                                        BY id_articulo
                                        BY id_calidad.
        
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
        
        IF LAST-OF(id_calidad) THEN
        DO:
            FIND orden_entrega OF tambores_industria NO-LOCK NO-ERROR.
            IF AVAILABLE orden_entrega THEN
            DO:
                v_importe_oe = orden_entrega.total_factura.
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