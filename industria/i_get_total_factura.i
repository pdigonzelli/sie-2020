DEFINE INPUT PARAMETER r_item_oe AS ROWID.
DEFINE OUTPUT PARAMETER p_total_factura AS DECIMAL.

define var r as rowid no-undo.
define var gall as decimal no-undo.
define var v_kilos as decimal no-undo.
define var v_kilos_brutos as decimal no-undo.
define var v_precio_total as decimal no-undo.
define var v_cantidad_tambores as integer no-undo.
define var v_gall_brix like inspecciones_lote.bx_correg no-undo.
define var v_id_orden_entrega like orden_entrega.id_orden_entrega no-undo.
DEFINE VAR v_item_oe AS INTEGER.
DEFINE VAR v_gasto_iks AS DECIMAL.


FIND items_orden_entrega WHERE ROWID(items_orden_entrega) = r_item_oe NO-ERROR.
IF AVAILABLE items_orden_entrega THEN DO:
    IF items_orden_entrega.id_condicion_venta = 22 THEN DO:
        FIND FIRST gastos_items_orden_entrega OF items_orden_entrega
                                          WHERE gastos_items_orden_entrega.id_gasto = 13 NO-LOCK NO-ERROR.
        v_gasto_iks = IF AVAILABLE gastos_items_orden_entrega THEN gastos_items_orden_entrega.importe ELSE 0.
    END.
    
    FIND items_contratos where items_contratos.id_contrato      = items_orden_entrega.id_contrato
                           and items_contratos.id_tipo_contrato = items_orden_entrega.id_tipo_contrato
                           and items_contratos.anio             = items_orden_entrega.anio
                           and items_contratos.item             = items_orden_entrega.item
                           no-lock no-error.
    IF available items_contratos THEN DO:
        find contratos of items_contratos no-lock no-error.
        if available contratos THEN DO:
            IF items_orden_entrega.modo_actualizacion THEN DO:
                  /* MODO AUTOMATICO, ES DECIR TOMA INFORMACION DE TAMBORES RELACIONADOS */
                v_id_orden_entrega = items_orden_entrega.id_orden_entrega.
                v_item_oe          = items_orden_entrega.ITEM_oe.
    
                run p_calcular-brix_item_oe.p (input v_id_orden_entrega,
                                               INPUT v_item_oe,
                                               output v_kilos,
                                               output v_kilos_brutos,
                                               output v_cantidad_tambores,
                                               output gall,
                                               output v_gall_brix).
                
            END.
            ELSE DO:
                           /* MODO MANUAL, TOMA LOS DATOS CARGADOS EN LA OE */
                v_cantidad_tambores  = items_orden_entrega.cantidad_tambores.
                v_kilos              = items_orden_entrega.kgs_netos_tambores.
                v_kilos_brutos       = items_orden_entrega.kgs_brutos_tambores.
                gall = 0.
                v_gall_brix = 0.
            END.
            CASE items_contratos.id_tipo_unidad_venta_origen:
                WHEN 1 then /* TONELADAS */ DO:
                    v_precio_total = ((v_kilos / 1000) * items_contrato.precio_origen).
                end.
                WHEN 2 then /* KILOS */ DO:
                    v_precio_total = (v_kilos * items_contrato.precio_origen).
                end.
                when 3 then /* GALONES */  DO: 
                    v_precio_total = gall * items_contrato.precio_origen.
                end.
                when 4 then /* LIBRAS */ DO:
                    v_precio_total = ((v_kilos * 2.20462) * items_contrato.precio_origen).
                end.
            END case. 
            p_total_factura = v_precio_total - v_gasto_iks.
            /*MESSAGE "ultimmo " p_total_factura VIEW-AS ALERT-BOX.*/
        END.
    END.
END.
