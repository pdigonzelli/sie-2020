DEFINE input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
DEFINE input parameter p_id_gasto like gastos_venta.id_gasto.
DEFINE var p_valor as decimal.
DEFINE var v_impuesto as decimal.

FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = p_id_orden_entrega 
                                 AND items_orden_entrega.ITEM_oe          = p_item_oe
                                NO-LOCK no-error.

IF AVAILABLE items_orden_entrega THEN DO:
    FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
    IF AVAILABLE items_contratos THEN DO:
        FIND FIRST r_gastos_items_contrato OF items_contratos
                                           WHERE r_gastos_items_contrato.id_gasto = p_id_gasto
                                           NO-LOCK NO-ERROR.
        IF AVAILABLE r_gastos_items_contrato THEN DO:

                CASE items_contratos.id_tipo_unidad_venta_origen:
                    WHEN 1 then /* TONELADAS */ DO:
                        p_valor = ((items_orden_entrega.kgs_netos_tambores / 1000) * r_gastos_items_contrato.importe).
                    END.
                    WHEN 2 then /* KILOS */ DO:
                        p_valor = (items_orden_entrega.kgs_netos_tambores * r_gastos_items_contrato.importe).
                    END.
                    WHEN 3 then /* GALONES */ DO:
                        p_valor = (items_orden_entrega.total_galones * r_gastos_items_contrato.importe).
                    END.
                    WHEN 4 THEN /* LIBRAS */ DO:
                        p_valor = ((items_orden_entrega.kgs_netos_tambores * 2.20462) * r_gastos_items_contrato.importe).
                    END.
                END CASE.
        END.
    END.
END.

IF p_id_gasto > 0 THEN DO:

    CREATE gastos_items_orden_entrega.
    ASSIGN gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
           gastos_items_orden_entrega.ITEM_oe             = p_item_oe
           gastos_items_orden_entrega.id_gasto            = p_id_gasto
           gastos_items_orden_entrega.importe             = p_valor.

    RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                      INPUT p_item_oe,
                                      INPUT p_id_gasto).
END.
