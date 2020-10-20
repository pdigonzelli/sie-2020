define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define input parameter p_id_gasto like gastos_venta.id_gasto.
DEFINE VAR v_importes AS DECIMAL INITIAL 0.


FOR EACH gastos_items_orden_entrega WHERE gastos_items_orden_entrega.id_orden_entrega =
                                          p_id_orden_entrega
                                      AND gastos_items_orden_entrega.id_gasto =
                                          p_id_gasto
                                     NO-LOCK.
    v_importes = v_importes + gastos_items_orden_entrega.importe.
END.
                                                
IF v_importes > 0 THEN DO:
    FIND FIRST gastos_orden_entrega WHERE gastos_orden_entrega.id_orden_entrega    = p_id_orden_entrega
                                      AND gastos_orden_entrega.id_gasto            = p_id_gasto
                                      NO-ERROR.
    IF NOT AVAILABLE gastos_orden_entrega THEN DO:
        create gastos_orden_entrega.
        assign gastos_orden_entrega.id_orden_entrega    = p_id_orden_entrega
               gastos_orden_entrega.id_gasto            = p_id_gasto
               gastos_orden_entrega.importe             = v_importes.
    END.
    ELSE DO:
        assign gastos_orden_entrega.importe             = v_importes.
    END.
END.
