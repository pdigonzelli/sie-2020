define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_seguro as decimal.

find items_orden_entrega where items_orden_entrega.id_orden_entrega = p_id_orden_entrega 
                           AND items_orden_entrega.ITEM_oe = p_item_oe
                         no-lock no-error.
IF AVAILABLE items_orden_entrega THEN DO:
    find last gastos_seguro no-lock no-error.

    if available gastos_seguro then
        do:
            p_seguro = ((gastos_seguro.importe * items_orden_entrega.total_factura) / 100000) * items_orden_entrega.contenedores.
            create gastos_items_orden_entrega.
            assign gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
                   gastos_items_orden_entrega.ITEM_oe             = p_item_oe
                   gastos_items_orden_entrega.id_gasto            = p_id_gasto
                   gastos_items_orden_entrega.importe             = p_seguro.

            RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                              INPUT p_item_oe,
                                              INPUT p_id_gasto).
        end.

END.

