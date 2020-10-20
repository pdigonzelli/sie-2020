for each gastos_items_orden_entrega where gastos_items_orden_entrega.id_orden_entrega = v_id_orden_entrega
                                      AND gastos_items_orden_entrega.item_oe = v_item_oe.
    delete gastos_items_orden_entrega.
end.
