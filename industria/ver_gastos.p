/*INSERT gastos_items_orden_entrega.*/

FOR EACH orden_entrega WHERE id_orden_entrega = 10904 NO-LOCK:
    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK:
        FOR EACH gastos_items_orden_entrega OF items_orden_entrega NO-LOCK:
            DISPLAY gastos_items_orden_entrega.
        END.
    END.
END.
