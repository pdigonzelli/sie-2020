FOR EACH orden_entrega WHERE id_orden_entrega = 8659 NO-LOCK:
    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK:
        DISPLAY id_articulo id_contrato ITEM c_fecha c_usuario.
    END.
END.
