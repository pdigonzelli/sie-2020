FOR EACH liq_tarjas WHERE id_tipo_planilla = 6 NO-LOCK:
    DISPLAY  id_sucursal id_sector id_proveedor id_origen liq_tarjas.fecha.

    FOR EACH liq_items_tarjas OF liq_tarjas NO-LOCK:
        DISPLAY liq_items_tarjas.legajo liq_items_tarjas.fecha liq_items_tarjas.cantidad liq_items_tarjas.id_codigo_abacus_cantidad
            liq_items_tarjas.id_tarea.
    END.
END.
