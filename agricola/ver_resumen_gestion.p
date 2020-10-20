FOR EACH liq_items_tarjas WHERE legajo = 80565 NO-LOCK BY fecha:
    DISPLAY liq_items_tarjas.fecha cant_horas cant_hs_norm.
END.


FOR EACH resumen_gestion WHERE legajo = 80565 NO-LOCK BY fecha:
    DISPLAY resumen_gestion.
END.
