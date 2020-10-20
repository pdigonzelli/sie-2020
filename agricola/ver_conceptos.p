FOR EACH conceptos_abacus WHERE id_concepto = 11 NO-LOCK:
    DISPLAY conceptos_abacus.
END.

FOR EACH liq_conceptos WHERE descripcion MATCHES ("*llu*") NO-LOCK:
    DISPLAY liq_conceptos.
END.
