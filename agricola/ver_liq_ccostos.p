FOR EACH liq_legajos WHERE id_empresa = 101 NO-LOCK:
    FIND FIRST liq_ccostos WHERE liq_ccostos.id_centro_costo = liq_legajos.id_ccostos_liq NO-LOCK NO-ERROR.
    IF AVAILABLE liq_ccostos THEN
        DISPLAY liq_legajos.legajo liq_ccostos.id_proveedor liq_ccostos.id_origen.
END.
