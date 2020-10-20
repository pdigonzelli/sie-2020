FOR EACH liq_legajo WHERE id_empresa = 101 AND legajo = 74716 NO-LOCK:
    DISPLAY liq_legajo.id_empresa liq_legajos.id_centro_costo. 
    FIND FIRST liq_centros_costos WHERE 
        liq_centros_costos.id_empresa = liq_legajo.id_empresa AND
        liq_centros_costos.id_centro_costo = liq_legajos.id_centro_costo NO-LOCK NO-ERROR.
    IF AVAILABLE liq_centros_costos THEN
    DO:
        DISPLAY liq_centros_costos.descripcion 
                liq_centros_costos.id_proveedor 
                liq_centros_costos.id_origen.
        FIND FIRST liq_ccostos WHERE liq_ccostos.id_proveedor = liq_centros_costos.id_proveedor AND
            liq_ccostos.id_origen = liq_centros_costos.id_origen NO-LOCK NO-ERROR.
        IF AVAILABLE liq_ccostos THEN
            DISPLAY liq_ccostos.id_centro_costo.
    END.
END.
