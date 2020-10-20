FOR EACH lotes_plantacion WHERE estado = YES AND codigo_trazabilidad <> "" NO-LOCK:
    FIND FIRST lote WHERE lote.id_proveedor = lotes_plantacion.id_proveedor AND
        lote.id_origen = lotes_plantacion.id_origen AND
        lote.id_lote = lotes_plantacion.id_lote NO-LOCK NO-ERROR.
    IF NOT AVAILABLE lote THEN
    DO:
        CREATE lote.
        BUFFER-COPY lotes_plantacion TO lote.
        DISPLAY lotes_plantacion.id_lote codigo_trazabilidad.
    END.
END.
