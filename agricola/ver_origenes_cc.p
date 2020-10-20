OUTPUT TO z:\temp\origenes.txt.

FOR EACH origenes WHERE estado = YES NO-LOCK:
    FIND FIRST liq_ccostosliq OF origenes WHERE liq_ccostos.id_centro_costo_liq >= 1000000000 NO-LOCK NO-ERROR.
    EXPORT DELIMITER ";" 
        origenes.id_proveedor
        origenes.id_origen
        origenes.descripcion
        (IF AVAILABLE liq_ccostosliq THEN liq_ccostosliq.id_centro_costo_liq ELSE "").

END.
OUTPUT CLOSE.
