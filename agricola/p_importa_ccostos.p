DEF TEMP-TABLE t-datos 
FIELD id_centro_costo AS CHARACTER
FIELD descripcion AS CHARACTER FORMAT "x(40)".

INPUT FROM z:\sistemas\sami\util\ccostos.csv.
REPEAT:
    CREATE t-datos.
    IMPORT DELIMITER ";" t-datos.
END.
INPUT CLOSE.

FOR EACH t-datos WHERE id_centro_costo <> "0"  NO-LOCK:
    FIND FIRST liq_ccostosliq WHERE liq_ccostosliq.id_centro_costo_liq = INTEGER(t-datos.id_centro_costo) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE liq_ccostosliq THEN
    DO:
        CREATE liq_ccostosliq.
        ASSIGN liq_ccostosliq.id_centro_costo_liq = INTEGER(t-datos.id_centro_costo)
               liq_ccostosliq.descripcion = t-datos.descripcion.
    END.
END.
