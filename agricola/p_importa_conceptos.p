DEF TEMP-TABLE t-datos 
FIELD id_concepto AS CHARACTER
FIELD descripcion AS CHARACTER FORMAT "x(40)"
FIELD desc_param AS CHARACTER FORMAT "x(40)"
FIELD id_parametro AS INTEGER.

INPUT FROM z:\sistemas\sami\util\conceptosliq.csv.
REPEAT:
    CREATE t-datos.
    IMPORT DELIMITER ";" t-datos.
END.
INPUT CLOSE.

FOR EACH t-datos WHERE id_concepto <> "0"  NO-LOCK:
    FIND FIRST liq_conceptos WHERE liq_conceptos.id_concepto = INTEGER(t-datos.id_concepto) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE liq_conceptos THEN
    DO:
        CREATE liq_conceptos.
        ASSIGN liq_conceptos.id_concepto = INTEGER(t-datos.id_concepto)
               liq_conceptos.descripcion = t-datos.descripcion
               liq_conceptos.id_parametro = t-datos.id_parametro. 
    END.
END.
