TRIGGER PROCEDURE FOR WRITE OF items_programa_despacho.
DEFINE BUFFER b_cal FOR calibres_programas_despacho.
IF AVAILABLE items_programa_despacho THEN
DO:
    FOR EACH b_cal OF items_programa_despacho:
        b_cal.cantidad = (b_cal.porcentaje * items_programa_despacho.cantidad) / 100.
    END.
END.

