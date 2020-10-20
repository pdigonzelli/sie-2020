FOR EACH liq_items_tarjas WHERE fecha >= DATE("01/10/16") 
    AND cantidad <> 0 NO-LOCK BREAK BY id_codigo_abacus_cantidad:
    IF LAST-OF(id_codigo_abacus_cantidad) THEN
    DO:
        FIND FIRST liq_conceptos WHERE liq_conceptos.id_concepto = liq_items_tarjas.id_codigo_abacus_cantidad NO-LOCK NO-ERROR.
        IF AVAILABLE liq_conceptos THEN
            DISPLAY liq_conceptos.
    END.
END.
