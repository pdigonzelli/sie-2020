FOR EACH contratos WHERE anio = 2002 AND id_contrato = "ar-1839C" NO-LOCK:
    DISPLAY contratos.id_contrato.
    FOR EACH items_contratos OF contratos NO-LOCK:
        DISPLAY ITEM c_fecha c_usuario.
    END.
END.
