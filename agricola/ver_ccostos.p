FOR EACH centros_costos_abacus NO-LOCK:
    
    /*FIND FIRST liq_centros_costos WHERE liq_centros_costos.id_centro_costo = centros_costos_abacus.id_centro_abacus NO-ERROR.
    IF AVAILABLE liq_centros_costos THEN
    DO:
        IF liq_centros_costos.id_proveedor = 0 AND centros_costos_abacus.id_proveedor <> 0 THEN
        DO:
            ASSIGN liq_centros_costos.id_proveedor = centros_costos_abacus.id_proveedor
                   liq_centros_costos.id_origen = centros_costos_abacus.id_origen.
            
            DISPLAY liq_centros_costos.id_proveedor liq_centros_costos.id_origen
            centros_costos_abacus.id_proveedor centros_costos_abacus.id_origen.
        END.
    END. */

    FIND FIRST liq_ccostosliq WHERE liq_ccostosliq.id_centro_costo = centros_costos_abacus.id_centro_abacus NO-ERROR.
    IF AVAILABLE liq_ccostosliq THEN
    DO:
        IF liq_ccostosliq.id_proveedor = 0 THEN
        DO:
            ASSIGN liq_ccostosliq.id_proveedor = centros_costos_abacus.id_proveedor
                   liq_ccostosliq.id_origen = centros_costos_abacus.id_origen.
            
           /* DISPLAY liq_centros_costos.id_proveedor liq_centros_costos.id_origen
            centros_costos_abacus.id_proveedor centros_costos_abacus.id_origen.*/
        END.
    END.


END.
