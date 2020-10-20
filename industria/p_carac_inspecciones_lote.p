DEFINE INPUT PARAMETER p_r_protocolos AS ROWID.
DEFINE INPUT PARAMETER p_id_caracteristica AS INTEGER.
DEFINE OUTPUT PARAMETER p_valor AS DECIMAL.

FIND FIRST protocolos WHERE ROWID(protocolos) = p_r_protocolos NO-LOCK NO-ERROR.
IF AVAILABLE protocolos THEN DO:
    FIND FIRST lotes_jugo OF protocolos NO-LOCK NO-ERROR.
    IF AVAILABLE lotes_jugo THEN DO:
        FIND FIRST inspecciones_lote OF lotes_jugo NO-LOCK NO-ERROR.
        IF AVAILABLE inspecciones_lote THEN DO:
            CASE p_id_caracteristica:
                WHEN 1 THEN /* BRIX REFRACTOMETER */ DO:
                    p_valor = inspecciones_lote.bx_20_20.
                END.
                WHEN 2 THEN /* ACIDITY % W/W */ DO:
                    p_valor = inspecciones_lote.acidez_w_w.
                END.
                WHEN 3 THEN /* CORRECTED BRIX */ DO:
                    p_valor = inspecciones_lote.bx_correg.
                END.
                WHEN 5 THEN /* ACIDITY W/V */ DO:
                    p_valor = inspecciones_lote.acidez_w_v.
                END.
                WHEN 21 THEN /* RATIO */ DO:
                    p_valor = Inspecciones_lote.Ratio.
                END.
                WHEN 23 THEN /* PULPA */ DO:
                    p_valor = Inspecciones_lote.Porcentaje_pulpa.
                END.
            END CASE.
            
        END.
    END.
END.

