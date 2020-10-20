DEFINE VAR h_con AS HANDLE.
DEFINE VAR v_r_oe AS ROWID.
define var r as rowid no-undo.

RUN get-container (OUTPUT h_con).
RUN get-rowid-oe IN h_con (OUTPUT v_r_oe).

FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = v_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    CASE orden_entrega.id_tipo_orden_entrega:
        WHEN 1 THEN DO: /* INDUSTRIA */
            run wc_calidades.w(output r).
            find calidades where rowid(calidades) = r no-lock no-error.
            if available calidades then 
            general.items_orden_entrega.id_calidad:screen-value = string(calidades.id_calidad).
        END.
        WHEN 2 THEN DO: /* FRUTA FRESCA */
            run wc_variedades.w(output r).
            find variedades where rowid(variedades) = r no-lock no-error.
            if available variedades then 
            general.items_orden_entrega.id_calidad:screen-value = string(variedades.id_variedad).
        END.
    END CASE.
END.


apply 'U1' to self.
