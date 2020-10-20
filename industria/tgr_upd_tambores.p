TRIGGER PROCEDURE FOR Write OF tambores_industria OLD BUFFER old_tambores.
DEFINE VAR v_x AS CHARACTER.

/* Variable Definitions */
IF old_tambores.id_sucursal_ubicacion <> 0 THEN DO:
    BUFFER-COMPARE tambores_industria TO old_tambores SAVE RESULT IN v_x.
    IF v_x MATCHES "id_sucursal_ubicacion" THEN DO:
        IF old_tambores.id_sucursal_ubicacion = 85 OR
           old_tambores.id_sucursal_ubicacion = 91 THEN DO:
           MESSAGE "No se puede modificar tambores que ya fueron despachados o facturados"
               VIEW-AS ALERT-BOX.
           UNDO, RETURN ERROR.
        END.
    END.
END.
 
