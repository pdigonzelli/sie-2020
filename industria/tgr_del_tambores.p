TRIGGER PROCEDURE FOR DELETE OF tambores_industria OLD BUFFER old_tambores.

/* Variable Definitions */
IF old_tambores.id_sucursal_ubicacion = 85 OR
   old_tambores.id_sucursal_ubicacion = 91 THEN DO:
   MESSAGE "No se puede modificar tambores que ya fueron despachados o facturados"
       VIEW-AS ALERT-BOX.
   UNDO, RETURN ERROR.
END. 
