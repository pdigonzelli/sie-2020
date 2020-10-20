DEFINE VAR i AS INTEGER NO-UNDO.
DEFINE VAR j AS INTEGER NO-UNDO.

FOR EACH tambores_industria WHERE id_sucursal_ubicacion = 86 NO-LOCK
    BREAK BY id_sucursal_ubicacion BY id_lote.
    i = i + 1.
    IF LAST-OF(id_lote) THEN 
    DO:
        FIND lotes_ubicacion OF tambores_industria  NO-ERROR.
        DISP
            tambores_industria.id_sucursal_ubicacion
            i (TOTAL)
            tambores_industria.id_lote  
            lotes_ubicacion.cantidad WHEN AVAILABLE lotes_ubicacion (TOTAL).
            j =  i - IF AVAILABLE lotes_ubicacion THEN lotes_ubicacion.cantidad ELSE 0.
            DISP j (TOTAL).
            IF j <> 0 THEN
                lotes_ubicacion.cantidad = i.
        i = 0.

    END.
END.
i = 0.
FOR EACH lotes_ubicacion WHERE lotes_ubicacion.id_sucursal_ubicacion = 86 .
    i = 0.
    FOR EACH tambores_industria OF lotes_ubicacion NO-LOCK.
          i = i + 1.
    END.
    FIND productos_terminados OF lotes_ubicacion NO-LOCK.
    IF i <> lotes_ubicacion.cantidad  THEN
        DISP
            lotes_ubicacion.id_sucursal_ubicacion
            i (TOTAL)
            lotes_ubicacion.lote
            lotes_ubicacion.id_articulo
            productos_terminados.descripcion
            lotes_ubicacion.cantidad WHEN AVAILABLE lotes_ubicacion (TOTAL).
    IF i = 0  AND lotes_ubicacion.cantidad <> 0 THEN
        DELETE lotes_ubicacion.
END.
