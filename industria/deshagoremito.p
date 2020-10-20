DEFINE VAR  i  AS INTEGER NO-UNDO.
DEFINE VAR  j  AS INTEGER NO-UNDO.

FOR EACH remitos WHERE remitos.id_tipo_movsto   = 122 AND 
                       remitos.id_sucursal      = 96 AND
                       remitos.nro              = 955 NO-LOCK , 
    EACH tambores_industria WHERE tambores_industria.id_tipo_movsto = remitos.id_tipo_movsto AND 
                                  tambores_industria.id_sucursal_remito = remitos.id_sucursal    AND 
                                  nro_remito  = remitos.nro 
    BREAK   BY tambores_industria.id_sucursal_ubicacion 
            BY tambores_industria.id_lote
            BY tambores_industria.nro_remito.

    i = i + 1.

    IF LAST-OF(id_lote) THEN
    DO:
       FIND lotes_ubicacion OF tambores_industria NO-ERROR.
       DISPLAY tambores_industria.id_lote i tambores_industria.id_sucursal_ubicacion lotes_ubicacion.cantidad WHEN AVAILABLE lotes_ubicacion.
       IF AVAILABLE lotes_ubicacion THEN
           DELETE lotes_ubicacion.
       tambores_industria.id_sucursal_ubicacion = remitos.id_sucursal.
       FIND lotes_ubicacion OF tambores_industria NO-ERROR.
       IF AVAILABLE lotes_ubicacion THEN
           DELETE lotes_ubicacion.

       i = 0.    
       
    END.
    /*
    id_sucursal_ubicacion = 96. */
END.
