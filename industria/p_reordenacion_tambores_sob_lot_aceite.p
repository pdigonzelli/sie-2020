DEFINE BUFFER bb_tam FOR tambores_industria.
DEFINE VAR i AS INTEGER.
DEFINE TEMP-TABLE tt_orden
    FIELD id_lote AS INTEGER
    FIELD anio AS INTEGER
    FIELD id_sucursal AS INTEGER
    FIELD id_articulo AS INTEGER
    FIELD orden AS INTEGER
    INDEX principal IS PRIMARY UNIQUE id_lote anio id_sucursal id_articulo .

FOR EACH lotes_ind WHERE id_lote = 605
                     AND anio = 2002
                     AND id_sucursal = 95.
    DISP id_lote c_fecha.
    FOR EACH lotes_aceite OF lotes_ind.
        DISP id_articulo.
        FOR EACH sobrante_lotes_aceite OF lotes_aceite.
            DISP sobrante_lotes_aceite.id_articulo.
            FOR EACH tambores_industria WHERE tambores_industria.id_empresa     = sobrante_lotes_aceite.id_empresa
                                          AND tambores_industria.id_sucursal    = sobrante_lotes_aceite.id_sucursal
                                          AND tambores_industria.id_tipotambor  = sobrante_lotes_aceite.id_tipotambor_sobrante
                                          AND tambores_industria.nromov  = sobrante_lotes_aceite.nromov_sobrante
                                          BREAK BY nromov_sobrante.
                /* IF FIRST-OF(nromov_sobrante) THEN DO:
                    i = 0.
                    FOR EACH bb_tam WHERE bb_tam.id_empresa    = tambores_industria.id_empresa
                                       AND bb_tam.id_sucursal   = tambores_industria.id_sucursal
                                       AND bb_tam.id_lote       = tambores_industria.id_lote
                                       AND bb_tam.anio          = tambores_industria.anio
                                       AND bb_tam.id_articulo   = tambores_industria.id_articulo
                                       AND bb_tam.nromov        <> tambores_industria.nromov
                                       AND bb_tam.c_fecha       <= tambores_industria.c_fecha
                                       AND bb_tam.c_hora        < tambores_industria.c_hora 
                                        NO-LOCK .
                        i = i + 1.
                    END.
                        FIND FIRST tt_orden WHERE tt_orden.id_lote      = lotes_ind.id_lote
                                              AND tt_orden.anio         = lotes_ind.anio
                                              AND tt_orden.id_sucursal  = lotes_ind.id_sucursal
                                              AND tt_orden.id_articulo  = sobrante_lotes_aceite.id_articulo
                                            NO-ERROR.
                        IF AVAILABLE tt_orden THEN DO:
                            ASSIGN tt_orden.orden = tt_orden.orden + i.
                            i = tt_orden.orden.
                            
                        END.
                        ELSE DO:
                            CREATE tt_orden.
                            ASSIGN tt_orden.id_lote      = lotes_ind.id_lote
                                   tt_orden.anio         = lotes_ind.anio
                                   tt_orden.id_sucursal  = lotes_ind.id_sucursal
                                   tt_orden.id_articulo  = sobrante_lotes_aceite.id_articulo
                                   tt_orden.orden        = i.
                        END.
                        MESSAGE bb_tam.id_tambor bb_tam.fecha tambores_industria.fecha tt_orden.orden VIEW-AS ALERT-BOX.
                    
                END. 
                i = i + 1. */
                DISP tambores_industria.id_tambor tambores_industria.id_locacion_ubicacion.
                /*UPDATE tambores_industria.id_tambor. */  
            END.
        END.
    END.
END.
