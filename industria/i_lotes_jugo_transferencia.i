            IF tambores_industria.id_articulo < 900 THEN
                v_articulo = tambores_industria.id_articulo + 900.
            ELSE
                v_articulo = tambores_industria.id_articulo - 900.
            
            RUN y_gstkcre.p (INPUT tambores_industria.id_empresa,
                             INPUT tambores_industria.id_sucursal,
                             INPUT tambores_industria.id_tipotambor,
                             INPUT tambores_industria.nromov,
                             INPUT tambores_industria.id_tambor,
                             INPUT tambores_industria.id_tambor,
                             INPUT 2) "tambores_industria".

            FIND FIRST bb_lotes WHERE bb_lotes.id_empresa   = tambores_industria.id_empresa
                                  AND bB_lotes.id_sucursal  = tambores_industria.id_sucursal
                                  AND bb_lotes.id_lote      = tambores_industria.id_lote
                                  AND bb_lotes.anio         = tambores_industria.anio
                                  AND bb_lotes.id_articulo  = v_articulo
                                  AND bb_lotes.id_calidad   = tambores_industria.id_calidad NO-ERROR.
            IF AVAILABLE bb_lotes THEN DO:

                ASSIGN tambores_industria.id_empresa            = bb_lotes.id_empresa
                       tambores_industria.id_sucursal           = bb_lotes.id_sucursal
                       tambores_industria.id_tipotambor         = bb_lotes.id_tipotambor    
                       tambores_industria.nromov                = bb_lotes.nromov
                       tambores_industria.id_articulo           = v_articulo
                       tambores_industria.id_contrato_of        = ""
                       tambores_industria.id_tipocontrato_of    = 0
                       tambores_industria.anio_of               = 0
                       tambores_industria.item_of               = 0
                       tambores_industria.id_orden_entrega      = 0
                       tambores_industria.ITEM_oe               = 0.
            END.
            ELSE DO:
                v_nromov = NEXT-VALUE(nromov).
                FIND FIRST bbLoteAnterior OF tambores_industria NO-LOCK NO-ERROR.
                IF AVAILABLE bbLoteAnterior THEN DO:
                    CREATE bb_lotes.
                    BUFFER-COPY bbLoteAnterior EXCEPT nromov
                                                      id_articulo
                                                      CONTROL_calidad
                                                      microbiologia
                                                      c_usuario
                                                      c_fecha
                                                      c_hora 
                                                TO bb_lotes.

                    ASSIGN bb_lotes.nromov          = v_nromov
                           bb_lotes.id_articulo     = v_articulo
                           bb_lotes.CONTROL_calidad = TRUE
                           bb_lotes.microbiologia   = TRUE
                           bb_lotes.c_usuario       = userid("userdb")
                           bb_lotes.c_fecha         = today
                           bb_lotes.c_hora          = string(time,"HH:MM:SS").
                                                      
                END.
                ELSE DO:
                    CREATE bb_lotes.
                    ASSIGN bb_lotes.id_empresa      = tambores_industria.id_empresa
                           bb_lotes.id_sucursal     = tambores_industria.id_sucursal
                           bb_lotes.id_tipotambor   = 3
                           bb_lotes.nromov          = v_nromov
                           bb_lotes.id_lote         = tambores_industria.id_lote
                           bb_lotes.anio            = tambores_industria.anio
                           bb_lotes.id_articulo     = v_articulo
                           bb_lotes.id_calidad      = tambores_industria.id_calidad
                           bb_lotes.estado_lote     = 1
                           bb_lotes.fecha           = tambores_industria.fecha
                           bb_lotes.CONTROL_calidad = TRUE
                           bb_lotes.microbiologia   = TRUE
                           bb_lotes.c_usuario       = userid("userdb")
                           bb_lotes.c_fecha         = today
                           bb_lotes.c_hora          = string(time,"HH:MM:SS").
                END.
                
                ASSIGN tambores_industria.nromov                = v_nromov
                       tambores_industria.id_articulo           = v_articulo
                       tambores_industria.id_contrato_of        = ""
                       tambores_industria.id_tipocontrato_of    = 0
                       tambores_industria.anio_of               = 0
                       tambores_industria.item_of               = 0
                       tambores_industria.id_orden_entrega      = 0
                       tambores_industria.ITEM_oe               = 0.
            END.

            
            RUN y_gstkcre.p (INPUT tambores_industria.id_empresa,
                             INPUT tambores_industria.id_sucursal,
                             INPUT tambores_industria.id_tipotambor,
                             INPUT tambores_industria.nromov,
                             INPUT tambores_industria.id_tambor,
                             INPUT tambores_industria.id_tambor,
                             INPUT 1) "tambores_industria". 
            /*
            ASSIGN lotes_jugo.id_articulo           = v_articulo
                   /*lotes_jugo.id_contrato_of        = ""
                   lotes_jugo.id_tipocontrato_of    = 0
                   lotes_jugo.anio_of               = 0
                   lotes_jugo.item_of               = 0
                   lotes_jugo.id_orden_entrega      = 0
                   lotes_jugo.ITEM_oe               = 0 */ .
              */
