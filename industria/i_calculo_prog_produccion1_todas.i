        FIND FIRST calidades OF items_contratos NO-LOCK NO-ERROR.
        FIND FIRST productos_terminados OF items_contratos NO-LOCK NO-ERROR.
        FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
        IF items_contratos.id_articulo = 52 OR
           items_contratos.id_articulo = 53 OR
           items_contratos.id_articulo = 71 THEN DO:
            FIND r_productos_calidad_envase OF items_contratos NO-LOCK NO-ERROR.
            IF AVAILABLE r_productos_calidad_envase THEN DO:
                v_kgs_items =  items_contratos.cantidad * r_productos_calidad_envase.kilos.
            END.
        END.
        ELSE
        DO:
            FIND r_envases_prod OF items_contratos NO-LOCK NO-ERROR.
            IF AVAILABLE r_envases_prod THEN DO:
                v_kgs_items =  items_contratos.cantidad * r_envases_prod.kilos.
            END.
        END.

        v_lotes_of = "".
        v_kgs_asignados_of = 0.
        FOR EACH tambores_industria WHERE tambores_industria.id_contrato_of      = items_contratos.id_contrato
                                      AND tambores_industria.id_tipocontrato_of  = items_contratos.id_tipo_contrato
                                      AND tambores_industria.anio_of             = items_contratos.anio
                                    NO-LOCK
                                      BREAK BY tambores_industria.id_lote.
            IF FIRST-OF(tambores_industria.id_lote) THEN DO:
                v_lotes_of = v_lotes_of + " " + string(tambores_industria.id_lote) + "/" + STRING(YEAR(tambores_industria.fecha)).
            END.
            
            v_kgs_asignados_of = v_kgs_asignados_of + tambores_industria.kilos_tambor.
        END.
          
        v_kgs_acum_asignados_of = v_kgs_acum_asignados_of + v_kgs_asignados_of.
        
        {..\industria\i_calculo_prog_produccion_todas.i}
        
        v_lotes = "".
        v_kgs_items = 0.
        v_kgs_asignados = 0.
        v_kgs_pendientes = 0.
        v_lotes_of = "".
        v_kgs_asignados_of = 0.
        v_kgs_pendientes_of = 0.
