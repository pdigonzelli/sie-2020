        FIND calidades OF items_contratos NO-LOCK NO-ERROR.
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
        FOR EACH lotes_jugo where lotes_jugo.id_contrato_of      = items_contratos.id_contrato
                              and lotes_jugo.id_tipocontrato_of  = items_contratos.id_tipo_contrato
                              and lotes_jugo.anio_of             = items_contratos.anio
                              no-lock.
            v_lotes_of = v_lotes_of + " " + string(lotes_jugo.id_lote) + "/" + STRING(YEAR(lotes_jugo.fecha)).
            FOR EACH tambores_industria OF lotes_jugo NO-LOCK.
                v_kgs_asignados_of = v_kgs_asignados_of + tambores_industria.kilos_tambor.
            END.
        END.
          
        FOR EACH lotes_aceite where lotes_aceite.id_contrato_of = items_contratos.id_contrato
                                and lotes_aceite.id_tipocontrato_of = items_contratos.id_tipo_contrato
                                and lotes_aceite.anio_of = items_contratos.anio
                                no-lock.
            v_lotes_of = v_lotes_of + " " + string(lotes_aceite.id_lote) + "/" + STRING(YEAR(lotes_aceite.fecha)).
            FOR EACH tambores_industria OF lotes_aceite NO-LOCK.
                v_kgs_asignados_of = v_kgs_asignados_of + tambores_industria.kilos_tambor.
            END.
        END.
        v_kgs_acum_asignados_of = v_kgs_acum_asignados_of + v_kgs_asignados_of.
        
        tiene-oe = FALSE.
        FOR EACH items_orden_entrega OF items_contratos NO-LOCK.
                
            v_lotes = "".
            v_kgs_asignados = 0.    
            FOR EACH lotes_jugo OF items_orden_entrega.
                v_lotes = v_lotes + " " + string(lotes_jugo.id_lote) + "/" + STRING(YEAR(lotes_jugo.fecha)).
                FOR EACH tambores_industria OF lotes_jugo NO-LOCK.
                    v_kgs_asignados = v_kgs_asignados + tambores_industria.kilos_tambor.
                END.
            END.
            
            FOR EACH lotes_aceite OF items_orden_entrega.
                v_lotes = v_lotes + " " + string(lotes_aceite.id_lote) + "/" + STRING(YEAR(lotes_aceite.fecha)).
                FOR EACH tambores_industria OF lotes_aceite NO-LOCK.
                    v_kgs_asignados = v_kgs_asignados + tambores_industria.kilos_tambor.
                END.
            END.
            v_kgs_acum_asignados = v_kgs_acum_asignados + v_kgs_asignados.
            {..\industria\i_calculo_prog_produccion.i}
            tiene-oe = TRUE.
        END.
        
        IF NOT tiene-oe THEN DO:
              {..\industria\i_calculo_prog_produccion.i}
        END.
        
        v_lotes = "".
        v_kgs_items = 0.
        v_kgs_asignados = 0.
        v_kgs_pendientes = 0.
        v_lotes_of = "".
        v_kgs_asignados_of = 0.
        v_kgs_pendientes_of = 0.
