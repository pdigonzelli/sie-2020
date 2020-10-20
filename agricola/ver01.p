       FIND FIRST liq_tareas WHERE liq_tareas.id_tarea = RowObjUpd.id_tarea NO-LOCK NO-ERROR.
       IF AVAILABLE liq_tareas THEN 
          DO:
             IF NOT liq_tareas.descripcion MATCHES ("*tareas c*") THEN
             DO:
                   IF RowObjUpd.cant_horas > 8 THEN 
                    DO:
                     CASE liq_tareas.id_tipo_excedente:
                         WHEN 1 THEN
                         DO:
                             ASSIGN RowObjUpd.cant_hs_norm = 8
                                    RowObjUpd.hs_adicionales_tareas_trabajadas = RowObjUpd.cant_horas - 8.
                         END.
                         WHEN 2 THEN 
                         DO:
                             ASSIGN RowObjUpd.cant_hs_norm = 8
                                    RowObjUpd.hs_plus_tareas_trabajadas = RowObjUpd.cant_horas - 8.
                         END.
                         OTHERWISE
                             DO:
                                 ASSIGN RowObjUpd.cant_hs_norm = 8
                                        RowObjUpd.hs_adicionales_tareas_trabajadas = RowObjUpd.cant_horas - 8.
                             END.
                     END CASE.
                    END.
                    ELSE RowObjUpd.cant_hs_norm = RowObjUpd.cant_horas.


                    IF liq_tareas.genera_hs_auto = YES THEN
                     DO:
                       IF liq_tareas.de_ha <> 0 THEN
                       DO:
                           IF liq_tareas.pulverizacion = YES THEN
                             RowObjUpd.hs_acond_finca = (liq_tareas.cant_ha * RowObjUpd.cant_hs_norm) / liq_tareas.de_ha. 
                            ELSE
                             RowObjUpd.hs_plus_tareas_auto = (liq_tareas.cant_ha * RowObjUpd.cant_hs_norm) / liq_tareas.de_ha. 
        
                       END.
                     END.
                     IF liq_tareas.id_codigo_abacus <> 0 AND liq_tareas.habilita_cantidad = YES THEN
                         RowObjUpd.id_codigo_abacus_cantidad = liq_tareas.id_codigo_abacus.
                     
                     
                     FIND FIRST categorias_tareas OF liq_tareas NO-LOCK NO-ERROR.
                     IF AVAILABLE categorias_tareas THEN
                     DO:
                        IF categorias_tareas.id_categoria <> 10 THEN
                            v_categoria = categorias_tareas.id_categoria.
                          ELSE
                          DO:
                             IF RowObject.nro_maquina = 0 AND
                                RowObject.nro_tractor = 0  THEN
                                 v_categoria = 2.
                               ELSE
                                 v_categoria = 3.
                          END.
                     END.
        
                   IF liq_tareas.id_codigo_abacus = 0 OR (liq_tareas.id_codigo_abacus <> 0 AND liq_tareas.habilita_horas = YES) THEN
                      DO:
                          FIND FIRST r_cat_pers_tareas WHERE
                              r_cat_pers_tareas.id_convenio = liq_legajos.id_convenio AND
                              r_cat_pers_tareas.id_categoria = liq_legajos.id_categoria AND
                              r_cat_pers_tareas.id_categoria_tarea = v_categoria NO-LOCK NO-ERROR.
                          IF AVAILABLE r_cat_pers_tareas THEN
                          DO:
                             ASSIGN RowObjUpd.id_codigo_abacus = r_cat_pers_tareas.id_codigo_abacus
                                    RowObjUpd.id_diferencial = r_cat_pers_tareas.id_diferencial
                                    RowObject.id_codigo_abacus_diferencial = r_cat_pers_tareas.id_codigo_abacus_diferencial.
                             IF liq_tareas.id_tipo_automatico <> 0 THEN
                             DO:
                                 FIND FIRST tipo_automatico WHERE tipo_automatico.id_tipo_automatico = liq_tareas.id_tipo_automatico NO-LOCK NO-ERROR.
                                 IF AVAILABLE tipo_automatico THEN
                                     RowObject.id_codigo_abacus_adicional = tipo_automatico.id_codigo_abacus.
                             END.
            
                          END.
                      END.
        
                /* Chequeo que el lote sea valido */
            
                IF liq_tareas.id_tipo_dato = 2 THEN
                DO:
                    FIND FIRST lotes_plantacion WHERE lotes_plantacion.id_proveedor = RowObjUpd.id_proveedor AND
                        lotes_plantacion.id_origen = RowObjUpd.id_origen AND
                        lotes_plantacion.id_lote = RowObjUpd.id_lote and
                        lotes_plantacion.estado = YES NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE lotes_plantacion THEN
                           RETURN "Lote no valido - Verifique".
                END.
            END.
            ELSE ASSIGN rowObjUpd.cant_hs_norm = 0
                        RowObjUpd.hs_adicionales_tareas_trabajadas = rowObjUpd.cant_horas.
