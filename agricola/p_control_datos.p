DEF VAR v_categoria AS INTEGER.
    
FOR EACH liq_items_tarjas :
    FIND FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_tarjas.id_empresa AND
                                 liq_legajos.legajo = liq_items_tarjas.legajo NO-LOCK NO-ERROR.


    IF liq_items_tarjas.cant_hs_norm > 8 THEN
        DISPLAY liq_items_tarjas.legajo liq_items_tarjas.fecha liq_items_tarjas.cant_hs_norm.

    v_categoria = 0. 
    FIND FIRST liq_tareas WHERE liq_tareas.id_tarea = liq_items_tarjas.id_tarea NO-LOCK NO-ERROR.
    IF NOT AVAILABLE liq_tareas THEN NEXT.


    IF liq_items_tarjas.hs_acond_finca <> 0 AND liq_tareas.id_tipo_automatico = 0 THEN
    DO:
            DISPLAY "Hs Acond no corresponde".
            /*ASSIGN liq_items_tarjas.hs_acond_finca = 0.  */
    END.

    IF liq_tareas.id_tipo_automatico = 1 AND liq_items_tarjas.hs_acond_finca = 0 and
        liq_items_tarjas.cant_horas <> 0 THEN
    DO:
          DISPLAY liq_items_tarjas.id_tarea liq_items_tarjas.cant_horas "Falta Hs Acond".
    END.


    IF liq_tareas.habilita_horas = NO AND liq_items_tarjas.cant_horas <> 0 THEN
    DO:
        DISPLAY "Tarea" liq_items_tarjas.id_tarea liq_tareas.habilita_horas liq_items_tarjas.cant_horas WITH FRAME D DOWN.
        
        /*ASSIGN liq_items_tarjas.cant_horas = 0
               liq_items_tarjas.cant_hs_norm = 0
               liq_items_tarjas.hs_acond_finca = 0
               liq_items_tarjas.hs_plus_tareas_automatico = 0
               liq_items_tarjas.hs_plus_tareas_trabajadas = 0
               liq_items_tarjas.hs_adicionales_tareas_trabajadas = 0
               liq_items_tarjas.id_codigo_abacus_diferencial = 0.*/

    END.


    IF liq_tareas.id_codigo_abacus = 0 THEN
    DO:
          
           CASE liq_tareas.id_categoria:
               WHEN 5 OR WHEN 6 THEN  /* Servicio y Mantenimiento */
               DO:
                   v_categoria = liq_legajos.id_categoria.
               END.
               WHEN 10 THEN /* Hibridos */
               DO:
                   IF liq_items_tarjas.nro_tractor <> 0 OR liq_items_tarjas.nro_maq <> 0 THEN
                       v_categoria = 3.
                     ELSE
                       v_categoria = 2.
               END.
               OTHERWISE
                   v_categoria = liq_tareas.id_categoria.
           END CASE.


            FIND FIRST r_cat_pers_tareas WHERE
                r_cat_pers_tareas.id_convenio = liq_legajos.id_convenio AND
                r_cat_pers_tareas.id_categoria = liq_legajos.id_categoria AND
                r_cat_pers_tareas.id_categoria_tarea = v_categoria NO-LOCK NO-ERROR.
            IF AVAILABLE r_cat_pers_tareas THEN
            DO:
                /* Chequeo Ajustes */
                IF liq_items_tarjas.id_codigo_abacus_diferencial <> r_cat_pers_tareas.id_codigo_abacus_diferencial THEN
                DO:
                    FIND FIRST liq_categorias OF liq_legajos NO-LOCK NO-ERROR.

                    DISPLAY "Ajuste" 
                            liq_legajos.id_categoria
                            liq_categorias.descripcion
                            liq_tareas.id_categoria
                            liq_items_tarjas.nro_tractor
                            liq_items_tarjas.nro_maq
                            v_categoria 
                            liq_items_tarjas.id_codigo_abacus_diferencial FORMAT ">>>9" 
                            r_cat_pers_tareas.id_codigo_abacus_diferencial FORMAT ">>>9" WITH FRAME A DOWN.   

                    /*ASSIGN liq_items_tarjas.id_codigo_abacus_diferencial = r_cat_pers_tareas.id_codigo_abacus_diferencial.*/
                END.
        
                /* Chequeo Codigos Abacus */
                IF liq_items_tarjas.id_codigo_abacus <> r_cat_pers_tareas.id_codigo_abacus THEN
                DO:
                    DISPLAY "Cod.Abacus" 
                            liq_legajos.id_categoria
                            liq_tareas.id_categoria 
                            liq_items_tarjas.id_codigo_abacus FORMAT ">>>9" 
                            r_cat_pers_tareas.id_codigo_abacus FORMAT ">>>9" WITH FRAME B DOWN.
                    
                END.
            END.   
    END.
    ELSE
    DO:
        IF liq_tareas.id_codigo_abacus <> liq_items_tarjas.id_codigo_abacus THEN
        DO:
        DISPLAY "Cod.Fijo"
                liq_items_tarjas.id_tarea
                liq_items_tarjas.cant_horas
                liq_items_tarjas.cantidad 
                liq_legajos.id_categoria
                liq_tareas.id_categoria 
                liq_tareas.id_codigo_abacus FORMAT ">>>9" 
                liq_items_tarjas.id_codigo_abacus FORMAT ">>>9" WITH FRAME C DOWN. 
           /*assign liq_items_tarjas.id_codigo_abacus = liq_tareas.id_codigo_abacus.  */     
        END.
    END.
  

END.
