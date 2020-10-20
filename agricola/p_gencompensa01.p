DEF INPUT PARAMETER p_empresa AS INTEGER.
DEF INPUT PARAMETER p_desde AS DATE.
DEF INPUT PARAMETER p_hasta AS DATE.
DEF INPUT PARAMETER p_total AS INTEGER.

DEF VAR v_desde AS DATE.
DEF VAR i AS INTEGER.
DEF VAR v_total_hs_trab AS DECIMAL.
DEF VAR v_total_hs_cat AS DECIMAL.
DEF BUFFER bresges FOR resumen_gestion.
DEF VAR v_fecha_compensada AS DATE.
DEF VAR v-cuenta AS INTEGER.
DEF VAR v-asigna AS INTEGER.
DEF VAR v_fecha AS DATE.
DEF VAR v-toma AS INTEGER.
DEF VAR v-valor AS INTEGER.
DEF VAR v-total-asignado AS DECIMAL.
DEF VAR v_hs_adic AS DECIMAL.
DEF VAR v_acum_hs_adic AS DECIMAL.

DEF VAR v_hs_liq AS DECIMAL.
DEF VAR v_dia_sem AS INTEGER.
DEF VAR v_resto AS INTEGER.
DEF VAR v_fecha_desde AS DATE.
DEF VAR v_fecha_hasta AS DATE.
DEF BUFFER bres FOR resumen_compensado.
DEF VAR v_total_hs_comp AS DECIMAL.


v-cuenta = 0.
FOR EACH liq_items_tarjas WHERE liq_items_tarjas.id_empresa = p_empresa AND
    liq_items_tarjas.fecha >= p_desde AND liq_items_tarjas.fecha <= p_hasta NO-LOCK, 
    FIRST liq_tareas OF liq_items_tarjas WHERE liq_tareas.liquida = YES NO-LOCK BREAK BY id_empresa BY legajo:

    IF LAST-OF(legajo) THEN
    DO:

        v_desde = p_desde.
        DO i = 1 TO p_total:
            FIND FIRST resumen_gestion WHERE resumen_gestion.id_empresa = liq_items_tarjas.id_empresa AND
                resumen_gestion.legajo = liq_items_tarjas.legajo AND
                resumen_gestion.fecha = v_desde NO-ERROR.
            IF NOT AVAILABLE resumen_gestion THEN
            DO:
                CREATE resumen_gestion.
                ASSIGN resumen_gestion.id_empresa = liq_items_tarjas.id_empresa
                       resumen_gestion.legajo = liq_items_tarjas.legajo
                       resumen_gestion.fecha = v_desde.
                v_desde = v_desde + 1.
            END.
        END.
    END. 
END.




FOR EACH resumen_gestion WHERE resumen_gestion.id_empresa = p_empresa AND
    resumen_gestion.fecha >= p_desde AND resumen_gestion.fecha <= p_hasta 
    BREAK BY resumen_gestion.id_empresa BY resumen_gestion.legajo BY resumen_gestion.fecha:

       FOR EACH liq_items_tarjas WHERE liq_items_tarjas.id_empresa = resumen_gestion.id_empresa AND
           liq_items_tarjas.legajo = resumen_gestion.legajo AND
           liq_items_tarjas.fecha = resumen_gestion.fecha  NO-LOCK:
           
           ASSIGN resumen_gestion.hs_trabajadas = resumen_gestion.hs_trabajadas + liq_items_tarjas.cant_horas
                  resumen_gestion.hs_categoria = resumen_gestion.hs_categoria + liq_items_tarjas.cant_hs_norm
                  resumen_gestion.hs_ajuste = resumen_gestion.hs_ajuste + (resumen_gestion.hs_trabajadas - resumen_gestion.hs_categoria)
                  resumen_gestion.hs_adicionales = resumen_gestion.hs_adicionales + liq_items_tarjas.hs_adicionales_tareas_trabajadas.

           IF resumen_gestion.hs_categoria > 8 THEN
           DO:
               ASSIGN resumen_gestion.hs_categoria = 8
                      resumen_gestion.hs_adicionales = resumen_gestion.hs_adicionales + (resumen_gestion.hs_trabajadas - resumen_gestion.hs_categoria).
           END.
       END.
END.



FOR EACH resumen_gestion WHERE resumen_gestion.id_empresa = p_empresa AND
    resumen_gestion.fecha >= p_desde AND resumen_gestion.fecha <= p_hasta BREAK BY resumen_gestion.id_empresa BY resumen_gestion.legajo BY resumen_gestion.fecha:
    
    /*MESSAGE resumen_gestion.fecha v_total_hs_trab v_total_hs_adic p_total VIEW-AS ALERT-BOX. */
    
    IF LAST-OF(resumen_gestion.legajo) THEN
    DO:
           v_fecha = p_desde.
           REPEAT WHILE  v_fecha <= p_hasta:
                   IF weekday(v_fecha) = 1 THEN NEXT.

                   FIND FIRST resumen_compensado WHERE resumen_compensado.id_empresa = resumen_gestion.id_empresa AND
                              resumen_compensado.fecha = v_fecha AND
                              resumen_compensado.legajo = resumen_gestion.legajo NO-ERROR.
                   IF NOT AVAILABLE resumen_compensado THEN
                   DO:
                       CREATE resumen_compensado.
                       ASSIGN resumen_compensado.id_empresa = resumen_gestion.id_empresa
                              resumen_compensado.legajo = resumen_gestion.legajo
                              resumen_compensado.fecha = v_fecha.


                       FIND FIRST bresges WHERE bresges.id_empresa = resumen_compensado.id_empresa AND
                                                bresges.fecha = resumen_compensado.fecha AND
                                                bresges.legajo = resumen_compensado.legajo NO-LOCK NO-ERROR.
                       IF AVAILABLE bresges THEN
                           ASSIGN resumen_compensado.hs_trabajadas = bresges.hs_trabajadas
                                  resumen_compensado.hs_trab_norm = bresges.hs_categoria.

                       IF resumen_compensado.hs_trab_norm > 8 THEN resumen_compensado.hs_trab_norm = 8.
                   END.
            v_fecha = v_fecha + 1.
           END.
    END.
END.

/* Proceso compensacion */

v_hs_adic = 0.
v_total_hs_trab = 0.
v_total_hs_cat = 0.

FOR EACH resumen_compensado WHERE resumen_compensado.fecha >= p_desde and
    resumen_compensado.fecha <= p_hasta BREAK BY resumen_compensado.legajo BY resumen_compensado.fecha:
    v_total_hs_trab = v_total_hs_trab + resumen_compensado.hs_trabajadas.
        IF resumen_compensado.hs_trabajadas >= 8 THEN
        DO:
            IF (v_total_hs_cat + 8) <= v_total_hs_trab  THEN
                 ASSIGN resumen_compensado.hs_categoria = 8.
               ELSE
                 ASSIGN resumen_compensado.hs_categoria = (v_total_hs_trab - v_total_hs_cat).

            v_hs_adic = resumen_compensado.hs_trabajadas - resumen_compensado.hs_categoria.
            v_acum_hs_adic = v_acum_hs_adic + v_hs_adic.
        END.
        ELSE
        DO:
           IF (resumen_compensado.hs_trabajadas + v_acum_hs_adic) >= 8 THEN
           DO:
               IF (v_total_hs_cat + 8) <= v_total_hs_trab  THEN
                ASSIGN resumen_compensado.hs_categoria = 8.
               ELSE
                ASSIGN resumen_compensado.hs_categoria = (v_total_hs_trab - v_total_hs_cat).

               v_hs_adic = (resumen_compensado.hs_trabajadas + v_acum_hs_adic) - 8.
               v_acum_hs_adic = v_acum_hs_adic - v_hs_adic.
           END.
           ELSE
           DO:
               ASSIGN resumen_compensado.hs_categoria = resumen_compensado.hs_trabajada + v_acum_hs_adic.
               v_hs_adic = 0.
               v_acum_hs_adic = 0.
    
           END.
        END.

        v_total_hs_cat = v_total_hs_cat + resumen_compensado.hs_categoria.

        ASSIGN resumen_compensado.hs_adic_liq = v_total_hs_trab - v_total_hs_cat.

    /*MESSAGE resumen_compensado.fecha v_hs_adic v_acum_hs_adic SKIP
            v_total_hs_trab v_total_hs_cat VIEW-AS ALERT-BOX.*/

    IF LAST-OF(resumen_compensado.legajo) THEN
    DO:
        v_hs_adic = 0.
        v_acum_hs_adic = 0.
        v_total_hs_trab = 0.
        v_total_hs_cat = 0.
    END.

END.



 FOR EACH resumen_compensado WHERE  resumen_compensado.fecha >= p_desde and
         resumen_compensado.fecha <= p_hasta BREAK 
        BY resumen_compensado.legajo 
        BY resumen_compensado.fecha:

    v_hs_liq = (resumen_compensado.hs_trabajadas - resumen_compensado.hs_categoria).
    ASSIGN resumen_compensado.hs_adic_liq = v_hs_liq
           resumen_compensado.hs_adic_comp = 0.
    v_dia_sem = WEEKDAY(resumen_compensado.fecha).
    IF v_hs_liq < 0 THEN
    DO:
        v_total_hs_comp = ABS(v_hs_liq).
        IF  v_dia_sem > 2 THEN v_resto = v_dia_sem - 2.
        v_fecha_hasta = resumen_compensado.fecha.
        v_fecha_desde = v_fecha_hasta - v_resto.

        FOR EACH bres WHERE bres.legajo = resumen_compensado.legajo 
            AND bres.fecha >= v_fecha_desde AND bres.fecha <= v_fecha_hasta and
                abs(bres.hs_adic_liq) > 0 AND v_total_hs_comp > 0 BY fecha:


              IF v_total_hs_comp > bres.hs_adic_liq THEN
                DO:
                    ASSIGN bres.hs_adic_comp = bres.hs_adic_liq
                           bres.hs_adic_liq = 0.

                    IF bres.hs_adic_comp > 0 THEN
                    v_total_hs_comp = v_total_hs_comp - bres.hs_adic_comp.
                    ELSE
                    v_total_hs_comp = 0.

                END.
                ELSE
                DO:
                    IF v_total_hs_comp > 0 THEN
                    DO:
                        ASSIGN bres.hs_adic_comp = v_total_hs_comp.
                               bres.hs_adic_liq = bres.hs_adic_liq - v_total_hs_comp.
                        v_total_hs_comp = 0.
                    END.
                END. 
        END.

        IF v_total_hs_comp = 0 THEN ASSIGN resumen_compensado.hs_adic_liq = 0
                                           resumen_compensado.hs_adic_comp = 0.
                               ELSE ASSIGN resumen_compensado.hs_adic_liq = (v_total_hs_comp * -1). 
    END.
END.



