DEF INPUT PARAMETER p_empresa AS INTEGER.
DEF INPUT PARAMETER p_desde AS DATE.
DEF INPUT PARAMETER p_hasta AS DATE.
DEF INPUT PARAMETER p_total AS INTEGER.

DEF VAR v_desde AS DATE.
DEF VAR i AS INTEGER.
DEF VAR v_total_hs_trab AS DECIMAL.
DEF VAR v_total_hs_adic AS DECIMAL.
DEF VAR v_hs_adic AS DECIMAL.
DEF BUFFER bresges FOR resumen_gestion.
DEF VAR v_fecha_compensada AS DATE.
DEF VAR v-cuenta AS INTEGER.
DEF VAR v-asigna AS INTEGER.
DEF VAR v_fecha AS DATE.
DEF VAR v-toma AS INTEGER.
DEF VAR v-valor AS INTEGER.
DEF VAR v-total-asignado AS DECIMAL.


v-cuenta = 0.
FOR EACH liq_items_tarjas WHERE liq_items_tarjas.id_empresa = p_empresa AND
    liq_items_tarjas.fecha >= p_desde AND liq_items_tarjas.fecha <= p_hasta AND
    liq_items_tarjas.por_compensacion = NO NO-LOCK BREAK BY id_empresa BY legajo:

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


       END.
END.


/* Proceso compensacion */

v_total_hs_trab = 0.
v_total_hs_adic = 0.

FOR EACH resumen_gestion WHERE resumen_gestion.id_empresa = p_empresa AND
    resumen_gestion.fecha >= p_desde AND resumen_gestion.fecha <= p_hasta AND
    resumen_gestion.legajo = 61127
      BREAK BY resumen_gestion.id_empresa BY resumen_gestion.legajo BY resumen_gestion.fecha:
    
    v_total_hs_trab = v_total_hs_trab + resumen_gestion.hs_trabajadas.
    v_total_hs_adic = v_total_hs_adic + resumen_gestion.hs_ajuste.


    /*MESSAGE resumen_gestion.fecha v_total_hs_trab v_total_hs_adic p_total VIEW-AS ALERT-BOX. */
    
    IF LAST-OF(resumen_gestion.legajo) THEN
    DO:

        FOR EACH bresges WHERE bresges.id_empresa = p_empresa AND
                 bresges.fecha >= p_desde AND bresges.fecha <= p_hasta AND
                 bresges.legajo = resumen_gestion.legajo NO-LOCK BY bresges.fecha:
            CREATE resumen_compensado.
            BUFFER-COPY bresges EXCEPT hs_ajuste hs_adicionales TO resumen_compensado.
        END.


        IF v_total_hs_adic > 0 THEN
        DO:


           /* Busco d¡a a asignar hs adicionales */
           v-asigna = v_total_hs_adic.
           v_fecha = p_desde - 1.
           

           REPEAT WHILE v-asigna > 0 AND v_fecha <= p_hasta:
                   
                   v_fecha  = v_fecha + 1.
                   IF weekday(v_fecha) = 1 THEN NEXT.


                   FIND FIRST resumen_compensado WHERE resumen_compensado.id_empresa = resumen_gestion.id_empresa AND
                              resumen_compensado.fecha = v_fecha AND
                              resumen_compensado.legajo = resumen_gestion.legajo NO-ERROR.
                   IF NOT AVAILABLE resumen_compensado THEN
                   DO:
                       CREATE resumen_compensado.
                       BUFFER-COPY bresges EXCEPT hs_categoria hs_adicionales TO resumen_compensado.
                   END.

                   FIND FIRST  bresges WHERE bresges.id_empresa = resumen_gestion.id_empresa AND
                        bresges.legajo = resumen_gestion.legajo AND
                        bresges.fecha = v_fecha NO-LOCK NO-ERROR.
                   IF AVAILABLE bresges THEN
                   DO:
                        IF bresges.hs_categoria >= 8  THEN
                                v-valor = 8.
                   END.

                   MESSAGE v_fecha v-valor VIEW-AS ALERT-BOX.

                   ASSIGN resumen_compensado.hs_categoria = v-valor
                          v-asigna = v-asigna - v-valor.
           END.

           v_total_hs_trab = 0.
           v_total_hs_adic = 0.
           v-total-asignado = 0.

     END.
  END.
END.

