DEF VAR v_hs_liq AS DECIMAL.
DEF VAR v_dia_sem AS INTEGER.
DEF VAR v_resto AS INTEGER.
DEF VAR v_desde AS DATE.
DEF VAR v_hasta AS DATE.
DEF BUFFER bres FOR resumen_compensado.
DEF VAR v_total_hs_comp AS DECIMAL.
DEF VAR v_legajo AS INTEGER.

DEF VAR v_fecha_desde AS DATE.
DEF VAR v_fecha_hasta AS DATE.

v_fecha_desde = DATE("16/05/2014").
v_fecha_hasta = DATE("27/05/2014").
v_legajo = 61127.
FOR EACH resumen_compensado WHERE legajo = v_legajo AND
    resumen_compensado.fecha >= v_fecha_desde AND
    resumen_compensado.fecha <= v_fecha_hasta:
    v_hs_liq = (hs_trabajadas - hs_categoria).
    ASSIGN resumen_compensado.hs_adic_liq = v_hs_liq
           resumen_compensado.hs_adic_comp = 0.
    v_dia_sem = WEEKDAY(fecha).



    IF v_hs_liq < 0 THEN
    DO:
        v_total_hs_comp = ABS(v_hs_liq).
        IF  v_dia_sem > 2 THEN v_resto = v_dia_sem - 2.
        v_hasta = resumen_compensado.fecha.
        v_desde = v_hasta - v_resto.
        FOR EACH bres WHERE bres.legajo = resumen_compensado.legajo 
            AND bres.fecha >= v_desde AND bres.fecha <= v_hasta and
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
    END.
END.


FOR EACH resumen_compensado WHERE legajo = v_legajo NO-LOCK:
    v_dia_sem = WEEKDAY(fecha).

    DISPLAY v_dia_sem resumen_compensado.fecha 
        resumen_compensado.hs_adic_liq hs_adic_comp. .

END.

