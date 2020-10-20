DEF INPUT PARAMETER p_id_empresa AS INTEGER.
DEF INPUT PARAMETER p_fecha_desde AS DATE. 
DEF INPUT PARAMETER p_fecha_hasta AS DATE. 
 
DEF VAR v_archivo AS CHARACTER.
DEF VAR v_empresa AS CHARACTER FORMAT "x(60)".
DEF VAR v_fecha AS CHARACTER FORMAT "x(70)".
DEF VAR v_total_trab AS DECIMAL.
DEF VAR v_total_cat AS DECIMAL.
DEF VAR v_hs_norm_comp AS DECIMAL.
DEF VAR v_hs_adic_comp AS DECIMAL.
DEF VAR v_hs_no_autorizadas AS DECIMAL.

v_empresa = "Empresa: " + STRING(p_id_empresa).
v_fecha = "Periodo : " + STRING(p_fecha_desde) + " - " + STRING(p_fecha_hasta).

v_archivo = "z:\temp\compensa.txt".

output to value(v_archivo).
  PUT "Reporte de Compensaci¢n".
  PUT SKIP.
  PUT v_empresa.
  PUT SKIP.
  PUT v_fecha.
  PUT SKIP.
  put "Legajo; Nombre; Fecha;Hs Trab;Hs Norm;Hs Norm;Hs Norm;Hs Adic;Hs Adic;Hs Adic".
  PUT SKIP.
  put "      ;       ;      ;Gest   ;       ;Comp   ;Total  ;Comp   ;Liq    ;No Aut".
  PUT SKIP.

for each resumen_compensado WHERE resumen_compensado.fecha >= p_fecha_desde AND
                                  resumen_compensado.fecha <= p_fecha_hasta NO-LOCK BREAK BY resumen_compensado.legajo BY resumen_compensado.fecha:
     
     v_hs_norm_comp = (resumen_compensado.hs_categoria - resumen_compensado.hs_trab_norm).
     IF v_hs_norm_comp < 0 THEN v_hs_norm_comp = 0.
     
     v_total_trab = v_total_trab + resumen_compensado.hs_trabajadas.
     v_total_cat = v_total_cat + resumen_compensado.hs_categoria.

     IF  (resumen_compensado.hs_trabajadas - resumen_compensado.hs_categoria) < 0 THEN
         v_hs_adic_comp = (resumen_compensado.hs_trabajadas - resumen_compensado.hs_categoria).
       ELSE
         v_hs_adic_comp = 0.

     FIND FIRST liq_legajos WHERE 
         liq_legajos.id_empresa =  resumen_compensado.id_empresa AND
         liq_legajos.legajo = resumen_compensado.legajo NO-LOCK NO-ERROR.
     PUT resumen_compensado.legajo.
     PUT ";".
     IF AVAILABLE liq_legajos THEN PUT liq_legajos.apellido_nombre.
                              ELSE PUT "".
     PUT ";".
     PUT resumen_compensado.fecha.
     PUT ";".
     PUT resumen_compensado.hs_trabajadas.
     PUT ";".
     PUT resumen_compensado.hs_trab_norm.
     PUT ";".
     PUT v_hs_norm_comp.
     PUT ";".
     PUT resumen_compensado.hs_categoria.
     PUT ";".
     PUT (resumen_compensado.hs_adic_comp * -1).
     PUT ";".
     PUT resumen_compensado.hs_adic_liq.
     PUT ";".
     PUT v_hs_no_autorizadas.
     PUT ";".

     IF LAST-OF(resumen_compensado.legajo) THEN
     DO:
        /* PUT (v_total_trab - v_total_cat).  */
         v_total_trab = 0.
         v_total_cat = 0.
         v_hs_no_autorizadas = 0.
         PUT SKIP.
     END.

     IF LAST-OF(resumen_compensado.fecha) THEN
         PUT SKIP.

END.
OUTPUT CLOSE.

run p_texto_a_excel.p (input "TEXT;" + v_archivo).
