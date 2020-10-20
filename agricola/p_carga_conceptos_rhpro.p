def temp-table t-temp6 
  FIELD id_concepto AS INTEGER
  FIELD descripcion AS CHARACTER FORMAT "x(25)".

DEF VAR i AS INTEGER.
DEF VAR v-titulo AS CHARACTER. 
DEF VAR v-path AS CHARACTER.
DEF VAR v_inicio AS INTEGER.
DEF VAR v_legajo AS INTEGER.

v-path = "z:\sistemas\sami\util\".


input from VALUE(v-path + "Conceptos-RHpro.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp6.
      import DELIMITER ";" t-temp6.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

/*FOR EACH t-temp6:
    DISPLAY t-temp6.
END.*/


FOR EACH t-temp6 WHERE t-temp6.id_concepto <> 0 NO-LOCK:
    FIND FIRST liq_conceptos WHERE
        liq_conceptos.id_concepto = t-temp6.id_concepto NO-ERROR.
    IF NOT AVAILABLE liq_conceptos THEN
        CREATE liq_conceptos.
    BUFFER-COPY t-temp6 TO liq_conceptos.
END.


MESSAGE "La importacion desde RHPRO fue realizada" VIEW-AS ALERT-BOX WARNING.
