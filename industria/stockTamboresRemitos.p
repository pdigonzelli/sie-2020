DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.

CURRENT-WINDOW:WIDTH = 150.


FOR EACH stock_historico WHERE YEAR(stock_historico.fecha) <= 2004 AND signo = "-" AND id_tipo_movimiento = 3.
  iSuc = INTEGER(ENTRY(1, REPLACE(datos_adicionales, "Proceso de Remitos", ""), "-")) NO-ERROR.
  iTip = INTEGER(ENTRY(2, REPLACE(datos_adicionales, "Proceso de Remitos", ""), "-")) NO-ERROR.
  iNro = INTEGER(ENTRY(3, REPLACE(datos_adicionales, "Proceso de Remitos", ""), "-")) NO-ERROR.
  FIND FIRST remitos WHERE remitos.id_sucursal = iSuc
                       AND remitos.id_tipo_movsto = iTip
                       AND remitos.nro = iNro
                     NO-LOCK NO-ERROR.
  IF AVAILABLE remitos THEN 
    DISP remitos.fecha_proceso stock_historico.fecha remitos.id_sucursal remitos.nro_comp WITH WIDTH 150.
  ELSE
    DISP stock_historico.fecha datos_adicionales FORMAT "x(40)"WITH WIDTH 150.
  
END.

