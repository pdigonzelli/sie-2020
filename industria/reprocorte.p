CURRENT-WINDOW:WIDTH = 150.


DEFINE BUFFER buffA FOR tambores_industria.
DEFINE BUFFER buffB FOR cargas.
DEFINE VARIABLE dA AS DATE       NO-UNDO.
DEFINE VARIABLE dB AS DATE       NO-UNDO.
DEFINE VARIABLE i  AS INTEGER    NO-UNDO.
DEFINE VARIABLE k  AS DECIMAL    NO-UNDO.

dA = DATE("01/09/2005").
dB = DATE("30/09/2005").



FOR EACH buffb WHERE buffb.fecha < dA
                 AND buffb.fecha >= DATE("01/08/2005"), 
    EACH buffa WHERE buffa.nromov_destino = buffb.nromov
                 AND buffa.fecha_reproceso > dA
                 AND (buffa.id_tipotambor = 1 OR buffa.id_tipotambor = 3)
               BREAK BY buffa.nromov.
  i = i + 1.
  k = k + buffa.kilos_tambor.
  IF LAST-OF(buffa.nromov) THEN DO:
    DISP buffa.id_lote buffa.anio buffa.id_tipotambor buffa.id_articulo buffa.id_sucursal i k buffa.fecha_reproceso buffb.fecha WITH WIDTH 150.
    i = 0.
    k = 0.
  END.
  
END.


