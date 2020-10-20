DEFINE VARIABLE v_galones AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKil     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iTam AS INTEGER    NO-UNDO.

FIND FIRST lotes_jugo
     WHERE lotes_jugo.id_lote     = 228
       AND lotes_jugo.anio        = 2006
       AND lotes_jugo.id_articulo = 53
    NO-LOCK.

dKil = 0.
iTam = 0.
FOR EACH tambores_industria OF lotes_jugo NO-LOCK.
  iTam = iTam + 1.
  dKil = tambores_industria.kilos_tambor.
END.




FIND LAST inspecciones_lote OF lotes_jugo NO-LOCK NO-ERROR.
IF AVAILABLE inspecciones_lote THEN DO:
     FIND LAST brix WHERE brix.brix <= ROUND(inspecciones_lote.bx_correg,1) NO-LOCK NO-ERROR.
     IF AVAILABLE brix THEN 
     DO:
       IF brix.pe > 0 THEN
         v_galones = iTam * (ROUND((dKil / brix.pe) / 3.785, 2)).
     
       MESSAGE "Peso Esp: " + string(brix.pe) SKIP "galones: " + string(v_galones) SKIP "Brix: " + STRING(inspecciones_lote.bx_correg)
         VIEW-AS 
           ALERT-BOX.
     END.
 END.
