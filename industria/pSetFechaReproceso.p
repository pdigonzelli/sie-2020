DEFINE BUFFER buRep FOR tambores_industria .
DEFINE VARIABLE iTime AS INTEGER    NO-UNDO.
CURRENT-WINDOW:WIDTH = 150.

iTime = ETIME(TRUE).

FOR EACH tambores_industria WHERE tambores_industria.fecha_reproceso = ?
                              AND tambores_industria.nromov_destino <> 0
                              AND tambores_industria.fecha >= DATE("01/01/2005")
                              AND (tambores_industria.id_tipotambor = 1 OR tambores_industria.id_tipotambor = 3 OR tambores_industria.id_tipotambor = 4 OR tambores_industria.id_tipotambor = 5).

  FIND FIRST buRep WHERE buRep.nromov = tambores_industria.nromov_destino 
                     AND buRep.fecha >= DATE("01/01/2005")                      
                   NO-LOCK NO-ERROR.
  IF AVAILABLE buRep  THEN DO:
    ASSIGN tambores_industria.fecha_reproceso = buRep.fecha.
  END.
  ELSE DO:
    FIND FIRST cargas WHERE cargas.nromov = tambores_industria.nromov_destino 
                        AND cargas.fecha >= DATE("01/01/2005")
                      NO-LOCK NO-ERROR.
    IF AVAILABLE cargas THEN
      ASSIGN tambores_industria.fecha_reproceso = cargas.fecha.

  END.
  
END.


MESSAGE ETIME / 1000
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

