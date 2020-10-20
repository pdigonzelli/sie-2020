
DEFINE VARIABLE hApp     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLib     AS HANDLE  NO-UNDO.
DEFINE VARIABLE lFlag    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cPath    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cProgram AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hMnu AS WIDGET-HANDLE     NO-UNDO.
  

PROPATH = "..\,..\industria\,,src,..\industria\prowinapi," + PROPATH + "l:\desarrollo\ventas".
 
RUN ..\industria\wTouchMolienda.w PERSISTENT SET hMnu.
RUN initializeObject IN hMnu.
WAIT-FOR CLOSE OF hMnu.
QUIT.
