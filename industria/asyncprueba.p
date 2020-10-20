DEFINE VARIABLE hServer AS HANDLE    NO-UNDO.
DEFINE VARIABLE hProc1  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hProc2  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAsync  AS HANDLE    NO-UNDO.
DEFINE VARIABLE lRet    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iLoop   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cData   AS CHARACTER NO-UNDO.

CREATE SERVER hServer.

lRet = hServer:CONNECT("-AppService asindustria -H 192.168.1.231", "", "") NO-ERROR.

IF lRet = FALSE THEN
    DO:
        MESSAGE "Connection Failed" VIEW-AS ALERT-BOX.
        RETURN.
    END.

RUN libReportes.p PERSISTENT SET hProc1 
                   ON hServer TRANSACTION DISTINCT.

PROCESS EVENTS.
RUN callReporteStock IN hProc1 ASYNCHRONOUS SET hAsync 
          EVENT-PROCEDURE "CallCompleted" (96).

/**
 ** Four requests were queued for execution on the AppServer.  We
 ** are going to "blow 'em all out of the water" now.
 **/

WAIT-FOR F10 OF THIS-PROCEDURE.

DELETE PROCEDURE hProc1.
hServer:DISCONNECT().
DELETE OBJECT hServer.


PROCEDURE CallCompleted:
  MESSAGE "completed" VIEW-AS ALERT-BOX.
  APPLY "F10" TO TARGET-PROCEDURE.

END PROCEDURE.



