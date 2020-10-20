DEFINE INPUT PARAMETER pcMensaje  AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE INPUT PARAMETER pcDatos    AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE INPUT PARAMETER pcPrograma AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE INPUT PARAMETER pcUsuario  AS CHARACTER FORMAT "X(15)"  NO-UNDO.

DEFINE VARIABLE Fecha    AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE Mensaje  AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE Datos    AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE Programa AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE Usuario  AS CHARACTER FORMAT "X(15)"  NO-UNDO.

Fecha    = STRING(TODAY) + " " + STRING(TIME, "HH:MM:SS").
Mensaje  = pcMensaje.
Datos    = pcDatos.
Programa = pcPrograma.
Usuario  = pcUsuario.

OUTPUT TO ..\industria\industria.LOG APPEND.
DISP SKIP(1) "--------------" SKIP(0).
DISP Fecha Mensaje Datos Programa Usuario WITH 1 COL SIDE-LABELS .
OUTPUT CLOSE.