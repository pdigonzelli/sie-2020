
/*------------------------------------------------------------------------
    File        : volcado.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Jan 02 16:34:15 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER operacion AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER CPARAMETROS AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER RESULTADO  AS CHARACTER NO-UNDO.


DEFINE VAR RES AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE VAR POPERACION AS CHARACTER NO-UNDO.
DEFINE VAR DIR AS CHARACTER NO-UNDO.
DEFINE VAR DUMP AS INTEGER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/*FILE-INFO:FILE-NAME = ".".
DIR = FILE-INFO:FULL-PATHNAME. */

DIR = SESSION:TEMP-DIRECTORY.

MESSAGE DIR CPARAMETROS.

POPERACION = 'python ' + DIR +  operacion + ' "' + CPARAMETROS + '"'.

MESSAGE DIR CPARAMETROS POPERACION.


INPUT THROUGH VALUE (POPERACION) NO-ECHO.
IMPORT UNFORMATTED RES.
INPUT CLOSE.

MESSAGE 'RES' RES .

RESULTADO = RES.


MESSAGE 'RESULTADO' resultado '2' RES.

IF ENTRY(1,RESULTADO,":") = 'ERROR' THEN
DO: 
    RESULTADO = 'ERROR:' + ENTRY(2,RESULTADO,":").
    MESSAGE 'E1' RESULTADO.
    UNDO, RETURN ERROR RESULTADO.
END.

/*
DUMP = INTEGER(RESULTADO) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    UNDO , RETURN ERROR RESULTADO. */

CATCH e AS Progress.Lang.Error :
    RESULTADO =  E:GetMessage(1).
    MESSAGE 'E2' RESULTADO.
    UNDO , RETURN ERROR RESULTADO.
END CATCH.

