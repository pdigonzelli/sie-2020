
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
FILE-INFO:FILE-NAME = ".".
DIR = FILE-INFO:FULL-PATHNAME + '\'.



POPERACION = 'python ' + DIR + operacion + ' "' + CPARAMETROS + '"'.


INPUT THROUGH VALUE (POPERACION) NO-ECHO.
IMPORT UNFORMATTED RES.
INPUT CLOSE.

RESULTADO = RES.



IF ENTRY(1,RESULTADO,":") = 'ERROR' THEN
DO: 
    RESULTADO = 'ERROR:' + ENTRY(2,RESULTADO,":").
    UNDO, RETURN ERROR RESULTADO.
END.


CATCH e AS Progress.Lang.Error :
    RESULTADO =  E:GetMessage(1).
    UNDO , RETURN ERROR RESULTADO.
END CATCH.

