
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
DEFINE OUTPUT PARAMETER RESULTADO  AS CHARACTER. 


DEFINE VAR RES AS CHARACTER NO-UNDO FORMAT 'X(40)'.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */




INPUT THROUGH VALUE (OPERACION) NO-ECHO.
IMPORT UNFORMATTED RES.
INPUT CLOSE.

RESULTADO = RES.


IF ENTRY(1,RESULTADO,":") = 'ERROR' THEN
DO: 
    RETURN ERROR  NEW Progress.Lang.AppError (ENTRY(2,RESULTADO,":"), 550).
END.
CATCH e AS Progress.Lang.Error :
    RESULTADO = 'ERROR:' + E:GetMessage(1).
    UNDO , THROW E.
END CATCH.

