
/*------------------------------------------------------------------------
    File        : pp159ing.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Feb 20 20:20:51 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER VPALLET AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

DEFINE VAR DIR AS CHARACTER NO-UNDO.  
DEFINE VAR operacion AS CHARACTER NO-UNDO.
DEFINE VAR CPARAMETROS AS CHARACTER NO-UNDO.  
DEFINE VAR RES AS CHARACTER NO-UNDO. 



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FILE-INFO:FILE-NAME = ".".
DIR = FILE-INFO:FULL-PATHNAME.


DO:

  OPERACION = 'pp377ing.py'.
  CPARAMETROS =  VPALLET.

  RUN OPERACION.P (INPUT operacion , INPUT CPARAMETROS, OUTPUT  RES) NO-ERROR.  
  MESSAGE RETURN-VALUE RES .
  IF ERROR-STATUS:ERROR THEN
    RETURN ERROR RETURN-VALUE.   
  CSTATUS = RES.
END.
CATCH EX  AS Progress.Lang.Error :
    CSTATUS = EX:GETMESSAGE(1).
    UNDO , THROW EX.
END CATCH.
