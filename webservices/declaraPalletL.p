
/*------------------------------------------------------------------------
    File        : declaraPallet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Mar 22 16:44:49 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER  VSUC AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  VPALLET AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  VENTORNO AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER CSTATUS AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER CPALLET AS CHARACTER NO-UNDO.

DEFINE VAR RET AS LOGICAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

RUN  PP159INGP1L.P (VSUC, VPALLET, OUTPUT CSTATUS , OUTPUT CPALLET ) NO-ERROR.
IF ERROR-STATUS:ERROR THEN  RETURN ERROR NEW Progress.Lang.AppError(RETURN-VALUE , 550).

CATCH EX AS Progress.Lang.Error :
    RETURN ERROR NEW Progress.Lang.AppError(EX:GETMESSAGE(1) , 550).
END CATCH. 

