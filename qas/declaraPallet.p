
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

DEFINE VAR hAppSrv AS HANDLE NO-UNDO.
DEFINE VAR RET AS LOGICAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

    
CREATE SERVER hAppSrv.


IF VENTORNO = 'PRD' THEN
    ret = hAppSrv:CONNECT("-H samiprogdesa -S 5162 -AppService sap2").
ELSE
    ret = hAppSrv:CONNECT("-H samiprogdesa -S 5162 -AppService sap3").

RUN  PP159INGP1.P ON hAppSrv  TRANSACTION DISTINCT (VSUC, VPALLET, OUTPUT CSTATUS , OUTPUT CPALLET ) NO-ERROR.
IF ERROR-STATUS:ERROR THEN  RETURN ERROR NEW PROGRESS.LANG.APPERROR(RETURN-VALUE , 550).

CATCH EX AS Progress.Lang.Error :
    RETURN ERROR NEW PROGRESS.LANG.APPERROR(EX:GETMESSAGE(1) , 550).
END CATCH. 
FINALLY.
    IF hAppSrv:CONNECTED () THEN
        ret = hAppSrv:DISCONNECT().
    DELETE OBJECT hAppSrv.
END FINALLY.

