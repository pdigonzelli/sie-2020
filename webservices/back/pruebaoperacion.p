
/*------------------------------------------------------------------------
    File        : pruebaoperacion.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Feb 24 13:31:53 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE VAR operacion AS CHARACTER NO-UNDO.
DEFINE VAR RES AS CHARACTER NO-UNDO.

operacion = 'python d:\desarrollos\webservices\pp140ingn.py "SPL,424983,30300010101,0000000778,200,KG,1109,,,EUREKA,25F0101,ESPINILLO I,FAMAILLA,UP-TU-0172-013,2,20151215,1,20160225,,NOUE,NOUSA,NOCHINA,,"'.
RUN operacion.P (OPERACION,'',OUTPUT RES).

CATCH E AS  Progress.Lang.Error :
    MESSAGE E:GETMESSAGE(1) VIEW-AS ALERT-BOX.
END CATCH.

