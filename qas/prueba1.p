
/*------------------------------------------------------------------------
    File        : prueba1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Mar 15 21:03:18 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR RES AS CHARACTER NO-UNDO.
DEFINE VAR Poperacion AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

Poperacion = 'python D:\DESARROLLOS\WEBSERVICES\pp141ingn.py  "SPL,424964,50111010101,0000000847,28744,KG,1101"'.
INPUT THROUGH VALUE (POPERACION) NO-ECHO.
IMPORT UNFORMATTED RES.
INPUT CLOSE.


MESSAGE RES VIEW-AS ALERT-BOX.
