
/*------------------------------------------------------------------------
    File        : pruebapallet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Mar 15 08:58:30 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VAR cStatus AS CHARACTER NO-UNDO.
DEFINE VAR DOC1 AS CHARACTER NO-UNDO.
DEFINE VAR CPALLET AS CHARACTER NO-UNDO.
DEFINE VAR OPALLET AS CHARACTER NO-UNDO.
/* ***************************  Main Block  *************************** */

FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET = 17000375 NO-LOCK.


RUN PP159INGP1.P (PALLETS.ID_SUC_TRABAJO , PALLETS.ID_PALLET, OUTPUT CSTATUS, OUTPUT OPALLET) .

FIND CURRENT PALLETS NO-LOCK.
MESSAGE pallets.id_pallet_sap VIEW-AS ALERT-BOX INFORMATION TITLE 'PALLET SAP'.
CATCH EX  AS Progress.Lang.Error :
    MESSAGE EX:GETMESSAGE(1) VIEW-AS ALERT-BOX.		
END CATCH.

{declaraPalletn.i}