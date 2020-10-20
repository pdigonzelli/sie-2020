
/*------------------------------------------------------------------------
    File        : pruebaanulacion.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Apr 11 13:19:50 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR CSTATUS AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND PALLETS WHERE pallets.id_pallet = 17000262 AND ID_SUC_TRABAJO = 98.   

RUN PP377ING-1.P (PALLETS.ID_PALLET_SAP,OUTPUT CSTATUS).
/*
RUN ANULAPALLET.P (pallets.id_pallet_sap , 'QAS', OUTPUT CSTATUS) NO-ERROR.
*/  
IF (ERROR-STATUS:ERROR OR CSTATUS <> 'OK')THEN DO:
    MESSAGE ('Error de Anulacion Pallet SAP: ' + RETURN-VALUE + " " + cstatus) VIEW-AS ALERT-BOX ERROR TITLE 'ERROR DE ANULACION'.                 
END.
ELSE
    MESSAGE CSTATUS VIEW-AS ALERT-BOX INFORMATION TITLE "STATUS".
CATCH e AS Progress.Lang.Error :
    MESSAGE E:GETMESSAGE(1) VIEW-AS ALERT-BOX.
END CATCH.
