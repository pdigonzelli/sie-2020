
/*------------------------------------------------------------------------
    File        : pruebacambio.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Apr 12 11:53:05 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR VORDEN AS INTEGER NO-UNDO.

 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET = 16000146  NO-LOCK.

VORDEN = 300000209.
RUN pp378ing.P (PALLETS.ID_SUC_TRABAJO, PALLETS.ID_PALLET, VORDEN , 'QAS').

CATCH E AS Progress.Lang.Error :
    MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.		
END CATCH.