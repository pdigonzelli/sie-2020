
/*------------------------------------------------------------------------
    File        : verpallet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Mar 23 20:04:22 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

CURRENT-WINDOW:WIDTH = 120.

FOR EACH PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET >= 16000163   NO-LOCK.
    DISPLAY pallets.id_pallet pallets.id_pallet_sap FORMAT 'X(30)' pallets.fecha_prod month(fecha_prod).
        
END.