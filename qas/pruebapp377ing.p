
/*------------------------------------------------------------------------
    File        : pruebapp377ing.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Fri May 19 19:58:11 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR COUTPUT AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND PALLETS WHERE PALLETS.ID_PALLET = 17000276.

debugger:initiate().
debugger:set-break().

RUN PP377ING-1.P(pallets.id_pallet_sap, OUTPUT COUTPUT).