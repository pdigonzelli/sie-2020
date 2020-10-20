
/*------------------------------------------------------------------------
    File        : PRUEBARES.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Apr 08 10:47:36 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR CSTATUS AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND pallets WHERE pallets.id_suc_trabajo = 98 AND
                     pallets.id_pallet = 17000220 NO-ERROR.
                     

RUN PRUEBAPP160INGP-1.P (PALLETS.ID_SUC_TRABAJO, PALLETS.ID_PALLET, OUTPUT CSTATUS).