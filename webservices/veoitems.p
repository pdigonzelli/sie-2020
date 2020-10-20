
/*------------------------------------------------------------------------
    File        : veoitems.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Fri Mar 25 01:01:28 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE BUFFER BITEMS FOR ITEMS_STOCK.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST BITEMS WHERE  BITEMS.NRO_PARTIDA = 425020 AND 
                                 BITEMS.nro_partida_serial = 2 AND
                                     (BITEMS.ID_TIPO_MOVSTO =  5  OR  BITEMS.ID_TIPO_MOVSTO =  71 OR BITEMS.ID_TIPO_MOVSTO =  72 OR BITEMS.ID_TIPO_MOVSTO =  77)
                                 NO-LOCK NO-ERROR.
    DISP  BITEMS.orden_entrega_sap.                                 