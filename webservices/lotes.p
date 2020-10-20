
/*------------------------------------------------------------------------
    File        : lotes.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Apr 13 08:05:17 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
OUTPUT TO D:\TEMP\LOTES.TXT.

FOR EACH PALLETS NO-LOCK WHERE ID_PALLET >= 16000000 AND ESTADO AND ID_PALLET_SAP <> '' , EACH items_pallets OF PALLETS NO-LOCK BY PALLETS.ID_PALLET BY items_pallets.ITEM_PALLET.
    EXPORT DELIMITER ';'    PALLETS.ID_PALLET 
                            pallets.id_pallet_sap 
                            items_pallets.item_pallet 
                            items_pallets.calibre 
                            items_pallets.codigo_trazabilidad 
                            items_pallets.ubicacion.
END. 

OUTPUT CLOSE.