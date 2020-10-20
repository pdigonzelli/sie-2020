
/*------------------------------------------------------------------------
    File        : lotespallets.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Apr 06 10:59:43 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

OUTPUT TO VALUE('D:\TEMP\LOTES2.TXT').
   
FOR EACH PALLETS WHERE PALLETS.ID_PALLET > 16000000 AND ESTADO NO-LOCK, EACH items_pallets OF PALLETS BY PALLETS.ID_PALLET BY items_pallets.ITEM_PALLET.
    EXPORT DELIMITER ';' pallets.id_pallet_sap pALLETS.ID_PALLET  items_pallets.item_pallet items_pallets.codigo_trazabilidad items_pallets.bultos .
end.

OUTPUT CLOSE.

/*    items_pallets.ubicacion = ''. 
    i = i + 1.
    IF items_pallets.ubicacion = '' THEN
        J = J + 1.
    pause 0.

*/


