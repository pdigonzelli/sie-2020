
/*------------------------------------------------------------------------
    File        : pruebaitemsstock.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Mar 19 12:45:26 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FOR LAST volcado_packing WHERE VOLCADO.NRO_PARTIDA = 425042 NO-LOCK.
    FIND items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
    Items_stock.id_tipo_movsto = 70 AND
    items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK.

    DISP DOCUMENTO_SAP.