
/*------------------------------------------------------------------------
    File        : veopartida.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Thu Mar 24 16:39:27 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 200.
FOR EACH items_stock WHERE C_FECHA >= DATE('01/01/2016')  
    AND (items_stock.id_tipo_movsto = 5 OR
       items_stock.id_tipo_movsto = 71 OR
       items_stock.id_tipo_movsto = 72 OR
       items_stock.id_tipo_movsto = 77) NO-LOCK BY NRO_PARTIDA by FECHA.

    DISP items_stock.id_tipo_movsto 
        items_stock.nro_partida
        items_stock.nro_partida_serial
        items_stock.FECHA 
        items_stock.C_FECHA
        items_stock.c_hora
        items_stock.documento_sap
        items_stock.orden_entrega_sap
        items_stock.renspa WITH WIDTH 180 .