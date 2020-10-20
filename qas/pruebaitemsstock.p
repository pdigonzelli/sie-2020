
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

CURRENT-WINDOW:WIDTH = 200.
    FOR EACH items_stock WHERE  items_stock.nro_partida = 425043 AND
    Items_stock.id_tipo_movsto = 70  AND
    items_stock.codigo_trazabilidad = '05Q01' AND 
    items_stock.FECHA = TODAY.

    /*DOCUMENTO_SAP = DOCUMENTO_SAP +  '&2016&000300000129&Se proceso correctamente el volcado'.*/
   /* DOCUMENTO_SAP = SUBSTRING(DOCUMENTO_SAP,1,66). */
    
    DISP DOCUMENTO_SAP FORMAT 'X(140)' ORDEN_ENTREGA  WITH WIDTH 190.