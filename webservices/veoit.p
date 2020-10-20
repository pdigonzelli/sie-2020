
/*------------------------------------------------------------------------
    File        : veoit.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Mar 30 17:36:13 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR EACH items_stock WHERE  items_stock.nro_partida = 425034 AND
                        items_stock.id_tipo_movsto = 70 AND
                        items_stock.nro_partida_serial = 3 NO-LOCK.
  MESSATE NRO.