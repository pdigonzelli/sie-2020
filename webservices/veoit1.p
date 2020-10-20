
/*------------------------------------------------------------------------
    File        : veoit1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Fri Apr 01 10:05:25 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FOR EACH items_stock WHERE NRO_PARTIDA = 425124 AND ID_TIPO_MOVSTO = 70 AND NRO_PARTIDA_SERIAL = 3.
    DISP items_stock.id_tipo_movsto items_stock.NRO items_stock.FECHA_OPERATIVA.