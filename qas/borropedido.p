
/*------------------------------------------------------------------------
    File        : veopedido.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Apr 12 11:00:24 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FOR LAST pedidos_packing  BY ID_ORDEN DESC.
    for each items_pedidos_packing of pedidos_packing.
        DELETE ITEMS_PEDIDOS_PACKING.
    end.
    DELETE pedidos_packing.
END.