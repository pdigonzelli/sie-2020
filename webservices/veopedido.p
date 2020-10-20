
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


FOR EACH pedidos_packing  NO-LOCK  BY ID_ORDEN DESC.
    disp pedidos_packing.id_orden format '99999999999999999' pedidos_packing.id_pedido_sap pedidos_packing.posicion_pedido_sap format '9999999999' with 2 columns.
    for each items_pedidos_packing of pedidos_packing.
        disp items_pedidos_packing with 2 columns.
        update items_pedidos_packing.calibre.
    end.
END.