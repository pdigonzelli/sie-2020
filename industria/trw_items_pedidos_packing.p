TRIGGER PROCEDURE FOR WRITE OF items_pedidos_packing.
    find pallets_testigos of items_pedidos_packing
        exclusive-lock no-error.

    if not available pallets_testigos then
        create pallets_testigos.

    assign
        pallets_testigos.id_empresa     = items_pedidos_packing.id_empresa
        pallets_testigos.id_orden       = items_pedidos_packing.id_orden
        pallets_testigos.item           = items_pedidos_packing.item
        pallets_testigos.cant_pedida    = items_pedidos_packing.cant_pallets
        pallets_testigos.imprime        = false.

    release pallets_testigos.
