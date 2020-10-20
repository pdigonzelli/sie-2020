TRIGGER PROCEDURE FOR DELETE OF items_pedidos_packing.
    find pallets_testigos of items_pedidos_packing
        exclusive-lock no-error.

    if available pallets_testigos then
        delete pallets_testigos.

    release pallets_testigos.
