TRIGGER PROCEDURE FOR DELETE OF pallets.
/*
    find pallets_testigos where 
        pallets_testigos.id_empresa = pallets.id_empresa and
        pallets_testigos.id_orden   = pallets.id_orden and 
        pallets_testigos.item       = pallets.item
        exclusive-lock no-error.

    if available pallets_testigos then
        assign pallets_testigos.cantidad   = pallets_testigos.cantidad - 1.

    release pallets_testigos.
*/
