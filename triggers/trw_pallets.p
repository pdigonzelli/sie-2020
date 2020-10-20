TRIGGER PROCEDURE FOR WRITE OF pallets.
/*
    find pallets_testigos of pallets
        exclusive-lock no-error.

    if available pallets_testigos then do:
        assign pallets_testigos.cantidad = pallets_testigos.cantidad + 1.

        if pallets_testigos.imprime = false then do:
            if pallets_testigos.cant_pedida < 21 then do:
                if pallets_testigos.cantidad = 1 or
                    pallets_testigos.cantidad = 9 then
                    assign
                        pallets_testigos.imprime    = true
                        pallets_testigos.testigos   = pallets_testigos.testigos + 1.
            end.
    
            if pallets_testigos.cant_pedida > 20 and
                pallets_testigos.cant_pedida < 61 then do:
                if pallets_testigos.cantidad = 1 or
                    pallets_testigos.cantidad = 9 or
                    pallets_testigos.cantidad = 24 or
                    pallets_testigos.cantidad = 39 or
                    pallets_testigos.cantidad = 54 then
                    assign
                        pallets_testigos.imprime    = true
                        pallets_testigos.testigos   = pallets_testigos.testigos + 1.
            end.

            if pallets_testigos.cant_pedida > 60 then do:
                if pallets_testigos.cantidad = 1 or
                    pallets_testigos.cantidad modulo 20 = 0 then
                    assign
                        pallets_testigos.imprime    = true
                        pallets_testigos.testigos   = pallets_testigos.testigos + 1.
            end.
        end.
    end.

    release pallets_testigos.
*/
