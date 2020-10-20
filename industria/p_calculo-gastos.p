define input parameter v_orden_entrega as integer.
define input parameter v_cond_venta as integer.
define input parameter v_contenedores as decimal.
        
        for each gastos_orden_entrega where gastos_orden_entrega.id_orden_entrega = v_orden_entrega.
            delete gastos_orden_entrega.
        end.

        for each r_clausulas_gastos where r_clausulas_gastos.id_clausula = v_cond_venta no-lock.
            if r_clausulas_gastos.programa <> "" then
                do:
                    run value(r_clausulas_gastos.programa) (input v_orden_entrega, input r_clausulas_gastos.id_gasto).
                end.        
            else
                do:
                    create gastos_orden_entrega.
                    assign gastos_orden_entrega.id_orden_entrega    = v_orden_entrega
                           gastos_orden_entrega.id_gasto            = r_clausulas_gastos.id_gasto
                           gastos_orden_entrega.importe             = 0.                     
                end.        
        end.

define buffer b_gastos for gastos_orden_entrega.

        find orden_entrega where orden_entrega.id_orden_entrega = v_orden_entrega no-lock no-error.
        if available orden_entrega then
            do:
                for each gastos_agencias where gastos_agencias.id_agencia = orden_entrega.id_agencia.
                
                    find first b_gastos where b_gastos.id_gasto         = gastos_agencias.id_gasto
                                          and b_gastos.id_orden_entrega = orden_entrega.id_orden_entrega no-lock no-error.
                                          
                    if not available b_gastos then do:
                        if gastos_agencias.id_gasto = 19 then
                            do:
                                create gastos_orden_entrega.
                                assign gastos_orden_entrega.id_orden_entrega    = v_orden_entrega
                                       gastos_orden_entrega.id_gasto            = gastos_agencias.id_gasto
                                       gastos_orden_entrega.importe             = gastos_agencias.importe.
                            end.
                        else
                            do:
                                create gastos_orden_entrega.
                                assign gastos_orden_entrega.id_orden_entrega    = v_orden_entrega
                                       gastos_orden_entrega.id_gasto            = gastos_agencias.id_gasto
                                       gastos_orden_entrega.importe             = gastos_agencias.importe * v_contenedores.
                            end.
                    end.

                end.
            end.
