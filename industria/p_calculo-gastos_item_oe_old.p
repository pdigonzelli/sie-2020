define input parameter v_orden_entrega as integer.
DEFINE INPUT PARAMETER v_item_oe AS INTEGER.
define input parameter v_cond_venta as integer.
define input parameter v_contenedores as decimal.


        for each gastos_items_orden_entrega where gastos_items_orden_entrega.id_orden_entrega = v_orden_entrega
                                              AND gastos_items_orden_entrega.ITEM_oe          = v_item_oe.
            delete gastos_items_orden_entrega.
        end.
        for each gastos_orden_entrega where gastos_orden_entrega.id_orden_entrega = v_orden_entrega.
            delete gastos_orden_entrega.
        end.

/* ACA SE CALCULAR LOS GASTOS DEFINIDOS EN LOS PROGRAMAS PARTICULARES SEGUN EL GASTO */

        for each r_clausulas_gastos_item_oe where r_clausulas_gastos_item_oe.id_clausula = 
                                                  v_cond_venta 
                                            NO-LOCK.
            
            if r_clausulas_gastos_item_oe.programa <> "" then
                do:
                    
                    run value(r_clausulas_gastos_item_oe.programa) (INPUT v_orden_entrega, 
                                                                    INPUT v_item_oe, 
                                                                    INPUT r_clausulas_gastos_item_oe.id_gasto).
                end.        
            else
                do:
                   CREATE gastos_items_orden_entrega.
                   ASSIGN gastos_items_orden_entrega.id_orden_entrega    = v_orden_entrega
                          gastos_items_orden_entrega.item_oe             = v_item_oe
                          gastos_items_orden_entrega.id_gasto            = r_clausulas_gastos_item_oe.id_gasto
                          gastos_items_orden_entrega.importe             = 0.
                    
                end.        
        end.

        /* ESTE CODIGO SERIA EL FOB ENTRE COMILLAS */
        define buffer b_gastos for gastos_orden_entrega.
/* ACA SE CARGAN LOS DEMAS GASTOS DEFINIDOS SEGUN LOS GASTOS CARGADOS POR CADA AGENCIA MARITIMA */

        find orden_entrega where orden_entrega.id_orden_entrega = v_orden_entrega no-lock no-error.
        if available orden_entrega then
            do:
                for each gastos_agencias where gastos_agencias.id_agencia = orden_entrega.id_agencia.
                    /* RECORRO TODOS LOS GASTOS DE LAS AGENCIAS */

                    find first b_gastos where b_gastos.id_gasto         = gastos_agencias.id_gasto
                                          and b_gastos.id_orden_entrega = orden_entrega.id_orden_entrega no-lock no-error.
                                          
                    if not available b_gastos then do:
                        /* SI NO LO ENCUENTRO LO BUSCO EN LA TABLA DE EXCEPCIONES r_gastos_agencias_clausulas*/
                        /* ESTO ES PARA PODER ELEJIR SI NO QUIERO QUE SE CARGUE ALGUN GASTO
                           SEGUN LA CLAUSULA */
                        FIND FIRST r_gastos_agencias_clausulas WHERE r_gastos_agencias_clausulas.id_agencia =
                                                                     orden_entrega.id_agencia
                                                                 AND r_gastos_agencias_clausulas.id_gasto =
                                                                     gastos_agencias.id_gasto
                                                                 AND r_gastos_agencias_clausulas.id_clausula =
                                                                     v_cond_venta
                                                                 NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE r_gastos_agencias_clausulas THEN DO:
                            if gastos_agencias.id_gasto = 19 then
                                do: /* ESTE ES EL GASTOS "BL" QUE ES EL UNICO QUE NO SE CALCULAR POR CONTENEDOR*/
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
                        END.
                    end.

                end.
            end.
        /******************************************/
