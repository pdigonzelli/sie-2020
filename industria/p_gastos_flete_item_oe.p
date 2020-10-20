DEFINE input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
DEFINE input parameter p_id_gasto like gastos_venta.id_gasto.
DEFINE var p_flete as decimal.
DEFINE VAR v_importes_flete AS DECIMAL INITIAL 0.

FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.
IF AVAILABLE orden_entrega THEN DO:

    FIND LAST gastos_flete WHERE gastos_flete.id_agencia = orden_entrega.id_agencia no-lock no-error.
    IF orden_entrega.id_tipo_orden = 1 /* INDUSTRIA */ THEN DO:
        FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = p_id_orden_entrega
                                         AND items_orden_entrega.ITEM_oe = p_item_oe NO-LOCK NO-ERROR.
        IF AVAILABLE items_orden_entrega AND 
                     items_orden_entrega.id_condicion_venta <> 15 /* CONSIGNACION */ THEN DO:
            if available gastos_flete THEN do:
                        p_flete = gastos_flete.importe * items_orden_entrega.contenedores.
                        
                        create gastos_items_orden_entrega.
                        assign gastos_items_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                               gastos_items_orden_entrega.id_gasto            = p_id_gasto
                               gastos_items_orden_entrega.importe             = p_flete
                               gastos_items_orden_entrega.ITEM_oe             = p_item_oe.
        
                        RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                                          INPUT p_item_oe,
                                                          INPUT p_id_gasto).
                    end.
            END.
    END.
    ELSE DO: /* FRUTA FRESCA */
        FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = p_id_orden_entrega
                                         AND items_orden_entrega.ITEM_oe = p_item_oe NO-LOCK NO-ERROR.
        IF AVAILABLE items_orden_entrega THEN DO:
            IF AVAILABLE gastos_flete THEN DO:
                FIND FIRST r_fletes_clientes_cons WHERE r_fletes_clientes_cons.id_cliente = items_orden_entrega.id_cliente
                                                    NO-LOCK NO-ERROR.

                IF items_orden_entrega.id_condicion_venta <> 15 OR AVAILABLE r_fletes_clientes_cons THEN DO:
                        
                    IF orden_entrega.forma_transporte THEN DO:
                        p_flete = gastos_flete.importe * items_orden_entrega.contenedores.
                    END.
                    ELSE DO:
                        p_flete = gastos_flete.precio_bodega * items_orden_entrega.cantidad_pallets.
                    END.
                        
                    create gastos_items_orden_entrega.
                    assign gastos_items_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                           gastos_items_orden_entrega.id_gasto            = p_id_gasto
                           gastos_items_orden_entrega.importe             = p_flete
                           gastos_items_orden_entrega.ITEM_oe             = p_item_oe.
    
                    RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                                      INPUT p_item_oe,
                                                      INPUT p_id_gasto).
            
                END.
            end.
        END.
    END.
    
END.
