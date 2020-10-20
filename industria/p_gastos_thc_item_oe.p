define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.

find items_orden_entrega where items_orden_entrega.id_orden_entrega = p_id_orden_entrega 
                           AND items_orden_entrega.ITEM_oe          = p_item_oe
                         no-lock no-error.
FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = p_id_orden_entrega NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST gastos_agencia WHERE gastos_agencias.id_agencia  = orden_entrega.id_agencia
                                AND gastos_agencia.id_gasto     = p_id_gasto NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_agencia THEN DO:
        p_valor = (gastos_agencias.importe * items_orden_entrega.contenedores).
        create gastos_items_orden_entrega.
        assign gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
               gastos_items_orden_entrega.ITEM_oe             = items_orden_entrega.ITEM_oe
               gastos_items_orden_entrega.id_gasto            = p_id_gasto
               gastos_items_orden_entrega.importe             = p_valor.

        RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                          INPUT p_item_oe,
                                          INPUT p_id_gasto).
    END.
    ELSE DO:
        find tipo_contenedor of items_orden_entrega no-lock no-error.
        if available tipo_contenedor THEN DO:
        /*        if orden_entrega.id_articulo = 51 OR
                   orden_entrega.id_articulo = 52 or 
                   orden_entrega.id_articulo = 53 OR 
                   orden_entrega.id_articulo = 71 then
                    do: */
                        p_valor = (tipo_contenedor.precio_thc * items_orden_entrega.contenedores).
                        create gastos_items_orden_entrega.
                        assign gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
                               gastos_items_orden_entrega.ITEM_oe             = items_orden_entrega.ITEM_oe
                               gastos_items_orden_entrega.id_gasto            = p_id_gasto
                               gastos_items_orden_entrega.importe             = p_valor.
        
                        RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                                          INPUT p_item_oe,
                                                          INPUT p_id_gasto).
        /*            end.
                 else
                    do:
                        create gastos_orden_entrega.
                        assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                               gastos_orden_entrega.id_gasto            = p_id_gasto
                               gastos_orden_entrega.importe             = 0.
                    end. */
        END.
    END.
    
END.

