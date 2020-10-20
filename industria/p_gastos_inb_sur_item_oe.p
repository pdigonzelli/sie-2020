define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe LIKE items_orden_entrega.ITEM_oe.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_flete as decimal.

find items_orden_entrega where items_orden_entrega.id_orden_entrega = p_id_orden_entrega 
                           AND items_orden_entrega.ITEM_oe          = p_item_oe
                            no-lock no-error.
FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = p_id_orden_entrega
                            NO-LOCK NO-ERROR.

FIND FIRST agencias WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
IF AVAILABLE agencias THEN DO:
    FIND FIRST gastos_agencias WHERE gastos_agencias.id_agencia = agencias.id_agencia
                                 AND gastos_agencia.id_gasto = 26
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_agencia THEN DO:
        FIND LAST gastos_flete WHERE gastos_flete.id_agencia = orden_entrega.id_agencia no-lock no-error.

        IF AVAILABLE gastos_flete THEN DO:
                p_flete = gastos_flete.inbalance_surcharge * items_orden_entrega.contenedores.
                
                CREATE gastos_items_orden_entrega.
                ASSIGN gastos_items_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                       gastos_items_orden_entrega.ITEM_oe             = p_item_oe
                       gastos_items_orden_entrega.id_gasto            = p_id_gasto
                       gastos_items_orden_entrega.importe             = p_flete.

                RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                                  INPUT p_item_oe,
                                                  INPUT p_id_gasto).
            END.
    END.
END.
