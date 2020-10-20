define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.

FIND orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.
IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST gastos_agencias where gastos_agencia.id_agencia = orden_entrega.id_agencia
                                 AND gastos_agencia.id_gasto   = 31 NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_agencias THEN DO:
            FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = p_id_orden_entrega
                                             AND items_orden_entrega.ITEM_oe          = p_item_oe
                                            NO-LOCK NO-ERROR.
            IF AVAILABLE items_orden_entrega THEN DO:
                p_valor = gastos_agencia.importe * items_orden_entrega.contenedores.
                
                create gastos_items_orden_entrega.
                assign gastos_items_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                       gastos_items_orden_entrega.id_gasto            = p_id_gasto
                       gastos_items_orden_entrega.importe             = p_valor
                       gastos_items_orden_entrega.ITEM_oe             = p_item_oe.
            END.
    END.
END.

