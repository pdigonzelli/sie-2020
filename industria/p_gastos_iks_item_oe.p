define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.

FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = p_id_orden_entrega 
                                 AND items_orden_entrega.ITEM_oe          = p_item_oe
                                NO-LOCK NO-ERROR.
IF AVAILABLE items_orden_entrega THEN DO:
    
    p_valor = items_orden_entrega.total_factura * 0.0175.

    CREATE gastos_items_orden_entrega.
    ASSIGN gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
           gastos_items_orden_entrega.ITEM_oe             = p_item_oe
           gastos_items_orden_entrega.id_gasto            = p_id_gasto
           gastos_items_orden_entrega.importe             = p_valor.

    RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                      INPUT p_item_oe,
                                      INPUT p_id_gasto).
END.


    
