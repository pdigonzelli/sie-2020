define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.

find items_orden_entrega where items_orden_entrega.id_orden_entrega = p_id_orden_entrega 
                           AND items_orden_entrega.ITEM_oe          = p_item_oe
                            no-lock no-error.
IF AVAILABLE items_orden_entrega THEN DO:
    
    p_valor = items_orden_entrega.total_factura * 0.0175.
    create gastos_items_orden_entrega.
    assign gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
           gastos_items_orden_entrega.ITEM_oe             = p_item_oe
           gastos_items_orden_entrega.id_gasto            = p_id_gasto
           gastos_items_orden_entrega.importe             = p_valor.
END.


    
