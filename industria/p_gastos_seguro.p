define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_seguro as decimal.

find orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.
find last gastos_seguro no-lock no-error.

if available gastos_seguro then
    do:
        p_seguro = (gastos_seguro.importe * orden_entrega.total_factura) / 100000.
        create gastos_orden_entrega.
        assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
               gastos_orden_entrega.id_gasto            = p_id_gasto
               gastos_orden_entrega.importe             = p_seguro.
    end.
