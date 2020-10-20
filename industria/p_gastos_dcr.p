define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.
define var v_impuesto as decimal.

find orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.
if available orden_entrega then do:
    for each items_contratos of orden_entrega no-lock.
        for each r_gastos_items_contrato of items_contratos no-lock.
            if r_gastos_items_contrato.id_gasto = 6 or
               r_gastos_items_contrato.id_gasto = 7 or
               r_gastos_items_contrato.id_gasto = 10 then do:
            
               v_impuesto = v_impuesto + r_gastos_items_contrato.importe.
            
            end.
        end.
    end.
end.

if p_id_gasto > 0 then
    do:
        p_valor = v_impuesto * orden_entrega.total_galones.
        
        create gastos_orden_entrega.
        assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
               gastos_orden_entrega.id_gasto            = p_id_gasto
               gastos_orden_entrega.importe             = p_valor.
    end.
