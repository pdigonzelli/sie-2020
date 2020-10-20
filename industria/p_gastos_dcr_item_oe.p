define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.
define var v_impuesto as decimal.

find items_orden_entrega where items_orden_entrega.id_orden_entrega = p_id_orden_entrega 
                           AND items_orden_entrega.ITEM_oe          = p_item_oe
                            no-lock no-error.

if available items_orden_entrega then do:
    for each items_contratos of items_orden_entrega no-lock.
        for each r_gastos_items_contrato of items_contratos no-lock.
            if r_gastos_items_contrato.id_gasto = 6 /* DELIVERY */ OR 
               r_gastos_items_contrato.id_gasto = 7 /* REPROCESO */ OR 
               r_gastos_items_contrato.id_gasto = 23 /* FINANCING AND STORAGE */ OR
               r_gastos_items_contrato.id_gasto = 24 /* PALLETAZING */ OR
               r_gastos_items_contrato.id_gasto = 28 /* WAREHOUSE */ /* OR 
               r_gastos_items_contrato.id_gasto = 10 /* COMISION */ */
                then do:
            
               v_impuesto = v_impuesto + r_gastos_items_contrato.importe.
            
            end.
        end.
    end.
end.

if p_id_gasto > 0 then
    do:
        p_valor = v_impuesto * items_orden_entrega.total_galones.
        
        create gastos_items_orden_entrega.
        assign gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
               gastos_items_orden_entrega.ITEM_oe             = p_item_oe
               gastos_items_orden_entrega.id_gasto            = p_id_gasto
               gastos_items_orden_entrega.importe             = p_valor.

        RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                          INPUT p_item_oe,
                                          INPUT p_id_gasto).
    end.
