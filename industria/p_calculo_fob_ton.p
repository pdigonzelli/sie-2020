define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define output parameter p_fob_ton as decimal.
define output parameter p_fob_unitario as decimal.

define var p_gastos as decimal initial 0.
define var total_kilos as integer.
define var v_fob_unitario as decimal.

find orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-error.

for each gastos_orden_entrega of orden_entrega no-lock.
    find r_clausulas_gastos where r_clausulas_gastos.id_gasto = gastos_orden_entrega.id_gasto
                              and r_clausulas_gastos.id_clausula = orden_entrega.id_condicion_venta no-lock no-error.
    if available r_clausulas_gastos then
        do: 
            p_gastos = p_gastos + gastos_orden_entrega.importe.
        end. 
end.



assign orden_entrega.fob_ton = orden_entrega.total_factura - p_gastos.
p_fob_ton = orden_entrega.total_factura - p_gastos.

if orden_entrega.id_tipo_contrato = 1 then
    do:
        v_fob_unitario = (1000 * p_fob_ton) / orden_entrega.kgs_netos_tambores.
    end.
else
    do:
        v_fob_unitario = p_fob_ton / orden_entrega.kgs_netos_tambores.
    end.
    
assign orden_entrega.fob_unitario = v_fob_unitario.
p_fob_unitario = v_fob_unitario.
