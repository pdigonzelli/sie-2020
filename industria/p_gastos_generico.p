define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.
define var v_impuesto as decimal.

find orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.
CASE p_id_gasto:
    WHEN 16 then
        v_impuesto = 60.
    WHEN 26 THEN
        v_impuesto = 200.
    /*
        if p_id_gasto = 17 then
            if orden_entrega.id_tipo_contenedor = 1 then
                v_impuesto = 165.
            else 
                v_impuesto = 300.
        else
            v_impuesto = 0.
        */
END CASE.

if p_id_gasto > 0 then
    do:
        p_valor = v_impuesto * orden_entrega.contenedores.
        
        create gastos_orden_entrega.
        assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
               gastos_orden_entrega.id_gasto            = p_id_gasto
               gastos_orden_entrega.importe             = p_valor.
    end.
