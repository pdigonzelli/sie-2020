define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.

find orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.
find tipo_contenedor of orden_entrega no-lock no-error.

if available tipo_contenedor then
    do:
/*        if orden_entrega.id_articulo = 51 OR
           orden_entrega.id_articulo = 52 or 
           orden_entrega.id_articulo = 53 OR 
           orden_entrega.id_articulo = 71 then
            do: */
                p_valor = (tipo_contenedor.precio_thc * orden_entrega.contenedores).
                create gastos_orden_entrega.
                assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                       gastos_orden_entrega.id_gasto            = p_id_gasto
                       gastos_orden_entrega.importe             = p_valor.
/*            end.
         else
            do:
                create gastos_orden_entrega.
                assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                       gastos_orden_entrega.id_gasto            = p_id_gasto
                       gastos_orden_entrega.importe             = 0.
            end. */
    end.
