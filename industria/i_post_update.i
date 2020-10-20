define var v_cond_venta as integer.
define var v_orden_entrega as integer.
define var r as rowid.
define var hcon as handle.
define buffer b_oe for orden_entrega.
define var v_total_tam_pedidos as integer initial 0.
define var v_total_tam_asignados as integer initial 0.
define var v_fob_ton as decimal.

assign
general.orden_entrega.anio             = integer(orden_entrega.anio:screen-value in frame F-Main)
general.orden_entrega.item             = integer(orden_entrega.item:screen-value in frame F-Main)
general.orden_entrega.id_tipo_contrato = integer(orden_entrega.id_tipo_contrato:screen-value in frame F-Main)
general.orden_entrega.semana_entrega   = integer(orden_entrega.semana_entrega:screen-value in frame F-Main).

 
run get-container (output hcon).
run dame_datos_orden in hcon (output r).

v_cond_venta    = integer(id_condicion_venta:screen-value in frame F-Main).
v_orden_entrega = integer(id_orden_entrega:screen-value in frame F-Main).

run calculo-gastos (input v_orden_entrega, input v_cond_venta).
run carga-fob (input v_orden_entrega).

find orden_entrega where rowid(orden_entrega) = r no-lock no-error.
find items_contratos of orden_entrega no-error.
if available items_contratos then
    do:
        v_total_tam_pedidos = items_contratos.cantidad.
        for each b_oe of items_contratos.
            if b_oe.id_orden_entrega <> orden_entrega.id_orden_entrega then
                    v_total_tam_asignados = v_total_tam_asignados + b_oe.cantidad_tambores.
            else
                    v_total_tam_asignados = v_total_tam_asignados + 
                                            integer(orden_entrega.cantidad_tambores:screen-value in frame F-Main).
        end.
        if v_total_tam_asignados = v_total_tam_pedidos then 
                items_contratos.pendiente = false.
        else 
                items_contratos.pendiente = true.
    end.

assign v_anio = 0
       v_item = 0
       v_id_tipo_contrato = 0
       v_semana_entrega = 0.
v_alta = 0.