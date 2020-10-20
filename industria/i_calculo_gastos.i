define input parameter v_orden_entrega as integer.
define input parameter v_cond_venta as integer.
define var hcon as handle.

run p_calculo-gastos.p (input v_orden_entrega,
                        input v_cond_venta,
                        input decimal(orden_entrega.contenedores:screen-value in frame F-Main)).

run get-container (output hcon).
run refresca-browser-gastos in hcon.
run carga-fob (input v_orden_entrega).
