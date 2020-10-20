define var r as rowid.
define var hcon as handle.

run get-container (output hcon).
run dame_datos_orden in hcon (output r).

find orden_entrega where rowid(orden_entrega) = r no-lock no-error.
if available orden_entrega then
    v_id_orden_entrega = orden_entrega.id_orden_entrega.
