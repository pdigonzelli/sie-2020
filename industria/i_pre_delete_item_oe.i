define var r as rowid.
define var hcon as handle.

run get-container (output hcon).
run GET-rowid-item-oe in hcon (output r).

find items_orden_entrega where rowid(items_orden_entrega) = r no-lock no-error.
if available items_orden_entrega THEN DO:
    v_id_orden_entrega = items_orden_entrega.id_orden_entrega.
    v_item_oe          = items_orden_entrega.ITEM_oe.
END.
    
