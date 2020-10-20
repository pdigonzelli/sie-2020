define input parameter p_orden_entrega as integer.

find orden_entrega where orden_entrega.id_orden_entrega = p_orden_entrega no-lock no-error.
if available orden_entrega then
    orden_entrega.id_estado = 4.
else
    message "No se encontro la Orden de Entrega " p_orden_entrega view-as alert-box.    
