define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_flete as decimal.

find orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.
FIND FIRST agencias WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
IF AVAILABLE agencias THEN DO:
    FIND FIRST gastos_agencias WHERE gastos_agencias.id_agencia = agencias.id_agencia
                                 AND gastos_agencia.id_gasto = 26
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_agencia THEN DO:
        find last gastos_flete where gastos_flete.id_agencia = orden_entrega.id_agencia no-lock no-error.

        if available gastos_flete then
            do:
                p_flete = gastos_flete.importe * orden_entrega.contenedores.
                
                create gastos_orden_entrega.
                assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                       gastos_orden_entrega.id_gasto            = p_id_gasto
                       gastos_orden_entrega.importe             = p_flete.
            end.
    END.
END.
