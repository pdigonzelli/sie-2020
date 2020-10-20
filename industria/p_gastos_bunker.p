define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.

find orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.
FIND FIRST r_agencias_contenedores OF orden_entrega NO-LOCK NO-ERROR.
IF AVAILABLE r_agencias_contenedores THEN DO:
    p_valor = r_agencias_contenedores.importe * orden_entrega.contenedores.
    create gastos_orden_entrega.
    assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
           gastos_orden_entrega.id_gasto            = p_id_gasto
           gastos_orden_entrega.importe             = p_valor.
END.
ELSE DO:
    FIND agencias WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
    IF AVAILABLE agencias THEN DO:
        MESSAGE "Falta cargar la tarifa del Bunker de los contenedores para la Agencia " 
                agencias.descripcion VIEW-AS ALERT-BOX.
    END.
    
END.

    
