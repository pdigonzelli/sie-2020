define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_valor as decimal.

FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = p_id_orden_entrega NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = p_id_orden_entrega
                                     AND items_orden_entrega.ITEM_oe          = p_item_oe
                                    NO-LOCK NO-ERROR.
    IF AVAILABLE items_orden_entrega THEN DO:
        FIND FIRST r_agencias_contenedores WHERE r_agencias_contenedores.id_agencia =
                                                 orden_entrega.id_agencia
                                             AND r_agencias_contenedores.id_tipo_contenedor =
                                                 items_orden_entrega.id_tipo_contenedor
                                             NO-LOCK NO-ERROR.
        IF AVAILABLE r_agencias_contenedores THEN DO:
            p_valor = r_agencias_contenedores.importe * items_orden_entrega.contenedores.
            create gastos_items_orden_entrega.
            assign gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
                   gastos_items_orden_entrega.ITEM_oe             = p_item_oe
                   gastos_items_orden_entrega.id_gasto            = p_id_gasto
                   gastos_items_orden_entrega.importe             = p_valor.

            RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                              INPUT p_item_oe,
                                              INPUT p_id_gasto).
        END.
        ELSE DO:
            FIND FIRST agencias WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
            IF AVAILABLE agencias THEN DO:
                FIND FIRST gastos_agencias WHERE gastos_agencia.id_agencia = agencias.id_agencia
                                             AND gastos_agencia.id_gasto   = p_id_gasto
                                            NO-LOCK NO-ERROR.
                IF AVAILABLE gastos_agencias THEN DO:
                    p_valor = gastos_agencias.importe * items_orden_entrega.contenedores.
                    CREATE gastos_items_orden_entrega.
                    ASSIGN gastos_items_orden_entrega.id_orden_entrega    = items_orden_entrega.id_orden_entrega
                           gastos_items_orden_entrega.ITEM_oe             = p_item_oe
                           gastos_items_orden_entrega.id_gasto            = p_id_gasto
                           gastos_items_orden_entrega.importe             = p_valor.
        
                    RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                                      INPUT p_item_oe,
                                                      INPUT p_id_gasto).
                END.
            END.
        END.
    END.
END.

    
