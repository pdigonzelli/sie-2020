
DEFINE INPUT PARAMETER p_id_orden_entrega LIKE orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
DEFINE INPUT PARAMETER p_id_gasto LIKE gastos_venta.id_gasto.

DEFINE VAR v_entry_total AS DECIMAL.
DEFINE VAR v_fob_factura AS DECIMAL.
DEFINE VAR v_total_cfr AS DECIMAL.
DEFINE VAR v_flete AS DECIMAL.

/*
/*interrupcion para debugger*/

DEFINE VAR dbg AS LOGICAL.
dbg = DEBUGGER:INITIATE().
dbg = DEBUGGER:SET-BREAK().
*/

FIND FIRST orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega NO-LOCK NO-ERROR.
FIND LAST gastos_flete WHERE gastos_flete.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.

FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega   = p_id_orden_entrega
                                 AND items_orden_entrega.ITEM_oe            = p_item_oe
                                NO-LOCK NO-ERROR.

IF AVAILABLE items_orden_entrega THEN DO:
    IF AVAILABLE gastos_flete THEN DO: /* CALCULO EL GASTO DE FLETE */
        v_flete = gastos_flete.importe * items_orden_entrega.contenedores.

        v_fob_factura = items_orden_entrega.fob_ton.
        v_total_cfr = v_fob_factura + v_flete. /* PRECIO CFR (FOB + FLETE) */
        MESSAGE "Fob " v_fob_factura " Flete " v_flete " Total CFR " v_total_cfr VIEW-AS ALERT-BOX.
        v_entry_total = v_total_cfr * 0.109.  /* 10.9% sobre el precio CFR (fob + flete) */
        MESSAGE "Duty " v_entry_total VIEW-AS ALERT-BOX.
        
        CREATE gastos_items_orden_entrega.
        ASSIGN gastos_items_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
               gastos_items_orden_entrega.ITEM_oe             = p_item_oe
               gastos_items_orden_entrega.id_gasto            = p_id_gasto
               gastos_items_orden_entrega.importe             = v_entry_total.
                                                
        v_entry_total = 0.

        /* EN LA RUTINA p_gastos_oe_desde_items_oe.p ACUMULO LOS GASTOS
           DE TODAS LAS PARTES DE OE Y GRABO ESE TOTAL COMO GASTO DE OE */
        RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                          INPUT p_item_oe,
                                          INPUT p_id_gasto).
    END.

    
END. /* DEL AVAILABLE ORDEN_ENTREGA */
