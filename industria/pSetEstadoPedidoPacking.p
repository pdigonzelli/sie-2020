DEFINE INPUT PARAMETER rPP AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER pColor AS INTEGER NO-UNDO.

DEFINE VAR vPalletsIPP AS INTEGER NO-UNDO.
DEFINE VAR vPalletsOE AS INTEGER NO-UNDO.
DEFINE VAR vColor AS CHAR NO-UNDO.
DEFINE BUFFER bbIPP FOR items_pedidos_packing.
DEFINE BUFFER bbItemOE FOR items_orden_entrega.

FIND FIRST pedidos_packing WHERE ROWID(pedidos_packing) = rPP NO-LOCK NO-ERROR.
IF AVAILABLE pedidos_packing THEN DO:
    /*  FOR EACH rowObject. */
    FOR EACH bbIPP WHERE bbIPP.id_empresa       = pedidos_packing.id_empresa
                     AND bbIPP.id_orden         = pedidos_packing.id_orden
                     AND bbIPP.id_punto_emisor  = pedidos_packing.id_punto_emisor
                    NO-LOCK
                    BY ITEM.
        vPalletsIPP = vPalletsIPP + bbIPP.cant_pallets.
        FOR EACH bbItemOE WHERE bbItemOE.id_empresa_ipp       = bbIPP.id_empresa
                            AND bbItemOE.id_orden_ipp         = bbIPP.id_orden
                            AND bbItemOE.id_punto_emisor_ipp  = bbIPP.id_punto_emisor
                            AND bbItemOE.item_ipp             = bbIPP.ITEM
                            NO-LOCK.
            vPalletsOE = vPalletsOE + bbItemOE.cantidad_pallets.
        END.

    END.

    IF vPalletsOE = 0 THEN DO: /* EL PEDIDO NO TIENE NINGUN "DESPACHO" */
        pColor = 15. /* BLANCO */
    END.
    ELSE DO:
        IF vPalletsIPP = vPalletsOE THEN DO: /* EL PEDIDO ESTA COMPLETAMENTE "DESPACHADO" */
            pColor = 10. /* VERDE */
        END.
        ELSE DO:
            IF vPalletsIPP > vPalletsOE THEN DO: /* EL PEDIDO ESTA PARCIALMENTE "DESPACHADO" */
                pColor = 14. /* AMARILLO */
            END.
            ELSE DO: /* EL PEDIDO ESTA OVER "DESPACHADO". SE DESPACHO MAS DE LO QUE EL PEDIDO DECIA */
                pColor = 12. /* ROJO */
            END.
        END.
    END. 
END.
 
