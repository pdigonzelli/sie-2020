/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al bunker
  Parameters:   INPUT  pIdOrdenEntrega id_orden_entrega
                INPUT  pItemOd         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                INPUT  pIdGasto        gastos_venta.id_gasto
                OUTPUT pBunker         resultado de la estimacion para bunker
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pBunker AS DECIMAL NO-UNDO.

DEFINE VAR vBunker   AS DECIMAL NO-UNDO.
DEFINE VAR vImpuesto AS DECIMAL NO-UNDO.

FIND orden_entrega WHERE orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
  FIND items_orden_entrega  WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega
                              AND items_orden_entrega.ITEM_oe = pItemOE 
                              NO-LOCK NO-ERROR.
  IF AVAILABLE items_orden_entrega THEN DO:
    FIND FIRST r_agencias_contenedores WHERE r_agencias_contenedores.id_agencia = orden_entrega.id_agencia
                                         AND r_agencias_contenedores.id_tipo_contenedor = items_orden_entrega.id_tipo_contenedor 
                                         NO-LOCK NO-ERROR.
    IF AVAILABLE r_agencias_contenedores THEN DO:
      vBunker = r_agencias_contenedores.importe * items_orden_entrega.contenedores.
    END.
  END.
END. /*available orden_entrega*/


pBunker = vBunker.
