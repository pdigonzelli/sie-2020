/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al inbalance surcharge
  Parameters:   pIdOrdenEntrega id_orden_entrega
                pItemOd         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                pIdGasto        gastos_venta.id_gasto
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pFlete AS DECIMAL NO-UNDO.

DEFINE VAR vFlete AS DECIMAL NO-UNDO.

FIND orden_entrega WHERE orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
  FIND items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega
                             AND items_orden_entrega.ITEM_oe = pItemOE 
                             NO-LOCK NO-ERROR.
  IF AVAILABLE items_orden_entrega THEN DO:
    FIND FIRST agencias WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
    FIND FIRST gastos_agencia WHERE gastos_agencia.id_agencia = agencias.id_agencia
                                AND gastos_agencia.id_gasto   = pIdGasto 
                                NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_agencia THEN DO:
      FIND LAST gastos_flete WHERE gastos_flete.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
      IF AVAILABLE gastos_flete THEN DO:
        vFlete = gastos_flete.inbalance_surcharge * items_orden_entrega.contenedores.
      END. /*available gastos_flete*/
    END. /*available gastos_agencia*/
  END. /*available items_orden_entrega*/
END. /*available orden_entrega*/


pFlete = vFlete.
