/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al inbalance surcharge
  Parameters:   pIdOrdenEntrega id_orden_entrega
                pItemOE         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                pIdGasto        gastos_venta.id_gasto
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pEbaf AS DECIMAL NO-UNDO.

DEFINE VAR vEbaf AS DECIMAL NO-UNDO.



FIND orden_entrega WHERE orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK NO-ERROR.
FIND items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = PIdOrdenEntrega
                           AND items_orden_entrega.ITEM_oe = pItemOE 
                           NO-LOCK NO-ERROR.
IF AVAILABLE items_orden_entrega THEN DO:
  FIND FIRST gastos_agencias WHERE gastos_agencia.id_agencia = orden_entrega.id_agencia
                               AND gastos_agencia.id_gasto = 31 
                               NO-LOCK NO-ERROR.
  IF AVAILABLE gastos_agencias THEN DO:
    vEbaf = gastos_agencia.importe * items_orden_entrega.contenedores.
  END. /*available gastos_agencias*/
END. /*available items_orden_entrega*/

pEbaf = vEbaf.

