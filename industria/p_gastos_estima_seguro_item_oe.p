/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al seguro
  Parameters:   pIdOrdenEntrega id_orden_entrega
                pItemOd         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                pIdGasto        gastos_venta.id_gasto
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pSeguro AS DECIMAL NO-UNDO.

DEFINE VAR vSeguro AS DECIMAL NO-UNDO.


FIND FIRST orden_entrega 
  WHERE orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK NO-ERROR.
FIND FIRST items_orden_entrega 
  WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega 
    AND items_orden_entrega.ITEM_oe = pItemOE NO-LOCK NO-ERROR.
IF AVAILABLE items_orden_entrega THEN DO:
  FIND LAST gastos_seguro NO-LOCK NO-ERROR.
  IF AVAILABLE gastos_seguro THEN DO:
    vSeguro = ((gastos_seguro.importe * items_orden_entrega.TOTAL_factura) / 100000) * items_orden_entrega.contenedores. /*?????*/
  END. /*available gastos_seguro*/
END. /*available items_orden_entrega*/

pSeguro = vSeguro.
