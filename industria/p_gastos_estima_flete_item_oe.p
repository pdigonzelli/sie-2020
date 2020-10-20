/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al flete
  Parameters:   pIdOrdenEntrega id_orden_entrega
                pItemOd         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                pIdGasto        gastos_venta.id_gasto
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pFlete AS DECIMAL NO-UNDO.

DEFINE VAR vFlete AS DECIMAL NO-UNDO.


FIND FIRST orden_entrega 
  WHERE orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK NO-ERROR.
FIND FIRST items_orden_entrega 
  WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega 
    AND items_orden_entrega.ITEM_oe = pItemOE NO-LOCK NO-ERROR.
IF AVAILABLE items_orden_entrega THEN DO:
  FIND LAST gastos_flete 
    WHERE gastos_flete.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
  IF AVAILABLE gastos_flete THEN DO:
    vFlete = gastos_flete.importe * items_orden_entrega.contenedores.
  END. /*available gastos_flete*/
END. /*available items_orden_entrega*/

pFlete = vFlete.
