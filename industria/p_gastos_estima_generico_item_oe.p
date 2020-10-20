/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al generico
  Parameters:   pIdOrdenEntrega id_orden_entrega
                pItemOd         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                pIdGasto        gastos_venta.id_gasto
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pGenerico AS DECIMAL NO-UNDO.

DEFINE VAR vGenerico AS DECIMAL NO-UNDO.
DEFINE VAR vImpuesto AS DECIMAL NO-UNDO.


FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega 
                                 AND items_orden_entrega.ITEM_oe = pItemOE 
                                 NO-LOCK NO-ERROR.
IF AVAILABLE items_orden_entrega THEN DO:
  IF pIdGasto = 16 THEN vImpuesto = 60. /**in land ????*/
  IF pIdGasto > 0 THEN DO:
    vGenerico = vImpuesto * items_orden_entrega.contenedores.
  END.
END. /*available items_orden_entrega*/

pGenerico = vGenerico.
