/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al inbalance surcharge
  Parameters:   pIdOrdenEntrega id_orden_entrega
                pItemOE         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                pIdGasto        gastos_venta.id_gasto
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pDcr AS DECIMAL NO-UNDO.

DEFINE VAR vDcr      AS DECIMAL NO-UNDO.
DEFINE VAR vImpuesto AS DECIMAL NO-UNDO.

FIND items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega
                           AND items_orden_entrega.ITEM_oe = pItemOE 
                           NO-LOCK NO-ERROR.
IF AVAILABLE items_orden_entrega THEN DO:
  FOR EACH items_contratos OF items_orden_entrega NO-LOCK .
    FOR EACH r_gastos_items_contrato OF items_contratos NO-LOCK .
      IF r_gastos_items_contrato.id_gasto = 6  OR
         r_gastos_items_contrato.id_gasto = 7  OR
         r_gastos_items_contrato.id_gasto = 23 OR
         r_gastos_items_contrato.id_gasto = 24 OR
         r_gastos_items_contrato.id_gasto = 28 THEN DO:

        vImpuesto = vImpuesto + r_gastos_items_contrato.importe.
      END. /*if r_gastos_items_contrato ...*/
    END. /*for each r_gastos_items_contratos*/
  END. /*for each items_contratos*/
END. /*available items_orden_entrega*/

IF pIdGasto > 0  THEN DO:
  vDcr = vImpuesto * items_orden_entrega.TOTAL_galones. /*esto es seguro que es erroneo porque hay que estimar el total de galones*/
END. /*if pIdGasto*/

pDcr = vDcr.
