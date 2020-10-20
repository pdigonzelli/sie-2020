/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al THC
  Parameters:   pIdOrdenEntrega id_orden_entrega
                pItemOd         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                pIdGasto        gastos_venta.id_gasto
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pThc AS DECIMAL NO-UNDO.

DEFINE VAR vThc  AS DECIMAL NO-UNDO.


FIND orden_entrega
  WHERE orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
  FIND items_orden_entrega 
    WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega
      AND items_orden_entrega.ITEM_oe = pItemOE NO-LOCK NO-ERROR.
  IF AVAILABLE items_orden_entrega THEN DO:
    FIND FIRST gastos_agencia 
      WHERE gastos_agencia.id_agencia = orden_entrega.id_agencia
        AND gastos_agencia.id_gasto   = pIdGasto NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_agencia THEN DO:
      vThc = gastos_agencia.importe * items_orden_entrega.contenedores.
    END. /*available gastos_agencia*/
    ELSE DO:
      FIND tipo_contenedor OF items_orden_entrega NO-LOCK NO-ERROR.
      IF AVAILABLE tipo_contenedor THEN DO:
        vThc = tipo_contenedor.precio_thc * items_orden_entrega.contenedores.
      END. /*available tipo_contenedor*/
    END. /*else if available gastos_agencia*/
  END. /*available items_orden_entrega*/
END. /*available orden_entrega*/

pThc = vThc.
