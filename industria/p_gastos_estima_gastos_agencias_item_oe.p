/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto por agencia maritima
  Parameters:   INPUT  pIdOrdenEntrega id_orden_entrega
                INPUT  pItemOE         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                INPUT  pIdGasto        gastos_venta.id_gasto
                INPUT  pCondVta        condicion de venta creo que de items_orden_entrega.id_condicion_venta
                OUTPUT pGasto          descripcion del gasto
                OUTPUT pMonto          monto del gasto
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega          NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                                   NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto                   NO-UNDO.
DEFINE INPUT PARAMETER pCondVta        LIKE items_orden_entrega.id_condicion_venta  NO-UNDO.

DEFINE OUTPUT PARAMETER pGasto AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pMonto AS DECIMAL   NO-UNDO.

DEFINE VAR vContenedores AS INTEGER NO-UNDO INITIAL 0.


FIND orden_entrega WHERE orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
  FIND FIRST gastos_agencias WHERE gastos_agencias.id_agencia = orden_entrega.id_agencia 
                               AND gastos_agencias.id_gasto   = pIdGasto
                               NO-LOCK NO-ERROR.
  IF AVAILABLE gastos_agencias THEN DO:
    /*
    FIND gastos_orden_entrega WHERE gastos_orden_entrega.id_orden_entrega = pIdOrdenEntrega 
                                AND gastos_orden_entrega.id_gasto         = pIdGasto
                                NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gastos_orden_entrega THEN DO: /*tengo que asegurarme que el gasto que estoy estimando no este en gastos_orden_entrega tampoco*/
      FIND r_gastos_agencias_clausulas WHERE r_gastos_agencia.id_agencia  = orden_entrega.id_agencia
                                         AND r_gastos_agencia.id_gasto    = pIdGasto
                                         AND r_gastos_agencia.id_clausula = pCondVta
                                         NO-LOCK NO-ERROR.
                                        
    /*comento esto porque no se porque puta no entra en el find first de gastos_agencias*/
      IF NOT AVAILABLE r_gastos_agencias_clausulas THEN DO: /*tengo que verificar que el gasto no figure como clausula porque se calcula en otro lado*/
    */  
        CASE gastos_agencias.id_gasto:
          WHEN 19 THEN DO:
            /*gasto BL, el unico que no se calcula por contenedor*/
            pMonto = gastos_agencias.importe.
          END.
          WHEN 29 THEN DO:
            /*gasto AGP que se calcula por tonelada y lo calculo para toda la orden*/
            FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK.
              pMonto = pMonto + ((items_orden_entrega.kgs_bruto / 10000) * gastos_agencia.importe).
            END.
          END.
          WHEN 32 THEN DO:
            pMonto = gastos_agencias.importe.
          END.
          OTHERWISE DO:
            FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK.
              vContenedores = vContenedores + items_orden_entrega.contenedores.
            END.
            pMonto = gastos_agencias.importe * vContenedores.
          END.
        END CASE.
      /*END. if not availbale r_gastos_agencias_clausula*/
    /*END. if not available gastos_orden_entrega*/
  END. /*if available gastos_agencias*/
END. /*if available orden_entrega*/

FIND gastos_venta WHERE gastos_venta.id_gasto = pIdGasto NO-LOCK NO-ERROR.
IF AVAILABLE gastos_venta THEN DO:
  pGasto = STRING(gastos_venta.id_gasto) + "-" + gastos_venta.descripcion.
END.

