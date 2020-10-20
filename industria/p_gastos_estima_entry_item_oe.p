/*------------------------------------------------------------------------------
  Purpose:      Estimar el gasto correspondiente al entry
  Parameters:   INPUT  pIdOrdenEntrega id_orden_entrega
                INPUT  pItemOd         no se, parece que es la identificaion univoca de la tabla items_orden_entrega
                INPUT  pIdGasto        gastos_venta.id_gasto
                OUTPUT pEntry          devuelve el valor del entry
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER pIdOrdenEntrega LIKE orden_entrega.id_orden_entrega   NO-UNDO.
DEFINE INPUT PARAMETER pItemOE         AS INTEGER                            NO-UNDO.
DEFINE INPUT PARAMETER pIdGasto        LIKE gastos_venta.id_gasto            NO-UNDO.

DEFINE OUTPUT PARAMETER pEntry AS DECIMAL NO-UNDO.

DEFINE VAR vGalones      AS DECIMAL NO-UNDO.
DEFINE VAR vEntry        AS DECIMAL NO-UNDO.
DEFINE VAR vEntryParcial AS DECIMAL NO-UNDO.

FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = pIdOrdenEntrega NO-LOCK NO-ERROR.
FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = pIdOrdenEntrega 
                                 AND items_orden_entrega.ITEM_oe = pItemOE NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
  FIND destinos WHERE destinos.id_destino = orden_entrega.id_destino 
                  AND destinos.id_destino_grupo = 25 NO-LOCK NO-ERROR.
  IF items_orden_entrega.id_articulo = 52 OR 
     items_orden_entrega.id_articulo = 53 THEN DO:
    /*estimar los lotes para calcular la cantidad de galones*/
    /*tener en cuenta los valores de la tabla tabla_entry para los grados brix y demas conseguir el promedio de grados brix*/
    FIND FIRST tabla_entry WHERE tabla_entry.brix_desde >= 50.00 /*este valor es el promedio maximo de grados brix, varia mucho de acuerdo a los gpl*/
                             AND tabla_entry.brix_desde <= 51.99 /*cualquier valor menor que 52 se traduce en un gasto menor, estima casi siempre un valor mayor al real*/
                             AND tabla_entry.id_articulo = items_orden_entrega.id_articulo
                             NO-LOCK NO-ERROR.                   
    IF AVAILABLE tabla_entry THEN DO:
        vGalones = items_orden_entrega.cantidad_tambores * 53.6.
        /* calculo el importe del entry en funcion de la cantidad de tambores especificada en el contrato */
        vEntryParcial = vGalones * 3.785.
        vEntryParcial = vEntryParcial * tabla_entry.grados.
        vEntryParcial = vEntryParcial * 0.079.
        /* vEntryParcial = vEntryParcial + vEntryParcial. */
        /*vEntryParcial = 0.*/
    END. 
    ELSE DO:
        /*MESSAGE "No se encontro valores cargados para los brix " items_orden_entrega.grados_brix
                view-as alert-box.
        RETURN.*/
    END.
                                        
  END. /*if jugo turbio o jugo claro*/
  IF items_orden_entrega.id_articulo = 51 OR
     items_orden_entrega.id_articulo = 57 OR
     items_orden_entrega.id_articulo = 58 THEN DO:
     vEntryParcial = items_orden_entrega.total_factura * 0.038.
  END. /*if de aceites, water fase o oil fase*/    

END. /*orden_entrega*/

vEntry = vEntryParcial.
pEntry = vEntry.
