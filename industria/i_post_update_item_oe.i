DEFINE var v_cond_venta as integer.
DEFINE var v_orden_entrega as integer.
DEFINE VAR v_items_oe AS INTEGER.
DEFINE var r as rowid.
DEFINE var hcon as handle.
DEFINE buffer b_items_oe for items_orden_entrega.
DEFINE var v_total_tam_pedidos as integer initial 0.
DEFINE var v_total_tam_asignados as integer initial 0.
DEFINE var v_fob_ton as decimal.

ASSIGN
general.items_orden_entrega.anio             = integer(items_orden_entrega.anio:screen-value in frame F-Main)
general.items_orden_entrega.item             = integer(items_orden_entrega.item:screen-value in frame F-Main)
general.items_orden_entrega.id_tipo_contrato = integer(items_orden_entrega.id_tipo_contrato:screen-value in frame F-Main)
general.items_orden_entrega.semana_entrega   = integer(items_orden_entrega.semana_entrega:screen-value in frame F-Main).

 
RUN get-container (output hcon). /* BUSCO EL HANDLE DE LA VENTANA CONTENEDORA DE SMART-OBJECTS */
RUN dame_datos_orden in hcon (output r). /* BUSCO EL ROWID DE LA TABLA ITEM_ORDEN_ENTREGA */

v_cond_venta    = INTEGER(id_condicion_venta:SCREEN-VALUE IN FRAME F-Main).
v_orden_entrega = items_orden_entrega.id_orden_entrega.
v_item_oe       = INTEGER(item_oe:SCREEN-VALUE IN FRAME F-Main).
/*
RUN calculo-gastos (INPUT v_orden_entrega, 
                    INPUT v_item_oe, 
                    INPUT v_cond_venta). /* LLAMA AL PROGRAMA i_calculo_gastos_item_oe.i */
*/
/* ESTA RUTINA YA SE EJECUTA EN EL PROGRAMA i_calculo_gastos_item_oe.i
RUN carga-fob (INPUT v_orden_entrega, 
               INPUT v_item_oe).
*/


FIND items_orden_entrega WHERE ROWID(items_orden_entrega) = r NO-LOCK NO-ERROR.
IF items_orden_entrega.id_orden_entrega > 1000 THEN DO:
    /* ACA SOLO HAGO EL CALCULO DE LAS OE DE EXPORTACION */
    FIND items_contratos OF items_orden_entrega NO-ERROR.
    IF AVAILABLE items_contratos THEN DO:
        v_total_tam_pedidos = items_contratos.cantidad.
        FOR EACH b_items_oe OF items_contratos.
            /* ACA ENTRO EN EL BUFFER PARA VER SI CUMPLO EL PEDIDO DE TAMBORES
                QUE SE HACE EN EL CONTRATO */
            IF b_items_oe.id_orden_entrega <> items_orden_entrega.id_orden_entrega AND
               b_items_oe.ITEM_oe          <> items_orden_entrega.ITEM_oe  THEN
                v_total_tam_asignados = v_total_tam_asignados + b_items_oe.cantidad_tambores.
            ELSE
                v_total_tam_asignados = v_total_tam_asignados + 
                                       INTEGER(items_orden_entrega.cantidad_tambores:screen-value in frame F-Main).
        END.
        IF v_total_tam_asignados = v_total_tam_pedidos THEN items_contratos.pendiente = FALSE.
        ELSE items_contratos.pendiente = TRUE.
    END.
END.

ASSIGN v_anio = 0
       v_item = 0
       v_id_tipo_contrato = 0
       v_semana_entrega = 0.
v_alta = 0.

RUN calculo-totales IN hcon.
/* TOTALIZO LOS VALORES DEL BROWSER */
