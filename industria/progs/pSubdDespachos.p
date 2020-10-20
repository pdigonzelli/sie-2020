DEFINE VARIABLE hLIb AS HANDLE     NO-UNDO.

DEFINE VARIABLE hLibCom AS HANDLE.
RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p'). 
DELETE OBJECT hLibCom.
FOR EACH items_orden_entrega 
    WHERE fecha >= DATE('12/03/07')
      AND fecha <= DATE('18/03/07').
  RUN createSubdDespacho IN hLib (id_orden_entrega, ITEM_oe).
END.
