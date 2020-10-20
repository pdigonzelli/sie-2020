CURRENT-WINDOW:WIDTH = 150.
DEFINE BUFFER b_vtas FOR subd_vtas.

FIND packing_list WHERE nro_pack_list = "17109 A".

FOR EACH items_packing_list of packing_list NO-LOCK BY 
     items_packing_list.nro_lote BY items_packing_list.desde_lote:

  DISP items_packing_list.id_packing_list.
     FOR EACH r_items_venta_pack_list WHERE
          r_items_venta_pack_list.id_sucursal = items_packing_list.id_sucursal AND
          r_items_venta_pack_list.id_packing_list = items_packing_list.id_packing_list AND
          r_items_venta_pack_list.ITEM_pack = items_packing_list.ITEM NO-LOCK:
         FIND FIRST b_vtas OF r_items_venta_pack_list NO-LOCK NO-ERROR.
         FIND FIRST tipocomp OF b_vtas NO-LOCK NO-ERROR.

       IF AVAILABLE b_vtas THEN DO:
       
         DISP r_items_venta_pack_list.nromov
           r_items_venta_pack_list.id_punto_venta
           items_packing_list.cantidad
           b_vtas.nro_comp
           r_items_venta_pack_list.ITEM
           WITH WIDTH 150.
       END.
       ELSE DO:
          DISP r_items_venta_pack_list.nromov
           r_items_venta_pack_list.id_punto_venta
           items_packing_list.cantidad
           r_items_venta_pack_list.ITEM
           WITH WIDTH 150.
       END.
     END.


END.
