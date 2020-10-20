



CURRENT-WINDOW:WIDTH=150.

FOR EACH subd_vtas WHERE id_operacion = 311 AND impreso AND nro_comp =686.
     DISP id_tipo_venta.            
     FOR EACH items_venta OF subd_vtas.

         /*
         DISP 
              items_venta.bultos 
              items_venta.cantidad 
              items_venta.kilos_por_envase 
              items_venta.peso
              items_venta.unidades
              items_venta.bultos
/*              items_venta.precio_base 
              items_venta.precio_local */
              items_venta.precio_origen 
              items_venta.tipo_unidad
              /*items_venta.subtotal_base 
              items_venta.subtotal_local */
              items_venta.subtotal_origen WITH WIDTH 150.*/

         FIND aux_subd_ventas OF subd_vtas NO-LOCK NO-ERROR.

         DISP aux_subd_ventas.nro_contrato.

         FOR EACH r_items_venta_pack_list 
                WHERE r_items_venta_pack_list.id_punto_venta = items_venta.id_punto_venta AND
                      r_items_venta_pack_list.nromov = items_venta.nromov AND
                      r_items_venta_pack_list.ITEM = items_venta.ITEM NO-LOCK:

             FIND FIRST items_packing_list WHERE
                      r_items_venta_pack_list.id_sucursal     =  items_packing_list.id_sucursal AND
                      r_items_venta_pack_list.id_packing_list = items_packing_list.id_packing_list AND
                      r_items_venta_pack_list.ITEM_pack       = items_packing_list.ITEM NO-LOCK NO-ERROR.
             IF AVAILABLE items_packing_list THEN DO:
             
                 FIND packing_list OF items_packing_list NO-LOCK NO-ERROR.
                 DISP packing_list.id_contrato. 

                 DISP  
                      items_packing_list.cantidad 
                      items_packing_list.hasta_lote 
                      items_packing_list.desde_lote
                      items_packing_list.nro_lote
                      items_packing_list.id_articulo
                      items_packing_list.kilos 
                      items_packing_list.kilos_contenedor
                      items_packing_list.kilos_brutos
                      items_packing_list.pallets.

             END.
         END.

     END.
END.
