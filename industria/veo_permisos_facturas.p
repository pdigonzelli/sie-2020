DEFINE VAR v-varios AS DECIMAL.
DEFINE VAR v-flete AS DECIMAL.
DEFINE VAR v-bult AS INTEGER.

CURRENT-WINDOW:WIDTH = 130.

FOR EACH permisos_embarque,FIRST aduanas WHERE aduanas.id_aduana = permisos_embarque.id_aduana:
      DISP permisos_embarque.anio
           permisos_embarque.id_permiso_embarque
           permisos_embarque.importe.

      FOR EACH r_subd_ventas_embarque WHERE r_subd_ventas_embarque.nro_embarque = permisos_embarque.id_permiso_embarque AND
                                            r_subd_ventas_embarque.id_aduana    = permisos_embarque.id_aduana           AND
                                            r_subd_ventas_embarque.anio         = permisos_embarque.anio:
             FIND subd_vtas OF r_subd_ventas_embarque NO-LOCK NO-ERROR.

              v-bult =  0.
             FOR EACH items_venta OF subd_vtas NO-LOCK:
               
               v-flete  = 0.
               v-varios = 0.
               DISP items_venta.precio_origen 
                    items_venta.subtotal_origen.

               FOR EACH r_gastos_items_venta OF items_venta NO-LOCK:
                   CASE r_gastos_items_venta.id_gasto:
                          WHEN 5 OR WHEN 11 THEN
                             v-flete = v-flete + r_gastos_items_venta.importe.
                          WHEN 12 THEN
                          DO:
                             FIND FIRST ventas.aux_subd_venta OF subd_vtas NO-LOCK NO-ERROR.
                             IF AVAILABLE aux_subd_venta  THEN
                             DO:
                             CASE ventas.aux_subd_venta.id_clausula :
                                 WHEN 11 OR WHEN 12 OR WHEN 14 THEN 
                                     v-varios = v-varios + r_gastos_items_venta.importe.
                                 OTHERWISE
                                     v-flete = v-flete + r_gastos_items_venta.importe.
                             END CASE.
                            END.
                          END.
                    END CASE.


               END.
               v-bult = v-bult + items_venta.cantidad.

            END.

            DISP subd_vtas.id_punto_venta
                 subd_vtas.nro_comp
                 subd_vtas.nro_proforma
                 subd_vtas.importe_origen
                 v-bult
                 v-flete
                 v-varios WITH WIDTH 120.



      END.
END.

