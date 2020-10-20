TRIGGER PROCEDURE FOR REPLICATION-WRITE OF items_venta.
define buffer b_auxitems for aux_items_venta.
DEFINE BUFFER b_rgastos FOR r_gastos_items_venta.
DEFINE BUFFER b_items FOR items_venta.
DEFINE BUFFER b_vtas FOR subd_vtas.
DEFINE VAR v_total LIKE items_venta.precio_origen.
DEFINE VAR v_unidad AS CHARACTER.
DEFINE VAR v_descripcion AS CHARACTER.

if available items_venta Then
  do:

     FIND FIRST b_vtas OF items_venta NO-LOCK NO-ERROR.
    
     FIND FIRST b_auxitems OF items_venta NO-ERROR.
     IF NOT AVAILABLE b_auxitems THEN
     DO:
         CREATE b_auxitems.
         BUFFER-COPY items_venta TO b_auxitems.
     END.
     ELSE
     DO:
         
         /* Carga descripcion del producto para ME */
         IF b_vtas.id_operacion = 311 AND b_vtas.mercado = NO THEN
             DO:
                 IF items_venta.id_articulo <> 0 and
                    b_auxitems.descripcion = "" THEN
                 DO:
                    FIND FIRST productos_terminados OF items_venta NO-LOCK.
                    FIND FIRST calidades OF items_venta NO-LOCK.
                    FIND FIRST aux_items_venta OF items_venta NO-LOCK.
                    CASE items_venta.tipo_unidad:
                        WHEN "K" THEN
                            v_unidad = "Kgs  of ". 
                        WHEN "T" THEN
                            v_unidad = "Tns  of ".
                        WHEN "G" THEN
                            v_unidad = "Gallons  of ". 
                        WHEN "L" THEN
                            v_unidad = "Pounds  of ".
                        OTHERWISE
                            v_unidad = "".
                    END CASE.
                    IF AVAILABLE productos_terminados THEN
                        v_descripcion = v_unidad +  productos_terminados.descripcion_ingles.
                    IF AVAILABLE calidades THEN
                    DO:
                        IF items_venta.id_articulo <> 50 AND
                           items_venta.id_articulo <> 51 AND
                           items_venta.id_articulo <> 57 AND
                           items_venta.id_articulo <> 58 THEN
                        v_descripcion = v_descripcion + "   " + calidades.descripcion_ingles.
                    END.
                    ASSIGN b_auxitems.descripcion = v_descripcion.
                 END.
             END.

             IF b_vtas.id_operacion = 311 AND b_vtas.mercado = YES THEN
             DO:

                 FIND FIRST productos_terminados OF items_venta NO-LOCK.
                 FIND FIRST calidades OF items_venta NO-LOCK.
                 FIND FIRST aux_items_venta OF items_venta NO-LOCK.
                 CASE items_venta.tipo_unidad:
                     WHEN "K" THEN
                         v_unidad = "Kgs  de ". 
                     WHEN "T" THEN
                         v_unidad = "Tns  de ".
                     OTHERWISE
                         v_unidad = "".
                 END CASE.
                 IF AVAILABLE productos_terminados THEN
                     v_descripcion = v_unidad +  productos_terminados.descripcion.
                 IF AVAILABLE calidades THEN
                     v_descripcion = v_descripcion  + "   " + calidades.descripcion.

                 ASSIGN b_auxitems.descripcion = v_descripcion.
             END.

     END.
     RELEASE b_auxitems.

     /* Carga bultos nominales y estandar */
     RUN p_carga_nomstd.p (INPUT ROWID(items_venta)).


     IF b_vtas.mercado = NO THEN /* Externo */
     DO:

         /* Crea gasto Flete */
         FIND FIRST b_rgastos OF items_venta WHERE b_rgastos.id_gasto = 5 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE b_rgastos THEN
         DO:
            CREATE b_rgastos.
            BUFFER-COPY items_venta TO b_rgastos.
            ASSIGN b_rgastos.id_gasto = 5.
         END.
      
           /* Crea gasto Seguro */
         FIND FIRST b_rgastos OF items_venta WHERE b_rgastos.id_gasto = 11 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE b_rgastos THEN
         DO:
            CREATE b_rgastos.
            BUFFER-COPY items_venta TO b_rgastos.
            ASSIGN b_rgastos.id_gasto = 11.
         END.
    
          /* Crea gasto Comision */
         FIND FIRST b_rgastos OF items_venta WHERE b_rgastos.id_gasto = 10 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE b_rgastos THEN
         DO:
            CREATE b_rgastos.
            BUFFER-COPY items_venta TO b_rgastos.
            ASSIGN b_rgastos.id_gasto = 10.
         END.
    
           /* Crea gasto Varios */
         FIND FIRST b_rgastos OF items_venta WHERE b_rgastos.id_gasto = 12 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE b_rgastos THEN
         DO:
            CREATE b_rgastos.
            BUFFER-COPY items_venta TO b_rgastos.
            ASSIGN b_rgastos.id_gasto = 12.
         END.

     END.

     /* Asigna a todos los items el mismo precio */

     IF items_venta.precio_origen <> 0 THEN
     FOR EACH b_items WHERE b_items.id_punto_venta = items_venta.id_punto_venta 
         AND b_items.nromov = items_venta.nromov AND b_items.precio_origen = 0:
         ASSIGN b_items.precio_origen = items_venta.precio_origen.
     END.
     
     /* Recalcula gastos */

     v_total = 0.
     for each b_rgastos of items_venta NO-LOCK:
        v_total = v_total + b_rgastos.importe.
     end.
     ASSIGN items_venta.gastos = v_total.

     RELEASE b_rgastos.

  end.
