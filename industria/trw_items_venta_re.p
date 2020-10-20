TRIGGER PROCEDURE FOR REPLICATION-WRITE OF items_venta_re.
define buffer b_auxitems for aux_items_venta_re.
DEFINE BUFFER b_rgastos FOR r_gastos_items_venta_re.
DEFINE BUFFER b_items FOR items_venta_re.
DEFINE VAR v_total LIKE items_venta_re.precio_origen.

if available items_venta_re Then
  do:
     FIND FIRST b_auxitems OF items_venta_re NO-LOCK NO-ERROR.
     IF NOT AVAILABLE b_auxitems THEN
     DO:
         CREATE b_auxitems.
         BUFFER-COPY items_venta_re TO b_auxitems.
     END.
     
     /* Crea gasto Flete */
     FIND FIRST b_rgastos OF items_venta_re WHERE 
         b_rgastos.id_gasto = 5 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE b_rgastos THEN
     DO:
        CREATE b_rgastos.
        BUFFER-COPY items_venta_re TO b_rgastos.
        ASSIGN b_rgastos.id_gasto = 5.
     END.
  
       /* Crea gasto Seguro */
     FIND FIRST b_rgastos OF items_venta_re WHERE 
         b_rgastos.id_gasto = 11 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE b_rgastos THEN
     DO:
        CREATE b_rgastos.
        BUFFER-COPY items_venta_re TO b_rgastos.
        ASSIGN b_rgastos.id_gasto = 11.
     END.

       /* Crea gasto Varios */
     FIND FIRST b_rgastos OF items_venta_re WHERE 
         b_rgastos.id_gasto = 12 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE b_rgastos THEN
     DO:
        CREATE b_rgastos.
        BUFFER-COPY items_venta_re TO b_rgastos.
        ASSIGN b_rgastos.id_gasto = 12.
     END.
     
     /* Asigna a todos los items el mismo precio */

     IF items_venta_re.precio_origen <> 0 THEN
     FOR EACH b_items WHERE b_items.id_punto_venta = items_venta_re.id_punto_venta 
         AND b_items.nromov = items_venta_re.nromov AND b_items.precio_origen = 0:
         ASSIGN b_items.precio_origen = items_venta_re.precio_origen.
     END.
     
     /* Recalcula gastos */

     v_total = 0.
     for each b_rgastos of items_venta_re:
        v_total = v_total + b_rgastos.importe.
     end.
     ASSIGN items_venta_re.gastos = v_total.



  end.
