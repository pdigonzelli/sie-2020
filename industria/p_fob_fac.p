 DEFINE INPUT PARAMETER p_rowid AS ROWID.
 DEFINE OUTPUT PARAMETER v_fob LIKE subd_vtas.importe_origen.
 DEFINE OUTPUT PARAMETER v_gastos LIKE subd_vtas.importe_origen.
 
 DEFINE BUFFER b_vtas FOR subd_vtas.
 DEFINE BUFFER b_items FOR items_venta.
  
 
 FIND FIRST b_vtas WHERE ROWID(b_vtas) = p_rowid and
     b_vtas.estado = YES and
     b_vtas.id_punto_venta <> 9999
     NO-LOCK NO-ERROR.
 IF NOT AVAILABLE b_vtas THEN RETURN.


 v_fob = 0.
 v_gastos = 0.

 FOR EACH b_items OF b_vtas NO-LOCK:
     
     FIND FIRST tipocomp OF b_vtas NO-LOCK NO-ERROR.
     IF tipocomp.signo THEN
        v_fob = v_fob + (b_items.cantidad * b_items.precio_origen).
       ELSE
        v_fob = v_fob - (b_items.cantidad * b_items.precio_origen).

     
     FOR EACH r_gastos_items_venta OF b_items WHERE importe <> 0 NO-LOCK: 
             IF tipocomp.signo THEN
                 v_gastos = v_gastos +
                 (b_items.cantidad * r_gastos_items_venta.importe).
               ELSE
                 v_gastos = v_gastos -
                   (b_items.cantidad * r_gastos_items_venta.importe).

         END.
 END.








