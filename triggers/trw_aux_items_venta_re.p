TRIGGER PROCEDURE FOR REPLICATION-WRITE OF aux_items_venta_re.
define buffer b_aux_items for aux_items_venta_re.

if available aux_items_venta_re Then
  do:
    /* Asigna nro_PO_cliente a todos los items */ 
    IF aux_items_venta_re.nro_PO_cliente <> "" THEN
        DO:
           FOR EACH b_aux_items WHERE
             b_aux_items.id_punto_venta = aux_items_venta_re.id_punto_venta AND
             b_aux_items.nromov = aux_items_venta_re.nromov AND
             b_aux_items.nro_PO_cliente = "":
                b_aux_items.nro_PO_cliente = aux_items_venta_re.nro_PO_cliente.
           END.
        END.

   /* Asigna nro_prod_cliente a todos los items */ 
    IF aux_items_venta_re.nro_prod_cliente <> "" THEN
        DO:
           FOR EACH b_aux_items  WHERE
             b_aux_items.id_punto_venta = aux_items_venta_re.id_punto_venta AND
             b_aux_items.nromov = aux_items_venta_re.nromov AND
             b_aux_items.nro_prod_cliente = "":
                b_aux_items.nro_prod_cliente = aux_items_venta_re.nro_prod_cliente.
           END.
        END.

   /* Asigna nro_release a todos los items */ 
    IF aux_items_venta_re.nro_release <> "" THEN
        DO:
           FOR EACH b_aux_items  WHERE
               b_aux_items.id_punto_venta = aux_items_venta_re.id_punto_venta AND
               b_aux_items.nromov = aux_items_venta_re.nromov AND
               b_aux_items.nro_release = "":
                b_aux_items.nro_release = aux_items_venta_re.nro_release.
           END.
        END.
end.
