TRIGGER PROCEDURE FOR REPLICATION-DELETE OF items_venta_re.
define buffer b_auxitems for aux_items_venta_re.
define buffer b_r_gastos for r_gastos_items_venta_re.
define buffer b_impuestos for impuestos_items_factura_re.
define buffer b_valores for valores_persistentes_articulos_re.
DEFINE BUFFER b_rvtarto FOR r_items_venta_remitos_re.
DEFINE BUFFER b_rvtapack FOR r_items_venta_pack_list_re.

if available items_venta_re Then
  do:
     FOR EACH b_auxitems OF items_venta_re:
         DELETE b_auxitems.
     END.
     FOR EACH b_r_gastos OF items_venta_re:
         DELETE b_r_gastos.
     END.
     FOR EACH b_impuestos OF items_venta_re :
        DELETE b_impuestos.
     END.
     FOR EACH b_valores OF items_venta_re:
        DELETE b_valores.
     END.
     FOR EACH b_rvtarto WHERE b_rvtarto.id_punto_venta = items_venta_re.id_punto_venta AND
         b_rvtarto.nromov = items_venta_re.nromov AND
         b_rvtarto.ITEM = items_venta_re.ITEM:
        DELETE b_rvtarto.
     END.
     FOR EACH b_rvtapack WHERE b_rvtapack.id_punto_venta = items_venta_re.id_punto_venta AND
       b_rvtapack.nromov = items_venta_re.nromov AND
       b_rvtapack.ITEM = items_venta_re.ITEM:
        DELETE b_rvtapack.
     END.


end.
