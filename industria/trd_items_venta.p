TRIGGER PROCEDURE FOR REPLICATION-DELETE OF items_venta.
define buffer b_auxitems for aux_items_venta.
define buffer b_r_gastos for r_gastos_items_venta.
define buffer b_impuestos for impuestos_items_factura.
define buffer b_valores for valores_persistentes_articulos.
DEFINE BUFFER b_rvtarto FOR r_items_venta_remitos.
DEFINE BUFFER b_rvtapack FOR r_items_venta_pack_list.
DEFINE BUFFER b_rvtades FOR r_items_venta_despacho.

if available items_venta Then
  do:
     FOR EACH b_auxitems OF items_venta:
         DELETE b_auxitems.
     END.
     FOR EACH b_r_gastos OF items_venta:
         DELETE b_r_gastos.
     END.
     FOR EACH b_impuestos OF items_venta :
        DELETE b_impuestos.
     END.
     FOR EACH b_valores OF items_venta:
        DELETE b_valores.
     END.
     FOR EACH b_rvtarto WHERE b_rvtarto.id_punto_venta = items_venta.id_punto_venta AND
         b_rvtarto.nromov = items_venta.nromov AND
         b_rvtarto.ITEM = items_venta.ITEM:
        DELETE b_rvtarto.
     END.
     FOR EACH b_rvtapack WHERE b_rvtapack.id_punto_venta = items_venta.id_punto_venta AND
       b_rvtapack.nromov = items_venta.nromov AND
       b_rvtapack.ITEM = items_venta.ITEM:
        DELETE b_rvtapack.
     END.

     FOR EACH b_rvtades WHERE b_rvtades.id_punto_venta = items_venta.id_punto_venta AND
              b_rvtades.nromov = items_venta.nromov AND
              b_rvtades.ITEM = items_venta.ITEM:
        DELETE b_rvtades.
     END.
end.
