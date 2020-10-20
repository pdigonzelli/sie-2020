TRIGGER PROCEDURE FOR REPLICATION-DELETE OF subd_vtas_re.
define buffer b_items for items_venta_re.
define buffer b_auxsub for aux_subd_ventas_re.
DEFINE BUFFER b_movi FOR movimientos_venta_re.
DEFINE buffer b_r_contenedor for ventas.r_subd_ventas_contenedor_re.
DEFINE buffer b_r_embarque for ventas.r_subd_ventas_embarque_re.

if available subd_vtas_re Then
  do:
     FOR EACH b_items OF subd_vtas_re:
         DELETE b_items.
     END.
     FOR EACH b_r_contenedor OF subd_vtas_re:
         DELETE b_r_contenedor.
     END.
     FOR EACH b_r_embarque OF subd_vtas_re:
         DELETE b_r_embarque.
     END.
     FOR EACH b_movi OF subd_vtas_re :
         DELETE b_movi.
     END.
     FOR EACH b_auxsub OF subd_vtas_re:
         DELETE b_auxsub.
     END.
  end.
