TRIGGER PROCEDURE FOR REPLICATION-DELETE OF subd_vtas.
define buffer b_items for items_venta.
define buffer b_auxsub for aux_subd_ventas.
DEFINE BUFFER b_movi FOR movimientos_venta.
DEFINE buffer b_r_contenedor for ventas.r_subd_ventas_contenedor.
DEFINE buffer b_r_embarque for ventas.r_subd_ventas_embarque.
DEFINE BUFFER b_r_fact FOR r_fact_ventas.

if available subd_vtas Then
  do:
     FOR EACH b_items OF subd_vtas:
         DELETE b_items.
     END.
     FOR EACH b_r_contenedor OF subd_vtas:
         DELETE b_r_contenedor.
     END.
     FOR EACH b_r_embarque OF subd_vtas:
         DELETE b_r_embarque.
     END.
     FOR EACH b_movi OF subd_vtas :
         DELETE b_movi.
     END.
     FOR EACH b_auxsub OF subd_vtas:
         DELETE b_auxsub.
     END.
     FOR EACH b_r_fact OF subd_vtas:
         DELETE b_r_fact.
     END.

  end.
