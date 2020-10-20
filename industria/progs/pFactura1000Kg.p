CURRENT-WINDOW:WIDTH = 150.

/* factura lote 205 81-3873, nromov = 18797
   factura lote 302 81-3987, nromov = 19126  */

FOR EACH subd_vtas
    WHERE id_punto_venta = 81
      AND nromov = 19126, 
    EACH items_venta OF subd_vtas
    .

    UPDATE items_venta.cantidad
        WITH WIDTH 150.
END.
