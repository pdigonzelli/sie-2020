CURRENT-WINDOW:WIDTH = 150.

FOR EACH subd_vtas
    WHERE id_punto_venta = 81
      AND nromov = 18707 
    NO-LOCK,
    EACH items_venta OF subd_vtas
    NO-LOCK, 
    EACH aux_items_venta OF items_venta
    NO-LOCK.
  
  DISP aux_items_venta
     WITH 2 COLUMNS WIDTH 150.

END.
