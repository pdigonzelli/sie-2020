DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE BUFFER bUbi FOR lotes_ubicacion.

FOR EACH lotes_cascara
    WHERE id_lote = 28
      AND anio = 2007 
    NO-LOCK.
  DISP id_lote anio nromov.
  FOR EACH lotes_ubicacion
        OF lotes_cascara.
    i = i + cantidad.
    /*  
    CREATE bubi.
    BUFFER-COPY lotes_ubicacion EXCEPT id_sucursal_ubicacion cantidad TO bubi.
    bubi.id_sucursal_ubicacion = 95.*/
   
     DISP id_sucursal_ubicacion cantidad.
     /*
    UPDATE id_sucursal_ubicacion  cantidad.*/
  END.
  DISP i.
END.

