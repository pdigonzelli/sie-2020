DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE BUFFER bUbi FOR lotes_ubicacion.

FOR EACH lotes_cascara
    WHERE id_lote = 2
      AND anio = 2007
    NO-LOCK.
  DISP id_lote anio nromov.
  FOR EACH lotes_ubicacion OF lotes_cascara.
     
    DISP id_sucursal_ubicacion cantidad.
    
    /*
    CREATE bubi.
    BUFFER-COPY lotes_ubicacion EXCEPT id_sucursal_ubicacion cantidad TO bubi.
    ASSIGN bubi.id_sucursal_ubicacion = 95
           bubi.cantidad = 177.  */
    /*  
     UPDATE id_sucursal_ubicacion cantidad.*/
    i = i + lotes_ubicacion.cantidad.
  END.
  DISP i (3682 - i).
END.
