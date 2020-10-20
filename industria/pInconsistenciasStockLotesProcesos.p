/*control de inconsistencias de stock*/

/*lotes cerrados y producciones origenes continuan en stock*/
CURRENT-WINDOW:WIDTH = 150.

FOR EACH lotes_jugo
    WHERE lotes_jugo.fecha_finalizacion <> ?
      AND lotes_jugo.fecha >= DATE("01/01/2007")
      .
  

  FOR EACH tambores_industria 
      WHERE tambores_industria.nromov_destino        = lotes_jugo.nromov
        AND tambores_industria.id_locacion_ubicacion = 4
        AND tambores_industria.id_estado            <> 8.
   /* 
    DISP lotes_jugo.id_lote lotes_jugo.anio lotes_jugo.nromov lotes_jugo.fecha 
         tambores_industria.id_lote tambores_industria.anio tambores_industria.id_tambor tambores_industria.id_estado WITH WIDTH 150.
   */ 
    
    tambores_industria.id_locacion_ubicacion = 10.
    tambores_industria.id_estado = 8.
    
  END.
  
END.


/*procesos cerrados y producciones involucradas inconsistentes*/

FOR EACH proceso
    WHERE proceso.fecha >= DATE("01/01/2007")
      AND proceso.fecha_fin <> ?.

  FOR EACH cargas
      WHERE cargas.nromov_proceso = proceso.nromov.
    
    FOR EACH tambores_industria
        WHERE tambores_industria.nromov_destino = cargas.nromov
          AND tambores_industria.id_locacion_ubicacion = 4
          AND tambores_industria.id_estado            <> 9.
      /*
      DISP proceso.id_proceso proceso.anio proceso.fecha
           cargas.id_carga cargas.anio
           tambores_industria.id_lote tambores_industria.anio tambores_industria.id_tambor tambores_industria.id_estado WITH WIDTH 150. 
      */
      
      tambores_industria.id_locacion_ubicacion = 10.
      tambores_industria.id_estado = 9.
      
    END.
    
    
  END.
  
END.
