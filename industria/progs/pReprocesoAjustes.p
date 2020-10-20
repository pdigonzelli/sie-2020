CURRENT-WINDOW:WIDTH = 150.
DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
        
            
FOR EACH registro_post_cierre.
  
  IF id_tipotambor <> 10 THEN DO:
    FOR FIRST lotes_jugo
        WHERE lotes_jugo.nromov = registro_post_cierre.nromov.
        .
        DISP "lote" lotes_jugo.id_lote lotes_jugo.anio lotes_jugo.fecha_finalizacion registro_post_cierre.fecha. 
        FOR EACH tambores_industria
            WHERE tambores_industria.nromov_destino = lotes_jugo.nromov
              AND tambores_industria.fecha_reproceso >= registro_post_cierre.fecha
            BREAK BY tambores_industria.nromov.
            i = i + 1.
            k = k + tambores_industria.kilos_tambor.
            IF LAST-OF(tambores_industria.nromov) THEN DO:
              DISP tambores_industria.id_lote 
                   tambores_industria.anio
                   tambores_industria.id_tipotambor
                   tambores_industria.fecha_reproceso
                   i
                   k
                   WITH WIDTH 150.
              i = 0.
              k = 0.
            END.
        END.
    END.
  END.

  IF registro_post_cierre.id_tipotambor = 13 THEN DO:
    FOR FIRST proceso
        WHERE proceso.nromo = registro_post_cierre.nromov.

        DISP "proceso" 
             proceso.id_proceso
             proceso.anio
             proceso.fecha
             registro_post_cierre.fecha.
             registro_post_cierre.id_tipotambor.
        FOR EACH cargas
            WHERE cargas.nromov_proceso = proceso.nromov.
          
          FOR EACH tambores_industria
              WHERE tambores_industria.nromov_destino = cargas.nromov
                AND tambores_industria.fecha_reproceso >= registro_post_cierre.fecha
              BREAK BY tambores_industria.nromov.
            i = i + 1.
            k = k + tambores_industria.kilos_tambor.
            IF LAST-OF(tambores_industria.nromov) THEN DO:
                DISP cargas.id_carga
                     tambores_industria.id_lote
                     tambores_industria.anio
                     tambores_industria.id_tipotambor
                     tambores_industria.fecha_reproceso
                     i 
                     k
                     WITH WIDTH 150.
                i = 0.
                k = 0.
            END.

          END.
        END.

    END.
  END.

    
END.
