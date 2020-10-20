DEFINE TEMP-TABLE ttLitoral
  FIELD id_empresa AS INTEGER
  FIELD id_sucursal AS INTEGER
  FIELD id_tipotambor AS INTEGER
  FIELD nromov AS INTEGER
  FIELD id_lote AS INTEGER
  FIELD anio AS INTEGER
  FIELD id_articulo AS INTEGER
  FIELD id_calidad AS INTEGER
  FIELD id_envase AS INTEGER
  FIELD tambores AS INTEGER
  FIELD kilos_tambor AS DECIMAL
  FIELD k400 AS DECIMAL
  FIELD fecha AS DATE
  FIELD nromov_destino AS INTEGER
  FIELD id_tipotambor_destino AS INTEGER
  FIELD id_tambor AS INTEGER
  FIELD fecha_reproceso AS DATE
  FIELD fruta_propia AS CHARACTER
  .

DEFINE TEMP-TABLE ttMovi
  FIELD id_lote_litoral AS INTEGER
  FIELD anio_litoral AS INTEGER
  FIELD articulo_litoral AS CHARACTER
  FIELD calidad_litoral AS CHARACTER
  FIELD tambores_litoral AS INTEGER
  FIELD kilos_litoral AS INTEGER
  FIELD fecha_litoral AS DATE
  FIELD fecha_reproceso AS DATE
  FIELD id_lote AS INTEGER
  FIELD anio AS INTEGER
  FIELD articulo_sami AS CHARACTER
  FIELD calidad_sami AS CHARACTER
  FIELD fecha_sami AS DATE
  FIELD id_proceso AS INTEGER
  FIELD anio_proceso AS INTEGER
  FIELD id_carga AS INTEGER
  FIELD fruta_propia AS CHARACTER
  .


DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

CURRENT-WINDOW:WIDTH = 150.

FOR EACH productos_terceros
    WHERE id_proveedor = 3416
    BY productos_terceros.id_lote.

  FOR EACH tambores_industria
        OF productos_terceros
      .
    

      CREATE ttLitoral.
      BUFFER-COPY tambores_industria EXCEPT tambores  k400 fruta_propia TO ttLitoral.
      



  END.
  
END.

FOR EACH tambores_industria
    WHERE id_sucursal = 461
    BY tambores_industria.id_lote.
 
      CREATE ttLitoral.
      BUFFER-COPY tambores_industria EXCEPT tambores fruta_propia k400 TO ttLitoral.
      ASSIGN fruta_propia = "fruta propia".


END.


FOR EACH ttLitoral
    BREAK BY ttLitoral.nromov_destino
     .

  i = i + 1.
  k = k + ttLitoral.kilos.

  IF LAST-OF(ttLitoral.nromov_destino) THEN DO:
    
    IF ttLitoral.id_tipotambor_destino <> 10 THEN DO:
    
      FIND FIRST tambores_industria
           WHERE tambores_industria.nromov = ttLitoral.nromov_destino
           NO-LOCK NO-ERROR.

      FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = ttLitoral.id_articulo  NO-LOCK NO-ERROR.
      FIND FIRST calidades WHERE calidades.id_calidad = ttLitoral.id_calidad NO-LOCK NO-ERROR.
      
      CREATE ttMovi.
      ASSIGN  ttMovi.id_lote_litoral  = ttLitoral.id_lote
              ttMovi.anio_litoral     = ttLitoral.anio  
              ttMovi.articulo_litoral = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
              ttMovi.calidad_litoral  = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
              ttMovi.fecha_litoral    = ttLitoral.fecha
              ttMovi.tambores_litoral = i
              ttMovi.kilos_litoral    = k
              ttMovi.fruta_propia     = ttLitoral.fruta_propia
              .
              
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      
      ASSIGN  ttMovi.id_lote          = tambores_industria.id_lote
              ttMovi.anio             = tambores_industria.anio
              ttMovi.articulo_sami    = productos_terminados.descripcion
              ttMovi.calidad_sami     = calidades.descripcion
              ttMovi.fecha_sami       = tambores_industria.fecha
              ttMovi.fecha_reproceso  = tambores_industria.fecha_reproceso
              .
      /*  
      DISP ttLitoral.id_lote
          ttLitoral.anio
          ttLitoral.id_articulo
          i
          tambores_industria.id_lote
          tambores_industria.anio
          WITH WIDTH 150.*/
    END.

    IF ttLitoral.id_tipotambor_destino = 10 THEN DO:

      FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = ttLitoral.id_articulo  NO-LOCK NO-ERROR.
      FIND FIRST calidades WHERE calidades.id_calidad = ttLitoral.id_calidad NO-LOCK NO-ERROR.

      
      FIND FIRST cargas 
           WHERE cargas.nromov = ttLitoral.nromov_destino
          NO-LOCK NO-ERROR.

      FIND FIRST proceso
           WHERE proceso.nromov = cargas.nromov_proceso
           NO-LOCK NO-ERROR.

      CREATE  ttMovi.
      ASSIGN  ttMovi.id_lote_litoral  = ttLitoral.id_lote
              ttMovi.anio_litoral     = ttLitoral.anio  
              ttMovi.articulo_litoral = productos_terminados.descripcion
              ttMovi.calidad_litoral  = calidades.descripcion
              ttMovi.fecha_litoral    = ttLitoral.fecha
              ttMovi.tambores_litoral = i
              ttMovi.kilos_litoral    = k
              ttMovi.id_proceso       = proceso.id_proceso
              ttMovi.anio_proceso     = proceso.anio
              ttMovi.id_carga         = cargas.id_carga
              ttMovi.fecha_reproceso  = ttLitoral.fecha_reproceso
              ttMovi.fruta_propia     = ttLitoral.fruta_propia
              .





    END.



      i = 0.
      k = 0.



  END.
  /*  
    DISP ttLitoral.id_lote ttLitoral.anio ttLitoral.tambores ttLitoral.id_articulo ttLitoral.id_sucursal
         ttLitoral.id_tambor ttLitoral.nromov_destino
      WITH WIDTH 150.*/
END.

RUN generateExcel.p (INPUT TABLE ttMovi,
                        INPUT " Movimientos Litoral",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).


/*  
FOR EACH ttMovi.
  DISP ttMovi WITH 2 COLUMNS WITH WIDTH 150.
END.*/
