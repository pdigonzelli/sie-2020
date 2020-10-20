
  DEFINE VARIABLE hQry AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLoc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAno AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTbs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCam AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lIn  AS LOGICAL    NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.
  /*
  cQry = DYNAMIC-FUNCTION('getQueryWhere').
  
  */
  CURRENT-WINDOW:WIDTH = 150.
  
  cQry = "FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_ubicacion = 95 AND tambores_industria.id_camara = 1    NO-LOCK     BY tambores_industria.id_lote        BY tambores_industria.id_tambor ".
  cQry = "FOR EACH tambores_industria WHERE (tambores_industria.id_sucursal_ubicacion = '95' AND tambores_industria.id_camara = '1')  NO-LOCK     BY tambores_industria.id_lote        BY tambores_industria.id_tambor INDEXED-REPOSITION".
  cQry = "FOR EACH tambores_industria WHERE (nro_columna_camara = 'D' AND tambores_industria.id_camara > '0' AND tambores_industria.id_sucursal_ubicacion = '95' AND tambores_industria.id_camara = '1') NO-LOCK     BY tambores_industria.id_camara BY tambores_industria.nro_fila_camara BY tambores_industria.nro_columna_camara BY tambores_industria.nromov BY tambores_industria.id_lote        BY tambores_industria.id_tambor INDEXED-REPOSITION".
  
  cQry = REPLACE(cQry, "tambores_industria", "buTam").
  
  CREATE QUERY hQry.  
  hQry:SET-BUFFERS(BUFFER buTam:HANDLE).
  hQry:QUERY-PREPARE(cQry).
  hQry:QUERY-OPEN().
  hQry:GET-FIRST().  
  IF hQry:QUERY-OFF-END THEN LEAVE.

  iNro = buTam.nromov.
  cLoc = STRING(buTam.id_camara) + buTam.nro_fila_camara + buTam.nro_columna_camara + STRING(buTam.nromov)  .

  

  REPEAT:      
    i = i + 1.
    hQry:GET-NEXT().  
    IF hQry:QUERY-OFF-END THEN LEAVE.    
  
    
    IF cLoc <> STRING(buTam.id_camara) + buTam.nro_fila_camara + buTam.nro_columna_camara + STRING(buTam.nromov) THEN DO:      
       
      cRet = cRet + cLot      + CHR(1) + 
                    cAno      + CHR(1) + 
                    cArt      + CHR(1) + 
                    cCal      + CHR(1) + 
                    cEnv      + CHR(1) + 
                    cFil      + CHR(1) + 
                    cCol      + CHR(1) +
                    STRING(i) + CHR(1) + 
                    cNro      + CHR(1) + 
                    cCam      + CHR(1) +  /*aqui va la descripcion de la camara*/
                    cSuc      + CHR(1) + 
                    cCam      + CHR(10).          
          
      i = 0.
      lIn = TRUE.
    END.

    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = buTam.id_articulo NO-LOCK NO-ERROR.
    FIND FIRST calidades WHERE calidades.id_calidad = buTam.id_calidad NO-LOCK NO-ERROR.
    FIND FIRST envases_prod WHERE envases_prod.id_envase = buTam.id_envase NO-LOCK NO-ERROR.

    ASSIGN cLot = STRING(buTam.id_lote)
       cAno = STRING(buTam.anio)
       cArt = productos_terminados.abreviatura
       cCal = calidades.descripcion
       cEnv = envases_prod.descripcion
       cFil = buTam.nro_fila_camara
       cCol = buTam.nro_columna_camara
       cNro = STRING(buTam.nromov)
       cCam = STRING(buTam.id_camara)
       cSuc = STRING(buTam.id_sucursal_ubicacion).

    cLoc = STRING(buTam.id_camara) + buTam.nro_fila_camara + buTam.nro_columna_camara + STRING(buTam.nromov).
    /*iNro = buTam.nromov.*/
    /*cLoc = STRING(iNro) + STRING(buTam.id_camara) + buTam.nro_fila_camara + buTam.nro_columna_camara.*/
  END.

  IF NOT lIn AND i > 0 THEN DO: /*solo un lote*/
    cRet = cRet + cLot      + CHR(1) + 
                  cAno      + CHR(1) + 
                  cArt      + CHR(1) + 
                  cCal      + CHR(1) + 
                  cEnv      + CHR(1) + 
                  cFil      + CHR(1) + 
                  cCol      + CHR(1) +
                  STRING(i) + CHR(1) + 
                  cNro      + CHR(1) + 
                  cCam      + CHR(1) +  /*aqui va la descripcion de la camara*/
                  cSuc      + CHR(1) + 
                  cCam      + CHR(10). 
  END.

  

  
  hQry:QUERY-CLOSE().
  DELETE OBJECT hQry.


  MESSAGE cRet VIEW-AS ALERT-BOX INFO BUTTONS OK.
