DEFINE TEMP-TABLE ttProd
  RCODE-INFORMATION
  FIELD id_produccion     AS INTEGER COLUMN-LABEL "Produccion"
  FIELD anio              AS INTEGER COLUMN-LABEL "Anio"
  FIELD id_articulo       AS INTEGER COLUMN-LABEL "CodArt"
  FIELD id_sucursal       AS INTEGER COLUMN-LABEL "CodSuc"
  FIELD fecha             AS CHARACTER COLUMN-LABEL "Fecha"
  FIELD producto          AS CHARACTER COLUMN-LABEL "articulo"
  FIELD calidad           AS CHARACTER COLUMN-LABEL "calidad"
  FIELD envase            AS CHARACTER COLUMN-LABEL "envase"
  FIELD kilos             AS DECIMAL COLUMN-LABEL "kilos"
  FIELD tambores          AS INTEGER COLUMN-LABEL "tambores"
  FIELD bx                AS CHARACTER COLUMN-LABEL "Brix Refractometer (at 20° C)"
  FIELD ac                AS CHARACTER COLUMN-LABEL "Acidity % (w/w as anhydrous citric acid)"
  FIELD bx_corr           AS CHARACTER COLUMN-LABEL "Corrected Brix"
  FIELD ac_wv             AS CHARACTER COLUMN-LABEL "Acidity (w/v as anhydrous citric acid)"
  FIELD pulp85            AS CHARACTER COLUMN-LABEL "Background Pulp at 8.5º Brix"
  FIELD pulpa             AS CHARACTER COLUMN-LABEL "Pulpa Nominal"
  FIELD na                AS CHARACTER COLUMN-LABEL "Sodio"
  FIELD ratio             AS CHARACTER COLUMN-LABEL "Ratio"
  .

DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
DEFINE VARIABLE i AS INTEGER    NO-UNDO.


FOR EACH tambores_industria
    WHERE tambores_industria.fecha >= DATE('01/01/2008')
      AND tambores_industria.fecha <= DATE('31/12/2009')
      AND (id_tipotambor = 1 OR id_tipotambor = 4 OR id_tipotambor = 5)
      AND (id_articulo = 521 OR id_articulo = 952 OR id_articulo = 52 OR id_articulo = 523 OR id_articulo = 524)
    BREAK BY nromov.
  i = i + 1.
  k = k + kilos_tambor.
  IF LAST-OF(nromov) THEN DO:
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST produccion_jugo OF tambores_industria NO-LOCK NO-ERROR.
 
    IF tambores_industria.id_tipotambor = 4 THEN DO: /*tambor de sobrante*/
    FOR FIRST sobrante WHERE sobrante.id_empresa             = tambores_industria.id_empresa
                         AND sobrante.id_sucursal            = tambores_industria.id_sucursal
                         AND sobrante.id_tipotambor_sobrante = tambores_industria.id_tipotambor
                         AND sobrante.nromov_sobrante        = tambores_industria.nromov
                       NO-LOCK.
      FOR FIRST inspecciones_lote WHERE inspecciones_lote.id_empresa    = sobrante.id_empresa
                                    AND inspecciones_lote.id_sucursal   = sobrante.id_sucursal
                                    AND inspecciones_lote.id_tipotambor = sobrante.id_tipotambor
                                    AND inspecciones_lote.nromov        = sobrante.nromov
                                    AND inspecciones_lote.final         = TRUE 
                                  NO-LOCK .
            CREATE ttProd.
            BUFFER-COPY tambores_industria EXCEPT fecha TO ttProd.
            ASSIGN  ttProd.bx             = STRING(inspecciones_lote.bx_20_20)
                    ttProd.ac             = STRING(inspecciones_lote.acidez_w_w)
                    ttProd.bx_corr        = STRING(inspecciones_lote.bx_correg)
                    ttProd.ac_wv          = STRING(inspecciones_lote.acidez_w_v)
                    ttProd.pulp85         = STRING(inspecciones_lote.porcentaje_pulpa)
                    ttProd.pulpa          = STRING(inspecciones_lote.porcentaje_pulpa)
                    ttProd.na             = STRING(0)
                    ttProd.ratio          = STRING(inspecciones_lote.ratio)
                    ttProd.tambores       = i
                    ttProd.kilos          = k
                    ttProd.id_produccion  = tambores_industria.id_lote
                    ttProd.producto       = productos_terminados.descripcion
                    ttProd.calidad        = calidades.descripcion
                    ttProd.envase         = envases_prod.descripcion
                    ttProd.fecha          = "'" + STRING(tambores_industria.fecha)
                    .
      END.
    END.
  END.

  IF tambores_industria.id_tipotambor = 1 THEN DO: /*tambor de produccion*/
    FOR FIRST produccion_jugo WHERE produccion_jugo.id_empresa    = tambores_industria.id_empresa
                                AND produccion_jugo.id_sucursal   = tambores_industria.id_sucursal
                                AND produccion_jugo.id_tipotambor = tambores_industria.id_tipotambor
                                AND produccion_jugo.nromov        = tambores_industria.nromov
                              NO-LOCK .
          CREATE ttProd.
          BUFFER-COPY tambores_industria EXCEPT fecha TO ttProd.
          ASSIGN  ttProd.bx             = STRING(produccion_jugo.bx_20_20)
                  ttProd.ac             = STRING(produccion_jugo.acidez_w_w)
                  ttProd.bx_corr        = STRING(produccion_jugo.bx_correg)
                  ttProd.ac_wv          = STRING(produccion_jugo.acidez_w_v)
                  ttProd.pulp85         = STRING(produccion_jugo.pulpa_85)
                  ttProd.pulpa          = STRING(produccion_jugo.pulpa)
                  ttProd.na             = STRING(produccion_jugo.sodio)
                  ttProd.ratio          = STRING(produccion_jugo.ratio)
                  ttProd.tambores       = i
                  ttProd.kilos          = k
                  ttProd.id_produccion  = produccion_jugo.id_produccion
                  ttProd.producto       = productos_terminados.descripcion
                  ttProd.calidad        = calidades.descripcion
                  ttProd.envase         = envases_prod.descripcion
                  ttProd.fecha          = "'" + STRING(tambores_industria.fecha)
                  .


    END.

  END.
    

    i = 0.
    k = 0.
        
  END.
END.



RUN generateExcel.p (INPUT TABLE ttProd,
                        INPUT " Producciones",
                        INPUT " Periodo Desde: 01/06/2008 Hasta: 31/12/2008"  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).
