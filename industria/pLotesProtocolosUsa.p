DEFINE TEMP-TABLE ttProto
  RCODE-INFORMATION
  FIELD id_lote           AS INTEGER COLUMN-LABEL "Lote"
  FIELD anio              AS INTEGER COLUMN-LABEL "Anio"
  FIELD id_articulo       AS INTEGER COLUMN-LABEL "codart"
  FIELD producto          AS CHARACTER COLUMN-LABEL "articulo"
  FIELD calidad           AS CHARACTER COLUMN-LABEL "calidad"
  FIELD envase            AS CHARACTER COLUMN-LABEL "envase"
  FIELD kilos             AS DECIMAL COLUMN-LABEL "kilos"
  FIELD tambores          AS INTEGER COLUMN-LABEL "tambores"
  FIELD bx                AS CHARACTER COLUMN-LABEL "Brix Refractometer (at 20° C)"
  FIELD ac                AS CHARACTER COLUMN-LABEL "Acidity % (w/w as anhydrous citric acid)"
  FIELD bx_corr           AS CHARACTER COLUMN-LABEL "Corrected Brix"
  FIELD dens_bx           AS CHARACTER COLUMN-LABEL "Density Brix Corrected"
  FIELD ac_wv             AS CHARACTER COLUMN-LABEL "Acidity (w/v as anhydrous citric acid)"
  FIELD amino             AS CHARACTER COLUMN-LABEL "Total amino nitrogen content"
  FIELD ascorbic          AS CHARACTER COLUMN-LABEL "Ascorbic acid"
  FIELD micro             AS CHARACTER COLUMN-LABEL "Microbiology"
  FIELD pulp              AS CHARACTER COLUMN-LABEL "Pulp at 8,5° Brix"
  FIELD pulp8             AS CHARACTER COLUMN-LABEL "Pulp at 8,0° Brix"
  FIELD pulp63            AS CHARACTER COLUMN-LABEL "Pulp at 6.3º Brix"
  FIELD pulp11            AS CHARACTER COLUMN-LABEL "Pulp at 11° Brix"
  FIELD pulp20            AS CHARACTER COLUMN-LABEL "Pulp concentration on 20 mesh"
  FIELD pulp8dgr          AS CHARACTER COLUMN-LABEL "Pulp at 8º Brix"
  FIELD pulp7dgr          AS CHARACTER COLUMN-LABEL "Pulp at 7º Brix"
  FIELD pulp85            AS CHARACTER COLUMN-LABEL "Background Pulp at 8.5º Brix"
  FIELD pulp820           AS CHARACTER COLUMN-LABEL "Pulp at 8º Brix 20/20"
  FIELD pulp920           AS CHARACTER COLUMN-LABEL "Pulp at 9º Brix 20/20"
  FIELD pulp45            AS CHARACTER COLUMN-LABEL "Pulp at 4.5% Ac. w/w"
  FIELD pulpanal          AS CHARACTER COLUMN-LABEL "Porcentaje Pulpa Analisis"
  FIELD ratio             AS CHARACTER COLUMN-LABEL "Ratio"
  FIELD descripcionpulpa  AS CHARACTER COLUMN-LABEL "Analisis Pulpa"
  FIELD valorpulpa        AS CHARACTER COLUMN-LABEL "Valor"
  FIELD caracteristicas   AS CHARACTER COLUMN-LABEL "caracteristicas"
  FIELD fecha_protocolo   AS CHARACTER COLUMN-LABEL "Fecha Protocolo"
  FIELD fecha_produccion  AS CHARACTER COLUMN-LABEL "Fecha Produccion".


DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE VARIABLE cCar AS CHARACTER  NO-UNDO.
DEFINE VARIABLE crlf AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cNom AS CHARACTER  NO-UNDO.

CURRENT-WINDOW:WIDTH = 150.
crlf =  CHR(13) + CHR(10).

FOR EACH lotes_jugo
    WHERE id_tipotambor  = 3
      AND lotes_jugo.fecha         >= DATE('01/01/2005')
      AND lotes_jugo.fecha         <= DATE('31/12/2009')
      AND (lotes_jugo.id_articulo = 52 OR lotes_jugo.id_articulo = 53)
    NO-LOCK.
  FOR EACH tambores_industria
        OF lotes_jugo
      NO-LOCK.
    i = i + 1.
    k = k + kilos_tambor.
  END.

  FIND FIRST protocolos
       WHERE protocolos.id_empresa    = lotes_jugo.id_empresa
         AND protocolos.id_sucursal   = lotes_jugo.id_sucursal
         AND protocolos.id_tipotambor = lotes_jugo.id_tipotambor
         AND protocolos.nromov        = lotes_jugo.nromov
       NO-LOCK NO-ERROR.

  FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
  FIND FIRST productos_terminados OF lotes_jugo NO-LOCK NO-ERROR.
  FIND FIRST envases_prod OF lotes_jugo NO-LOCK NO-ERROR.
  FIND FIRST inspecciones_lote OF lotes_jugo NO-LOCK NO-ERROR.

  CREATE ttProto.
  ASSIGN ttProto.id_lote     = lotes_jugo.id_lote
    ttProto.anio             = lotes_jugo.anio
    ttProto.id_articulo      = lotes_jugo.id_articulo
    ttProto.producto         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
    ttProto.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
    ttProto.envase           = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE"
    ttProto.tambores         = i
    ttProto.kilos            = k
    ttProto.pulpanal         = IF AVAILABLE inspecciones_lote THEN STRING(inspecciones_lote.porcentaje_pulpa) ELSE ""
    ttProto.caracteristicas  = "" /* cCar */
    ttProto.fecha_protocolo  = IF AVAILABLE protocolos THEN STRING(protocolo.fecha, "99/99/9999") ELSE ""
    ttProto.fecha_produccion = STRING(lotes_jugo.fecha_finalizacion, "99/99/9999").


  IF AVAILABLE protocolos THEN DO:
    FOR EACH items_protocolos
          OF protocolos
        NO-LOCK.
      CASE items_protocolos.id_caracteristica:
        WHEN 1 THEN ttProto.bx = items_protocolos.valor_caracter.
        WHEN 2 THEN ttProto.ac = items_protocolos.valor_caracter.
        WHEN 3 THEN ttProto.bx_corr = items_protocolos.valor_caracter.
        WHEN 4 THEN ttProto.dens_bx = items_protocolos.valor_caracter.
        WHEN 5 THEN ttProto.ac_wv = items_protocolos.valor_caracter.
        WHEN 6 THEN ttProto.amino = items_protocolos.valor_caracter.
        WHEN 7 THEN ttProto.ascorbic = items_protocolos.valor_caracter.
        WHEN 21 THEN ttProto.ratio = items_protocolos.valor_caracter.
        WHEN 12 THEN ttProto.micro = items_protocolos.valor_caracter.
        WHEN 23 THEN do: 
          ASSIGN ttProto.descripcionpulpa  = "Pulp at 8,5° Brix".
                 ttProto.valorpulpa        = items_protocolos.valor_caracter.
                 ttProto.pulp              = items_protocolos.valor_caracter.
        END.      
        WHEN 72 THEN DO:
          ASSIGN cNom                     = "Pulp at 8,0° Brix"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp8            = items_protocolos.valor_caracter.
        END.
        WHEN 74 THEN do: 
          ASSIGN cNom                     = "Pulp at 6.3º Brix"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp63           = items_protocolos.valor_caracter.
        END.
        WHEN 75 THEN do:       
          ASSIGN cNom                     = "Pulp at 11° Brix"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp11           = items_protocolos.valor_caracter.
        END.
        WHEN 81 THEN do: 
          ASSIGN cNom                     = "Pulp concentration on 20 mesh" 
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp20           = items_protocolos.valor_caracter.
        END.
        WHEN 85 THEN DO:
          ASSIGN cNom                     = "Pulp at 8º Brix"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp8dgr         = items_protocolos.valor_caracter.
        END.
        WHEN 86 THEN  do:
          ASSIGN cNom                     = "Pulp at 7º Brix"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp7dgr         = items_protocolos.valor_caracter.
        END.
        WHEN 90 THEN do:
          ASSIGN cNom                     = "Background Pulp at 8.5º"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp85           = items_protocolos.valor_caracter.
        END.
        WHEN 99 THEN do:
          ASSIGN cNom                     = "Pulp at 8º Brix 20/20"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp820          = items_protocolos.valor_caracter.
        END.
        WHEN 104 THEN do:
          ASSIGN cNom                     = "Pulp at 9º Brix 20/20"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp920          = items_protocolos.valor_caracter.
        END.
        WHEN 107 THEN do:
          ASSIGN cNom                     = "Pulp at 4.5% Ac. w/w"
                 ttProto.descripcionpulpa = cNom
                 ttProto.valorpulpa       = items_protocolos.valor_caracter
                 ttProto.pulp45           = items_protocolos.valor_caracter.
        END.
    
    
      END CASE.

      FIND FIRST caracteristicas_quimicas OF items_protocolos
          NO-LOCK NO-ERROR.
      cCar = cCar + caracteristicas_quimicas.descripcion + ": " + string(items_protocolos.valor_caracter) + crlf.

    END.
  END.

  
  
 
  i = 0.
  k = 0.
  cCar = "".


END.


RUN generateExcel.p (INPUT TABLE ttProto,
                        INPUT " Lotes 2004 2005 - Protocolos",
                        INPUT " Fecha Desde: 01/01/2005"  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).


/*
    FIND FIRST protocolos
         WHERE protocolos.id_empresa = tambores_industria.id_empresa
           AND protocolos.id_sucursal = tambores_industria.id_sucursal
           AND protocolos.id_tipotambor = tambores_industria.id_tipotambor
           AND protocolos.nromov = tambores_industria.nromov
         NO-LOCK NO-ERROR.
    IF AVAILABLE protocolos THEN DO:
      FOR EACH items_protocolos
            OF protocolos
          NO-LOCK.
        DISP items_protocolos.id_caracteristica
             items_protocolos.valor.
      END.
    END.
*/
