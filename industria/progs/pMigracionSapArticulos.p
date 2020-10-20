CURRENT-WINDOW:WIDTH = 200.
DEFINE TEMP-TABLE ttProductos 
  RCODE-INFORMATION
  FIELD flia        AS CHARACTER  COLUMN-LABEL "Flia Producto"
  FIELD id_articulo AS INTEGER    COLUMN-LABEL "Codigo Producto"
  FIELD articulo    AS CHARACTER  COLUMN-LABEL "Producto"
  FIELD art_ingles  AS CHARACTER  COLUMN-LABEL "Producto-ingles"
  FIELD art_abrev   AS CHARACTER  COLUMN-LABEL "Producto-abrev"
  FIELD art_abrev_i AS CHARACTER  COLUMN-LABEL "Producto-abrev-ingles"

  FIELD id_calidad  AS INTEGER    COLUMN-LABEL "Codigo Calidad"
  FIELD calidad     AS CHARACTER  COLUMN-LABEL "Calidad"
  FIELD cal_abrev   AS CHARACTER  COLUMN-LABEL "Calidad-abrev"
 
  FIELD id_envase   AS INTEGER    COLUMN-LABEL "Codigo Envase"
  FIELD envase      AS CHARACTER  COLUMN-LABEL "Envase"
  FIELD env_abrev   AS CHARACTER  COLUMN-LABEL "Envase-abrev"

  FIELD rnpa        AS CHARACTER  COLUMN-LABEL "RNPA"
  FIELD kilos_env   AS DECIMAL    COLUMN-LABEL "Kilos Envase"
  .

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

DEFINE VARIABLE hLibCom AS HANDLE.
RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
DELETE OBJECT hLibCom.

FOR EACH tambores_industria
    WHERE tambores_industria.fecha >= DATE('01/01/2007')
    BREAK BY tambores_industria.id_articulo.

  IF LAST-OF(tambores_industria.id_articulo) THEN DO:

    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.

    CREATE ttProductos.
    ASSIGN  ttProductos.flia        = DYNAMIC-FUNCTION('getDescTipoTambor' IN hLib, tambores_industria.id_tipotambor)      
            ttProductos.id_articulo = productos_terminados.id_articulo
            ttProductos.articulo    = productos_terminados.descripcion
            ttProductos.art_ingles  = productos_terminados.descripcion_ingles
            ttProductos.art_abrev   = productos_terminados.abreviatura
            ttProductos.art_abrev_i = productos_terminados.abreviatura_ingles
            ttProductos.id_calidad  = calidades.id_calidad
            ttProductos.calidad     = calidades.descripcion
            ttProductos.cal_abrev   = calidades.abreviatura
            ttProductos.id_envase   = envases_prod.id_envase
            ttProductos.envase      = envases_prod.descripcion
            ttProductos.env_abrev   = envases_prod.abreviatura
            .

    FOR FIRST r_productos_calidad_envase
        WHERE r_productos_calidad_envase.id_articulo  = tambores_industria.id_articulo
          AND r_productos_calidad_envase.id_calidad   = tambores_industria.id_calidad
          AND r_productos_calidad_envase.id_envase    = tambores_industria.id_envase
        NO-LOCK.
      ASSIGN ttProductos.kilos_env   = r_productos_calidad_envase.kilos.
    END.

    FOR FIRST r_productos_calidad
        WHERE r_productos_calidad.id_articulo = tambores_industria.id_articulo
          AND r_productos_calidad.id_calidad  = tambores_industria.id_calidad
        NO-LOCK.
      ASSIGN ttProductos.rnpa        = r_productos_calidad.rnpa.
    END.
      
    


  END.
END.


RUN generateExcel.p (INPUT TABLE ttProductos,
                        INPUT " Productos Elaborados Industria",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).
