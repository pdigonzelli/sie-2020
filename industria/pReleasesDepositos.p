DEFINE TEMP-TABLE ttReleases
  RCODE-INFORMATION
  FIELD id_sucursal_ubicacion AS INTEGER COLUMN-LABEL "cod"
  FIELD deposito AS CHARACTER COLUMN-LABEL "deposito"
  FIELD id_lote AS INTEGER COLUMN-LABEL "lote"
  FIELD anio AS INTEGER COLUMN-LABEL "anio"
  FIELD rel AS INTEGER COLUMN-LABEL "Nro Release"
  FIELD tbs AS INTEGER COLUMN-LABEL "tambores"
  FIELD kgs AS DECIMAL COLUMN-LABEL "kilos"
  FIELD producto AS CHARACTER COLUMN-LABEL "producto"
  FIELD pais  AS CHARACTER COLUMN-LABEL "pais"
  FIELD fecha AS DATE COLUMN-LABEL "fecha release"
  FIELD fecha_confirmacion AS DATE COLUMN-LABEL "fecha confirmacion"

  .

FOR EACH ttReleases.
  DELETE ttReleases.
END.

FOR EACH RELEASE_delivery
    WHERE RELEASE_delivery.fecha_creacion >= DATE("01/01/2005").

  FOR EACH items_release_delivery 
        OF RELEASE_delivery
      NO-LOCK.

    FIND FIRST sucursales WHERE sucursales.id_sucursal = ITEMs_release_delivery.id_sucursal_ubicacion NO-LOCK NO-ERROR.
    FIND FIRST tambores_industria WHERE ITEMs_release_delivery.nromov = tambores_industria.nromov NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.

    CREATE ttReleases.
    ASSIGN ttReleases.id_sucursal_ubicacion = ITEMs_release_delivery.id_sucursal_ubicacion
      ttReleases.id_lote = tambores_industria.id_lote
      ttReleases.anio = tambores_industria.anio
      ttReleases.deposito = IF AVAILABLE sucursales THEN sucursales.nombre ELSE "NONE"
      ttReleases.producto = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
      ttReleases.pais = IF AVAILABLE sucursales THEN sucursales.pais ELSE "NONE"
      ttReleases.tbs = ITEMs_release_delivery.tambores
      ttReleases.kgs = items_release_delivery.kilos_brutos
      ttReleases.fecha = RELEASE_delivery.fecha_creacion 
      ttReleases.fecha_confirmacion = RELEASE_delivery.fecha_confirmacion
      ttReleases.rel = RELEASE_delivery.numero_release
        .

  END.
END.


  RUN generateExcel.p (TABLE ttReleases,
                       " Releases Depsitos ",
                       " ",
                       7,
                       8,
                       "Arial",
                       8).
