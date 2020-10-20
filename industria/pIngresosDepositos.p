DEFINE TEMP-TABLE ttIngresos
  RCODE-INFORMATION
  FIELD id_sucursal_ubicacion AS INTEGER COLUMN-LABEL "cod"
  FIELD deposito AS CHARACTER COLUMN-LABEL "deposito"
  FIELD id_lote AS INTEGER COLUMN-LABEL "lote"
  FIELD anio AS INTEGER COLUMN-LABEL "anio"
  FIELD tbs AS INTEGER COLUMN-LABEL "tambores"
  FIELD kgs AS DECIMAL COLUMN-LABEL "kilos"
  FIELD producto AS CHARACTER COLUMN-LABEL "producto"
  FIELD pais  AS CHARACTER COLUMN-LABEL "pais"
  FIELD fecha AS DATE COLUMN-LABEL "fecha ingreso"
  .

FOR EACH ttingresos.
  DELETE ttingresos.
END.

FOR EACH ingreso_lote_ubicacion
    WHERE ingreso_lote_ubicacion.fecha >= DATE("01/01/2005").

  FOR EACH item_ingreso_lote_ubicacion
      WHERE ingreso_lote_ubicacion.id_sucursal_ubicacion = ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion
        AND ingreso_lote_ubicacion.nromov_ingreso = ITEM_ingreso_lote_ubicacion.nromov_ingreso
        AND fecha >= DATE("01/01/2005").
    FIND FIRST sucursales WHERE sucursales.id_sucursal = ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion NO-LOCK NO-ERROR.
    FIND FIRST tambores_industria WHERE ITEM_ingreso_lote_ubicacion.nromov = tambores_industria.nromov NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.

    CREATE ttIngresos.
    ASSIGN ttingresos.id_sucursal_ubicacion = ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion
      ttIngresos.id_lote = tambores_industria.id_lote
      ttingresos.anio = tambores_industria.anio
      ttingresos.deposito = IF AVAILABLE sucursales THEN sucursales.nombre ELSE "NONE"
      ttingresos.producto = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
      ttingresos.pais = IF AVAILABLE sucursales THEN sucursales.pais ELSE "NONE"
      ttingresos.tbs = ITEM_ingreso_lote_ubicacion.cantidad
      ttingresos.fecha = ingreso_lote_ubicacion.fecha 
        .

  END.
END.


  RUN generateExcel.p (TABLE ttIngresos,
                       " Ingresos Depsitos ",
                       " ",
                       7,
                       8,
                       "Arial",
                       8).
