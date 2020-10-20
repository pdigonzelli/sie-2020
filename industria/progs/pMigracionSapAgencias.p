

DEFINE TEMP-TABLE ttAgencias
  RCODE-INFORMATION
  FIELD sector      AS CHARACTER COLUMN-LABEL "Sector"
  FIELD id_agencia  AS INTEGER COLUMN-LABEL "Cod Agencia"
  FIELD agencia     AS CHARACTER COLUMN-LABEL "Agencia"
  FIELD fecha_desde AS DATE COLUMN-LABEL "Vigencia Desde"
  FIELD fecha_hasta AS DATE COLUMN-LABEL "Vigencia Hasta"
  FIELD id_gasto    AS INTEGER  COLUMN-LABEL "Cod Gasto"
  FIELD gasto       AS CHARACTER COLUMN-LABEL "Gasto"
  FIELD importe     AS CHARACTER  COLUMN-LABEL "Importe"
  .

FOR EACH lista_gastos_agencia NO-LOCK,
    EACH items_lista_gastos_agencia OF lista_gastos_agencia NO-LOCK,
    FIRST agencias OF lista_gastos_agencia NO-LOCK, 
    FIRST gastos_venta OF items_lista_gastos_agencia NO-LOCK.

  CREATE ttAgencias.
  ASSIGN  ttAgencias.sector       = IF lista_gastos_agencia.id_tipo_lista = 1 THEN "INDUSTRIA" ELSE "FRUTA FRESCA"
          ttAgencias.id_agencia   = agencias.id_agencia
          ttAgencias.agencia      = agencias.descripcion
          ttAgencias.fecha_desde  = lista_gastos_agencia.fecha_desde
          ttAgencias.fecha_hasta  = lista_gastos_agencia.fecha_hasta
          ttAgencias.id_gasto     = gastos_venta.id_gasto
          ttAgencias.gasto        = gastos_venta.descripcion
          ttAgencias.importe      = STRING(items_lista_gastos_agencia.importe, "9999999.99")
          .

END.



RUN generateExcel.p (INPUT TABLE ttAgencias,
                        INPUT " Tarifas Agencias",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).
