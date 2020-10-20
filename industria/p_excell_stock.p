/**************************************GENERADOR DE EXCELL **********************************/
  
      create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AR6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AR1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AR6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 5.
    chWorkSheet:Columns("B"):ColumnWidth = 5.
    chWorkSheet:Columns("C"):ColumnWidth = 1.
    chWorkSheet:Columns("D"):ColumnWidth = 5.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 15.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 15.
    chWorkSheet:Columns("I"):ColumnWidth = 5.
    chWorkSheet:Columns("J"):ColumnWidth = 15.
    chWorkSheet:Columns("K"):ColumnWidth = 5.
    chWorkSheet:Columns("L"):ColumnWidth = 15.
    chWorkSheet:Columns("M"):ColumnWidth = 5.
    chWorkSheet:Columns("N"):ColumnWidth = 25.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("R"):ColumnWidth = 15.

    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:R3"):MergeCells = True.
  chWorkSheet:Range("B5:R5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "REPORTE DE STOCK".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 22.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  /*
  chWorkSheet:Range("B5"):Value = "LOTE".
  chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B5"):interior:colorindex = 19.
  chWorkSheet:Range("B5"):Font:colorindex = 1.
  chWorkSheet:Range("C5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C5"):Font:colorindex = 2.
  chWorkSheet:Range("D5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D5"):Font:colorindex = 2.
  chWorkSheet:Range("E5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E5"):Font:colorindex = 2.
  chWorkSheet:Range("F5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F5"):Font:colorindex = 2.
  chWorkSheet:Range("G5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G5"):Font:colorindex = 2.
  chWorkSheet:Range("H5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H5"):Font:colorindex = 2.
  chWorkSheet:Range("I5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I5"):Font:colorindex = 1.
  chWorkSheet:Range("J5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J5"):Font:colorindex = 2.
  chWorkSheet:Range("K5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K5"):Font:colorindex = 2.
  chWorkSheet:Range("L5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L5"):Font:colorindex = 2.
  chWorkSheet:Range("M5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M5"):Font:colorindex = 1.
  chWorkSheet:Range("N5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N5"):interior:colorindex = 22.
  chWorkSheet:Range("N5"):Font:colorindex = 1.
  chWorkSheet:Range("N5"):Value = "PACKING LIST".
  chWorkSheet:Range("O5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O5"):Font:colorindex = 2.
  chWorkSheet:Range("P5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P5"):Font:colorindex = 2.
  chWorkSheet:Range("Q5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q5"):Font:colorindex = 2.
  chWorkSheet:Range("R5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R5"):Font:colorindex = 2.
  chWorkSheet:Range("S5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S5"):Font:colorindex = 2.
    */
  
  chWorkSheet:Range("B6"):Value = "LOTE".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "AÑO".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "ENVASE".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "ARTICULO".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "CALIDAD".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "OF".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "CONTRATO".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "CLIENTE".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "TAMBORES".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "KILOS".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "CONV. 400".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "ESTADO".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.

  FOR EACH stock_tambores WHERE (IF v_articulo <> 0 THEN stock_tambores.id_articulo = v_articulo ELSE TRUE) 
                          BY stock_tambores.orden_reporte
                          BY stock_tambores.id_lote.
                         /* BY stock_tambores.id_envase
                          BY stock_tambores.id_articulo
                          BY stock_tambores.id_calidad. */
    
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_lote.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.anio_lote.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_envase.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.envase.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_articulo.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.articulo.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_calidad.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.calidad.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.orden_fabricacion.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_contrato.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_cliente.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.cliente.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.tambores.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.kilos.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.kilos_400.
    cRange = "R" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.estado.
    
    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/
