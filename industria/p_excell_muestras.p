/**************************************GENERADOR DE EXCELL **********************************/
  
      create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
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
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00". */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "PEDIDO DE MUESTRAS".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 22.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
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
  
  chWorkSheet:Range("B6"):Value = "MUESTRA".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "AÑO".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "FECHA PEDIDO".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "ENVIAR A".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "SOLICITADO POR".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "DIRECTO AL CLIENTE".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CARACTERISTICAS".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "ENVASE".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "CANTIDAD".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "PROTOCOLOS".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "LOTES".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "ENVIADO TUC".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "COURIER TUC".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "NRO GUIA TUC".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "ENVIADO BUE".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "COURIER BUE".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "NRO GUIA BUE".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH muestras WHERE muestras.fecha >= DATE("01/01/03")
                      AND muestras.fecha <= DATE("01/01/04")
                      BY muestras.orden_reporte.
    FIND FIRST productos_terminados OF muestras NO-LOCK NO-ERROR.
    FIND FIRST contactos_muestras WHERE contactos_muestras.

    FOR EACH items_muestras WHERE items_muestras.id_muestra = muestras.id_muestra
                              AND items_muestras.anio_muestra = muestras.anio_muestra
                            NO-LOCK.
    
        cfila  = string(ifila).
        cRange = "B" + cfila.
        chWorkSheet:Range(crange):value = muestras.id_muestra.
        cRange = "C" + cfila.
        chWorkSheet:Range(crange):value = muestras.anio_muestra.
        cRange = "D" + cfila.
        chWorkSheet:Range(crange):value = muestras.fecha.
        
        cRange = "E" + cfila.
        chWorkSheet:Range(crange):value = IF AVAILABLE productos_termiandos THEN productos_terminados.descripcion_ingles ELSE "NONE".
        cRange = "F" + cfila.
        chWorkSheet:Range(crange):value = muestras.envase.
        cRange = "G" + cfila.
        chWorkSheet:Range(crange):value = muestras.id_articulo.
        cRange = "H" + cfila.
        chWorkSheet:Range(crange):value = muestras.articulo.
        cRange = "I" + cfila.
        chWorkSheet:Range(crange):value = muestras.id_calidad.
        cRange = "J" + cfila.
        chWorkSheet:Range(crange):value = muestras.calidad.
        cRange = "K" + cfila.
        chWorkSheet:Range(crange):value = muestras.orden_fabricacion.
        cRange = "L" + cfila.
        chWorkSheet:Range(crange):value = muestras.id_contrato.
        cRange = "M" + cfila.
        chWorkSheet:Range(crange):value = muestras.id_cliente.
        cRange = "N" + cfila.
        chWorkSheet:Range(crange):value = muestras.cliente.
        cRange = "O" + cfila.
        chWorkSheet:Range(crange):value = muestras.tambores.
        cRange = "P" + cfila.
        chWorkSheet:Range(crange):value = muestras.kilos.
        cRange = "Q" + cfila.
        chWorkSheet:Range(crange):value = muestras.kilos_400.
        
        ifila = ifila + 1.
    END.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/
