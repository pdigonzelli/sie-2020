/*-- VARIABLES DE EXCEL --*/

    define var chExcelAplication as com-handle.
    define var chWorkbook        as com-handle.
    define var chWorkSheet       as com-handle.
    define var chchart           as com-handle.
    define var chWorkSheetRange  as com-handle.
  
    define var ifila  as integer.
    define var cfila  as character.
    define var crange as character.

 /*-- FIN VARIABLES DE EXCEL --*/


/*-- CONFIGURACION INICIAL --*/

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
    chWorkSheet:Columns("C"):ColumnWidth = 7.
    chWorkSheet:Columns("D"):ColumnWidth = 10.
    chWorkSheet:Columns("E"):ColumnWidth = 15.
    chWorkSheet:Columns("F"):ColumnWidth = 25.
    chWorkSheet:Columns("G"):ColumnWidth = 25.
    chWorkSheet:Columns("H"):ColumnWidth = 25.
    chWorkSheet:Columns("I"):ColumnWidth = 5.
    chWorkSheet:Columns("J"):ColumnWidth = 25.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 5.
    chWorkSheet:Columns("M"):ColumnWidth = 10.
    chWorkSheet:Columns("N"):ColumnWidth = 10.
    chWorkSheet:Columns("O"):ColumnWidth = 10.
    chWorkSheet:Columns("P"):ColumnWidth = 10.
    chWorkSheet:Columns("Q"):ColumnWidth = 10.
    chWorkSheet:Columns("R"):ColumnWidth = 10.
    chWorkSheet:Columns("S"):ColumnWidth = 10.
    chWorkSheet:Columns("T"):ColumnWidth = 10.
    chWorkSheet:Columns("U"):ColumnWidth = 10.
    chWorkSheet:Columns("V"):ColumnWidth = 10.
    chWorkSheet:Columns("W"):ColumnWidth = 10.
    chWorkSheet:Columns("X"):ColumnWidth = 10.
    chWorkSheet:Columns("Y"):ColumnWidth = 10.
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "REPORTE DE OE".
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  */
  
  chWorkSheet:Range("B6"):Value = "OE".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "SEMANA".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "CONDICION".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "FECHA EMBARQUE".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "VAPOR".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "DESTINO".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "CLIENTE".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CANTIDAD".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "MERCADERIA".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "CONTRATO".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "PARTE".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "CONDICION VENTA".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "CONDICION PAGO".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "FLETE".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "THC".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "TOLL".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "HANDLING".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S6"):Value = "IN LAND".
  chWorkSheet:Range("S6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("T6"):Value = "BUNKER".
  chWorkSheet:Range("T6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("U6"):Value = "GASTOS DESTINO".
  chWorkSheet:Range("U6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("V6"):Value = "GASTOS BL".
  chWorkSheet:Range("V6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("W6"):Value = "THC DESTINO".
  chWorkSheet:Range("W6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("X6"):Value = "VARIOS".
  chWorkSheet:Range("X6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Y6"):Value = "SEGURO".
  chWorkSheet:Range("Y6"):BorderAround(1,2,1,1).

  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 9.
  for each orden_entrega no-lock by year(orden_entrega.fecha)
                                 by semana_embarque.
                                              
    find items_contratos of orden_entrega no-lock no-error.
    find clientes of orden_entrega no-lock no-error.
    find calidades of orden_entrega no-lock no-error.
    find destinos where destinos.id_destino = orden_entrega.id_destino no-lock no-error.    
    find estados_oe of orden_entrega no-lock no-error.
    find vapores of orden_entrega no-lock no-error.                
    find clausulas where clausulas.id_clausula = orden_entrega.id_condicion_venta no-lock no-error.
    find productos_terminados of orden_entrega no-lock no-error.
    {..\industria\p_exporta_oe_1.i}
                        
  end.
                       
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
