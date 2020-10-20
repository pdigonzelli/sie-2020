 
 /*-- CONFIGURACION INICIAL --*/
 create "Excel.Application" chExcelAplication.
 chExcelAplication:visible = true.
 chWorkbook  = chExcelAplication:Workbooks:add().
 chWorkSheet = chExcelAplication:Sheets:Item(1).

 chWorkSheet:Range("A1:AP10"):Font:Bold = true.
 chWorkSheet:Range("A1:AP1900"):Font:size= 8.
 chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
