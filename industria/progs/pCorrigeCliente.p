DEFINE VARIABLE chWorkBook           AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chWorkSheet          AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chExcelAplicattion   AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE iRow                 AS INTEGER    NO-UNDO.
DEFINE VARIABLE i                    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cKunnr               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRange               AS CHARACTER  NO-UNDO.

CREATE "Excel.Application" chExcelAplicattion.

chWorkbook  = chExcelAplicattion:Workbooks:OPEN('D:\facundoj\sap\progress2sap\clientes walter.xls').
chWorkSheet = chExcelAplicattion:Sheets:ITEM(1).

chExcelAplicattion:VISIBLE = TRUE.
iRow = 8.

DO i = 9 TO 200:
  cRange = "A" + STRING(i).
  cKunnr = ENTRY(1, chWorkSheet:Range(cRange):VALUE, ".").

  FOR FIRST BATCH_input_clientes_sap
      WHERE kunnr = cKunnr,
      FIRST clientes
      WHERE clientes.id_cliente = INTEGER(BATCH_input_clientes_sap.altkn).
    
    ASSIGN 
           clientes.domicilio      = chWorkSheet:Range("D" + STRING(i)):VALUE
           clientes.provincia      = chWorkSheet:Range("H" + STRING(i)):VALUE
           clientes.localidad      = chWorkSheet:Range("E" + STRING(i)):VALUE
           clientes.codigo_postal  = chWorkSheet:Range("F" + STRING(i)):VALUE
           clientes.citde          = chWorkSheet:Range("I" + STRING(i)):VALUE

           chWorkSheet:Range("M" + STRING(i)):VALUE = "X"
    .


  END.

END.

IF VALID-HANDLE(chExcelAplicattion) THEN RELEASE OBJECT chExcelAplicattion.
IF VALID-HANDLE(chWorkBook)         THEN RELEASE OBJECT chWorkBook.
IF VALID-HANDLE(chWorkSheet)        THEN RELEASE OBJECT chWorkSheet. 














