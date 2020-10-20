  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR viFila  AS INTEGER.
  DEFINE VAR vcFila  AS CHARACTER.
  DEFINE VAR cRange AS CHARACTER.
  DEFINE VARIABLE i  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dBrix AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSol  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPeso AS DECIMAL    NO-UNDO.


  CREATE "Excel.Application" chExcelAplication.
  
  /*chExcelAplication:VISIBLE = TRUE.*/
  chWorkbook  = chExcelAplication:Workbooks:OPEN('e:\docsindustria\tabla.xls').
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).

  
  viFila = 1.

  DO i = 701 TO 900.
    cRange = "A" + STRING(i).
    dBrix  = DECIMAL(chWorkSheet:Range(cRange):VALUE).
    cRange = "B" + STRING(i).
    dPeso  = DECIMAL(chWorkSheet:Range(cRange):VALUE).
    cRange = "C" + STRING(i).
    dSol   = DECIMAL(chWorkSheet:Range(cRange):VALUE).

    /*DISP dbrix dsol.*/
    
    FIND FIRST brix WHERE brix.brix = dBrix NO-ERROR.
    IF AVAILABLE brix THEN DO:      
      ASSIGN brix.solido_soluble = dSol.
    END.
    ELSE DO:
      /*
      CREATE brix.
      ASSIGN brix.brix = dBrix
             brix.pe = dPeso
             brix.solido_soluble = dPeso.
             */
    END.
    

  END.
  
  /*
  /*Actualizar Dinamicas*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(2).
  chWorkSheet:PivotTables("reproc"):RefreshTable().
  */
  /*chExcelAplication:VISIBLE = TRUE.*/
  /***********Release Variables*****************/
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 
