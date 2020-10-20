

/*------------------------------------------------------------------------------
  Purpose:     reimpresion remitos juicio
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCoe AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKgs AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLiR AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLiC AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTip AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGpl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGp2 AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO INITIAL "".


  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR iFila  AS INTEGER.
  DEFINE VAR iCol   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPrt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNroCopia AS INTEGER    NO-UNDO.
  DEFINE VAR cFila  AS CHARACTER.
  DEFINE VAR cRange AS CHARACTER.

  DEFINE VARIABLE iLast AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRem AS CHARACTER  NO-UNDO.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\remitosparareimprimirjuicio.xls'). 
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).

 

  RUN libRemitos.p PERSISTENT SET hLib.

  iFila = 2.
  iCol  = 1.
  iLast = 90.

  DO iFila = 2 TO iLast:
    cRange = "A" + STRING(iFila).
    cRem   = chWorkSheet:Range(cRange):VALUE.
    cRem   = STRING(cRem, "99-99999999") .
    cRem   = REPLACE(cRem, ".", "").

    cRem   = "00" + ENTRY(1, cRem, "-") + ENTRY(2, cRem, "-").

    cRange = "D" + STRING(iFila).
    IF chWorkSheet:Range(cRange):VALUE = "impreso" THEN NEXT.

    FOR FIRST remitos
        WHERE remitos.id_operacion = 311 
          AND remitos.nro_comp MATCHES cRem.
    
        iPrt = remitos.impresion.
        remitos.impresion = 0.
        iNroCopia = 2.

       RUN fillReportTable IN hLib (remitos.id_sucursal, remitos.id_tipo_movsto, remitos.nro).

      RUN  aderb\_prntrb2(
                      "..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                       "rptRemitoIndustria",                    /* RB-REPORT-NAME */
                       "",                             /* RB-DB-CONNECTION */
                       "O",                             /* RB-INCLUDE-RECORDS */
                       "",                              /* RB-FILTER */
                       RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                       "",                             /* RB-PRINT-DESTINATION */
                       SESSION:PRINTER-NAME,                              /* RB-PRINTER-NAME */
                       "",                              /* RB-PRINTER-PORT */
                       "",                              /* RB-OUTPUT-FILE */
                       iNroCopia,                   /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                       no,                              /* RB-TEST-PATTERN */
                       "remito",         /* RB-WINDOW-TITLE */
                       yes,                           /* RB-DISPLAY-ERRORS */
                       yes,                           /* RB-DISPLAY-STATUS */
                       NO,                              /* RB-NO-WAIT */
                       "" /* RB-OTHER-PARAMETERS */,
                       ""
                     ).

      /*  
      DISP remitos.nro remitos.nro_comp remitos.fecha cRem FORMAT "x(50)".*/
      cRange = "D" + STRING(iFila).
      chWorkSheet:Range(cRange):VALUE = "impreso".
      remitos.impresion = iPrt.

    END.
    
    

  END.


  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 




