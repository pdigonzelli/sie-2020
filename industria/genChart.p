/*                                            /jeff

xl-gen-chart.p
==============
*/

&scoped-define xlColumnClustered       51
&scoped-define xlPie                    5
&scoped-define xlLineStacked           63
&scoped-define xlColumns                2
&scoped-define xlLocationAsNewSheet     1
&scoped-define xlCategory               1
&scoped-define xlPrimary                1
&scoped-define xlValue                  2
&scoped-define xlPrimary                1
&scoped-define xlExcel9795             43
&scoped-define xlExcel5                39
&scoped-define xlWorkbookNormal     -4143
&scoped-define xlWBATWorksheet      -4167
&scoped-define xlColorIndexNone     -4142

  DEFINE INPUT  PARAMETER chFile  AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER chTitle AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER chType  AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER chData  AS HANDLE    NO-UNDO.
  DEFINE INPUT  PARAMETER chPrint AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER chError AS CHARACTER NO-UNDO.

  DEFINE VARIABLE xlRow              AS INTEGER    NO-UNDO.
  DEFINE VARIABLE xlCol              AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hbuf               AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hfield             AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hquery             AS HANDLE     NO-UNDO.
  DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE chChart            AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE xl-rng             AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE chKind             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE chFirst            AS LOGICAL    NO-UNDO.

  chError = "".
  CASE chType:
    WHEN "BAR"  THEN chKind = {&xlColumnClustered}.
    WHEN "PIE"  THEN chKind = {&xlPie}.
    WHEN "LINE" THEN chKind = {&xlLineStacked}.
    WHEN ?      THEN
    DO:
      chError = "Chart type not specified.".
      RETURN.
    END.
    OTHERWISE
    DO:
      chError = "Unknown chart type: " + chType + ".".
      RETURN.
    END.
  END CASE.

  hbuf   = chData:DEFAULT-BUFFER-HANDLE.

  CREATE QUERY hquery.
  hquery:SET-BUFFERS(hbuf:HANDLE).
  hquery:QUERY-PREPARE("FOR EACH " + chData:NAME).
  hquery:QUERY-OPEN.
  chFirst = TRUE.

  xlRow = 1.
  REPEAT WITH FRAME y:
    hquery:GET-NEXT().
    IF hquery:QUERY-OFF-END THEN LEAVE.
    IF chFirst THEN
    DO:
      CREATE "Excel.Application" chExcelApplication.
      ASSIGN chExcelApplication:Visible = true.
      chExcelApplication:Workbooks:Add({&xlWBATWorksheet}).
      ASSIGN chWorkbook  = chExcelApplication:WorkBooks:Item(1)
             chWorkSheet = chExcelApplication:Sheets:Item(1).
      ASSIGN chWorkSheet:Name = "Chart Data".
      chFirst = FALSE.
    END.
    DO xlCol = 1 TO hbuf:NUM-FIELDS:
      hfield = hbuf:BUFFER-FIELD(xlCol).
      xl-rng = CHR(64 + INTEGER(xlCol / 26))
             + CHR(64 + xlCol - (26 * INTEGER(xlCol / 26)))
             + STRING(xlRow).
      xl-rng = REPLACE(xl-rng, "@", "").
      chWorksheet:Range(xl-rng):Value = hfield:BUFFER-VALUE.
    END.
    xlRow = xlRow + 1.
  END.
  xlRow = xlRow - 1.
  xlCol = hbuf:NUM-FIELDS.

  hquery:QUERY-CLOSE().
  DELETE OBJECT hquery.

  IF chFirst = TRUE THEN
  DO:
    chError = "No data to chart.".
    RETURN.
  END.

  xl-rng = "A1:"
         + CHR(64 + INTEGER(xlCol / 26))
         + CHR(64 + xlCol - (26 * INTEGER(xlCol / 26)))
         + STRING(xlRow).
  xl-rng = REPLACE(xl-rng, "@", "").

  chWorkSheet:ChartObjects:Add(0, 0, 0, 0). /* Left, Top, Width, Height */
  ASSIGN chChart = chWorkSheet:ChartObjects:Item(1):Chart.
  ASSIGN chChart:ChartType = chKind.
  chChart:SetSourceData(chWorksheet:Range(xl-rng), {&xlColumns}).
  chChart:Location({&xlLocationAsNewSheet}, chTitle).
  ASSIGN chChart:HasTitle = False.
  /*
  chChart:PlotArea:Interior:ColorIndex = 4.
  ASSIGN chChart:HasTitle = True.
  ASSIGN chChart:ChartTitle:Text = /* chTitle + */ "First Quarter Sales".
  */
  ASSIGN chChart:Axes({&xlCategory}, {&xlPrimary}):HasTitle = False.
  ASSIGN chChart:Axes({&xlValue},    {&xlPrimary}):HasTitle = False.

  IF chFile <> "" THEN
  DO:
    IF chExcelapplication:Version BEGINS "8":U
       THEN chWorkBook:SaveAs(chFile, {&xlExcel9795},,,,,,).
       ELSE chWorkBook:SaveAs(chFile, {&xlWorkbookNormal},,,,,,).
  END.
  IF chPrint = TRUE THEN chWorkBook:PrintOut(,,,,,,).

  chWorkBook:Close().

  RELEASE OBJECT chChart      NO-ERROR.
  RELEASE OBJECT chWorkSheet  NO-ERROR.
  RELEASE OBJECT chWorkBook   NO-ERROR.
  chExcelApplication:Quit().
  RELEASE OBJECT chExcelApplication NO-ERROR.
