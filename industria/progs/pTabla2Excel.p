/***************Define needed variables************/
DEFINE VARIABLE hExcel AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorkbook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hBufferHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
DEFINE VARIABLE hFieldHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE hQueryHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE iRowNum AS INTEGER NO-UNDO.
DEFINE VARIABLE ColumnRange AS CHARACTER NO-UNDO.

/********Open Excel and initialize variables********/
CREATE "Excel.Application" hExcel.

        hExcel:VISIBLE = TRUE.
        hWorkbook = hExcel:Workbooks:Add().
        hWorkSheet = hExcel:Sheets:Item(1).
        cTableName = "balanzas".
        iRowNum = 1.

/* Pump field names as EXCEL column headers*/
CREATE BUFFER hBufferHandle FOR TABLE cTableName.
DO iCounter = 1 TO hBufferHandle:NUM-FIELDS:
        hFieldHandle = hBufferHandle:BUFFER-FIELD(iCounter).
        hExcel:Worksheets("Hoja1"):Cells(iRowNum,iCounter ) = hFieldHandle:NAME.
END.

/* Create dynamic query for the cTablename */
CREATE QUERY hQueryHandle.
hQueryHandle:SET-BUFFERS(hBufferHandle).
hQueryHandle:QUERY-PREPARE("for each " + cTableName).
hQueryHandle:QUERY-OPEN.

/* Pump the table data into your worksheet */
REPEAT:
        iRowNum = iRowNum + 1.
        hQueryHandle:GET-NEXT().
        IF hQueryHandle:QUERY-OFF-END THEN LEAVE.
        ELSE DO iCounter = 1 TO hBufferHandle:NUM-FIELDS:
                hFieldHandle = hBufferHandle:BUFFER-FIELD(iCounter).
                hExcel:Worksheets("Hoja1"):Cells(iRowNum,iCounter ) =
                        hFieldHandle:BUFFER-VALUE.
        END.
END.


/* Define populated column range and auto-format width */
/* Hint: First Column is column "A" hence CHR(65) usage*/

ASSIGN
        ColumnRange = CHR(65) + ":" + CHR(65 + hBufferHandle:NUM-FIELDS - 1).

hExcel:COLUMNS(ColumnRange):SELECT.
hExcel:SELECTION:COLUMNS:AUTOFIT.

/* Perform housekeeping and cleanup steps */
hExcel:Application:Workbooks:CLOSE() NO-ERROR.
hExcel:Application:QUIT NO-ERROR.
RELEASE OBJECT hWorksheet.
RELEASE OBJECT hWorkbook.
RELEASE OBJECT hExcel.
DELETE OBJECT hBufferHandle.
DELETE OBJECT hQueryHandle.
