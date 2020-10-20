define input parameter p-archivo as character.

DEF VAR che AS COM-HANDLE NO-UNDO.
DEF VAR chb AS COM-HANDLE NO-UNDO.
DEF VAR chs AS COM-HANDLE NO-UNDO.
DEF VAR chQT AS  COM-HANDLE NO-UNDO.

create "Excel.Application" che.
che:VISIBLE = TRUE.
chb  = che:Workbooks:Add().

RUN OpenSheet(che,INPUT-OUTPUT chs,1).
chqt = chs:QueryTables:Add(p-archivo,chs:Cells(1,1)).
chqt:TextFileSemicolonDelimiter = true.
chqt:REFRESH().
RELEASE OBJECT chqt.

RUN OpenSheet(che,INPUT-OUTPUT chs,1).

RELEASE OBJECT chs.
RELEASE OBJECT chb.
RELEASE OBJECT che.

PROCEDURE OpenSheet:
DEF INPUT PARAM phExcel AS COM-HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM phExcelWorkSheet AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM piSheetNum AS INT.

DEF VAR iNumSheets AS INT NO-UNDO.

IF VALID-HANDLE(phExcelWorkSheet)
THEN RELEASE OBJECT phExcelWorkSheet.

iNumSheets = phExcel:sheets:COUNT.

IF iNumSheets >= piSheetNum
THEN phExcelWorkSheet = phExcel:sheets:ITEM(piSheetNum).
ELSE DO:
    phExcelWorkSheet = phExcel:sheets:ITEM(iNumSheets).
    phExcelWorkSheet:SELECT.
    phExcelWorkSheet = phExcel:Sheets:ADD.
END.

phExcelWorkSheet:SELECT.

END PROCEDURE.
