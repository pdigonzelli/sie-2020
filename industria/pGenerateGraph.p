
DEFINE VARIABLE chChartSpace AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chChart      AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSerie      AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSC         AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chConst      AS COM-HANDLE     NO-UNDO.

DEFINE VARIABLE vcTitle  AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE vcData   AS CHARACTER INITIAL "" NO-UNDO.

CREATE "OWC.Chart" chChartSpace.

chChart = chChartSpace:Charts:ADD(0).
/*chConst = chChart:Constants().*/
chSC    = chChart:SeriesCollection.
chSerie = chChart:SeriesCollection:ADD(0).

chChart:hasTitle        = TRUE.
chChart:TITLE:Caption   = "Chart de Prueba Facundo".
chChart:TITLE:FONT      = "tahoma".
chChart:TITLE:FONT:SIZE = 10.
chChart:TITLE:FONT:Bold = TRUE.

/*chChart:Border:COLOR = chConst:chColorNone.
chChart:Border:COLOR = chConst:chColor*/

vcTitle = "RED" + CHR(10) + "GREEN" + CHR(10) + "BLUE".
vcData  = "20" + CHR(10) + "61" + CHR(10) + "70".
  /*
ENTRY(1, vcTitle, "\t") = "RED".
ENTRY(2, vcTitle, "\t") = "GREEN".
ENTRY(3, vcTitle, "\t") = "BLUE".

ENTRY(1, vcData, "\t") = "20".
ENTRY(2, vcData, "\t") = "61".
ENTRY(3, vcData, "\t") = "70".
*/

/*
chSerie:setData(chConst:chDimCategories, INTEGER(chConst:chDataLiteral), vcTitle).
chSerie:setData(chConst:chDimCategories, INTEGER(chConst:chDataLiteral), vcData).
*/

chSerie:Caption = "Caption de Serie".
chSerie:setData (chChart:Constants:chDimCategories, chChart:Constants:chDataLiteral, vcTitle).

/*chSerie:setData (chChart:Constants:chDimValues    , INTEGER(chChart:Constants:chDataLiteral), vcData).*/

/*
chSerie:setData(0, 1, vcTitle).
chSerie:setData(1, 1, vcData).
  */
chChart:ExportPicture("D:\chart.gif", "gif", 200, 200).



