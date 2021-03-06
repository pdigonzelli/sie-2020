&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hLib  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hApp  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hAsy  AS HANDLE     NO-UNDO.
DEFINE VARIABLE lFlag AS LOGICAL    NO-UNDO.

define var alta as logical no-undo initial false.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fecha_desde fecha_hasta BUTTON-26 BUTTON-23 ~
sucursal articulo calidad chkLotes chkRepro chkCalidad BUTTON-4 BUTTON-5 ~
nombre_articulo nombre_calidad RECT-2 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS fecha_desde fecha_hasta sucursal articulo ~
calidad chkLotes chkRepro chkCalidad nombre_articulo nombre_calidad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCitral W-Win 
FUNCTION getCitral RETURNS INTEGER
  (INPUT piEmpresa AS INTEGER, 
   INPUT piSucursal AS INTEGER, 
   INPUT piTipoTambor AS INTEGER, 
   INPUT piNroMov AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-20 
     LABEL "Reporte Lotes Reprocesados Excell" 
     SIZE 36 BY 1.19.

DEFINE BUTTON BUTTON-21 
     LABEL "Rep Lotes Reprocesados 2 Excell" 
     SIZE 36 BY 1.19.

DEFINE BUTTON BUTTON-22 
     LABEL "Lotes(Real)" 
     SIZE 14 BY .95.

DEFINE BUTTON BUTTON-23 
     LABEL "Producciones(Real)" 
     SIZE 24 BY .95.

DEFINE BUTTON BUTTON-24 
     LABEL "Expor.Excell Prod. y Lotes Completo" 
     SIZE 37 BY 1.14.

DEFINE BUTTON BUTTON-25 
     LABEL "Lotes + Producciones" 
     SIZE 37 BY 1.14.

DEFINE BUTTON BUTTON-26 
     LABEL "Lotes" 
     SIZE 9.6 BY .95.

DEFINE BUTTON BUTTON-4 
     LABEL "Prod. Lotes Definitivos Excel" 
     SIZE 37 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "Exportacion Excell Producciones" 
     SIZE 36 BY 1.19.

DEFINE BUTTON BUTTON-6 
     LABEL "Reporte Produc-Reproceso Excell" 
     SIZE 36 BY 1.19.

DEFINE VARIABLE articulo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE calidad AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Calidad" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fecha_desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fecha_hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "hasta" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE nombre_articulo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 50 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE nombre_calidad AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 46 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE sucursal AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Planta" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 115 BY 50
     BGCOLOR 7 FGCOLOR 7 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 7.86.

DEFINE VARIABLE chkCalidad AS LOGICAL INITIAL no 
     LABEL "Reporte agrupado por Calidad" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE chkLotes AS LOGICAL INITIAL yes 
     LABEL "Incluye Lotes En Produccion" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE chkRepro AS LOGICAL INITIAL yes 
     LABEL "Incluye Reprocesos" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fecha_desde AT ROW 2.91 COL 10 COLON-ALIGNED
     fecha_hasta AT ROW 2.91 COL 35.2 COLON-ALIGNED
     BUTTON-26 AT ROW 2.91 COL 55.4
     BUTTON-22 AT ROW 2.91 COL 65.4
     BUTTON-23 AT ROW 3.86 COL 55.4
     sucursal AT ROW 4.1 COL 10 COLON-ALIGNED
     articulo AT ROW 5.29 COL 10 COLON-ALIGNED
     calidad AT ROW 6.48 COL 10 COLON-ALIGNED
     chkLotes AT ROW 7.43 COL 48
     chkRepro AT ROW 7.67 COL 12
     chkCalidad AT ROW 8.38 COL 48
     BUTTON-4 AT ROW 9.57 COL 3
     BUTTON-5 AT ROW 9.57 COL 45
     BUTTON-6 AT ROW 10.76 COL 45
     BUTTON-25 AT ROW 11.38 COL 3
     BUTTON-20 AT ROW 11.95 COL 45
     BUTTON-21 AT ROW 13.14 COL 45
     BUTTON-24 AT ROW 13.19 COL 3
     nombre_articulo AT ROW 5.29 COL 18 COLON-ALIGNED NO-LABEL
     nombre_calidad AT ROW 6.48 COL 22 COLON-ALIGNED NO-LABEL
     RECT-2 AT Y 285 X 285
     RECT-4 AT ROW 1.48 COL 1
     "Reporte de Producciones:" VIEW-AS TEXT
          SIZE 56 BY .71 AT ROW 1.71 COL 5
     "(95 Famailla - 96 Lavalle)" VIEW-AS TEXT
          SIZE 25 BY .95 AT ROW 4.1 COL 21
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.2 BY 15.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Reporte de Producciones"
         HEIGHT             = 16.05
         WIDTH              = 80.8
         MAX-HEIGHT         = 32.33
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 32.33
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "visibles" W-Win _INLINE
/* Actions: ? custom/support/cusvis.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" W-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR BUTTON BUTTON-20 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-20:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-21 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-21:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-22 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-24 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-25 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-6:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       chkLotes:HIDDEN IN FRAME F-Main           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Reporte de Producciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Reporte de Producciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  IF VALID-HANDLE(hApp) THEN DO:
    hApp:DISCONNECT().
    DELETE OBJECT hApp.
  END.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL articulo W-Win
ON LEAVE OF articulo IN FRAME F-Main /* Articulo */
DO:
  find productos_terminados where productos_terminados.id_articulo =
                                  integer(articulo:screen-value in frame F-Main)  no-lock no-error.
  if available productos_terminados then
    nombre_articulo:screen-value in frame F-Main = productos_terminados.descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL articulo W-Win
ON MOUSE-SELECT-DBLCLICK OF articulo IN FRAME F-Main /* Articulo */
DO:
  define var r as rowid.
  
  run wc_articulos.w (output r).
  find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
  if available productos_terminados then
    articulo:screen-value in frame F-Main = string(productos_terminados.id_articulo).
    nombre_articulo:screen-value in frame F-Main = productos_terminados.descripcion.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-20 W-Win
ON CHOOSE OF BUTTON-20 IN FRAME F-Main /* Reporte Lotes Reprocesados Excell */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  /*v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).*/
  
  run p_reprocesos.p (INPUT v_fecha_desde, 
                      INPUT v_fecha_hasta /* ,
                      INPUT v_sucursal,
                      INPUT v_articulo,
                      INPUT v_calidad*/ ).
  
  

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
    chWorkSheet:Columns("B"):ColumnWidth = 7.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 1.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 5.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 5.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    chWorkSheet:Columns("J"):ColumnWidth = 5.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 5.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 5.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".    */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "REPORTE DE LOTES REPROCESADOS".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 3.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 9.
        chWorkSheet:Range("B5"):Font:colorindex = 2.
    END.
  
  chWorkSheet:Range("B6"):Value = "FECHA".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "LOTE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "A�O".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "TAMBORES".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "ARTICULO".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "CALIDAD".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "ENVASE".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "KILOS".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "CONV. 400".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 6.
  FOR EACH lotes_reproceso BY lotes_reproceso.fecha
                           BY lotes_reproceso.id_lote.
                         
    ifila = ifila + 1.
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.fecha.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.anio_lote.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.tambores.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.id_articulo.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.articulo.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.id_calidad.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.calidad.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.id_envase.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.envase.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.kilos.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.kilos_400.
    
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-21 W-Win
ON CHOOSE OF BUTTON-21 IN FRAME F-Main /* Rep Lotes Reprocesados 2 Excell */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  /*v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).*/
  
  run p_reprocesos_viendo_lote_finales.p (INPUT v_fecha_desde, 
                      INPUT v_fecha_hasta /* ,
                      INPUT v_sucursal,
                      INPUT v_articulo,
                      INPUT v_calidad*/ ).
  
  

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
    chWorkSheet:Columns("B"):ColumnWidth = 7.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 1.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 5.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 5.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    chWorkSheet:Columns("J"):ColumnWidth = 5.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 5.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 5.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".    */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "REPORTE DE LOTES REPROCESADOS".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 3.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 9.
        chWorkSheet:Range("B5"):Font:colorindex = 2.
    END.
  
  chWorkSheet:Range("B6"):Value = "FECHA".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "LOTE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "A�O".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "TAMBORES".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "ARTICULO".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "CALIDAD".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "ENVASE".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "KILOS".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "CONV. 400".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 6.
  FOR EACH lotes_reproceso BY lotes_reproceso.fecha
                           BY lotes_reproceso.id_lote.
                         
    ifila = ifila + 1.
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.fecha.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.anio_lote.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.tambores.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.id_articulo.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.articulo.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.id_calidad.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.calidad.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.id_envase.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.envase.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.kilos.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = lotes_reproceso.kilos_400.
    
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-22 W-Win
ON CHOOSE OF BUTTON-22 IN FRAME F-Main /* Lotes(Real) */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

  SELF:SENSITIVE = NOT SELF:SENSITIVE.
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  /*
  run w_calculo_produccion_industria.p (input v_fecha_desde, 
                                        input v_fecha_hasta,
                                        input v_sucursal,
                                        input v_articulo,
                                        input v_calidad).
  */                                        

  PROCESS EVENTS.
  
  IF lFlag THEN DO:
    RUN callProduccionIndustria IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted"
                                        (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                         INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_desde:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main), 
                                         "Lotes", 
                                         LOGICAL(chkRepro:SCREEN-VALUE)).


    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callProduccionIndustria IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                         INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_desde:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main), 
                                         "Lotes",
                                         LOGICAL(chkRepro:SCREEN-VALUE)).

  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "produccion_industria.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "produccion_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "produccion_industria.id_articulo = " + string(v_articulo) +
                           " and produccion_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
           
          if v_sucursal <> 0 and (v_articulo <> 0 or v_calidad <> 0) then do:
            v_filtro = v_filtro + " and produccion_industria.id_sucursal = " + string(v_sucursal).
          end.
          
          if v_sucursal <> 0 and (v_articulo = 0 and v_calidad = 0) then do:
            v_filtro = "produccion_industria.id_sucursal = " + string(v_sucursal).
          end.
          
          if v_articulo = 52 or v_articulo = 53 then
            do:
                v_filtro = "(" + v_filtro + ") or produccion_industria.id_articulo = " 
                                          + string(v_articulo + 900).       
            end.
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "parte_produccion_rapido_cfecha",                    /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         v_filtro,                              /* RB-FILTER */
         RB-MEMO-FILE,                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         "?",                              /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
         1,                              /* RB-NUMBER-COPIES  - zero */                  
         0,                              /* RB-BEGIN-PAGE - zero */
         0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Reporte de Produccion",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/

  SELF:SENSITIVE = NOT SELF:SENSITIVE.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-23 W-Win
ON CHOOSE OF BUTTON-23 IN FRAME F-Main /* Producciones(Real) */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  SELF:SENSITIVE = NOT SELF:SENSITIVE.
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  /*
  run w_calculo_produccion_industria_prod_jugo.p (input v_fecha_desde, 
                                                  input v_fecha_hasta,
                                                  input v_sucursal,
                                                  input v_articulo,
                                                  input v_calidad).
  */

  PROCESS EVENTS.

  IF lFlag THEN DO:
    RUN callProduccionIndustria IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted"
                                        (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                         INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_desde:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main), 
                                         "Producciones",
                                         LOGICAL(chkRepro:SCREEN-VALUE),
                                         LOGICAL(chkLotes:SCREEN-VALUE)).

    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callProduccionIndustria IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                         INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_desde:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main), 
                                         "Producciones",
                                         LOGICAL(chkRepro:SCREEN-VALUE),
                                         LOGICAL(chkLotes:SCREEN-VALUE)).


  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 then
            do:
                v_filtro = "produccion_industria.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 then
            do:
                v_filtro = "".       
            end.
           
          if v_sucursal <> 0 and v_articulo <> 0 then do:
            v_filtro = v_filtro + " and produccion_industria.id_sucursal = " + string(v_sucursal).
          end.
          
          if v_sucursal <> 0 and v_articulo = 0 then do:
            v_filtro = "produccion_industria.id_sucursal = " + string(v_sucursal).
          end.
          
          
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "parte_produccion_rapido_cfecha",                    /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         v_filtro,                              /* RB-FILTER */
         RB-MEMO-FILE,                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         "?",                              /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
         1,                              /* RB-NUMBER-COPIES  - zero */                  
         0,                              /* RB-BEGIN-PAGE - zero */
         0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Reporte de Produccion",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/

  SELF:SENSITIVE = NOT SELF:SENSITIVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-24 W-Win
ON CHOOSE OF BUTTON-24 IN FRAME F-Main /* Expor.Excell Prod. y Lotes Completo */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_produccion_industria_prod_jugo.p (input v_fecha_desde, 
                                                  input v_fecha_hasta,
                                                  input v_sucursal,
                                                  input 0,
                                                  input 0).
  
  RUN w_calculo_lotesjugo_lotesaceitesoloultimos.p (input v_fecha_desde, 
                                                    input v_fecha_hasta,
                                                    input v_sucursal,
                                                    input 0,
                                                    input 0).

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
    chWorkSheet:Columns("B"):ColumnWidth = 7.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 1.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 5.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 5.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    chWorkSheet:Columns("J"):ColumnWidth = 5.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 5.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 5.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".    */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "PARTE DE PRODUCCION DE MATERIA PRIMA".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 33.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  
  chWorkSheet:Range("B6"):Value = "FECHA".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "LOTE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "A�O".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "TAMBORES".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "KGS".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "OF".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CONTRATO".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "ARTICULO".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "CALIDAD".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "ENVASE".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "KILOS".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "CONV. 400".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH produccion_industria BY produccion_industria.fecha
                                BY produccion_industria.id_lote.
                         /* BY produccion_industria.id_envase
                          BY produccion_industria.id_articulo
                          BY produccion_industria.id_calidad. */
    
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.fecha.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.anio_lote.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.tambores.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_tambor.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = produccion_general.orden_fabricacion.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_contrato.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_articulo.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.articulo.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_calidad.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.calidad.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_envase.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.envase.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_400.
    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-25 W-Win
ON CHOOSE OF BUTTON-25 IN FRAME F-Main /* Lotes + Producciones */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run pLotesMasProducciones.p (input v_fecha_desde, 
                                        input v_fecha_hasta,
                                        input v_sucursal,
                                        input v_articulo,
                                        input v_calidad).
  
  

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
    chWorkSheet:Columns("B"):ColumnWidth = 7.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 1.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 5.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 5.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    chWorkSheet:Columns("J"):ColumnWidth = 5.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 5.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 5.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".    */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "PARTE DE PRODUCCION".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 31.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  
  chWorkSheet:Range("A6"):Value = "SUC".
  chWorkSheet:Range("A6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B6"):Value = "FECHA".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "LOTE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "A�O".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "TAMBORES".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "KGS".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "OF".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CONTRATO".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "ARTICULO".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "CALIDAD".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "ENVASE".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "KILOS".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "CONV. 400".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH produccion_industria BY produccion_industria.fecha
                                BY produccion_industria.id_lote.
                         /* BY produccion_industria.id_envase
                          BY produccion_industria.id_articulo
                          BY produccion_industria.id_calidad. */
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = produccion_industria.id_sucursal
                                 NO-LOCK NO-ERROR.
    

    cfila  = string(ifila).
    cRange = "A" + cfila.
    chWorkSheet:Range(crange):value = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE STRING(produccion_industria.id_sucursal).
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.fecha.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.anio_lote.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.tambores.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_tambor.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = produccion_general.orden_fabricacion.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_contrato.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_articulo.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.articulo.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_calidad.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.calidad.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_envase.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.envase.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_400.
    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-26 W-Win
ON CHOOSE OF BUTTON-26 IN FRAME F-Main /* Lotes */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  DEFINE VARIABLE cReport AS CHARACTER  NO-UNDO.
  
  SELF:SENSITIVE = NOT SELF:SENSITIVE.

  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).

  /*
  run w_calculo_produccion_industria.p (input v_fecha_desde, 
                                        input v_fecha_hasta,
                                        input v_sucursal,
                                        input v_articulo,
                                        input v_calidad).
  */


  PROCESS EVENTS.

  IF lFlag THEN DO:  
    RUN callProduccionIndustria IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                        (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                         INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_desde:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main), 
                                         "Lotes", 
                                         LOGICAL(chkRepro:SCREEN-VALUE),
                                         LOGICAL(chkLotes:SCREEN-VALUE)).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callProduccionIndustria IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                         INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_desde:SCREEN-VALUE IN FRAME F-Main), 
                                         DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main), 
                                         "Lotes", 
                                         LOGICAL(chkRepro:SCREEN-VALUE), 
                                         LOGICAL(chkLotes:SCREEN-VALUE)).


   IF LOGICAL(chkCalidad:SCREEN-VALUE) THEN
     cReport = "parte_produccion_rapido_xcal".
   ELSE
     cReport = "parte_produccion_rapido".


  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "produccion_industria.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "produccion_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "produccion_industria.id_articulo = " + string(v_articulo) +
                           " and produccion_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
           
          if v_sucursal <> 0 and (v_articulo <> 0 or v_calidad <> 0) then do:
            v_filtro = v_filtro + " and produccion_industria.id_sucursal = " + string(v_sucursal).
          end.
          
          if v_sucursal <> 0 and (v_articulo = 0 and v_calidad = 0) then do:
            v_filtro = "produccion_industria.id_sucursal = " + string(v_sucursal).
          end.
          
          if v_articulo = 52 or v_articulo = 53 then
            do:
                v_filtro = "(" + v_filtro + ") or produccion_industria.id_articulo = " 
                                          + string(v_articulo + 900).       
            end.
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         cReport,                    /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         v_filtro,                              /* RB-FILTER */
         RB-MEMO-FILE,                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         "?",                              /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
         1,                              /* RB-NUMBER-COPIES  - zero */                  
         0,                              /* RB-BEGIN-PAGE - zero */
         0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Reporte de Produccion",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/

  SELF:SENSITIVE = NOT SELF:SENSITIVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Prod. Lotes Definitivos Excel */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

  DEFINE VARIABLE cCod AS CHARACTER  NO-UNDO.
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  /*
  run w_calculo_produccion_industria.p (input v_fecha_desde, 
                                        input v_fecha_hasta,
                                        input v_sucursal,
                                        input v_articulo,
                                        input v_calidad).
  */

  RUN callProduccionIndustria IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                       INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main), 
                                       DATE(fecha_desde:SCREEN-VALUE IN FRAME F-Main), 
                                       DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main), 
                                       "Lotes", 
                                       LOGICAL(chkRepro:SCREEN-VALUE), 
                                       LOGICAL(chkLotes:SCREEN-VALUE)).
  

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
    chWorkSheet:Columns("B"):ColumnWidth = 7.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 1.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 5.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 5.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    chWorkSheet:Columns("J"):ColumnWidth = 5.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 5.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 5.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    chWorkSheet:Columns("R"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".    */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "PARTE DE PRODUCCION".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 31.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  
  chWorkSheet:Range("A6"):Value = "SUC".
  chWorkSheet:Range("A6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B6"):Value = "FECHA".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "LOTE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "A�O".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "CODIGO LOTE".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "TAMBORES".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "KGS".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "OF".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "CONTRATO".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "ARTICULO".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "CALIDAD".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "ENVASE".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "KILOS".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "CONV. 400".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH produccion_industria BY produccion_industria.fecha
                                BY produccion_industria.id_lote.
                         /* BY produccion_industria.id_envase
                          BY produccion_industria.id_articulo
                          BY produccion_industria.id_calidad. */
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = produccion_industria.id_sucursal
                                 NO-LOCK NO-ERROR.

    FOR FIRST tambores_industria
        WHERE produccion_industria.nromov = tambores_industria.nromov
        NO-LOCK.
      IF tambores_industria.codigo_lote <> ? THEN
        cCod = tambores_industria.codigo_lote.
      ELSE 
        cCod = "".
    END.
    

    cfila  = string(ifila).
    cRange = "A" + cfila.
    chWorkSheet:Range(crange):value = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE STRING(produccion_industria.id_sucursal).
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.fecha.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.anio_lote.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = cCod.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.tambores.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_tambor.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = produccion_general.orden_fabricacion.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_contrato.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_articulo.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.articulo.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_calidad.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.calidad.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_envase.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.envase.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos.
    cRange = "R" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_400.
    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Exportacion Excell Producciones */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  DEFINE VARIABLE iCitral AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cCod AS CHARACTER  NO-UNDO.
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  /*
  run w_producciones_industria.p (input v_fecha_desde, 
                                        input v_fecha_hasta,
                                        input v_sucursal,
                                        input v_articulo,
                                        input v_calidad).
  */

  RUN callProduccionIndustria IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                       INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main), 
                                       DATE(fecha_desde:SCREEN-VALUE IN FRAME F-Main), 
                                       DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main), 
                                       "ProdExcel",
                                       LOGICAL(chkRepro:SCREEN-VALUE), 
                                       LOGICAL(chkLotes:SCREEN-VALUE)).

  

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
    chWorkSheet:Columns("B"):ColumnWidth = 7.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 1.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 5.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 5.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    chWorkSheet:Columns("J"):ColumnWidth = 5.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 5.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 5.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".    */
    chWorkSheet:Columns("R"):ColumnWidth = 15.
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "PARTE DE PRODUCCION DE MATERIA PRIMA".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 33.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  
  chWorkSheet:Range("B6"):Value = "FECHA".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "LOTE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "A�O".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "CODIGO".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "TAMBORES".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "KGS".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "OF".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CONTRATO".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "ARTICULO".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "CALIDAD".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "ENVASE".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "KILOS".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "CONV. 400".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "CITRAL".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH produccion_industria BY produccion_industria.fecha
                                BY produccion_industria.id_lote.
                         /* BY produccion_industria.id_envase
                          BY produccion_industria.id_articulo
                          BY produccion_industria.id_calidad. */
    
    
    FOR FIRST tambores_industria
        WHERE tambores_industria.nromov = produccion_industria.nromov
        NO-LOCK.
      IF tambores_industria.codigo_lote <> ? THEN
        cCod = tambores_industria.codigo_lote.
      ELSE 
        cCod = "".
    END.
    
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.fecha.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.anio_lote.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = cCod.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.tambores.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_tambor.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = produccion_general.orden_fabricacion.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_contrato.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_articulo.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.articulo.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_calidad.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.calidad.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_envase.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.envase.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_400.
    cRange = "R" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.citral.
    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 W-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Reporte Produc-Reproceso Excell */
DO:
  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_sucursal = integer(sucursal:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_producciones_industria_items.p (input v_fecha_desde, 
                                        input v_fecha_hasta,
                                        input v_sucursal,
                                        input v_articulo,
                                        input v_calidad).
  
  

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
    chWorkSheet:Columns("B"):ColumnWidth = 7.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 1.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 5.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 5.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    chWorkSheet:Columns("J"):ColumnWidth = 5.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 5.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 5.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".    */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "PARTE DE PRODUCCION DE MATERIA PRIMA".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 33.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  
  chWorkSheet:Range("B6"):Value = "FECHA".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "LOTE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "A�O".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "TAMBORES".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "KGS".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "OF".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CONTRATO".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "ARTICULO".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "CALIDAD".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "ENVASE".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "KILOS".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "CONV. 400".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 6.
  FOR EACH produccion_industria BY produccion_industria.fecha
                                BY produccion_industria.id_lote.
                         /* BY produccion_industria.id_envase
                          BY produccion_industria.id_articulo
                          BY produccion_industria.id_calidad. */
    
    ifila = ifila + 1.
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.fecha.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.anio_lote.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.tambores.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_tambor.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = produccion_general.orden_fabricacion.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_contrato.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_articulo.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.articulo.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_calidad.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.calidad.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_envase.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.envase.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_400.
    
    
    FOR EACH items_produccion_industria OF produccion_industria
                                        BREAK BY items_produccion_industria.id_tipotambor.
        
        IF FIRST-OF(items_produccion_industria.id_tipotambor) THEN DO:
            ifila = ifila + 1.
            cfila  = string(ifila).
            cRange = "B" + cfila.
            chWorkSheet:Range("B" + cfila + ":G" + cfila):MergeCells = True.
            chWorkSheet:Range(crange):Value = items_produccion_industria.tipotambor.
            chWorkSheet:Range(crange):BorderAround(1,2,1,1).
            chWorkSheet:Range(crange):interior:colorindex = 22.
            chWorkSheet:Range(crange):Font:colorindex = 2.
        END.
        ifila = ifila + 1.
        cfila  = string(ifila).
        cRange = "C" + cfila.
        chWorkSheet:Range(crange):value = items_produccion_industria.id_lote.
        cRange = "D" + cfila.
        chWorkSheet:Range(crange):value = "/".
        cRange = "E" + cfila.
        chWorkSheet:Range(crange):value = items_produccion_industria.anio_lote.
        cRange = "F" + cfila.
        chWorkSheet:Range(crange):value = items_produccion_industria.tambores.
        cRange = "G" + cfila.
        chWorkSheet:Range(crange):value = items_produccion_industria.kilos_tambor.
    END.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL calidad W-Win
ON LEAVE OF calidad IN FRAME F-Main /* Calidad */
DO:
  find calidades where calidades.id_calidad =
                       integer(calidad:screen-value in frame F-Main)  no-lock no-error.
  if available calidades then
    nombre_calidad:screen-value in frame F-Main = calidades.descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL calidad W-Win
ON MOUSE-SELECT-DBLCLICK OF calidad IN FRAME F-Main /* Calidad */
DO:
  define var r as rowid.
  
  run wc_calidades.w (output r).
  find calidades where rowid(calidades) = r no-lock no-error.
  if available calidades then
    calidad:screen-value in frame F-Main = string(calidades.id_calidad).
    nombre_calidad:screen-value in frame F-Main = calidades.descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/method/winitialize.i}
{custom/method/ctitulo.i}
run deshabilita_viewer.
run habilitar_relacion_viewer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 14.81 , 59.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             BUTTON-24:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros W-Win 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callCompleted W-Win 
PROCEDURE callCompleted :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "F10" TO TARGET-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectApp W-Win 
PROCEDURE connectApp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.

  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  hApp = DYNAMIC-FUNCTION('connectApp' IN hLibCom).
 
  IF VALID-HANDLE(hApp) THEN 
    RUN libReportes.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
  ELSE 
    RUN libReportes.p PERSISTENT SET hLib .

  lFlag = VALID-HANDLE(hApp).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consulta W-Win 
PROCEDURE consulta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_consultas as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_consultas = "" then
    message "No hay consultas disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_consultas,output cresult).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer W-Win 
PROCEDURE deshabilita_viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fecha_desde fecha_hasta sucursal articulo calidad chkLotes chkRepro 
          chkCalidad nombre_articulo nombre_calidad 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fecha_desde fecha_hasta BUTTON-26 BUTTON-23 sucursal articulo calidad 
         chkLotes chkRepro chkCalidad BUTTON-4 BUTTON-5 nombre_articulo 
         nombre_calidad RECT-2 RECT-4 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-deshabilitados-paleta W-Win 
PROCEDURE get-deshabilitados-paleta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter hprograma as handle no-undo.
define output parameter estados as character no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresion W-Win 
PROCEDURE impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_reportes,output cresult).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  RUN connectApp.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy W-Win 
PROCEDURE post-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create W-Win 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete W-Win 
PROCEDURE post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update W-Win 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy W-Win 
PROCEDURE pre-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
alta = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create W-Win 
PROCEDURE pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
alta = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete W-Win 
PROCEDURE pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update W-Win 
PROCEDURE pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reporte_despacho W-Win 
PROCEDURE reporte_despacho :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var v_fecha_desde as date.
define var v_fecha_hasta as date.
define var v_articulo as integer.
define var v_calidad as integer.
define var v_filtro as char.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".



v_articulo = integer(articulo:screen-value in frame F-Main).
v_calidad = integer(calidad:screen-value in frame F-Main).

if fecha_desde:screen-value in frame F-Main <> "" and 
   fecha_hasta:screen-value in frame F-Main = "" then
    do:
        v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
        v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
        v_filtro = "remitos.fecha = date('" + string(v_fecha_desde) + "')".
    end.

if fecha_desde:screen-value in frame F-Main <> "" and 
   fecha_hasta:screen-value in frame F-Main <> "" then
    do:
        v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
        v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
        v_filtro = "remitos.fecha >= date('" + string(v_fecha_desde) + "') and remitos.fecha <= date('" + string(v_fecha_hasta) + "')".
    end.
               
if v_articulo > 0 then
    v_filtro = v_filtro + " and tambores_industria.id_articulo = " + string(v_articulo).
    
if v_calidad > 0 then
    v_filtro = v_filtro + " and tambores_industria.id_calidad = " + string(v_calidad).    
             
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_despachos",                    /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         v_filtro,                              /* RB-FILTER */
         RB-MEMO-FILE,                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         "?",                              /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
         1,                              /* RB-NUMBER-COPIES  - zero */                  
         0,                              /* RB-BEGIN-PAGE - zero */
         0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Reporte de Despachos",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
      
  /************************************************************************************************************/
  





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetea-registro W-Win 
PROCEDURE resetea-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCitral W-Win 
FUNCTION getCitral RETURNS INTEGER
  (INPUT piEmpresa AS INTEGER, 
   INPUT piSucursal AS INTEGER, 
   INPUT piTipoTambor AS INTEGER, 
   INPUT piNroMov AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCitral AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa = piEmpresa
                                AND tambores_industria.id_sucursal = piSucursal
                                AND tambores_industria.id_tipotambor = piTipoTambor
                                AND tambores_industria.nromov = piNroMov
                              NO-LOCK.
    iCitral = iCitral + tambores_industria.citral.
  END.

  RETURN iCitral.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

