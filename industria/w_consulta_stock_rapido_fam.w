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
define var alta as logical no-undo initial false.
DEFINE VAR hpp AS HANDLE.
DEFINE VAR flag AS LOGICAL.
DEFINE VARIABLE hLib  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hApp  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hAsy  AS HANDLE     NO-UNDO.
DEFINE VARIABLE lFlag AS LOGICAL    NO-UNDO.

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

/*TEMP TABLE PARA REPORTE DE CASCARA POR RANGOS*/

DEFINE TEMP-TABLE tt_rpt_cascara
    /* RCODE-INFORMATION */
    FIELD id_sucursal_ubicacion AS INTEGER    COLUMN-LABEL "id Sucursal"
    FIELD sucursal              AS CHARACTER  COLUMN-LABEL "Sucursal"
    FIELD id_tambor_desde       AS INTEGER    COLUMN-LABEL "Bolsa Desde"
    FIELD id_tambor_hasta       AS INTEGER    COLUMN-LABEL "Bolsa Hasta"
    FIELD cantidad              AS INTEGER    COLUMN-LABEL "Cantidad en Lotes"
    FIELD id_lote               AS INTEGER    COLUMN-LABEL "Lote"
    FIELD cantidad_dispo        AS INTEGER    COLUMN-LABEL "Cantidad Disponible".

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
&Scoped-Define ENABLED-OBJECTS BUTTON-47 BUTTON-57 BUTTON-48 BUTTON-42 ~
BUTTON-43 BUTTON-44 sucursal articulo calidad chkLotes BUTTON-39 BUTTON-52 ~
FI-anio BUTTON-25 BUTTON-29 BUTTON-45 semana BUTTON-14 BUTTON-49 BUTTON-26 ~
BUTTON-18 BUTTON-28 BUTTON-19 BUTTON-55 BUTTON-35 BUTTON-53 BUTTON-56 ~
BUTTON-50 BUTTON-51 BUTTON-54 BUTTON-40 BUTTON-27 BUTTON-37 nombre_articulo ~
nombre_calidad RECT-10 RECT-11 RECT-12 RECT-13 RECT-2 RECT-28 RECT-29 ~
RECT-36 RECT-37 RECT-4 RECT-6 RECT-7 RECT-8 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS sucursal articulo calidad chkLotes ~
FILL-IN-10 FILL-IN-11 FI-anio semana FILL-IN-7 FILL-IN-15 FILL-IN-12 ~
FILL-IN-8 FILL-IN-9 FILL-IN-18 FILL-IN-13 FILL-IN-16 FILL-IN-14 FILL-IN-17 ~
nombre_articulo nombre_calidad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-14 
     LABEL "Stock" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-18 
     LABEL "Stock" 
     SIZE 25 BY 1.14.

DEFINE BUTTON BUTTON-19 
     LABEL "Reporte" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-21 
     LABEL "Listado" 
     SIZE 9 BY 1.14.

DEFINE BUTTON BUTTON-23 
     LABEL "Stock x OF" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-25 
     LABEL "Reporte" 
     SIZE 13.4 BY 1.14.

DEFINE BUTTON BUTTON-26 
     LABEL "Reporte" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-27 
     LABEL "Stock x OF" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-28 
     LABEL "Stock Detallado" 
     SIZE 25 BY 1.14.

DEFINE BUTTON BUTTON-29 
     LABEL "Reporte Marily" 
     SIZE 20 BY 1.14.

DEFINE BUTTON BUTTON-35 
     LABEL "Stock" 
     SIZE 11 BY 1.14.

DEFINE BUTTON BUTTON-37 
     LABEL "Listado x Lote" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-38 
     LABEL "Stock x OF" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-39 
     LABEL "Lavalle" 
     SIZE 13.4 BY 1.14.

DEFINE BUTTON BUTTON-40 
     LABEL "Listado x Calidad" 
     SIZE 18 BY 1.14.

DEFINE BUTTON BUTTON-42  NO-FOCUS
     LABEL "List x Lote" 
     SIZE 14 BY 1.14.

DEFINE BUTTON BUTTON-43  NO-FOCUS
     LABEL "List x Calidad" 
     SIZE 14 BY 1.14.

DEFINE BUTTON BUTTON-44  NO-FOCUS
     LABEL "Stock x OF" 
     SIZE 14 BY 1.14.

DEFINE BUTTON BUTTON-45 
     LABEL "Stock a Excel" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-47  NO-FOCUS
     LABEL "List Dep Ext" 
     SIZE 14 BY 1.14.

DEFINE BUTTON BUTTON-48  NO-FOCUS
     LABEL "List x Estado" 
     SIZE 14 BY 1.14.

DEFINE BUTTON BUTTON-49 
     LABEL "Remitos" 
     SIZE 28 BY 1.14.

DEFINE BUTTON BUTTON-50 
     LABEL "Exportar XLS" 
     SIZE 19 BY 1.14.

DEFINE BUTTON BUTTON-51 
     LABEL "Exportar XLS" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-52 
     LABEL "Stock p/mail" 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-53 
     LABEL "Stock XLS" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-54 
     LABEL "Exportar XLS" 
     SIZE 25 BY 1.14.

DEFINE BUTTON BUTTON-55 
     LABEL "Exportar XLS" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-56 
     LABEL "Formacion" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-57  NO-FOCUS
     LABEL "Prog 2 SAP" 
     SIZE 14 BY 1.14.

DEFINE VARIABLE articulo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE calidad AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Calidad" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FI-anio AS CHARACTER FORMAT "X(256)":U 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .95 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Listados Stock Completos Ordenados" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1
     BGCOLOR 6 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U INITIAL "Ordenes de Entrega" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U INITIAL "Consulta de Contratos" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-13 AS CHARACTER FORMAT "X(256)":U INITIAL "Listados Stock Cascara" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 9 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-14 AS CHARACTER FORMAT "X(256)":U INITIAL "Analisis Procesos Clarificado" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 21 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-15 AS CHARACTER FORMAT "X(256)":U INITIAL "Remitos a Excel" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U INITIAL "Consumo Envases" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 10 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-17 AS CHARACTER FORMAT "X(256)":U INITIAL "Analisis Jugo Turbio" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-18 AS CHARACTER FORMAT "X(256)":U INITIAL "Ingreso Balanza Industria" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1
     BGCOLOR 26 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "Listados Stock Materia Prima de Jugos" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Listados Stock Materia Prima de Aceites" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Listado x Citral x Kilos" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 2 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE nombre_articulo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 50 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE nombre_calidad AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 46 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE semana AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Semana" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE sucursal AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Plantas o Frigorificos" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 3.81.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 3.57.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 3.57.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 3.57.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 115 BY 50
     BGCOLOR 7 FGCOLOR 7 .

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30 BY 3.57.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 29 BY 3.57.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30 BY 3.57.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 3.57.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 124 BY 5.71.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 32 BY 3.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 3.57.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 3.57.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 3.81.

DEFINE VARIABLE chkLotes AS LOGICAL INITIAL no 
     LABEL "Incluye lotes en Produccion" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-47 AT ROW 22.19 COL 50
     BUTTON-57 AT ROW 9.71 COL 48.6
     BUTTON-48 AT ROW 9.71 COL 32
     BUTTON-42 AT ROW 8.48 COL 4.2
     BUTTON-43 AT ROW 8.48 COL 19
     BUTTON-44 AT ROW 8.48 COL 34.2
     sucursal AT ROW 1.71 COL 61 COLON-ALIGNED
     articulo AT ROW 2.91 COL 11 COLON-ALIGNED
     calidad AT ROW 4.1 COL 11 COLON-ALIGNED
     chkLotes AT ROW 5.52 COL 13
     FILL-IN-10 AT ROW 7.43 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-11 AT ROW 7.43 COL 64 COLON-ALIGNED NO-LABEL
     BUTTON-39 AT ROW 7.91 COL 103
     BUTTON-52 AT ROW 8.43 COL 49.6
     FI-anio AT ROW 8.62 COL 74 COLON-ALIGNED
     BUTTON-25 AT ROW 9.57 COL 88
     BUTTON-29 AT ROW 9.57 COL 103
     BUTTON-45 AT ROW 9.71 COL 4
     semana AT ROW 9.81 COL 74 COLON-ALIGNED
     FILL-IN-7 AT ROW 11.71 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-15 AT ROW 11.71 COL 33 COLON-ALIGNED NO-LABEL
     FILL-IN-12 AT ROW 11.71 COL 64 COLON-ALIGNED NO-LABEL
     BUTTON-14 AT ROW 13.14 COL 5
     BUTTON-49 AT ROW 13.14 COL 35
     BUTTON-26 AT ROW 13.14 COL 68
     BUTTON-23 AT ROW 13.62 COL 92
     BUTTON-21 AT ROW 13.62 COL 104
     BUTTON-38 AT ROW 13.62 COL 113
     FILL-IN-8 AT ROW 15.52 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 15.52 COL 64 COLON-ALIGNED NO-LABEL
     FILL-IN-18 AT ROW 15.52 COL 91 COLON-ALIGNED NO-LABEL
     BUTTON-18 AT ROW 16.95 COL 4
     BUTTON-28 AT ROW 16.95 COL 37.6
     BUTTON-19 AT ROW 16.95 COL 68
     BUTTON-55 AT ROW 16.95 COL 101
     FILL-IN-13 AT ROW 19.33 COL 1 COLON-ALIGNED NO-LABEL
     FILL-IN-16 AT ROW 19.33 COL 41 COLON-ALIGNED NO-LABEL
     FILL-IN-14 AT ROW 19.33 COL 64 COLON-ALIGNED NO-LABEL
     FILL-IN-17 AT ROW 19.33 COL 95 COLON-ALIGNED NO-LABEL
     BUTTON-35 AT ROW 21 COL 3
     BUTTON-53 AT ROW 21 COL 16
     BUTTON-56 AT ROW 21 COL 29
     BUTTON-50 AT ROW 21 COL 44
     BUTTON-51 AT ROW 21 COL 67
     BUTTON-54 AT ROW 21 COL 98
     BUTTON-40 AT ROW 22.19 COL 17
     BUTTON-27 AT ROW 22.19 COL 37.2
     BUTTON-37 AT ROW 22.24 COL 2
     nombre_articulo AT ROW 2.91 COL 19 COLON-ALIGNED NO-LABEL
     nombre_calidad AT ROW 4.1 COL 23 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 7.19 COL 65
     RECT-11 AT ROW 11.24 COL 65
     RECT-12 AT ROW 19.1 COL 2
     RECT-13 AT ROW 15.05 COL 92
     RECT-2 AT Y 215 X 455
     RECT-28 AT ROW 19.1 COL 65
     RECT-29 AT ROW 19.1 COL 96
     RECT-36 AT ROW 11.24 COL 34
     RECT-37 AT ROW 19.1 COL 42
     RECT-4 AT ROW 1.24 COL 1
     RECT-6 AT ROW 11.24 COL 2
     RECT-7 AT ROW 15.05 COL 2
     RECT-8 AT ROW 15.05 COL 65
     RECT-9 AT ROW 7.19 COL 2
     "74 -> Phoenix" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 2.67 COL 75
     "92 -> Merco Tuc" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 4.57 COL 102
     "86 -> Hiwa" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 1.71 COL 102
     "426-> Hall's Warehouse" VIEW-AS TEXT
          SIZE 24 BY .71 AT ROW 3.38 COL 75
     "73 -> Chesterfield" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 1.95 COL 75
     "96 -> Lavalle" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 6 COL 102
     "89 -> Swiff" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 3.86 COL 102
     "95 -> Famailla" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 5.29 COL 102
     "87 -> Metan" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 2.43 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.2 BY 22.52.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "88 -> Cool Queen" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 3.14 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.2 BY 22.52.


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
         TITLE              = "Reporte Stock"
         HEIGHT             = 22.43
         WIDTH              = 124.6
         MAX-HEIGHT         = 25.29
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 25.29
         VIRTUAL-WIDTH      = 160
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
/* SETTINGS FOR BUTTON BUTTON-21 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-21:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-23 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-23:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       BUTTON-27:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       BUTTON-37:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-38 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-38:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       BUTTON-40:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       BUTTON-47:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-12 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-13 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-14 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-15 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-16 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-17 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-18 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Reporte Stock */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Reporte Stock */
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


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 W-Win
ON CHOOSE OF BUTTON-14 IN FRAME F-Main /* Stock */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  /*
  CREATE SERVER hpp.
    flag = hpp:CONNECT("-AppService asindustria -H tucuman1").
    IF flag THEN DO:
        run w_calculo_stock_tambores_mat_prima.p ON SERVER hpp TRANSACTION DISTINCT (input integer(sucursal:screen-value in frame F-Main)).
        
    END.
    ELSE run w_calculo_stock_tambores_mat_prima.p (input integer(sucursal:screen-value in frame F-Main)).
  
    flag = hpp:DISCONNECT().
  */

  PROCESS EVENTS.

    IF lFlag THEN DO:    
      RUN callReporteMPJugo IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)) .
      WAIT-FOR F10 OF THIS-PROCEDURE.
    END.
    ELSE
      RUN callReporteMPJugo IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)).


  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_citrix",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 W-Win
ON CHOOSE OF BUTTON-18 IN FRAME F-Main /* Stock */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  
  for each stock_tambores.
    delete stock_tambores.
  end.
  

  CREATE SERVER hpp.
    flag = hpp:CONNECT("-AppService asindustria -H tucuman1").
    IF flag THEN DO:
        
        run p_calculo_stock_mat_prima_aceite.p ON SERVER hpp TRANSACTION DISTINCT (input integer(sucursal:screen-value in frame F-Main)).
        
    END.
    ELSE run p_calculo_stock_mat_prima_aceite.p (input integer(sucursal:screen-value in frame F-Main)).
  
   flag = hpp:DISCONNECT().
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_citrix",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 W-Win
ON CHOOSE OF BUTTON-19 IN FRAME F-Main /* Reporte */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 

  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          v_filtro = "tambores_industria.id_tipotambor = 2 and " + 
                     " tambores_industria.id_locacion_ubicacion = 4 and " + 
                     " tambores_industria.id_sucursal = " + sucursal:screen-value in frame F-Main .                    
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "prod_aceite_citral",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-21 W-Win
ON CHOOSE OF BUTTON-21 IN FRAME F-Main /* Listado */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 or 
   integer(sucursal:screen-value in frame F-Main) = 86 or
   integer(sucursal:screen-value in frame F-Main) = 87 or 
   integer(sucursal:screen-value in frame F-Main) = 88 or 
   integer(sucursal:screen-value in frame F-Main) = 89 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  /*colocar aqui la llamada al procedimiento que hace la tabla reporteadora para cascara*/
  
  run w_calculo_stock_nuevo_tambores.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_citrix_ord",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-23 W-Win
ON CHOOSE OF BUTTON-23 IN FRAME F-Main /* Stock x OF */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_nuevo_tambores.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "rep_stock_agrupado_of_citrix_ord",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-25 W-Win
ON CHOOSE OF BUTTON-25 IN FRAME F-Main /* Reporte */
DO:
  
define var v_semana as integer.
define var v_anio as integer.
define var v_filtro as char.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

SELF:SENSITIVE = NOT SELF:SENSITIVE.


v_semana = integer(semana:screen-value in frame F-Main).
v_anio = integer(fi-anio:screen-value in frame F-Main).

if v_semana >= 0 then
    do:
  /*      run p_reporte_oe_lote_semana.p (input v_semana).*/
      
        
    IF lFlag THEN DO:
      /*RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)) .*/
      RUN p_reporte_item_oe_lote_semana.p ON SERVER hApp ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (v_anio, v_semana) .
      WAIT-FOR F10 OF THIS-PROCEDURE.
      /*RUN p_reporte_item_oe_lote_semana.p ON SERVER hpp TRANSACTION DISTINCT (INPUT v_anio, INPUT v_semana).  */
    END.
    ELSE 
      MESSAGE "sin app" VIEW-AS ALERT-BOX.
      RUN p_reporte_item_oe_lote_semana.p (INPUT v_anio, INPUT v_semana).


        /************************************************************************************************************/
        /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
        /************************************************************************************************************/
                            
        v_filtro = "".
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                          /* "reporte_oe_lote_semana_citrix",                    /* RB-REPORT-NAME */*/
                           "rep_item_oe_lote_semana_citrix",                    /* RB-REPORT-NAME */
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
                           "Reporte de Orden de Entregas Semana",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
        /************************************************************************************************************/

    end.
else message "Por favor ingrese la semana que desea imprimir." view-as alert-box.    

SELF:SENSITIVE = NOT SELF:SENSITIVE.
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-26 W-Win
ON CHOOSE OF BUTTON-26 IN FRAME F-Main /* Reporte */
DO:
  define var r as rowid.
  if userid("userdb") = "y_daniel" or userid("userdb") = "y_adrianca" then
    do:
        run wc_contratos.w (output r).
        run p_reporte_contrato_detallado.p (input r).
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-27 W-Win
ON CHOOSE OF BUTTON-27 IN FRAME F-Main /* Stock x OF */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  DEFINE VAR hpp AS HANDLE.
  DEFINE VAR flag AS LOGICAL.
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 or 
   integer(sucursal:screen-value in frame F-Main) = 86 or
   integer(sucursal:screen-value in frame F-Main) = 87 or 
   integer(sucursal:screen-value in frame F-Main) = 88 or 
   integer(sucursal:screen-value in frame F-Main) = 0  or 
   integer(sucursal:screen-value in frame F-Main) = 89 OR
   integer(sucursal:screen-value in frame F-Main) = 123 THEN DO: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  /*
  CREATE SERVER hpp.
  flag = hpp:CONNECT("-AppService asindustria -H tucuman1").
  IF flag THEN 
    run w_calculo_stock_nuevo_tambores_opt.p ON SERVER hpp TRANSACTION DISTINCT (input integer(sucursal:screen-value in frame F-Main)).

  ELSE run w_calculo_stock_nuevo_tambores_opt.p (input integer(sucursal:screen-value in frame F-Main)) .
  
  flag = hpp:DISCONNECT().
  */
/*interrupcion para debugger*/

  run w_calculo_stock_nuevo_tambores_opt.p (input integer(sucursal:screen-value in frame F-Main)) .

  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "stock_agrup_of_citrix_ord_total",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-28 W-Win
ON CHOOSE OF BUTTON-28 IN FRAME F-Main /* Stock Detallado */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  DEFINE VAR tiempo AS INTEGER.
  
  SELF:SENSITIVE = NOT SELF:SENSITIVE.
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 or 
   integer(sucursal:screen-value in frame F-Main) = 86 or
   integer(sucursal:screen-value in frame F-Main) = 87 or 
   integer(sucursal:screen-value in frame F-Main) = 88 or 
   INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 461 OR
   integer(sucursal:screen-value in frame F-Main) = 89 THEN do:

    for each produccion_industria.
        delete produccion_industria.
    end.
    
    PROCESS EVENTS.


    IF lFlag THEN DO:
      RUN p_prod_seleccionados.p ON SERVER hApp TRANSACTION DISTINCT ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
        (input integer(sucursal:screen-value in frame F-Main)).
      WAIT-FOR F10 OF THIS-PROCEDURE.
    END.
    ELSE 
      RUN p_prod_seleccionados.p (INPUT INTEGER(sucursal:screen-value in frame F-Main)).

    
    /**************************************GENERADOR DE EXCELL **********************************/
  
    CREATE "Excel.Application" chExcelAplication.
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
    chWorkSheet:Columns("C"):ColumnWidth = 1.
    chWorkSheet:Columns("D"):ColumnWidth = 5.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 15.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 15.
    chWorkSheet:Columns("I"):ColumnWidth = 8.
    chWorkSheet:Columns("J"):ColumnWidth = 10.
    chWorkSheet:Columns("K"):ColumnWidth = 8.
    chWorkSheet:Columns("L"):ColumnWidth = 8.
    chWorkSheet:Columns("M"):ColumnWidth = 8.
    chWorkSheet:Columns("O"):ColumnWidth = 8.
    /*chWorkSheet:Columns("P"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".*/
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "REPORTE DE STOCK DE MATERIA PRIMA DE ACEITE".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 22.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  
  chWorkSheet:Range("B6"):Value = "LOTE".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "AÑO".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "ARTICULO".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "SUCURSAL".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "TAMBORES".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "KILOS TAMBOR".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "KILOS TOTAL".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "CITRAL".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "Kilos 400".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "NROMOV".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "ORIGEN".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH produccion_industria BY produccion_industria.id_produccion_industria.
                         
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_lote.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.anio_lote.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_articulo.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.articulo.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.id_sucursal.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.sucursal.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.tambores.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_tambor.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.kilos_400.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.nromov.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = produccion_industria.origen_water.

    
    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/
  
  
end.
else
    message "Por favor ingrese el codigo de sucursal de la planta que desea consultar." 
            view-as alert-box.  

    SELF:SENSITIVE = NOT SELF:SENSITIVE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-29 W-Win
ON CHOOSE OF BUTTON-29 IN FRAME F-Main /* Reporte Marily */
DO:
  
define var v_semana as integer.
define var v_anio as integer.
define var v_filtro as char.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

v_semana = integer(semana:screen-value in frame F-Main).
v_anio = integer(fi-anio:screen-value in frame F-Main).

if v_semana > 0 then
    do:
  /*      run p_reporte_oe_lote_semana.p (input v_semana).*/

        
    CREATE SERVER hpp.
    flag = hpp:CONNECT("-AppService asindustria -H tucuman1").
    IF flag THEN DO:
        run p_reporte_item_oe_lote_semana.p ON SERVER hpp TRANSACTION DISTINCT (INPUT v_anio, INPUT v_semana).
        
    END.
    ELSE RUN p_reporte_item_oe_lote_semana.p (INPUT v_anio, INPUT v_semana).

    
        /************************************************************************************************************/
        /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
        /************************************************************************************************************/
                            
        v_filtro = "".
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                          /* "reporte_oe_lote_semana_citrix",                    /* RB-REPORT-NAME */*/
                           "rep_item_oe_lote_semana_marily",                    /* RB-REPORT-NAME */
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
                           "Reporte de Orden de Entregas Semana",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
        /************************************************************************************************************/

    end.
else message "Por favor ingrese la semana que desea imprimir." view-as alert-box.    
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-35 W-Win
ON CHOOSE OF BUTTON-35 IN FRAME F-Main /* Stock */
DO:
  DEFINE VAR v_filtro     AS CHAR.
  DEFINE VAR v_articulo   AS INTEGER.
  DEFINE VAR v_calidad    AS INTEGER.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
IF INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main) <> 0 THEN DO: 
  
  v_articulo = INTEGER(articulo:SCREEN-VALUE IN frame F-Main).
  v_calidad  = INTEGER(calidad:SCREEN-VALUE IN frame F-Main).
  
  
  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                 v_articulo).
  
  RUN aderb\_prntrb2("..\industria\reports.prl",    /* RB-REPORT-LIBRARY */
                     "reporte_stock_rapido_cascara", /* RB-REPORT-NAME */
                     "",                            /* RB-DB-CONNECTION */
                     "O",                           /* RB-INCLUDE-RECORDS */
                     v_filtro,                      /* RB-FILTER */
                     RB-MEMO-FILE,                  /* RB-MEMO-FILE */
                     "D",                           /* RB-PRINT-DESTINATION */
                     "?",                           /* RB-PRINTER-NAME */
                     "",                            /* RB-PRINTER-PORT */
                     "",                            /* RB-OUTPUT-FILE */
                     1,                             /* RB-NUMBER-COPIES  - zero */                  
                     0,                             /* RB-BEGIN-PAGE - zero */
                     0,                             /* RB-END-PAGE - zero */
                     no,                            /* RB-TEST-PATTERN */
                     "Reporte de Stock",            /* RB-WINDOW-TITLE */
                     yes,                           /* RB-DISPLAY-ERRORS */
                     yes,                           /* RB-DISPLAY-STATUS */
                     no,                            /* RB-NO-WAIT */
                     "",                            /* RB-OTHER-PARAMETERS */
                     "").   
    
 
  /************************************************************************************************************/
END.
ELSE
  MESSAGE "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
  ALERT-BOX.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-37
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-37 W-Win
ON CHOOSE OF BUTTON-37 IN FRAME F-Main /* Listado x Lote */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
 
  
if integer(sucursal:screen-value in frame F-Main) = 73 or
   integer(sucursal:screen-value in frame F-Main) = 74 or 
   integer(sucursal:screen-value in frame F-Main) = 86 or
   integer(sucursal:screen-value in frame F-Main) = 87 or 
   integer(sucursal:screen-value in frame F-Main) = 88 or 
   integer(sucursal:screen-value in frame F-Main) = 89 OR
   integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 or 
   integer(sucursal:screen-value in frame F-Main) = 426 OR
   integer(sucursal:screen-value in frame F-Main) = 123 THEN DO: 

  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  /*colocar aqui la llamada al procedimiento que hace la tabla reporteadora para cascara*/
  
  CREATE SERVER hpp.
  flag = hpp:CONNECT("-AppService asindustria -H 192.168.1.4").
  IF flag THEN 
    run w_calculo_stock_nuevo_tambores_opt.p ON SERVER hpp TRANSACTION DISTINCT (input integer(sucursal:screen-value in frame F-Main)).

  ELSE RUN w_calculo_stock_nuevo_tambores_opt.p (INPUT INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)) .
  
   flag = hpp:DISCONNECT(). 
   

  /*RUN w_calculo_stock_nuevo_tambores_opt.p (INPUT INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)) .*/

  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
                      
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_citrix_ord",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-38
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-38 W-Win
ON CHOOSE OF BUTTON-38 IN FRAME F-Main /* Stock x OF */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 or 
   integer(sucursal:screen-value in frame F-Main) = 86 or
   integer(sucursal:screen-value in frame F-Main) = 87 or 
   integer(sucursal:screen-value in frame F-Main) = 88 or 
   integer(sucursal:screen-value in frame F-Main) = 89 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_nuevo_tambores.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "stock_agrup_of_citrix_ord_total",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-39
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-39 W-Win
ON CHOOSE OF BUTTON-39 IN FRAME F-Main /* Lavalle */
DO:
  
define var v_semana as integer.
define var v_anio as integer.
define var v_filtro as char.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".


v_semana = integer(semana:screen-value in frame F-Main).
v_anio = integer(fi-anio:screen-value in frame F-Main).

if v_semana >= 0 then
    do:
  /*      run p_reporte_oe_lote_semana.p (input v_semana).*/

        
    CREATE SERVER hpp.
    flag = hpp:CONNECT("-AppService asindustria -H tucuman1").
    IF flag THEN DO:
        run p_reporte_item_oe_lote_semana.p ON SERVER hpp TRANSACTION DISTINCT (INPUT v_anio, INPUT v_semana).
        
    END.
    ELSE RUN p_reporte_item_oe_lote_semana.p (INPUT v_anio, INPUT v_semana).
        
        /************************************************************************************************************/
        /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
        /************************************************************************************************************/
                            
        v_filtro = "".
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "rep_item_oe_lote_semana",                    /* RB-REPORT-NAME */
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
                           "Reporte de Orden de Entregas Semanal",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
        /************************************************************************************************************/

    end.
else message "Por favor ingrese la semana que desea imprimir." view-as alert-box.    
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-40
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-40 W-Win
ON CHOOSE OF BUTTON-40 IN FRAME F-Main /* Listado x Calidad */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  
  
if integer(sucursal:screen-value in frame F-Main) = 73 or
   integer(sucursal:screen-value in frame F-Main) = 74 or 
   integer(sucursal:screen-value in frame F-Main) = 86 or
   integer(sucursal:screen-value in frame F-Main) = 87 or 
   integer(sucursal:screen-value in frame F-Main) = 88 or 
   integer(sucursal:screen-value in frame F-Main) = 89 OR
   integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 or 
   integer(sucursal:screen-value in frame F-Main) = 426 OR
   integer(sucursal:screen-value in frame F-Main) = 123 THEN DO: 

  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  /*colocar aqui la llamada al procedimiento que hace la tabla reporteadora para cascara*/
  
  CREATE SERVER hpp.
  flag = hpp:CONNECT("-AppService asindustria -H tucuman1").
  IF flag THEN 
    run w_calculo_stock_nuevo_tambores_opt.p ON SERVER hpp TRANSACTION DISTINCT (input integer(sucursal:screen-value in frame F-Main)).

  ELSE RUN w_calculo_stock_nuevo_tambores_opt.p (INPUT INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)) .
  
  /* flag = hpp:DISCONNECT(). */

  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "rep_stock_rapido_citrix_ord_cal",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-42
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-42 W-Win
ON CHOOSE OF BUTTON-42 IN FRAME F-Main /* List x Lote */
DO:
  DEFINE VARIABLE cFiltro      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO INITIAL "".

  DEFINE VARIABLE iArticulo    AS INTEGER  NO-UNDO.
  DEFINE VARIABLE iCalidad     AS INTEGER  NO-UNDO.

  SELF:SENSITIVE = NOT SELF:SENSITIVE.
 
  
  IF INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 50 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 73 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 74 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 86 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 87 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 88 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 89 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 95 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 96 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 426 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 428 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 461 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 462 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 501 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 502 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 503 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 511 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 512 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 123 THEN DO: 
  
    iArticulo = INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main).
    iCalidad  = INTEGER(calidad:SCREEN-VALUE IN FRAME F-Main).
    
    
    PROCESS EVENTS.

    IF lFlag THEN DO:    
      RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                                                                          LOGICAL(chkLotes:SCREEN-VALUE IN FRAME F-Main)) .
      WAIT-FOR F10 OF THIS-PROCEDURE.
    END.
    ELSE
      RUN callReporteStock IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), LOGICAL(chkLotes:SCREEN-VALUE IN FRAME F-Main)).

        
    /************************************************************************************************************/
    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
    /************************************************************************************************************/
          
    IF iArticulo <> 0 AND iCalidad = 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo).
    END. 
    
    IF iArticulo = 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo <> 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo) +
                " and stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo = 0 AND iCalidad = 0 THEN DO:
      cFiltro = "".       
    END.
                
                  
    RUN  aderb\_prntrb2("..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                        "reporte_stock_rapido_citrix_ord",                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        cFiltro,                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Reporte de Stock",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        "" /* RB-OTHER-PARAMETERS */,
                        "").   


  /************************************************************************************************************/
  END.
  ELSE
    MESSAGE "Por favor ingrese la codigo de sucursal de la planta que desea consultar." VIEW-AS ALERT-BOX.  
    

  SELF:SENSITIVE = NOT SELF:SENSITIVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-43
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-43 W-Win
ON CHOOSE OF BUTTON-43 IN FRAME F-Main /* List x Calidad */
DO:
  DEFINE VARIABLE cFiltro      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO INITIAL "".

  DEFINE VARIABLE iArticulo    AS INTEGER  NO-UNDO.
  DEFINE VARIABLE iCalidad     AS INTEGER  NO-UNDO.

  SELF:SENSITIVE = NOT SELF:SENSITIVE.
 
  
  IF INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 50 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 73 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 74 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 86 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 87 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 88 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 89 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 95 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 96 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 426 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 461 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 462 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 123 THEN DO: 
  
    iArticulo = INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main).
    iCalidad  = INTEGER(calidad:SCREEN-VALUE IN FRAME F-Main).
    
    
    PROCESS EVENTS.

    IF lFlag THEN DO:    
      RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                                                                          LOGICAL(chkLotes:SCREEN-VALUE IN FRAME F-Main)) .

      WAIT-FOR F10 OF THIS-PROCEDURE.
    END.
    ELSE
      RUN callReporteStock IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                    LOGICAL(chkLotes:SCREEN-VALUE IN FRAME F-Main)).
        
    /************************************************************************************************************/
    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
    /************************************************************************************************************/
          
    IF iArticulo <> 0 AND iCalidad = 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo).
    END. 
    
    IF iArticulo = 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo <> 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo) +
                " and stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo = 0 AND iCalidad = 0 THEN DO:
      cFiltro = "".       
    END.
                
                  
    RUN  aderb\_prntrb2("..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                        "rep_stock_rapido_citrix_ord_cal",                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        cFiltro,                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Reporte de Stock",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        "" /* RB-OTHER-PARAMETERS */,
                        "").   


  /************************************************************************************************************/
  END.
  ELSE
    MESSAGE "Por favor ingrese la codigo de sucursal de la planta que desea consultar." VIEW-AS ALERT-BOX.  


  SELF:SENSITIVE = NOT SELF:SENSITIVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-44
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-44 W-Win
ON CHOOSE OF BUTTON-44 IN FRAME F-Main /* Stock x OF */
DO:
  DEFINE VARIABLE cFiltro      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO INITIAL "".

  DEFINE VARIABLE iArticulo    AS INTEGER  NO-UNDO.
  DEFINE VARIABLE iCalidad     AS INTEGER  NO-UNDO.

  SELF:SENSITIVE = NOT SELF:SENSITIVE.
 
  
  IF INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 50 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 73 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 74 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 86 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 87 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 88 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 89 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 95 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 96 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 426 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 461 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 462 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 123 THEN DO: 
  
    iArticulo = INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main).
    iCalidad  = INTEGER(calidad:SCREEN-VALUE IN FRAME F-Main).
    
    
    PROCESS EVENTS.

    IF lFlag THEN DO:    
      RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                                                                          LOGICAL(chkLotes:SCREEN-VALUE IN FRAME F-Main)) .

      WAIT-FOR F10 OF THIS-PROCEDURE.
    END.
    ELSE
      RUN callReporteStock IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main),
                                    LOGICAL(chkLotes:SCREEN-VALUE IN FRAME F-Main)).
        
    /************************************************************************************************************/
    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
    /************************************************************************************************************/
          
    IF iArticulo <> 0 AND iCalidad = 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo).
    END. 
    
    IF iArticulo = 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo <> 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo) +
                " and stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo = 0 AND iCalidad = 0 THEN DO:
      cFiltro = "".       
    END.
                
                  
    RUN  aderb\_prntrb2("..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                        "stock_agrup_of_citrix_ord_total",                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        cFiltro,                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Reporte de Stock",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        "" /* RB-OTHER-PARAMETERS */,
                        "").   


  /************************************************************************************************************/
  END.
  ELSE
    MESSAGE "Por favor ingrese la codigo de sucursal de la planta que desea consultar." VIEW-AS ALERT-BOX.  

  SELF:SENSITIVE = NOT SELF:SENSITIVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-45
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-45 W-Win
ON CHOOSE OF BUTTON-45 IN FRAME F-Main /* Stock a Excel */
DO:
  RUN w_rep_excell_stock.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-47
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-47 W-Win
ON CHOOSE OF BUTTON-47 IN FRAME F-Main /* List Dep Ext */
DO:
  DEFINE VARIABLE cFiltro      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO INITIAL "".

  DEFINE VARIABLE iArticulo    AS INTEGER  NO-UNDO.
  DEFINE VARIABLE iCalidad     AS INTEGER  NO-UNDO.

  SELF:SENSITIVE = NOT SELF:SENSITIVE.
 
  
  IF INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 73 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 74 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 86 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 87 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 88 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 89 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 95 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 96 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 426 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 462 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 123 THEN DO: 
  
    iArticulo = INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main).
    iCalidad  = INTEGER(calidad:SCREEN-VALUE IN FRAME F-Main).
    
    
    PROCESS EVENTS.

    IF lFlag THEN DO:    
      RUN callReporteStockDepExt IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)) .
      WAIT-FOR F10 OF THIS-PROCEDURE.
    END.
    ELSE
      RUN callReporteStockDepExt IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)).

        
    /************************************************************************************************************/
    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
    /************************************************************************************************************/
          
    IF iArticulo <> 0 AND iCalidad = 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo).
    END. 
    
    IF iArticulo = 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo <> 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo) +
                " and stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo = 0 AND iCalidad = 0 THEN DO:
      cFiltro = "".       
    END.
                
                  
    RUN  aderb\_prntrb2("..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                        "reporte_stock_dep_ext",                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        cFiltro,                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Reporte de Stock",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        "" /* RB-OTHER-PARAMETERS */,
                        "").   


  /************************************************************************************************************/
  END.
  ELSE
    MESSAGE "Por favor ingrese la codigo de sucursal de la planta que desea consultar." VIEW-AS ALERT-BOX.  
    

  SELF:SENSITIVE = NOT SELF:SENSITIVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-48
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-48 W-Win
ON CHOOSE OF BUTTON-48 IN FRAME F-Main /* List x Estado */
DO:
  DEFINE VARIABLE cFiltro      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO INITIAL "".

  DEFINE VARIABLE iArticulo    AS INTEGER  NO-UNDO.
  DEFINE VARIABLE iCalidad     AS INTEGER  NO-UNDO.

  SELF:SENSITIVE = NOT SELF:SENSITIVE.
 
  
  IF INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 73 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 74 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 86 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 87 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 88 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 89 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 95 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 96 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 426 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 428 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 461 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 462 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 502 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 503 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 511 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 512 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 123 THEN DO: 
  
    iArticulo = INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main).
    iCalidad  = INTEGER(calidad:SCREEN-VALUE IN FRAME F-Main).
    
    
    PROCESS EVENTS.

    IF lFlag THEN DO:    
      RUN callReporteEstadosTambores IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)) .
      WAIT-FOR F10 OF THIS-PROCEDURE.
    END.
    ELSE
      RUN callReporteEstadosTambores IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main)).

        
    /************************************************************************************************************/
    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
    /************************************************************************************************************/
          
    IF iArticulo <> 0 AND iCalidad = 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo).
    END. 
    
    IF iArticulo = 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo <> 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo) +
                " and stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo = 0 AND iCalidad = 0 THEN DO:
      cFiltro = "".       
    END.
                
                  
    RUN  aderb\_prntrb2("..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                        "reporte_stock_rapido_citrix_est",                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        cFiltro,                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Reporte de Stock",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        "" /* RB-OTHER-PARAMETERS */,
                        "").   


  /************************************************************************************************************/
  END.
  ELSE
    MESSAGE "Por favor ingrese la codigo de sucursal de la planta que desea consultar." VIEW-AS ALERT-BOX.  
    

  SELF:SENSITIVE = NOT SELF:SENSITIVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 W-Win
ON CHOOSE OF BUTTON-49 IN FRAME F-Main /* Remitos */
DO:
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  RUN wdParams.w (OUTPUT iSuc,
                  OUTPUT dDes, 
                  OUTPUT dHas, 
                  OUTPUT iArt).

  RUN exportRemitoFactura IN hLib (iSuc, dDes, dHas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-50
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-50 W-Win
ON CHOOSE OF BUTTON-50 IN FRAME F-Main /* Exportar XLS */
DO:
   DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
   DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
   DEFINE VARIABLE dDes AS DATE       NO-UNDO.
   DEFINE VARIABLE dHas AS DATE       NO-UNDO.

   DEFINE VARIABLE hLibCom AS HANDLE.
   RUN libCommonFunctions.p PERSISTENT SET hLibCom.
   hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
   DELETE OBJECT hLibCom.

   RUN wdParams.w (OUTPUT iSuc,
                   OUTPUT dDes, 
                   OUTPUT dHas, 
                   OUTPUT iArt, 
                   OUTPUT iLin).

   RUN exportConsumoEnvases IN hLib (dDes, dHas, iArt).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-51
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-51 W-Win
ON CHOOSE OF BUTTON-51 IN FRAME F-Main /* Exportar XLS */
DO:
   DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  RUN wdParams.w (OUTPUT iSuc,
                  OUTPUT dDes, 
                  OUTPUT dHas, 
                  OUTPUT iArt, 
                  OUTPUT iLin).

  RUN exportConsumosClarificado IN hLib (dDes, dHas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-52
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-52 W-Win
ON CHOOSE OF BUTTON-52 IN FRAME F-Main /* Stock p/mail */
DO:
  DEFINE VARIABLE cFiltro      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO INITIAL "".

  DEFINE VARIABLE iArticulo    AS INTEGER  NO-UNDO.
  DEFINE VARIABLE iCalidad     AS INTEGER  NO-UNDO.

  SELF:SENSITIVE = NOT SELF:SENSITIVE.
 
  
  IF INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 50 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 73 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 74 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 86 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 87 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 88 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 89 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 95 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 96 OR 
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 426 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 428 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 461 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 462 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 501 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 502 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 503 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 511 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 512 OR
     INTEGER(sucursal:SCREEN-VALUE IN frame F-Main) = 123 THEN DO: 
  
    iArticulo = INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main).
    iCalidad  = INTEGER(calidad:SCREEN-VALUE IN FRAME F-Main).
    
    
    PROCESS EVENTS.

    IF lFlag THEN DO:    
      RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                                                                          LOGICAL(chkLotes:SCREEN-VALUE IN FRAME F-Main)) .
      WAIT-FOR F10 OF THIS-PROCEDURE.
    END.
    ELSE
      RUN callReporteStock IN hLib (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), LOGICAL(chkLotes:SCREEN-VALUE IN FRAME F-Main)).

        
    /************************************************************************************************************/
    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
    /************************************************************************************************************/
          
    IF iArticulo <> 0 AND iCalidad = 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo).
    END. 
    
    IF iArticulo = 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo <> 0 AND iCalidad <> 0 THEN DO:
      cFiltro = "stock_tambores.id_articulo = " + STRING(iArticulo) +
                " and stock_tambores.id_calidad = " + STRING(iCalidad).       
    END.
    
    IF iArticulo = 0 AND iCalidad = 0 THEN DO:
      cFiltro = "".       
    END.


    DEFINE VARIABLE hLibCom AS HANDLE.
    RUN libCommonFunctions.p PERSISTENT SET hLibCom.

    RUN mailPdf IN hLibCom ("..\industria\reports.prl",
                            "reporte_stock_rapido_citrix_ord",
                            cFiltro,
                            "",
                            "Reporte de Stock",
                            "Reporte de Stock").
  END.
  ELSE
    MESSAGE "Por favor ingrese la codigo de sucursal de la planta que desea consultar." VIEW-AS ALERT-BOX.  
    

  SELF:SENSITIVE = NOT SELF:SENSITIVE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-53
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-53 W-Win
ON CHOOSE OF BUTTON-53 IN FRAME F-Main /* Stock XLS */
DO:
  DEFINE VAR v_filtro     AS CHAR.
  DEFINE VAR v_articulo   AS INTEGER.
  DEFINE VAR v_calidad    AS INTEGER.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
IF INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main) <> 0 THEN DO: 
  
  v_articulo = INTEGER(articulo:SCREEN-VALUE IN frame F-Main).
  v_calidad  = INTEGER(calidad:SCREEN-VALUE IN frame F-Main).
  
  
  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main), 
                                 v_articulo).
  
  {..\industria\p_excell_stock.p}
    
 
  /************************************************************************************************************/
END.
ELSE
  MESSAGE "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
  ALERT-BOX.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-54
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-54 W-Win
ON CHOOSE OF BUTTON-54 IN FRAME F-Main /* Exportar XLS */
DO:
   DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  RUN wdParams.w (OUTPUT iSuc,
                  OUTPUT dDes, 
                  OUTPUT dHas, 
                  OUTPUT iArt, 
                  OUTPUT iLin).

  RUN exportExcelAnalisisJugo IN hLib (dDes, dHas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-55
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-55 W-Win
ON CHOOSE OF BUTTON-55 IN FRAME F-Main /* Exportar XLS */
DO:
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  RUN wdParams.w (OUTPUT iSuc,
                  OUTPUT dDes, 
                  OUTPUT dHas, 
                  OUTPUT iArt, 
                  OUTPUT iLin).

  RUN parteIngresoBalanza IN hLib (dDes, dHas, iLin).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-56
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-56 W-Win
ON CHOOSE OF BUTTON-56 IN FRAME F-Main /* Formacion */
DO:
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.
  RUN wdParams.w (OUTPUT iSuc,
                  OUTPUT dDes, 
                  OUTPUT dHas, 
                  OUTPUT iArt, 
                  OUTPUT iLin).

  RUN exportProdLotesCascara IN hLib (dDes, dHas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-57
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-57 W-Win
ON CHOOSE OF BUTTON-57 IN FRAME F-Main /* Prog 2 SAP */
DO:

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  RUN exportExcelStockSAP IN hLib.

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
       RUN set-position IN h_cus-misc ( 11.48 , 93.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             semana:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  DISPLAY sucursal articulo calidad chkLotes FILL-IN-10 FILL-IN-11 FI-anio 
          semana FILL-IN-7 FILL-IN-15 FILL-IN-12 FILL-IN-8 FILL-IN-9 FILL-IN-18 
          FILL-IN-13 FILL-IN-16 FILL-IN-14 FILL-IN-17 nombre_articulo 
          nombre_calidad 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-47 BUTTON-57 BUTTON-48 BUTTON-42 BUTTON-43 BUTTON-44 sucursal 
         articulo calidad chkLotes BUTTON-39 BUTTON-52 FI-anio BUTTON-25 
         BUTTON-29 BUTTON-45 semana BUTTON-14 BUTTON-49 BUTTON-26 BUTTON-18 
         BUTTON-28 BUTTON-19 BUTTON-55 BUTTON-35 BUTTON-53 BUTTON-56 BUTTON-50 
         BUTTON-51 BUTTON-54 BUTTON-40 BUTTON-27 BUTTON-37 nombre_articulo 
         nombre_calidad RECT-10 RECT-11 RECT-12 RECT-13 RECT-2 RECT-28 RECT-29 
         RECT-36 RECT-37 RECT-4 RECT-6 RECT-7 RECT-8 RECT-9 
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

  /* Code placed here will execute AFTER standard behavior.    */
  fi-anio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(YEAR(TODAY),"9999").  

  button-37:VISIBLE IN FRAME F-Main = FALSE.
  button-40:VISIBLE IN FRAME F-Main = FALSE.
  button-27:VISIBLE IN FRAME F-Main = FALSE.

  IF USERID("userdb") MATCHES "*munoz*" THEN
    button-27:LABEL IN FRAME F-Main = "El boton de Muñoz".

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

