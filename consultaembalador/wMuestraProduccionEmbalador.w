&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
DEFINE VARIABLE h_asprod                   AS HANDLE          NO-UNDO.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
{src/adm2/widgetprto.i}
DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.



DEFINE VAR HLIB AS HANDLE NO-UNDO.
DEFINE VAR hp   AS HANDLE NO-UNDO.

DEFINE VAR CEMBALADOR AS CHARACTER NO-UNDO FORMAT '9999999999'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-3 RECT-4 fdt1 F-EMBALADOR F-SUC ~
F-MEN10E F-MEN20E F-MAY20E F-STOTALE fdt2 F-TOTAL F-MEN10 F-MEN20 F-MAY20 ~
F-STOTAL fdt1-2 F-MEN10E-2 F-MEN20E-2 F-MAY20E-2 F-STOTALE-2 fdt2-2 ~
F-TOTAL-2 F-MEN-11 F-MEN-21 F-MAY-21 F-STOTAL-2 fdt1-3 F-MEN10E-3 ~
F-MEN20E-3 F-MAY20E-3 F-STOTALE-3 fdt2-3 FTARJETAS F-TOTAL-3 F-MEN-12 ~
F-MEN-22 F-MAY-22 F-STOTAL-3 
&Scoped-Define DISPLAYED-OBJECTS fdt1 F-EMBALADOR F-SUC F-MEN10E F-MEN20E ~
F-MAY20E F-STOTALE fdt2 F-TOTAL F-MEN10 F-MEN20 F-MAY20 F-STOTAL fdt1-2 ~
F-MEN10E-2 F-MEN20E-2 F-MAY20E-2 F-STOTALE-2 fdt2-2 F-TOTAL-2 F-MEN-11 ~
F-MEN-21 F-MAY-21 F-STOTAL-2 fdt1-3 F-MEN10E-3 F-MEN20E-3 F-MAY20E-3 ~
F-STOTALE-3 fdt2-3 FTARJETAS F-TOTAL-3 F-MEN-12 F-MEN-22 F-MAY-22 ~
F-STOTAL-3 FTEXTO 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD embalador wWin 
FUNCTION embalador RETURNS CHARACTER
  ( INPUT CEMBALADOR AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queryName wWin 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE F-EMBALADOR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 217 BY 3.81
     BGCOLOR 11 FGCOLOR 9 FONT 31 NO-UNDO.

DEFINE VARIABLE F-MAY-21 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FONT 30 NO-UNDO.

DEFINE VARIABLE F-MAY-22 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FONT 30 NO-UNDO.

DEFINE VARIABLE F-MAY20 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FONT 30 NO-UNDO.

DEFINE VARIABLE F-MAY20E AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FONT 30 NO-UNDO.

DEFINE VARIABLE F-MAY20E-2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FONT 30 NO-UNDO.

DEFINE VARIABLE F-MAY20E-3 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN-11 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN-12 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN-21 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 2 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN-22 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 2 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN10 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN10E AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN10E-2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN10E-3 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN20 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 2 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN20E AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 2 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN20E-2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 2 FONT 30 NO-UNDO.

DEFINE VARIABLE F-MEN20E-3 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 2 FONT 30 NO-UNDO.

DEFINE VARIABLE F-STOTAL AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-STOTAL-2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-STOTAL-3 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-STOTALE AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-STOTALE-2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-STOTALE-3 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-SUC AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 2.38
     FONT 30 NO-UNDO.

DEFINE VARIABLE F-TOTAL AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-TOTAL-2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE F-TOTAL-3 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 3.52
     FGCOLOR 9 FONT 30 NO-UNDO.

DEFINE VARIABLE fdt1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 34.2 BY 1.19
     FONT 16 NO-UNDO.

DEFINE VARIABLE fdt1-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 34.2 BY 1.19
     FONT 16 NO-UNDO.

DEFINE VARIABLE fdt1-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 34.2 BY 1.19
     FONT 16 NO-UNDO.

DEFINE VARIABLE fdt2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 34.2 BY 1.19
     FONT 16 NO-UNDO.

DEFINE VARIABLE fdt2-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 34.2 BY 1.19
     FONT 16 NO-UNDO.

DEFINE VARIABLE fdt2-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 34.2 BY 1.19
     FONT 16 NO-UNDO.

DEFINE VARIABLE FTARJETAS AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.6 BY 2.14
     FONT 30 NO-UNDO.

DEFINE VARIABLE FTEXTO AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 89.2 BY 2.14
     FONT 30 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 26.43.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 26.43.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 26.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fdt1 AT ROW 9.1 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 218 NO-TAB-STOP 
     F-EMBALADOR AT ROW 4.19 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 6 NO-TAB-STOP 
     F-SUC AT ROW 1.33 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     F-MEN10E AT ROW 13.52 COL 49 RIGHT-ALIGNED NO-LABEL WIDGET-ID 206 NO-TAB-STOP 
     F-MEN20E AT ROW 17.62 COL 49 RIGHT-ALIGNED NO-LABEL WIDGET-ID 210 NO-TAB-STOP 
     F-MAY20E AT ROW 21.48 COL 49 RIGHT-ALIGNED NO-LABEL WIDGET-ID 200 NO-TAB-STOP 
     F-STOTALE AT ROW 25.76 COL 49.8 RIGHT-ALIGNED NO-LABEL WIDGET-ID 214 NO-TAB-STOP 
     fdt2 AT ROW 9.1 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 220 NO-TAB-STOP 
     F-TOTAL AT ROW 30.38 COL 58 RIGHT-ALIGNED NO-LABEL WIDGET-ID 216 NO-TAB-STOP 
     F-MEN10 AT ROW 13.52 COL 66.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 204 NO-TAB-STOP 
     F-MEN20 AT ROW 17.62 COL 66.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 208 NO-TAB-STOP 
     F-MAY20 AT ROW 21.48 COL 66.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 198 NO-TAB-STOP 
     F-STOTAL AT ROW 25.76 COL 66.8 RIGHT-ALIGNED NO-LABEL WIDGET-ID 212 NO-TAB-STOP 
     fdt1-2 AT ROW 9.1 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 138 NO-TAB-STOP 
     F-MEN10E-2 AT ROW 13.52 COL 124 RIGHT-ALIGNED NO-LABEL WIDGET-ID 126 NO-TAB-STOP 
     F-MEN20E-2 AT ROW 17.62 COL 124 RIGHT-ALIGNED NO-LABEL WIDGET-ID 130 NO-TAB-STOP 
     F-MAY20E-2 AT ROW 21.48 COL 124 RIGHT-ALIGNED NO-LABEL WIDGET-ID 202 NO-TAB-STOP 
     F-STOTALE-2 AT ROW 25.76 COL 124 RIGHT-ALIGNED NO-LABEL WIDGET-ID 134 NO-TAB-STOP 
     fdt2-2 AT ROW 9.1 COL 111 COLON-ALIGNED NO-LABEL WIDGET-ID 140 NO-TAB-STOP 
     F-TOTAL-2 AT ROW 30.38 COL 133 RIGHT-ALIGNED NO-LABEL WIDGET-ID 136 NO-TAB-STOP 
     F-MEN-11 AT ROW 13.52 COL 141.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 124 NO-TAB-STOP 
     F-MEN-21 AT ROW 17.62 COL 141.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 128 NO-TAB-STOP 
     F-MAY-21 AT ROW 21.48 COL 141.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 196 NO-TAB-STOP 
     F-STOTAL-2 AT ROW 25.76 COL 141.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 132 NO-TAB-STOP 
     fdt1-3 AT ROW 9.1 COL 150 COLON-ALIGNED NO-LABEL WIDGET-ID 176 NO-TAB-STOP 
     F-MEN10E-3 AT ROW 13.52 COL 198 RIGHT-ALIGNED NO-LABEL WIDGET-ID 166 NO-TAB-STOP 
     F-MEN20E-3 AT ROW 17.62 COL 198 RIGHT-ALIGNED NO-LABEL WIDGET-ID 168 NO-TAB-STOP 
     F-MAY20E-3 AT ROW 21.48 COL 198 RIGHT-ALIGNED NO-LABEL WIDGET-ID 160 NO-TAB-STOP 
     F-STOTALE-3 AT ROW 25.76 COL 198 RIGHT-ALIGNED NO-LABEL WIDGET-ID 172 NO-TAB-STOP 
     fdt2-3 AT ROW 9.1 COL 185 COLON-ALIGNED NO-LABEL WIDGET-ID 178 NO-TAB-STOP 
     FTARJETAS AT ROW 1.38 COL 191.6 COLON-ALIGNED NO-LABEL WIDGET-ID 122
     F-TOTAL-3 AT ROW 30.38 COL 207 RIGHT-ALIGNED NO-LABEL WIDGET-ID 174 NO-TAB-STOP 
     F-MEN-12 AT ROW 13.52 COL 215.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 162 NO-TAB-STOP 
     F-MEN-22 AT ROW 17.62 COL 215.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 164 NO-TAB-STOP 
     F-MAY-22 AT ROW 21.48 COL 215.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 158 NO-TAB-STOP 
     F-STOTAL-3 AT ROW 25.76 COL 215.4 RIGHT-ALIGNED NO-LABEL WIDGET-ID 170 NO-TAB-STOP 
     FTEXTO AT ROW 1.43 COL 47.4 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     "10 - 20" VIEW-AS TEXT
          SIZE 28 BY 3.52 AT ROW 17.43 COL 152 WIDGET-ID 186
          FGCOLOR 2 FONT 30
     "STOTAL" VIEW-AS TEXT
          SIZE 30 BY 3.52 AT ROW 25.52 COL 152 WIDGET-ID 190
          FGCOLOR 9 FONT 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 273.2 BY 34.48 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     "TOT" VIEW-AS TEXT
          SIZE 18 BY 3.52 AT ROW 30.05 COL 171 WIDGET-ID 192
          FGCOLOR 9 FONT 30
     "10 - 20" VIEW-AS TEXT
          SIZE 28 BY 3.52 AT ROW 17.43 COL 78 WIDGET-ID 148
          FGCOLOR 2 FONT 30
     "S/P" VIEW-AS TEXT
          SIZE 12.8 BY 2.05 AT ROW 10.95 COL 129.6 WIDGET-ID 146
          FONT 18
     "C/P" VIEW-AS TEXT
          SIZE 13 BY 2.05 AT ROW 10.91 COL 37 WIDGET-ID 224
          FONT 18
     "S/P" VIEW-AS TEXT
          SIZE 12.8 BY 2.05 AT ROW 10.95 COL 54.6 WIDGET-ID 226
          FONT 18
     "10 - 20" VIEW-AS TEXT
          SIZE 28 BY 3.52 AT ROW 17.43 COL 3 WIDGET-ID 228
          FGCOLOR 2 FONT 30
     "> 20" VIEW-AS TEXT
          SIZE 20 BY 3.52 AT ROW 21.24 COL 11 WIDGET-ID 230
          FONT 30
     "STOTAL" VIEW-AS TEXT
          SIZE 30 BY 3.52 AT ROW 25.52 COL 3 WIDGET-ID 232
          FGCOLOR 9 FONT 30
     "TOT" VIEW-AS TEXT
          SIZE 18 BY 3.52 AT ROW 30.05 COL 22 WIDGET-ID 234
          FGCOLOR 9 FONT 30
     "< 10" VIEW-AS TEXT
          SIZE 19 BY 3.52 AT ROW 13.14 COL 11 WIDGET-ID 236
          FGCOLOR 9 FONT 30
     "> 20" VIEW-AS TEXT
          SIZE 20 BY 3.52 AT ROW 21.24 COL 86 WIDGET-ID 150
          FONT 30
     "C/P" VIEW-AS TEXT
          SIZE 13 BY 2.05 AT ROW 10.91 COL 186 WIDGET-ID 182
          FONT 18
     "< 10" VIEW-AS TEXT
          SIZE 19 BY 3.52 AT ROW 13.14 COL 86 WIDGET-ID 156
          FGCOLOR 9 FONT 30
     "TOT" VIEW-AS TEXT
          SIZE 18 BY 3.52 AT ROW 30.05 COL 97 WIDGET-ID 154
          FGCOLOR 9 FONT 30
     "STOTAL" VIEW-AS TEXT
          SIZE 30 BY 3.52 AT ROW 25.52 COL 78 WIDGET-ID 152
          FGCOLOR 9 FONT 30
     "S/P" VIEW-AS TEXT
          SIZE 12.8 BY 2.05 AT ROW 10.95 COL 203.6 WIDGET-ID 184
          FONT 18
     "98 - L" VIEW-AS TEXT
          SIZE 23 BY 1.43 AT ROW 1.33 COL 6.8 WIDGET-ID 78
          FONT 20
     "Tarjetas Leídas" VIEW-AS TEXT
          SIZE 52.2 BY 2.19 AT ROW 1.43 COL 140.8 WIDGET-ID 120
          FONT 18
     "97 - Famailla" VIEW-AS TEXT
          SIZE 23 BY 1.43 AT ROW 2.52 COL 6.8 WIDGET-ID 80
          FONT 20
     "C/P" VIEW-AS TEXT
          SIZE 13 BY 2.05 AT ROW 10.91 COL 112 WIDGET-ID 144
          FONT 18
     "< 10" VIEW-AS TEXT
          SIZE 19 BY 3.52 AT ROW 13.14 COL 160 WIDGET-ID 194
          FGCOLOR 9 FONT 30
     "> 20" VIEW-AS TEXT
          SIZE 20 BY 3.52 AT ROW 21.24 COL 160 WIDGET-ID 188
          FONT 30
     RECT-1 AT ROW 8.62 COL 2 WIDGET-ID 222
     RECT-3 AT ROW 8.62 COL 77 WIDGET-ID 142
     RECT-4 AT ROW 8.62 COL 151 WIDGET-ID 180
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 273.2 BY 34.48 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Embaladores"
         HEIGHT             = 34.52
         WIDTH              = 226.8
         MAX-HEIGHT         = 46.52
         MAX-WIDTH          = 364.8
         VIRTUAL-HEIGHT     = 46.52
         VIRTUAL-WIDTH      = 364.8
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       F-EMBALADOR:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MAY-21 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MAY-21:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MAY-22 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MAY-22:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MAY20 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MAY20:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MAY20E IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MAY20E:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MAY20E-2 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MAY20E-2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MAY20E-3 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MAY20E-3:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN-11 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN-11:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN-12 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN-12:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN-21 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN-21:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN-22 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN-22:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN10 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN10:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN10E IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN10E:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN10E-2 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN10E-2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN10E-3 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN10E-3:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN20 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN20:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN20E IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN20E:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN20E-2 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN20E-2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-MEN20E-3 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-MEN20E-3:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-STOTAL IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-STOTAL:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-STOTAL-2 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-STOTAL-2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-STOTAL-3 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-STOTAL-3:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-STOTALE IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-STOTALE:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-STOTALE-2 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-STOTALE-2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-STOTALE-3 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-STOTALE-3:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       F-SUC:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-TOTAL IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-TOTAL:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-TOTAL-2 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-TOTAL-2:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN F-TOTAL-3 IN FRAME fMain
   ALIGN-R                                                              */
ASSIGN 
       F-TOTAL-3:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fdt1:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fdt1-2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fdt1-3:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fdt2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fdt2-2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fdt2-3:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FTARJETAS:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN FTEXTO IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FTEXTO:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Embaladores */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Embaladores */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}
    /*
ON 'ANY-KEY':U OF WWIN ANYWHERE
DO:
    APPLY 'CHOOSE' TO BUTTON-1 IN FRAME {&FRAME-NAME}.
    RETURN.
END.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addContadorTarjetasLeidas wWin 
PROCEDURE addContadorTarjetasLeidas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FTARJETAS:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING (INTEGER(FTARJETAS:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /*{adm2/support/changePage.i}.*/  
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject wWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY fdt1 F-EMBALADOR F-SUC F-MEN10E F-MEN20E F-MAY20E F-STOTALE fdt2 
          F-TOTAL F-MEN10 F-MEN20 F-MAY20 F-STOTAL fdt1-2 F-MEN10E-2 F-MEN20E-2 
          F-MAY20E-2 F-STOTALE-2 fdt2-2 F-TOTAL-2 F-MEN-11 F-MEN-21 F-MAY-21 
          F-STOTAL-2 fdt1-3 F-MEN10E-3 F-MEN20E-3 F-MAY20E-3 F-STOTALE-3 fdt2-3 
          FTARJETAS F-TOTAL-3 F-MEN-12 F-MEN-22 F-MAY-22 F-STOTAL-3 FTEXTO 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-3 RECT-4 fdt1 F-EMBALADOR F-SUC F-MEN10E F-MEN20E F-MAY20E 
         F-STOTALE fdt2 F-TOTAL F-MEN10 F-MEN20 F-MAY20 F-STOTAL fdt1-2 
         F-MEN10E-2 F-MEN20E-2 F-MAY20E-2 F-STOTALE-2 fdt2-2 F-TOTAL-2 F-MEN-11 
         F-MEN-21 F-MAY-21 F-STOTAL-2 fdt1-3 F-MEN10E-3 F-MEN20E-3 F-MAY20E-3 
         F-STOTALE-3 fdt2-3 FTARJETAS F-TOTAL-3 F-MEN-12 F-MEN-22 F-MAY-22 
         F-STOTAL-3 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQuery wWin 
PROCEDURE getQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER queryText AS CHARACTER NO-UNDO.
    queryText = queryText1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSort wWin 
PROCEDURE getSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER querySort AS CHARACTER NO-UNDO.
    querySort = querySort1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inicializaPantalla wWin 
PROCEDURE inicializaPantalla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VAR cnada AS CHARACTER NO-UNDO INITIAL "0".

   F-MEN10E:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN20E:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN20:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MAY20E:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MAY20:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".

   F-STOTALE:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-STOTAL:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".

   
   F-TOTAL:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".


   FDT1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   FDT2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".



   F-MEN10E-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN-11:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN20E-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN-21:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MAY20E:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MAY-21:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".

   F-STOTALE-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-STOTAL-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".

   
   F-TOTAL-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".


   FDT1-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   FDT2-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

   F-MEN10E-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN-12:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN20E-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MEN-22:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MAY20E-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-MAY-22:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".

   F-STOTALE-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
   F-STOTAL-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".

   
   F-TOTAL-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".


   FDT1-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   FDT2-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VAR CEMBALADOR AS CHARACTER NO-UNDO FORMAT '9999999999'.

  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN P_LIBEMBALADORES.P PERSISTENT  SET HLIB.
  
/*  
  RUN PCONSULTAEMBALADORES.P PERSISTENT  SET HP.
  
  
  SUBSCRIBE TO 'MUESTRAEMBALADOR' IN HP. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    FTARJETAS:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
 
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE muestraembalador wWin 
PROCEDURE muestraembalador :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER PCEMBALADOR AS CHARACTER FORMAT '9999999999' NO-UNDO.
    
    
    DEFINE VAR ICAJMEN10E    AS INTEGER NO-UNDO.
    DEFINE VAR ICAJMEN10     AS INTEGER NO-UNDO.
    DEFINE VAR ICAJMEN20E    AS INTEGER NO-UNDO.
    DEFINE VAR ICAJMEN20     AS INTEGER NO-UNDO.
    DEFINE VAR ICAJMAY20E    AS INTEGER NO-UNDO.
    DEFINE VAR ICAJMAY20     AS INTEGER NO-UNDO.
    DEFINE VAR ICAJTOT       AS INTEGER NO-UNDO.
    
    DEFINE VAR RESP          AS LOGICAL NO-UNDO.

    DEFINE VAR DT1           AS DATETIME NO-UNDO.
    DEFINE VAR DT2           AS DATETIME NO-UNDO.
        
    DEFINE VAR DFECHA        AS DATE NO-UNDO.
    DEFINE VAR HOUR          AS INTEGER NO-UNDO.
    
    DEFINE VAR I             AS INTEGER NO-UNDO.    
    DEFINE VAR J             AS INTEGER EXTENT 9.

    F-EMBALADOR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = EMBALADOR(PCEMBALADOR) + ' - '  + PCEMBALADOR.

    DT1 = NOW.      
    
    DO I = 1 TO 3:
    
        FIND LAST CAJAS WHERE CREACION < DT1 AND CAJAS.ID_EMBALADOR = INTEGER(PCEMBALADOR)  NO-LOCK NO-ERROR.
    
        IF NOT AVAILABLE CAJAS THEN LEAVE.
    
    
        DFECHA = cajas.fecha_operativa.
        DT1 = DATETIME( MONTH(DFECHA) , DAY(DFECHA) ,  YEAR (DFECHA)  , 6  , 00).
        DT2 = DATETIME( MONTH(DFECHA + 1) , DAY(DFECHA + 1) ,  YEAR (DFECHA + 1)  , 5  , 59 , 59).
    
    
    
    
        RUN CAJASPOREMBALADOR IN hlib  (  INTEGER(F-SUC:SCREEN-VALUE IN FRAME {&FRAME-NAME}) , 
                                    0 , 
                                    INTEGER(PCEMBALADOR) , 
                                    DT1,
                                    DT2,
                                    OUTPUT ICAJMEN10E ,
                                    OUTPUT ICAJMEN10 ,
                                    OUTPUT ICAJMEN20E ,
                                    OUTPUT ICAJMEN20 ,
                                    OUTPUT ICAJMAY20E ,
                                    OUTPUT ICAJMAY20 ,
                                    OUTPUT ICAJTOT ).
                                
                                
        CASE I.
            WHEN 1 THEN DO:
                F-MEN10E:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN10E).
                F-MEN10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN10).
                F-MEN20E:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN20E).
                F-MEN20:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN20).
                F-MAY20E:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMAY20E).
                F-MAY20:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMAY20).
             
                F-STOTALE:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                     STRING(ICAJMEN10E + ICAJMEN20E + ICAJMAY20E).
                F-STOTAL:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                             STRING(ICAJMEN10 + ICAJMEN20 + ICAJMAY20).
             
                
                F-TOTAL:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJTOT).
             
             
                FDT1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DT1, "99/99/99 HH:MM:SS").
                FDT2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DT2, "99/99/99 HH:MM:SS").            
            END.
            WHEN 2 THEN DO:
                F-MEN10E-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN10E).
                F-MEN-11:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN10).
                F-MEN20E-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN20E).
                F-MEN-21:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN20).
                F-MAY20E-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMAY20E).
                F-MAY-21:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMAY20).
             
                F-STOTALE-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                     STRING(ICAJMEN10E + ICAJMEN20E + ICAJMAY20E).
                F-STOTAL-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                             STRING(ICAJMEN10 + ICAJMEN20 + ICAJMAY20).
             
                
                F-TOTAL-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJTOT).
             
             
                FDT1-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DT1, "99/99/99 HH:MM:SS").
                FDT2-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DT2, "99/99/99 HH:MM:SS").                        
            END.
            WHEN 3 THEN DO:
                F-MEN10E-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN10E).
                F-MEN-12:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN10).
                F-MEN20E-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN20E).
                F-MEN-22:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMEN20).
                F-MAY20E-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMAY20E).
                F-MAY-22:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJMAY20).
             
                F-STOTALE-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                     STRING(ICAJMEN10E + ICAJMEN20E + ICAJMAY20E).
                F-STOTAL-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                             STRING(ICAJMEN10 + ICAJMEN20 + ICAJMAY20).
             
                
                F-TOTAL-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ICAJTOT).
             
             
                FDT1-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DT1, "99/99/99 HH:MM:SS").
                FDT2-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DT2, "99/99/99 HH:MM:SS").                        
            END.
        END CASE.
                                


               
       
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE muestraEstado wWin 
PROCEDURE muestraEstado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER PCMENSAJE AS CHARACTER NO-UNDO.

  FTEXTO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = PCMENSAJE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSort wWin 
PROCEDURE setSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xSort AS CHARACTER NO-UNDO.

querySort1 = xSort.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVariables wWin 
PROCEDURE setVariables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pisucursal AS INTEGER.
DEFINE INPUT PARAMETER pcembalador AS CHARACTER.

  F-SUC:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pisucursal).

  cembalador = pcembalador.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject wWin 
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION embalador wWin 
FUNCTION embalador RETURNS CHARACTER
  ( INPUT CEMBALADOR AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND EMBALADORES WHERE EMBALADORES.ID_EMBALADOR = INTEGER(CEMBALADOR) NO-LOCK NO-ERROR.
  IF AVAILABLE EMBALADORES THEN
    RETURN EMBALADORES.NOMBRE.

  RETURN "SIN NOMBRE " + CEMBALADOR.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION queryName wWin 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

