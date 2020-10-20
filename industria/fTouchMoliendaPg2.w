&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
DEFINE VARIABLE chSil-1 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-2 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-3 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-4 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-5 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-6 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-7 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-8 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-9 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-10 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-11 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-12 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-13 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-14 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-15 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-16 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-17 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-18 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chSil-19 AS COM-HANDLE     NO-UNDO.

DEFINE VARIABLE chCurrSil   AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE hCurrWidget AS HANDLE     NO-UNDO.
DEFINE VARIABLE iLinea      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTurno      AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnNum-7 btnNum-8 fiInicioHr fiInicioMin ~
edtSil-1 edtSil-2 edtSil-3 edtSil-4 fiFinHr fiFinMin btnNum-9 fiKilos ~
fiKilosDescarte edtSil-5 edtSil-6 edtSil-7 edtSil-8 btnClear btnLimpiar ~
btnSave edtSil-9 edtSil-10 edtSil-11 edtSil-12 btnVolver btnNum-4 btnNum-5 ~
btnNum-6 edtSil-13 edtSil-14 edtSil-15 edtSil-16 btnNum-1 btnNum-2 btnNum-3 ~
btnCorregir btnNum-0 btnEnter edtSil-17 edtSil-18 edtSil-19 fiLabel RECT-1 ~
RECT-2 
&Scoped-Define DISPLAYED-OBJECTS fiSilo fiInicioHr fiInicioMin edtSil-1 ~
edtSil-2 edtSil-3 edtSil-4 fiFinHr fiFinMin fiKilos fiKilosDescarte ~
edtSil-5 edtSil-6 edtSil-7 edtSil-8 edtSil-9 edtSil-10 edtSil-11 edtSil-12 ~
edtSil-13 edtSil-14 edtSil-15 edtSil-16 edtSil-17 edtSil-18 edtSil-19 ~
fiLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextOcxHandle fFrameWin 
FUNCTION getNextOcxHandle RETURNS COM-HANDLE
  (piCurrSil AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWidgetHandle fFrameWin 
FUNCTION getWidgetHandle RETURNS HANDLE
  (pcName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame-1 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-1 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-10 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-10 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-11 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-11 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-12 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-12 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-13 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-13 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-14 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-14 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-15 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-15 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-16 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-16 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-17 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-17 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-18 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-18 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-20 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-20 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-21 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-21 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-4 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-4 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-5 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-5 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-6 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-6 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-7 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-7 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-8 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-8 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-9 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-9 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClear 
     IMAGE-UP FILE "src/adm2/image/botones/btnborrar.gif":U
     LABEL "Borrar" 
     SIZE 21 BY 3.57
     FONT 25.

DEFINE BUTTON btnCorregir 
     IMAGE-UP FILE "src/adm2/image/botones/btnsupr.gif":U
     LABEL "Supr" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnEnter 
     IMAGE-UP FILE "src/adm2/image/botones/btnenter.gif":U
     LABEL "Enter" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnLimpiar 
     IMAGE-UP FILE "src/adm2/image/botones/btnlimpiar.gif":U
     LABEL "Limpiar" 
     SIZE 21 BY 3.57
     BGCOLOR 12 FONT 25.

DEFINE BUTTON btnNum-0 
     LABEL "0" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-1 
     LABEL "1" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-2 
     LABEL "2" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-3 
     LABEL "3" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-4 
     LABEL "4" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-5 
     LABEL "5" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-6 
     LABEL "6" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-7  NO-FOCUS
     LABEL "7" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-8  NO-FOCUS
     LABEL "8" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-9  NO-FOCUS
     LABEL "9" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "src/adm2/image/botones/btngrabar.gif":U
     LABEL "Grabar" 
     SIZE 21 BY 3.57
     FONT 25.

DEFINE BUTTON btnVolver 
     IMAGE-UP FILE "src/adm2/image/botones/btnvolver.gif":U NO-FOCUS
     LABEL "Volver" 
     SIZE 21 BY 3.57
     FONT 25.

DEFINE VARIABLE edtSil-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-10 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-11 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-12 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-13 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-14 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-15 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-16 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-17 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-18 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-19 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-2 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-3 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-4 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-5 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-6 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-7 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-8 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE edtSil-9 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 25 BY 2.62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFinHr AS CHARACTER FORMAT "X(2)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiFinMin AS CHARACTER FORMAT "X(2)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiInicioHr AS CHARACTER FORMAT "X(2)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiInicioMin AS CHARACTER FORMAT "X(2)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiKilos AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiKilosDescarte AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Kgs Descarte:" 
      VIEW-AS TEXT 
     SIZE 41 BY 1.76
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiSilo AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.91
     BGCOLOR 12 FGCOLOR 15 FONT 25 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 15.24.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 18.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnNum-7 AT ROW 16.71 COL 131
     btnNum-8 AT ROW 16.71 COL 152
     fiSilo AT ROW 1.24 COL 187.8 COLON-ALIGNED NO-LABEL
     fiInicioHr AT ROW 3.38 COL 166.8 COLON-ALIGNED HELP
          "ctrlHora" NO-LABEL
     fiInicioMin AT ROW 3.38 COL 187.8 COLON-ALIGNED HELP
          "ctrlMin" NO-LABEL
     edtSil-1 AT ROW 4.81 COL 2 NO-LABEL
     edtSil-2 AT ROW 4.81 COL 30 NO-LABEL
     edtSil-3 AT ROW 4.81 COL 57 NO-LABEL
     edtSil-4 AT ROW 4.81 COL 85 NO-LABEL
     fiFinHr AT ROW 5.52 COL 166.8 COLON-ALIGNED HELP
          "ctrlHora" NO-LABEL
     fiFinMin AT ROW 5.52 COL 187.8 COLON-ALIGNED HELP
          "ctrlMin" NO-LABEL
     btnNum-9 AT ROW 16.71 COL 173
     fiKilos AT ROW 7.67 COL 166.8 COLON-ALIGNED NO-LABEL
     fiKilosDescarte AT ROW 9.81 COL 166.8 COLON-ALIGNED NO-LABEL
     edtSil-5 AT ROW 11.24 COL 2 NO-LABEL
     edtSil-6 AT ROW 11.24 COL 30 NO-LABEL
     edtSil-7 AT ROW 11.24 COL 57 NO-LABEL
     edtSil-8 AT ROW 11.24 COL 85 NO-LABEL
     btnClear AT ROW 12.19 COL 136.6
     btnLimpiar AT ROW 12.19 COL 159.4
     btnSave AT ROW 12.19 COL 182
     edtSil-9 AT ROW 17.67 COL 2 NO-LABEL
     edtSil-10 AT ROW 17.67 COL 30 NO-LABEL
     edtSil-11 AT ROW 17.67 COL 57 NO-LABEL
     edtSil-12 AT ROW 17.67 COL 85 NO-LABEL
     btnVolver AT ROW 12.19 COL 114
     btnNum-4 AT ROW 21 COL 131
     btnNum-5 AT ROW 21 COL 152
     btnNum-6 AT ROW 21 COL 173
     edtSil-13 AT ROW 24.1 COL 2 NO-LABEL
     edtSil-14 AT ROW 24.1 COL 30 NO-LABEL
     edtSil-15 AT ROW 24.1 COL 57 NO-LABEL
     edtSil-16 AT ROW 24.1 COL 85 NO-LABEL
     btnNum-1 AT ROW 25.29 COL 131
     btnNum-2 AT ROW 25.29 COL 152
     btnNum-3 AT ROW 25.29 COL 173
     btnCorregir AT ROW 29.57 COL 131
     btnNum-0 AT ROW 29.57 COL 152
     btnEnter AT ROW 29.57 COL 173
     edtSil-17 AT ROW 30.52 COL 2 NO-LABEL
     edtSil-18 AT ROW 30.52 COL 30 NO-LABEL
     edtSil-19 AT ROW 30.52 COL 85 NO-LABEL
     fiLabel AT ROW 9.81 COL 123 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 113
     RECT-2 AT ROW 16.24 COL 113
     "1" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 1.24 COL 2
          FONT 25
     "Termina:" VIEW-AS TEXT
          SIZE 34 BY 1.43 AT ROW 5.76 COL 124
          FONT 25
     "12" VIEW-AS TEXT
          SIZE 7 BY 1.43 AT ROW 14.1 COL 84
          FONT 25
     "17" VIEW-AS TEXT
          SIZE 7 BY 1.43 AT ROW 26.95 COL 1
          FONT 25
     "18" VIEW-AS TEXT
          SIZE 7 BY 1.43 AT ROW 26.95 COL 29
          FONT 25
     "Inicio:" VIEW-AS TEXT
          SIZE 34 BY 1.43 AT ROW 3.62 COL 124
          FONT 25
     "6" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 7.67 COL 30
          FONT 25
     ":" VIEW-AS TEXT
          SIZE 3 BY 1.43 AT ROW 5.76 COL 184.8
          FONT 25
     "13" VIEW-AS TEXT
          SIZE 7 BY 1.43 AT ROW 20.52 COL 1
          FONT 25
     "8" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 7.67 COL 85
          FONT 25
     "16" VIEW-AS TEXT
          SIZE 7 BY 1.43 AT ROW 20.52 COL 84
          FONT 25
     "Directa" VIEW-AS TEXT
          SIZE 27 BY 1.43 AT ROW 28.38 COL 63
          FONT 25
     "11" VIEW-AS TEXT
          SIZE 6 BY 1.43 AT ROW 14.1 COL 56
          FONT 25
     "4" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 1.24 COL 85
          FONT 25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203 BY 33.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     "2" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 1.24 COL 30
          FONT 25
     "10" VIEW-AS TEXT
          SIZE 7 BY 1.43 AT ROW 14.1 COL 29
          FONT 25
     "Kgs:" VIEW-AS TEXT
          SIZE 14 BY 1.67 AT ROW 7.91 COL 125
          FONT 25
     ":" VIEW-AS TEXT
          SIZE 3 BY 1.43 AT ROW 3.62 COL 184.8
          FONT 25
     "Molienda" VIEW-AS TEXT
          SIZE 27 BY 1.43 AT ROW 26.71 COL 63
          FONT 25
     "15" VIEW-AS TEXT
          SIZE 7 BY 1.43 AT ROW 20.52 COL 56
          FONT 25
     "Cajonera:" VIEW-AS TEXT
          SIZE 29 BY 1.67 AT ROW 1.38 COL 159
          FONT 25
     "5" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 7.67 COL 2
          FONT 25
     "3" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 1.24 COL 57
          FONT 25
     "14" VIEW-AS TEXT
          SIZE 7 BY 1.43 AT ROW 20.52 COL 29
          FONT 25
     "7" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 7.67 COL 57
          FONT 25
     "9" VIEW-AS TEXT
          SIZE 5 BY 1.43 AT ROW 14.1 COL 2
          FONT 25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203 BY 33.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 33.33
         WIDTH              = 203.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

ASSIGN 
       edtSil-1:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-1:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-10:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-10:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-11:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-11:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-12:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-12:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-13:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-13:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-14:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-14:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-15:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-15:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-16:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-16:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-17:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-17:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-18:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-18:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-19:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-19:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-2:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-3:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-3:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-4:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-4:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-5:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-5:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-6:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-6:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-7:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-7:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-8:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-8:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       edtSil-9:RETURN-INSERTED IN FRAME fMain  = TRUE
       edtSil-9:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiFinHr:PRIVATE-DATA IN FRAME fMain     = 
                "fiFinMin".

ASSIGN 
       fiFinMin:PRIVATE-DATA IN FRAME fMain     = 
                "fiKilos".

ASSIGN 
       fiInicioHr:PRIVATE-DATA IN FRAME fMain     = 
                "fiInicioMin".

ASSIGN 
       fiInicioMin:PRIVATE-DATA IN FRAME fMain     = 
                "fiFinHr".

ASSIGN 
       fiKilos:PRIVATE-DATA IN FRAME fMain     = 
                "fiKilosDescarte".

/* SETTINGS FOR FILL-IN fiSilo IN FRAME fMain
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-1 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.24
       COLUMN          = 8
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.24
       COLUMN          = 36
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.24
       COLUMN          = 63
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-4 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.24
       COLUMN          = 91
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-5 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 7.67
       COLUMN          = 8
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-6 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 7.67
       COLUMN          = 36
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-7 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 7.67
       COLUMN          = 63
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-8 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 7.67
       COLUMN          = 91
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-9 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 14.1
       COLUMN          = 8
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-10 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 14.1
       COLUMN          = 36
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-11 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 14.1
       COLUMN          = 63
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-12 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 14.1
       COLUMN          = 91
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-13 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 20.52
       COLUMN          = 8
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-14 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 20.52
       COLUMN          = 36
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-15 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 20.52
       COLUMN          = 63
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-16 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 20.52
       COLUMN          = 91
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-17 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 26.95
       COLUMN          = 8
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-18 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 26.95
       COLUMN          = 36
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-21 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 26.95
       COLUMN          = 91
       HEIGHT          = 3.57
       WIDTH           = 19
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-20 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 27.67
       COLUMN          = 93
       HEIGHT          = 2.38
       WIDTH           = 12
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame-1:NAME = "CtrlFrame-1":U .
/* CtrlFrame-1 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-4:NAME = "CtrlFrame-4":U .
/* CtrlFrame-4 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-5:NAME = "CtrlFrame-5":U .
/* CtrlFrame-5 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-6:NAME = "CtrlFrame-6":U .
/* CtrlFrame-6 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-7:NAME = "CtrlFrame-7":U .
/* CtrlFrame-7 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-8:NAME = "CtrlFrame-8":U .
/* CtrlFrame-8 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-9:NAME = "CtrlFrame-9":U .
/* CtrlFrame-9 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-10:NAME = "CtrlFrame-10":U .
/* CtrlFrame-10 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-11:NAME = "CtrlFrame-11":U .
/* CtrlFrame-11 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-12:NAME = "CtrlFrame-12":U .
/* CtrlFrame-12 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-13:NAME = "CtrlFrame-13":U .
/* CtrlFrame-13 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-14:NAME = "CtrlFrame-14":U .
/* CtrlFrame-14 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-15:NAME = "CtrlFrame-15":U .
/* CtrlFrame-15 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-16:NAME = "CtrlFrame-16":U .
/* CtrlFrame-16 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-17:NAME = "CtrlFrame-17":U .
/* CtrlFrame-17 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-18:NAME = "CtrlFrame-18":U .
/* CtrlFrame-18 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-21:NAME = "CtrlFrame-21":U .
/* CtrlFrame-21 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-20:NAME = "CtrlFrame-20":U .
/* CtrlFrame-20 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-2:MOVE-AFTER(CtrlFrame-1).
      CtrlFrame-3:MOVE-AFTER(CtrlFrame-2).
      CtrlFrame-4:MOVE-AFTER(CtrlFrame-3).
      CtrlFrame-5:MOVE-AFTER(fiFinMin:HANDLE IN FRAME fMain).
      CtrlFrame-6:MOVE-AFTER(CtrlFrame-5).
      CtrlFrame-7:MOVE-AFTER(CtrlFrame-6).
      CtrlFrame-8:MOVE-AFTER(CtrlFrame-7).
      CtrlFrame-9:MOVE-AFTER(btnSave:HANDLE IN FRAME fMain).
      CtrlFrame-10:MOVE-AFTER(CtrlFrame-9).
      CtrlFrame-11:MOVE-AFTER(CtrlFrame-10).
      CtrlFrame-12:MOVE-AFTER(CtrlFrame-11).
      CtrlFrame-13:MOVE-AFTER(edtSil-12:HANDLE IN FRAME fMain).
      CtrlFrame-14:MOVE-AFTER(CtrlFrame-13).
      CtrlFrame-15:MOVE-AFTER(CtrlFrame-14).
      CtrlFrame-16:MOVE-AFTER(CtrlFrame-15).
      CtrlFrame-17:MOVE-AFTER(btnNum-3:HANDLE IN FRAME fMain).
      CtrlFrame-18:MOVE-AFTER(CtrlFrame-17).
      CtrlFrame-21:MOVE-AFTER(CtrlFrame-18).
      CtrlFrame-20:MOVE-AFTER(CtrlFrame-21).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear fFrameWin
ON CHOOSE OF btnClear IN FRAME fMain /* Borrar */
DO:
  DEFINE VARIABLE hOcx AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE hEdt AS WIDGET-HANDLE     NO-UNDO.
  DEFINE VARIABLE cOk  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lOk  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE hCnt AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cCod AS CHARACTER  NO-UNDO.

  hCnt = DYNAMIC-FUNCTION('getContainerSource').

  RUN wdTouchInputCod.w (DYNAMIC-FUNCTION('getTurno' IN hCnt),
                         DYNAMIC-FUNCTION('getFecha' IN hCnt),
                         OUTPUT cOk).
  RUN maximizeWindow IN hCnt.
  IF cOk = "" THEN RETURN.
  RUN wdTouchMsgBox.w ("Borrado de Registro", "Esta Seguro que desea eliminar la informacion?", OUTPUT lOk).
  RUN maximizeWindow IN hCnt.
  IF NOT lOk THEN RETURN.

  RUN deleteMolienda.
  RUN clearFillIns.

  hOcx = getNextOcxHandle(INTEGER(fiSilo:SCREEN-VALUE) - 1).
  hOcx:DiscreteValue1 = FALSE.
  hEdt = getWidgetHandle("edtSil-" + fiSIlo:SCREEN-VALUE).
  hEdt:SCREEN-VALUE = "".

  RUN refreshTotales.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCorregir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCorregir fFrameWin
ON CHOOSE OF btnCorregir IN FRAME fMain /* Supr */
DO:
  hCurrWidget:SCREEN-VALUE = SUBSTRING(hCurrWidget:SCREEN-VALUE, 1, LENGTH(hCurrWidget:SCREEN-VALUE) - 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEnter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEnter fFrameWin
ON CHOOSE OF btnEnter IN FRAME fMain /* Enter */
DO:
  DEFINE VARIABLE hNextWidget AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib        AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cChk        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lChk        AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.

  IF hCurrWidget:HELP <> "" AND hCurrWidget:HELP <> ? THEN 
    cChk = DYNAMIC-FUNCTION(hCurrWidget:HELP IN hLib, hCurrWidget:SCREEN-VALUE).

  IF cChk <> "" THEN DO:
    RUN wdTouchMsgBox.w (cChk, "", OUTPUT lChk).
    RUN maximizeContainer.
    hCurrWidget = getWidgetHandle(hCurrWidget:NAME).
    APPLY "ENTRY" TO hCurrWidget.
    RETURN NO-APPLY.
  END.
  

  hCurrWidget = getWidgetHandle(hCurrWidget:PRIVATE-DATA).
  APPLY "ENTRY" TO hCurrWidget .
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLimpiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLimpiar fFrameWin
ON CHOOSE OF btnLimpiar IN FRAME fMain /* Limpiar */
DO:
  RUN clearFillIns.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-0 fFrameWin
ON CHOOSE OF btnNum-0 IN FRAME fMain /* 0 */
DO:
  RUN putNumber(0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-1 fFrameWin
ON CHOOSE OF btnNum-1 IN FRAME fMain /* 1 */
DO:
  RUN putNumber(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-2 fFrameWin
ON CHOOSE OF btnNum-2 IN FRAME fMain /* 2 */
DO:
  RUN putNumber(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-3 fFrameWin
ON CHOOSE OF btnNum-3 IN FRAME fMain /* 3 */
DO:
  RUN putNumber(3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-4 fFrameWin
ON CHOOSE OF btnNum-4 IN FRAME fMain /* 4 */
DO:
  RUN putNumber(4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-5 fFrameWin
ON CHOOSE OF btnNum-5 IN FRAME fMain /* 5 */
DO:
  RUN putNumber(5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-6 fFrameWin
ON CHOOSE OF btnNum-6 IN FRAME fMain /* 6 */
DO:
  RUN putNumber(6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-7 fFrameWin
ON CHOOSE OF btnNum-7 IN FRAME fMain /* 7 */
DO:
  RUN putNumber(7).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-8 fFrameWin
ON CHOOSE OF btnNum-8 IN FRAME fMain /* 8 */
DO:
  RUN putNumber(8).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-9 fFrameWin
ON CHOOSE OF btnNum-9 IN FRAME fMain /* 9 */
DO:
  RUN putNumber(9).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave fFrameWin
ON CHOOSE OF btnSave IN FRAME fMain /* Grabar */
DO:
  DEFINE VARIABLE cLeg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNom AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTag AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cChk AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMsg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hCnt AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lOk  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE fDta AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hOcx AS COM-HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.

  /* traigo kilos camion desde el nro de pesada */
  IF fiSilo:SCREEN-VALUE = "19" THEN DO:
    /* cuando se trata de molienda directa tomo el nro pesada y traigo los kilos de la balanza */
    RUN wdTouchInputMolDta.w (INTEGER(fiKilosDescarte:SCREEN-VALUE), OUTPUT fDta).
    fiKilos:SCREEN-VALUE = STRING(fDta).
    RUN maximizeContainer.
    fiKilos:SENSITIVE = TRUE.
    IF fDta = 0 THEN RETURN.
  END.
    

  hCnt = DYNAMIC-FUNCTION('getContainerSource').

  cLeg = fiInicioHr:SCREEN-VALUE + ":" + fiInicioMin:SCREEN-VALUE + "-" + 
         fiFinHr:SCREEN-VALUE + ":" + fiFinMin:SCREEN-VALUE + ">" + 
         fiKilos:SCREEN-VALUE + " (" + 
         fiKilosDescarte:SCREEN-VALUE + ")".
  cTag = DYNAMIC-FUNCTION('getLinea' IN hCnt) + CHR(1) + 
         fiSilo:SCREEN-VALUE + CHR(1) + 
         fiInicioHr:SCREEN-VALUE + ":" + fiInicioMin:SCREEN-VALUE + CHR(1) + 
         fiFinHr:SCREEN-VALUE + ":" + fiFinMin:SCREEN-VALUE + CHR(1) + 
         fiKilos:SCREEN-VALUE + CHR(1) + 
         fiKilosDescarte:SCREEN-VALUE.
  cNom = "edtSil-" + fiSilo:SCREEN-VALUE.

  cChk = DYNAMIC-FUNCTION('ctrlKilosCajonera' IN hLib, fiKilos:SCREEN-VALUE).
  /*  */
  IF cChk <> "" AND fiSilo:SCREEN-VALUE <> "19" THEN DO:
    RUN wdTouchMsgBox.w (cChk, "La Informacion No se Registrara", OUTPUT lOk).
    RUN maximizeContainer.
    fiKilos:SCREEN-VALUE = "".
    APPLY "ENTRY" TO fiKilos.
    RETURN.
  END.

  RUN setWidgetValue(cNom, cLeg).
  RUN setOcxValue(INTEGER(fiSilo:SCREEN-VALUE), TRUE, cTag).
  RUN saveMolienda.
  RUN clearFillIns.
  IF fiSilo:SCREEN-VALUE = "19" THEN RETURN.
  hOcx = getNextOcxHandle(INTEGER(fiSilo:SCREEN-VALUE)).
  RUN setFillIns(INTEGER(fiSilo:SCREEN-VALUE) + 1, hOcx:Tag).
  RUN refreshTotales.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVolver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVolver fFrameWin
ON CHOOSE OF btnVolver IN FRAME fMain /* Volver */
DO:
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.

  {get ContainerSource hCont}.

  RUN switchPage IN hCont (0, "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-1 fFrameWin OCX.Click
PROCEDURE CtrlFrame-1.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  RUN setFillIns (1, chSil-1:Tag).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-10 fFrameWin OCX.Click
PROCEDURE CtrlFrame-10.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (10, chSil-10:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-11 fFrameWin OCX.Click
PROCEDURE CtrlFrame-11.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (11, chSil-11:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-12 fFrameWin OCX.Click
PROCEDURE CtrlFrame-12.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (12, chSil-12:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-13 fFrameWin OCX.Click
PROCEDURE CtrlFrame-13.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (13, chSil-13:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-14 fFrameWin OCX.Click
PROCEDURE CtrlFrame-14.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (14, chSil-14:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-15 fFrameWin OCX.Click
PROCEDURE CtrlFrame-15.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (15, chSil-15:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-16 fFrameWin OCX.Click
PROCEDURE CtrlFrame-16.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (16, chSil-16:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-17 fFrameWin OCX.Click
PROCEDURE CtrlFrame-17.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (17, chSil-17:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-18 fFrameWin OCX.Click
PROCEDURE CtrlFrame-18.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (18, chSil-18:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 fFrameWin OCX.Click
PROCEDURE CtrlFrame-2.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (2, chSil-2:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-20 fFrameWin OCX.Click
PROCEDURE CtrlFrame-20.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (19, chSil-19:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 fFrameWin OCX.Click
PROCEDURE CtrlFrame-3.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (3, chSil-3:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-4 fFrameWin OCX.Click
PROCEDURE CtrlFrame-4.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (4, chSil-4:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-5 fFrameWin OCX.Click
PROCEDURE CtrlFrame-5.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (5, chSil-5:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-6 fFrameWin OCX.Click
PROCEDURE CtrlFrame-6.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (6, chSil-6:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-7 fFrameWin OCX.Click
PROCEDURE CtrlFrame-7.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (7, chSil-7:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-8 fFrameWin OCX.Click
PROCEDURE CtrlFrame-8.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (8, chSil-8:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-9 fFrameWin OCX.Click
PROCEDURE CtrlFrame-9.SFStandard.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN setFillIns (9, chSil-9:Tag).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKilosDescarte
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearEditors fFrameWin 
PROCEDURE clearEditors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hWidget AS WIDGET-HANDLE     NO-UNDO.

  hWidget = FRAME {&FRAME-NAME}:FIRST-CHILD .
  hWidget = hWidget:FIRST-CHILD.
  DO WHILE VALID-HANDLE(hWidget):
    IF hWidget:TYPE = "EDITOR" THEN
      hWidget:SCREEN-VALUE = "".
    hWidget = hWidget:NEXT-SIBLING.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearFillIns fFrameWin 
PROCEDURE clearFillIns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  fiInicioHr:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = "".
  fiInicioMin:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = "".
  fiFinHr:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = "".
  fiFinMin:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = "".
  fiKilos:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = "".
  fiKilosDescarte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  hCurrWidget = fiInicioHr:HANDLE.
  APPLY "ENTRY" TO fiInicioHr IN FRAME {&FRAME-NAME} .
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearOcx fFrameWin 
PROCEDURE clearOcx :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hOcx AS COM-HANDLE     NO-UNDO.

  hOcx = chCtrlFrame-1:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-2:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-3:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-4:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-5:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-6:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-7:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-8:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-9:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-10:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-11:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-12:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-13:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-14:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-15:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-16:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-17:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-18:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".
  hOcx = chCtrlFrame-20:SFStandard.
  hOcx:DiscreteValue1 = FALSE.
  hOcx:Tag = "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load fFrameWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "fTouchMoliendaPg2.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame-1 = CtrlFrame-1:COM-HANDLE
    UIB_S = chCtrlFrame-1:LoadControls( OCXFile, "CtrlFrame-1":U)
    chCtrlFrame-10 = CtrlFrame-10:COM-HANDLE
    UIB_S = chCtrlFrame-10:LoadControls( OCXFile, "CtrlFrame-10":U)
    chCtrlFrame-11 = CtrlFrame-11:COM-HANDLE
    UIB_S = chCtrlFrame-11:LoadControls( OCXFile, "CtrlFrame-11":U)
    chCtrlFrame-12 = CtrlFrame-12:COM-HANDLE
    UIB_S = chCtrlFrame-12:LoadControls( OCXFile, "CtrlFrame-12":U)
    chCtrlFrame-13 = CtrlFrame-13:COM-HANDLE
    UIB_S = chCtrlFrame-13:LoadControls( OCXFile, "CtrlFrame-13":U)
    chCtrlFrame-14 = CtrlFrame-14:COM-HANDLE
    UIB_S = chCtrlFrame-14:LoadControls( OCXFile, "CtrlFrame-14":U)
    chCtrlFrame-15 = CtrlFrame-15:COM-HANDLE
    UIB_S = chCtrlFrame-15:LoadControls( OCXFile, "CtrlFrame-15":U)
    chCtrlFrame-16 = CtrlFrame-16:COM-HANDLE
    UIB_S = chCtrlFrame-16:LoadControls( OCXFile, "CtrlFrame-16":U)
    chCtrlFrame-17 = CtrlFrame-17:COM-HANDLE
    UIB_S = chCtrlFrame-17:LoadControls( OCXFile, "CtrlFrame-17":U)
    chCtrlFrame-18 = CtrlFrame-18:COM-HANDLE
    UIB_S = chCtrlFrame-18:LoadControls( OCXFile, "CtrlFrame-18":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    chCtrlFrame-20 = CtrlFrame-20:COM-HANDLE
    UIB_S = chCtrlFrame-20:LoadControls( OCXFile, "CtrlFrame-20":U)
    chCtrlFrame-21 = CtrlFrame-21:COM-HANDLE
    UIB_S = chCtrlFrame-21:LoadControls( OCXFile, "CtrlFrame-21":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
    chCtrlFrame-4 = CtrlFrame-4:COM-HANDLE
    UIB_S = chCtrlFrame-4:LoadControls( OCXFile, "CtrlFrame-4":U)
    chCtrlFrame-5 = CtrlFrame-5:COM-HANDLE
    UIB_S = chCtrlFrame-5:LoadControls( OCXFile, "CtrlFrame-5":U)
    chCtrlFrame-6 = CtrlFrame-6:COM-HANDLE
    UIB_S = chCtrlFrame-6:LoadControls( OCXFile, "CtrlFrame-6":U)
    chCtrlFrame-7 = CtrlFrame-7:COM-HANDLE
    UIB_S = chCtrlFrame-7:LoadControls( OCXFile, "CtrlFrame-7":U)
    chCtrlFrame-8 = CtrlFrame-8:COM-HANDLE
    UIB_S = chCtrlFrame-8:LoadControls( OCXFile, "CtrlFrame-8":U)
    chCtrlFrame-9 = CtrlFrame-9:COM-HANDLE
    UIB_S = chCtrlFrame-9:LoadControls( OCXFile, "CtrlFrame-9":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "fTouchMoliendaPg2.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteMolienda fFrameWin 
PROCEDURE deleteMolienda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCnt AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSil AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.

  hCnt = DYNAMIC-FUNCTION('getContainerSource').
  iSil = INTEGER(fiSilo:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  
  IF iSil = 19 THEN DO:
    iSil = IF DYNAMIC-FUNCTION('getLinea' IN hCnt) = "6" THEN 98 ELSE 99.
  END.
  ELSE
    iSil = IF DYNAMIC-FUNCTION('getLinea' IN hCnt) = "6" THEN iSil + 16 ELSE iSil.

  RUN deleteMoliendaSilo IN hLib (DYNAMIC-FUNCTION('getLinea' IN hCnt),
                                  DYNAMIC-FUNCTION('getTurno' IN hCnt),
                                  DYNAMIC-FUNCTION('getFecha' IN hCnt),
                                  iSil).


  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
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
  DISPLAY fiSilo fiInicioHr fiInicioMin edtSil-1 edtSil-2 edtSil-3 edtSil-4 
          fiFinHr fiFinMin fiKilos fiKilosDescarte edtSil-5 edtSil-6 edtSil-7 
          edtSil-8 edtSil-9 edtSil-10 edtSil-11 edtSil-12 edtSil-13 edtSil-14 
          edtSil-15 edtSil-16 edtSil-17 edtSil-18 edtSil-19 fiLabel 
      WITH FRAME fMain.
  ENABLE btnNum-7 btnNum-8 fiInicioHr fiInicioMin edtSil-1 edtSil-2 edtSil-3 
         edtSil-4 fiFinHr fiFinMin btnNum-9 fiKilos fiKilosDescarte edtSil-5 
         edtSil-6 edtSil-7 edtSil-8 btnClear btnLimpiar btnSave edtSil-9 
         edtSil-10 edtSil-11 edtSil-12 btnVolver btnNum-4 btnNum-5 btnNum-6 
         edtSil-13 edtSil-14 edtSil-15 edtSil-16 btnNum-1 btnNum-2 btnNum-3 
         btnCorregir btnNum-0 btnEnter edtSil-17 edtSil-18 edtSil-19 fiLabel 
         RECT-1 RECT-2 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  chSil-1 = chCtrlFrame-1:SFStandard.
  chSil-2 = chCtrlFrame-2:SFStandard.
  chSil-3 = chCtrlFrame-3:SFStandard.
  chSil-4 = chCtrlFrame-4:SFStandard.
  chSil-5 = chCtrlFrame-5:SFStandard.
  chSil-6 = chCtrlFrame-6:SFStandard.
  chSil-7 = chCtrlFrame-7:SFStandard.
  chSil-8 = chCtrlFrame-8:SFStandard.
  chSil-9 = chCtrlFrame-9:SFStandard.
  chSil-10 = chCtrlFrame-10:SFStandard.
  chSil-11 = chCtrlFrame-11:SFStandard.
  chSil-12 = chCtrlFrame-12:SFStandard.
  chSil-13 = chCtrlFrame-13:SFStandard.
  chSil-14 = chCtrlFrame-14:SFStandard.
  chSil-15 = chCtrlFrame-15:SFStandard.
  chSil-16 = chCtrlFrame-16:SFStandard.
  chSil-17 = chCtrlFrame-17:SFStandard.
  chSil-18 = chCtrlFrame-18:SFStandard.
  chSil-19 = chCtrlFrame-20:SFStandard.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadSilos fFrameWin 
PROCEDURE loadSilos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcStreamSilos AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSil AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLeg AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNam AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hCnt AS HANDLE     NO-UNDO.

  RUN clearEditors.
  RUN clearOcx.

  /* deshabilito recursos linea l */
  hCnt = DYNAMIC-FUNCTION('getContainerSource').
  IF DYNAMIC-FUNCTION('getLinea' IN hCnt) = 5 THEN DO:
    fiKilosDescarte:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
    chSil-17:VISIBLE = FALSE.
    chSil-18:VISIBLE = FALSE.
  END.

  IF DYNAMIC-FUNCTION('getLinea' IN hCnt) = 6 THEN DO:
    fiKilosDescarte:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
    chSil-17:VISIBLE = TRUE.
    chSil-18:VISIBLE = TRUE.
  END.

  DO i = 1 TO NUM-ENTRIES(pcStreamSilos, CHR(10)):
    cRow = ENTRY(i, pcStreamSilos, CHR(10)).
    cLeg = ENTRY(3, cRow, CHR(1)) + "-" +
           ENTRY(4, cRow, CHR(1)) + ">" +
           ENTRY(5, cRow, CHR(1)) + " (" + 
           ENTRY(6, cRow, CHR(1)) + ")".
    iLin = INTEGER(ENTRY(1, cRow, CHR(1))).
    iSil = INTEGER(ENTRY(2, cRow, CHR(1))).

    IF iLin = 5 THEN DO:
      IF iSil = 99 THEN iSil = 19.
      cNam = "edtSil-" + STRING(iSil).
    END.

    IF iLin = 6 THEN DO:
      IF iSil = 98 THEN 
        iSil = 19.
      ELSE 
        iSil = iSil - 16.

      cNam = "edtSil-" + STRING(iSil).
      chSil-17:VISIBLE = TRUE.
      chSil-18:VISIBLE = TRUE.
      fiKilosDescarte:SENSITIVE = TRUE.
    END.

    RUN setWidgetValue (cNam, cLeg).
    RUN setOcxValue (iSil, TRUE, cRow).
  
  END.

  RUN refreshTotales.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE maximizeContainer fFrameWin 
PROCEDURE maximizeContainer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.

  hCont = DYNAMIC-FUNCTION('getContainerSource').

  RUN maximizeWindow IN hCont.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putNumber fFrameWin 
PROCEDURE putNumber :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piNum AS INTEGER    NO-UNDO.
  
  
  IF fiSilo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
    MESSAGE "Primero Seleccione un Silo"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  
  hCurrWidget:SCREEN-VALUE = hCurrWidget:SCREEN-VALUE + STRING(piNum).
  BELL.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshTotales fFrameWin 
PROCEDURE refreshTotales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hCnt AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cTot AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.
/*
  hCnt = DYNAMIC-FUNCTION('getContainerSource').
  cTot = DYNAMIC-FUNCTION('getTotalesTurno' IN hLib, DYNAMIC-FUNCTION('getLinea' IN hCnt),
                                                     DYNAMIC-FUNCTION('getFecha' IN hCnt),
                                                     DYNAMIC-FUNCTION('getTurno' IN hCnt)).

  fiTotMolido:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ENTRY(1, cTot, CHR(1)).
  fiTotDescarte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2, cTot, CHR(1)).
  fiTotMolienda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(3, cTot, CHR(1)).
  
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveMolienda fFrameWin 
PROCEDURE saveMolienda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCnt AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSil AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cIni AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFin AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cChk AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iIni AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFin AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lOk  AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.

  hCnt = DYNAMIC-FUNCTION('getContainerSource').
  cIni = fiInicioHr:SCREEN-VALUE IN FRAME {&FRAME-NAME} + ":" + fiInicioMin:SCREEN-VALUE.
  cFin = fiFinHr:SCREEN-VALUE + ":" + fiFinMin:SCREEN-VALUE.
  iIni = (IF INTEGER(fiInicioHr:SCREEN-VALUE) = 0 THEN 1 ELSE INTEGER(fiInicioHr:SCREEN-VALUE)) * 60 * 60. + INTEGER(fiInicioMin:SCREEN-VALUE) * 60.
  iFin = (IF INTEGER(fiFinHr:SCREEN-VALUE) = 0 THEN 1 ELSE INTEGER(fiFinHr:SCREEN-VALUE)) * 60 * 60. + INTEGER(fiFinMin:SCREEN-VALUE) * 60.

  IF fiSilo:SCREEN-VALUE = "19" THEN 
    ASSIGN iSil = IF DYNAMIC-FUNCTION('getLinea' IN hCnt) = "5" THEN 99 ELSE 98
           iNro = DECIMAL(fiKilosDescarte:SCREEN-VALUE)
           fiKilosDescarte:SCREEN-VALUE = "0"
           .
  ELSE
    ASSIGN iSil = INTEGER(fiSilo:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           iSil = IF DYNAMIC-FUNCTION('getLinea' IN hCnt) = "6" THEN iSil + 16 ELSE iSil
           iNro = 0.


  /*  
  IF iIni > iFin THEN DO:
    RUN wdTouchMsgBox.w ("La hora de finalizacion de molienda debe ser menor a la hora de comienzo", "La informacion no se registrara", OUTPUT lOk).
    RUN maximizeContainer.
    RETURN.
  END.
  */
  RUN setMoliendaSilo IN Hlib (DYNAMIC-FUNCTION('getLinea' IN hCnt),
                               DYNAMIC-FUNCTION('getTurno' IN hCnt),
                               DYNAMIC-FUNCTION('getFecha' IN hCnt),
                               iSil,
                               cIni,
                               cFin,
                               DECIMAL(fiKilos:SCREEN-VALUE),
                               DECIMAL(fiKilosDescarte:SCREEN-VALUE), 
                               iNro).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFillIns fFrameWin 
PROCEDURE setFillIns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piNum AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcTag AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cHoraIni AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMinIni  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cHoraFin AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMinFin  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKil     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDes     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTag     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lQst     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE hCont    AS HANDLE     NO-UNDO.

  {get ContainerSource hCont}.
  fiSilo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(piNum).

  fiLabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Kgs Descarte:".
  fiKilos:SENSITIVE = TRUE.
  IF piNum = 19 THEN DO:
    fiLabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Nro Pesada:".
    fiKilos:SENSITIVE = FALSE.
    fiKilosDescarte:SENSITIVE = TRUE.
  END.

  IF pcTag = "" THEN DO:
    RUN clearFillIns.
    RETURN NO-APPLY.
  END.

  
  RUN maximizeWindow IN hCont.
  RUN wdTouchMsgBox ("Esta Cajonera Tiene Lecturas Anteriores dentro de este Turno.", "Desea Ingresar un Nuevo Registro?", OUTPUT lQst).
  RUN maximizeWindow IN hCont.

  

  IF lQst THEN DO:
    RUN clearFillIns.
    RETURN NO-APPLY.
  END.
  
  cTag     = ENTRY(3, pcTag, CHR(1)).
  cHoraIni = ENTRY(1, cTag, ":").
  cMinIni  = ENTRY(2, cTag, ":").
  cTag     = ENTRY(4, pcTag, CHR(1)).
  cHoraFin = ENTRY(1, cTag, ":").
  cMinFin  = ENTRY(2, cTag, ":").
  cKil     = ENTRY(5, pcTag, CHR(1)).
  cDes     = ENTRY(6, pcTag, CHR(1)).
  
  
  fiInicioHr:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = cHoraIni.
  fiInicioMin:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = cMinIni.
  fiFinHr:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = cHoraFin.
  fiFinMin:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = cMinFin.
  fiKilos:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = cKil.
  fiKilosDescarte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cDes.

  APPLY "ENTRY" TO fiInicioHr IN FRAME {&FRAME-NAME} .
  hCurrWidget = fiInicioHr:HANDLE.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOcxValue fFrameWin 
PROCEDURE setOcxValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piNum    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plVal    AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pcArgs   AS CHARACTER  NO-UNDO.


  DEFINE VARIABLE hOcx AS COM-HANDLE     NO-UNDO.
  
  CASE piNum:
    WHEN 1 THEN 
      ASSIGN chSil-1:DiscreteValue1 = plVal
             chSil-1:Tag            = pcArgs
             chCurrSil              = chSil-1.
    WHEN 2 THEN 
      ASSIGN chSil-2:DiscreteValue1 = plVal
             chSil-2:Tag            = pcArgs
             chCurrSil              = chSil-2.
    WHEN 3 THEN 
      ASSIGN chSil-3:DiscreteValue1 = plVal
             chSil-3:Tag            = pcArgs
             chCurrSil              = chSil-3.
    WHEN 4 THEN 
      ASSIGN chSil-4:DiscreteValue1 = plVal
             chSil-4:Tag            = pcArgs
             chCurrSil              = chSil-4.
    WHEN 5 THEN 
      ASSIGN chSil-5:DiscreteValue1 = plVal
             chSil-5:Tag            = pcArgs
             chCurrSil              = chSil-5.
    WHEN 6 THEN 
      ASSIGN chSil-6:DiscreteValue1 = plVal
             chSil-6:Tag            = pcArgs
             chCurrSil              = chSil-6.
    WHEN 7 THEN 
      ASSIGN chSil-7:DiscreteValue1 = plVal
             chSil-7:Tag            = pcArgs
             chCurrSil              = chSil-7.
    WHEN 8 THEN 
      ASSIGN chSil-8:DiscreteValue1 = plVal
             chSil-8:Tag            = pcArgs
             chCurrSil              = chSil-8.
    WHEN 9 THEN 
      ASSIGN chSil-9:DiscreteValue1 = plVal
             chSil-9:Tag            = pcArgs
             chCurrSil              = chSil-9.
    WHEN 10 THEN 
      ASSIGN chSil-10:DiscreteValue1 = plVal
             chSil-10:Tag            = pcArgs
             chCurrSil               = chSil-10.
    WHEN 11 THEN 
      ASSIGN chSil-11:DiscreteValue1 = plVal
             chSil-11:Tag            = pcArgs
             chCurrSil               = chSil-11.
    WHEN 12 THEN 
      ASSIGN chSil-12:DiscreteValue1 = plVal
             chSil-12:Tag            = pcArgs
             chCurrSil               = chSil-12.
    WHEN 13 THEN 
      ASSIGN chSil-13:DiscreteValue1 = plVal
             chSil-13:Tag            = pcArgs
             chCurrSil               = chSil-13.
    WHEN 14 THEN 
      ASSIGN chSil-14:DiscreteValue1 = plVal
             chSil-14:Tag            = pcArgs
             chCurrSil               = chSil-14.
    WHEN 15 THEN 
      ASSIGN chSil-15:DiscreteValue1 = plVal
             chSil-15:Tag            = pcArgs
             chCurrSil               = chSil-15.
    WHEN 16 THEN 
      ASSIGN chSil-16:DiscreteValue1 = plVal
             chSil-16:Tag            = pcArgs
             chCurrSil               = chSil-16.
    WHEN 17 THEN 
      ASSIGN chSil-17:DiscreteValue1 = plVal
             chSil-17:Tag            = pcArgs
             chCurrSil               = chSil-17.
    WHEN 18 THEN 
      ASSIGN chSil-18:DiscreteValue1 = plVal
             chSil-18:Tag            = pcArgs
             chCurrSil               = chSil-18.
    WHEN 19 THEN
      ASSIGN chSil-19:DiscreteValue1 = plVal
             chSil-19:Tag            = pcArgs
             chCurrSil               = chSil-19.

  END CASE.

  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setWidgetValue fFrameWin 
PROCEDURE setWidgetValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcName   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcValue  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hWidget AS WIDGET-HANDLE     NO-UNDO.

  hWidget = FRAME {&FRAME-NAME}:FIRST-CHILD .
  hWidget = hWidget:FIRST-CHILD.
  DO WHILE VALID-HANDLE(hWidget):
    IF hWidget:NAME = pcName THEN 
      hWidget:SCREEN-VALUE = pcValue + CHR(10) + hWidget:SCREEN-VALUE.
    
    hWidget = hWidget:NEXT-SIBLING.
  END.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextOcxHandle fFrameWin 
FUNCTION getNextOcxHandle RETURNS COM-HANDLE
  (piCurrSil AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hRet AS COM-HANDLE     NO-UNDO.

  CASE piCurrSil:
    WHEN 1 THEN hRet = chSil-2.
    WHEN 2 THEN hRet = chSil-3.
    WHEN 3 THEN hRet = chSil-4.
    WHEN 4 THEN hRet = chSil-5.
    WHEN 5 THEN hRet = chSil-6.
    WHEN 6 THEN hRet = chSil-7.
    WHEN 7 THEN hRet = chSil-8.
    WHEN 8 THEN hRet = chSil-9.
    WHEN 9 THEN hRet = chSil-10.
    WHEN 10 THEN hRet = chSil-11.
    WHEN 11 THEN hRet = chSil-12.
    WHEN 12 THEN hRet = chSil-13.
    WHEN 13 THEN hRet = chSil-14.
    WHEN 14 THEN hRet = chSil-15.
    WHEN 15 THEN hRet = chSil-16.
    WHEN 16 THEN hRet = chSil-17.
    WHEN 17 THEN hRet = chSil-18.
    WHEN 18 THEN hRet = chSil-19.
    WHEN 19 THEN hRet = chSil-1.
    OTHERWISE hRet = chSil-1.
  END CASE.

  RETURN hRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWidgetHandle fFrameWin 
FUNCTION getWidgetHandle RETURNS HANDLE
  (pcName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hReturn AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hWidget AS WIDGET-HANDLE     NO-UNDO.

  hWidget = FRAME {&FRAME-NAME}:FIRST-CHILD .
  hWidget = hWidget:FIRST-CHILD.
  DO WHILE VALID-HANDLE(hWidget):
    IF hWidget:NAME = pcName THEN 
      hReturn = hWidget.
    
    hWidget = hWidget:NEXT-SIBLING.
  END.  

  RETURN hReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

