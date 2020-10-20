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
DEFINE VARIABLE chCal AS COM-HANDLE     NO-UNDO.

DEFINE VARIABLE iLinea AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTurno AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFecha AS DATE       NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS fiFecha fiLinea fiTurno btnL btnF btn1 btn2 ~
btn3 btnNext btnExit btnInfo fiLineaL fiLineaF fiTurno1 fiTurno2 fiTurno3 ~
IMAGE-1 RECT-1 RECT-10 RECT-2 RECT-3 RECT-5 RECT-6 RECT-7 RECT-8 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS fiFecha fiA-1 fiB-1 fiC-1 fiA-2 fiB-2 ~
fiC-2 fiA-3 fiB-3 fiC-3 fiLinea fiA-4 fiB-4 fiC-4 fiTurno fiLineaL fiLineaF ~
fiTurno1 fiTurno2 fiTurno3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn1 
     LABEL "1" 
     SIZE 15 BY 2.86
     FONT 25.

DEFINE BUTTON btn2 
     LABEL "2" 
     SIZE 15 BY 2.86
     FONT 25.

DEFINE BUTTON btn3 
     LABEL "3" 
     SIZE 15 BY 2.86
     FONT 25.

DEFINE BUTTON btnExit 
     LABEL "Salir" 
     SIZE 31 BY 3.33
     FONT 25.

DEFINE BUTTON btnF 
     LABEL "F" 
     SIZE 15 BY 2.86
     FONT 25.

DEFINE BUTTON btnInfo 
     LABEL "Resumen" 
     SIZE 31 BY 3.33
     FONT 25.

DEFINE BUTTON btnL 
     LABEL "L" 
     SIZE 15 BY 2.86
     FONT 25.

DEFINE BUTTON btnNext 
     LABEL "Molienda" 
     SIZE 62 BY 3.33
     FGCOLOR 20 FONT 25.

DEFINE VARIABLE fiA-1 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "1" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiA-2 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiA-3 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiA-4 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiB-1 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiB-2 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiB-3 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiB-4 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiC-1 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiC-2 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiC-3 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiC-4 AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFecha AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.91
     BGCOLOR 12 FONT 25 NO-UNDO.

DEFINE VARIABLE fiLinea AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.91
     BGCOLOR 10 FONT 25 NO-UNDO.

DEFINE VARIABLE fiLineaF AS CHARACTER FORMAT "X(256)":U INITIAL "LINEA F" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiLineaL AS CHARACTER FORMAT "X(256)":U INITIAL "LINEA L" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTurno AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.91
     BGCOLOR 11 FONT 25 NO-UNDO.

DEFINE VARIABLE fiTurno1 AS CHARACTER FORMAT "X(256)":U INITIAL "06:00 - 14:00" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTurno2 AS CHARACTER FORMAT "X(256)":U INITIAL "14:00 - 22:00" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTurno3 AS CHARACTER FORMAT "X(256)":U INITIAL "22:00 - 06:00" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "src/adm2/image/san_miguel.jpg":U
     STRETCH-TO-FIT
     SIZE 89 BY 5.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62.2 BY 5.71.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY .24.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91 BY 6.91.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 13.33.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62.2 BY 6.19.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 6.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 1 BY 6.19.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 1 BY 6.19.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY .24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiFecha AT ROW 2.19 COL 162 COLON-ALIGNED NO-LABEL
     fiA-1 AT ROW 2.43 COL 108 RIGHT-ALIGNED
     fiB-1 AT ROW 2.43 COL 123 RIGHT-ALIGNED NO-LABEL
     fiC-1 AT ROW 2.43 COL 138 RIGHT-ALIGNED NO-LABEL
     fiA-2 AT ROW 3.62 COL 108 RIGHT-ALIGNED
     fiB-2 AT ROW 3.62 COL 123 RIGHT-ALIGNED NO-LABEL
     fiC-2 AT ROW 3.62 COL 138 RIGHT-ALIGNED NO-LABEL
     fiA-3 AT ROW 4.81 COL 108 RIGHT-ALIGNED
     fiB-3 AT ROW 4.81 COL 123 RIGHT-ALIGNED NO-LABEL
     fiC-3 AT ROW 4.81 COL 138 RIGHT-ALIGNED NO-LABEL
     fiLinea AT ROW 6.24 COL 162 COLON-ALIGNED NO-LABEL
     fiA-4 AT ROW 6.48 COL 108 RIGHT-ALIGNED NO-LABEL
     fiB-4 AT ROW 6.48 COL 123 RIGHT-ALIGNED NO-LABEL
     fiC-4 AT ROW 6.48 COL 138 RIGHT-ALIGNED NO-LABEL
     fiTurno AT ROW 10.29 COL 162 COLON-ALIGNED NO-LABEL
     btnL AT ROW 16.48 COL 150
     btnF AT ROW 16.48 COL 179
     btn1 AT ROW 22.43 COL 143
     btn2 AT ROW 22.43 COL 165
     btn3 AT ROW 22.43 COL 186
     btnNext AT ROW 27.67 COL 141
     btnExit AT ROW 31 COL 141
     btnInfo AT ROW 31 COL 172
     fiLineaL AT ROW 19.81 COL 150 COLON-ALIGNED NO-LABEL
     fiLineaF AT ROW 19.81 COL 179 COLON-ALIGNED NO-LABEL
     fiTurno1 AT ROW 26.24 COL 141 COLON-ALIGNED NO-LABEL
     fiTurno2 AT ROW 26.24 COL 162 COLON-ALIGNED NO-LABEL
     fiTurno3 AT ROW 26.24 COL 183 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 1.95 COL 2
     RECT-1 AT ROW 15.05 COL 141
     RECT-10 AT ROW 1.95 COL 94
     RECT-2 AT ROW 1 COL 1
     RECT-3 AT ROW 1 COL 141
     RECT-5 AT ROW 21.24 COL 141
     RECT-6 AT ROW 1 COL 92
     RECT-7 AT ROW 1.24 COL 109
     RECT-8 AT ROW 1.24 COL 124
     RECT-9 AT ROW 6 COL 94
     "Turno:" VIEW-AS TEXT
          SIZE 21 BY 1.43 AT ROW 10.52 COL 142
          FONT 25
     "Fecha:" VIEW-AS TEXT
          SIZE 21 BY 1.43 AT ROW 2.43 COL 142
          FONT 25
     "Linea" VIEW-AS TEXT
          SIZE 17 BY 1.19 AT ROW 14.57 COL 143
          FONT 25
     "Turno" VIEW-AS TEXT
          SIZE 17 BY 1.19 AT ROW 20.76 COL 142
          FONT 25
     "Linea:" VIEW-AS TEXT
          SIZE 21 BY 1.43 AT ROW 6.48 COL 142
          FONT 25
     "Linea L" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 99
          FONT 6
     "Linea F" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.24 COL 112
          FONT 6
     "Totales" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.24 COL 127
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.2 BY 33.52.


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
         HEIGHT             = 33.52
         WIDTH              = 202.2.
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

/* SETTINGS FOR FILL-IN fiA-1 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiA-2 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiA-3 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiA-4 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiB-1 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiB-2 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiB-3 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiB-4 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiC-1 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiC-2 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiC-3 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiC-4 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       fiLineaF:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiLineaL:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiTurno1:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiTurno2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiTurno3:READ-ONLY IN FRAME fMain        = TRUE.

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

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 8.14
       COLUMN          = 1
       HEIGHT          = 26.19
       WIDTH           = 139
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {232E456A-87C3-11D1-8BE3-0000F8754DA1} type: MonthView */
      CtrlFrame:MOVE-AFTER(fiC-4:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fMain fFrameWin
ON F11 OF FRAME fMain
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn1 fFrameWin
ON CHOOSE OF btn1 IN FRAME fMain /* 1 */
DO:
  fiTurno:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "T1 (06-14)".

  fiTurno1:BGCOLOR IN FRAME {&FRAME-NAME}      = 11.
  fiTurno2:BGCOLOR IN FRAME {&FRAME-NAME}      = ?.
  fiTurno3:BGCOLOR IN FRAME {&FRAME-NAME}      = ?.

  iTurno = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2 fFrameWin
ON CHOOSE OF btn2 IN FRAME fMain /* 2 */
DO:
  fiTurno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "T2 (14-22)".

  fiTurno1:BGCOLOR IN FRAME {&FRAME-NAME}      = ?.
  fiTurno2:BGCOLOR IN FRAME {&FRAME-NAME}      = 11.
  fiTurno3:BGCOLOR IN FRAME {&FRAME-NAME}      = ?.

  iTurno = 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn3 fFrameWin
ON CHOOSE OF btn3 IN FRAME fMain /* 3 */
DO:
  fiTurno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "T3 (22-06)".

  fiTurno1:BGCOLOR IN FRAME {&FRAME-NAME}      = ?.
  fiTurno2:BGCOLOR IN FRAME {&FRAME-NAME}      = ?.
  fiTurno3:BGCOLOR IN FRAME {&FRAME-NAME}      = 11.

  iTurno = 3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit fFrameWin
ON CHOOSE OF btnExit IN FRAME fMain /* Salir */
DO:
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.
  {get ContainerSource hCont}.
  RUN exitApp IN hCont.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnF fFrameWin
ON CHOOSE OF btnF IN FRAME fMain /* F */
DO:
  fiLinea:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "LINEA F".
  fiLineaF:BGCOLOR IN FRAME {&FRAME-NAME}      = 10.
  fiLineaF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "LINEA F".

  fiLineaL:BGCOLOR IN FRAME {&FRAME-NAME}      = ?.

  iLinea = 6.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnInfo fFrameWin
ON CHOOSE OF btnInfo IN FRAME fMain /* Resumen */
DO:
  DEFINE VARIABLE cCod AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOk  AS CHARACTER  NO-UNDO.

  IF fiFecha:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Debe Ingresar una Fecha Valida"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  IF fiLinea:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Debe Ingresar una Linea de Produccion Valida"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  IF fiTurno:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Debe Ingresar un Turno Valido"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  RUN wdTouchInputCod.w (iTurno, DATE(fiFecha:SCREEN-VALUE), OUTPUT cOk).
  RUN maximizeContainer.
  IF cOk = "" THEN RETURN.
  RUN wdTouchResumenTurno.w (DATE(fiFecha:SCREEN-VALUE), iLinea, iTurno, INTEGER(cOk)).
  RUN maximizeContainer. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME btnL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnL fFrameWin
ON CHOOSE OF btnL IN FRAME fMain /* L */
DO:
  fiLinea:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "LINEA L".
  fiLineaL:BGCOLOR IN FRAME {&FRAME-NAME}      = 10.
  fiLineaL:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "LINEA L".
  fiLineaF:BGCOLOR IN FRAME {&FRAME-NAME}      = ?.

  iLinea = 5.
 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext fFrameWin
ON CHOOSE OF btnNext IN FRAME fMain /* Molienda */
DO:
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cArgs AS CHARACTER  NO-UNDO.

  IF fiFecha:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Debe Ingresar una Fecha Valida"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  IF fiLinea:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Debe Ingresar una Linea de Produccion Valida"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  IF fiTurno:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Debe Ingresar un Turno Valido"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  {get ContainerSource hCont}.
  cArgs = STRING(dFecha) + "," + STRING(iLinea) + "," + STRING(iTurno).
  RUN switchPage IN hCont (1, cArgs).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame fFrameWin OCX.DateClick
PROCEDURE CtrlFrame.MonthView.DateClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    DateClicked
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-DateClicked AS DATE NO-UNDO.

  fiFecha:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(p-DateClicked).
  dFecha = p-DateClicked.
  RUN recalcTotales.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

OCXFile = SEARCH( "fTouchMoliendaPg1.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "fTouchMoliendaPg1.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY fiFecha fiA-1 fiB-1 fiC-1 fiA-2 fiB-2 fiC-2 fiA-3 fiB-3 fiC-3 fiLinea 
          fiA-4 fiB-4 fiC-4 fiTurno fiLineaL fiLineaF fiTurno1 fiTurno2 fiTurno3 
      WITH FRAME fMain.
  ENABLE fiFecha fiLinea fiTurno btnL btnF btn1 btn2 btn3 btnNext btnExit 
         btnInfo fiLineaL fiLineaF fiTurno1 fiTurno2 fiTurno3 IMAGE-1 RECT-1 
         RECT-10 RECT-2 RECT-3 RECT-5 RECT-6 RECT-7 RECT-8 RECT-9 
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

  chCal = chCtrlFrame:MonthView.
  chCal:FONT:NAME = "Arial".
  chCal:FONT:SIZE = 30.
  chCal:VALUE     = TODAY.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcTotales fFrameWin 
PROCEDURE recalcTotales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.

  ASSIGN fiA-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = DYNAMIC-FUNCTION('getMoliendaTurno' IN hLib, DATE(chCal:VALUE), 1, 5)
         fiA-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = DYNAMIC-FUNCTION('getMoliendaTurno' IN hLib, DATE(chCal:VALUE), 2, 5)
         fiA-3:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = DYNAMIC-FUNCTION('getMoliendaTurno' IN hLib, DATE(chCal:VALUE), 3, 5)
         fiB-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = DYNAMIC-FUNCTION('getMoliendaTurno' IN hLib, DATE(chCal:VALUE), 1, 6)
         fiB-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = DYNAMIC-FUNCTION('getMoliendaTurno' IN hLib, DATE(chCal:VALUE), 2, 6)
         fiB-3:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = DYNAMIC-FUNCTION('getMoliendaTurno' IN hLib, DATE(chCal:VALUE), 3, 6)

         fiC-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(DECIMAL(fiA-1:SCREEN-VALUE) + DECIMAL(fiB-1:SCREEN-VALUE))
         fiC-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(DECIMAL(fiA-2:SCREEN-VALUE) + DECIMAL(fiB-2:SCREEN-VALUE))
         fiC-3:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(DECIMAL(fiA-3:SCREEN-VALUE) + DECIMAL(fiB-3:SCREEN-VALUE))
      
         fiA-4:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(DECIMAL(fiA-1:SCREEN-VALUE) + DECIMAL(fiA-2:SCREEN-VALUE) + DECIMAL(fiA-3:SCREEN-VALUE))
         fiB-4:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(DECIMAL(fiB-1:SCREEN-VALUE) + DECIMAL(fiB-2:SCREEN-VALUE) + DECIMAL(fiB-3:SCREEN-VALUE))

         fiC-4:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(DECIMAL(fiA-4:SCREEN-VALUE) + DECIMAL(fiB-4:SCREEN-VALUE))     
         .
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

