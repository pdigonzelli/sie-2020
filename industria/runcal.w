&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

Def var hCal as handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RadioLanguage BUTTON-1 SLIDER-1 SLIDER-2 ~
SLIDER-3 SLIDER-4 
&Scoped-Define DISPLAYED-OBJECTS RadioLanguage SLIDER-1 SLIDER-2 SLIDER-3 ~
SLIDER-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Run calendar" 
     SIZE 19 BY 1.14.

DEFINE VARIABLE RadioLanguage AS CHARACTER INITIAL "English" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "English", "English",
"Fran�ais", "Fran�ais",
"Deutsch", "Deutsch",
"Dutch", "Dutch"
     SIZE 24.4 BY 3.48 NO-UNDO.

DEFINE VARIABLE SLIDER-1 AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 100 HORIZONTAL 
     TIC-MARKS TOP FREQUENCY 5
     SIZE 71.2 BY 1.48 TOOLTIP "Today ++" NO-UNDO.

DEFINE VARIABLE SLIDER-2 AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 15 HORIZONTAL 
     TIC-MARKS TOP FREQUENCY 1
     SIZE 71.2 BY 1.48 TOOLTIP "BGColor" NO-UNDO.

DEFINE VARIABLE SLIDER-3 AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 15 HORIZONTAL 
     TIC-MARKS TOP FREQUENCY 1
     SIZE 71.2 BY 1.48 TOOLTIP "Foreground Color" NO-UNDO.

DEFINE VARIABLE SLIDER-4 AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 1 MAX-VALUE 8 HORIZONTAL 
     TIC-MARKS TOP FREQUENCY 1
     SIZE 71.2 BY 1.48 TOOLTIP "Font" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RadioLanguage AT ROW 1.81 COL 41.2 NO-LABEL
     BUTTON-1 AT ROW 2.76 COL 6.8
     SLIDER-1 AT ROW 6.24 COL 5 NO-LABEL
     SLIDER-2 AT ROW 8.91 COL 5 NO-LABEL
     SLIDER-3 AT ROW 11.71 COL 5.4 NO-LABEL
     SLIDER-4 AT ROW 14.29 COL 5.4 NO-LABEL
     "FONT" VIEW-AS TEXT
          SIZE 14.2 BY .62 AT ROW 13.48 COL 7.4
     "FGCOLOR" VIEW-AS TEXT
          SIZE 14.2 BY .62 AT ROW 10.91 COL 7.4
     "DAY +" VIEW-AS TEXT
          SIZE 16.4 BY 1.33 AT ROW 4.81 COL 7.2
     "BGCOLOR" VIEW-AS TEXT
          SIZE 14.2 BY .62 AT ROW 8.1 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 17
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Calendar test"
         HEIGHT             = 15.48
         WIDTH              = 79.8
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Calendar test */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Calendar test */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Run calendar */
DO:
  run calendar.w PERSISTENT set hCal
        ("language=english|font=7|ProgramHandle=" + string(This-procedure)).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RadioLanguage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RadioLanguage W-Win
ON VALUE-CHANGED OF RadioLanguage IN FRAME F-Main
DO:
if valid-handle(hCal) then
  Run calendarSetAttributes in hCal("Language=" + self:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-1 W-Win
ON VALUE-CHANGED OF SLIDER-1 IN FRAME F-Main
DO:
if valid-handle(hCal) then
  Run calendarSetAttributes in hCal("Date=" + string(Today + input {&SELF-NAME})).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-2 W-Win
ON VALUE-CHANGED OF SLIDER-2 IN FRAME F-Main
DO:
  if valid-handle(hCal) then
      Run calendarSetAttributes in hCal("BGColor=" + self:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-3 W-Win
ON VALUE-CHANGED OF SLIDER-3 IN FRAME F-Main
DO:
  if valid-handle(hCal) then
      Run calendarSetAttributes in hCal("FGColor=" + self:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-4 W-Win
ON VALUE-CHANGED OF SLIDER-4 IN FRAME F-Main
DO:
  if valid-handle(hCal) then
      Run calendarSetAttributes in hCal("FONT=" + self:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
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
  DISPLAY RadioLanguage SLIDER-1 SLIDER-2 SLIDER-3 SLIDER-4 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RadioLanguage BUTTON-1 SLIDER-1 SLIDER-2 SLIDER-3 SLIDER-4 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onDateChanged W-Win 
PROCEDURE onDateChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Def input parameter hCal    as Handle NO-UNDO.
Def input parameter iDate   as Date   NO-UNDO.
Def input parameter hINFO   as Handle NO-UNDO.


hInfo:SCREEN-VALUE = "Nothing to do on " + string(iDate, '99/99/9999').

If Weekday(iDate) = 1 then      /* skip the sunday */
    Run calendarSetattributes in hCal
    	           ("Date=" + string(Idate + 1)).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onLeaveInfo W-Win 
PROCEDURE onLeaveInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Def input parameter hCal    as Handle NO-UNDO.
Def input parameter iDate   as Date   NO-UNDO.
Def input parameter hINFO   as Handle NO-UNDO.

Message hINFO:SCREEN-VALUE view-as alert-box INFO.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
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


