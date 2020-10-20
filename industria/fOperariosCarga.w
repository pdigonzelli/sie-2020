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
DEFINE VARIABLE chEnc AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chCho AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chCar AS COM-HANDLE     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48 BY 7.38.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48 BY 7.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 15.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-1 AT ROW 1.48 COL 2
     RECT-2 AT ROW 9.57 COL 2
     RECT-3 AT ROW 1.48 COL 51
     "Encargados" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.24 COL 4
          FONT 6
     "Choferes" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 9.33 COL 4
          FONT 6
     "Cargadores" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.24 COL 53
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.8 BY 18.29.


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
         HEIGHT             = 16.1
         WIDTH              = 100.8.
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
                                                                        */
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
       ROW             = 1.95
       COLUMN          = 3
       HEIGHT          = 6.67
       WIDTH           = 46
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.95
       COLUMN          = 52
       HEIGHT          = 14.76
       WIDTH           = 48
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 10.05
       COLUMN          = 3
       HEIGHT          = 6.67
       WIDTH           = 46
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {BDD1F04B-858B-11D1-B16A-00C0F0283628} type: ListView */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {BDD1F04B-858B-11D1-B16A-00C0F0283628} type: ListView */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {BDD1F04B-858B-11D1-B16A-00C0F0283628} type: ListView */
      CtrlFrame-3:MOVE-AFTER(CtrlFrame).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame-3).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame fFrameWin OCX.ItemCheck
PROCEDURE CtrlFrame.ListView.ItemCheck .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Item
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER p-Item AS COM-HANDLE NO-UNDO.

  RUN itemCheck(p-Item).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 fFrameWin OCX.ItemCheck
PROCEDURE CtrlFrame-2.ListView.ItemCheck .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Item
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Item AS COM-HANDLE NO-UNDO.

RUN itemCheck(p-Item).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 fFrameWin OCX.ItemCheck
PROCEDURE CtrlFrame-3.ListView.ItemCheck .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Item
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Item AS COM-HANDLE NO-UNDO.

RUN itemCheck(p-Item).


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

OCXFile = SEARCH( "foperarioscarga.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "foperarioscarga.wrx":U SKIP(1)
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
  ENABLE RECT-1 RECT-2 RECT-3 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillLists fFrameWin 
PROCEDURE fillLists :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cEnc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCon AS HANDLE     NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIns AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE chNo AS COM-HANDLE     NO-UNDO.


  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.
  
  hCon = DYNAMIC-FUNCTION('getContainerSource').
  IF NOT VALID-HANDLE(hCon) THEN RETURN.
  cIns = DYNAMIC-FUNCTION('getClaveInspeccion' IN hCon).

  /* encargados */
  cEnc = DYNAMIC-FUNCTION('getListaPersonalOf' IN hLib, 'ENCARGADO').
  DO i = 1 TO NUM-ENTRIES(cEnc, CHR(1)):
    cRow = ENTRY(i, cEnc, CHR(1)).
    cKey = ENTRY(1, cRow).
    cTxt = ENTRY(2, cRow) + ", " + ENTRY(3, cRow).

    chNo = chEnc:ListItems:ADD(, , cTxt).
    chNo:Tag = cKey.
    chNo:CHECKED = DYNAMIC-FUNCTION('getOperarioInspeccion' IN hLib, integer(ENTRY(1, cIns, CHR(1))),
                                                                     integer(ENTRY(2, cIns, CHR(1))),
                                                                     integer(ENTRY(3, cIns, CHR(1))),
                                                                     integer(ENTRY(5, cIns, CHR(1))), 
                                                                     INTEGER(cKey)).
  END.

  /* chofers */
  cEnc = DYNAMIC-FUNCTION('getListaPersonalOf' IN hLib, 'CHOFER').
  DO i = 1 TO NUM-ENTRIES(cEnc, CHR(1)):
    cRow = ENTRY(i, cEnc, CHR(1)).
    cKey = ENTRY(1, cRow).
    cTxt = ENTRY(2, cRow) + ", " + ENTRY(3, cRow).

    chNo = chCho:ListItems:ADD(, , cTxt).
    chNo:Tag = cKey.
    chNo:CHECKED = DYNAMIC-FUNCTION('getOperarioInspeccion' IN hLib, integer(ENTRY(1, cIns, CHR(1))),
                                                                     integer(ENTRY(2, cIns, CHR(1))),
                                                                     integer(ENTRY(3, cIns, CHR(1))),
                                                                     integer(ENTRY(5, cIns, CHR(1))), 
                                                                     INTEGER(cKey)).
  END.

  /* cargadores */
  cEnc = DYNAMIC-FUNCTION('getListaPersonalOf' IN hLib, 'CARGADOR').
  DO i = 1 TO NUM-ENTRIES(cEnc, CHR(1)):
    cRow = ENTRY(i, cEnc, CHR(1)).
    cKey = ENTRY(1, cRow).
    cTxt = ENTRY(2, cRow) + ", " + ENTRY(3, cRow).

    chNo = chCar:ListItems:ADD(, , cTxt).
    chNo:Tag = cKey.
    chNo:CHECKED = DYNAMIC-FUNCTION('getOperarioInspeccion' IN hLib, integer(ENTRY(1, cIns, CHR(1))),
                                                                     integer(ENTRY(2, cIns, CHR(1))),
                                                                     integer(ENTRY(3, cIns, CHR(1))),
                                                                     integer(ENTRY(5, cIns, CHR(1))), 
                                                                     INTEGER(cKey)).
  END.

 

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

  chEnc = chCtrlFrame:listview.
  chCho = chCtrlFrame-2:Listview.
  chCar = chCtrlFrame-3:ListView.

  RUN fillLists.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemCheck fFrameWin 
PROCEDURE itemCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phItem AS COM-HANDLE     NO-UNDO.

  DEFINE VARIABLE hLib      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCon      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom   AS HANDLE.
  DEFINE VARIABLE cIns      AS CHARACTER  NO-UNDO.
  
  hCon = DYNAMIC-FUNCTION('getContainerSource').
  cIns = DYNAMIC-FUNCTION('getClaveInspeccion' IN hCon).
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.

  RUN setInspeccionOperario IN hLib (INTEGER(ENTRY(1, cIns, CHR(1))), 
                                     INTEGER(ENTRY(2, cIns, CHR(1))),
                                     INTEGER(ENTRY(3, cIns, CHR(1))),
                                     INTEGER(ENTRY(5, cIns, CHR(1))),
                                     INTEGER(phItem:Tag), 
                                     LOGICAL(phItem:CHECKED)).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

