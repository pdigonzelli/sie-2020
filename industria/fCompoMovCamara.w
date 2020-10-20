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
DEFINE VARIABLE iSucUbi AS INTEGER    NO-UNDO.

DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCam AS INTEGER    NO-UNDO.
DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCol AS CHARACTER  NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnAdd radFilter lstFilter cmbOps fiValue ~
btnFilter btnDel btnReset btnAddClause btnDelClause RECT-24 RECT-25 
&Scoped-Define DISPLAYED-OBJECTS radFilter lstFilter cmbOps fiValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bttubicacioncamara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttubicacioncamara AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "src/adm2/image/rollback.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 5.2 BY 1.19.

DEFINE BUTTON btnAddClause 
     IMAGE-UP FILE "src/adm2/image/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Agregar" 
     SIZE 5.2 BY 1.19 TOOLTIP "Agregar".

DEFINE BUTTON btnDel 
     IMAGE-UP FILE "src/adm2/image/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnadd 2" 
     SIZE 5.2 BY 1.19.

DEFINE BUTTON btnDelClause 
     IMAGE-UP FILE "src/adm2/image/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Quitar" 
     SIZE 5.2 BY 1.19 TOOLTIP "Qutar".

DEFINE BUTTON btnFilter 
     IMAGE-UP FILE "src/adm2/image/filter.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Filtrar" 
     SIZE 5.2 BY 1.19.

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "src/adm2/image/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Deshacer" 
     SIZE 5.2 BY 1.19 TOOLTIP "Deshacer".

DEFINE VARIABLE cmbOps AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS ">=","<=","=" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiValue AS CHARACTER FORMAT "x(256)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 NO-UNDO.

DEFINE VARIABLE radFilter AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Lote", 1,
"Anio", 2,
"Articulo", 3,
"Tambor", 4
     SIZE 14 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 4.29.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 6 BY 2.62.

DEFINE VARIABLE lstFilter AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 3.33 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnAdd AT ROW 7.76 COL 71.4
     radFilter AT ROW 1.24 COL 2 NO-LABEL
     lstFilter AT ROW 1.48 COL 38.6 NO-LABEL
     cmbOps AT ROW 2.76 COL 15 COLON-ALIGNED NO-LABEL
     fiValue AT ROW 2.76 COL 22.4 COLON-ALIGNED NO-LABEL
     btnFilter AT ROW 1.48 COL 63
     btnDel AT ROW 9 COL 71.4
     btnReset AT ROW 3.62 COL 63
     btnAddClause AT ROW 1.48 COL 33
     btnDelClause AT ROW 3.62 COL 33
     RECT-24 AT ROW 1 COL 1
     RECT-25 AT ROW 7.67 COL 71
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.6 BY 11.81.


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
         HEIGHT             = 11.81
         WIDTH              = 76.6.
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
       btnAddClause:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnDelClause:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnFilter:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnReset:HIDDEN IN FRAME fMain           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd fFrameWin
ON CHOOSE OF btnAdd IN FRAME fMain /* Button 1 */
DO:
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cRids AS CHARACTER  NO-UNDO.

  {get ContainerSource hCont}.
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wMovimientosCamara.w" THEN DO:
    cRids = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bttUbicacionCamara).
    cRids = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_dttUbicacionCamara, cRids).

    RUN addMovimientoCamara IN hCont (cRids).
  END.

  RUN fillTT IN h_dttUbicacionCamara (iEmp, iSuc, iCam, cFil, cCol).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME btnAddClause
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddClause fFrameWin
ON CHOOSE OF btnAddClause IN FRAME fMain /* Agregar */
DO:
  DEFINE VARIABLE cFilterStr AS CHARACTER  NO-UNDO.

  CASE INTEGER(radFilter:SCREEN-VALUE).
    WHEN 1 THEN 
      cFilterStr = "id_lote;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 2 THEN
      cFilterStr = "anio;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 3 THEN
      cFilterStr = "id_articulo;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 4 THEN
      cFilterStr = "id_tambor;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
  END CASE.
  
  lstFilter:ADD-LAST(cFilterStr).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDel fFrameWin
ON CHOOSE OF btnDel IN FRAME fMain /* btnadd 2 */
DO:
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.

  {get ContainerSource hCont}.
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wMovimientosCamara.w" THEN DO:
    RUN delMovimientoCamara IN hCont.
  END.

  RUN fillTT IN h_dttUbicacionCamara (iEmp, iSuc, iCam, cFil, cCol).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelClause
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelClause fFrameWin
ON CHOOSE OF btnDelClause IN FRAME fMain /* Quitar */
DO:
  lstFilter:DELETE(lstFilter:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter fFrameWin
ON CHOOSE OF btnFilter IN FRAME fMain /* Filtrar */
DO:
  DEFINE VARIABLE cColumns   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValues    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOperators AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCond       AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  
  DO i = 1 TO NUM-ENTRIES(lstFilter:LIST-ITEMS):
    cCond = ENTRY(i, lstFilter:LIST-ITEMS).
    ASSIGN cColumns   = cColumns + "," + ENTRY(1, cCond, ";")
           cOperators = cOperators + "," + ENTRY(2, cCond, ";")
           cValues    = cValues + CHR(1) + ENTRY(3, cCond, ";").    
  END.

  cColumns   = SUBSTRING(cColumns, 2, LENGTH(cColumns)).
  cOperators = SUBSTRING(cOperators, 2, LENGTH(cOperators)).
  cValues    = SUBSTRING(cValues, 2, LENGTH(cValues)).

  DYNAMIC-FUNCTION('assignQuerySelection' IN h_dttUbicacionCamara, cColumns, cValues, cOperators).
  DYNAMIC-FUNCTION('openQuery' IN h_dttUbicacionCamara).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset fFrameWin
ON CHOOSE OF btnReset IN FRAME fMain /* Deshacer */
DO:
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  cQry = lstFilter:LIST-ITEMS.
  DO i = 1 TO NUM-ENTRIES(cQry):
    lstFilter:DELETE(ENTRY(i, cQry)).
  END.
  
  cQry = DYNAMIC-FUNCTION('getQueryString' IN h_dttUbicacionCamara).
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dttUbicacionCamara, cQry).    
  DYNAMIC-FUNCTION('openQuery' IN h_dttUbicacionCamara).
END.

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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dttubicacioncamara.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedttubicacioncamaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dttubicacioncamara ).
       RUN repositionObject IN h_dttubicacioncamara ( 1.00 , 66.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bttubicacioncamara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bttubicacioncamara ).
       RUN repositionObject IN h_bttubicacioncamara ( 5.52 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bttubicacioncamara ( 7.14 , 68.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bttubicacioncamara. */
       RUN addLink ( h_dttubicacioncamara , 'Data':U , h_bttubicacioncamara ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bttubicacioncamara ,
             fiValue:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY radFilter lstFilter cmbOps fiValue 
      WITH FRAME fMain.
  ENABLE btnAdd radFilter lstFilter cmbOps fiValue btnFilter btnDel btnReset 
         btnAddClause btnDelClause RECT-24 RECT-25 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshData fFrameWin 
PROCEDURE refreshData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcFil AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcCol AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cQry    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColumn AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOperat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValues AS CHARACTER  NO-UNDO.

  ASSIGN iEmp = piEmp
         iSuc = piSuc
         iCam = piCam
         cFil = pcFil
         cCol = pcCol.

  RUN fillTT IN h_dttUbicacionCamara (iEmp, iSuc, iCam, cFil, cCol).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

