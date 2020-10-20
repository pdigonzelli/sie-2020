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

DEFINE VARIABLE iArticulo AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSucursal AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTipoTamb AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFecha    AS DATE       NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnFilter btnAdd btnDel radFilter lstFilter ~
cmbOps fiValue btnReset btnAddClause btnDelClause fiCantidad fiKilos ~
RECT-25 RECT-26 RECT-27 
&Scoped-Define DISPLAYED-OBJECTS radFilter lstFilter cmbOps fiValue ~
fiTambores fiCantidad fiKilos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bttoildrums AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttoildrums AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

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

DEFINE VARIABLE fiCantidad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cant. Tambores" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiKilos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kilos" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiTambores AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fiValue AS CHARACTER FORMAT "x(256)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 NO-UNDO.

DEFINE VARIABLE radFilter AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sucursal", 1,
"Lote/Prod", 2,
"Anio", 3,
"Articulo", 4,
"Nro Tambor", 5
     SIZE 20 BY 4.29 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 4.76.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 6 BY 2.62.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 1.19.

DEFINE VARIABLE lstFilter AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 21 BY 2.81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnFilter AT ROW 1.86 COL 60
     btnAdd AT ROW 9.43 COL 67.4
     btnDel AT ROW 10.67 COL 67.4
     radFilter AT ROW 1.24 COL 2 NO-LABEL
     lstFilter AT ROW 1.91 COL 39 NO-LABEL
     cmbOps AT ROW 2.91 COL 15 COLON-ALIGNED NO-LABEL
     fiValue AT ROW 2.91 COL 22.4 COLON-ALIGNED NO-LABEL
     fiTambores AT ROW 5.76 COL 59 COLON-ALIGNED NO-LABEL
     btnReset AT ROW 3.57 COL 60.2
     btnAddClause AT ROW 1.86 COL 33.4
     btnDelClause AT ROW 3.52 COL 33.4
     fiCantidad AT ROW 13.62 COL 17 COLON-ALIGNED
     fiKilos AT ROW 13.62 COL 48 COLON-ALIGNED
     RECT-25 AT ROW 1 COL 1
     RECT-26 AT ROW 9.33 COL 67
     RECT-27 AT ROW 13.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73 BY 13.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Design Page: 4
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
         HEIGHT             = 13.62
         WIDTH              = 73.
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

ASSIGN 
       fiCantidad:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fiKilos:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiTambores IN FRAME fMain
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd fFrameWin
ON CHOOSE OF btnAdd IN FRAME fMain /* Button 1 */
DO:
  DEFINE VARIABLE hCont AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRds AS CHARACTER  NO-UNDO.
  
  {get ContainerSource hCont}.
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wLotesAceite.w*" THEN DO:
    cMes = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bttOilDrums).
    cRds = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_dttOilDrums, cMes).
    RUN addDrumsToBatch IN hCont (cRds).
  END.
  
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
      cFilterStr = "id_sucursal;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 2 THEN
      cFilterStr = "id_lote_prod;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 3 THEN
      cFilterStr = "anio;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 4 THEN
      cFilterStr = "id_articulo;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 5 THEN
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
  
  {get ContainerSource hCont}.
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wLotesAceite.w*" THEN DO:
    RUN removeDrumsFromBatch IN hCont.
  END.
  
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

  DYNAMIC-FUNCTION('assignQuerySelection' IN h_dttOilDrums, cColumns, cValues, cOperators).
  DYNAMIC-FUNCTION('openQuery' IN h_dttOilDrums).
  fiTambores:SCREEN-VALUE IN FRAME fMain = STRING(DYNAMIC-FUNCTION('getRowsNumber' IN h_dttOilDrums)).
  RUN calculateSelectedValues.
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
  
  cQry = DYNAMIC-FUNCTION('getQueryString' IN h_dttOilDrums).
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dttOilDrums, cQry).    
  DYNAMIC-FUNCTION('openQuery' IN h_dttOilDrums).
  fiTambores:SCREEN-VALUE IN FRAME fMain = STRING(DYNAMIC-FUNCTION('getRowsNumber' IN h_dttOilDrums)).
  RUN calculateSelectedValues.
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
             INPUT  'dttoildrums.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedttoildrumsUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dttoildrums ).
       RUN repositionObject IN h_dttoildrums ( 1.00 , 63.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bttoildrums.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bttoildrums ).
       RUN repositionObject IN h_bttoildrums ( 6.95 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bttoildrums ( 6.19 , 63.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Prod.|Sbte.|Lotes|Pot 3°|Prod. 3°' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 5.76 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 7.62 , 65.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bttoildrums. */
       RUN addLink ( h_dttoildrums , 'Data':U , h_bttoildrums ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             fiValue:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculateSelectedValues fFrameWin 
PROCEDURE calculateSelectedValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRows AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet  AS CHARACTER  NO-UNDO.
  

  cRows = DYNAMIC-FUNCTION('getSelectedRows' IN h_bttOilDrums).
  cRet  = DYNAMIC-FUNCTION('getSumSelectedValues' IN h_dttOilDrums, cRows).

  fiCantidad:SCREEN-VALUE IN FRAME fMain  = ENTRY(1, cRet, CHR(1)).
  fiKilos:SCREEN-VALUE IN FRAME fMain     = ENTRY(2, cRet, CHR(1)).
  

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
  DISPLAY radFilter lstFilter cmbOps fiValue fiTambores fiCantidad fiKilos 
      WITH FRAME fMain.
  ENABLE btnFilter btnAdd btnDel radFilter lstFilter cmbOps fiValue btnReset 
         btnAddClause btnDelClause fiCantidad fiKilos RECT-25 RECT-26 RECT-27 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postOpenQuery fFrameWin 
PROCEDURE postOpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  fiTambores:SCREEN-VALUE IN FRAME fMain = DYNAMIC-FUNCTION('getRowsNumber' IN h_dttOilDrums).

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
DEFINE VARIABLE iPageNum AS INTEGER    NO-UNDO.

  
  iPageNum = DYNAMIC-FUNCTION('getCurrentPage').

  CASE iPageNum:
    WHEN 1 THEN DO:
      RUN fillTTOilDrums IN h_dttOilDrums (iSucursal, 2, dFecha, iArticulo).
    END.
    WHEN 2 THEN DO:
      RUN fillTTOilDrums IN h_dttOilDrums (iSucursal, 8, dFecha, iArticulo).
    END.
    WHEN 3 THEN DO:
      RUN fillTTOilDrums IN h_dttOilDrums (iSucursal, 6, dFecha, iArticulo).
    END.
    WHEN 4 THEN DO:
      RUN fillTTOilDrums IN h_dttOilDrums (iSucursal, 7, dFecha, iArticulo).
    END.
    WHEN 5 THEN DO:
      RUN fillTTOilDrums IN h_dttOilDrums (iSucursal, 9, dFecha, iArticulo).
    END.
  END CASE.
  
  fiTambores:SCREEN-VALUE IN FRAME fMain = DYNAMIC-FUNCTION('getRowsNumber' IN h_dttOilDrums).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectPage fFrameWin 
PROCEDURE selectPage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER piPageNum AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT piPageNum).

  RUN refreshData.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setParams fFrameWin 
PROCEDURE setParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piArticulo AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTamb AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha    AS DATE       NO-UNDO.

  ASSIGN iArticulo = piArticulo
         iSucursal = piSucursal
         iTipoTamb = piTipoTamb
         dFecha    = pdFecha.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

