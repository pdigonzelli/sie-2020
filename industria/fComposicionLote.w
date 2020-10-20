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
&Scoped-Define ENABLED-OBJECTS btnFilter cmbOps btnReset btnAddClause ~
btnDelClause fiValue lstFilter radFilter btnAdd btnDel RECT-24 RECT-25 ~
RECT-26 
&Scoped-Define DISPLAYED-OBJECTS fiTambores cmbOps fiValue lstFilter ~
radFilter fiKilos fiLitros fiBrix fiAcidez fiSodio 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowIdsList fFrameWin 
FUNCTION getRowIdsList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bttdrums AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttdrums AS HANDLE NO-UNDO.
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

DEFINE VARIABLE fiAcidez AS CHARACTER FORMAT "X(256)":U 
     LABEL "Acidez" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE fiBrix AS CHARACTER FORMAT "X(256)":U 
     LABEL "Brix" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE fiKilos AS CHARACTER FORMAT "X(256)":U 
     LABEL "kilos" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE fiLitros AS CHARACTER FORMAT "X(256)":U 
     LABEL "Litros" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE fiSodio AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sodio" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE fiTambores AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .95 NO-UNDO.

DEFINE VARIABLE fiValue AS CHARACTER FORMAT "x(256)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 NO-UNDO.

DEFINE VARIABLE radFilter AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Brix", 1,
"Acidez", 2,
"Sodio", 3,
"Lote/Prod", 4,
"Tambor", 5,
"Articulo", 6
     SIZE 14 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 4.29.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 6 BY 2.62.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 1.19.

DEFINE VARIABLE lstFilter AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 2.81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnFilter AT ROW 1.71 COL 63
     fiTambores AT ROW 5.52 COL 68 RIGHT-ALIGNED NO-LABEL
     cmbOps AT ROW 2.76 COL 15 COLON-ALIGNED NO-LABEL
     btnReset AT ROW 3.43 COL 63.2
     btnAddClause AT ROW 1.71 COL 33.4
     btnDelClause AT ROW 3.38 COL 33.4
     fiValue AT ROW 2.76 COL 22.4 COLON-ALIGNED NO-LABEL
     lstFilter AT ROW 1.76 COL 39 NO-LABEL
     radFilter AT ROW 1.24 COL 2 NO-LABEL
     btnAdd AT ROW 9.43 COL 70.6
     btnDel AT ROW 10.67 COL 70.6
     fiKilos AT ROW 14.57 COL 12 RIGHT-ALIGNED
     fiLitros AT ROW 14.57 COL 27.2 RIGHT-ALIGNED
     fiBrix AT ROW 14.57 COL 38 RIGHT-ALIGNED
     fiAcidez AT ROW 14.57 COL 53 RIGHT-ALIGNED
     fiSodio AT ROW 14.57 COL 67 RIGHT-ALIGNED
     RECT-24 AT ROW 1 COL 1
     RECT-25 AT ROW 9.33 COL 70.2
     RECT-26 AT ROW 14.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.6 BY 14.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Design Page: 1
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
         HEIGHT             = 14.52
         WIDTH              = 75.6.
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
   NOT-VISIBLE Custom                                                   */
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

/* SETTINGS FOR FILL-IN fiAcidez IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiBrix IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiKilos IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiLitros IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiSodio IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTambores IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
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
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.

  {get ContainerSource hCont}.
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wLotesJugo.w*" THEN DO:
    cMes = getRowIdsList().    
    RUN addDrumsToBatch IN hCont (cMes).
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
      cFilterStr = "brix;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 2 THEN
      cFilterStr = "acidez;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 3 THEN
      cFilterStr = "sodio;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 4 THEN
      cFilterStr = "id_lote_prod;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 5 THEN
      cFilterStr = "id_tambor;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 6 THEN
      cFilterStr = "id_articulo;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
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
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wLotesJugo.w*" THEN DO:
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

  DYNAMIC-FUNCTION('assignQuerySelection' IN h_dttDrums, cColumns, cValues, cOperators).
  DYNAMIC-FUNCTION('openQuery' IN h_dttDrums).
  fiTambores:SCREEN-VALUE IN FRAME fMain = STRING(DYNAMIC-FUNCTION('getRowsNumber' IN h_dttDrums)).
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
  
  cQry = DYNAMIC-FUNCTION('getQueryString' IN h_dttDrums).
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dttDrums, cQry).    
  DYNAMIC-FUNCTION('openQuery' IN h_dttDrums).
  fiTambores:SCREEN-VALUE IN FRAME fMain = STRING(DYNAMIC-FUNCTION('getRowsNumber' IN h_dttDrums)).
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
             INPUT  'dttdrums.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedttdrumsUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dttdrums ).
       RUN repositionObject IN h_dttdrums ( 13.38 , 67.00 ) NO-ERROR.
       /* Size in AB:  ( 2.10 , 9.60 ) */

       RUN constructObject (
             INPUT  'bttdrums.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bttdrums ).
       RUN repositionObject IN h_bttdrums ( 6.71 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bttdrums ( 7.38 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Produccion|Sobrante|Arrastre|Lote|Terceros' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 5.52 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 8.81 , 68.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bttdrums. */
       RUN addLink ( h_dttdrums , 'Data':U , h_bttdrums ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bttdrums ,
             fiTambores:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_bttdrums , 'AFTER':U ).
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
  

  cRows = DYNAMIC-FUNCTION('getSelectedRows' IN h_bttDrums).
  cRet  = DYNAMIC-FUNCTION('getSumSelectedValues' IN h_dttDrums, cRows).

  fiKilos:SCREEN-VALUE IN FRAME fMain  = ENTRY(1, cRet, CHR(1)).
  fiLitros:SCREEN-VALUE IN FRAME fMain = ENTRY(2, cRet, CHR(1)).
  fiBrix:SCREEN-VALUE IN FRAME fMain   = ENTRY(3, cRet, CHR(1)).
  fiAcidez:SCREEN-VALUE IN FRAME fMain = ENTRY(4, cRet, CHR(1)).
  fiSodio:SCREEN-VALUE IN FRAME fMain  = ENTRY(5, cRet, CHR(1)).


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
  DISPLAY fiTambores cmbOps fiValue lstFilter radFilter fiKilos fiLitros fiBrix 
          fiAcidez fiSodio 
      WITH FRAME fMain.
  ENABLE btnFilter cmbOps btnReset btnAddClause btnDelClause fiValue lstFilter 
         radFilter btnAdd btnDel RECT-24 RECT-25 RECT-26 
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

  fiTambores:SCREEN-VALUE IN FRAME fMain = STRING(DYNAMIC-FUNCTION('getRowsNumber' IN h_dttDrums)).

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
  DEFINE VARIABLE cQry     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPageNum AS INTEGER    NO-UNDO.

  
  iPageNum = DYNAMIC-FUNCTION('getCurrentPage').

  IF iPageNum = 0 THEN RETURN.
 
  CASE iPageNum:
    WHEN 1 THEN DO:
      RUN fillTTDrums IN h_dttDrums (iSucursal, 1, dFecha).
    END.
    WHEN 2 THEN DO:
      RUN fillTTDrums IN h_dttDrums (iSucursal, 4, dFecha).
    END.
    WHEN 3 THEN DO:
      RUN fillTTDrums IN h_dttDrums (iSucursal, 5, dFecha).
    END.
    WHEN 4 THEN DO:
      RUN fillTTDrums IN h_dttDrums (iSucursal, 3, dFecha).
    END.
    WHEN 5 THEN DO:
      RUN fillTTDrums IN h_dttDrums (iSucursal, 9, dFecha).
    END.
  END CASE.
  
  fiTambores:SCREEN-VALUE IN FRAME fMain = STRING(DYNAMIC-FUNCTION('getRowsNumber' IN h_dttDrums)).
  

END PROCEDURE.

  
  
  
  /*
  /*cambio la query del sdo segun la pagina seleccionada*/
  CASE iPageNum:
    WHEN 1 THEN DO: /*producciones*/
      cQry = "FOR EACH tambores_industria WHERE " + 
             "tambores_industria.fecha >= DATE('01/01/2001') AND " + 
             "tambores_industria.fecha <= " + STRING(dFecha) + " AND " + 
             "tambores_industria.id_tipotambor = 1 AND " + 
             "tambores_industria.id_locacion_ubicacion = 4 AND " +
             "tambores_industria.id_sucursal_ubicacion = " + STRING(iSucursal).
    END.
    WHEN 2 THEN DO: /*sobrante*/
      cQry = "FOR EACH tambores_industria WHERE " + 
             "tambores_industria.fecha >= DATE('01/01/2001') AND " + 
             "tambores_industria.fecha <= " + STRING(dFecha) + " AND " + 
             "tambores_industria.id_sucursal_destino = 0 AND " +
             "tambores_industria.id_tipotambor_destino = 0 AND " +
             "tambores_industria.nromov_destino = 0 AND " + 
             "tambores_industria.id_tipotambor = 4 AND " + 
             "tambores_industria.id_locacion_ubicacion = 4 AND " +
             "tambores_industria.id_sucursal_ubicacion = " + STRING(iSucursal).
    END.
    WHEN 3 THEN DO: /*arrastre*/
      cQry = "FOR EACH tambores_industria WHERE " + 
             "tambores_industria.fecha >= DATE('01/01/2001') AND " + 
             "tambores_industria.fecha <= " + STRING(dFecha) + " AND " + 
             "tambores_industria.id_tipotambor = 5 AND " + 
             "tambores_industria.id_locacion_ubicacion = 4 AND " +
             "tambores_industria.id_sucursal = " + STRING(iSucursal).
    END.
    WHEN 4 THEN DO: /*lotes*/
      cQry = "FOR EACH tambores_industria WHERE " + 
             "tambores_industria.fecha >= DATE('01/01/2001') AND " + 
             "tambores_industria.fecha <= " + STRING(dFecha) + " AND " + 
             "tambores_industria.id_tipotambor = 3 AND " + 
             "tambores_industria.id_locacion_ubicacion = 4 AND " +
             "tambores_industria.id_sucursal_ubicacion = " + STRING(iSucursal) + " AND " + 
             "(" + DYNAMIC-FUNCTION('getArticulosMateriaPrima' IN h_dCompoLoteJugo, iArticulo) + ")". 
    END.
    WHEN 5 THEN DO:
      cQry = "FOR EACH tambores_industria WHERE " + 
             "tambores_industria.fecha >= DATE('01/01/2001') AND " + 
             "tambores_industria.fecha <= " + STRING(dFecha) + " AND " + 
             "tambores_industria.id_tipotambor = 9 AND " + 
             "tambores_industria.id_locacion_ubicacion = 4 AND " +
             "tambores_industria.id_sucursal_ubicacion = " + STRING(iSucursal).
    END.
  END CASE.

  DYNAMIC-FUNCTION('setQueryWhere' IN h_dCompoLoteJugo, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dCompoLoteJugo).

  fiTambores:SCREEN-VALUE IN FRAME fMain = STRING(DYNAMIC-FUNCTION('getRowsNumber' IN h_dCompoLoteJugo)).
  */

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

  /* Code placed here will execute AFTER standard behavior.    */
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowIdsList fFrameWin 
FUNCTION getRowIdsList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRows AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet  AS CHARACTER  NO-UNDO.
  
  cRows = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bttDrums).
  cRet  = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_dttDrums, cRows).
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

