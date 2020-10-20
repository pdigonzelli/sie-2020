&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
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

DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.


DEFINE VARIABLE chTree  AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chImage AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chProg  AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chNode  AS COM-HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-2006 btn2004 cmbLotesJugo cmbLotesAceite ~
cmbProdJugo cmbTerceros btnPJ2006 btn2003 btn2005 btnAc2005 btnPJ2005 ~
fiLabel RECT-13 RECT-14 RECT-37 
&Scoped-Define DISPLAYED-OBJECTS cmbLotesJugo cmbLotesAceite cmbProdJugo ~
cmbTerceros fiLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queryName wWin 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-fMain 
       MENU-ITEM m_Query_Constructor LABEL "Query Constructor".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bttubicaciontambores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttubicaciontambores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_toolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-2006  NO-FOCUS FLAT-BUTTON
     LABEL "LJ 2004" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn2003  NO-FOCUS FLAT-BUTTON
     LABEL "LJ 2006" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn2004  NO-FOCUS FLAT-BUTTON
     LABEL "LA 2006" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn2005  NO-FOCUS FLAT-BUTTON
     LABEL "LJ 2005" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnAc2005  NO-FOCUS FLAT-BUTTON
     LABEL "LA 2005" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnPJ2005  NO-FOCUS FLAT-BUTTON
     LABEL "PJ 2005" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnPJ2006  NO-FOCUS FLAT-BUTTON
     LABEL "PJ 2006" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbLotesAceite AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Lotes Aceite" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "2004","2005","2006","2007","2008" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cmbLotesJugo AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Lotes Jugo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "2004","2005","2006","2007","2008" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cmbProdJugo AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Producciones Jugo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "2004","2005","2006","2007","2008" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cmbTerceros AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Terceros" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "2004","2005","2006","2007","2008" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiLabel AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 113 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 8.33.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 153 BY 12.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 98 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn-2006 AT ROW 10.52 COL 138
     btn2004 AT ROW 12.91 COL 138
     cmbLotesJugo AT ROW 2.57 COL 66.6 COLON-ALIGNED
     cmbLotesAceite AT ROW 2.57 COL 98.6 COLON-ALIGNED
     cmbProdJugo AT ROW 2.57 COL 134.8 COLON-ALIGNED
     cmbTerceros AT ROW 3.86 COL 135 COLON-ALIGNED
     btnPJ2006 AT ROW 16.48 COL 138
     btn2003 AT ROW 11.71 COL 138
     btn2005 AT ROW 14.1 COL 138
     btnAc2005 AT ROW 15.29 COL 138
     btnPJ2005 AT ROW 17.67 COL 138
     fiLabel AT ROW 9.57 COL 3 NO-LABEL
     RECT-13 AT ROW 1 COL 1
     RECT-14 AT ROW 9.81 COL 1
     RECT-37 AT ROW 2.43 COL 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 153.6 BY 20.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Trazabilidad Lotes"
         HEIGHT             = 20.81
         WIDTH              = 153.4
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 158
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 158
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME fMain
                                                                        */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

/* SETTINGS FOR FILL-IN fiLabel IN FRAME fMain
   ALIGN-L                                                              */
ASSIGN 
       fiLabel:AUTO-RESIZE IN FRAME fMain      = TRUE
       fiLabel:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 10.52
       COLUMN          = 2
       HEIGHT          = 10.95
       WIDTH           = 151
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 19.1
       COLUMN          = 144
       HEIGHT          = 1.91
       WIDTH           = 8
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
      RUN adjustTabOrder ( h_bttubicaciontambores , CtrlFrame , 'BEFORE':U ).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON CTRL-F12 OF wWin /* Trazabilidad Lotes */
DO:
  MESSAGE "entra" VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RUN xlprolite.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Trazabilidad Lotes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Trazabilidad Lotes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  RUN beforeExit.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-2006
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-2006 wWin
ON CHOOSE OF btn-2006 IN FRAME fMain /* LJ 2004 */
DO:
  RUN fillData IN h_dttUbicacionTambores (2004, 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2003
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2003 wWin
ON CHOOSE OF btn2003 IN FRAME fMain /* LJ 2006 */
DO:
  RUN fillData IN h_dttUbicacionTambores (2006, 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2004
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2004 wWin
ON CHOOSE OF btn2004 IN FRAME fMain /* LA 2006 */
DO:
  RUN fillData IN h_dttUbicacionTambores (2006, 6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2005
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2005 wWin
ON CHOOSE OF btn2005 IN FRAME fMain /* LJ 2005 */
DO:
  RUN fillData IN h_dttUbicacionTambores (2005, 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAc2005
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAc2005 wWin
ON CHOOSE OF btnAc2005 IN FRAME fMain /* LA 2005 */
DO:
  RUN fillData IN h_dttUbicacionTambores (2005, 6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPJ2005
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPJ2005 wWin
ON CHOOSE OF btnPJ2005 IN FRAME fMain /* PJ 2005 */
DO:
  RUN fillData IN h_dttUbicacionTambores (2005, 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPJ2006
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPJ2006 wWin
ON CHOOSE OF btnPJ2006 IN FRAME fMain /* PJ 2006 */
DO:
  RUN fillData IN h_dttUbicacionTambores (2006, 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbLotesAceite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbLotesAceite wWin
ON VALUE-CHANGED OF cmbLotesAceite IN FRAME fMain /* Lotes Aceite */
DO:
  RUN fillData IN h_dttUbicacionTambores (INTEGER(SELF:SCREEN-VALUE), 6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbLotesJugo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbLotesJugo wWin
ON VALUE-CHANGED OF cmbLotesJugo IN FRAME fMain /* Lotes Jugo */
DO:
  RUN fillData IN h_dttUbicacionTambores (INTEGER(SELF:SCREEN-VALUE), 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbProdJugo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbProdJugo wWin
ON VALUE-CHANGED OF cmbProdJugo IN FRAME fMain /* Producciones Jugo */
DO:
  RUN fillData IN h_dttUbicacionTambores (INTEGER(SELF:SCREEN-VALUE), 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbTerceros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbTerceros wWin
ON VALUE-CHANGED OF cmbTerceros IN FRAME fMain /* Terceros */
DO:
  RUN fillData IN h_dttUbicacionTambores (INTEGER(SELF:SCREEN-VALUE), 9).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Query_Constructor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Query_Constructor wWin
ON CHOOSE OF MENU-ITEM m_Query_Constructor /* Query Constructor */
DO:
    DEFINE VAR xSDOS        AS CHARACTER    NO-UNDO.
    DEFINE VAR i            AS INTEGER      NO-UNDO.
    DEFINE VAR iPage        AS INTEGER      NO-UNDO.
    DEFINE VAR iActualPage  AS INTEGER      NO-UNDO.
    DEFINE VAR xDataSource  AS CHARACTER    NO-UNDO.
    DEFINE VAR hDataSource  AS HANDLE       NO-UNDO.

    xSDOS = DYNAMIC-FUNCTION ('getSDO').
    {get CurrentPage iActualPage}.

    DO i = 1 TO NUM-ENTRIES(xSDOS):
        qh = WIDGET-HANDLE(ENTRY(i,xSDOS)).
        {get ObjectPage iPage qh}.
        {get DataSource xDataSource qh}.
        hDataSource = WIDGET-HANDLE(xDataSource).
        IF ( iPage = iActualPage OR iPage = 0 ) AND NOT valid-handle(hDataSource)THEN
            RUN adm2/support/wquery.w ( INPUT qh ).
        ELSE
            MESSAGE 'No puede ejecutar consulta en el detalle' VIEW-AS ALERT-BOX WARNING.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
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
             INPUT  'dttubicaciontambores.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch50CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedttubicaciontamboresUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dttubicaciontambores ).
       RUN repositionObject IN h_dttubicaciontambores ( 7.91 , 53.00 ) NO-ERROR.
       /* Size in AB:  ( 2.10 , 10.00 ) */

       RUN constructObject (
             INPUT  'bttubicaciontambores.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bttubicaciontambores ).
       RUN repositionObject IN h_bttubicaciontambores ( 5.29 , 56.00 ) NO-ERROR.
       RUN resizeObject IN h_bttubicaciontambores ( 4.05 , 98.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsBanda1,Banda3SubModulesTableIOTypeSupportedLinksToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdate,Filter,Excelaction,Customaction,Undo,Commit,First,Prev,Next,LastHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_toolbar ).
       RUN repositionObject IN h_toolbar ( 1.00 , 56.00 ) NO-ERROR.
       RUN resizeObject IN h_toolbar ( 1.24 , 98.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_lote,anio,id_sucursal,id_sucursal_ubicacion,id_articulo,nromovOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth15DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_sucursal_ubicacionSuc Ubicacionid_articuloCod Articuloid_loteLoteid_sucursalSuc CreacionFieldToolTipsFieldHelpIdsid_sucursal_ubicacion0anio0id_articulo0id_lote0id_sucursal0nromov0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 7.86 , 52.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dttubicaciontambores. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dttubicaciontambores ).

       /* Links to SmartDataBrowser h_bttubicaciontambores. */
       RUN addLink ( h_dttubicaciontambores , 'Data':U , h_bttubicaciontambores ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynfilter ,
             h_toolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_bttubicaciontambores ,
             cmbTerceros:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeExit wWin 
PROCEDURE beforeExit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libApiMenu.p').
  DELETE OBJECT hLibCom.
  
  RUN cleanUpInfoInParent IN hLib.

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
  {adm2/support/changePage.i}.  
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "wTrazaIndust.wrx":U ).
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
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wTrazaIndust.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createButtons wWin 
PROCEDURE createButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*Boton de trazabilidad*/
  RUN ToolBarButtonAdd.p (h_toolbar, 
                          "fillAction", 
                          "tlbFillTree", 
                          "Mostrar Trazabilidad", 
                          "im-sort.bmp", 
                          "tlbFillTree", 
                          "BANDA1").

  /*Boton de stock a fecha*/
  RUN ToolBarButtonAdd.p (h_toolbar, 
                          "stockAction", 
                          "Stock Fecha", 
                          "Stock a Fecha", 
                          "stock.bmp", 
                          "tlbStockFecha", 
                          "BANDA1").
  /*
  /*Boton de lotes 2004*/
  RUN ToolBarButtonAdd.p (h_toolbar, 
                          "fill2004Action", 
                          "LJ 2004", 
                          "Lotes de Jugo año 2004", 
                          "", 
                          "tlbFill2004", 
                          "NAVIGATION").

  /*Boton de lotes 2005*/
  RUN ToolBarButtonAdd.p (h_toolbar, 
                          "fill2005Action", 
                          "LJ 2005", 
                          "Lotes de Jugo año 2005", 
                          "", 
                          "tlbFill2005", 
                          "NAVIGATION").

  /*Boton de lotes Aceite 2005*/
  RUN ToolBarButtonAdd.p (h_toolbar, 
                          "fill2005AcAction", 
                          "LA 2005", 
                          "Lotes de Aceite año 2005", 
                          "", 
                          "tlbFill2005Aceite", 
                          "NAVIGATION").


  /*Boton de prods 2004*/
  RUN ToolBarButtonAdd.p (h_toolbar, 
                          "fill2004ProdsAction", 
                          "PJ 2004", 
                          "Producciones de Jugo año 2004", 
                          "", 
                          "tlbFill2004Prods", 
                          "NAVIGATION").
  
  /*Boton de prods 2005*/
  RUN ToolBarButtonAdd.p (h_toolbar, 
                          "fill2005ProdsAction", 
                          "PJ 2005", 
                          "Producciones de Jugo año 2005", 
                          "", 
                          "tlbFill2005Prods", 
                          "NAVIGATION").

  /*Producciones Aceite 2005*/
  RUN ToolBarButtonAdd.p (h_toolbar, 
                          "fill2005ProdAcAction", 
                          "PA 2005", 
                          "Producciones Aceite año 2005", 
                          "", 
                          "tlbFill2005ProdAc", 
                          "NAVIGATION").
  
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects wWin 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN createButtons.

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
  DISPLAY cmbLotesJugo cmbLotesAceite cmbProdJugo cmbTerceros fiLabel 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE btn-2006 btn2004 cmbLotesJugo cmbLotesAceite cmbProdJugo cmbTerceros 
         btnPJ2006 btn2003 btn2005 btnAc2005 btnPJ2005 fiLabel RECT-13 RECT-14 
         RECT-37 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTree wWin 
PROCEDURE fillTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       1. origen
                1.1 producciones
                1.2 lotes
               2. remitos
                2.1 manuales
                2.2 automaticos
               3. reprocesos
               4. ubicacion
               
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hNode     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cNodeText AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cText     AS CHARACTER  NO-UNDO.  
  DEFINE VARIABLE cProds    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cProd     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRepros   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRepro    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRtos     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSobs     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArras    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArr      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSob      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRto      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cUbis     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cUbi      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReleases AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRel      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAjustes  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAju      AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE i         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAcc      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAccKg    AS DECIMAL    NO-UNDO.

  fiLabel:SCREEN-VALUE IN FRAME fMain = "Lote " +
                                        STRING(DYNAMIC-FUNCTION('columnValue' IN h_dttUbicacionTambores, 'id_lote')) + 
                                        "/" + 
                                        STRING(DYNAMIC-FUNCTION('columnValue' IN h_dttUbicacionTambores, 'anio')) + 
                                        " " + 
                                        STRING(DYNAMIC-FUNCTION('columnValue' IN h_dttUbicacionTambores, 'articulo')) + 
                                        " " + 
                                        STRING(DYNAMIC-FUNCTION('columnValue' IN h_dttUbicacionTambores, 'calidad')) + 
                                        " x(" + 
                                        STRING(DYNAMIC-FUNCTION('columnValue' IN h_dttUbicacionTambores, 'cantidad')) + 
                                        ") " +
                                        STRING(DYNAMIC-FUNCTION('columnValue' IN h_dttUbicacionTambores, 'kilos')) + 
                                        " Kgs. el " +
                                        DYNAMIC-FUNCTION('columnValue' IN h_dttUbicacionTambores, 'fecha'). 
  
  chTree:Nodes:CLEAR().

  /******ORIGENES********/
  chNode  = chTree:Nodes:ADD(, , "Origen", "Origenes", "orig").  
  cProds = DYNAMIC-FUNCTION('getOrigenProducciones' IN h_dttUbicacionTambores).
  
  DO i = 1 TO NUM-ENTRIES(cProds, CHR(14)) - 1:
    cProd = ENTRY(i, cProds, CHR(14)).
    CASE ENTRY(7, cProd). /*id_tipotambor*/
      WHEN "1" THEN DO:
        cText = "Produccion".
        cIco  = "prod".
      END.
      WHEN "2" THEN DO:
        cText = "Produccion".
        cIco  = "prod".
      END.
      WHEN "3" THEN DO:
        cText = "Lote".
        cIco  = "lote".
      END.
      WHEN "4" THEN DO:
        cText = "Sobrante".
        cIco  = "prods".
      END.
      WHEN "5" THEN DO:
        cText = "Arrastre".
        cIco  = "arras".
      END.
      WHEN "6" THEN DO:
        cText = "Lote".
        cIco  = "lote".
      END.
      WHEN "7" THEN DO:
        cText = "Foldeado".
        cIco  = "fold".
      END.
      WHEN "8" THEN DO:
        cText = "Sobrante".
        cIco  = "prods".
      END.
      WHEN "9" THEN DO:
        cText = "Prod Terceros".
        cIco  = "lote".
      END.
      WHEN "10" THEN DO:
        cText = "Cargas".
        cIco  = "carg".
      END.      
    END CASE.

    iAcc   = iAcc + INTEGER(ENTRY(3, cProd)).
    iAccKg = iAccKg + DECIMAL(ENTRY(9, cProd)).

    cNodeText = cText + " " +   /*quedaria asi Produccion nro 32/2005 x(53) tambores de aceite de limon*/
                "nro: " + ENTRY(1, cProd) +             /*id_lote*/
                "/" + 
                ENTRY(2, cProd) +                       /*anio*/
                " x(" + 
                ENTRY(3, cProd) +                       /*cantidad*/
                ") tambores por " + 
                ENTRY(9, cProd) + " Kgs. " +            /*Kgs*/
                ENTRY(10, cProd) + " de " +             /*calidad*/
                ENTRY(4, cProd) + " el " +              /*articulo desc*/
                ENTRY(11, cProd).                       /*fecha*/
    cKey = "Prod" + STRING(ENTRY(8, cProd)).
    chNode = chTree:Nodes:ADD("Origen", 4, cKey, cNodeText, cIco).    

  END.
  chTree:Nodes("Origen"):TEXT = chTree:Nodes("Origen"):TEXT + " x(" + STRING(iAcc) + ") " + STRING(iAccKg) + " Kgs." .

  
  /******************** REMITOS *********************/
  chNode  = chTree:Nodes:ADD(, , "Rtos", "Remitos", "remi").
  iAcc    = 0.
  cRtos = DYNAMIC-FUNCTION('getRemitosInfo' IN h_dttUbicacionTambores).
  DO i = 1 TO NUM-ENTRIES(cRtos, CHR(14)) - 1:    
    cRto = ENTRY(i, cRtos, CHR(14)).
    iAcc = iAcc + INTEGER(ENTRY(4, cRto)).

    IF ENTRY(5, cRto) = "123" THEN
      cIco = "remito".
    IF ENTRY(5, cRto) = "122" THEN
      cIco = "manual".
    IF ENTRY(6, cRto) = "no" THEN 
      cIco = "anulado".

    cText     = "Remito ".
    cNodeText = cText + " " +                        /*quedaria asi Produccion nro 32/2005 x(53) tambores de aceite de limon*/
                "nro: " + ENTRY(9, cRto) +           /*nro_comp*/
                " desde " + 
                ENTRY(1, cRto) +                     /*sucursal*/
                " hasta " + 
                ENTRY(2, cRto) +                     /*suc destino*/
                " del " + 
                ENTRY(3, cRto) +                     /*fecha*/
                " x(" + 
                ENTRY(4, cRto) +                     /*cantidad*/ 
                ") tambores emitido por " + 
                ENTRY(7, cRto) +                      /*c_usuario*/
                " a " + 
                ENTRY(11, cRto) + 
                " con oe " + 
                ENTRY(10, cRto).
    cKey      = "Rto" + STRING(ENTRY(8, cRto)).
    chNode    = chTree:Nodes:ADD("Rtos", 4, cKey, cNodeText, cIco).    

  END.
  chTree:Nodes("Rtos"):TEXT = chTree:Nodes("Rtos"):TEXT + " x(" + STRING(i - 1) + ")".
  
  /**********AJUSTES OEs ***************/
  chNode  = chTree:Nodes:ADD(, , "AjustesOE", "Ajustes de OEs", "ajustes").
  cAjustes = DYNAMIC-FUNCTION('getAjustesOE' IN h_dttUbicacionTambores).

  DO i = 1 TO NUM-ENTRIES(cAjustes, CHR(14)):
    cAju = ENTRY(i, cAjustes, CHR(14)).
    cText = "Ajuste Nro: " + ENTRY(1, cAju) + " fecha " + ENTRY(7, cAju) + " desde OE " + ENTRY(2, cAju) + " parte " + ENTRY(3, cAju) 
          + " Rolleado a OE " + ENTRY(4, cAju) + " parte " + ENTRY(5, cAju) + " Realizado por " + ENTRY(6, cAju).
    cKey = "ajuste" + ENTRY(1, cAju).
      
    chNode = chTree:Nodes:ADD("AjustesOE", 4, cKey, cText, "ajte").
  END.
  

  /****************REPROCESOS***********/
  iAcc    = 0.
  iAccKg  = 0.
  chNode  = chTree:Nodes:ADD(, , "Reproc", "Reprocesos", "repro").
  cRepros = DYNAMIC-FUNCTION('getDestinoReproceso' IN h_dttUbicacionTambores).
  DO i = 1 TO NUM-ENTRIES(cRepros, CHR(14)) - 1:
    cRepro = ENTRY(i, cRepros, CHR(14)).
    iAcc   = iAcc + INTEGER(ENTRY(3, cRepro)).
    iAccKg = iAccKg + DECIMAL(ENTRY(12, cRepro)).
    cText  = "Lote ".
    cIco   = "lote".
    IF ENTRY(7, cRepro) = "10" THEN DO:
      cText = "Carga ".
      cIco  = "carga".
    END.

    cNodeText = cText + " " +   /*quedaria asi Produccion nro 32/2005 x(53) tambores de aceite de limon*/
                "nro: " + ENTRY(1, cRepro) +             /*id_lote*/
                "/" + 
                ENTRY(2, cRepro) +                       /*anio*/
                " x(" + 
                ENTRY(3, cRepro) +                       /*cantidad*/
                ") tambores por " + 
                ENTRY(12, cRepro) +                      /*kilos*/
                " Kgs de " + 
                ENTRY(4, cRepro) +                       /*articulo desc*/                
                " en planta " +   
                ENTRY(10, cRepro) +                       /*sucursal*/
                " proceso nro: " + 
                ENTRY(11, cRepro) .

    cKey = "Lot" + STRING(ENTRY(8, cRepro)).
    chNode = chTree:Nodes:ADD("Reproc", 4, cKey, cNodeText, cIco).    
    hNode = chTree:Nodes:ADD(cKey, 4, cKey + "f" + ENTRY(14, cRepro), "Fecha Creacion: " + ENTRY(14, cRepro), "fechas").
    hNode = chTree:Nodes:ADD(cKey, 4, cKey + "i" + ENTRY(15, cRepro), "Fecha Creacion Real: " + ENTRY(15, cRepro), "fechas").
    hNode = chTree:Nodes:ADD(cKey, 4, cKey + "c" + ENTRY(16, cRepro), "Fecha Cierre: " + ENTRY(16, cRepro), "fechas").
    hNode = chTree:Nodes:ADD(cKey, 4, cKey + "r" + ENTRY(16, cRepro), "Fecha Volcado: " + ENTRY(9, cRepro), "fechas").
  END.
  chTree:Nodes("Reproc"):TEXT = chTree:Nodes("Reproc"):TEXT + " x(" + STRING(iAcc) + ") por " + STRING(iAccKg) + " Kgs".


  /****************UBICACION***********/
  chNode  = chTree:Nodes:ADD(, , "Ubic", "Stock", "stoc").
  iAcc    = 0.
  iAccKg  = 0.
  cUbis   = DYNAMIC-FUNCTION('getUbicacionTambores' IN h_dttUbicacionTambores).
  DO i = 1 TO NUM-ENTRIES(cUbis, CHR(14)) - 1:
    cUbi = ENTRY(i, cUbis, CHR(14)).
    IF (INTEGER(ENTRY(5, cUbi)) <> 10) AND (INTEGER(ENTRY(5, cUbi)) <> 8) THEN DO:
      iAcc = iAcc + INTEGER(ENTRY(2, cUbi)).
      iAccKg = iAccKg + INTEGER(ENTRY(3, cUbi)).
    END.
    cText = "En".
    cNodeText = ENTRY(2, cUbi) +                             /*cantidad*/
                " tambores en suc " + ENTRY(1, cUbi) +        /*sucursal*/
                " por " + ENTRY(3, cUbi) + " Kgs.".           /*kilos*/
    cKey = "Ubi" + STRING(ENTRY(1, cUbi)) + "can" + ENTRY(2, cUbi).
    chNode = chTree:Nodes:ADD("Ubic", 4, cKey, cNodeText, "lote").    

  END.
  chTree:Nodes("Ubic"):TEXT = chTree:Nodes("Ubic"):TEXT + " x(" + STRING(iAcc) + ") por " + STRING(iAccKg) + " Kgs.".

  
  /****************DERIVADOS***********/
  chNode = chTree:Nodes:ADD(, , "Extra", "Derivados", "extra").
  
  /*arrastre*/
  iAcc   = 0.
  iAccKg = 0.  
  chNode = chTree:Nodes:ADD("Extra", 4, "extraarras", "Arrastre", "arras").    
  cArras = DYNAMIC-FUNCTION('getArrastreInfo' IN h_dttUbicacionTambores).  
  DO i = 1 TO NUM-ENTRIES(cArras, CHR(14)) - 1:
    cArr = ENTRY(i, cArras, CHR(14)) .
    cNodeText = "Arrastre Nro " + 
                ENTRY(1, cArr) + 
                " de Lote " + 
                ENTRY(4, cArr) + 
                " x(" + 
                ENTRY(2, cArr) + 
                ") tambores por " + 
                ENTRY(3, cArr) + 
                " Kgs".
    cKey = "ExtraArr" + ENTRY(1, cArr).
    chNode = chTree:Nodes:ADD("extraarras", 4, cKey, cNodeText, "lote").    
    iAcc = iAcc + INTEGER(ENTRY(2, cArr)).
    iAccKg = iAccKg + DECIMAL(ENTRY(3, cArr)).
  END.
  
  chTree:Nodes("extraarras"):TEXT = chTree:Nodes("extraarras"):TEXT + " x(" + STRING(iAcc) + ") por " + STRING(iAccKg) + " Kgs".
 

  /*sobrante*/
  iAcc   = 0.
  iAccKg = 0.
  chNode = chTree:Nodes:ADD("Extra", 4, "extrasob", "Sobrante", "sbte").    
  cSobs = DYNAMIC-FUNCTION('getSobranteInfo' IN h_dttUbicacionTambores).  
  DO i = 1 TO NUM-ENTRIES(cSobs, CHR(14)) - 1:
    cSob = ENTRY(i, cSobs, CHR(14)) .
    cNodeText = "Sobrante Nro " + 
                ENTRY(1, cSob) + 
                " de Lote " + 
                ENTRY(4, cSob) + 
                " x(" + 
                ENTRY(2, cSob) + 
                ") tambores por " + 
                ENTRY(3, cSob) + 
                " Kgs".
    cKey = "ExtraSob" + ENTRY(1, cSob).
    chNode = chTree:Nodes:ADD("extrasob", 4, cKey, cNodeText, "prods").    
    iAcc = iAcc + INTEGER(ENTRY(2, cSob)).
    iAccKg = iAccKg + DECIMAL(ENTRY(3, cSob)).
  END.
  
  chTree:Nodes("extrasob"):TEXT = chTree:Nodes("extrasob"):TEXT + " x(" + STRING(iAcc) + ") por " + STRING(iAccKg) + " Kgs".


  /****************RELEASES***********/
  chNode  = chTree:Nodes:ADD(, , "Rels", "Releases", "rels").
  iAcc = 0.
  cReleases = DYNAMIC-FUNCTION('getReleasesInfo' IN h_dttUbicacionTambores).
  
  DO i = 1 TO NUM-ENTRIES(cReleases, CHR(14)) - 1:
    cRel = ENTRY(i, cReleases, CHR(14)).
    iAcc = iAcc + INTEGER(ENTRY(5, cRel)).
    cNodeText = "Rel." + 
                ENTRY(1, cRel) +  
                " desde " + 
                ENTRY(2, cRel) + 
                " para " + 
                ENTRY(3, cRel) + 
                " lote dep nro. " +
                ENTRY(4, cRel) + 
                " x(" + 
                ENTRY(5, cRel) + 
                ") tambores con fecha " +
                ENTRY(6, cRel) + 
                " procesado el dia " + 
                ENTRY(7, cRel).
    
    cKey = "Rel" + STRING(ENTRY(1, cRel)) + "can" + ENTRY(5, cRel).
    chNode = chTree:Nodes:ADD("Rels", 4, cKey, cNodeText, "remito").    

  END.
  chTree:Nodes("Rels"):TEXT = chTree:Nodes("Rels"):TEXT + " x(" + STRING(iAcc) + ")".




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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN loadImages.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "fillAction").  
  SUBSCRIBE TO "tlbFillTree" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "stockAction").  
  SUBSCRIBE TO "tlbStockFecha" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "fill2004Action").  
  SUBSCRIBE TO "tlbFill2004" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "fill2005Action").  
  SUBSCRIBE TO "tlbFill2005" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "fill2004ProdsAction").  
  SUBSCRIBE TO "tlbFill2004Prods" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "fill2005ProdsAction").  
  SUBSCRIBE TO "tlbFill2005Prods" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "fill2005AcAction").  
  SUBSCRIBE TO "tlbFill2005Aceite" IN h_toolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_toolbar, "fill2005ProdAcAction").  
  SUBSCRIBE TO "tlbFill2005ProdAc" IN h_toolbar.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadImages wWin 
PROCEDURE loadImages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  chTree  = chCtrlFrame:TreeView.
  chImage = chCtrlFrame-2:ImageList.
  
  chImage:ImageHeight = 16.
  chImage:ImageWidth  = 16.

  chImage:ListImages:Add(1, "carga", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\172.bmp")).
  chImage:ListImages:Add(2, "orig", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\450.bmp")).
  chImage:ListImages:Add(3, "stoc", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\sqlexp.bmp")).
  chImage:ListImages:Add(4, "rels", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\463.bmp")).
  chImage:ListImages:Add(5, "prod", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\998.bmp")).
  chImage:ListImages:Add(6, "manual", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\170.bmp")).
  chImage:ListImages:Add(7, "remito", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\211.bmp")).
  chImage:ListImages:Add(8, "anulado", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\296.bmp")).
  chImage:ListImages:Add(9, "prods", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\1180.bmp")).
  chImage:ListImages:Add(10, "extra", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\1015.bmp")).
  chImage:ListImages:Add(11, "arras", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\1145.bmp")).
  chImage:ListImages:Add(12, "sbte", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\1137.bmp")).
  chImage:ListImages:Add(13, "lote", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\startdbs.bmp")).
  chImage:ListImages:Add(14, "remi", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\TRUCK02H.bmp")).
  chImage:ListImages:Add(15, "fechas", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\671.bmp")).
  chImage:ListImages:Add(16, "repro", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\096.bmp")).
  chImage:ListImages:Add(16, "ajustes", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\446.bmp")).
  chImage:ListImages:Add(16, "ajte", LOAD-PICTURE("src\adm2\image\iconos bmp fondo blanco\redo.bmp")).

  chTree:ImageList = chImage.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExit wWin 
PROCEDURE tlbExit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN beforeExit.
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbFill2004 wWin 
PROCEDURE tlbFill2004 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillData IN h_dttUbicacionTambores (2004, 3).
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbFill2004Prods wWin 
PROCEDURE tlbFill2004Prods :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillData IN h_dttUbicacionTambores (2004, 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbFill2005 wWin 
PROCEDURE tlbFill2005 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillData IN h_dttUbicacionTambores (2005, 3).
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbFill2005Aceite wWin 
PROCEDURE tlbFill2005Aceite :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillData IN h_dttUbicacionTambores (2005, 6).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbFill2005ProdAc wWin 
PROCEDURE tlbFill2005ProdAc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillData IN h_dttUbicacionTambores (2005, 2).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbFill2005Prods wWin 
PROCEDURE tlbFill2005Prods :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillData IN h_dttUbicacionTambores (2005, 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbFillTree wWin 
PROCEDURE tlbFillTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillTree.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbPrint wWin 
PROCEDURE tlbPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbStockFecha wWin 
PROCEDURE tlbStockFecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wStockTambores.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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

