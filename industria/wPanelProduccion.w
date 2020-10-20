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

DEFINE VARIABLE chGraph AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE dFec1   AS DATE       NO-UNDO.
DEFINE VARIABLE dFec2   AS DATE       NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS fiSemana fiAnio btnFilter RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fiSemana fiAnio 

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

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bbalanzatickets AS HANDLE NO-UNDO.
DEFINE VARIABLE h_blotesaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_blotesjugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bregistroproduccioncascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dbalanzatickets AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotesaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotesjugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dregistroproduccioncascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ftotalesaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ftotalescascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ftotalesfruta AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ftotalesproduccion AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFilter 
     LABEL "Consultar" 
     SIZE 15 BY 2.86.

DEFINE VARIABLE fiAnio AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Anio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiSemana AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Semana" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 7.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiSemana AT ROW 3.62 COL 17 COLON-ALIGNED
     fiAnio AT ROW 5.05 COL 17 COLON-ALIGNED
     btnFilter AT ROW 3.38 COL 39
     RECT-1 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.4 BY 23.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Panel de Info de Produccion"
         HEIGHT             = 23.52
         WIDTH              = 143.4
         MAX-HEIGHT         = 33
         MAX-WIDTH          = 203.2
         VIRTUAL-HEIGHT     = 33
         VIRTUAL-WIDTH      = 203.2
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
   Custom                                                               */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1
       COLUMN          = 61
       HEIGHT          = 8.57
       WIDTH           = 83
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {827E9F50-96A4-11CF-823E-000021570103} type: Graph */
      CtrlFrame:MOVE-AFTER(btnFilter:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Panel de Info de Produccion */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Panel de Info de Produccion */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  RUN beforeExit.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter wWin
ON CHOOSE OF btnFilter IN FRAME fMain /* Consultar */
DO:
  DEFINE VARIABLE cFec    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  cFec = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, INTEGER(fiSemana:SCREEN-VALUE), 
                                                        INTEGER(fiAnio:SCREEN-VALUE)).

  IF cFec NE ',' THEN DO:
    ASSIGN dFec1 = DATE(ENTRY(1, cFec))
           dFec2 = DATE(ENTRY(2, cFec))
           .
  END.
  ELSE DO:  
    MESSAGE "Verifique la Semana que Desea Consultar"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  
  SELF:SENSITIVE = FALSE.
  RUN runQueries (dFec1, dFec2).
  SELF:SENSITIVE = TRUE.

 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.PropertiesSet
PROCEDURE CtrlFrame.Graph.PropertiesSet .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/



END PROCEDURE.

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
             INPUT  'dregistroproduccioncascara.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedregistroproduccioncascaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dregistroproduccioncascara ).
       RUN repositionObject IN h_dregistroproduccioncascara ( 7.67 , 34.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dlotesjugo.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotesjugoUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dlotesjugo ).
       RUN repositionObject IN h_dlotesjugo ( 7.67 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dbalanzatickets.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch400CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedbalanzaticketsUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dbalanzatickets ).
       RUN repositionObject IN h_dbalanzatickets ( 7.67 , 12.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dlotesaceite.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotesaceiteUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dlotesaceite ).
       RUN repositionObject IN h_dlotesaceite ( 7.67 , 23.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Lotes Jugo|Ingreso de Fruta|Lotes Aceite|Cascara' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 10.05 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 14.29 , 143.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsBanda1SubModulesTableIOTypeSupportedLinksToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 59.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('4,1,2,3') NO-ERROR.

       /* Links to SmartDataObject h_dregistroproduccioncascara. */
       RUN addLink ( h_dynfilter-3 , 'Filter':U , h_dregistroproduccioncascara ).

       /* Links to SmartDataObject h_dlotesjugo. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dlotesjugo ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_dlotesjugo ).

       /* Links to SmartDataObject h_dbalanzatickets. */
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_dbalanzatickets ).

       /* Links to SmartDataObject h_dlotesaceite. */
       RUN addLink ( h_dynfilter-2 , 'Filter':U , h_dlotesaceite ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'blotesjugo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotesjugo ).
       RUN repositionObject IN h_blotesjugo ( 11.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_blotesjugo ( 12.86 , 81.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ftotalesproduccion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ftotalesproduccion ).
       RUN repositionObject IN h_ftotalesproduccion ( 17.19 , 83.00 ) NO-ERROR.
       /* Size in AB:  ( 7.05 , 57.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 11.24 , 84.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 59.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_articulo,id_calidad,id_contrato_ofOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_calidadCalidadid_loteLoteFieldToolTipsFieldHelpIdsanio0id_articulo0id_calidad0id_contrato_of0id_lote0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 12.67 , 84.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.43 , 59.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_blotesjugo. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_blotesjugo ).

       /* Links to SmartFrame h_ftotalesproduccion. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_ftotalesproduccion ).
       RUN addLink ( h_folder , 'Page':U , h_ftotalesproduccion ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'bbalanzatickets.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bbalanzatickets ).
       RUN repositionObject IN h_bbalanzatickets ( 11.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bbalanzatickets ( 12.86 , 69.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 11.24 , 72.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 1.24 , 71.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ftotalesfruta.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ftotalesfruta ).
       RUN repositionObject IN h_ftotalesfruta ( 12.91 , 72.00 ) NO-ERROR.
       /* Size in AB:  ( 10.33 , 70.80 ) */

       /* Links to SmartDataBrowser h_bbalanzatickets. */
       RUN addLink ( h_dbalanzatickets , 'Data':U , h_bbalanzatickets ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'blotesaceite.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotesaceite ).
       RUN repositionObject IN h_blotesaceite ( 11.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_blotesaceite ( 12.86 , 87.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ftotalesaceite.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ftotalesaceite ).
       RUN repositionObject IN h_ftotalesaceite ( 16.71 , 91.00 ) NO-ERROR.
       /* Size in AB:  ( 7.33 , 52.20 ) */

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_articulo,id_sucursalOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsDesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-2 ).
       RUN repositionObject IN h_dynfilter-2 ( 11.24 , 90.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-2 ( 3.71 , 53.40 ) NO-ERROR.

       /* Links to SmartDataBrowser h_blotesaceite. */
       RUN addLink ( h_dlotesaceite , 'Data':U , h_blotesaceite ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'bregistroproduccioncascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bregistroproduccioncascara ).
       RUN repositionObject IN h_bregistroproduccioncascara ( 11.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bregistroproduccioncascara ( 12.86 , 73.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_linea_produccion,id_turnoOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_linea_produccionlineaid_turnoturnoFieldToolTipsFieldHelpIdsid_linea_produccion0id_turno0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-3 ).
       RUN repositionObject IN h_dynfilter-3 ( 11.24 , 76.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-3 ( 3.71 , 66.40 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ftotalescascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ftotalescascara ).
       RUN repositionObject IN h_ftotalescascara ( 16.00 , 76.00 ) NO-ERROR.
       /* Size in AB:  ( 7.67 , 66.40 ) */

       /* Links to SmartDataBrowser h_bregistroproduccioncascara. */
       RUN addLink ( h_dregistroproduccioncascara , 'Data':U , h_bregistroproduccioncascara ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 4 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

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

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
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

OCXFile = SEARCH( "wPanelProduccion.wrx":U ).
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
ELSE MESSAGE "wPanelProduccion.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createControls wWin 
PROCEDURE createControls :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

   

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
  DISPLAY fiSemana fiAnio 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fiSemana fiAnio btnFilter RECT-1 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillGraph wWin 
PROCEDURE fillGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phSdo  AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcArts AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcLegs AS CHARACTER  NO-UNDO.


  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE p     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRowC AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRowT AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRowP AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRowF AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLeg  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatC AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatT AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatP AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDatF AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dVal  AS DECIMAL    NO-UNDO.
  
  chGraph:DataReset     = 9.
  chGraph:GraphType     = 6.
  chGraph:Hot           = 1.
  chGraph:GridStyle     = 1.
  chGraph:BACKGROUND    = 7.
  chGraph:GridStyle     = 3.
  chGraph:GridLineStyle = 2.
  chGraph:ForeGroundUse = 7.
  chGraph:FOREGROUND    = 8.
  chGraph:DataLabels    = 1.
  chGraph:ForeGroundUse = 13.
  chGraph:FOREGROUND    = 0.
  chGraph:ThickLines    = 1.
  chGraph:True3D        = 0.
  chGraph:NumSets       = 3.
  chGraph:NumPoints     = 7.
  chGraph:LabelStyle    = 0.
  chGraph:FontUse       = 2.
  chGraph:FontSize      = 70.
  chGraph:FontUse       = 3.
  chGraph:FontSize      = 80.

  /*chGraph:PropertyPages = 1.*/
  
  /*chGraph:GraphTitle = "Lotes " + vcSuc.*/


  cDatC = DYNAMIC-FUNCTION('getDatosGraficoProduccion' IN phSdo, INTEGER(ENTRY(1, pcArts))).
  cDatT = DYNAMIC-FUNCTION('getDatosGraficoProduccion' IN phSdo, INTEGER(ENTRY(2, pcArts))).
  cDatP = DYNAMIC-FUNCTION('getDatosGraficoProduccion' IN phSdo, INTEGER(ENTRY(3, pcArts))).
  /*cDatF = DYNAMIC-FUNCTION('getDatosGrafico' IN h_dBalanzaTickets, 1).*/
  
  j = dFec2 - dFec1.

  DO i = 0 TO j :
    cLeg = STRING(dFec1 + i).
    chGraph:LABEL(i + 1) = cLeg.

    /*set 1*/
    DO p = 1 TO NUM-ENTRIES(cDatC, CHR(10)).
      cRowC = ENTRY(p, cDatC, CHR(10)).
      IF ENTRY(1, cRowC) = cLeg THEN DO:      
        chGraph:ThisSet = 1.
        chGraph:ThisPoint = i + 1.
        chGraph:Data(i + 1) = DECIMAL(ENTRY(2, cRowC)).
        chGraph:ThickLines = 1.
        LEAVE.
      END.
    END.

    /*set 2*/
    DO p = 1 TO NUM-ENTRIES(cDatT, CHR(10)).
      cRowT = ENTRY(p, cDatT, CHR(10)).
      IF ENTRY(1, cRowT) = cLeg THEN DO:      
        chGraph:ThisSet = 2.
        chGraph:ThisPoint = i + 1.
        chGraph:Data(i + 1) = DECIMAL(ENTRY(2, cRowT)).
        chGraph:ThickLines = 1.
        LEAVE.
      END.
    END.

    /*set 3*/
    DO p = 1 TO NUM-ENTRIES(cDatP, CHR(10)).
      cRowP = ENTRY(p, cDatP, CHR(10)).
      IF ENTRY(1, cRowP) = cLeg THEN DO:      
        chGraph:ThisSet = 3.
        chGraph:ThisPoint = i + 1.
        chGraph:Data(i + 1) = DECIMAL(ENTRY(2, cRowP)).
        chGraph:ThickLines = 1.
        LEAVE.
      END.
    END.
/*
    /*fruta*/
    DO p = 1 TO NUM-ENTRIES(cDatF, CHR(10)).
      cRowF = ENTRY(p, cDatF, CHR(10)).
      IF ENTRY(1, cRowF) = cLeg THEN DO:      
        chGraph:ThisSet = 4.
        chGraph:ThisPoint = i + 1.
        chGraph:Data(i + 1) = DECIMAL(ENTRY(2, cRowF)).
        LEAVE.
      END.
    END.
*/
  END.

  chGraph:Legend(1) = ENTRY(1, pcLegs).
  chGraph:Legend(2) = ENTRY(2, pcLegs).
  chGraph:Legend(3) = ENTRY(3, pcLegs).
  /*chGraph:Legend(4) = "Fruta".*/
  
  chGraph:DrawMode = 2.

END PROCEDURE.

  /*
  chGraph:ThisSet = 1.
  chGraph:Legend(1) = "Claro".
  DO i = 1 TO NUM-ENTRIES(cDat, CHR(10)):
    cRow              = ENTRY(i, cDat, CHR(10)).
    chGraph:Label(i)  = ENTRY(1, cRow).
    chGraph:Data(i)   = DECIMAL(ENTRY(2, cRow)).
  END.

  cDat = DYNAMIC-FUNCTION('getDatosGraficoProduccion' IN h_dLotesJugo, 52).
  chGraph:ThisSet = 2.
  chGraph:Legend(2) = "Turbio".
  DO i = 1 TO NUM-ENTRIES(cDat, CHR(10)):
    cRow              = ENTRY(i, cDat, CHR(10)).
    chGraph:Label(i)  = ENTRY(1, cRow).
    chGraph:Data(i)   = DECIMAL(ENTRY(2, cRow)).
  END.

  cDat = DYNAMIC-FUNCTION('getDatosGraficoProduccion' IN h_dLotesJugo, 71).
  chGraph:ThisSet = 3.
  chGraph:Legend(3) = "Pulpa".
  DO i = 1 TO NUM-ENTRIES(cDat, CHR(10)):
    cRow              = ENTRY(i, cDat, CHR(10)).
    chGraph:Label(i)  = ENTRY(1, cRow).    
    chGraph:Data(i)   = DECIMAL(ENTRY(2, cRow)).
  END.

  
*/

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
  
   chGraph = chCtrlFrame:Graph.
  
  RUN initPages("2,3,4").

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postOpenQuery wWin 
PROCEDURE postOpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  /*
  RUN calcTots IN h_dLotesJugo.
  RUN setTotales IN h_fTotalesProduccion (h_dLotesJugo).
  RUN setTotales IN h_fTotalesFruta (h_dBalanzaTickets).*/
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runQueries wWin 
PROCEDURE runQueries :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdDesde AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta AS DATE       NO-UNDO.
  
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOps AS CHARACTER  NO-UNDO.

  /*Jugo*/
  cQry = "lotes_jugo.fecha >= DATE('" + STRING(pdDesde) + "') AND " + 
         "lotes_jugo.fecha <= DATE('" + STRING(pdHasta) + "')". 
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dLotesJugo, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dLotesJugo).
  RUN calcTots IN h_dLotesJugo.       

  /*Fruta*/
  ASSIGN cCol = 'fecha_operativa'
         cVal = STRING(pdDesde) + ";" + STRING(pdHasta)
         cOps = '>=,<='
         .
  DYNAMIC-FUNCTION('assignQuerySelection' IN h_dBalanzaTickets, cCol, cVal, cOps).
  DYNAMIC-FUNCTION('openQuery' IN h_dBalanzaTickets).

  /*Aceite*/ 
  ASSIGN cCol = 'lotes_aceite.fecha,lotes_aceite.id_locacion_ubicacion'
         cVal = STRING(pdDesde) + ";" + STRING(pdHasta) + ";4"
         cOps = '>=,<=,='
         .
  DYNAMIC-FUNCTION('assignQuerySelection' IN h_dLotesAceite, cCol, cVal, cOps).
  DYNAMIC-FUNCTION('openQuery' IN h_dLotesAceite).

  /*Cascara*/
  cQry = "registro_produccion_cascara.fecha >= DATE('" + STRING(pdDesde) + "') AND " + 
         "registro_produccion_cascara.fecha <= DATE('" + STRING(pdHasta) + "')". 
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dRegistroProduccionCascara, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dRegistroProduccionCascara).


  RUN selectPage(getCurrentPage()).
  RUN setTotales IN h_fTotalesProduccion (h_dLotesJugo).
  RUN setTotales IN h_fTotalesFruta (h_dBalanzaTickets).
  RUN setTotales IN h_fTotalesAceite (h_dLotesAceite).
  RUN setTotales IN h_fTotalesCascara (h_dRegistroProduccionCascara).





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectPage wWin 
PROCEDURE selectPage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER piPageNum AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT piPageNum).

  IF NOT VALID-HANDLE(chGraph) THEN RETURN.

  IF piPageNum = 1 THEN DO:
    RUN fillGraph (h_dLotesJugo, "53,52,71", "Claro,Turbio,Pulpa").
  END.

  IF piPageNum = 2 THEN DO:
    RUN fillGraph (h_dBalanzaTickets, "1,2,3", "Linea L,Linea F,-").
  END.

  IF piPageNum = 3 THEN DO:
    RUN fillGraph (h_dLotesAceite, "51,581,571", "Aceite,Water,OilPhase").
  END.

  IF piPageNum = 4 THEN DO:
    RUN fillGraph (h_dRegistroProduccionCascara, "1,3,4", "Linea L,Linea F, F Tipo 4").
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbPrint wWin 
PROCEDURE tlbPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cStream AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  
  RUN exportExcelProduccion IN hLib (INTEGER(fiSemana:SCREEN-VALUE IN FRAME {&FRAME-NAME}), 
                                     DYNAMIC-FUNCTION('getValues' IN h_fTotalesProduccion), 
                                     DYNAMIC-FUNCTION('getValues' IN h_fTotalesAceite),
                                     DYNAMIC-FUNCTION('getValues' IN h_fTotalesCascara),
                                     DYNAMIC-FUNCTION('getValues' IN h_fTotalesFruta)).

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

