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

/* some fragments from winmain.w */
DEFINE VARIABLE hPalette AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-32 

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


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bgastosordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemsoe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemsoe-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemsoe-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brlotecascaraoe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dgastosordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsordenentrega-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsordenentrega-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dremitos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drlotecascaraoe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ftotalesgastosoe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vgastosordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vitemsoe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_voeindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vremitos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vtotalesitemoe AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82 BY 4.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-32 AT ROW 2.19 COL 69
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150.8 BY 22.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 2
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Ordenes Embarque"
         HEIGHT             = 22.52
         WIDTH              = 150.8
         MAX-HEIGHT         = 33
         MAX-WIDTH          = 203.6
         VIRTUAL-HEIGHT     = 33
         VIRTUAL-WIDTH      = 203.6
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Ordenes Embarque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON ENTRY OF wWin /* Ordenes Embarque */
DO:
  RUN SetTopMost IN hPalette(YES) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON LEAVE OF wWin /* Ordenes Embarque */
DO:
  RUN SetTopMost IN hPalette(NO) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Ordenes Embarque */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  
  RUN beforeExit.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
             INPUT  'dordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeSTATE-RESETDestroyStatelessnoDisconnectAppServernoObjectNamedordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dordenentrega ).
       RUN repositionObject IN h_dordenentrega ( 5.29 , 69.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bordenentrega ).
       RUN repositionObject IN h_bordenentrega ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bordenentrega ( 5.48 , 67.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dgastosordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsgastos_orden_entrega.id_orden_entrega,id_orden_entregaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedgastosordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dgastosordenentrega ).
       RUN repositionObject IN h_dgastosordenentrega ( 5.29 , 90.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_orden_entrega.id_orden_entrega,id_orden_entregaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsordenentrega ).
       RUN repositionObject IN h_ditemsordenentrega ( 5.29 , 79.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dremitos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsremitos.id_orden_entrega,id_orden_entrega,remitos.item_oe,item_oeRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedremitosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dremitos ).
       RUN repositionObject IN h_dremitos ( 5.05 , 139.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_orden_entrega.id_orden_entrega,id_orden_entregaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsordenentrega-2 ).
       RUN repositionObject IN h_ditemsordenentrega-2 ( 5.29 , 101.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_orden_entrega.id_orden_entrega,id_orden_entregaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsordenentrega-3 ).
       RUN repositionObject IN h_ditemsordenentrega-3 ( 5.29 , 111.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'drlotecascaraoe.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_lote_cascara_oe.id_orden_entrega,id_orden_entrega,r_lote_cascara_oe.id_lote,item_oeRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrlotecascaraoeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drlotecascaraoe ).
       RUN repositionObject IN h_drlotecascaraoe ( 5.29 , 128.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_orden_entrega,id_orden_entrega,tambores_industria.item_oe,item_oeRowsToBatch1000CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria ).
       RUN repositionObject IN h_dtamboresindustria ( 5.29 , 121.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda1,Banda2,Banda3SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsCustomactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 82.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_orden_entrega,anio,id_agenciaOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsDesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.43 , 71.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 3.81 , 79.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'OE|Partes|Gastos|Totales|Producto|Remitos' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 6.71 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 16.67 , 150.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('3,2,4') NO-ERROR.

       /* Links to SmartDataObject h_dordenentrega. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dordenentrega ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dordenentrega ).

       /* Links to SmartDataBrowser h_bordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_bordenentrega ).

       /* Links to SmartDataObject h_dgastosordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_dgastosordenentrega ).
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_dgastosordenentrega ).

       /* Links to SmartDataObject h_ditemsordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_ditemsordenentrega ).
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_ditemsordenentrega ).

       /* Links to SmartDataObject h_dremitos. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_dremitos ).

       /* Links to SmartDataObject h_ditemsordenentrega-2. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_ditemsordenentrega-2 ).
       RUN addLink ( h_dyntoolbar-5 , 'Navigation':U , h_ditemsordenentrega-2 ).

       /* Links to SmartDataObject h_ditemsordenentrega-3. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_ditemsordenentrega-3 ).

       /* Links to SmartDataObject h_drlotecascaraoe. */
       RUN addLink ( h_ditemsordenentrega-3 , 'Data':U , h_drlotecascaraoe ).

       /* Links to SmartDataObject h_dtamboresindustria. */
       RUN addLink ( h_ditemsordenentrega-3 , 'Data':U , h_dtamboresindustria ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'voeindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_voeindustria ).
       RUN repositionObject IN h_voeindustria ( 10.52 , 17.00 ) NO-ERROR.
       /* Size in AB:  ( 11.05 , 118.40 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableioSubModulesTableIOTypeUpdateSupportedLinksTableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUndo,CommitHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 8.14 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 147.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_voeindustria. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_voeindustria ).
       RUN addLink ( h_voeindustria , 'Update':U , h_dordenentrega ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_voeindustria ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'bitemsoe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsoe ).
       RUN repositionObject IN h_bitemsoe ( 7.91 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsoe ( 3.33 , 148.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vitemsoe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vitemsoe ).
       RUN repositionObject IN h_vitemsoe ( 12.91 , 8.00 ) NO-ERROR.
       /* Size in AB:  ( 10.10 , 136.20 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsFilterHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 11.48 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 1.24 , 148.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bitemsoe. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_bitemsoe ).

       /* Links to SmartDataViewer h_vitemsoe. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_vitemsoe ).
       RUN addLink ( h_vitemsoe , 'Update':U , h_ditemsordenentrega ).
       RUN addLink ( h_dyntoolbar-3 , 'TableIo':U , h_vitemsoe ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'bgastosordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bgastosordenentrega ).
       RUN repositionObject IN h_bgastosordenentrega ( 8.14 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bgastosordenentrega ( 13.10 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vgastosordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vgastosordenentrega ).
       RUN repositionObject IN h_vgastosordenentrega ( 11.24 , 84.00 ) NO-ERROR.
       /* Size in AB:  ( 3.57 , 47.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda3SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsCustomactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 8.14 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.24 , 80.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ftotalesgastosoe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ftotalesgastosoe ).
       RUN repositionObject IN h_ftotalesgastosoe ( 21.48 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.48 , 65.40 ) */

       /* Links to SmartDataBrowser h_bgastosordenentrega. */
       RUN addLink ( h_dgastosordenentrega , 'Data':U , h_bgastosordenentrega ).

       /* Links to SmartDataViewer h_vgastosordenentrega. */
       RUN addLink ( h_dgastosordenentrega , 'Data':U , h_vgastosordenentrega ).
       RUN addLink ( h_vgastosordenentrega , 'Update':U , h_dgastosordenentrega ).
       RUN addLink ( h_dyntoolbar-4 , 'TableIo':U , h_vgastosordenentrega ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'bitemsoe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsoe-2 ).
       RUN repositionObject IN h_bitemsoe-2 ( 8.14 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsoe-2 ( 5.24 , 147.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vtotalesitemoe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vtotalesitemoe ).
       RUN repositionObject IN h_vtotalesitemoe ( 15.05 , 8.00 ) NO-ERROR.
       /* Size in AB:  ( 7.95 , 136.20 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-5 ).
       RUN repositionObject IN h_dyntoolbar-5 ( 13.62 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-5 ( 1.24 , 147.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bitemsoe-2. */
       RUN addLink ( h_ditemsordenentrega-2 , 'Data':U , h_bitemsoe-2 ).

       /* Links to SmartDataViewer h_vtotalesitemoe. */
       RUN addLink ( h_ditemsordenentrega-2 , 'Data':U , h_vtotalesitemoe ).
       RUN addLink ( h_vtotalesitemoe , 'Update':U , h_ditemsordenentrega-2 ).
       RUN addLink ( h_dyntoolbar-5 , 'Tableio':U , h_vtotalesitemoe ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN constructObject (
             INPUT  'brlotecascaraoe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brlotecascaraoe ).
       RUN repositionObject IN h_brlotecascaraoe ( 12.43 , 84.00 ) NO-ERROR.
       RUN resizeObject IN h_brlotecascaraoe ( 10.71 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria ).
       RUN repositionObject IN h_btamboresindustria ( 12.43 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria ( 10.71 , 81.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bitemsoe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsoe-3 ).
       RUN repositionObject IN h_bitemsoe-3 ( 8.14 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsoe-3 ( 4.05 , 148.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_brlotecascaraoe. */
       RUN addLink ( h_drlotecascaraoe , 'Data':U , h_brlotecascaraoe ).

       /* Links to SmartDataBrowser h_btamboresindustria. */
       RUN addLink ( h_dtamboresindustria , 'Data':U , h_btamboresindustria ).

       /* Links to SmartDataBrowser h_bitemsoe-3. */
       RUN addLink ( h_ditemsordenentrega-3 , 'Data':U , h_bitemsoe-3 ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN constructObject (
             INPUT  'vremitos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vremitos ).
       RUN repositionObject IN h_vremitos ( 8.14 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 14.57 , 147.00 ) */

       /* Links to SmartDataViewer h_vremitos. */
       RUN addLink ( h_dremitos , 'Data':U , h_vremitos ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 6 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createButtons wWin 
PROCEDURE createButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE VARIABLE hLibCom AS HANDLE.
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  RUN addToolbarButton IN hLibCom (h_dynToolbar-4, 
                                   "calculateOEAction", 
                                   "tlbCalculate", 
                                   "Calcular Gastos", 
                                   "507.bmp",
                                   "tlbCalculate", 
                                   "Banda3").

  RUN addToolbarButton IN hLibCom (h_dynToolbar, 
                                   "prtContratoAction", 
                                   "tlbPrintContrato", 
                                   "Imprimir Contrato de OE", 
                                   "671.bmp",
                                   "tlbPrtContrato", 
                                   "Banda2").

  RUN addToolbarButton IN hLibCom (h_dynToolbar, 
                                   "scrContratoAction", 
                                   "tlbScrContrato", 
                                   "Pantalla Contrato", 
                                   "591.bmp",
                                   "tlbScrContrato", 
                                   "Banda2").

  RUN addToolbarButton IN hLibCom (h_dynToolbar-3, 
                                   "ajusteAction", 
                                   "AjusteOE", 
                                   "Ajuste de OE", 
                                   "vapor.gif",
                                   "tlbAjuste", 
                                   "FUNCTION").


  
  
  
  
  
  
  
  DELETE OBJECT hLibCom.
  

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

  RUN initPages("1,2,3,4,5").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableObject wWin 
PROCEDURE enableObject :
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
  ENABLE RECT-32 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  RELEASE orden_entrega.
  RELEASE items_orden_entrega.
  RELEASE contratos.
  RELEASE items_contratos.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "mailAction").  
  SUBSCRIBE TO "tlbMail" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-4, "calculateOEAction").  
  SUBSCRIBE TO "tlbCalculate" IN h_dyntoolbar-4.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "prtContratoAction").  
  SUBSCRIBE TO "tlbPrtContrato" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "scrContratoAction").  
  SUBSCRIBE TO "tlbScrContrato" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-3, "ajusteAction").  
  SUBSCRIBE TO "tlbAjuste" IN h_dyntoolbar-3.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "excelAction").  
  SUBSCRIBE TO "tlbExcel" IN h_dyntoolbar.


  RUN readParamsFile.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readParamsFile wWin 
PROCEDURE readParamsFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFile AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLine AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVal  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOps  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPos  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOE   AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibTam  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibTam   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.

  cKey = DYNAMIC-FUNCTION('getParamsFile' IN hLibTam).

  IF LENGTH(cKey) > 0 THEN DO:
    ASSIGN iOE = INTEGER(ENTRY(1, cKey, CHR(1))).
  END.

                                                       
  IF iOE <> 0 THEN DO:
    cCol = "id_orden_entrega".
    cOps = "=".
    cVal = STRING(iOE).

    DYNAMIC-FUNCTION('assignQuerySelection' IN h_dOrdenEntrega, cCol, cVal, cOps).
    DYNAMIC-FUNCTION('openQuery' IN h_dOrdenEntrega).    
    RUN deleteParamsFile IN hLibTam.
  END.

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

  IF piPageNum = 2 THEN DO:
    RUN fetchFirst IN h_ditemsordenentrega.
  END.

  IF piPageNum = 3 THEN DO:
    RUN setTotals.
  END.

  IF piPageNum = 4 THEN DO:
    RUN fetchFirst IN h_ditemsordenentrega-2.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTotals wWin 
PROCEDURE setTotals :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.
                                                                                    
  RUN setTotals IN h_fTotalesGastosOE (DYNAMIC-FUNCTION('getTotalGastosOE' IN hLib, DYNAMIC-FUNCTION('columnValue' IN h_dOrdenEntrega, 'id_orden_entrega'))).

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAjuste wWin 
PROCEDURE tlbAjuste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRowIds AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lAnswer AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p'). 
  DELETE OBJECT hLibCom.


  cRowIds = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bItemsOE).

  MESSAGE "Esta Seguro que desea realizar un ajuste para las partes de OE's seleccionadas"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lAnswer.

  IF NOT lAnswer THEN RETURN.

  RUN createAjusteOE IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dOrdenEntrega, 'id_orden_entrega'), cRowIds).
  

  RUN blankFields IN h_dynfilter.
  RUN applyFilter IN h_dynfilter.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbCalculate wWin 
PROCEDURE tlbCalculate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibOE AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cRows  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibOE = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.  
  
  DYNAMIC-FUNCTION('calcularOE' IN hLibOE, DYNAMIC-FUNCTION('columnValue' IN h_dOrdenEntrega, 'id_orden_entrega'), 
                   DYNAMIC-FUNCTION('getRecalcula' IN h_fTotalesGastosOE)).

  DYNAMIC-FUNCTION('openQuery' IN h_dGastosOrdenEntrega).
  DYNAMIC-FUNCTION('openQuery' IN h_dItemsOrdenEntrega-2).
  
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExcel wWin 
PROCEDURE tlbExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p'). 
  DELETE OBJECT hLibCom.

  RUN exportExcelOEs IN hLib.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbMail wWin 
PROCEDURE tlbMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cPdf    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lCon    AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cTo     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCC     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSub    AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  
  RUN callReporteOEIndustria IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dOrdenEntrega, 'id_orden_entrega')).

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports_9.prl",
                                                    "rptOEIndustria",
                                                    "",
                                                    "",
                                                    "").
  RUN confirmPdf IN hLibCom (cPdf, OUTPUT lCon).

  RUN wdPromptMailAddrs.w (OUTPUT cTo, OUTPUT cCC).
  
  cTo = cTo + IF cCC <> "" THEN "," + cCC ELSE "".
  IF cTo = "" THEN DO:
    MESSAGE "Debe Ingresar Direcciones de Correo Validas" SKIP "El Mail no sera enviado"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  cTxt = "OE " + STRING(DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_orden_entrega'))   
       + " Para el contrato " + STRING(DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_contrato')) 
       + " parte " + STRING(DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'item_oe')).

  cSub = "generacion de OE".

  cTxt = cTxt .

  RUN ..\industria\sendMail.p("", 2, cSub, cTxt, cTo, cPdf).
      

  MESSAGE "El mail fue enviado con Exito!!" SKIP cPdf
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbMailObservaciones wWin 
PROCEDURE tlbMailObservaciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.

  cTxt = "Se a creado la OE " + string(DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_orden_entrega'))   
       + " Para el contrato " + string(DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_contrato')) 
       + " parte " + string(DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'item_oe')).

  cTo  = "jsaravia@sa-sanmiguel.com,karin@sa-sanmiguel.com,rmassone@sa-sanmiguel.com,amallea@sa-sanmiguel.com,rvelez@sa-sanmiguel.com,lmunoz@sa-sanmiguel.com,facundoj@sa-sanmiguel.com".
  cSub = "confirmacion creacion de OE".

  RUN ..\industria\sendMail.p("", 2, cSub, cTxt, cTo, "").


  





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

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.
  
  RUN callReporteOEIndustria IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dOrdenEntrega, 'id_orden_entrega')).

                                      
  RUN p_reportes_9.p ("rptOEIndustria",
                      "Reporte de Orden de Embarque",
                      "",
                      "").        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbPrtContrato wWin 
PROCEDURE tlbPrtContrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  
  cQry = "contratos.id_contrato = '" + DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_contrato') + 
         "' AND contratos.anio = " + DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'anio').

  RUN p_reportes_9.p ("reporte_contratos",
                      "Reporte de Contratos",
                      cQry,
                      "").        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbScrContrato wWin 
PROCEDURE tlbScrContrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE hWin AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hPnt AS HANDLE     NO-UNDO.

  hPnt = CURRENT-WINDOW:HANDLE.

  RUN wItemsContratos.w PERSISTENT SET hWin (DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_tipo_contrato'), 
                                             DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'anio'), 
                                             DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_contrato')).

  hWin:CURRENT-WINDOW:PARENT = hPnt:PARENT.
  RUN initializeObject IN hWin .
  

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

