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

DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibRem AS HANDLE     NO-UNDO.


DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.

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
&Scoped-Define ENABLED-OBJECTS RECT-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldRemito wWin 
FUNCTION getFieldRemito RETURNS CHARACTER
  (pcFieldName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowIdRemito wWin 
FUNCTION getRowIdRemito RETURNS ROWID
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
DEFINE VARIABLE h_bitemsremito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bremitos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brtamborremito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dconfirmacionremito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsremito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dremitos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drtamborremito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_flecturatambores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fradtipotamborvertical AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vconfirmacionremito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vremitos AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-5 AT ROW 2.43 COL 90
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150.4 BY 26.05.


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
         TITLE              = "Remitos"
         HEIGHT             = 26.05
         WIDTH              = 150.4
         MAX-HEIGHT         = 33.38
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.38
         VIRTUAL-WIDTH      = 204.8
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
ON END-ERROR OF wWin /* Remitos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Remitos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
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
             INPUT  'dremitos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedremitosUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dremitos ).
       RUN repositionObject IN h_dremitos ( 3.62 , 84.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bremitos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bremitos ).
       RUN repositionObject IN h_bremitos ( 2.43 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bremitos ( 8.33 , 88.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ditemsremito.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_factura.id_sucursal,id_sucursal,items_factura.id_tipo_movsto,id_tipo_movsto,items_factura.nro,nroRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsremitoUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsremito ).
       RUN repositionObject IN h_ditemsremito ( 8.38 , 84.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'drtamborremito.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_tambor_remito.id_sucursal_remito,id_sucursal,r_tambor_remito.id_tipo_movsto,id_tipo_movsto,r_tambor_remito.nro_remito,nro,r_tambor_remito.item_factura,itemRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrtamborremitoUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drtamborremito ).
       RUN repositionObject IN h_drtamborremito ( 6.00 , 84.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dconfirmacionremito.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsconfirmacion_remito.id_sucursal_remito,id_sucursal,confirmacion_remito.id_tipo_movsto,id_tipo_movsto,confirmacion_remito.nro_remito,nroRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedconfirmacionremitoUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dconfirmacionremito ).
       RUN repositionObject IN h_dconfirmacionremito ( 9.57 , 95.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda1,Banda3,Banda2SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsCopyHiddenActionsCustomactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsCopyNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 150.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_sucursal,nro_comprobante,nro,id_tipo_movsto,id_orden_entrega,fechaOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesfechaRANGEFieldFormatsFieldWidthsFieldLabelsid_tipo_movstoTipoFieldToolTipsFieldHelpIdsid_orden_entrega0id_sucursal0id_tipo_movsto0nro0nro_comprobante0fecha0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.67 , 92.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 7.86 , 58.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Remito|Partes|Lectura|Confirmacion Arribo' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 11.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 15.95 , 150.20 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2,4') NO-ERROR.

       /* Links to SmartDataObject h_dremitos. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dremitos ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dremitos ).

       /* Links to SmartDataBrowser h_bremitos. */
       RUN addLink ( h_dremitos , 'Data':U , h_bremitos ).

       /* Links to SmartDataObject h_ditemsremito. */
       RUN addLink ( h_dremitos , 'Data':U , h_ditemsremito ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_ditemsremito ).

       /* Links to SmartDataObject h_drtamborremito. */
       RUN addLink ( h_ditemsremito , 'Data':U , h_drtamborremito ).
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_drtamborremito ).

       /* Links to SmartDataObject h_dconfirmacionremito. */
       RUN addLink ( h_dremitos , 'Data':U , h_dconfirmacionremito ).
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_dconfirmacionremito ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vremitos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vremitos ).
       RUN repositionObject IN h_vremitos ( 12.19 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 14.57 , 147.00 ) */

       /* Links to SmartDataViewer h_vremitos. */
       RUN addLink ( h_dremitos , 'Data':U , h_vremitos ).
       RUN addLink ( h_vremitos , 'Update':U , h_dremitos ).
       RUN addLink ( h_dyntoolbar , 'TableIo':U , h_vremitos ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'brtamborremito.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brtamborremito ).
       RUN repositionObject IN h_brtamborremito ( 18.86 , 29.00 ) NO-ERROR.
       RUN resizeObject IN h_brtamborremito ( 7.86 , 114.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bitemsremito.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsremito ).
       RUN repositionObject IN h_bitemsremito ( 13.62 , 29.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsremito ( 5.00 , 121.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Tableio,Banda2SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsExcelaction,Add,Copy,Save,Reset,Cancel,UpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 12.19 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 148.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'fradtipotamborvertical.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fradtipotamborvertical ).
       RUN repositionObject IN h_fradtipotamborvertical ( 13.62 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 12.81 , 26.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionVerticalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsDeleteHiddenActionsAdd,Copy,Save,Reset,Cancel,UpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsDeleteNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 18.86 , 144.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 7.86 , 5.60 ) NO-ERROR.

       /* Links to SmartDataBrowser h_brtamborremito. */
       RUN addLink ( h_drtamborremito , 'Data':U , h_brtamborremito ).
       RUN addLink ( h_brtamborremito , 'Update':U , h_drtamborremito ).
       RUN addLink ( h_dyntoolbar-3 , 'TableIo':U , h_brtamborremito ).

       /* Links to SmartDataBrowser h_bitemsremito. */
       RUN addLink ( h_ditemsremito , 'Data':U , h_bitemsremito ).
       RUN addLink ( h_bitemsremito , 'Update':U , h_ditemsremito ).
       RUN addLink ( h_dyntoolbar-2 , 'Tableio':U , h_bitemsremito ).

       /* Links to SmartFrame h_fradtipotamborvertical. */
       RUN addLink ( h_folder , 'Page':U , h_fradtipotamborvertical ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'flecturatambores.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_flecturatambores ).
       RUN repositionObject IN h_flecturatambores ( 12.43 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 13.48 , 146.00 ) */

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'vconfirmacionremito.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vconfirmacionremito ).
       RUN repositionObject IN h_vconfirmacionremito ( 16.24 , 27.00 ) NO-ERROR.
       /* Size in AB:  ( 7.81 , 97.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 12.43 , 27.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.24 , 98.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vconfirmacionremito. */
       RUN addLink ( h_dconfirmacionremito , 'Data':U , h_vconfirmacionremito ).
       RUN addLink ( h_vconfirmacionremito , 'Update':U , h_dconfirmacionremito ).
       RUN addLink ( h_dyntoolbar-4 , 'TableIo':U , h_vconfirmacionremito ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 4 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

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

  /*Boton de Reporte de Despachos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "rptDespachosAction", 
                          "OrdenDespacho", 
                          "Imprimir Orden de Despacho", 
                          "1252.bmp", 
                          "tlbRptDespachos", 
                          "BANDA3").

  
  /*Boton de Precintos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "precintosAction", 
                          "Precintos", 
                          "Control de Precintos", 
                          "precintos.bmp", 
                          "tlbPrecintos", 
                          "BANDA3").

  /*Boton de Inspeccion Camiones*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "inspeccionAction", 
                          "Inspeccion", 
                          "Inspeccion de Camion", 
                          "truck02h.bmp", 
                          "tlbInspeccion", 
                          "BANDA3").

  /*Boton de Procesamiento de Remito*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "procesarAction", 
                          "Procesar", 
                          "Procesamiento de Remito", 
                          "205.bmp", 
                          "tlbProcesar", 
                          "BANDA3").

  /*Boton de Anulacion de Remito*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "anularAction", 
                          "Anular", 
                          "Anular de Remito", 
                          "207.bmp", 
                          "tlbAnular", 
                          "BANDA3").

  /*Boton de Habilitar Creacion de Item*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "habilitaCreacionAction", 
                          "Agregar", 
                          "Agregar Item", 
                          "507.bmp", 
                          "tlbAddItem", 
                          "BANDA2").

  /*Boton de Buscar Tambores*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "searchTamboresAction", 
                          "Buscar Tambores", 
                          "Buscar Tambores", 
                          "1137.bmp", 
                          "tlbAddTambores", 
                          "BANDA2").
  
  /*Boton de Cancelar Accion*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "cancelAddAction", 
                          "Cancelar", 
                          "Cancelar", 
                          "cross.bmp", 
                          "tlbCancelAction", 
                          "BANDA2").


  

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

  RUN initPages("1").
  RUN createButtons.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createRemitosWater wWin 
PROCEDURE createRemitosWater :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cFile AS CHARACTER  NO-UNDO.

  SYSTEM-DIALOG GET-FILE cFile.

  IF SEARCH(cFile) = ? THEN RETURN.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.

  RUN createRemitosWaterPhase IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'),
                                       DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro'), 
                                       cFile).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customEnableFunctions wWin 
PROCEDURE customEnableFunctions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER plEnable AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cFunction AS CHARACTER  NO-UNDO.

  IF plEnable THEN
    cFunction = "enableActions".
  ELSE 
    cFunction = "disableActions".

  
  DYNAMIC-FUNCTION(cFunction IN h_dyntoolbar, "procesarAction"). 
  DYNAMIC-FUNCTION(cFunction IN h_dyntoolbar, "update").
  DYNAMIC-FUNCTION(cFunction IN h_dyntoolbar, "delete").
  DYNAMIC-FUNCTION(cFunction IN h_dyntoolbar-2, "delete").
  DYNAMIC-FUNCTION(cFunction IN h_dyntoolbar-2, "habilitaCreacionAction").
  DYNAMIC-FUNCTION(cFunction IN h_dyntoolbar-2, "searchTamboresAction").
  

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
  ENABLE RECT-5 
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


  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib    = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  hLibRem = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "excelAction").  
  SUBSCRIBE TO "tlbExcel" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "precintosAction").  
  SUBSCRIBE TO "tlbPrecintos" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "inspeccionAction").  
  SUBSCRIBE TO "tlbInspeccion" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "procesarAction").  
  SUBSCRIBE TO "tlbProcesar" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "anularAction").  
  SUBSCRIBE TO "tlbAnular" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "rptDespachosAction").  
  SUBSCRIBE TO "tlbRptDespachos" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "habilitaCreacionAction").  
  SUBSCRIBE TO "tlbAddItem" IN h_dyntoolbar-2.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printRemito wWin 
PROCEDURE printRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iNroCopia AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLibRep   AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  RUN impresionRemito IN hLibRep (DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal') ,
                                  DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto'), 
                                  DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro') ).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshCabecera wWin 
PROCEDURE refreshCabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*  RUN refreshRow IN h_dRemitos.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshScreen wWin 
PROCEDURE refreshScreen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  DYNAMIC-FUNCTION('openQuery' IN h_dRemitos).
  DYNAMIC-FUNCTION('openQuery' IN h_dItemsRemito).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAddItem wWin 
PROCEDURE tlbAddItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN enableFields IN h_fRadTipoTamborVertical (TRUE).

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "searchTamboresAction").  
  SUBSCRIBE TO "tlbAddTambores" IN h_dyntoolbar-2.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "cancelAddAction").  
  SUBSCRIBE TO "tlbCancelAction" IN h_dyntoolbar-2.

  DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar-2, "habilitaCreacionAction").  
  UNSUBSCRIBE TO "tlbAddItem" IN h_dyntoolbar-2.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAddTambores wWin 
PROCEDURE tlbAddTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.

  /* controlo que no agreguen items a un remito ya procesado */
  IF DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'fecha_proceso') <> ? THEN DO:
    MESSAGE "Imposible agregar items a un remito que ya fue procesado!."
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
     
    RUN tlbCancelAction.
    RETURN.  
  END.

  /* caso excepcional para water */
  IF DYNAMIC-FUNCTION('getTipoTambor' IN h_fRadTipoTamborVertical) = 581 THEN DO:
    RUN createRemitosWater.
  END.
  

  IF DYNAMIC-FUNCTION('getTipoTambor' IN h_fRadTipoTamborVertical) <> 11 AND 
     DYNAMIC-FUNCTION('getTipoTambor' IN h_fRadTipoTamborVertical) <> 581 THEN DO: 
   
    /*PRODUCTOS INDUSTRIALES*/
    RUN wdPickUpDrums.w (0, /*DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'),*/
                         DYNAMIC-FUNCTION('getTipoTambor' IN h_fRadTipoTamborVertical),
                         OUTPUT cReturn).

    /*crear items_factura*/
    RUN addItemRemito IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'), 
                               DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto'),
                               DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro'),
                               cReturn).

  END.
  IF DYNAMIC-FUNCTION('getTipoTambor' IN h_fRadTipoTamborVertical) = 11 THEN DO:   
    /*CASCARA*/
    RUN wGetLoteCascaraRemito.w (DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'), 
                                 OUTPUT iEmp,
                                 OUTPUT iSuc,
                                 OUTPUT iTip,
                                 OUTPUT iNro,
                                 OUTPUT iCan,
                                 OUTPUT iLot,
                                 OUTPUT iAno).
    /*crear item factura para cascara*/
    RUN addItemRemitoCascara IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto'),
                                      DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro'),
                                      iEmp, iSuc, iTip, iNro, iCan).

  END. /*if tipotambor <> cascara ...*/
  

  DYNAMIC-FUNCTION('openQuery' IN h_dItemsRemito).
  DYNAMIC-FUNCTION('openQuery' IN h_drTamborRemito).


  RUN recalcKilosRemito IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto'),
                                 DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro')).


  RUN tlbCancelAction.

  RUN refreshRow IN h_dremitos .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAnular wWin 
PROCEDURE tlbAnular :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  MESSAGE "Desea Anular el Remito?" SKIP DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro_comp')
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.

  IF lChoice THEN DO:
    RUN anulacionRemito IN hLibRem (DYNAMIC-FUNCTION('getRowId' IN h_dRemitos)).
    RUN refreshRow IN h_dRemitos.
  END.
  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbCancelAction wWin 
PROCEDURE tlbCancelAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN enableFields IN h_fRadTipoTamborVertical (FALSE).

  DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar-2, "searchTamboresAction").  
  UNSUBSCRIBE TO "tlbAddTambores" IN h_dyntoolbar-2.

  DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar-2, "cancelAddAction").  
  UNSUBSCRIBE TO "tlbCancelAction" IN h_dyntoolbar-2.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "habilitaCreacionAction").  
  SUBSCRIBE TO "tlbAddItem" IN h_dyntoolbar-2.

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
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.

  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  RUN wdParams.w (OUTPUT iSuc,
                  OUTPUT dDes, 
                  OUTPUT dHas, 
                  OUTPUT iArt, 
                  OUTPUT iLin).

  RUN exportRemitoFactura IN hLib (iSuc, dDes, dHas).

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

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbInspeccion wWin 
PROCEDURE tlbInspeccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                               
  RUN wInspeccionesCamiones.w (DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'),
                               DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto'), 
                               DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro'), 
                               DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'fecha')).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbPrecintos wWin 
PROCEDURE tlbPrecintos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN saveParamsFile IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'), 
                              DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto'), 
                              DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro'), 
                              0). 
   
  
  RUN wControlPrecinto.w .

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
  RUN printRemito.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbProcesar wWin 
PROCEDURE tlbProcesar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                                 
  RUN processRemito IN hLibRem (DYNAMIC-FUNCTION('getRowId' IN h_dRemitos), TRUE).
  RUN refreshRow IN h_dRemitos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbRptDespachos wWin 
PROCEDURE tlbRptDespachos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cFiltro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOps AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cReport AS CHARACTER  NO-UNDO.
                                               
  cFiltro = "remitos.id_sucursal = " + DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal') +
            " AND remitos.id_tipo_movsto = " + DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto') + 
            " AND remitos.nro = " + DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro').
                                    
  cOps = DYNAMIC-FUNCTION('getOperariosCarga' IN hLibRem, DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_sucursal'),
                                                          DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'id_tipo_movsto'),
                                                          DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, 'nro')).
  cOps = REPLACE(cOps, CHR(10), "|").
  cOps = "operarios=" + cOps.

  RUN fetchFirst IN h_dItemsRemito.
  IF DYNAMIC-FUNCTION('columnValue' IN h_dItemsRemito, 'id_tipotambor') = "11" THEN
    cReport = "despacho_productos_cascara".
  ELSE
    cReport = "despacho_productos".

  RUN  aderb\_prntrb2("..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                        cReport,                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        cFiltro,                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Hoja de Despacho",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        cOps /* RB-OTHER-PARAMETERS */,
                        "").   

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldRemito wWin 
FUNCTION getFieldRemito RETURNS CHARACTER
  (pcFieldName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN DYNAMIC-FUNCTION('columnValue' IN h_dRemitos, pcFieldName).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowIdRemito wWin 
FUNCTION getRowIdRemito RETURNS ROWID
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN DYNAMIC-FUNCTION('getRowId' IN h_dRemitos).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

