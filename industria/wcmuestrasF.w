&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
DEFINE VARIABLE h_asbroker1                AS HANDLE          NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS FILL-IN-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bcfulljoin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcitemsmuestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcitemsmuestras-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcitemsmuestras-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcitemsprotocolo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcitemsprotocolo-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcmuestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcmuestras-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcmuestras-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcprotocolos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcprotocolos-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcprotocolos-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brwjoincontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcitemsmuestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcitemsprotocolos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcmuestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcprotocolos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcprotocolosfulljoin AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcrmuestrasprotocolo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dctamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sdojoincontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vctamboresindustria AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Click en los Folders para Acceder a las Consultas" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FILL-IN-2 AT ROW 11.24 COL 49 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 134.6 BY 22.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Muestras y Protocolos"
         HEIGHT             = 22.14
         WIDTH              = 134.6
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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
       FILL-IN-2:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Muestras y Protocolos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Muestras y Protocolos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
             INPUT  'dcmuestras.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsmuestras.id_muestra,id_muestra,muestras.anio_muestra,anioRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcmuestrasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcmuestras ).
       RUN repositionObject IN h_dcmuestras ( 6.95 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dcitemsmuestras.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_muestras.id_muestra,id_muestra,items_muestras.anio_muestra,anio_muestraRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcitemsmuestrasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcitemsmuestras ).
       RUN repositionObject IN h_dcitemsmuestras ( 6.95 , 11.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dcrmuestrasprotocolo.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_muestras_protocolos.id_muestra,id_muestra,r_muestras_protocolos.item_muestra,item_muestra,r_muestras_protocolos.anio_muestra,anio_muestraRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcrmuestrasprotocoloUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcrmuestrasprotocolo ).
       RUN repositionObject IN h_dcrmuestrasprotocolo ( 6.95 , 20.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dcprotocolos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsprotocolos.id_protocolo,id_protocolo,protocolos.anio,anio,protocolos.id_articulo,id_articuloRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcprotocolosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcprotocolos ).
       RUN repositionObject IN h_dcprotocolos ( 6.95 , 28.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dcitemsprotocolos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_protocolos.id_protocolo,id_protocolo,items_protocolos.anio,anio,items_protocolos.id_articulo,id_articuloRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcitemsprotocolosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcitemsprotocolos ).
       RUN repositionObject IN h_dcitemsprotocolos ( 6.95 , 36.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dctamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_empresa,id_empresa,tambores_industria.id_sucursal,id_sucursal,tambores_industria.id_tipotambor,id_tipotambor,tambores_industria.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedctamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dctamboresindustria ).
       RUN repositionObject IN h_dctamboresindustria ( 6.95 , 44.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Consulta por Muestras|Consulta por Lotes|Cascara|Por Contratos' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 8.10 , 78.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2,1,4') NO-ERROR.

       /* Links to SmartDataObject h_dcmuestras. */
       RUN addLink ( h_dcprotocolosfulljoin , 'Data':U , h_dcmuestras ).
       RUN addLink ( h_dynfilter , 'Filter':U , h_dcmuestras ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dcmuestras ).
       RUN addLink ( h_sdojoincontratos , 'Data':U , h_dcmuestras ).

       /* Links to SmartDataObject h_dcitemsmuestras. */
       RUN addLink ( h_dcmuestras , 'Data':U , h_dcitemsmuestras ).

       /* Links to SmartDataObject h_dcrmuestrasprotocolo. */
       RUN addLink ( h_dcitemsmuestras , 'Data':U , h_dcrmuestrasprotocolo ).

       /* Links to SmartDataObject h_dcprotocolos. */
       RUN addLink ( h_dcrmuestrasprotocolo , 'Data':U , h_dcprotocolos ).

       /* Links to SmartDataObject h_dcitemsprotocolos. */
       RUN addLink ( h_dcprotocolos , 'Data':U , h_dcitemsprotocolos ).

       /* Links to SmartDataObject h_dctamboresindustria. */
       RUN addLink ( h_dcprotocolos , 'Data':U , h_dctamboresindustria ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'bcmuestras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcmuestras ).
       RUN repositionObject IN h_bcmuestras ( 2.43 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_bcmuestras ( 6.67 , 55.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcitemsprotocolo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcitemsprotocolo ).
       RUN repositionObject IN h_bcitemsprotocolo ( 16.48 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_bcitemsprotocolo ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vctamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vctamboresindustria ).
       RUN repositionObject IN h_vctamboresindustria ( 9.81 , 70.00 ) NO-ERROR.
       /* Size in AB:  ( 3.00 , 28.00 ) */

       RUN constructObject (
             INPUT  'bcprotocolos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcprotocolos ).
       RUN repositionObject IN h_bcprotocolos ( 16.48 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bcprotocolos ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcitemsmuestras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcitemsmuestras ).
       RUN repositionObject IN h_bcitemsmuestras ( 9.57 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bcitemsmuestras ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 79.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 54.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_muestra,fechaOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesfechaRANGEFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsfecha0id_muestra0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.43 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 3.71 , 74.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bcmuestras. */
       RUN addLink ( h_dcmuestras , 'Data':U , h_bcmuestras ).

       /* Links to SmartDataBrowser h_bcitemsprotocolo. */
       RUN addLink ( h_dcitemsprotocolos , 'Data':U , h_bcitemsprotocolo ).

       /* Links to SmartDataViewer h_vctamboresindustria. */
       RUN addLink ( h_dctamboresindustria , 'Data':U , h_vctamboresindustria ).

       /* Links to SmartDataBrowser h_bcprotocolos. */
       RUN addLink ( h_dcprotocolos , 'Data':U , h_bcprotocolos ).

       /* Links to SmartDataBrowser h_bcitemsmuestras. */
       RUN addLink ( h_dcitemsmuestras , 'Data':U , h_bcitemsmuestras ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'dcprotocolosfulljoin.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcprotocolosfulljoinUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcprotocolosfulljoin ).
       RUN repositionObject IN h_dcprotocolosfulljoin ( 6.95 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bcfulljoin.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcfulljoin ).
       RUN repositionObject IN h_bcfulljoin ( 2.43 , 80.60 ) NO-ERROR.
       RUN resizeObject IN h_bcfulljoin ( 6.67 , 54.40 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 1.00 , 79.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 56.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_lote,id_articulo,id_sucursalOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsDesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-2 ).
       RUN repositionObject IN h_dynfilter-2 ( 2.67 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-2 ( 4.43 , 72.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcmuestras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcmuestras-2 ).
       RUN repositionObject IN h_bcmuestras-2 ( 9.57 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bcmuestras-2 ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcitemsmuestras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcitemsmuestras-2 ).
       RUN repositionObject IN h_bcitemsmuestras-2 ( 9.57 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_bcitemsmuestras-2 ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcprotocolos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcprotocolos-2 ).
       RUN repositionObject IN h_bcprotocolos-2 ( 16.48 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bcprotocolos-2 ( 6.43 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcitemsprotocolo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcitemsprotocolo-2 ).
       RUN repositionObject IN h_bcitemsprotocolo-2 ( 16.48 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_bcitemsprotocolo-2 ( 6.43 , 66.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dcprotocolosfulljoin. */
       RUN addLink ( h_dynfilter-2 , 'Filter':U , h_dcprotocolosfulljoin ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_dcprotocolosfulljoin ).

       /* Links to SmartDataBrowser h_bcfulljoin. */
       RUN addLink ( h_dcprotocolosfulljoin , 'Data':U , h_bcfulljoin ).

    END. /* Page 2 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'sdojoincontratos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamesdojoincontratosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_sdojoincontratos ).
       RUN repositionObject IN h_sdojoincontratos ( 6.95 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'brwjoincontratos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brwjoincontratos ).
       RUN repositionObject IN h_brwjoincontratos ( 2.43 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_brwjoincontratos ( 6.67 , 55.60 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 1.00 , 79.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.24 , 56.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsorden_fabricacion,id_contrato,anio_ofOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsDesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-4 ).
       RUN repositionObject IN h_dynfilter-4 ( 2.43 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-4 ( 4.43 , 72.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcmuestras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcmuestras-4 ).
       RUN repositionObject IN h_bcmuestras-4 ( 9.57 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bcmuestras-4 ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcitemsmuestras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcitemsmuestras-4 ).
       RUN repositionObject IN h_bcitemsmuestras-4 ( 9.57 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_bcitemsmuestras-4 ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bcprotocolos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcprotocolos-4 ).
       RUN repositionObject IN h_bcprotocolos-4 ( 16.48 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bcprotocolos-4 ( 6.67 , 66.00 ) NO-ERROR.

       /* Links to SmartDataObject h_sdojoincontratos. */
       RUN addLink ( h_dynfilter-4 , 'Filter':U , h_sdojoincontratos ).
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_sdojoincontratos ).

       /* Links to SmartDataBrowser h_brwjoincontratos. */
       RUN addLink ( h_sdojoincontratos , 'Data':U , h_brwjoincontratos ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 4 */

  END CASE.

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
  DISPLAY FILL-IN-2 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE FILL-IN-2 
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

