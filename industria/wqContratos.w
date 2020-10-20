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

DEFINE VARIABLE chGraphPie  AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE hLib        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibTbs     AS HANDLE     NO-UNDO.
DEFINE VARIABLE lIni        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iCurrPoint  AS INTEGER    NO-UNDO.


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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-31 
&Scoped-Define DISPLAYED-OBJECTS fiPortion fiPed fiDes fiCom fiFal 

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
DEFINE VARIABLE h_bcontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemscontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemscontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dqcontratospartes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiCom AS CHARACTER FORMAT "X(256)":U 
     LABEL "Comprom" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE fiDes AS CHARACTER FORMAT "X(256)":U 
     LABEL "Despachados" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE fiFal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Faltantes" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE fiPed AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pedidos" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE fiPortion AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 52 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 12.38.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 8.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiPortion AT ROW 21.14 COL 137 RIGHT-ALIGNED NO-LABEL
     fiPed AT ROW 21.24 COL 8 COLON-ALIGNED
     fiDes AT ROW 21.24 COL 31 COLON-ALIGNED
     fiCom AT ROW 21.24 COL 50 COLON-ALIGNED
     fiFal AT ROW 21.24 COL 72 COLON-ALIGNED
     RECT-1 AT ROW 9.57 COL 85
     RECT-31 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138.4 BY 23.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 3
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Contratos"
         HEIGHT             = 21.1
         WIDTH              = 138.4
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

/* SETTINGS FOR FILL-IN fiCom IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDes IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFal IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPed IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPortion IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       fiPortion:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 9.81
       COLUMN          = 86
       HEIGHT          = 11.19
       WIDTH           = 52
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {827E9F50-96A4-11CF-823E-000021570103} type: Graph */
      RUN adjustTabOrder ( h_folder , CtrlFrame , 'BEFORE':U ).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta Contratos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta Contratos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.HotHit
PROCEDURE CtrlFrame.Graph.HotHit .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    HitSet
    HitPoint
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-HitSet   AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-HitPoint AS INTEGER NO-UNDO.

RUN graphClick(p-HitPoint).


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
             INPUT  'dcontratos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldscontratos.id_contrato,id_contrato,contratos.id_tipo_contrato,id_tipo_contratoRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcontratosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcontratos ).
       RUN repositionObject IN h_dcontratos ( 6.00 , 57.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bcontratos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcontratos ).
       RUN repositionObject IN h_bcontratos ( 2.43 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_bcontratos ( 7.14 , 77.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ditemscontratos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_contratos.id_contrato,id_contrato,items_contratos.id_tipo_contrato,id_tipo_contrato,items_contratos.anio,anioRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemscontratosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemscontratos ).
       RUN repositionObject IN h_ditemscontratos ( 7.67 , 58.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 10.00 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_contrato_of,id_contrato,tambores_industria.id_tipocontrato_of,id_tipo_contrato,tambores_industria.anio_of,anio,tambores_industria.item_of,itemRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria ).
       RUN repositionObject IN h_dtamboresindustria ( 8.38 , 68.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dqcontratospartes.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedqcontratospartesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dqcontratospartes ).
       RUN repositionObject IN h_dqcontratospartes ( 4.10 , 57.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda1SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 77.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsanio,id_contrato,item,id_cliente,id_articulo,orden_fabricacionOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsDesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.48 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 7.86 , 59.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Partes Contrato|Tambores|OEs' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 9.57 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 11.43 , 84.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dcontratos. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dcontratos ).

       /* Links to SmartDataBrowser h_bcontratos. */
       RUN addLink ( h_dcontratos , 'Data':U , h_bcontratos ).

       /* Links to SmartDataObject h_ditemscontratos. */
       RUN addLink ( h_dcontratos , 'Data':U , h_ditemscontratos ).

       /* Links to SmartDataObject h_dtamboresindustria. */
       RUN addLink ( h_ditemscontratos , 'Data':U , h_dtamboresindustria ).

       /* Links to SmartDataObject h_dqcontratospartes. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dqcontratospartes ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'bitemscontratos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemscontratos ).
       RUN repositionObject IN h_bitemscontratos ( 10.76 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemscontratos ( 10.00 , 82.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bitemscontratos. */
       RUN addLink ( h_ditemscontratos , 'Data':U , h_bitemscontratos ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria ).
       RUN repositionObject IN h_btamboresindustria ( 10.76 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria ( 10.00 , 82.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_btamboresindustria. */
       RUN addLink ( h_dtamboresindustria , 'Data':U , h_btamboresindustria ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'ditemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_orden_entrega.id_contrato,id_contrato,items_orden_entrega.item,itemRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsordenentrega ).
       RUN repositionObject IN h_ditemsordenentrega ( 8.38 , 79.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemsordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsordenentrega ).
       RUN repositionObject IN h_bitemsordenentrega ( 10.76 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsordenentrega ( 10.00 , 82.00 ) NO-ERROR.

       /* Links to SmartDataObject h_ditemsordenentrega. */
       RUN addLink ( h_ditemscontratos , 'Data':U , h_ditemsordenentrega ).

       /* Links to SmartDataBrowser h_bitemsordenentrega. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_bitemsordenentrega ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

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

OCXFile = SEARCH( "wqContratos.wrx":U ).
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
ELSE MESSAGE "wqContratos.wrx":U SKIP(1)
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
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          'refreshGraphAction', 
                          'ActuralizarGrafico', 
                          'Actualizar Grafico', 
                          'charts.bmp', 
                          'tlbRefreshGraph', 
                          'BANDA1').



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

  RUN libCommonFunctions.p PERSISTENT SET hLib.
  RUN libTamboresIndustria.p PERSISTENT SET hLibTbs.


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
  DISPLAY fiPortion fiPed fiDes fiCom fiFal 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-31 
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
  DEFINE VARIABLE cData AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLeg  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dVal  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  
  RUN initGraph.

  cData = DYNAMIC-FUNCTION('getCumplimentoContrato' IN hLibTbs, DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato'), 
                                                                DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato'),
                                                                DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio')).
  
  chGraphPie:GraphTitle = "Ctrct: " + DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato').
  chGraphPie:NumPoints  = NUM-ENTRIES(cData, CHR(10)).

  DO i = 1 TO NUM-ENTRIES(cData, CHR(10)):
    cRow = ENTRY(i, cData, CHR(10)).

    dVal = ROUND(DECIMAL(ENTRY(2, cRow, CHR(1))), 2).
    cLeg = STRING(INTEGER(ENTRY(3, cRow, CHR(1)))).
    cLeg = cLeg  + " %". 

    chGraphPie:Legend(i) = ENTRY(4, cRow, CHR(1)).
    chGraphPie:Data(i)   = STRING(dVal).
    chGraphPie:LABEL(i)  = cLeg.

    dTot = dTot + dVal.
  END.
  chGraphPie:Tag = cData.

  chGraphPie:DrawMode = 3.

  ASSIGN fiPed:SCREEN-VALUE IN FRAME fMain = STRING(dTot)
         fiFal:SCREEN-VALUE IN FRAME fMain = ENTRY(2,ENTRY(1,cData, CHR(10)), CHR(1))
         fiDes:SCREEN-VALUE IN FRAME fMain = ENTRY(2,ENTRY(2,cData, CHR(10)), CHR(1))
         fiCom:SCREEN-VALUE IN FRAME fMain = ENTRY(2,ENTRY(3,cData, CHR(10)), CHR(1)).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterContratos wWin 
PROCEDURE filterContratos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.

  IF lIni THEN DO:
    cLst = DYNAMIC-FUNCTION('getContratosList' IN h_dqContratosPartes).
    cLst = REPLACE(cLst, ",", "','").
    cLst = "'" + cLst + "'".
  
    cQry = DYNAMIC-FUNCTION('getFilterString' IN hLib, cLst, 'OR', 'contratos.id_contrato').
    DYNAMIC-FUNCTION('setQueryWhere' IN h_dContratos, cQry).
    DYNAMIC-FUNCTION('openQuery' IN h_dContratos).
  END.
  lIni = TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE graphClick wWin 
PROCEDURE graphClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE INPUT  PARAMETER piHitPoint AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  
  IF iCurrPoint = piHitPoint THEN
    chGraphPie:Extra(piHitPoint) = 0.
  ELSE 
    chGraphPie:Extra(piHitPoint) = 1.

  iCurrPoint = piHitPoint.

  chGraphPie:DrawMode = 3.

  cRow = ENTRY(piHitPoint, chGraphPie:Tag, CHR(10)).

                                          
  fiPortion:SCREEN-VALUE IN FRAME fMain = ENTRY(1, cRow, CHR(1)) + ": " + ENTRY(2, cRow, CHR(1)) + " tbs.".
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initGraph wWin 
PROCEDURE initGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  

/*chGraphPie:GraphTitle = "Composicion Carga".*/
  chGraphPie:DataReset      = 9.
  chGraphPie:Labels         = 1.
  chGraphPie:GraphType      = 2.
  chGraphPie:Hot            = 1.
  chGraphPie:FontUse        = 2.
  chGraphPie:FontSize       = 100.
  chGraphPie:Palette        = 4.
  chGraphPie:NumPoints      = 1.


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

  chGraphPie = chCtrlFrame:Graph.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "refreshGraphAction").  
  SUBSCRIBE TO "tlbRefreshGraph" IN h_dyntoolbar.

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
  
  RUN filterContratos.
  

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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
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
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLin AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.
  DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMem AS CHARACTER  NO-UNDO.


  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  RUN wdParams.w (OUTPUT iSuc,
                  OUTPUT dDes, 
                  OUTPUT dHas, 
                  OUTPUT iArt, 
                  OUTPUT iLin).

  RUN callEstadosContratos IN hLib (0, iArt, dDes, dHas).

  RUN  aderb\_prntrb2(
         "..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
         "estado_contratos",                    /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         cFil,                              /* RB-FILTER */
         cMem,                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         "?",                              /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
         1,                              /* RB-NUMBER-COPIES  - zero */                  
         0,                              /* RB-BEGIN-PAGE - zero */
         0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Reporte de Estados de Contratos",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   



  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbRefreshGraph wWin 
PROCEDURE tlbRefreshGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillGraph.

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

