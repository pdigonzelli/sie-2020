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
&Scoped-Define ENABLED-OBJECTS RECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteUbicacion wWin 
FUNCTION getLoteUbicacion RETURNS HANDLE
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
DEFINE VARIABLE h_bingresoloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemingresoloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dingresoloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemingresoloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vingreso AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vitemingresoloteubicacion AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 4.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-1 AT ROW 2.57 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.4 BY 24.52.


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
         TITLE              = "Ingreso de Lotes a Deposito de Mercado Externo"
         HEIGHT             = 24.52
         WIDTH              = 96.4
         MAX-HEIGHT         = 35.67
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 35.67
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
ON END-ERROR OF wWin /* Ingreso de Lotes a Deposito de Mercado Externo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Ingreso de Lotes a Deposito de Mercado Externo */
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
             INPUT  'dingresoloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedingresoloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dingresoloteubicacion ).
       RUN repositionObject IN h_dingresoloteubicacion ( 16.71 , 87.00 ) NO-ERROR.
       /* Size in AB:  ( 1.71 , 10.20 ) */

       RUN constructObject (
             INPUT  'ditemingresoloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitem_ingreso_lote_ubicacion.id_sucursal_ubicacion,id_sucursal_ubicacion,item_ingreso_lote_ubicacion.nromov_ingreso,nromov_ingresoRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemingresoloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemingresoloteubicacion ).
       RUN repositionObject IN h_ditemingresoloteubicacion ( 19.10 , 87.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 9.00 ) */

       RUN constructObject (
             INPUT  'dloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dloteubicacion ).
       RUN repositionObject IN h_dloteubicacion ( 21.24 , 87.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 9.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Function,Banda1SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsFilter,PrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 94.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2') NO-ERROR.

       /* Links to SmartDataObject h_dingresoloteubicacion. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dingresoloteubicacion ).

       /* Links to SmartDataObject h_ditemingresoloteubicacion. */
       RUN addLink ( h_dingresoloteubicacion , 'Data':U , h_ditemingresoloteubicacion ).
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_ditemingresoloteubicacion ).

       /* Links to SmartDataObject h_dloteubicacion. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dloteubicacion ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_dloteubicacion ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'bingresoloteubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bingresoloteubicacion ).
       RUN repositionObject IN h_bingresoloteubicacion ( 2.67 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bingresoloteubicacion ( 6.67 , 94.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vingreso.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vingreso ).
       RUN repositionObject IN h_vingreso ( 9.81 , 13.00 ) NO-ERROR.
       /* Size in AB:  ( 5.48 , 71.00 ) */

       /* Links to SmartDataBrowser h_bingresoloteubicacion. */
       RUN addLink ( h_dingresoloteubicacion , 'Data':U , h_bingresoloteubicacion ).

       /* Links to SmartDataViewer h_vingreso. */
       RUN addLink ( h_dingresoloteubicacion , 'Data':U , h_vingreso ).
       RUN addLink ( h_vingreso , 'Update':U , h_dingresoloteubicacion ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vingreso ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'bitemingresoloteubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth0FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemingresoloteubicacion ).
       RUN repositionObject IN h_bitemingresoloteubicacion ( 15.76 , 7.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemingresoloteubicacion ( 5.00 , 79.80 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vitemingresoloteubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vitemingresoloteubicacion ).
       RUN repositionObject IN h_vitemingresoloteubicacion ( 21.00 , 9.00 ) NO-ERROR.
       /* Size in AB:  ( 3.91 , 74.40 ) */

       RUN constructObject (
             INPUT  'bloteubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth0FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bloteubicacion ).
       RUN repositionObject IN h_bloteubicacion ( 7.43 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bloteubicacion ( 6.43 , 94.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 1.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 36.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldssaldo,id_loteOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_loteLote SMFieldToolTipsFieldHelpIdsLoteDeposito0id_articulo0id_lote0calidad0envase0saldo0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.67 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 3.71 , 92.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda3SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsAdd,CopyHiddenActionsFilter,Principalaction,Wizitemsaction,Printaction,CustomactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsAdd,CopyNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 14.33 , 7.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 1.24 , 80.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bitemingresoloteubicacion. */
       RUN addLink ( h_ditemingresoloteubicacion , 'Data':U , h_bitemingresoloteubicacion ).

       /* Links to SmartDataViewer h_vitemingresoloteubicacion. */
       RUN addLink ( h_ditemingresoloteubicacion , 'Data':U , h_vitemingresoloteubicacion ).
       RUN addLink ( h_vitemingresoloteubicacion , 'Update':U , h_ditemingresoloteubicacion ).
       RUN addLink ( h_dyntoolbar-3 , 'TableIo':U , h_vitemingresoloteubicacion ).

       /* Links to SmartDataBrowser h_bloteubicacion. */
       RUN addLink ( h_dloteubicacion , 'Data':U , h_bloteubicacion ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnIngreso wWin 
PROCEDURE btnIngreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR cRows AS CHARACTER NO-UNDO.
  DEFINE VAR iRow  AS INTEGER   NO-UNDO.

  cRows = DYNAMIC-FUNCTION ('getSelectedRows' IN h_bLoteUbicacion).
  DYNAMIC-FUNCTION ('createItemFromLoteUbicacion' IN h_ditemIngresoLoteUbicacion , h_dLoteUbicacion , cRows).
  IF RETURN-VALUE <> "" THEN
      MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnItemsIngreso wWin 
PROCEDURE btnItemsIngreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR cRows AS CHARACTER NO-UNDO.
  DEFINE VAR iRow  AS INTEGER   NO-UNDO.

  IF getCurrentPage() = 1 THEN
  DO:
    cRows = DYNAMIC-FUNCTION ('getSelectedRows' IN h_bIngresoLoteUbicacion).
    IF cRows <> "" THEN DO:        
      DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "PrincipalAction").
      DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "WizItemsAction").
      RUN selectPage IN THIS-PROCEDURE (2).
          /*Boton de Ingreso*/   
  RUN ToolBarButtonAdd.p (h_dyntoolbar-3, 
                          "IngresoAction", 
                          "btnIngreso", 
                          "Reportes", 
                          "Ingreso.gif", 
                          "btnIngreso", 
                          "FUNCTION").
    END.
    ELSE MESSAGE "Debe selecionar un ingreso" VIEW-AS ALERT-BOX WARNING.
  END.
  ELSE DO:
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "PrincipalAction").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "WizItemsAction").
    RUN selectPage IN THIS-PROCEDURE (1).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnPrincipal wWin 
PROCEDURE btnPrincipal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR cRows AS CHARACTER NO-UNDO.
  DEFINE VAR iRow  AS INTEGER   NO-UNDO.

  IF getCurrentPage() = 1 THEN
  DO:
    cRows = DYNAMIC-FUNCTION ('getSelectedRows' IN h_bIngresoLoteUbicacion).
    IF cRows <> "" THEN DO:        
      DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "PrincipalAction").
      DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "WizItemsAction").
      RUN selectPage IN THIS-PROCEDURE (2).
    END.
    ELSE MESSAGE "Debe selecionar un ingreso" VIEW-AS ALERT-BOX WARNING.
  END.
  ELSE DO:
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "PrincipalAction").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "WizItemsAction").
    RUN selectPage IN THIS-PROCEDURE (1).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnPrintReport wWin 
PROCEDURE btnPrintReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wReportesIngresosLoteUbicacion.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnProcesarIngreso wWin 
PROCEDURE btnProcesarIngreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR lResp        AS LOGICAL FORMAT "Si/No" NO-UNDO.
    DEFINE VAR hRowObject   AS HANDLE NO-UNDO.

    RUN procesarIngresoLoteUbicacion IN h_dIngresoLoteUbicacion.
    IF RETURN-VALUE <> "" THEN
    DO:
        MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
    END.
    MESSAGE "Emite Comprobante de Ingreso" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lResp.
    IF lResp THEN
    DO:
        RUN emiteIngresoLoteUbicacion IN h_dIngresoLoteUbicacion.
    END.


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
DEFINE VAR hRowObject AS HANDLE NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  
{adm2/support/changePage.i}.  

 
  RUN SUPER.
  

  /* Code placed here will execute AFTER standard behavior.    */
IF getCurrentPage() <> 2 THEN DO:
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "ProcessAction").
  DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar-3, "IngresoAction"). 
END.
ELSE DO:
  DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "ProcessAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-3, "IngresoAction").  
END.
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

  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN resetToolbar IN h_dyntoolbar.
  RUN resetToolbar IN h_dyntoolbar-3.

  /*Boton de Principal*/
  
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "PrincipalAction", 
                          "btnPrincipal", 
                          "Volver a Principal", 
                          "Home.gif", 
                          "btnPrincipal", 
                          "FUNCTION").
  /*Boton de Items*/
  
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "WizItemsAction", 
                          "btnItemsIngreso", 
                          "Agregar o quitar Items de Ingreso", 
                          "Items.gif", 
                          "btnItemsIngreso", 
                          "FUNCTION").
  /*Boton de Procesamiento*/
  
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "ProcessAction", 
                          "btnProcesarIngreso", 
                          "Procesar Ingreso", 
                          "Procesar.gif", 
                          "btnProcesarIngreso", 
                          "FUNCTION").
  /*Boton de Impresion*/
  
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "PrintAction", 
                          "btnPrintReport", 
                          "Reportes", 
                          "print.bmp", 
                          "btnPrintReport", 
                          "FUNCTION").
  /*Boton de Ingreso*/
  
  RUN ToolBarButtonAdd.p (h_dyntoolbar-3, 
                          "IngresoAction", 
                          "btnIngreso", 
                          "Ingresar Lote", 
                          "Ingreso.gif", 
                          "btnIngreso", 
                          "BANDA3").



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
  ENABLE RECT-1 
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

  RUN destroyObject IN h_dyntoolbar.
  RUN destroyObject IN h_dyntoolbar-2.
  RUN destroyObject IN h_dyntoolbar-3.
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
  
  /* Code placed here will execute AFTER standard behavior.    */

  RUN openLoteUbicacion.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "WizItemsAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "PrintAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "ProcessAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.
  SUBSCRIBE TO "btnItemsIngreso"    IN h_dyntoolbar.
  SUBSCRIBE TO "btnPrintReport"     IN h_dyntoolbar.
  SUBSCRIBE TO "btnPrincipal"       IN h_dyntoolbar.
  SUBSCRIBE TO "btnProcesarIngreso" IN h_dyntoolbar.
  SUBSCRIBE TO "btnIngreso"         IN h_dyntoolbar-3.
  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openLoteUbicacion wWin 
PROCEDURE openLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DYNAMIC-FUNCTION('openDesdeSucursal' IN h_dLoteUbicacion , 82).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paintBrowser wWin 
PROCEDURE paintBrowser :
/*------------------------------------------------------------------------------
  Purpose:     este procedure es convocado por el browser de bLoteUbicacion
               pero se utiliza en la pantalla de releases aqui no hace nada,
               esta puesto para que no de error.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE suscribeToolbarButton wWin 
PROCEDURE suscribeToolbarButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER phToolbar AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER pcName    AS CHARACTER NO-UNDO.
  
  
  DYNAMIC-FUNCTION( 'createToolbar':U IN phToolbar, "myButtonAction" ).

  /* enable my action */
  DYNAMIC-FUNCTION( 'enableActions' IN phToolbar, "myButtonAction" ).
  /* subscribe the container to act at my action */
  SUBSCRIBE TO pcName IN phToolbar.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbItems wWin 
PROCEDURE tlbItems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  MESSAGE "ahi va" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteUbicacion wWin 
FUNCTION getLoteUbicacion RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN h_dloteUbicacion.   /* Function return value. */

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

