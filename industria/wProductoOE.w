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
&Scoped-Define ENABLED-OBJECTS fiAdd fiRemove btnAdd btnRemove RECT-1 ~
RECT-3 
&Scoped-Define DISPLAYED-OBJECTS fiAdd fiRemove 

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
DEFINE VARIABLE h_bitemsoe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_blotesaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_blotesjugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotesaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotesjugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "src/adm2/image/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "(+) >>" 
     SIZE 5.2 BY 1.19.

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "src/adm2/image/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "(-) <<" 
     SIZE 5.2 BY 1.19.

DEFINE VARIABLE fiAdd AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiRemove AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY 1.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiAdd AT ROW 14.33 COL 75.4 COLON-ALIGNED NO-LABEL
     fiRemove AT ROW 15.95 COL 75.4 COLON-ALIGNED NO-LABEL
     btnAdd AT ROW 14.19 COL 71.6
     btnRemove AT ROW 15.76 COL 71.6
     RECT-1 AT ROW 14.1 COL 71
     RECT-3 AT ROW 15.67 COL 71
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 139.6 BY 23.43
         FGCOLOR 25 .


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
         TITLE              = "Vincular Producto a OEs"
         HEIGHT             = 23.43
         WIDTH              = 139.8
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
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Vincular Producto a OEs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Vincular Producto a OEs */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd wWin
ON CHOOSE OF btnAdd IN FRAME fMain /* (+) >> */
DO: 
  MESSAGE DYNAMIC-FUNCTION('getCurrentPage':U)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  /*
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  viCant = DYNAMIC-FUNCTION('columnValue' IN h_dProduccionCascara, 'bolsas_disponibles').
  IF (INTEGER(fiAdd:SCREEN-VALUE) > 0 AND INTEGER(fiAdd:SCREEN-VALUE) <= viCant)   THEN DO:
    RUN addQty IN h_dLotesCascara (INTEGER(fiAdd:SCREEN-VALUE), 
                                   DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'id_empresa'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'id_sucursal'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'nromov'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_dProduccionCascara, 'id_sucursal'), 
                                   DYNAMIC-FUNCTION('columnValue' IN h_dProduccionCascara, 'id_produccion'), 
                                   "+").
    RUN refreshRow IN h_dLotesCascara.
    RUN refreshRow IN h_dProduccionCascara.
    DYNAMIC-FUNCTION('openQuery' IN h_dRProduccionLoteCascara).
    fiBolsasRestantes:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('getCantidadRestante' IN h_dLotesCascara)).
    fiAdd:SCREEN-VALUE = "".
    fiRemove:SCREEN-VALUE = "".
  END.
  ELSE DO:
    MESSAGE "La cantidad debe estar entre 1 y " + STRING(viCant) + " o esta produccion ya tiene el total de las bolsas comprometidas." VIEW-AS ALERT-BOX TITLE "Cantidad Erronea".
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove wWin
ON CHOOSE OF btnRemove IN FRAME fMain /* (-) << */
DO:
  /*
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  viCant = DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'cantidad').
  IF (INTEGER(fiRemove:SCREEN-VALUE) > 0 AND INTEGER(fiRemove:SCREEN-VALUE) <= viCant)   THEN DO:
    RUN addQty IN h_dLotesCascara (INTEGER(fiRemove:SCREEN-VALUE), 
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'id_empresa_lote'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'id_sucursal_lote'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'nromov_lote'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'id_sucursal_prod'), 
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'id_produccion_prod'), 
                                   "-").
    RUN refreshRow IN h_dLotesCascara.
    DYNAMIC-FUNCTION('openQuery' IN h_dProduccionCascara).
    DYNAMIC-FUNCTION('openQuery' IN h_drProduccionLoteCascara).
    fiBolsasRestantes:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('getCantidadRestante' IN h_dLotesCascara)).
    fiRemove:SCREEN-VALUE = "".
    fiAdd:SCREEN-VALUE = "".
  END.
  ELSE DO:
    MESSAGE "La cantidad debe estar entre 1 y " + STRING(viCant) VIEW-AS ALERT-BOX TITLE "Cantidad Erronea".
  END.
  */
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
             INPUT  'ditemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_ditemsordenentrega ).
       RUN repositionObject IN h_ditemsordenentrega ( 2.67 , 67.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemsoe.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsoe ).
       RUN repositionObject IN h_bitemsoe ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsoe ( 5.71 , 71.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_orden_entrega,id_orden_entrega,tambores_industria.item_oe,item_oeRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dtamboresindustria ).
       RUN repositionObject IN h_dtamboresindustria ( 4.33 , 67.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria ).
       RUN repositionObject IN h_btamboresindustria ( 7.43 , 87.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria ( 16.43 , 53.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dlotesjugo.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotesjugoUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dlotesjugo ).
       RUN repositionObject IN h_dlotesjugo ( 18.14 , 77.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dlotesaceite.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotesaceiteUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dlotesaceite ).
       RUN repositionObject IN h_dlotesaceite ( 20.05 , 77.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda1SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 73.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 67.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_orden_entrega,item_oe,id_contratoOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsid_contrato0id_orden_entrega0item_oe0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.43 , 76.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.43 , 62.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Jugo|Aceite' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 7.43 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 16.91 , 69.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1,2') NO-ERROR.

       /* Links to SmartDataObject h_ditemsordenentrega. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_ditemsordenentrega ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_ditemsordenentrega ).

       /* Links to SmartDataBrowser h_bitemsoe. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_bitemsoe ).

       /* Links to SmartDataObject h_dtamboresindustria. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_dtamboresindustria ).

       /* Links to SmartDataBrowser h_btamboresindustria. */
       RUN addLink ( h_dtamboresindustria , 'Data':U , h_btamboresindustria ).

       /* Links to SmartDataObject h_dlotesjugo. */
       RUN addLink ( h_dynfilter-2 , 'Filter':U , h_dlotesjugo ).

       /* Links to SmartDataObject h_dlotesaceite. */
       RUN addLink ( h_dynfilter-3 , 'Filter':U , h_dlotesaceite ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'blotesjugo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotesjugo ).
       RUN repositionObject IN h_blotesjugo ( 13.14 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_blotesjugo ( 10.95 , 67.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_lote,anio,id_articuloOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_loteLoteFieldToolTipsFieldHelpIdsid_lote0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-2 ).
       RUN repositionObject IN h_dynfilter-2 ( 8.62 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-2 ( 4.43 , 67.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_blotesjugo. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_blotesjugo ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'blotesaceite.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotesaceite ).
       RUN repositionObject IN h_blotesaceite ( 13.62 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_blotesaceite ( 10.48 , 67.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_lote,anio,id_articuloOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsanio0id_articulo0id_lote0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-3 ).
       RUN repositionObject IN h_dynfilter-3 ( 8.62 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-3 ( 4.43 , 67.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_blotesaceite. */
       RUN addLink ( h_dlotesaceite , 'Data':U , h_blotesaceite ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

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
  DISPLAY fiAdd fiRemove 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fiAdd fiRemove btnAdd btnRemove RECT-1 RECT-3 
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

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  /*RUN initQuery.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initQuery wWin 
PROCEDURE initQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.

  cQry = "items_orden_entrega.fecha >= DATE('01/01/2006')".
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dItemsOrdenEntrega, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dItemsOrdenEntrega).
  
  
  cQry = "id_orden_entrega = 0 AND item_oe = 0 AND fecha >= DATE('01/01/" + STRING(YEAR(TODAY) - 1).
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dLotesJugo, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dLotesJugo).

  cQry = "id_orden_entrega = 0 AND item_oe = 0 AND id_articulo = 51 AND fecha >= DATE('01/01/" + STRING(YEAR(TODAY) - 1).  
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dLotesAceite, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dLotesAceite).


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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.


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

