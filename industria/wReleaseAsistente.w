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
DEFINE INPUT PARAMETER piRelease AS INTEGER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS edtInstructions edtTitle RECT-5 RECT-6 ~
RECT-7 
&Scoped-Define DISPLAYED-OBJECTS edtTitle 

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
DEFINE VARIABLE h_bidcontrato AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemscontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemsreleasedelivery AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemscontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsreleasedelivery AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dreleasedelivery AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbarCustom AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vreleaseresumido AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE edtInstructions AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 45 BY 4.29 NO-UNDO.

DEFINE VARIABLE edtTitle AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
      VIEW-AS TEXT 
     SIZE 18.8 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 10.48.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47.4 BY 11.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     edtInstructions AT ROW 1.24 COL 70 NO-LABEL
     edtTitle AT ROW 6 COL 1.8
     RECT-5 AT ROW 6.24 COL 1
     RECT-6 AT ROW 6.24 COL 69
     RECT-7 AT ROW 1 COL 1
     "Items Release" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 6 COL 70
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.4 BY 17.14.


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
         TITLE              = "Asistente Creacion Item Release"
         HEIGHT             = 17.14
         WIDTH              = 115.4
         MAX-HEIGHT         = 34.33
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.33
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

/* SETTINGS FOR EDITOR edtInstructions IN FRAME fMain
   NO-DISPLAY                                                           */
ASSIGN 
       edtInstructions:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN edtTitle IN FRAME fMain
   ALIGN-L                                                              */
ASSIGN 
       edtTitle:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Asistente Creacion Item Release */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Asistente Creacion Item Release */
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
             INPUT  'dreleasedelivery.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedreleasedeliveryUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dreleasedelivery ).
       RUN repositionObject IN h_dreleasedelivery ( 1.24 , 70.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemsreleasedelivery.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_release_delivery.id_release_delivery,id_release_deliveryRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsreleasedeliveryUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsreleasedelivery ).
       RUN repositionObject IN h_ditemsreleasedelivery ( 1.24 , 81.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemsreleasedelivery.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsreleasedelivery ).
       RUN repositionObject IN h_bitemsreleasedelivery ( 6.71 , 70.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsreleasedelivery ( 10.95 , 46.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vreleaseresumido.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vreleaseresumido ).
       RUN repositionObject IN h_vreleaseresumido ( 1.24 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 4.48 , 64.60 ) */

       RUN constructObject (
             INPUT  'dcontratos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcontratosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcontratos ).
       RUN repositionObject IN h_dcontratos ( 3.38 , 71.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemscontratos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_contratos.id_contrato,id_contrato,items_contratos.id_tipo_contrato,id_tipo_contratoRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemscontratosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemscontratos ).
       RUN repositionObject IN h_ditemscontratos ( 3.62 , 82.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dloteubicacion ).
       RUN repositionObject IN h_dloteubicacion ( 3.38 , 94.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsFunctionSubModulesTableIOTypeSupportedLinksToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsFilter,MybuttonactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbarCustom ).
       RUN repositionObject IN h_dyntoolbarCustom ( 16.71 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbarCustom ( 1.24 , 68.00 ) NO-ERROR.

       /* Links to SmartDataObject h_ditemsreleasedelivery. */
       RUN addLink ( h_dreleasedelivery , 'Data':U , h_ditemsreleasedelivery ).

       /* Links to SmartDataBrowser h_bitemsreleasedelivery. */
       RUN addLink ( h_ditemsreleasedelivery , 'Data':U , h_bitemsreleasedelivery ).

       /* Links to SmartDataViewer h_vreleaseresumido. */
       RUN addLink ( h_dreleasedelivery , 'Data':U , h_vreleaseresumido ).

       /* Links to SmartDataObject h_ditemscontratos. */
       RUN addLink ( h_dcontratos , 'Data':U , h_ditemscontratos ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'bitemscontratos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemscontratos ).
       RUN repositionObject IN h_bitemscontratos ( 6.71 , 20.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemscontratos ( 9.52 , 48.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bidcontrato.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bidcontrato ).
       RUN repositionObject IN h_bidcontrato ( 6.71 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bidcontrato ( 9.52 , 17.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bitemscontratos. */
       RUN addLink ( h_ditemscontratos , 'Data':U , h_bitemscontratos ).

       /* Links to SmartDataBrowser h_bidcontrato. */
       RUN addLink ( h_dcontratos , 'Data':U , h_bidcontrato ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'bloteubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bloteubicacion ).
       RUN repositionObject IN h_bloteubicacion ( 6.71 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bloteubicacion ( 9.52 , 66.00 ) NO-ERROR.

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
  DISPLAY edtTitle 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE edtInstructions edtTitle RECT-5 RECT-6 RECT-7 
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

  
  /*Boton Atras*/
  RUN ToolBarButtonAdd.p (h_dyntoolbarCustom, 
                          "tlbActBack", 
                          "btnBack", 
                          "Atras", 
                          "Anterior.gif", 
                          "tlbBtnBack", 
                          "FUNCTION").
  /*Boton Siguente*/
  RUN ToolBarButtonAdd.p (h_dyntoolbarCustom, 
                          "tlbActNext", 
                          "btnNext", 
                          "Siguiente", 
                          "Siguiente.gif", 
                          "tlbBtnNext", 
                          "FUNCTION").
  /*Boton Cancelar*/
  RUN ToolBarButtonAdd.p (h_dyntoolbarCustom, 
                          "tlbActCancelar", 
                          "btnCancelar", 
                          "Cancelar", 
                          "cross.bmp", 
                          "tlbBtnCancelar", 
                          "FUNCTION").
  /*Boton Aceptar*/
  RUN ToolBarButtonAdd.p (h_dyntoolbarCustom, 
                          "tlbActAceptar", 
                          "btnAceptar", 
                          "Aceptar", 
                          "check.bmp", 
                          "tlbBtnAceptar", 
                          "FUNCTION").
  
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbarCustom, "tlbActCancelar").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbarCustom, "tlbActNext").
  
  SUBSCRIBE TO 'tlbBtnCancelar' IN h_dyntoolbarCustom.
  SUBSCRIBE TO 'tlbBtnAceptar'  IN h_dyntoolbarCustom.
  SUBSCRIBE TO 'tlbBtnNext'     IN h_dyntoolbarCustom.
  SUBSCRIBE TO 'tlbBtnBack'     IN h_dyntoolbarCustom.

  /*filtro sdo releases por el parametro pasado en return-value*/

  DEFINE VARIABLE cQuery   AS CHARACTER  NO-UNDO.

  IF piRelease <> ? THEN DO:
    cQuery   = "FOR EACH general.release_delivery WHERE release_delivery.id_release_delivery = " + STRING(piRelease) + " NO-LOCK ".
    DYNAMIC-FUNCTION('setQueryWhere' IN h_dreleasedelivery, INPUT cQuery ).
    DYNAMIC-FUNCTION('openQuery' IN h_dReleaseDelivery).
    RUN openQueryFromCliente IN h_dContratos (DYNAMIC-FUNCTION('columnValue' IN h_dReleaseDelivery, "id_cliente")).
  END.

  edtInstructions:SCREEN-VALUE IN FRAME fMain = "Selecciones las Partes de Contrato Pertenecientes al cliente del Release y presione Siguiente".
  edtTitle:SCREEN-VALUE IN FRAME fMain = "Partes Contrato".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbBtnAceptar wWin 
PROCEDURE tlbBtnAceptar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR cRows AS CHARACTER NO-UNDO.
  DEFINE VAR hRowObject AS HANDLE NO-UNDO.

  hRowObject = DYNAMIC-FUNCTION ('getRowObject' IN h_ditemscontratos).

  cRows =  DYNAMIC-FUNCTION('getSelectedRows' IN h_bLoteUbicacion).

  IF cRows <> "" THEN DO:
    DYNAMIC-FUNCTION('createItemReleaseFromLUbicacionContrato' IN h_ditemsreleasedelivery , INPUT h_dLoteubicacion , INPUT cRows , 
                      INPUT hRowObject:BUFFER-FIELD('id_contrato'):BUFFER-VALUE , INPUT hRowObject:BUFFER-FIELD('item'):BUFFER-VALUE).

    DYNAMIC-FUNCTION('openQuery' IN h_dloteubicacion).
    IF RETURN-VALUE <> "" THEN DO:
      MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbBtnBack wWin 
PROCEDURE tlbBtnBack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  RUN selectPage(DYNAMIC-FUNCTION('getCurrentPage') - 1).
  IF DYNAMIC-FUNCTION('getCurrentPage') <=2 THEN DO:
    edtTitle:SCREEN-VALUE IN FRAME fMain = "Lotes Deposito".
    edtInstructions:SCREEN-VALUE IN FRAME fMain = "Seleccione los lotes que desea incorporar como item del release y haga click en aceptar para agregarlos.".
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbarCustom, "tlbActAceptar").
  END.
  IF DYNAMIC-FUNCTION('getCurrentPage') <= 1 THEN DO:
    edtInstructions:SCREEN-VALUE IN FRAME fMain = "Selecciones las Partes de Contrato Pertenecientes al cliente del Release y presione Siguiente".
    edtTitle:SCREEN-VALUE IN FRAME fMain = "Partes Contrato".
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbarCustom, "tlbActBack").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbBtnCancelar wWin 
PROCEDURE tlbBtnCancelar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbBtnNext wWin 
PROCEDURE tlbBtnNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRows   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  
  RUN selectPage(DYNAMIC-FUNCTION('getCurrentPage') + 1).
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbarCustom, "tlbActBack").
  
  IF DYNAMIC-FUNCTION('getCurrentPage') = 2 THEN DO:
    edtTitle:SCREEN-VALUE IN FRAME fMain = "Lotes Deposito".
    edtInstructions:SCREEN-VALUE IN FRAME fMain = "Seleccione los lotes que desea incorporar como item del release y haga click en aceptar para agregarlos.".
    cRows =  DYNAMIC-FUNCTION('getSelectedRows' IN h_bItemsContratos).
    DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
      cFields = DYNAMIC-FUNCTION('fetchRow' IN h_dItemsContratos , INTEGER(ENTRY(iRow,cRows)), 'id_articulo').
      IF cFields EQ ? OR cFields = "" THEN DO: 
        RETURN-VALUE = "Error en cFields".
        RETURN "Error".
      END.
      RUN openQueryForContrato IN h_dloteubicacion (DYNAMIC-FUNCTION('columnValue' IN h_dReleaseDelivery, "id_sucursal_ubicacion"), INTEGER(ENTRY(2, cFields, CHR(1)))).
    END.
  END.

  IF DYNAMIC-FUNCTION('getCurrentPage') = 1 THEN DO:
    edtInstructions:SCREEN-VALUE IN FRAME fMain = "Selecciones las Partes de Contrato Pertenecientes al cliente del Release y presione Siguiente".
    edtTitle:SCREEN-VALUE IN FRAME fMain = "Partes Contrato".
  END.
  
  IF DYNAMIC-FUNCTION('getCurrentPage') >= 2 THEN DO:
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbarCustom, "tlbActAceptar").
  END.
  IF DYNAMIC-FUNCTION('getCurrentPage') >= 3 THEN DO:
    RUN selectPage(2).
  END.
  

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

