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
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE hApp AS HANDLE     NO-UNDO.
DEFINE VARIABLE hAsy AS HANDLE     NO-UNDO.
DEFINE VARIABLE lFlg AS LOGICAL    NO-UNDO.


DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.

DEFINE  TEMP-TABLE ttReport
  RCODE-INFORMATION
  FIELD oe                AS CHARACTER COLUMN-LABEL "OE"
  FIELD id_cliente        AS CHARACTER COLUMN-LABEL "Cod.Cliente"
  FIELD cliente           AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD contrato          AS CHARACTER COLUMN-LABEL "Contrato"
  FIELD id_producto       AS CHARACTER COLUMN-LABEL "Cod.Producto"
  FIELD producto          AS CHARACTER COLUMN-LABEL "Producto"
  FIELD calidad           AS CHARACTER COLUMN-LABEL "Calidad"
  FIELD envase            AS CHARACTER COLUMN-LABEL "Envase"
  FIELD cantidad          AS CHARACTER COLUMN-LABEL "Cantidad"
  FIELD nro_lote          AS CHARACTER COLUMN-LABEL "Lote"
  FIELD id_vapor          AS CHARACTER COLUMN-LABEL "Cod.Vapor"
  FIELD vapor             AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD fecha_vapor       AS CHARACTER COLUMN-LABEL "Fecha Vapor"
  FIELD contenedor        AS CHARACTER COLUMN-LABEL "Contenedor"
  FIELD eta               AS CHARACTER COLUMN-LABEL "Eta"
  FIELD eta_real          AS CHARACTER COLUMN-LABEL "Eta Real"
  FIELD customs_entry     AS CHARACTER COLUMN-LABEL "Customs Entry"
  FIELD fda_released      AS CHARACTER COLUMN-LABEL "Fda Released"
  FIELD customs_released  AS CHARACTER COLUMN-LABEL "Customs Released"
  FIELD PICK_up           AS CHARACTER COLUMN-LABEL "Pick Up"
  FIELD delivered         AS CHARACTER COLUMN-LABEL "Delivered"
  FIELD id_destino        AS CHARACTER COLUMN-LABEL "Cod.Destino"
  FIELD destino           AS CHARACTER COLUMN-LABEL "Destino"
  FIELD factura           AS CHARACTER COLUMN-LABEL "Factura".

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
&Scoped-Define ENABLED-OBJECTS radFilter lstFilter cmbOps fiValue ~
btnAddClause btnDelClause btnFilter btnReset RECT-27 RECT-28 RECT-8 
&Scoped-Define DISPLAYED-OBJECTS radFilter lstFilter cmbOps fiValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createButtons wWin 
FUNCTION createButtons RETURNS CHARACTER
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
DEFINE VARIABLE h_bcontenedores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_benvio AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcontenedores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_denvio AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcontenedores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_venvio AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddClause 
     IMAGE-UP FILE "src/adm2/image/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Agregar" 
     SIZE 4.8 BY 1.05 TOOLTIP "Agregar".

DEFINE BUTTON btnDelClause 
     IMAGE-UP FILE "src/adm2/image/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Quitar" 
     SIZE 4.8 BY 1.05 TOOLTIP "Qutar".

DEFINE BUTTON btnFilter 
     IMAGE-UP FILE "src/adm2/image/filter.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Filtrar" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "src/adm2/image/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Deshacer" 
     SIZE 4.8 BY 1.05 TOOLTIP "Deshacer".

DEFINE VARIABLE cmbOps AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS ">=","<=","=" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiValue AS CHARACTER FORMAT "x(256)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE radFilter AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "OE", 1,
"Semana", 2,
"Cod. Envio", 3,
"Anio", 4,
"ETA", 5
     SIZE 14 BY 5.71 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.24.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.24.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 6.19.

DEFINE VARIABLE lstFilter AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 4.05 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     radFilter AT ROW 2.67 COL 73 NO-LABEL
     lstFilter AT ROW 4.1 COL 111 NO-LABEL
     cmbOps AT ROW 5.52 COL 87 COLON-ALIGNED NO-LABEL
     fiValue AT ROW 5.52 COL 94 COLON-ALIGNED NO-LABEL
     btnAddClause AT ROW 2.76 COL 111.8
     btnDelClause AT ROW 2.76 COL 122.6
     btnFilter AT ROW 2.76 COL 128
     btnReset AT ROW 2.76 COL 139.2
     RECT-27 AT ROW 2.67 COL 111
     RECT-28 AT ROW 2.67 COL 127.6
     RECT-8 AT ROW 2.43 COL 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 146.6 BY 24.38.


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
         TITLE              = "Administracion de Envios"
         HEIGHT             = 24.38
         WIDTH              = 146.6
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

ASSIGN 
       btnAddClause:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnDelClause:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnFilter:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnReset:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Administracion de Envios */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Administracion de Envios */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  


  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddClause
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddClause wWin
ON CHOOSE OF btnAddClause IN FRAME fMain /* Agregar */
DO:
  DEFINE VARIABLE cFilterStr AS CHARACTER  NO-UNDO.

  CASE INTEGER(radFilter:SCREEN-VALUE).
    WHEN 1 THEN 
      cFilterStr = "id_orden_entrega;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 2 THEN
      cFilterStr = "semana;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 3 THEN
      cFilterStr = "id_envio;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 4 THEN
      cFilterStr = "anio;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 5 THEN
      cFilterStr = "fecha_llegada_destino;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
  END CASE.
  
  lstFilter:ADD-LAST(cFilterStr).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelClause
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelClause wWin
ON CHOOSE OF btnDelClause IN FRAME fMain /* Quitar */
DO:
  lstFilter:DELETE(lstFilter:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter wWin
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

  DYNAMIC-FUNCTION('assignQuerySelection' IN h_dEnvio, cColumns, cValues, cOperators).
  DYNAMIC-FUNCTION('openQuery' IN h_dEnvio).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset wWin
ON CHOOSE OF btnReset IN FRAME fMain /* Deshacer */
DO:
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  cQry = lstFilter:LIST-ITEMS.
  DO i = 1 TO NUM-ENTRIES(cQry):
    lstFilter:DELETE(ENTRY(i, cQry)).
  END.
  
  cQry = DYNAMIC-FUNCTION('getQueryString' IN h_dEnvio).
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dEnvio, cQry).    
  DYNAMIC-FUNCTION('openQuery' IN h_dEnvio).

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
             INPUT  'denvio.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedenvioUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_denvio ).
       RUN repositionObject IN h_denvio ( 2.67 , 93.00 ) NO-ERROR.
       /* Size in AB:  ( 2.14 , 10.60 ) */

       RUN constructObject (
             INPUT  'benvio.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_benvio ).
       RUN repositionObject IN h_benvio ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_benvio ( 7.62 , 70.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Function,Banda1,Banda2SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsCopy,AddHiddenActionsFilter,PrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsCopy,AddNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 72.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 75.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Envio|Contenedores' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 8.86 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 16.43 , 145.00 ) NO-ERROR.

       /* Links to SmartDataObject h_denvio. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_denvio ).

       /* Links to SmartDataBrowser h_benvio. */
       RUN addLink ( h_denvio , 'Data':U , h_benvio ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'venvio.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_venvio ).
       RUN repositionObject IN h_venvio ( 10.05 , 11.00 ) NO-ERROR.
       /* Size in AB:  ( 15.05 , 128.60 ) */

       /* Links to SmartDataViewer h_venvio. */
       RUN addLink ( h_denvio , 'Data':U , h_venvio ).
       RUN addLink ( h_venvio , 'Update':U , h_denvio ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_venvio ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'dcontenedores.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldscontenedores.id_envio,id_envioRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcontenedoresUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcontenedores ).
       RUN repositionObject IN h_dcontenedores ( 11.24 , 122.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bcontenedores.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcontenedores ).
       RUN repositionObject IN h_bcontenedores ( 11.71 , 33.00 ) NO-ERROR.
       RUN resizeObject IN h_bcontenedores ( 6.67 , 81.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vcontenedores.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcontenedores ).
       RUN repositionObject IN h_vcontenedores ( 18.62 , 33.00 ) NO-ERROR.
       /* Size in AB:  ( 6.24 , 81.40 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsAdd,Delete,CopyHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsAdd,Delete,CopyNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 10.29 , 33.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 81.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dcontenedores. */
       RUN addLink ( h_denvio , 'Data':U , h_dcontenedores ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_dcontenedores ).

       /* Links to SmartDataBrowser h_bcontenedores. */
       RUN addLink ( h_dcontenedores , 'Data':U , h_bcontenedores ).

       /* Links to SmartDataViewer h_vcontenedores. */
       RUN addLink ( h_dcontenedores , 'Data':U , h_vcontenedores ).
       RUN addLink ( h_vcontenedores , 'Update':U , h_dcontenedores ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vcontenedores ).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectApp wWin 
PROCEDURE connectApp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE SERVER hApp.
  lFlg = hApp:CONNECT("-AppService asindustria -H 192.168.1.231").

  IF lFlg THEN 
    /*RUN libReportes.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.*/
    RUN pGeneraEnvios-Facu.p ON SERVER hApp TRANSACTION DISTINCT.
  ELSE 
    /*RUN libReportes.p PERSISTENT SET hLib .*/
    RUN pGeneraEnvios-Facu.p.

  
  IF lFlg THEN DO:
    hApp:DISCONNECT().
    DELETE OBJECT hApp.
    /*DELETE OBJECT hLib.*/
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

  /*createButtons().*/

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
  DISPLAY radFilter lstFilter cmbOps fiValue 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE radFilter lstFilter cmbOps fiValue btnAddClause btnDelClause btnFilter 
         btnReset RECT-27 RECT-28 RECT-8 
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

  RUN connectApp.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.
  
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "excelAction").
  SUBSCRIBE TO "tlbExcel" IN h_dyntoolbar.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExcel wWin 
PROCEDURE tlbExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillTTReport IN h_dEnvio (INPUT-OUTPUT TABLE ttReport).

  RUN generateExcel.p (INPUT TABLE ttReport,
                       INPUT " Detalles de Envios ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createButtons wWin 
FUNCTION createButtons RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*Boton para Agregar Clientes*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "exportAction", 
                          "tlbExport", 
                          "Exportar a Excel", 
                          "excel.bmp", 
                          "tlbExport", 
                          "FUNCTION").

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

