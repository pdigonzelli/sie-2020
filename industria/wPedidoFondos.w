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


DEFINE TEMP-TABLE ttPedidoFondos
  RCODE-INFORMATION
  FIELD despachante   AS CHARACTER COLUMN-LABEL "Despachante"
  FIELD oe            AS CHARACTER COLUMN-LABEL "OE"
  FIELD vapor         AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD Fecha         AS CHARACTER COLUMN-LABEL "Fecha"
  FIELD cliente       AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD producto      AS CHARACTER COLUMN-LABEL "Producto"
  FIELD destino       AS CHARACTER COLUMN-LABEL "Destino"
  FIELD condicion_vta AS CHARACTER COLUMN-LABEL "Condicion Vta"
  FIELD compania      AS CHARACTER COLUMN-LABEL "Cia. Maritima"
  FIELD contenedores  AS CHARACTER COLUMN-LABEL "Tot. Contenedores"
  FIELD gasto         AS CHARACTER COLUMN-LABEL "Gasto"
  FIELD importe       AS CHARACTER COLUMN-LABEL "Importe".


DEFINE TEMP-TABLE ttReglas
    FIELD nro         AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Nro Regla"
    FIELD regla       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Regla"
    FIELD valor       AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Valor"
    FIELD antecedente AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Antecedente"
    FIELD consecuente AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Consecuente"  .

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
DEFINE VARIABLE h_bgastositemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bgastosordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dgastositemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dgastosordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcitemsordenentrega AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82 BY 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-1 AT ROW 2.29 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.2 BY 22.38.


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
         TITLE              = "Pedido de Fondos para OE"
         HEIGHT             = 22.38
         WIDTH              = 82.2
         MAX-HEIGHT         = 34.24
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.24
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
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
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
ON END-ERROR OF wWin /* Pedido de Fondos para OE */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Pedido de Fondos para OE */
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
             INPUT  'dordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dordenentrega ).
       RUN repositionObject IN h_dordenentrega ( 7.24 , 43.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_orden_entrega.id_orden_entrega,id_orden_entregaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsordenentrega ).
       RUN repositionObject IN h_ditemsordenentrega ( 7.24 , 53.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemsordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsordenentrega ).
       RUN repositionObject IN h_bitemsordenentrega ( 8.57 , 43.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsordenentrega ( 4.71 , 40.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dgastositemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsgastos_items_orden_entrega.id_orden_entrega,id_orden_entregaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedgastositemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dgastositemsordenentrega ).
       RUN repositionObject IN h_dgastositemsordenentrega ( 7.52 , 63.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 10.80 ) */

       RUN constructObject (
             INPUT  'dgastosordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsgastos_orden_entrega.id_orden_entrega,id_orden_entregaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedgastosordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dgastosordenentrega ).
       RUN repositionObject IN h_dgastosordenentrega ( 7.67 , 74.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 9.20 ) */

       RUN constructObject (
             INPUT  'bordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bordenentrega ).
       RUN repositionObject IN h_bordenentrega ( 8.57 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bordenentrega ( 4.71 , 41.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,FunctionSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdate,FilterHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 82.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_orden_entrega,fecha_embarque,semana_embarque,anioOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelssemana_embarqueSemanaFieldToolTipsFieldHelpIdsanio0fecha_embarque0id_orden_entrega0semana_embarque0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.57 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 5.57 , 80.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Datos Pedido|Gastos Item OE|Gastos OE' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 13.52 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 9.67 , 82.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dordenentrega. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dordenentrega ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dordenentrega ).

       /* Links to SmartDataObject h_ditemsordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_ditemsordenentrega ).

       /* Links to SmartDataBrowser h_bitemsordenentrega. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_bitemsordenentrega ).

       /* Links to SmartDataObject h_dgastositemsordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_dgastositemsordenentrega ).

       /* Links to SmartDataObject h_dgastosordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_dgastosordenentrega ).

       /* Links to SmartDataBrowser h_bordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_bordenentrega ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vcitemsordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcitemsordenentrega ).
       RUN repositionObject IN h_vcitemsordenentrega ( 14.57 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 8.00 , 68.20 ) */

       /* Links to SmartDataViewer h_vcitemsordenentrega. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_vcitemsordenentrega ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'bgastositemsordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bgastositemsordenentrega ).
       RUN repositionObject IN h_bgastositemsordenentrega ( 14.81 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bgastositemsordenentrega ( 8.10 , 80.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bgastositemsordenentrega. */
       RUN addLink ( h_dgastositemsordenentrega , 'Data':U , h_bgastositemsordenentrega ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'bgastosordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bgastosordenentrega ).
       RUN repositionObject IN h_bgastosordenentrega ( 14.81 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bgastosordenentrega ( 8.10 , 79.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bgastosordenentrega. */
       RUN addLink ( h_dgastosordenentrega , 'Data':U , h_bgastosordenentrega ).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects wWin 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  createButtons().

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

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "PrintAction").
  SUBSCRIBE TO "tlbPrintPedido" IN h_dyntoolbar.

/*
  DEFINE VARIABLE vcQry AS CHARACTER  NO-UNDO.

  vcQry = "id_tipo_orden_entrega = 1 OR id_tipo_orden_entrega = 2".

  DYNAMIC-FUNCTION('setQueryWhere' IN h_dOrdenEntrega, vcQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dOrdenEntrega).
  
  
*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbPrintPedido wWin 
PROCEDURE tlbPrintPedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VARIABLE vdTotal AS DECIMAL    NO-UNDO.
  DEFINE VAR viFila  AS INTEGER.
  DEFINE VAR vcFila  AS CHARACTER.
  DEFINE VAR vcRange AS CHARACTER.

  /*recuperar datos*/
  /*RUN fillTTPedidoFondos IN h_dItemsOrdenEntrega (INPUT-OUTPUT TABLE ttPedidoFondos).*/
  RUN pMotorInferencia.p (INPUT-OUTPUT TABLE ttReglas, 
                          "..\industria\reglas.rul",
                          DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_orden_entrega')).

  CREATE "Excel.Application" chExcelAplication.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\Industria\pedidoFondos.xls').
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).
  chWorkSheet:Range("A1:AM1"):Font:Bold = TRUE.

  chWorkSheet:Range("A2:AM1"):Font:size    = 8.
  chWorkSheet:Range("A2:AM6000"):Font:size = 8.

  chWorkSheet:Range("B5"):Value  = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'despachante').
  chWorkSheet:Range("B6"):Value  = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'id_orden_entrega').
  chWorkSheet:Range("B8"):Value  = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'vapor').
  chWorkSheet:Range("B9"):Value  = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'fecha').
  chWorkSheet:Range("B10"):Value = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'cliente').
  chWorkSheet:Range("B11"):Value = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'producto').
  chWorkSheet:Range("B13"):Value = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'destino').
  chWorkSheet:Range("B14"):Value = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'condvta').
  chWorkSheet:Range("B15"):Value = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'agencia').
  chWorkSheet:Range("B16"):Value = DYNAMIC-FUNCTION('columnValue' IN h_dItemsOrdenEntrega, 'contenedores').

  /*gastos*/
  viFila = 20.
  /*
  FOR EACH ttPedidoFondos WHERE INTEGER(ttPedidoFondos.importe) > 0.
    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):BorderAround(1,2,1,1).
    chWorkSheet:Range(vcRange):VALUE = ttPedidoFondos.gasto.
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):BorderAround(1,2,1,1).
    chWorkSheet:Range(vcRange):VALUE = ttPedidoFondos.importe.
    vdTotal = vdTotal + DECIMAL(ttPedidoFondos.importe).
    viFila = viFila + 1.
  END.
  */
  FOR EACH ttReglas. 
    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):BorderAround(1,2,1,1).
    chWorkSheet:Range(vcRange):VALUE = ttReglas.regla.
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):BorderAround(1,2,1,1).
    chWorkSheet:Range(vcRange):VALUE = TRIM(ttReglas.valor).
    vdTotal = vdTotal + DECIMAL(ttReglas.valor).
    viFila = viFila + 1.
  END.
  
  vcRange = "A" + STRING(viFila).
  chWorkSheet:Range(vcRange):BorderAround(1,2,1,1).
  chWorkSheet:Range(vcRange):VALUE = "TOTAL".
  vcRange = "B" + STRING(viFila).
  chWorkSheet:Range(vcRange):BorderAround(1,2,1,1).
  chWorkSheet:Range(vcRange):VALUE = vdTotal.
  
  vcRange = "A" + STRING(viFila + 3).
  chWorkSheet:Range(vcRange):VALUE = "Tipo de Cambio:".
  vcRange = "A" + STRING(viFila + 6).
  chWorkSheet:Range(vcRange):VALUE = "Autorizacion:".

  chExcelAplication:VISIBLE = TRUE.
  /***********Release Variables*****************/
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 

END PROCEDURE.




  
/*  
  chWorkSheet:Range("A1:AM1"):Font:size    = 8.
  chWorkSheet:Range("A2:AM6000"):Font:size = 8.
   
  chWorkSheet:Range("A1"):Value = "Semana".
  chWorkSheet:Range("A1:A1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("A"):ColumnWidth = 10.

  chWorkSheet:Range("B1"):Value = "Puerto Origen".
  chWorkSheet:Range("B1:B1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("B"):ColumnWidth = 10.

  chWorkSheet:Range("C1"):Value = "Companñia".
  chWorkSheet:Range("C1:C1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("C"):ColumnWidth = 10.

  chWorkSheet:Range("D1"):Value = "Region".
  chWorkSheet:Range("D1:D1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("D"):ColumnWidth = 10.

  chWorkSheet:Range("E1"):Value = "Puerto Destino".
  chWorkSheet:Range("E1:E1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("E"):ColumnWidth = 10.

  chWorkSheet:Range("F1"):Value = "Producto".
  chWorkSheet:Range("F1:F1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("F"):ColumnWidth = 10.

  chWorkSheet:Range("G1"):Value = "Vapor".
  chWorkSheet:Range("G1:G1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("G"):ColumnWidth = 10.

  chWorkSheet:Range("H1"):Value = "Fecha".
  chWorkSheet:Range("H1:H1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("H"):ColumnWidth = 10.

  chWorkSheet:Range("I1"):Value = "Cantidad".
  chWorkSheet:Range("I1:I1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("I"):ColumnWidth = 10.

  viFila = 2.
  FOR EACH ttCompetencia NO-LOCK.
    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.semana.
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.puerto_origen.
    vcRange = "C" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.compania.
    vcRange = "D" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.region.
    vcRange = "E" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.puerto_destino.
    vcRange = "F" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.producto.
    vcRange = "G" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.vapor.
    vcRange = "H" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.fecha.
    vcRange = "I" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.cantidad.

    viFila = viFila + 1.
  END.
  

  
  */

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
                          "printAction", 
                          "tlbPrintPedido", 
                          "Imprimir", 
                          "excel.bmp", 
                          "tlbPrintPedido", 
                          "FUNCTION").


  RETURN "".   /* Function return value. */

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

