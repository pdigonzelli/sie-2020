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

DEFINE TEMP-TABLE ttOrigenes
  RCODE-INFORMATION
  FIELD id_tambor           AS CHARACTER COLUMN-LABEL "Tambor"
  FIELD lote                AS CHARACTER COLUMN-LABEL "Lote"
  FIELD id_sucursal         AS CHARACTER COLUMN-LABEL "Cod Suc"
  FIELD sucursal            AS CHARACTER COLUMN-LABEL "Sucursal"
  FIELD id_articulo         AS CHARACTER COLUMN-LABEL "Cod Art"
  FIELD producto            AS CHARACTER COLUMN-LABEL "Producto"
  FIELD fecha               AS DATE COLUMN-LABEL "Fecha"
  FIELD kilos               AS CHARACTER COLUMN-LABEL "Kilos".


DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-24 

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
DEFINE VARIABLE h_blotesaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_blotesind AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bsobranteaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcomposicionloteaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotesaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotesind AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsobranteaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fcomposicionloteaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ftotalesorigenes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vlotesaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vlotesind AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vsobranteaceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vtaratambores AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-24 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136 BY 23.67.


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
         TITLE              = "Lotes de Aceite"
         HEIGHT             = 23.67
         WIDTH              = 136
         MAX-HEIGHT         = 35.67
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 35.67
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
ON END-ERROR OF wWin /* Lotes de Aceite */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Lotes de Aceite */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  
  RUN mailingInsumos IN hLibTam.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addDrumsToBatch wWin 
PROCEDURE addDrumsToBatch :
/*------------------------------------------------------------------------------
  Purpose:     tomo las filas seleccionadas y se las paso al sdo
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcRows   AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cEntries AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMessage AS CHARACTER  NO-UNDO.

  
  cEntries = STRING(NUM-ENTRIES(pcRows, CHR(10))).
  cMessage = "Confirma que desea agregar " + cEntries + " tambores al lote " + DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_lote') + "?".

  MESSAGE cMessage VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.

  CASE lChoice.
    WHEN TRUE THEN DO:
      RUN tieDrumsToBatch IN h_dLotesAceite (pcRows, 
                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_empresa'), 
                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_sucursal'),
                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_tipotambor'),
                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'nromov'),
                                             TRUE).
      RUN refreshData IN h_fComposicionLoteAceite.
      DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
      RUN refreshRow IN h_dLotesAceite.
    END.
  END CASE.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
             INPUT  'dlotesind.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotesindUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dlotesind ).
       RUN repositionObject IN h_dlotesind ( 6.71 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'blotesind.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotesind ).
       RUN repositionObject IN h_blotesind ( 2.43 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_blotesind ( 6.67 , 74.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dlotesaceite.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldslotes_aceite.id_empresa,id_empresa,lotes_aceite.id_sucursal,id_sucursal,lotes_aceite.id_lote,id_lote,lotes_aceite.anio,anio,lotes_aceite.nro_partida,nro_partidaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotesaceiteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dlotesaceite ).
       RUN repositionObject IN h_dlotesaceite ( 6.71 , 12.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dcomposicionloteaceite.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldscomposicion_lote_aceite.id_empresa,id_empresa,composicion_lote_aceite.id_sucursal,id_sucursal,composicion_lote_aceite.id_tipotambor,id_tipotambor,composicion_lote_aceite.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcomposicionloteaceiteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcomposicionloteaceite ).
       RUN repositionObject IN h_dcomposicionloteaceite ( 6.71 , 24.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_empresa_destino,id_empresa,tambores_industria.id_sucursal_destino,id_sucursal,tambores_industria.id_tipotambor_destino,id_tipotambor,tambores_industria.nromov_destino,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria ).
       RUN repositionObject IN h_dtamboresindustria ( 6.71 , 23.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dsobranteaceite.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldssobrante_lotes_aceite.id_empresa,id_empresa,sobrante_lotes_aceite.id_sucursal,id_sucursal,sobrante_lotes_aceite.id_tipotambor,id_tipotambor,sobrante_lotes_aceite.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedsobranteaceiteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dsobranteaceite ).
       RUN repositionObject IN h_dsobranteaceite ( 6.71 , 35.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_empresa,id_empresa,tambores_industria.id_sucursal,id_sucursal,tambores_industria.id_tipotambor,id_tipotambor,tambores_industria.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposyesServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria-2 ).
       RUN repositionObject IN h_dtamboresindustria-2 ( 6.71 , 46.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda1SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsFilter,PrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 74.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_lote,anio,id_sucursal,descripcion,FechaOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFechaRANGEFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsid_sucursal0Fecha0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 6.71 , 59.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Cabecera|Lotes Aceite|Origen Tambores|Sobrantes' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 9.33 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 15.24 , 135.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('4,2') NO-ERROR.

       /* Links to SmartDataObject h_dlotesind. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dlotesind ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dlotesind ).

       /* Links to SmartDataBrowser h_blotesind. */
       RUN addLink ( h_dlotesind , 'Data':U , h_blotesind ).

       /* Links to SmartDataObject h_dlotesaceite. */
       RUN addLink ( h_dlotesind , 'Data':U , h_dlotesaceite ).

       /* Links to SmartDataObject h_dcomposicionloteaceite. */
       RUN addLink ( h_dlotesaceite , 'Data':U , h_dcomposicionloteaceite ).

       /* Links to SmartDataObject h_dtamboresindustria. */
       RUN addLink ( h_dlotesaceite , 'Data':U , h_dtamboresindustria ).

       /* Links to SmartDataObject h_dsobranteaceite. */
       RUN addLink ( h_dlotesaceite , 'Data':U , h_dsobranteaceite ).
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_dsobranteaceite ).

       /* Links to SmartDataObject h_dtamboresindustria-2. */
       RUN addLink ( h_dlotesaceite , 'Data':U , h_dtamboresindustria-2 ).
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_dtamboresindustria-2 ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vlotesind.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vlotesind ).
       RUN repositionObject IN h_vlotesind ( 11.48 , 27.00 ) NO-ERROR.
       /* Size in AB:  ( 12.10 , 84.20 ) */

       /* Links to SmartDataViewer h_vlotesind. */
       RUN addLink ( h_dlotesind , 'Data':U , h_vlotesind ).
       RUN addLink ( h_vlotesind , 'Update':U , h_dlotesind ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vlotesind ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria-2 ).
       RUN repositionObject IN h_btamboresindustria-2 ( 17.19 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria-2 ( 7.14 , 68.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vtaratambores.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vtaratambores ).
       RUN repositionObject IN h_vtaratambores ( 18.86 , 75.00 ) NO-ERROR.
       /* Size in AB:  ( 3.95 , 55.80 ) */

       RUN constructObject (
             INPUT  'blotesaceite.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotesaceite ).
       RUN repositionObject IN h_blotesaceite ( 10.52 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_blotesaceite ( 6.19 , 68.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vlotesaceite.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vlotesaceite ).
       RUN repositionObject IN h_vlotesaceite ( 12.19 , 74.00 ) NO-ERROR.
       /* Size in AB:  ( 4.38 , 58.40 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,TransactionSubModulesTableIOTypeUpdateSupportedLinksTableio-source,Commit-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsFilter,Undo,CommitHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 10.52 , 72.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 62.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsCopy,DeleteHiddenActionsFilterHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsCopy,DeleteNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 17.19 , 72.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 1.24 , 62.80 ) NO-ERROR.

       /* Links to SmartDataBrowser h_btamboresindustria-2. */
       RUN addLink ( h_dtamboresindustria-2 , 'Data':U , h_btamboresindustria-2 ).

       /* Links to SmartDataViewer h_vtaratambores. */
       RUN addLink ( h_dtamboresindustria-2 , 'Data':U , h_vtaratambores ).
       RUN addLink ( h_vtaratambores , 'Update':U , h_dtamboresindustria-2 ).
       RUN addLink ( h_dyntoolbar-3 , 'TableIo':U , h_vtaratambores ).

       /* Links to SmartDataBrowser h_blotesaceite. */
       RUN addLink ( h_dlotesaceite , 'Data':U , h_blotesaceite ).

       /* Links to SmartDataViewer h_vlotesaceite. */
       RUN addLink ( h_dlotesaceite , 'Data':U , h_vlotesaceite ).
       RUN addLink ( h_vlotesaceite , 'Update':U , h_dlotesaceite ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vlotesaceite ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria ).
       RUN repositionObject IN h_btamboresindustria ( 10.52 , 75.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria ( 12.14 , 59.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'fcomposicionloteaceite.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fcomposicionloteaceite ).
       RUN repositionObject IN h_fcomposicionloteaceite ( 10.52 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 13.62 , 73.00 ) */

       RUN constructObject (
             INPUT  'ftotalesorigenes.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_ftotalesorigenes ).
       RUN repositionObject IN h_ftotalesorigenes ( 22.91 , 75.00 ) NO-ERROR.
       /* Size in AB:  ( 1.48 , 59.00 ) */

       /* Links to SmartDataBrowser h_btamboresindustria. */
       RUN addLink ( h_dtamboresindustria , 'Data':U , h_btamboresindustria ).

       /* Links to SmartFrame h_ftotalesorigenes. */
       RUN addLink ( h_folder , 'Page':U , h_ftotalesorigenes ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.nromov,nromov_sobranteRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria-3 ).
       RUN repositionObject IN h_dtamboresindustria-3 ( 6.71 , 56.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria-3 ).
       RUN repositionObject IN h_btamboresindustria-3 ( 15.05 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria-3 ( 9.29 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bsobranteaceite.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bsobranteaceite ).
       RUN repositionObject IN h_bsobranteaceite ( 10.52 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bsobranteaceite ( 4.05 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vsobranteaceite.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vsobranteaceite ).
       RUN repositionObject IN h_vsobranteaceite ( 12.19 , 70.00 ) NO-ERROR.
       /* Size in AB:  ( 7.10 , 59.60 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda2SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsExitaction,Printaction,ExcelactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 10.52 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.24 , 66.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dtamboresindustria-3. */
       RUN addLink ( h_dsobranteaceite , 'Data':U , h_dtamboresindustria-3 ).

       /* Links to SmartDataBrowser h_btamboresindustria-3. */
       RUN addLink ( h_dtamboresindustria-3 , 'Data':U , h_btamboresindustria-3 ).

       /* Links to SmartDataBrowser h_bsobranteaceite. */
       RUN addLink ( h_dsobranteaceite , 'Data':U , h_bsobranteaceite ).

       /* Links to SmartDataViewer h_vsobranteaceite. */
       RUN addLink ( h_dsobranteaceite , 'Data':U , h_vsobranteaceite ).
       RUN addLink ( h_vsobranteaceite , 'Update':U , h_dsobranteaceite ).
       RUN addLink ( h_dyntoolbar-4 , 'TableIo':U , h_vsobranteaceite ).

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

  RUN customEvent.

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
  /*Boton de imresion de corregir tara*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "taraAction", 
                          "tlbTara", 
                          "Corregir Tara", 
                          "stock.bmp", 
                          "tlbTara", 
                          "TRANSACTION").
  
  /*Boton de imresion de etiquetas*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "prtEtiqAction", 
                          "tlbEtiquetas", 
                          "Imprimir Etiquetas Lote", 
                          "etiquetas.bmp", 
                          "tlbEtiquetas", 
                          "TRANSACTION").

  /*Boton de mail a deposito*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "mailAction", 
                          "Mail Deposito", 
                          "Mail a Deposito (informe de insumos)", 
                          "mail.gif", 
                          "tlbMailDeposito", 
                          "TRANSACTION").
  
  /*Boton de impresion orgienes de lote*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-3, 
                          "prtOrigenesAction", 
                          "tlbOrigenes", 
                          "Imprimir Origenes Lote", 
                          "excel.bmp", 
                          "tlbOrigenes", 
                          "FUNCTION").
  
  /*Boton de imresion de etiquetas de sobrante*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-4, 
                          "prtEtiqSobAction", 
                          "tlbEtiquetasSob", 
                          "Imprimir Etiquetas Sobrante de Lote", 
                          "etiquetas.bmp", 
                          "tlbEtiquetasSob", 
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

  RUN initPages("1,2,3,4").
  RUN createButtons.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customEvent wWin 
PROCEDURE customEvent :
/*------------------------------------------------------------------------------
  Purpose:     recibe el evento de cambio de fila del browser y dispara un procedure
               en el frame.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iPageNum AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cQtys    AS CHARACTER  NO-UNDO.
  
  iPageNum = DYNAMIC-FUNCTION('getCurrentPage').

  IF iPageNum = 3 THEN DO:
    RUN setParams IN h_fComposicionLoteAceite (DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_articulo'), 
                                               DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_sucursal'), 
                                               DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_tipotambor'), 
                                               DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'fecha')).
    RUN refreshData IN h_fComposicionLoteAceite.
    RUN refreshTotalesOrigenes.
  END.  


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayReferencias wWin 
PROCEDURE displayReferencias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTot AS CHARACTER  NO-UNDO.

  cTot = DYNAMIC-FUNCTION('getCantidadesOrigen' IN h_dLotesAceite).

  RUN wdReferencias.w (cTot).

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
  ENABLE RECT-24 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTempTable wWin 
PROCEDURE fillTempTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  FOR EACH ttOrigenes.
    DELETE ttOrigenes.
  END.
  
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino    = DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_empresa')
                                AND tambores_industria.id_sucursal_destino   = DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_sucursal')
                                AND tambores_industria.id_tipotambor_destino = DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_tipotambor')
                                AND tambores_industria.nromov_destino        = DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'nromov')
                              BY tambores_industria.id_lote BY tambores_industria.id_tambor.
    FIND FIRST sucursales OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.

    CREATE ttOrigenes.
    ASSIGN ttOrigenes.id_tambor   = STRING(tambores_industria.id_tambor)
           ttOrigenes.lote        = STRING(tambores_industria.id_lote) + "/" + STRING(tambores_industria.anio)
           ttOrigenes.id_sucursal = STRING(tambores_industria.id_sucursal)
           ttOrigenes.sucursal    = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NONE"
           ttOrigenes.id_articulo = STRING(tambores_industria.id_articulo)
           ttOrigenes.producto    = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
           ttOrigenes.fecha       = tambores_industria.fecha
           ttOrigenes.kilo        = STRING(tambores_industria.kilos_tambor).
    

    
  END.


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

  RUN libImpresionEtiquetas.p PERSISTENT SET hLib.
  RUN libTamboresIndustria.p  PERSISTENT SET hLibTam.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "prtEtiqAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-3, "prtOrigenesAction").  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-4, "prtEtiqSobAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "taraAction").  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "mailAction").  
  
  SUBSCRIBE TO "tlbMailDeposito" IN h_dyntoolbar-2.  
  SUBSCRIBE TO "tlbTara" IN h_dyntoolbar-2.  
  SUBSCRIBE TO "tlbEtiquetas" IN h_dyntoolbar-2.
  SUBSCRIBE TO "tlbOrigenes" IN h_dyntoolbar-3.  
  SUBSCRIBE TO "tlbEtiquetasSob" IN h_dyntoolbar-4.
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

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
  DEFINE VARIABLE iEmp  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.


  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  hLibTam   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 


  cKey = DYNAMIC-FUNCTION('getParamsFile' IN hLibTam).

  IF LENGTH(cKey) > 0 THEN DO:
    ASSIGN iEmp = INTEGER(ENTRY(1, cKey, CHR(1)))
           iSuc = INTEGER(ENTRY(2, cKey, CHR(1)))
           iTip = INTEGER(ENTRY(3, cKey, CHR(1)))
           iNro = INTEGER(ENTRY(4, cKey, CHR(1))).
  END.


  IF iNro <> 0 THEN DO:
    cCol = "id_lote,anio".
    cOps = "=,=".
    cVal = STRING(iEmp) + CHR(1) + 
           STRING(iSuc) + CHR(1).

    DYNAMIC-FUNCTION('assignQuerySelection' IN h_dLotesInd, cCol, cVal, cOps).
    DYNAMIC-FUNCTION('openQuery' IN h_dLotesInd).    
    RUN deleteParamsFile IN hLibTam.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshTotalesOrigenes wWin 
PROCEDURE refreshTotalesOrigenes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cQtys AS CHARACTER  NO-UNDO.
  
  cQtys = DYNAMIC-FUNCTION('getCantidadKilos' IN h_dTamboresIndustria).
  i = INTEGER(ENTRY(1, cQtys)).
  k = INTEGER(ENTRY(2, cQtys)).
  
  RUN displayTotales IN h_fTotalesOrigenes (i, k).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE removeDrumsFromBatch wWin 
PROCEDURE removeDrumsFromBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRows          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEntries       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMessage       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSelectedDrums AS CHARACTER  NO-UNDO.

  cRows           = DYNAMIC-FUNCTION('getSelectedRows' IN h_bTamboresIndustria).
  /*cSelectedDrums  = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_dTamboresIndustria, cRows).*/
  cSelectedDrums  = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bTamboresIndustria).
  cSelectedDrums  = REPLACE(cSelectedDrums, ",", CHR(10)).
  cEntries        = STRING(NUM-ENTRIES(cSelectedDrums, CHR(10))).
  cMessage        = "Confirma que desea quitar " + cEntries + " tambores del lote " + DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_lote') + "?".

  MESSAGE cMessage VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.

  CASE lChoice.
    WHEN TRUE THEN DO:
      RUN tieDrumsToBatch IN h_dLotesAceite (cSelectedDrums, 
                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_empresa'), 
                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_sucursal'), 
                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_tipotambor'), 
                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'nromov'), 
                                             FALSE). /*desvincula*/
      
      DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
      RUN refreshData IN h_fComposicionLoteAceite.
      RUN refreshRow IN h_dLotesAceite.
    END.
  END CASE.
  
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtiquetas wWin 
PROCEDURE tlbEtiquetas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN wdPrintEtiquetasAceite.w (DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_empresa'), 
                                DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_sucursal'),
                                DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_tipotambor'),
                                DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'nromov')).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtiquetasSob wWin 
PROCEDURE tlbEtiquetasSob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN etqOilSobrante IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dSobranteAceite, 'id_empresa'), 
                              DYNAMIC-FUNCTION('columnValue' IN h_dSobranteAceite, 'id_sucursal'),
                              DYNAMIC-FUNCTION('columnValue' IN h_dSobranteAceite, 'id_tipotambor_sobrante'),
                              DYNAMIC-FUNCTION('columnValue' IN h_dSobranteAceite, 'nromov_sobrante')).
  


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
  
  RUN mailingInsumos IN hLibTam.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbMailDeposito wWin 
PROCEDURE tlbMailDeposito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN mailingInsumos IN hLib (DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_empresa'), 
                              DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_sucursal'), 
                              DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_tipotambor'), 
                              DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'nromov')).
  



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbOrigenes wWin 
PROCEDURE tlbOrigenes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillTempTable.
  RUN generateExcel.p (INPUT TABLE ttOrigenes,
                       INPUT " Origenes Lote " + DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_lote') + "/" + DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'anio'),
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).
  



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbTara wWin 
PROCEDURE tlbTara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wgCorregirTara.w (DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_empresa'), 
                        DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_sucursal'), 
                        DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_tipotambor'), 
                        DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'nromov'), 
                        DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'id_lote'), 
                        DYNAMIC-FUNCTION('columnValue' IN h_dLotesAceite, 'anio')). 
  
  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria-2).


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

