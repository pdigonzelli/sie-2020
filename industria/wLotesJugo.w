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

DEFINE VARIABLE hApp    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibEtq AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibRep AS HANDLE     NO-UNDO.
DEFINE VARIABLE lFlag   AS LOGICAL    NO-UNDO.

DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
DEFINE VARIABLE iEn1 AS INTEGER    NO-UNDO.
DEFINE VARIABLE iEn2 AS INTEGER    NO-UNDO.
DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAno AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTam AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dFec AS CHARACTER  NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-23 

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

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_barrastrelote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcomposicionlote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_binspeccioneslote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_blotesjugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bsobrante AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_darrastrelote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcomposicionlote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dinspeccioneslote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotesjugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsobrante AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fcomposicionlote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_varrastrelote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcomposicionlote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vinspeccioneslote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vlotesjugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vsobrante AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vtaratamboresindustria AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-23 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148.2 BY 26.43.


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
         TITLE              = "Lotes de Jugo"
         HEIGHT             = 24.62
         WIDTH              = 148.4
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Lotes de Jugo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Lotes de Jugo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  RUN mailingInsumos IN hLibTam.

  IF VALID-HANDLE(hApp) THEN DO:
    hApp:DISCONNECT().
    DELETE OBJECT hApp.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addDrumsToBatch wWin 
PROCEDURE addDrumsToBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcSelectedDrums AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE cEntries AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMessage AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCondic  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cCodAjt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dFechaC  AS DATE       NO-UNDO.

  cEntries = STRING(NUM-ENTRIES(pcSelectedDrums, CHR(10))).
  cMessage = "Confirma que desea agregar " + cEntries + " tambores al lote " + DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_lote') + "?".

  iCondic = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_condicion_origen').
  IF iCondic = 1 THEN DO:
    MESSAGE "Imposible asignar origenes a un lote que se forma con Solo Jugo de Linea" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    RETURN.
  END.

  /* concesion para mi usuario */
  IF USERID('userdb') <> 'y_facundoj' THEN DO:

  
    /*control fecha cierre*/
    dFechaC = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'fecha_finalizacion').
    cCodAjt = DYNAMIC-FUNCTION('getControlAjuste' IN h_dLotesJugo).
  
    IF dFechaC <> ? THEN DO:
      IF cCodAjt <> '' THEN DO: /*control de nro de ajuste*/
        RUN wdNroAjuste.w ("", OUTPUT cAux).
        IF cCodAjt <> cAux THEN DO:
          MESSAGE "El Nro de Control de Ajuste Ingresado no es Correcto, no se Permitira el Ajuste." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN.
        END.
      END.
      ELSE DO:
        MESSAGE "Imposible asignar origenes a un lote que ya se cerro." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
      END.
    END.

  END.
  
  MESSAGE cMessage VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.

  CASE lChoice.
    WHEN TRUE THEN DO:
      RUN tieDrumsToBatch IN h_dLotesJugo (pcSelectedDrums, 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                           TRUE). /*vincula*/
      RUN refreshData IN h_fComposicionLote.
      DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
      RUN refreshRow IN h_dLotesJugo.
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
             INPUT  'dlotesjugo.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotesjugoUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dlotesjugo ).
       RUN repositionObject IN h_dlotesjugo ( 6.71 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 10.00 ) */

       RUN constructObject (
             INPUT  'blotesjugo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotesjugo ).
       RUN repositionObject IN h_blotesjugo ( 2.43 , 63.00 ) NO-ERROR.
       RUN resizeObject IN h_blotesjugo ( 6.67 , 86.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'darrastrelote.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsarrastre_lote.id_empresa,id_empresa,arrastre_lote.id_sucursal,id_sucursal,arrastre_lote.id_tipotambor,id_tipotambor,arrastre_lote.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedarrastreloteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_darrastrelote ).
       RUN repositionObject IN h_darrastrelote ( 6.71 , 30.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.nromov,nromov_arrastreRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria-3 ).
       RUN repositionObject IN h_dtamboresindustria-3 ( 8.14 , 30.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dcomposicionlote.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldscomposicion_lote.id_empresa,id_empresa,composicion_lote.id_sucursal,id_sucursal,composicion_lote.id_tipotambor,id_tipotambor,composicion_lote.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcomposicionloteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcomposicionlote ).
       RUN repositionObject IN h_dcomposicionlote ( 6.71 , 21.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dinspeccioneslote.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsInspecciones_lote.id_empresa,id_empresa,Inspecciones_lote.id_sucursal,id_sucursal,Inspecciones_lote.id_tipotambor,id_tipotambor,Inspecciones_lote.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedinspeccionesloteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dinspeccioneslote ).
       RUN repositionObject IN h_dinspeccioneslote ( 6.71 , 58.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_empresa_destino,id_empresa,tambores_industria.id_sucursal_destino,id_sucursal,tambores_industria.id_tipotambor_destino,id_tipotambor,tambores_industria.nromov_destino,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria ).
       RUN repositionObject IN h_dtamboresindustria ( 6.71 , 11.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_empresa,id_empresa,tambores_industria.id_sucursal,id_sucursal,tambores_industria.id_tipotambor,id_tipotambor,tambores_industria.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria-2 ).
       RUN repositionObject IN h_dtamboresindustria-2 ( 6.71 , 50.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dsobrante.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldssobrante.id_empresa,id_empresa,sobrante.id_sucursal,id_sucursal,sobrante.id_tipotambor,id_tipotambor,sobrante.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedsobranteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dsobrante ).
       RUN repositionObject IN h_dsobrante ( 6.71 , 39.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.nromov,nromov_sobranteRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria-4 ).
       RUN repositionObject IN h_dtamboresindustria-4 ( 8.14 , 39.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda1SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 63.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 86.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_lote,anio,id_sucursal,id_articulo,FechaOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFechaRANGEFieldFormatsFieldWidthsFieldLabelsid_loteLoteid_sucursalSucursalFieldToolTipsFieldHelpIdsanio0id_articulo0id_lote0id_sucursal0Fecha0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 6.71 , 58.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Lote|Origen Tambores|Tambores|Arrastre|Sobrante|Analisis' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 9.33 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 16.19 , 148.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('4,6,3,5') NO-ERROR.

       /* Links to SmartDataObject h_dlotesjugo. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dlotesjugo ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dlotesjugo ).

       /* Links to SmartDataBrowser h_blotesjugo. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_blotesjugo ).

       /* Links to SmartDataObject h_darrastrelote. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_darrastrelote ).
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_darrastrelote ).

       /* Links to SmartDataObject h_dtamboresindustria-3. */
       RUN addLink ( h_darrastrelote , 'Data':U , h_dtamboresindustria-3 ).

       /* Links to SmartDataObject h_dcomposicionlote. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_dcomposicionlote ).

       /* Links to SmartDataObject h_dinspeccioneslote. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_dinspeccioneslote ).
       RUN addLink ( h_dyntoolbar-5 , 'Navigation':U , h_dinspeccioneslote ).

       /* Links to SmartDataObject h_dtamboresindustria. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_dtamboresindustria ).

       /* Links to SmartDataObject h_dtamboresindustria-2. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_dtamboresindustria-2 ).
       RUN addLink ( h_dyntoolbar-6 , 'Navigation':U , h_dtamboresindustria-2 ).

       /* Links to SmartDataObject h_dsobrante. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_dsobrante ).
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_dsobrante ).

       /* Links to SmartDataObject h_dtamboresindustria-4. */
       RUN addLink ( h_dsobrante , 'Data':U , h_dtamboresindustria-4 ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vlotesjugo.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vlotesjugo ).
       RUN repositionObject IN h_vlotesjugo ( 10.52 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 14.10 , 145.40 ) */

       /* Links to SmartDataViewer h_vlotesjugo. */
       RUN addLink ( h_dlotesjugo , 'Data':U , h_vlotesjugo ).
       RUN addLink ( h_vlotesjugo , 'Update':U , h_dlotesjugo ).
       RUN addLink ( h_dyntoolbar , 'TableIo':U , h_vlotesjugo ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria ).
       RUN repositionObject IN h_btamboresindustria ( 10.52 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria ( 14.76 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'fcomposicionlote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fcomposicionlote ).
       RUN repositionObject IN h_fcomposicionlote ( 10.76 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 14.52 , 75.60 ) */

       /* Links to SmartDataBrowser h_btamboresindustria. */
       RUN addLink ( h_dtamboresindustria , 'Data':U , h_btamboresindustria ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'bcomposicionlote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcomposicionlote ).
       RUN repositionObject IN h_bcomposicionlote ( 10.52 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bcomposicionlote ( 6.67 , 74.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vcomposicionlote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcomposicionlote ).
       RUN repositionObject IN h_vcomposicionlote ( 11.95 , 79.00 ) NO-ERROR.
       /* Size in AB:  ( 4.14 , 67.80 ) */

       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria-2 ).
       RUN repositionObject IN h_btamboresindustria-2 ( 17.67 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria-2 ( 7.62 , 85.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vtaratamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vtaratamboresindustria ).
       RUN repositionObject IN h_vtaratamboresindustria ( 20.76 , 101.00 ) NO-ERROR.
       /* Size in AB:  ( 2.14 , 31.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,FunctionSubModulesTableIOTypeUpdateSupportedLinksTableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsFilterHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 10.52 , 78.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 69.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsAdd,Copy,DeleteHiddenActionsPrev,NextHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsAdd,Copy,DeleteNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-6 ).
       RUN repositionObject IN h_dyntoolbar-6 ( 17.67 , 89.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-6 ( 1.24 , 58.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bcomposicionlote. */
       RUN addLink ( h_dcomposicionlote , 'Data':U , h_bcomposicionlote ).

       /* Links to SmartDataViewer h_vcomposicionlote. */
       RUN addLink ( h_dcomposicionlote , 'Data':U , h_vcomposicionlote ).
       RUN addLink ( h_vcomposicionlote , 'Update':U , h_dcomposicionlote ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vcomposicionlote ).

       /* Links to SmartDataBrowser h_btamboresindustria-2. */
       RUN addLink ( h_dtamboresindustria-2 , 'Data':U , h_btamboresindustria-2 ).

       /* Links to SmartDataViewer h_vtaratamboresindustria. */
       RUN addLink ( h_dtamboresindustria-2 , 'Data':U , h_vtaratamboresindustria ).
       RUN addLink ( h_vtaratamboresindustria , 'Update':U , h_dtamboresindustria-2 ).
       RUN addLink ( h_dyntoolbar-6 , 'TableIo':U , h_vtaratamboresindustria ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria-3 ).
       RUN repositionObject IN h_btamboresindustria-3 ( 18.14 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria-3 ( 7.14 , 144.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'barrastrelote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_barrastrelote ).
       RUN repositionObject IN h_barrastrelote ( 10.52 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_barrastrelote ( 7.38 , 68.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'varrastrelote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_varrastrelote ).
       RUN repositionObject IN h_varrastrelote ( 12.19 , 74.00 ) NO-ERROR.
       /* Size in AB:  ( 5.67 , 55.40 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda2SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsExcelactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 10.52 , 74.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 1.24 , 73.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_btamboresindustria-3. */
       RUN addLink ( h_dtamboresindustria-3 , 'Data':U , h_btamboresindustria-3 ).

       /* Links to SmartDataBrowser h_barrastrelote. */
       RUN addLink ( h_darrastrelote , 'Data':U , h_barrastrelote ).

       /* Links to SmartDataViewer h_varrastrelote. */
       RUN addLink ( h_darrastrelote , 'Data':U , h_varrastrelote ).
       RUN addLink ( h_varrastrelote , 'Update':U , h_darrastrelote ).
       RUN addLink ( h_dyntoolbar-3 , 'TableIo':U , h_varrastrelote ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria-4 ).
       RUN repositionObject IN h_btamboresindustria-4 ( 19.10 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria-4 ( 6.19 , 144.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bsobrante.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bsobrante ).
       RUN repositionObject IN h_bsobrante ( 10.52 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bsobrante ( 8.33 , 67.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vsobrante.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vsobrante ).
       RUN repositionObject IN h_vsobrante ( 12.19 , 75.00 ) NO-ERROR.
       /* Size in AB:  ( 6.71 , 60.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda3SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsCustomactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 10.52 , 73.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.24 , 74.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_btamboresindustria-4. */
       RUN addLink ( h_dtamboresindustria-4 , 'Data':U , h_btamboresindustria-4 ).

       /* Links to SmartDataBrowser h_bsobrante. */
       RUN addLink ( h_dsobrante , 'Data':U , h_bsobrante ).

       /* Links to SmartDataViewer h_vsobrante. */
       RUN addLink ( h_dsobrante , 'Data':U , h_vsobrante ).
       RUN addLink ( h_vsobrante , 'Update':U , h_dsobrante ).
       RUN addLink ( h_dyntoolbar-4 , 'TableIo':U , h_vsobrante ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN constructObject (
             INPUT  'binspeccioneslote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_binspeccioneslote ).
       RUN repositionObject IN h_binspeccioneslote ( 10.52 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_binspeccioneslote ( 6.91 , 115.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vinspeccioneslote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vinspeccioneslote ).
       RUN repositionObject IN h_vinspeccioneslote ( 19.33 , 28.00 ) NO-ERROR.
       /* Size in AB:  ( 5.86 , 86.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,TransactionSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-source,Commit-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUndo,CommitHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-5 ).
       RUN repositionObject IN h_dyntoolbar-5 ( 17.67 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-5 ( 1.24 , 115.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_binspeccioneslote. */
       RUN addLink ( h_dinspeccioneslote , 'Data':U , h_binspeccioneslote ).

       /* Links to SmartDataViewer h_vinspeccioneslote. */
       RUN addLink ( h_dinspeccioneslote , 'Data':U , h_vinspeccioneslote ).
       RUN addLink ( h_vinspeccioneslote , 'Update':U , h_dinspeccioneslote ).
       RUN addLink ( h_dyntoolbar-5 , 'TableIo':U , h_vinspeccioneslote ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 6 */

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
  
  
  /*
  IF lFlag THEN DO:    
    RUN libTamboresIndustria.p  PERSISTENT SET hLibTam ON SERVER hApp TRANSACTION DISTINCT.
  END.
  ELSE DO:    
    RUN libTamboresIndustria.p  PERSISTENT SET hLibTam.
  END.
  */
  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibEtq = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libImpresionEtiquetas.p').
  hLibRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  


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
  /*Boton de re apertura de lote*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "openBatchAction", 
                          "tlbOpenBatch", 
                          "Reabrir Lote (solo personal autorizado)", 
                          "112_p5.bmp", 
                          "tlbOpenBatch", 
                          "BANDA1").

   /*Boton excel de camara*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "trazaIndustAction", 
                          "Trazabilidad Industria", 
                          "Trazabilidad Industria", 
                          "sort-u.bmp", 
                          "tlbTrazaIndust", 
                          "BANDA1").
  
  
  /*Boton de llamada a dialog de referencias*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "DialogAction", 
                          "tlbRefer", 
                          "Ver Referencias de Color", 
                          "items.gif", 
                          "tlbRefer", 
                          "BANDA1").
/*
  /*Boton de transferencia litoral citrus*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "transferAction", 
                          "tlbTransfer", 
                          "Transferir lote de Molienda Externa", 
                          "205.bmp", 
                          "tlbTransfer", 
                          "BANDA1").
*/
  /*etiquetas tambores*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "etqAperturaLoteAction", 
                          "tlbEtqApertura", 
                          "Etiquetas Tambores", 
                          "etiquetas.bmp", 
                          "tlbEtqApertura", 
                          "FUNCTION").    

  /*boton para imprimir la etiqueta de tambor*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "etqTamborAction", 
                          "tlbEtqTambor", 
                          "Imprimir Etiqueta Tambor Seleccionado", 
                          "1137.bmp", 
                          "tlbEtqTambor", 
                          "FUNCTION").    

    /*boton para enviar correo a deposito*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          "mailAction", 
                          "tlbMailDeposito", 
                          "Mail a Deposito (Salida de Insumos)", 
                          "mail.gif", 
                          "tlbMailDeposito", 
                          "FUNCTION").    


  /*Arrastre*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-3, 
                          "etqArrastreAction", 
                          "tlbEtqArrastre", 
                          "Etiquetas Arrastre", 
                          "etiquetas.bmp", 
                          "tlbEtqArrastre", 
                          "Banda2").
  
  /*Sobrante*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-4, 
                          "etqSobranteAction", 
                          "tlbEtqSobrante", 
                          "Etiquetas Sobrante", 
                          "etiquetas.bmp", 
                          "tlbEtqSobrante", 
                          "Banda3").
    
  /*
  /*Inspecciones sin Aditivos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          "etqInspeccionesAction", 
                          "tlbEtqInspecciones", 
                          "Etiquetas Sin Aditivos", 
                          "etiquetas.bmp", 
                          "tlbEtqInspecciones", 
                          "TRANSACTION").
  
  /*Inspecciones con Aditivos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          "etqInspeccionesConAditivosAction", 
                          "tlbEtqInspeccionesConAditivos", 
                          "Etiquetas Con Aditivos", 
                          "etiquetas.bmp", 
                          "tlbEtqInspeccionesConAditivos", 
                          "TRANSACTION").
  
  /*Rango sin aditivos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          "etqRangoAction", 
                          "tlbEtqRango", 
                          "Etiquetas por Rango", 
                          "etiquetas.bmp", 
                          "tlbEtqRango", 
                          "TRANSACTION").

  /*Rango con aditivos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          "etqRangoAditivosAction", 
                          "tlbEtqRangoAditivos", 
                          "Etiquetas por Rango con Aditivos", 
                          "etiquetas.bmp", 
                          "tlbEtqRangoAditivos", 
                          "TRANSACTION").

  /*Inspecciones sin nombre*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          "etqSinNombreAction", 
                          "tlbEtqSinNombre", 
                          "Etiquetas sin Nombre", 
                          "etiquetas.bmp", 
                          "tlbEtqSinNombre", 
                          "TRANSACTION").
  */
  
  /*varias etiquetas analisis*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          "etqAllEtiqAction", 
                          "tlbAllEtiquetas", 
                          "Impresion de Etiquetas", 
                          "etiquetas.bmp", 
                          "tlbAllEtiquetas", 
                          "TRANSACTION").

  /*boton de cierre de lote*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          "closeBatchAction", 
                          "closeBatch", 
                          "Cerrar Proceso", 
                          "check.bmp", 
                          "tlbCloseBatch", 
                          "TRANSACTION").
  
  
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
  
  RUN initPages("2,3,4,5,6").
  RUN createButtons.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customEvent wWin 
PROCEDURE customEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iPageNum AS INTEGER    NO-UNDO.

  
  iPageNum = DYNAMIC-FUNCTION('getCurrentPage').

  IF iPageNum = 2 THEN DO:
    RUN setParams IN h_fComposicionLote (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_articulo'), 
                                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'fecha')).
    RUN refreshData IN h_fComposicionLote.
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE datosMailDeposito wWin 
PROCEDURE datosMailDeposito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  iEmp = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa').
  iSuc = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal').
  iTip = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor').
  iNro = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov').
  cLot = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_lote').
  cAno = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'anio').
  cArt = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'articulo').
  cCal = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'calidad').
  cEnv = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'envase').
  cTam = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'tambores').
  iEn1 = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'cantidad_envases_nuevo').
  iEn2 = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'cantidad_tambores_recup').
  dFec = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'fecha').

  
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableButtonsFechaCierre wWin 
PROCEDURE enableButtonsFechaCierre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cAction AS CHARACTER  NO-UNDO.

  IF DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'fecha_finalizacion') <> ? THEN 
    cAction = "Add".     
  ELSE
    cAction = "Remove".

  DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar, cAction, 'update,delete').
  DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar-2, cAction, 'add,copy,update,delete').
  DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar-5, cAction, 'add,copy,update,delete,closeBatchAction').
  
  /*DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar-3, cAction, 'add,copy,update,delete').*/
  /*DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar-4, cAction, 'add,copy,update,delete').*/
  



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
  ENABLE RECT-23 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeButtons wWin 
PROCEDURE initializeButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "openBatchAction").  
  SUBSCRIBE TO "tlbOpenBatch" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "dialogAction").  
  SUBSCRIBE TO "tlbRefer" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "trazaIndustAction").  
  SUBSCRIBE TO "tlbTrazaIndust" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "transferAction").  
  SUBSCRIBE TO "tlbTransfer" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "etqAperturaLoteAction").  
  SUBSCRIBE TO "tlbEtqApertura" IN h_dyntoolbar-2.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "etqTamborAction").  
  SUBSCRIBE TO "tlbEtqTambor" IN h_dyntoolbar-2.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "mailAction").  
  SUBSCRIBE TO "tlbMailDeposito" IN h_dyntoolbar-2.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "dateOpenningAction").  
  SUBSCRIBE TO "tlbDateOpenning" IN h_dyntoolbar-2.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-3, "etqArrastreAction").  
  SUBSCRIBE TO "tlbEtqArrastre" IN h_dyntoolbar-3.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-4, "etqSobranteAction").  
  SUBSCRIBE TO "tlbEtqSobrante" IN h_dyntoolbar-4.



  /*
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "etqInspeccionesAction").  
  SUBSCRIBE TO "tlbEtqInspecciones" IN h_dyntoolbar-5.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "etqInspeccionesConAditivosAction").  
  SUBSCRIBE TO "tlbEtqInspeccionesConAditivos" IN h_dyntoolbar-5.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "etqRangoAction").  
  SUBSCRIBE TO "tlbEtqRango" IN h_dyntoolbar-5.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "etqRangoAditivosAction").  
  SUBSCRIBE TO "tlbEtqRangoAditivos" IN h_dyntoolbar-5.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "etqSinNombreAction").  
  SUBSCRIBE TO "tlbEtqSinNombre" IN h_dyntoolbar-5.
  */

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "etqAllEtiqAction").  
  SUBSCRIBE TO "tlbAllEtiquetas" IN h_dyntoolbar-5.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "closeBatchAction").  
  SUBSCRIBE TO "tlbCloseBatch" IN h_dyntoolbar-5.



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
  RUN initializeButtons.
  RUN readParamsFile.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadOrigenes wWin 
PROCEDURE loadOrigenes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN setParams IN h_fComposicionLote (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_articulo'),
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'),
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'fecha')).

  RUN enableButtonsFechaCierre.


   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailDeposito wWin 
PROCEDURE mailDeposito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcAction AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cUsr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBdy AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE crlf AS CHARACTER  NO-UNDO.

  crlf = CHR(13) + CHR(10).

  CASE pcAction:
    WHEN "create" THEN DO:
      RUN datosMailDeposito.    
      cSub  = "Aviso de creacion de Lote para registro de salida de insumos del lote " + cLot + "/" + cAno.
    END.
    WHEN "delete" THEN
      cSub  = "Devolucion de Consumo lote " + cLot + "/" + cAno.
  END CASE.

  IF iSuc = 95 THEN 
    cUsr = "depositof@sa-sanmiguel.com,mmuroni@sa-sanmiguel.com".

  IF iSuc = 96 THEN 
    cUsr = "jchayle@sa-sanmiguel.com".

  
  cUsr = cUsr + ",rvelez@sa-sanmiguel.com,diegomf@sa-sanmiguel.com,depositoc@sa-sanmiguel.com,rocaran@sa-sanmiguel.com,facundoj@sa-sanmiguel.com".
  
  
  cBdy      = "Lote:        " + cLot + "/" + cAno + crlf + 
              "Tipo:        " + cArt + crlf + 
              "Calidad:     " + cCal + crlf + 
              "Envase:      " + cEnv + crlf + 
              "Cantidad:    " + STRING(cTam) + "  tambores" + crlf +
              "Env. Nuevos  " + STRING(iEn1) + crlf + 
              "Env. Recup.  " + STRING(iEn2) + crlf + 
              "Fecha Creacion: " + dFec + crlf + crlf + 
              STRING(iEmp) + "," + STRING(iSuc) + "," + STRING(iTip) + "," + STRING(iNro).

  RUN ..\industria\sendMail.p("",           /* SIEMPRE TIENE QUE IR */
                              2,            /* PRIORIDAD */
                              cSub,     /* SUBJECT */
                              cBdy,        /* BODY     */
                              cUsr,         /* DEST. SEP COMAS */
                              "").          /* ARCHIVOS ATTACHED SEP POR COMAS */



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

  cKey = DYNAMIC-FUNCTION('getParamsFile' IN hLibTam).

  IF LENGTH(cKey) > 0 THEN DO:
    ASSIGN iEmp = INTEGER(ENTRY(1, cKey, CHR(1)))
           iSuc = INTEGER(ENTRY(2, cKey, CHR(1)))
           iTip = INTEGER(ENTRY(3, cKey, CHR(1)))
           iNro = INTEGER(ENTRY(4, cKey, CHR(1))).
  END.

                                                       
  IF iNro <> 0 THEN DO:
    cCol = "id_empresa,id_sucursal,id_tipotambor,nromov".
    cOps = "=,=,=,=".
    cVal = STRING(iEmp) + CHR(1) + 
           STRING(iSuc) + CHR(1) +
           STRING(iTip) + CHR(1) + 
           STRING(iNro).

    DYNAMIC-FUNCTION('assignQuerySelection' IN h_dLotesJugo, cCol, cVal, cOps).
    DYNAMIC-FUNCTION('openQuery' IN h_dLotesJugo).    
    RUN deleteParamsFile IN hLibTam.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshData wWin 
PROCEDURE refreshData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFechaFin AS DATE       NO-UNDO.
  
  RUN refreshRow IN h_dLotesJugo.
  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria-2).
  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria-3).
  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria-4).

  IF INTEGER(DYNAMIC-FUNCTION('getCurrentPage')) = 2 THEN
    RUN refreshData IN h_fComposicionLote.


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
  DEFINE VARIABLE dFechaCierre   AS DATE       NO-UNDO.

  cRows           = DYNAMIC-FUNCTION('getSelectedRows' IN h_bTamboresIndustria).
  /*cSelectedDrums  = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_dTamboresIndustria, cRows).*/
  cSelectedDrums  = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bTamboresIndustria).
  cSelectedDrums  = REPLACE(cSelectedDrums, ",", CHR(10)).
  cEntries        = STRING(NUM-ENTRIES(cSelectedDrums, CHR(10))).
  cMessage        = "Confirma que desea quitar " + cEntries + " tambores del lote " + DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_lote') + "?".

  MESSAGE cMessage VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.

  CASE lChoice.
    WHEN TRUE THEN DO:
      dFechaCierre = DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'fecha_finalizacion').
      IF dFechaCierre <> ? THEN DO:
        MESSAGE "Imposible quitar origenes de un lote que ya se cerro" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        IF USERID('userdb' ) <> 'y_facundoj' THEN RETURN.
      END.
      RUN tieDrumsToBatch IN h_dLotesJugo (cSelectedDrums, 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                           FALSE). /*desvincula*/
      
      DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
      RUN refreshData IN h_fComposicionLote.
      RUN refreshRow IN h_dLotesJugo.
    END.
  END CASE.



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

  /* Code placed here will execute AFTER standard behavior.    */
  /*
  IF piPageNum = 2 THEN DO:
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "addTamboresAction").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "removeTamboresAction").
     
    SUBSCRIBE TO "tlbAddTambores"     IN h_dyntoolbar-2.
    SUBSCRIBE TO "tlbRemoveTambores"  IN h_dyntoolbar-2.

  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAllEtiquetas wWin 
PROCEDURE tlbAllEtiquetas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  RUN wdPrintEtiqJugo.w (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbCloseBatch wWin 
PROCEDURE tlbCloseBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF USERID('userdb') = "y_dbrizuela" OR USERID('userdb') = "y_afernandez" THEN
    RUN setFechaCierreLote IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'),
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                       TODAY).
  ELSE 
    MESSAGE "Ud NO esta autorizado para realizar esta operacion" VIEW-AS ALERT-BOX INFO BUTTONS OK.

                                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbDateOpenning wWin 
PROCEDURE tlbDateOpenning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.

  
  IF DYNAMIC-FUNCTION('getValidUser' IN hLibCom, USERID("userdb"), 'usuariosAperturaLote.val') THEN DO:
    RUN setFechaAperturaLote IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                         DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'fecha')).
    DYNAMIC-FUNCTION('openQuery' IN h_dLotesJugo).
  END.
  ELSE 
    MESSAGE "Ud. no esta autorizado para realizar esta accion" VIEW-AS ALERT-BOX ERROR BUTTONS OK.


  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqApertura wWin 
PROCEDURE tlbEtqApertura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  RUN setFechaAperturaLote IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                       TODAY).
*/  
  RUN etqAperturaLote IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),   
                                  DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                  DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                  DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).
  
  RUN refreshRow IN h_dLotesJugo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqArrastre wWin 
PROCEDURE tlbEtqArrastre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN etqArrastre IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),   
                              DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                              DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                              DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqComunes wWin 
PROCEDURE tlbEtqComunes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN etqInspeccionesLote IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),   
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).

  RUN refreshRow IN h_dLotesJugo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqInspecciones wWin 
PROCEDURE tlbEtqInspecciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
  RUN setFechaCierreLote IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                     TODAY).

  RUN etqSinAditivos IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),   
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).


  RUN refreshRow IN h_dLotesJugo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqInspeccionesConAditivos wWin 
PROCEDURE tlbEtqInspeccionesConAditivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN setFechaCierreLote IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                     TODAY).

  RUN etqConAditivos IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),   
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).


  RUN refreshRow IN h_dLotesJugo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqRango wWin 
PROCEDURE tlbEtqRango :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE v1    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE v2    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE v_suc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE r     AS ROWID      NO-UNDO.
  
  RUN w_rango_etiquetas.w (output v1,
                           output v2,
                           OUTPUT v_suc).
                           
  RUN etqRangoSinAditivos IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'),  
                                      v1, 
                                      v2, 
                                      v_suc).
  
  RUN setFechaCierreLote IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                     TODAY).

  RUN refreshRow IN h_dLotesJugo.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqRangoAditivos wWin 
PROCEDURE tlbEtqRangoAditivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE v1    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE v2    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE v_suc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE r     AS ROWID      NO-UNDO.
  
  RUN w_rango_etiquetas.w (output v1,
                           output v2,
                           OUTPUT v_suc).
                           
  RUN etqRangoConAditivos IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'),   
                                      v1, 
                                      v2, 
                                      v_suc).

  RUN setFechaCierreLote IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                     TODAY).

  RUN refreshRow IN h_dLotesJugo.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqSinNombre wWin 
PROCEDURE tlbEtqSinNombre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN etqSinNombre IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),   
                               DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                               DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                               DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqSobrante wWin 
PROCEDURE tlbEtqSobrante :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN etqSobranteJugo IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),   
                                  DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                  DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                  DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqTambor wWin 
PROCEDURE tlbEtqTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cZebra AS CHARACTER  NO-UNDO.

  cZebra = "\\192.168.2.11\zebra".
  /*cZebra = "\\facundoj\ibm4400".*/

  RUN etqLabelTambor IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dTamboresIndustria-2, 'id_empresa'),   
                                 DYNAMIC-FUNCTION('columnValue' IN h_dTamboresIndustria-2, 'id_sucursal'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dTamboresIndustria-2, 'id_tipotambor'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dTamboresIndustria-2, 'nromov'),
                                 DYNAMIC-FUNCTION('columnValue' IN h_dTamboresIndustria-2, 'id_tambor'), 
                                 cZebra).
                                  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbEtqTamboresLote wWin 
PROCEDURE tlbEtqTamboresLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN etqTamboresLoteJugo IN hLibEtq (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'),   
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).


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
  /*
  RUN mailingInsumos IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                 DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov'), 
                                 "create").
  
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbOpenBatch wWin 
PROCEDURE tlbOpenBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cUser    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lControl AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE hLib     AS HANDLE     NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLib.

  cUser    = USERID("userdb").
  lControl = DYNAMIC-FUNCTION('getValidUser' IN hLib, cUser, 'usuariosAperturaLote.val').
  
  IF lControl THEN DO:
    RUN setFechaReAperturaLote IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                           DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).
    DYNAMIC-FUNCTION('openQuery' IN h_dLotesJugo).
  END.
  ELSE 
    MESSAGE "Ud. no esta autorizado para realizar esta accion" VIEW-AS ALERT-BOX ERROR BUTTONS OK.



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
  
  RUN callComposicionLoteJugo IN hLibRep (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                          DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                          DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'), 
                                          DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).


  RUN  aderb\_prntrb2("..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                       "compo_lote_jugo",                    /* RB-REPORT-NAME */
                       "",                             /* RB-DB-CONNECTION */
                       "O",                             /* RB-INCLUDE-RECORDS */
                       "",                              /* RB-FILTER */
                       "",                              /* RB-MEMO-FILE */
                       "D",                             /* RB-PRINT-DESTINATION */
                       "?",                              /* RB-PRINTER-NAME */
                       "",                              /* RB-PRINTER-PORT */
                       "",                              /* RB-OUTPUT-FILE */
                       1,                              /* RB-NUMBER-COPIES  - zero */                  
                       0,                              /* RB-BEGIN-PAGE - zero */
                       0,                              /* RB-END-PAGE - zero */
                       no,                              /* RB-TEST-PATTERN */
                       "Conformacion Lote Jugo",         /* RB-WINDOW-TITLE */
                       yes,                           /* RB-DISPLAY-ERRORS */
                       yes,                           /* RB-DISPLAY-STATUS */
                       no,                              /* RB-NO-WAIT */
                       "" /* RB-OTHER-PARAMETERS */,
                       ""
                       ).  
  



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbRefer wWin 
PROCEDURE tlbRefer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTot AS CHARACTER  NO-UNDO.

  cTot = DYNAMIC-FUNCTION('getCantidadesOrigen' IN h_dLotesJugo).

  RUN wdReferencias.w (cTot).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbRemoveTambores wWin 
PROCEDURE tlbRemoveTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbTransfer wWin 
PROCEDURE tlbTransfer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                                            
  RUN transferMoliendaExterna IN hLibTam (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_empresa'), 
                                          DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'),
                                          DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_tipotambor'),
                                          DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbTrazaIndust wWin 
PROCEDURE tlbTrazaIndust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  RUN callReporteCamara IN hLibRep (DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'id_sucursal'), 
                                    DYNAMIC-FUNCTION('columnValue' IN h_dLotesJugo, 'nromov')).
  RUN exportExcelCamara IN hLibRep.
  */
  
  RUN wTrazaIndust.w.

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

