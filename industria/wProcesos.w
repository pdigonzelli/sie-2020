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

DEFINE VARIABLE hLib     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibRpt  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibCom  AS HANDLE     NO-UNDO.
DEFINE VARIABLE cUser    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lControl AS LOGICAL    NO-UNDO.


DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.

DEFINE VARIABLE chGraphPie AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chGraphBar AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chTank     AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chLevelH2O AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chLevelJgo AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chLevelRep AS COM-HANDLE     NO-UNDO.

DEFINE VARIABLE viCurrPoint AS INTEGER INITIAL -1   NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS fiPortion RECT-28 RECT-29 RECT-30 RECT-31 
&Scoped-Define DISPLAYED-OBJECTS fiPortion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursalProceso wWin 
FUNCTION getSucursalProceso RETURNS INTEGER
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

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-4 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-4 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-5 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-5 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-6 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-6 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bcargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcomposicioncarga AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bproceso AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bpuntoenvase AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcomposicioncarga AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dproceso AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dpuntoenvase AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drprocesotambor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fradtipotambor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcargasanalisis AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcomposicioncarga AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vproceso AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vpuntoenvase AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiPortion AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 6.67.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 6.67.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48 BY 16.67.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35.6 BY 7.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiPortion AT ROW 24.95 COL 148.2 RIGHT-ALIGNED NO-LABEL
     RECT-28 AT ROW 2.43 COL 63
     RECT-29 AT ROW 2.43 COL 102
     RECT-30 AT ROW 9.1 COL 103
     RECT-31 AT ROW 17.91 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150.4 BY 24.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 4
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Procesos"
         HEIGHT             = 24.81
         WIDTH              = 150.4
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
/* SETTINGS FOR FILL-IN fiPortion IN FRAME fMain
   ALIGN-R                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 9.33
       COLUMN          = 104
       HEIGHT          = 15.48
       WIDTH           = 46
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 18.14
       COLUMN          = 2
       HEIGHT          = 7.38
       WIDTH           = 34
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-4 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 18.86
       COLUMN          = 10
       HEIGHT          = 4.29
       WIDTH           = 5
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-5 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 18.86
       COLUMN          = 16
       HEIGHT          = 4.29
       WIDTH           = 5
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-6 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 18.86
       COLUMN          = 22
       HEIGHT          = 4.29
       WIDTH           = 5
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {827E9F50-96A4-11CF-823E-000021570103} type: Graph */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {7974C36F-6D88-11D1-9E25-00AA002156AE} type: SFStandard */
      CtrlFrame-4:NAME = "CtrlFrame-4":U .
/* CtrlFrame-4 OCXINFO:CREATE-CONTROL from: {9F2B3505-199C-11D2-9E4E-00AA002156AE} type: SFCutaway */
      CtrlFrame-5:NAME = "CtrlFrame-5":U .
/* CtrlFrame-5 OCXINFO:CREATE-CONTROL from: {9F2B3505-199C-11D2-9E4E-00AA002156AE} type: SFCutaway */
      CtrlFrame-6:NAME = "CtrlFrame-6":U .
/* CtrlFrame-6 OCXINFO:CREATE-CONTROL from: {9F2B3505-199C-11D2-9E4E-00AA002156AE} type: SFCutaway */
      RUN adjustTabOrder ( h_folder , CtrlFrame , 'BEFORE':U ).
      CtrlFrame-3:MOVE-AFTER(CtrlFrame).
      CtrlFrame-4:MOVE-AFTER(CtrlFrame-3).
      CtrlFrame-5:MOVE-AFTER(CtrlFrame-4).
      CtrlFrame-6:MOVE-AFTER(CtrlFrame-5).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Procesos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Procesos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  RUN mailingInsumos IN hLib.

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
             INPUT  'dproceso.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedprocesoUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dproceso ).
       RUN repositionObject IN h_dproceso ( 6.48 , 139.00 ) NO-ERROR.
       /* Size in AB:  ( 2.10 , 10.80 ) */

       RUN constructObject (
             INPUT  'bproceso.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bproceso ).
       RUN repositionObject IN h_bproceso ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bproceso ( 7.86 , 61.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vproceso.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vproceso ).
       RUN repositionObject IN h_vproceso ( 2.67 , 64.00 ) NO-ERROR.
       /* Size in AB:  ( 6.19 , 36.20 ) */

       RUN constructObject (
             INPUT  'dcargas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldscargas.id_empresa_proceso,id_empresa,cargas.id_sucursal_proceso,id_sucursal,cargas.id_tipotambor_proceso,id_tipotambor,cargas.nromov_proceso,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcargasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcargas ).
       RUN repositionObject IN h_dcargas ( 8.86 , 139.00 ) NO-ERROR.
       /* Size in AB:  ( 2.14 , 10.80 ) */

       RUN constructObject (
             INPUT  'bcargas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcargas ).
       RUN repositionObject IN h_bcargas ( 9.10 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bcargas ( 8.57 , 36.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcomposicioncarga.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldscomposicion_carga.id_empresa,id_empresa,composicion_carga.id_sucursal,id_sucursal,composicion_carga.id_tipotambor,id_tipotambor,composicion_carga.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcomposicioncargaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcomposicioncarga ).
       RUN repositionObject IN h_dcomposicioncarga ( 11.24 , 139.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 11.00 ) */

       RUN constructObject (
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldstambores_industria.id_empresa_destino,id_empresa,tambores_industria.id_sucursal_destino,id_sucursal,tambores_industria.id_tipotambor_destino,id_tipotambor,tambores_industria.nromov_destino,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria ).
       RUN repositionObject IN h_dtamboresindustria ( 13.38 , 139.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'drprocesotambor.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_proceso_tambor.id_empresa_proceso,id_empresa,r_proceso_tambor.id_sucursal_proceso,id_sucursal,r_proceso_tambor.id_tipotambor_proceso,id_tipotambor,r_proceso_tambor.nromov_proceso,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrprocesotamborUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drprocesotambor ).
       RUN repositionObject IN h_drprocesotambor ( 15.52 , 139.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dpuntoenvase.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldspunto_envase.id_empresa_proceso,id_empresa,punto_envase.id_sucursal_proceso,id_sucursal,punto_envase.id_tipotambor_proceso,id_tipotambor,punto_envase.nromov_proceso,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedpuntoenvaseUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dpuntoenvase ).
       RUN repositionObject IN h_dpuntoenvase ( 17.67 , 139.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 11.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda1,Banda2SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsPrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 63.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 88.40 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_sucursal,id_proceso,AnioOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth10DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsAnio0id_sucursal0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.67 , 103.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.43 , 46.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Cargas|Analisis|Compos|Tbs. Reproc|Jugo Conc.' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 9.10 , 37.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 16.67 , 66.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('3,5') NO-ERROR.

       /* Links to SmartDataObject h_dproceso. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dproceso ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dproceso ).

       /* Links to SmartDataBrowser h_bproceso. */
       RUN addLink ( h_dproceso , 'Data':U , h_bproceso ).

       /* Links to SmartDataViewer h_vproceso. */
       RUN addLink ( h_dproceso , 'Data':U , h_vproceso ).
       RUN addLink ( h_vproceso , 'Update':U , h_dproceso ).
       RUN addLink ( h_dyntoolbar , 'TableIo':U , h_vproceso ).

       /* Links to SmartDataObject h_dcargas. */
       RUN addLink ( h_dproceso , 'Data':U , h_dcargas ).

       /* Links to SmartDataBrowser h_bcargas. */
       RUN addLink ( h_dcargas , 'Data':U , h_bcargas ).

       /* Links to SmartDataObject h_dcomposicioncarga. */
       RUN addLink ( h_dcargas , 'Data':U , h_dcomposicioncarga ).
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_dcomposicioncarga ).

       /* Links to SmartDataObject h_dtamboresindustria. */
       RUN addLink ( h_dcargas , 'Data':U , h_dtamboresindustria ).

       /* Links to SmartDataObject h_drprocesotambor. */
       RUN addLink ( h_dproceso , 'Data':U , h_drprocesotambor ).

       /* Links to SmartDataObject h_dpuntoenvase. */
       RUN addLink ( h_dproceso , 'Data':U , h_dpuntoenvase ).
       RUN addLink ( h_dyntoolbar-6 , 'Navigation':U , h_dpuntoenvase ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vcargas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcargas ).
       RUN repositionObject IN h_vcargas ( 13.38 , 39.00 ) NO-ERROR.
       /* Size in AB:  ( 10.14 , 62.20 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,TransactionSubModulesTableIOTypeUpdateSupportedLinksTableio-source,Commit-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsExitaction,Filter,Undo,CommitHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 10.29 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 64.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vcargas. */
       RUN addLink ( h_dcargas , 'Data':U , h_vcargas ).
       RUN addLink ( h_vcargas , 'Update':U , h_dcargas ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vcargas ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vcargasanalisis.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcargasanalisis ).
       RUN repositionObject IN h_vcargasanalisis ( 13.38 , 43.00 ) NO-ERROR.
       /* Size in AB:  ( 9.91 , 54.40 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableioSubModulesTableIOTypeUpdateSupportedLinksTableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsAdd,Copy,DeleteHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsAdd,Copy,DeleteNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 10.29 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 1.24 , 64.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vcargasanalisis. */
       RUN addLink ( h_dcargas , 'Data':U , h_vcargasanalisis ).
       RUN addLink ( h_vcargasanalisis , 'Update':U , h_dcargas ).
       RUN addLink ( h_dyntoolbar-3 , 'TableIo':U , h_vcargasanalisis ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'bcomposicioncarga.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcomposicioncarga ).
       RUN repositionObject IN h_bcomposicioncarga ( 10.29 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_bcomposicioncarga ( 6.91 , 64.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vcomposicioncarga.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcomposicioncarga ).
       RUN repositionObject IN h_vcomposicioncarga ( 19.10 , 39.00 ) NO-ERROR.
       /* Size in AB:  ( 5.86 , 60.60 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 17.43 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.24 , 64.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bcomposicioncarga. */
       RUN addLink ( h_dcomposicioncarga , 'Data':U , h_bcomposicioncarga ).

       /* Links to SmartDataViewer h_vcomposicioncarga. */
       RUN addLink ( h_dcomposicioncarga , 'Data':U , h_vcomposicioncarga ).
       RUN addLink ( h_vcomposicioncarga , 'Update':U , h_dcomposicioncarga ).
       RUN addLink ( h_dyntoolbar-4 , 'TableIo':U , h_vcomposicioncarga ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria ).
       RUN repositionObject IN h_btamboresindustria ( 13.62 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria ( 11.91 , 64.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsBanda3SubModulesTableIOTypeSupportedLinksToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdate,CustomactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-5 ).
       RUN repositionObject IN h_dyntoolbar-5 ( 10.29 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-5 ( 1.24 , 64.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'fradtipotambor.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fradtipotambor ).
       RUN repositionObject IN h_fradtipotambor ( 11.95 , 38.00 ) NO-ERROR.
       /* Size in AB:  ( 1.38 , 62.80 ) */

       /* Links to SmartDataBrowser h_btamboresindustria. */
       RUN addLink ( h_dtamboresindustria , 'Data':U , h_btamboresindustria ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN constructObject (
             INPUT  'vpuntoenvase.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vpuntoenvase ).
       RUN repositionObject IN h_vpuntoenvase ( 11.71 , 39.00 ) NO-ERROR.
       /* Size in AB:  ( 9.52 , 63.00 ) */

       RUN constructObject (
             INPUT  'bpuntoenvase.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bpuntoenvase ).
       RUN repositionObject IN h_bpuntoenvase ( 21.24 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_bpuntoenvase ( 4.29 , 64.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-6 ).
       RUN repositionObject IN h_dyntoolbar-6 ( 10.29 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-6 ( 1.24 , 64.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vpuntoenvase. */
       RUN addLink ( h_dpuntoenvase , 'Data':U , h_vpuntoenvase ).
       RUN addLink ( h_vpuntoenvase , 'Update':U , h_dpuntoenvase ).
       RUN addLink ( h_dyntoolbar-6 , 'TableIo':U , h_vpuntoenvase ).

       /* Links to SmartDataBrowser h_bpuntoenvase. */
       RUN addLink ( h_dpuntoenvase , 'Data':U , h_bpuntoenvase ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 5 */

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

OCXFile = SEARCH( "wProcesos.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
    chCtrlFrame-4 = CtrlFrame-4:COM-HANDLE
    UIB_S = chCtrlFrame-4:LoadControls( OCXFile, "CtrlFrame-4":U)
    chCtrlFrame-5 = CtrlFrame-5:COM-HANDLE
    UIB_S = chCtrlFrame-5:LoadControls( OCXFile, "CtrlFrame-5":U)
    chCtrlFrame-6 = CtrlFrame-6:COM-HANDLE
    UIB_S = chCtrlFrame-6:LoadControls( OCXFile, "CtrlFrame-6":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wProcesos.wrx":U SKIP(1)
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
  RUN ToolBarButtonAdd.p (h_dyntoolbar-2, 
                          'printOCAction', 
                          'OrdenCarga', 
                          'Imprimir Orden de Carga', 
                          'excel.bmp', 
                          'tlbPrintOC', 
                          'TRANSACTION') NO-ERROR.

  /*Boton de re apertura de lote*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "reOpenProcessAction", 
                          "tlbReOpenProcess", 
                          "Reabrir Proceso (Solo Personal Autorizado)", 
                          "112_p5.bmp", 
                          "tlbReOpenProcess", 
                          "BANDA1").

  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          'closeProcessAction', 
                          'CerrarProceso', 
                          'Cerrar el Proceso', 
                          'check.bmp', 
                          'tlbCloseProcess', 
                          'BANDA1').
  
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          'printProcesoAction', 
                          'ReporteProceso', 
                          'Reporte de Proceso', 
                          'print.bmp', 
                          'tlbPrintProceso', 
                          'BANDA1').
  
  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          'addDrumsAction', 
                          'AgregarTambores', 
                          'Agregar Tambores a Carga', 
                          'commit.bmp', 
                          'tlbAddDrums', 
                          'BANDA3').

  RUN ToolBarButtonAdd.p (h_dyntoolbar-5, 
                          'removeDrumsAction', 
                          'QuitarTambores', 
                          'Quitar Tambores de Carga', 
                          'deleterec.bmp', 
                          'tlbRemoveDrums', 
                          'BANDA3').

  

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

  RUN initPages("2,3,4,5").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableProcesoCerradoControls wWin 
PROCEDURE enableProcesoCerradoControls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdFechaCierre AS DATE       NO-UNDO.

  DEFINE VARIABLE cAction  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAction2 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAction3 AS CHARACTER  NO-UNDO.

  IF pdFechaCierre <> ? THEN
    ASSIGN cAction  = "Add"     
           cAction2 = "disableActions"
           cAction3 = "enableActions"
           . 
  ELSE
    ASSIGN cAction  = "Remove"
           cAction2 = "enableActions"
           cAction3 = "disableActions"
           .
 
  DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar, cAction, 'add,copy,update,delete').
  DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar-2, cAction, 'add,copy,update,delete').
  DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar-3, cAction, 'add,copy,update,delete').
  DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar-4, cAction, 'add,copy,update,delete').
  DYNAMIC-FUNCTION('modifyDisabledActions' IN h_dyntoolbar-6, cAction, 'add,copy,update,delete').
  /*
  DYNAMIC-FUNCTION(cAction2 IN h_dyntoolbar-5, 'addDrumsAction').
  DYNAMIC-FUNCTION(cAction2 IN h_dyntoolbar-5, 'removeDrumsAction').
  */ 
  DYNAMIC-FUNCTION(cAction3 IN h_dyntoolbar, 'reOpenProcessAction').
  DYNAMIC-FUNCTION(cAction2 IN h_dyntoolbar, 'closeProcessAction').
  

  



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
  DISPLAY fiPortion 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fiPortion RECT-28 RECT-29 RECT-30 RECT-31 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillBarGraph wWin 
PROCEDURE fillBarGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cData AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRow  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dCap  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dVal  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLeg  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dDif  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRes  AS DECIMAL    NO-UNDO.


  dTot  = DECIMAL(DYNAMIC-FUNCTION('getSolidosTotales' IN h_dProceso)) .
  dRes  = DECIMAL(DYNAMIC-FUNCTION('getSolidosResultado' IN h_dProceso)).
  dDif  = dTot - dRes.

                                                
  /*chGraphBar:GraphTitle     = "Composicion Carga " + DYNAMIC-FUNCTION('columnValue' IN h_dCargas, 'id_carga').*/
  chGraphBar:DataReset      = 9.
  chGraphBar:Labels         = 1.
  chGraphBar:GraphType      = 4.
  chGraphBar:Hot            = 1.
  chGraphBar:NumPoints      = 3.
  /*chGraphBar:FontUse        = 2.
  chGraphBar:FontSize       = 200.*/
  chGraphBar:Palette        = 4.
  chGraphBar:Rotation       = "0".
  chGraphBar:Tag            = "s.s.".
                                       
  
  chGraphBar:Legend(1) = "Prod".
  chGraphBar:Data(1)   = STRING(dTot).
  chGraphBar:LABEL(1)  = "Producido".
  chGraphBar:COLOR(1)  = 3.

  chGraphBar:Legend(2) = "Enva".
  chGraphBar:Data(2)   = STRING(dRes).
  chGraphBar:LABEL(2)  = "Envasado".
  chGraphBar:COLOR(2)  = 14.

  chGraphBar:Legend(3) = "Perd".
  chGraphBar:Data(3)   = STRING(dDif).
  chGraphBar:LABEL(3)  = "Perdida".
  chGraphBar:COLOR(3)  = 12.





  chGraphBar:DrawMode = 2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillPieGraph wWin 
PROCEDURE fillPieGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cData AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dCap  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dVal  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLeg  AS CHARACTER  NO-UNDO.

  cData = DYNAMIC-FUNCTION('getDatosGrafico' IN h_dCargas).
  
  IF cData = ? OR cData = "" THEN DO:
    chGraphPie:NumPoints      = 1.
    chGraphPie:Legend(1)      = "Sin Datos".
    chGraphPie:Data(1)        = "0".
    chGraphPie:LABEL(1)       = "Sin Datos".
    chGraphPie:DrawMode       = 3.
    RETURN.
  END.
  
  dCap  = DYNAMIC-FUNCTION('getCapacidadTanque' IN h_dCargas).
                                                
  chGraphPie:GraphTitle     = "Composicion Carga " + DYNAMIC-FUNCTION('columnValue' IN h_dCargas, 'id_carga').
  chGraphPie:DataReset      = 9.
  chGraphPie:Labels         = 1.
  chGraphPie:GraphType      = 2.
  chGraphPie:Hot            = 1.
  chGraphPie:NumPoints      = NUM-ENTRIES(cData, CHR(10)).
  chGraphPie:FontUse        = 2.
  chGraphPie:FontSize       = 100.
  chGraphPie:Palette        = 4.
  chGraphPie:Tag            = "lts".

  DO i = 1 TO NUM-ENTRIES(cData, CHR(10)):
    cRow = ENTRY(i, cData, CHR(10)).

    dVal = ROUND(DECIMAL(ENTRY(2, cRow, CHR(1))), 2).
    cLeg = STRING(INTEGER(ENTRY(3, cRow, CHR(1)))).
    cLeg = cLeg  + " %". 

    chGraphPie:Legend(i) = ENTRY(1, cRow, CHR(1)).
    chGraphPie:Data(i)   = STRING(dVal, ">>>,>>>,>>9.99").
    chGraphPie:LABEL(i)  = cLeg.
    chGraphPie:COLOR(i)  = ENTRY(4, cRow, CHR(1)).
  END.
  

  chGraphPie:DrawMode = 3.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillProcesoGraph wWin 
PROCEDURE fillProcesoGraph :
/*------------------------------------------------------------------------------
  Purpose:     /*grafica la torta del proceso y luego el grafico de barras*/
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cData AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dCap  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dVal  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLeg  AS CHARACTER  NO-UNDO.

  cData = DYNAMIC-FUNCTION('getDatosGrafico' IN h_dProceso).

  IF cData = "" OR cData = ?THEN DO:
    RETURN.
  END.
  
  chGraphPie:DrawMode       = 3.                                               
  chGraphPie:GraphTitle     = "Composicion Proceso " + DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_proceso').
  chGraphPie:DataReset      = 9.
  chGraphPie:Labels         = 1.
  chGraphPie:GraphType      = 2.
  chGraphPie:Hot            = 1.
  chGraphPie:NumPoints      = NUM-ENTRIES(cData, CHR(10)).
  chGraphPie:FontUse        = 2.
  chGraphPie:FontSize       = 100.
  chGraphPie:Palette        = 4.

  DO i = 1 TO NUM-ENTRIES(cData, CHR(10)):
    cRow = ENTRY(i, cData, CHR(10)).

    dVal = ROUND(DECIMAL(ENTRY(3, cRow, CHR(1))), 2).
    cLeg = STRING(INTEGER(ENTRY(4, cRow, CHR(1)))).
    cLeg = cLeg  + " %". 

    chGraphPie:Legend(i) = ENTRY(1, cRow, CHR(1)).
    chGraphPie:Data(i)   = STRING(dVal).
    chGraphPie:LABEL(i)  = cLeg.
  END.
  chGraphPie:Tag = "Sol.Sble.".

  chGraphPie:DrawMode = 3.

  /*RUN fillBarGraph.*/
    
  RUN enableProcesoCerradoControls (DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'fecha_fin')).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTankGraph wWin 
PROCEDURE fillTankGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cData AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dCap  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dVal  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cLeg  AS CHARACTER  NO-UNDO.

  RUN fillPieGraph.

  chLevelH2O:LevelMin = 0.
  chLevelJgo:LevelMin = 0.
  chLevelRep:LevelMin = 0.
  chLevelH2O:LevelMax = DYNAMIC-FUNCTION('getCapacidadTanque' IN h_dCargas).
  chLevelJgo:LevelMax = chLevelH2O:LevelMax.
  chLevelRep:LevelMax = chLevelH2O:LevelMax.

  cData = DYNAMIC-FUNCTION('getDatosGrafico' IN h_dCargas).
  
  IF cData = ? OR cData = "" THEN DO:
    RETURN.
  END.

  dVal             = 0.
  chLevelH2O:Level = 0.
  chLevelJgo:Level = 0.
  chLevelRep:Level = 0.

  DO i = 1 TO NUM-ENTRIES(cData, CHR(10)):
    cRow = ENTRY(i, cData, CHR(10)).
    dVal = ROUND(DECIMAL(ENTRY(2, cRow, CHR(1))), 2).

    IF ENTRY(1, cRow, CHR(1)) MATCHES '*agua*' THEN
      chLevelH2O:Level = dVal.

    IF ENTRY(1, cRow, CHR(1)) MATCHES '*jugo*' OR ENTRY(1, cRow, CHR(1)) MATCHES '*j.c.l.*' THEN
      chLevelJgo:Level = dVal.

    IF ENTRY(1, cRow, CHR(1)) MATCHES '*repro*' THEN
      chLevelRep:Level = dVal.


    
  END.

  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genGraphBitMap wWin 
PROCEDURE genGraphBitMap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColor    AS INTEGER    NO-UNDO.

  cFileName             = "..\industria\pie.bmp".
  iColor                = chGraphPie:BACKGROUND.
  chGraphPie:BACKGROUND = 15.
  chGraphPie:ImageFile  = cFileName.
  chGraphPie:DrawMode   = 3.
  chGraphPie:DrawMode   = 6.
  chGraphPie:BACKGROUND = iColor.
  chGraphPie:DrawMode   = 3.

  cFileName             = "..\industria\bar.bmp".
  iColor                = chGraphBar:BACKGROUND.
  chGraphBar:BACKGROUND = 15.
  chGraphBar:ImageFile  = cFileName.
  chGraphBar:DrawMode   = 3.
  chGraphBar:DrawMode   = 6.
  chGraphBar:BACKGROUND = iColor.
  chGraphBar:DrawMode   = 2.

  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE graphBarClick wWin 
PROCEDURE graphBarClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piHitPoint AS INTEGER    NO-UNDO.
  
  IF viCurrPoint = piHitPoint THEN
    chGraphBar:Extra(piHitPoint) = 0.
  ELSE 
    chGraphBar:Extra(piHitPoint) = 1.

  viCurrPoint = piHitPoint.

  chGraphBar:DrawMode = 2.

  /*fiBar:SCREEN-VALUE IN FRAME fMain = chGraphBar:LABEL(piHitPoint) + ": " + STRING(chGraphBar:Data(piHitPoint), ">>>,>>>,>>9.99") + " " + chGraphBar:Tag.*/


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

  DEFINE VARIABLE cVal AS CHARACTER  NO-UNDO.
  
  IF viCurrPoint = piHitPoint THEN
    chGraphPie:Extra(piHitPoint) = 0.
  ELSE 
    chGraphPie:Extra(piHitPoint) = 1.

  viCurrPoint = piHitPoint.

  chGraphPie:DrawMode = 3.
  
  cVal = STRING(ROUND(DECIMAL(chGraphPie:Data(piHitPoint)), 2)).
  
  fiPortion:SCREEN-VALUE IN FRAME fMain = chGraphPie:Legend(piHitPoint) + ": " + cVal + " " + chGraphPie:Tag.

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
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').

  RUN libReportes.p PERSISTENT SET hLibRpt.

  chGraphPie = chCtrlFrame:Graph.
  /*chGraphBar = chCtrlFrame-2:Graph.*/
  chTank     = chCtrlFrame-3:SFStarndard.
  chLevelH2O = chCtrlFrame-4:SFCutaway.
  chLevelJgo = chCtrlFrame-5:SFCutaway.
  chLevelRep = chCtrlFrame-6:SFCutaway.

  
  DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "reOpenProcessAction").  
  SUBSCRIBE TO "tlbReOpenProcess" IN h_dyntoolbar.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "closeProcessAction").  
  SUBSCRIBE TO "tlbCloseProcess" IN h_dyntoolbar.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printProcesoAction").  
  SUBSCRIBE TO "tlbPrintProceso" IN h_dyntoolbar.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "excelAction").  
  SUBSCRIBE TO "tlbExcel" IN h_dyntoolbar.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "printOCAction").  
  SUBSCRIBE TO "tlbPrintOC" IN h_dyntoolbar-2.
  
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "addDrumsAction").  
  SUBSCRIBE TO "tlbAddDrums" IN h_dyntoolbar-5.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-5, "removeDrumsAction").  
  SUBSCRIBE TO "tlbRemoveDrums" IN h_dyntoolbar-5.

  DEFINE VARIABLE cUser    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lControl AS LOGICAL    NO-UNDO.


  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAddDrums wWin 
PROCEDURE tlbAddDrums :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE rCga  AS ROWID      NO-UNDO.
  DEFINE VARIABLE iTipo AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cAux  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCod  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dFec  AS DATE       NO-UNDO.


  DEFINE VARIABLE hWin  AS WIDGET-HANDLE     NO-UNDO.
  DEFINE VARIABLE hMnu  AS WIDGET-HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libApiMenu.p').
  hMnu = DYNAMIC-FUNCTION('getParentWindowHandle' IN hLib).
  DELETE OBJECT hLibCom.

  /*control fecha cierre*/
  dFec = DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'fecha_fin').
  IF dFec <> ? THEN DO:

    cAux = DYNAMIC-FUNCTION('getControlAjuste' IN h_dProceso).
    RUN wdNroAjuste.w ('', OUTPUT cCod).
    IF cCod <> cAux THEN DO:
      MESSAGE "El Nro de Control de Ajuste es Incorrecto, no se permitira el Ajuste"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      IF USERID('userdb') <> 'y_facundoj' THEN RETURN.
    END.
  END.
  
  
  
  ASSIGN rCga  = DYNAMIC-FUNCTION('getRowId' IN h_dCargas)
         iTipo = DYNAMIC-FUNCTION('getSelectedValue' IN h_fRadTipoTambor).
         iSuc  = DYNAMIC-FUNCTION('columnValue' IN h_dCargas, 'id_sucursal').
  
  RUN wqOrigenTambores.w PERSISTENT SET hWin (iSuc, iTipo, rCga, "carga", OUTPUT cAux).
  hWin:CURRENT-WINDOW:PARENT = hMnu:CURRENT-WINDOW.
  RUN initializeObject IN hWin.
  WAIT-FOR "CLOSE" OF hWin.
  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
  RETURN NO-APPLY.
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbCloseProcess wWin 
PROCEDURE tlbCloseProcess :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF USERID("userdb") = "y_dbrizuela" OR USERID("userdb") = "y_afernandez" THEN 
    RUN setFechaCierre IN h_dProceso  (DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_empresa'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_sucursal'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_tipotambor'), 
                                       DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'nromov'), 
                                       TRUE).
  ELSE
    MESSAGE "Ud. No esta Autorizado para Realizar esta Operacion" VIEW-AS ALERT-BOX INFO BUTTONS OK.

  RUN refreshRow IN h_dProceso.


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
  RUN exportExcelProceso IN hLibRpt (DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_empresa'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_sucursal'),
                                     DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_tipotambor'),
                                     DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'nromov')).


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
  
  RUN mailingInsumos IN hLib.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbPrintOC wWin 
PROCEDURE tlbPrintOC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN exportExcelOrdenCarga IN hLibRpt (DYNAMIC-FUNCTION('columnValue' IN h_dCargas, 'id_empresa'), 
                                        DYNAMIC-FUNCTION('columnValue' IN h_dCargas, 'id_sucursal'),
                                        DYNAMIC-FUNCTION('columnValue' IN h_dCargas, 'id_tipotambor'),
                                        DYNAMIC-FUNCTION('columnValue' IN h_dCargas, 'nromov')).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbPrintProceso wWin 
PROCEDURE tlbPrintProceso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFiltro       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE RB-MEMO-FILE  AS CHARACTER  NO-UNDO.

  RUN genGraphBitMap.
            
  RUN callReporteProceso IN hLibRpt (DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_empresa'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_sucursal'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_tipotambor'), 
                                     DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'nromov')).
  
  RUN  aderb\_prntrb2("..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                          "reporte_procesos",                    /* RB-REPORT-NAME */
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
                          "Reporte de Stock",         /* RB-WINDOW-TITLE */
                          yes,                           /* RB-DISPLAY-ERRORS */
                          yes,                           /* RB-DISPLAY-STATUS */
                          no,                              /* RB-NO-WAIT */
                          "" /* RB-OTHER-PARAMETERS */,
                          "").   



  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbRemoveDrums wWin 
PROCEDURE tlbRemoveDrums :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cSelectedDrums AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEntries       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMessage       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCod           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dFec           AS DATE       NO-UNDO.


  cSelectedDrums  = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bTamboresIndustria).
  cSelectedDrums  = REPLACE(cSelectedDrums, ",", CHR(10)).
  cEntries        = STRING(NUM-ENTRIES(cSelectedDrums, CHR(10))).
  cMessage        = "Confirma que desea quitar " + cEntries + " tambores de la carga " + DYNAMIC-FUNCTION('columnValue' IN h_dCargas, 'id_carga').

  /*control fecha cierre*/
  dFec = DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'fecha_fin').
  IF dFec <> ? THEN DO:
    cAux = DYNAMIC-FUNCTION('getControlAjuste' IN h_dProceso).
    RUN wdNroAjuste.w ('', OUTPUT cCod).
    IF cCod <> cAux THEN DO:
      MESSAGE "El Nro de Control de Ajuste es Incorrecto, no se permitira el Ajuste"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      IF USERID('userdb') <> 'y_facundoj' THEN RETURN.
    END.
  END.
    

  MESSAGE cMessage VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.

  IF lChoice THEN DO:
    RUN removeDrumsFromCarga IN h_dCargas (cSelectedDrums).
  END.

  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbReOpenProcess wWin 
PROCEDURE tlbReOpenProcess :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*get privilegios usuario*/
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  cUser    = USERID("userdb").
  lControl = DYNAMIC-FUNCTION('getValidUser' IN hLibCom, cUser, 'usuariosAperturaLote.val').

  DELETE OBJECT hLibCom.
  
  IF lControl THEN DO:
    RUN setFechaCierre IN h_dProceso (DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_empresa'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_sucursal'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_tipotambor'), 
                                      DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'nromov'), 
                                      FALSE).
    
    DYNAMIC-FUNCTION('openQuery' IN h_dProceso).

    RUN enableProcesoCerradoControls (DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'fecha_fin')).
  END.
  ELSE 
    MESSAGE "Ud. no esta Autorizado para Realizar esta Operacion!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursalProceso wWin 
FUNCTION getSucursalProceso RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

         
  RETURN DYNAMIC-FUNCTION('columnValue' IN h_dProceso, 'id_sucursal').

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
