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
DEFINE VARIABLE hLib  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hApp  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hAsy  AS HANDLE     NO-UNDO.
DEFINE VARIABLE lFlag AS LOGICAL    NO-UNDO.

DEFINE VARIABLE chGraphPie AS COM-HANDLE     NO-UNDO.



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
&Scoped-Define ENABLED-OBJECTS chkFull BUTTON-36 radMode fi400Prod fiSuc ~
fiDesde fiHasta BUTTON-1 fiSolCargas fiSolPtoEnv fi400Cga fiEficiencia ~
fiPerdida RECT-1 RECT-37 
&Scoped-Define DISPLAYED-OBJECTS chkFull radMode fi400Prod fiSuc fiDesde ~
fiHasta fiSolCargas fiSolPtoEnv fi400Cga fiEficiencia fiPerdida 

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
DEFINE VARIABLE h_bperdidacargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bperdidaptoenv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bttcompocargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btttamboresrepro AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dperdidacargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dperdidaptoenv AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttcompocargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtttamboresrepro AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Recalcular" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-36 
     LABEL "Button 36" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi400Cga AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "kilos 400 Cgas" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi400Prod AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "kilos 400 Env" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiDesde AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiEficiencia AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Eficiencia %" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiHasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiPerdida AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Perdida %" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiSolCargas AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Solidos Cargas" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiSolPtoEnv AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Solidos Env" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiSuc AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE radMode AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "en Func Sol", 3,
"en Func Kg400", 2
     SIZE 18 BY 1.67 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 9.76.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 5.71.

DEFINE VARIABLE chkFull AS LOGICAL INITIAL no 
     LABEL "Completa" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     chkFull AT ROW 6.81 COL 68
     BUTTON-36 AT ROW 6.48 COL 100
     radMode AT ROW 22.91 COL 103 NO-LABEL
     fi400Prod AT ROW 19.33 COL 101 COLON-ALIGNED
     fiSuc AT ROW 3.14 COL 66 COLON-ALIGNED
     fiDesde AT ROW 4.33 COL 66 COLON-ALIGNED
     fiHasta AT ROW 5.52 COL 66 COLON-ALIGNED
     BUTTON-1 AT ROW 3.14 COL 105
     fiSolCargas AT ROW 15.76 COL 101 COLON-ALIGNED
     fiSolPtoEnv AT ROW 16.95 COL 101 COLON-ALIGNED
     fi400Cga AT ROW 18.14 COL 101 COLON-ALIGNED
     fiEficiencia AT ROW 20.52 COL 101 COLON-ALIGNED
     fiPerdida AT ROW 21.71 COL 101 COLON-ALIGNED
     RECT-1 AT ROW 15.29 COL 60
     RECT-37 AT ROW 2.43 COL 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.8 BY 24.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Calculo de Perdidas"
         HEIGHT             = 24.24
         WIDTH              = 121.8
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
   Custom                                                               */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 15.52
       COLUMN          = 61
       HEIGHT          = 9.29
       WIDTH           = 26
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {827E9F50-96A4-11CF-823E-000021570103} type: Graph */

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Calculo de Perdidas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Calculo de Perdidas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Recalcular */
DO:
  DEFINE VARIABLE cTotCga AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTotPto AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dEfic   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPerd   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dVolc   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dEnva AS DECIMAL    NO-UNDO.

  SELF:SENSITIVE = FALSE.

  RUN connectApp.
  lFlag = FALSE.

  IF lFlag THEN DO:
    RUN callReportePerdida IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                   (DATE(fiDesde:SCREEN-VALUE), 
                                    DATE(fiHasta:SCREEN-VALUE), 
                                    INTEGER(fiSuc:SCREEN-VALUE), 
                                    LOGICAL(chkFull:SCREEN-VALUE)).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE DO:
    RUN callReportePerdida IN hLib (DATE(fiDesde:SCREEN-VALUE), 
                                    DATE(fiHasta:SCREEN-VALUE), 
                                    INTEGER(fiSuc:SCREEN-VALUE), 
                                    LOGICAL(chkFull:SCREEN-VALUE)).

  END.
  
  RUN dataLoad IN h_dttCompoCargas.
  RUN dataLoad IN h_dttTamboresRepro.
  DYNAMIC-FUNCTION('openQuery' IN h_dPerdidaCargas).
  DYNAMIC-FUNCTION('openQuery' IN h_dPerdidaPtoEnv).

  IF lFlag THEN DO:
    hApp:DISCONNECT().
    DELETE OBJECT hApp.
    DELETE OBJECT hAsy.
  END.

  SELF:SENSITIVE = TRUE.

  RUN recalcPerdida.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-36
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-36 wWin
ON CHOOSE OF BUTTON-36 IN FRAME fMain /* Button 36 */
DO:
  RUN recalcPerdida.
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


&Scoped-define SELF-NAME radMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radMode wWin
ON VALUE-CHANGED OF radMode IN FRAME fMain
DO:
  RUN recalcPerdida.
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
             INPUT  'dperdidacargas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedperdidacargasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dperdidacargas ).
       RUN repositionObject IN h_dperdidacargas ( 2.67 , 88.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bperdidacargas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bperdidacargas ).
       RUN repositionObject IN h_bperdidacargas ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bperdidacargas ( 7.14 , 52.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dperdidaptoenv.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedperdidaptoenvUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dperdidaptoenv ).
       RUN repositionObject IN h_dperdidaptoenv ( 4.57 , 88.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bperdidaptoenv.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bperdidaptoenv ).
       RUN repositionObject IN h_bperdidaptoenv ( 15.29 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bperdidaptoenv ( 9.76 , 58.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dttcompocargas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedttcompocargasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dttcompocargas ).
       RUN repositionObject IN h_dttcompocargas ( 4.57 , 101.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bttcompocargas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bttcompocargas ).
       RUN repositionObject IN h_bttcompocargas ( 8.38 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bttcompocargas ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtttamboresrepro.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtttamboresreproUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtttamboresrepro ).
       RUN repositionObject IN h_dtttamboresrepro ( 4.57 , 111.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'btttamboresrepro.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btttamboresrepro ).
       RUN repositionObject IN h_btttamboresrepro ( 8.38 , 68.00 ) NO-ERROR.
       RUN resizeObject IN h_btttamboresrepro ( 6.67 , 53.80 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda1SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 54.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 68.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dperdidacargas. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dperdidacargas ).

       /* Links to SmartDataBrowser h_bperdidacargas. */
       RUN addLink ( h_dperdidacargas , 'Data':U , h_bperdidacargas ).

       /* Links to SmartDataBrowser h_bperdidaptoenv. */
       RUN addLink ( h_dperdidaptoenv , 'Data':U , h_bperdidaptoenv ).

       /* Links to SmartDataBrowser h_bttcompocargas. */
       RUN addLink ( h_dttcompocargas , 'Data':U , h_bttcompocargas ).

       /* Links to SmartDataBrowser h_btttamboresrepro. */
       RUN addLink ( h_dtttamboresrepro , 'Data':U , h_btttamboresrepro ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bperdidacargas ,
             fi400Prod:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_bttcompocargas ,
             h_bperdidacargas , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar ,
             h_bttcompocargas , 'AFTER':U ).
       RUN adjustTabOrder ( h_btttamboresrepro ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_bperdidaptoenv ,
             h_btttamboresrepro , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callCompleted wWin 
PROCEDURE callCompleted :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "F10" TO TARGET-PROCEDURE.


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
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  hApp = DYNAMIC-FUNCTION('connectApp' IN hLibCom).

  IF VALID-HANDLE(hApp) THEN 
    RUN libReportes.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
  ELSE 
    RUN libReportes.p PERSISTENT SET hLib .

  lFlag = VALID-HANDLE(hApp).

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

OCXFile = SEARCH( "wPerdidaClarificado.wrx":U ).
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
ELSE MESSAGE "wPerdidaClarificado.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY chkFull radMode fi400Prod fiSuc fiDesde fiHasta fiSolCargas 
          fiSolPtoEnv fi400Cga fiEficiencia fiPerdida 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE chkFull BUTTON-36 radMode fi400Prod fiSuc fiDesde fiHasta BUTTON-1 
         fiSolCargas fiSolPtoEnv fi400Cga fiEficiencia fiPerdida RECT-1 RECT-37 
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

                                                
  chGraphPie:GraphTitle     = "Eficiencia vs Perdida".
  chGraphPie:DataReset      = 9.
  chGraphPie:Labels         = 1.
  chGraphPie:GraphType      = 2.
  chGraphPie:Hot            = 1.
  chGraphPie:NumPoints      = 2.
  chGraphPie:FontUse        = 2.
  chGraphPie:FontSize       = 100.
  chGraphPie:Palette        = 4.
  chGraphPie:Tag            = "lts".


  chGraphPie:Legend(1) = "Efic".
  chGraphPie:Data(1)   = fiEficiencia:SCREEN-VALUE IN FRAME fMain.
  chGraphPie:LABEL(1)  = "Efic".
  chGraphPie:COLOR(1)  = 11.

  chGraphPie:Legend(2) = "Perd".
  chGraphPie:Data(2)   = fiPerdida:SCREEN-VALUE IN FRAME fMain.
  chGraphPie:LABEL(2)  = "Perd".
  chGraphPie:COLOR(2)  = 12.

  chGraphPie:DrawMode = 3.
  

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

  chGraphPie = chCtrlFrame:Graph.

  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcPerdida wWin 
PROCEDURE recalcPerdida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTotCga AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTotPto AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dVolc   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dEnva   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPerd   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dEfic   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iEntry  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cPer    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPerCga AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPerPto AS CHARACTER  NO-UNDO.


  RUN libTamboresIndustria.p PERSISTENT SET hLib.
    
  iEntry = INTEGER(radMode:SCREEN-VALUE IN FRAME fMain).
    
  cTotCga = DYNAMIC-FUNCTION('getRowObjectSum' IN h_dPerdidaCargas).
  cTotPto = DYNAMIC-FUNCTION('getRowObjectSum' IN h_dPerdidaPtoEnv).

  dVolc = DECIMAL(ENTRY(iEntry, cTotCga, CHR(1))).
  dEnva = DECIMAL(ENTRY(iEntry, cTotPto, CHR(1))).
  dPerd = ((dVolc - dEnva) * 100) / dVolc.
  dEfic = 100 - dPerd.

  
  fi400Cga:SCREEN-VALUE IN FRAME fMain    = ENTRY(2, cTotCga, CHR(1)).
  fi400Prod:SCREEN-VALUE IN FRAME fMain   = ENTRY(2, cTotPto, CHR(1)).

  fiSolCargas:SCREEN-VALUE IN FRAME fMain = ENTRY(3, cTotCga, CHR(1)). 
  fiSolPtoEnv:SCREEN-VALUE IN FRAME fMain = ENTRY(3, cTotPto, CHR(1)).

  fiEficiencia:SCREEN-VALUE IN FRAME fMain = STRING(dEfic).
  fiPerdida:SCREEN-VALUE IN FRAME fMain    = STRING(dPerd).
  

  RUN fillPieGraph.



END PROCEDURE.

/*
  cPer = DYNAMIC-FUNCTION('getPerdidaClarificado' IN hLib, DATE(fiDesde:SCREEN-VALUE), 
                                                           DATE(fiHasta:SCREEN-VALUE), 
                                                           INTEGER(fiSuc:SCREEN-VALUE), 
                                                           FALSE).

  cPerCga = ENTRY(1, cPer, CHR(10)).
  cPerPto = ENTRY(2, cPer, CHR(10)).
*/

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

  APPLY "CLOSE" TO THIS-PROCEDURE.
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

