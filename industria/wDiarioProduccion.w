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

DEFINE VARIABLE chTree  AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chImage AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chMonth AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chNode  AS COM-HANDLE     NO-UNDO.

DEFINE VARIABLE dFec AS DATE       NO-UNDO.


DEFINE VARIABLE fRepClaro   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE fRepTurbio  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE fRepClaro4  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE fRepTurbio4 AS DECIMAL    NO-UNDO.
DEFINE VARIABLE fJgoNatural AS DECIMAL    NO-UNDO.
DEFINE VARIABLE fAgua       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iRepTbsCl   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iRepTbsTb   AS INTEGER    NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS fiFecha RECT-33 RECT-34 RECT-35 
&Scoped-Define DISPLAYED-OBJECTS fiClaro fiClaro400 fiTbsClaro fiTurbio ~
fiTurbio400 fiTbsTurbio fiPulpa fiPulpa400 fiTbsPulpa fiAceite fiTbsAceite ~
fiWaterPhase fiTbsWater fiOilPhase fiTbsOil fiRepClaro fiRepClaro400 ~
fiRepTbsClaro fiRepTurbio fiRepTurbio400 fiRepTbsTurbio fiNatural fiAgua ~
fiFecha 

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
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bttubicaciontambores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttubicaciontambores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiAceite AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Aceite" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiAgua AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Agua" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiClaro AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Jugo Claro" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiClaro400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiFecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiNatural AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     LABEL "Jugo Natural" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiOilPhase AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Foldeados" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiPulpa AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Pulpa" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiPulpa400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiRepClaro AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? 
     LABEL "Jugo Claro" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiRepClaro400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiRepTbsClaro AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiRepTbsTurbio AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiRepTurbio AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Jugo Turbio" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiRepTurbio400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiTbsAceite AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiTbsClaro AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiTbsOil AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiTbsPulpa AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiTbsTurbio AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiTbsWater AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiTurbio AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Jugo Turbio" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiTurbio400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiWaterPhase AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Prod. Ac." 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 8.33.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 3.57.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 3.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiClaro AT ROW 3.38 COL 105 RIGHT-ALIGNED
     fiClaro400 AT ROW 3.38 COL 124 RIGHT-ALIGNED NO-LABEL
     fiTbsClaro AT ROW 3.38 COL 134 RIGHT-ALIGNED NO-LABEL
     fiTurbio AT ROW 4.57 COL 105 RIGHT-ALIGNED
     fiTurbio400 AT ROW 4.57 COL 124 RIGHT-ALIGNED NO-LABEL
     fiTbsTurbio AT ROW 4.57 COL 134 RIGHT-ALIGNED NO-LABEL
     fiPulpa AT ROW 5.76 COL 105 RIGHT-ALIGNED
     fiPulpa400 AT ROW 5.76 COL 124 RIGHT-ALIGNED NO-LABEL
     fiTbsPulpa AT ROW 5.76 COL 134 RIGHT-ALIGNED NO-LABEL
     fiAceite AT ROW 6.95 COL 105 RIGHT-ALIGNED
     fiTbsAceite AT ROW 6.95 COL 134 RIGHT-ALIGNED NO-LABEL
     fiWaterPhase AT ROW 8.14 COL 105 RIGHT-ALIGNED DISABLE-AUTO-ZAP 
     fiTbsWater AT ROW 8.14 COL 134 RIGHT-ALIGNED NO-LABEL
     fiOilPhase AT ROW 9.33 COL 105 RIGHT-ALIGNED
     fiTbsOil AT ROW 9.33 COL 134 RIGHT-ALIGNED NO-LABEL
     fiRepClaro AT ROW 12.19 COL 105 RIGHT-ALIGNED
     fiRepClaro400 AT ROW 12.19 COL 124 RIGHT-ALIGNED NO-LABEL
     fiRepTbsClaro AT ROW 12.19 COL 134 RIGHT-ALIGNED NO-LABEL
     fiRepTurbio AT ROW 13.38 COL 105 RIGHT-ALIGNED
     fiRepTurbio400 AT ROW 13.38 COL 124 RIGHT-ALIGNED NO-LABEL
     fiRepTbsTurbio AT ROW 13.38 COL 134 RIGHT-ALIGNED NO-LABEL
     fiNatural AT ROW 15.95 COL 105 RIGHT-ALIGNED
     fiAgua AT ROW 17.14 COL 105 RIGHT-ALIGNED
     fiFecha AT ROW 1.24 COL 77.4 COLON-ALIGNED
     RECT-33 AT ROW 2.43 COL 72
     RECT-34 AT ROW 11.05 COL 72
     RECT-35 AT ROW 14.81 COL 72
     "Kilos" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.67 COL 89
          FONT 6
     "Produccion" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 2.19 COL 73
          FONT 6
     "Tbs. Reprocesados" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 10.76 COL 73
          FONT 6
     "Lts Reprocesados" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 14.62 COL 73
          FONT 6
     "Kilos 400" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 2.67 COL 112
          FONT 6
     "Tbs" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 2.67 COL 129
          FONT 6
     "Kilos" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.48 COL 88
          FONT 6
     "Kilos 400" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 11.48 COL 111
          FONT 6
     "Tbs" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 11.48 COL 128
          FONT 6
     "Litros" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 15.29 COL 88
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 135.4 BY 25.43.


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
         TITLE              = "Parte Diario de Produccion Industria"
         HEIGHT             = 25.43
         WIDTH              = 135.4
         MAX-HEIGHT         = 32.91
         MAX-WIDTH          = 203.2
         VIRTUAL-HEIGHT     = 32.91
         VIRTUAL-WIDTH      = 203.2
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

/* SETTINGS FOR FILL-IN fiAceite IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiAgua IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiClaro IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiClaro400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiNatural IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiOilPhase IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiPulpa IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiPulpa400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiRepClaro IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiRepClaro400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiRepTbsClaro IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiRepTbsTurbio IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiRepTurbio IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiRepTurbio400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTbsAceite IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTbsClaro IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTbsOil IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTbsPulpa IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTbsTurbio IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTbsWater IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTurbio IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTurbio400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiWaterPhase IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1
       COLUMN          = 2
       HEIGHT          = 17.38
       WIDTH           = 69
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 7.19
       COLUMN          = 109
       HEIGHT          = 1.91
       WIDTH           = 8
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 18.62
       COLUMN          = 2
       HEIGHT          = 7.33
       WIDTH           = 44
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {232E456A-87C3-11D1-8BE3-0000F8754DA1} type: MonthView */
      RUN adjustTabOrder ( h_dyntoolbar , CtrlFrame , 'AFTER':U ).
      CtrlFrame-2:MOVE-AFTER(fiTbsAceite:HANDLE IN FRAME fMain).
      CtrlFrame-3:MOVE-AFTER(fiAgua:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Parte Diario de Produccion Industria */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Parte Diario de Produccion Industria */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  RUN beforeExit.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 wWin OCX.DateClick
PROCEDURE CtrlFrame-3.MonthView.DateClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    DateClicked
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-DateClicked AS DATE NO-UNDO.

dFec = p-DateClicked.

fiFecha:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dFec).

RUN fillTreeView.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acumularKilos wWin 
PROCEDURE acumularKilos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
             INPUT  'dttubicaciontambores.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedttubicaciontamboresUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dttubicaciontambores ).
       RUN repositionObject IN h_dttubicaciontambores ( 7.67 , 117.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bttubicaciontambores.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bttubicaciontambores ).
       RUN repositionObject IN h_bttubicaciontambores ( 18.62 , 47.00 ) NO-ERROR.
       RUN resizeObject IN h_bttubicaciontambores ( 7.62 , 89.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda1SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 95.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 41.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dttubicaciontambores. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dttubicaciontambores ).

       /* Links to SmartDataBrowser h_bttubicaciontambores. */
       RUN addLink ( h_dttubicaciontambores , 'Data':U , h_bttubicaciontambores ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             fiClaro:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_bttubicaciontambores ,
             fiAgua:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcTotales wWin 
PROCEDURE calcTotales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cKil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iTam AS INTEGER    NO-UNDO.


  /* claro */
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 53).
  iTam = INTEGER(ENTRY(1, cKil, CHR(1))).
  fKil = DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 532).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 534).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 535).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  ASSIGN
    fiTbsClaro:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iTam)
    fiClaro:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(fKil)
    fiClaro400:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(fKi4).

  
  /* turbio */
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 52).
  iTam = INTEGER(ENTRY(1, cKil, CHR(1))).
  fKil = DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 521).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  ASSIGN
    fiTbsTurbio:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iTam)
    fiTurbio:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(fKil)
    fiTurbio400:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(fKi4).


  /* pulpa */
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 71).
  iTam = INTEGER(ENTRY(1, cKil, CHR(1))).
  fKil = DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 571).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  ASSIGN
    fiTbsPulpa:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iTam)
    fiPulpa:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(fKil)
    fiPulpa400:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(fKi4).

  
  /* aceite */
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 51).
  iTam = INTEGER(ENTRY(1, cKil, CHR(1))).
  fKil = DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 74).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 595).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  ASSIGN
    fiTbsAceite:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iTam)
    fiAceite:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(fKil).

  /* producciones aceite */
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 512).
  iTam = INTEGER(ENTRY(1, cKil, CHR(1))).
  fKil = DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 501).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 581).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  ASSIGN
    fiTbsWater:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iTam)
    fiWaterPhase:SCREEN-VALUE IN FRAME {&FRAME-NAME}= STRING(fKil).

  /* foldeados */
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 58).
  iTam = INTEGER(ENTRY(1, cKil, CHR(1))).
  fKil = DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 59).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  cKil = DYNAMIC-FUNCTION('getKilosOfArticulo' IN h_dttUbicacionTambores, 762).
  iTam = iTam + DECIMAL(ENTRY(1, cKil, CHR(1))).
  fKil = fKil + DECIMAL(ENTRY(2, cKil, CHR(1))).
  fKi4 = fKi4 + DECIMAL(ENTRY(3, cKil, CHR(1))).
  ASSIGN
    fiTbsOil:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iTam)
    fiOilPhase:SCREEN-VALUE IN FRAME {&FRAME-NAME}= STRING(fKil).


  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcTotalesRepro wWin 
PROCEDURE calcTotalesRepro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN fiRepClaro:SCREEN-VALUE IN FRAM {&FRAME-NAME}      = STRING(fRepClaro)
         fiRepClaro400:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(fRepClaro4)
         fiRepTurbio:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(fRepTurbio)
         fiRepTurbio400:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fRepTurbio4)
         fiRepTbsClaro:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iRepTbsCl)
         fiRepTbsTurbio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iRepTbsTb)
         fiNatural:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = STRING(fJgoNatural)
         fiAgua:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = STRING(fAgua)
         .

  

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

OCXFile = SEARCH( "wDiarioProduccion.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wDiarioProduccion.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY fiClaro fiClaro400 fiTbsClaro fiTurbio fiTurbio400 fiTbsTurbio fiPulpa 
          fiPulpa400 fiTbsPulpa fiAceite fiTbsAceite fiWaterPhase fiTbsWater 
          fiOilPhase fiTbsOil fiRepClaro fiRepClaro400 fiRepTbsClaro fiRepTurbio 
          fiRepTurbio400 fiRepTbsTurbio fiNatural fiAgua fiFecha 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fiFecha RECT-33 RECT-34 RECT-35 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeCascara wWin 
PROCEDURE fillTreeCascara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER phRoot AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER piTip  AS INTEGER        NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.

  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE chNode  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE cTxt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco    AS CHARACTER  NO-UNDO.

  IF piTip = 11 THEN DO: /* lotes */
    FOR EACH lotes_cascara
        WHERE lotes_cascara.fecha = pdFec
        NO-LOCK.
  
      FOR EACH r_produccion_cascara_lote 
          WHERE r_produccion_cascara_lote.id_empresa_lote    = lotes_cascara.id_empresa
            AND r_produccion_cascara_lote.id_sucursal_lote   = lotes_cascara.id_sucursal
            AND r_produccion_cascara_lote.id_tipotambor_lote = lotes_cascara.id_tipotambor
            AND r_produccion_cascara_lote.nromov_lote        = lotes_cascara.nromov
          NO-LOCK.

        i = i + r_produccion_cascara_lote.cantidad.

      END.

      k = i * 50.

      cTxt = "Lote " + STRING(lotes_cascara.id_lote) + "/" + STRING(lotes_cascara.anio) + " por " +
             STRING(i) + " bolsas por " +
             STRING(k) + " Kgs.".
      cKey = "lotecasc" + STRING(lotes_cascara.nromov).
      cIco = "lotecascara".
            
      
      chNode = phTree:Nodes:ADD(phRoot:KEY, 4, cKey, cTxt, cIco).
      RUN addLote IN h_dttUbicacionTambores(lotes_cascara.id_empresa,
                                            lotes_cascara.id_sucursal,
                                            lotes_cascara.id_tipotambor,
                                            lotes_cascara.nromov,
                                            0).

    END.
  END.

  IF piTip = 12 THEN DO: /* producciones */
    FOR EACH produccion_cascara
        WHERE produccion_cascara.fecha = pdFec
        BREAK BY produccion_cascara.nromov.

      i = produccion_cascara.cantidad.
      k = i * 50.


      cTxt = "Prod " + STRING(produccion_cascara.id_produccion) + "/" + STRING(produccion_cascara.anio) + " por " +
             STRING(i) + " bolsas por " +
             STRING(k) + " Kgs.".
      cKey = "prodcasc" + STRING(produccion_cascara.nromov).
      cIco = "prodcascara".
            
      
      chNode = phTree:Nodes:ADD(phRoot:KEY, 4, cKey, cTxt, cIco).
      RUN addLote IN h_dttUbicacionTambores(produccion_cascara.id_empresa,
                                            produccion_cascara.id_sucursal,
                                            produccion_cascara.id_tipotambor,
                                            produccion_cascara.nromov,
                                            0).

    END.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeFruta wWin 
PROCEDURE fillTreeFruta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER phRoot AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piArt  AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  FOR EACH balanza_tickets
      WHERE balanza_tickets.id_balanza      = 3
        AND balanza_tickets.fecha_operativa = pdFec
      NO-LOCK, 
      EACH balanza_pesada 
        OF balanza_tickets
      WHERE balanza_tickets.id_materia_prima = piArt
      NO-LOCK.
    fKil = fKil + balanza_tickets.peso_neto_ticket.
  END.

  IF fKil = 0 THEN RETURN.

  cTxt = "Neto Pesado Limon: " + STRING(fKil) + " Kgs.".
  cKey = "netopesadolimon" .
  cIco = "limon".

  phTree:Nodes:ADD(phRoot, 4, cKey, cTxt, cIco).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeProc wWin 
PROCEDURE fillTreeProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER phRoot AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec  AS DATE           NO-UNDO.

  DEFINE VARIABLE hNode   AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE hRepro  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE hProc   AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cTxt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE p       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE r       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE s       AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH proceso
      WHERE proceso.fecha     = dFec
        AND proceso.fecha_fin = ?
      NO-LOCK.

    r = 0.
    s = 0.
    
    ASSIGN cTxt  = "Proceso " + STRING(proceso.id_proceso) + "/" + STRING(proceso.anio)
           cKey  = "proceso" + STRING(proceso.nromov)
           cIco  = "proc"
           hProc = phTree:Nodes:ADD(phRoot:KEY, 4, cKey, cTxt, cIco).
  
    FOR EACH cargas
        WHERE cargas.nromov_proceso = proceso.nromov
        NO-LOCK.

      ASSIGN cTxt   = "Carga Nro " + STRING(cargas.id_carga) 
             cKey   = "carga" + STRING(cargas.nromov)
             cIco   = "carga"
             hNode  = phTree:Nodes:ADD(hProc:KEY, 4, cKey, cTxt, cIco)
             hRepro = phTree:Nodes:ADD(hNode:KEY, 4, "repro" + cKey, "Reprocesado en esta carga", "repro").

      FOR EACH tambores_industria 
          WHERE tambores_industria.nromov_destino = cargas.nromov
          BREAK BY tambores_industria.nromov.

        i = i + 1.
        k = k + tambores_industria.kilos_tambor.

        IF LAST-OF(tambores_industria.nromov) THEN DO:
          FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
          FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
          
          cTxt = STRING(tambores_industria.id_lote) + "/" + STRING(tambores_industria.anio) + " (" +
                 STRING(tambores_industria.id_articulo) + ") " +
                 productos_terminados.descripcion + " " + 
                 calidades.descripcion + ", " + 
                 STRING(i) + " tambores por " + 
                 STRING(k) + " kgs.".
          cKey = "tambor" + STRING(tambores_industria.nromov) + STRING(tambores_industria.id_tambor).
          
          /* acumulador kilos */
          CASE tambores_industria.id_articulo:
            WHEN 53 THEN 
              ASSIGN fRepClaro  = fRepClaro + k
                     fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                     iRepTbsCl  = iRepTbsCl + i.
        
            WHEN 532 THEN 
              ASSIGN fRepClaro  = fRepClaro + k
                     fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                     iRepTbsCl  = iRepTbsCl + i.
        
             WHEN 524 THEN 
              ASSIGN fRepClaro  = fRepClaro + k
                     fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                     iRepTbsCl  = iRepTbsCl + i.
              
            WHEN 534 THEN 
              ASSIGN fRepClaro  = fRepClaro + k
                     fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                     iRepTbsCl  = iRepTbsCl + i.
        
            WHEN 535 THEN 
              ASSIGN fRepClaro  = fRepClaro + k
                     fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                     iRepTbsCl  = iRepTbsCl + i.
        
            WHEN 52 THEN 
              ASSIGN fRepTurbio  = fRepTurbio + k
                     fRepTurbio4 = fRepTurbio4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                     iRepTbsTb   = iRepTbsTb + i.
            
            WHEN 521 THEN 
              ASSIGN fRepTurbio  = fRepTurbio + k
                     fRepTurbio4 = fRepTurbio4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                     iRepTbsTb   = iRepTbsTb + i.
        
        
          END CASE.
          
          IF tambores_industria.id_tipotambor = 3 THEN
            cIco = "tblote".
  
          IF tambores_industria.id_tipotambor = 1 THEN 
            cIco = "tbprod".

          IF tambores_industria.id_tipotambor = 4 THEN 
            cIco = "tbSobrante".

          IF tambores_industria.id_tipotambor = 5 THEN 
            cIco = "tbArrastre".
  
          hNode = phTree:Nodes:ADD(hRepro:KEY, 4, cKey, cTxt, cIco).
  
          p = p + i.
          q = q + k.
          i = 0.
          k = 0.
        END.
      END.

      FOR EACH composicion_carga
          WHERE composicion_carga.nromov = cargas.nromov
          NO-LOCK.
        
        FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = composicion_carga.id_articulo NO-LOCK NO-ERROR.
        
        ASSIGN cTxt   = productos_terminados.descripcion + " por " + STRING(composicion_carga.cantidad) + " Lts."
               cKey   = "compo" + STRING(cargas.nromov) + STRING(composicion_carga.id_articulo)
               cIco   = "compo"
               hNode  = phTree:Nodes:ADD(hRepro:KEY, 4, cKey, cTxt, cIco)
               .
        CASE composicion_carga.id_articulo:
          WHEN 600 THEN fJgoNatural = fJgoNatural + composicion_carga.cantidad.
          WHEN 500 THEN fAgua = fAgua + composicion_carga.cantidad.
        END CASE.
      END.

      IF p <> 0 AND q <> 0  THEN
        hRepro:TEXT = hRepro:TEXT + " (" + STRING(p) + " tambores por " + STRING(q) + ") Kgs.".
      
      r = r + p.
      s = s + q.
      p = 0.
      q = 0.

    END.

    hProc:TEXT = hProc:TEXT + " (" + STRING(r) + " tambores por " + STRING(s) + " Kgs.)".

  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeProduct wWin 
PROCEDURE fillTreeProduct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER phRoot AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER piTip  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEst  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pcIco  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE p     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cTxt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hNode AS COM-HANDLE     NO-UNDO.


  FOR EACH tambores_industria 
      WHERE tambores_industria.fecha          = pdFec
        AND tambores_industria.id_articulo    = piArt
        AND tambores_industria.id_tipotambor  = piTip 
        AND (IF (piTip = 6 OR piTip = 7) THEN TRUE ELSE tambores_industria.id_estado = piEst)
      BREAK BY tambores_industria.nromov
      BY tambores_industria.anio BY tambores_industria.id_lote.
    i = i + 1.
    k = k + tambores_industria.kilos_tambor.
    IF LAST-OF(tambores_industria.nromov) THEN DO:

      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST estados_tambor WHERE estados_tambor.id_estado = tambores_industria.id_estado NO-LOCK NO-ERROR.

      cTxt = STRING(tambores_industria.id_lote) + "/" + 
             STRING(tambores_industria.anio) + " (" + 
             STRING(tambores_industria.id_articulo) + ") " + 
             productos_terminados.descripcion + " " + 
             calidades.descripcion + ", " + 
             STRING(i) + " tambores por" + 
             STRING(k, ">>>>>>>>9.99") + " kgs en estado " +
             estados_tambor.descripcion.
      cKey  = "loteproducto" + STRING(tambores_industria.nromov) + STRING(tambores_industria.id_tambor) + STRING(RANDOM(5000, 10000)).
      hNode = phTree:Nodes:ADD(phRoot:KEY, 4, cKey, cTxt, pcIco).
      hNode:TAG = STRING(k).
      /*agrego en sdo ttubicacion*/
      RUN addLote IN h_dttUbicacionTambores(tambores_industria.id_empresa,
                                            tambores_industria.id_sucursal,
                                            tambores_industria.id_tipotambor,
                                            tambores_industria.nromov,
                                            piEst).
      


      p     = p + i.
      q     = q + k.
      i     = 0.
      k     = 0.
    END.


  END.
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeRebatch wWin 
PROCEDURE fillTreeRebatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER phRoot AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pdFec  AS DATE           NO-UNDO.

  DEFINE VARIABLE hNode   AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE hRepro  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cTxt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE p       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q       AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH lotes_jugo
      WHERE lotes_jugo.fecha              = dFec
        AND lotes_jugo.fecha_finalizacion = ?
      NO-LOCK.

    FIND FIRST productos_terminados OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.

    cTxt = STRING(lotes_jugo.id_lote) + "/" + STRING(lotes_jugo.anio) + " " +
           productos_terminados.descripcion + " " + 
           calidades.descripcion.
    cKey = "lotejugo" + STRING(lotes_jugo.nromov).
    cIco = "tblote".

    hNode  = phTree:Nodes:ADD(phRoot:KEY, 4, cKey, cTxt, cIco).
    hRepro = phTree:Nodes:ADD(hNode:KEY, 4, "repro" + cKey, "Reprocesado en este Lote", "repro").
    
    FOR EACH tambores_industria 
        WHERE tambores_industria.nromov_destino = lotes_jugo.nromov
        BREAK BY tambores_industria.nromov.
      i = i + 1.
      k = k + tambores_industria.kilos_tambor.
      IF LAST-OF(tambores_industria.nromov) THEN DO:
        FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
        
        cTxt = STRING(tambores_industria.id_lote) + "/" + STRING(tambores_industria.anio) + " " +
               productos_terminados.descripcion + " " + 
               calidades.descripcion + " " + 
               STRING(i) + " tambores por " + 
               STRING(k) + " kgs.".
        cKey = "tambor" + STRING(tambores_industria.nromov) + STRING(tambores_industria.id_tambor).

        /* acumulador kilos */
        CASE tambores_industria.id_articulo:
          WHEN 53 THEN 
            ASSIGN fRepClaro  = fRepClaro + k
                   fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                   iRepTbsCl  = iRepTbsCl + i.
      
          WHEN 532 THEN 
            ASSIGN fRepClaro  = fRepClaro + k
                   fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                   iRepTbsCl  = iRepTbsCl + i.
      
          WHEN 534 THEN 
            ASSIGN fRepClaro  = fRepClaro + k
                   fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                   iRepTbsCl  = iRepTbsCl + i.
      
          WHEN 535 THEN 
            ASSIGN fRepClaro  = fRepClaro + k
                   fRepClaro4 = fRepClaro4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                   iRepTbsCl  = iRepTbsCl + i.
      
          WHEN 52 THEN 
            ASSIGN fRepTurbio  = fRepTurbio + k
                   fRepTurbio4 = fRepTurbio4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                   iRepTbsTb   = iRepTbsTb + i.
          
          WHEN 521 THEN 
            ASSIGN fRepTurbio  = fRepTurbio + k
                   fRepTurbio4 = fRepTurbio4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k)
                   iRepTbsTb   = iRepTbsTb + i.
        END CASE.

        IF tambores_industria.id_tipotambor = 1 THEN 
          cIco = "tbprod".
        IF tambores_industria.id_tipotambor = 3 THEN
          cIco = "tblote".
        IF tambores_industria.id_tipotambor = 4 THEN
          cIco = "tbSobrante".
        IF tambores_industria.id_tipotambor = 5 THEN
          cIco = "tbArrastre".

        hNode = phTree:Nodes:ADD(hRepro:KEY, 4, cKey, cTxt, cIco).

        p = p + i.
        q = q + k.
        i = 0.
        k = 0.
      END.
    END.

    

    IF p <> 0 AND q <> 0 THEN DO: 
      hRepro:TEXT = hRepro:TEXT + " (" + STRING(p) + " tambores por " + STRING(q) + "Kgs.)".
      p = 0.
      q = 0.
    END.                                                                                 


  END.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeView wWin 
PROCEDURE fillTreeView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE chRoot1 AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chRoot2 AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chRoot3 AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chRoot4 AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chRoot5 AS COM-HANDLE     NO-UNDO.

  DEFINE VARIABLE hNode AS COM-HANDLE     NO-UNDO.

  ASSIGN fRepClaro    = 0
         fRepTurbio   = 0
         fRepClaro4   = 0
         fRepTurbio4  = 0
         iRepTbsCl    = 0
         iRepTbsTb    = 0
         fJgoNatural  = 0
         fAgua        = 0
         .


  chTree:NODES:CLEAR().
  RUN clearTT IN h_dttUbicacionTambores.

  chRoot1 = chTree:Nodes:ADD(,,'raiz1', "Productos Industriales", 'root1').

  /* JUGOS */
  chRoot2 = chTree:Nodes:ADD(chRoot1:KEY, 4, 'raizjugos', "Jugos", 'jugos').

  chRoot3 = chTree:Nodes:ADD(chRoot2:KEY, 4, 'raizlotes', "Lotes", 'raizlotes').
  chNode  = chTree:Nodes:ADD(chRoot3:KEY, 4, 'raizloteclaro', "Jugo Claro", 'lote').
  RUN fillTreeProduct (chTree, chNode, 3, 53, 4, dFec, 'tblote').
  RUN fillTreeProduct (chTree, chNode, 3, 53, 8, dFec, 'tblote').
  RUN fillTreeProduct (chTree, chNode, 3, 53, 9, dFec, 'tblote').
  RUN fillTreeProduct (chTree, chNode, 3, 53, 11, dFec, 'tblote').
  RUN fillTreeProduct (chTree, chNode, 3, 53, 12, dFec, 'tblote').
  chNode  = chTree:Nodes:ADD(chRoot3:KEY, 4, 'raizloteturbio', "Jugo Turbio", 'lote').
  RUN fillTreeProduct (chTree, chNode, 3, 52, 4, dFec, 'tblote').
  RUN fillTreeProduct (chTree, chNode, 3, 52, 8, dFec, 'tblote').
  RUN fillTreeProduct (chTree, chNode, 3, 52, 9, dFec, 'tblote').
  RUN fillTreeProduct (chTree, chNode, 3, 52, 11, dFec, 'tblote').
  RUN fillTreeProduct (chTree, chNode, 3, 52, 12, dFec, 'tblote').
  chNode  = chTree:Nodes:ADD(chRoot3:KEY, 4, 'raizlotepulpa', "Pulpa", 'lote').
  RUN fillTreeProduct (chTree, chNode, 3, 71, 7, dFec, 'tblote').

  chRoot3 = chTree:Nodes:ADD(chRoot2:KEY, 4, 'raizproducciones', "Producciones", 'producciones').
  chNode  = chTree:Nodes:ADD(chRoot3:KEY, 4, 'raizprodclaro', "Jugo Claro", 'producjugo').
  RUN fillTreeProduct (chTree, chNode, 1, 532, 5, dFec, 'tbprod').
  RUN fillTreeProduct (chTree, chNode, 1, 532, 8, dFec, 'tbprod').
  RUN fillTreeProduct (chTree, chNode, 1, 532, 9, dFec, 'tbprod').
  chNode  = chTree:Nodes:ADD(chRoot3:KEY, 4, 'raizprodturbio', "Jugo Turbio", 'producjugo').
  RUN fillTreeProduct (chTree, chNode, 1, 521, 5, dFec, 'tbprod').
  RUN fillTreeProduct (chTree, chNode, 1, 521, 8, dFec, 'tbprod').
  RUN fillTreeProduct (chTree, chNode, 1, 521, 9, dFec, 'tbprod').
  chNode  = chTree:Nodes:ADD(chRoot3:KEY, 4, 'raizprodpulpa', "Pulpa", 'producjugo').
  RUN fillTreeProduct (chTree, chNode, 1, 571, 5, dFec, 'tbprod').

  chRoot3 = chTree:Nodes:ADD(chRoot2:KEY, 4, 'raizsobrantes', "Sobrantes", 'sobrantes').
  RUN fillTreeProduct (chTree, chRoot3, 4, 534, 2, dFec, 'tbSobrante').
  RUN fillTreeProduct (chTree, chRoot3, 4, 52, 2, dFec, 'tbSobrante').
  RUN fillTreeProduct (chTree, chRoot3, 4, 534, 8, dFec, 'tbSobrante').
  RUN fillTreeProduct (chTree, chRoot3, 4, 52, 8, dFec, 'tbSobrante').
  RUN fillTreeProduct (chTree, chRoot3, 4, 534, 9, dFec, 'tbSobrante').
  RUN fillTreeProduct (chTree, chRoot3, 4, 52, 9, dFec, 'tbSobrante').

  chRoot3 = chTree:Nodes:ADD(chRoot2:KEY, 4, 'raizarrastre', "Arrastres", 'arrastres').
  RUN fillTreeProduct (chTree, chRoot3, 5, 535, 2, dFec, 'tbArrastre').
  RUN fillTreeProduct (chTree, chRoot3, 5, 52, 2, dFec, 'tbArrastre').
  RUN fillTreeProduct (chTree, chRoot3, 5, 535, 8, dFec, 'tbArrastre').
  RUN fillTreeProduct (chTree, chRoot3, 5, 52, 8, dFec, 'tbArrastre').
  RUN fillTreeProduct (chTree, chRoot3, 5, 535, 9, dFec, 'tbArrastre').
  RUN fillTreeProduct (chTree, chRoot3, 5, 52, 9, dFec, 'tbArrastre').



  
  /* ACEITES */
  chRoot2 = chTree:Nodes:ADD(chRoot1:KEY, 4, 'raizaceites', "Aceites", 'cgaaceite').

  chNode  = chTree:Nodes:ADD(chRoot2:KEY, 4, 'raizloteac', "Lotes", 'aceites').
  RUN fillTreeProduct (chTree, chNode, 6, 51, 4, dFec, 'tbaceite').
  RUN fillTreeProduct (chTree, chNode, 6, 57, 4, dFec, 'tbaceite').
  RUN fillTreeProduct (chTree, chNode, 6, 74, 6, dFec, 'tbaceite').
  RUN fillTreeProduct (chTree, chNode, 6, 595, 6, dFec, 'tbaceite').

  chNode  = chTree:Nodes:ADD(chRoot2:KEY, 4, 'raizprodac', "Producciones", 'prodAceite').
  RUN fillTreeProduct (chTree, chNode, 2, 512, 6, dFec, 'tbProdAceite').
  RUN fillTreeProduct (chTree, chNode, 2, 501, 6, dFec, 'tbProdAceite').
  RUN fillTreeProduct (chTree, chNode, 2, 581, 6, dFec, 'tbProdAceite').

  chNode  = chTree:Nodes:ADD(chRoot2:KEY, 4, 'raizfoldac', "Foldeado", 'foldeados').
  RUN fillTreeProduct (chTree, chNode, 7, 58, 4, dFec, 'tbFoldeado').
  RUN fillTreeProduct (chTree, chNode, 7, 59, 6, dFec, 'tbFoldeado').
  RUN fillTreeProduct (chTree, chNode, 7, 762, 6, dFec, 'tbFoldeado').

  /* CASCARA */
  chRoot2 = chTree:Nodes:ADD(chRoot1:KEY, 4, 'raizcascara', "Cascara", 'cascara').
  RUN fillTreeCascara (chTree, chRoot2, 11, dFec).
  RUN fillTreeCascara (chTree, chRoot2, 12, dFec).


  /* EN PROCESO */
  chRoot1 = chTree:Nodes:ADD(,,'raiz2', "Procesos Abiertos", 'root1').

  /* PROCESOS */
  chRoot2 = chTree:Nodes:ADD(chRoot1:KEY, 4, 'raizprocesos', "Procesos Clarificado", 'procesos').
  RUN fillTreeProc (chTree, chRoot2, dFec).  
  

  /* REBATCHEOS */
  chRoot2 = chTree:Nodes:ADD(chRoot1:KEY, 4, 'raizrebatcheos', "Rebatcheos Clarificado", 'rebatcheos').
  RUN fillTreeRebatch (chTree, chRoot2, dFec).  
  RUN calcTotalesRepro.

  /* FRUTA */
  chRoot3 = chTree:Nodes:ADD(,,'raiz3', "Fruta", 'fruta').
  RUN fillTreeFruta(chTree, chRoot3, dFec, 1).

  DYNAMIC-FUNCTION('openQuery' IN h_dttUbicacionTambores).

  RUN calcTotales .


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

  chTree = chCtrlFrame:TreeView.
  chImage = chCtrlFrame-2:ImageList.
  chMonth = chCtrlFrame-3:MonthView.
  chTree:ImageList = chImage.


  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.

  chMonth:VALUE = STRING(TODAY).


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

  APPLY "CLOSE" TO THIS-PROCEDURE.
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

  RUN exportParte IN h_dttUbicacionTambores (dFec).

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

