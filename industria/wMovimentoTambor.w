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
&Scoped-Define ENABLED-OBJECTS fi-cantidad fi-desde fi-hasta RECT-10 ~
RECT-12 RECT-2 RECT-3 RECT-5 RECT-6 RECT-8 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS fi-cantidad fi-desde fi-hasta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getbrowseLoteUbicacionHandle wWin 
FUNCTION getbrowseLoteUbicacionHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteUbicacionHandle wWin 
FUNCTION getLoteUbicacionHandle RETURNS HANDLE
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
DEFINE VARIABLE h_bloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bsucursalstock AS HANDLE NO-UNDO.
DEFINE VARIABLE h_btipomovimiento AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dmovimientotambor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsucursalstock AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtipomovimiento AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vmovimientotambor AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1  NO-FOCUS FLAT-BUTTON
     LABEL "&Transferir >>" 
     SIZE 22.2 BY 1.14
     FGCOLOR 9 FONT 6.

DEFINE VARIABLE fi-cantidad AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-desde AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-hasta AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL 
     SIZE 131.4 BY 5.86.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35.4 BY 7.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15.2 BY 1.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15.6 BY 1.86.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL 
     SIZE 85.8 BY 10.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 16.8 BY 1.29.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL 
     SIZE 98.4 BY 6.71.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL 
     SIZE 55.8 BY 6.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 20.76 COL 14
     fi-cantidad AT ROW 16.24 COL 15 COLON-ALIGNED
     fi-desde AT ROW 17.43 COL 15 COLON-ALIGNED
     fi-hasta AT ROW 18.62 COL 15 COLON-ALIGNED
     RECT-10 AT ROW 9.14 COL 19.6
     RECT-12 AT ROW 15.52 COL 7
     RECT-2 AT ROW 1 COL 60
     RECT-3 AT ROW 9.1 COL 3.4
     RECT-5 AT ROW 15.29 COL 43.6
     RECT-6 AT ROW 1 COL 3.4
     RECT-8 AT ROW 2.29 COL 60
     RECT-9 AT ROW 2.29 COL 3.4
     "Origen" VIEW-AS TEXT
          SIZE 10.2 BY 1 AT ROW 1.14 COL 72.2 RIGHT-ALIGNED
          FONT 6
     "Destino" VIEW-AS TEXT
          SIZE 10.2 BY .71 AT ROW 9.67 COL 15 RIGHT-ALIGNED
          FONT 6
     "Movimiento" VIEW-AS TEXT
          SIZE 13.8 BY 1 AT ROW 1.14 COL 5.2
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203 BY 35.43.


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
         TITLE              = "Movimientos"
         HEIGHT             = 25.67
         WIDTH              = 157.8
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

/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       RECT-5:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR TEXT-LITERAL "Origen"
          SIZE 10.2 BY 1 AT ROW 1.14 COL 72.2 RIGHT-ALIGNED             */

/* SETTINGS FOR TEXT-LITERAL "Destino"
          SIZE 10.2 BY .71 AT ROW 9.67 COL 15 RIGHT-ALIGNED             */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Movimientos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Movimientos */
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
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Transferir >> */
DO:
  DEFINE VAR vcRowLoteUbicacion     AS CHARACTER    NO-UNDO.
  DEFINE VAR vcRowSucursalDestino   AS CHARACTER    NO-UNDO.
  DEFINE VAR vcRowTipoMovimiento    AS CHARACTER    NO-UNDO.
  DEFINE VAR vhLoteUbicacion        AS HANDLE       NO-UNDO.
  DEFINE VAR vhSucursalDestino      AS HANDLE       NO-UNDO.
  DEFINE VAR vhTipoMovimiento       AS HANDLE       NO-UNDO.



  vcRowLoteUbicacion    = DYNAMIC-FUNCTION ('getSelectedRows' IN h_bLoteUbicacion).
  IF NUM-ENTRIES (vcRowLoteUbicacion ) > 1 OR 
     NUM-ENTRIES (vcRowLoteUbicacion ) = 0 OR
     NUM-ENTRIES (vcRowLoteUbicacion ) = ? THEN
  DO:
      MESSAGE 'Debe seleccionar un lote ' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  vcRowSucursalDestino  = DYNAMIC-FUNCTION ('getSelectedRows' IN h_bSucursalStock).
  IF NUM-ENTRIES (vcRowSucursalDestino ) > 1 OR 
     NUM-ENTRIES (vcRowSucursalDestino ) = 0 OR
     NUM-ENTRIES (vcRowSucursalDestino ) = ? THEN
  DO:
      MESSAGE 'Debe seleccionar una sucursal destino del lote ' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  
  vcRowTipoMovimiento  = DYNAMIC-FUNCTION ('getSelectedRows' IN h_bTipoMovimiento).
  IF NUM-ENTRIES (vcRowTipoMovimiento ) > 1 OR 
     NUM-ENTRIES (vcRowTipoMovimiento ) = 0 OR
     NUM-ENTRIES (vcRowTipoMovimiento ) = ? THEN
  DO:
      MESSAGE 'Debe seleccionar una sucursal destino del lote ' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.

  vhLoteUbicacion      = DYNAMIC-FUNCTION ('getRowObject' IN h_dLoteUbicacion ).
  vhSucursalDestino    = DYNAMIC-FUNCTION ('getRowObject' IN h_dSucursalStock ).
  vhTipoMovimiento     = DYNAMIC-FUNCTION ('getRowObject' IN h_dTipoMovimiento ).
  RUN createMovimientoTambor  IN h_dMovimientoTambor (INPUT vhLoteUbicacion , INPUT vhSucursalDestino , INPUT vhTipoMOvimiento , integer(fi-cantidad:SCREEN-VALUE) , INPUT h_dLoteUbicacion ).
  IF RETURN-VALUE <> "" THEN
  DO:
      MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR .
      RETURN NO-APPLY.
  END.
  fi-cantidad:SCREEN-VALUE  = '0'.
  fi-desde:SCREEN-VALUE     = '0'.
  fi-hasta:SCREEN-VALUE     = '0'.
  RUN RefreshRow IN h_dLoteUbicacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cantidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cantidad wWin
ON VALUE-CHANGED OF fi-cantidad IN FRAME fMain /* Cantidad */
DO:

  DEFINE VAR vhRowObject        AS HANDLE   NO-UNDO.
  DEFINE VAR vhLibLoteUbicacion AS HANDLE   NO-UNDO.    
  DEFINE VAR viDesde            AS INTEGER  NO-UNDO.
  DEFINE VAR vihasta            AS INTEGER  NO-UNDO.

  IF INTEGER(SELF:SCREEN-VALUE) <> 0 THEN
      button-1:SENSITIVE = TRUE.
  ELSE
      button-1:SENSITIVE = FALSE.

  IF NUM-ENTRIES ( DYNAMIC-FUNCTION('getSelectedRows' IN h_bLoteUbicacion)) > 0 THEN
  DO:
      vhRowObject  = DYNAMIC-FUNCTION('getRowObject' IN h_dloteUbicacion).
    
      RUN getDesdeHasta IN h_dLoteUbicacion
              (  input vhRowObject:BUFFER-FIELD('id_empresa'):BUFFER-VALUE,
                 input vhRowObject:BUFFER-FIELD('id_sucursal'):BUFFER-VALUE,
                 input vhRowObject:BUFFER-FIELD('id_tipotambor'):BUFFER-VALUE,
                 INPUT vhRowObject:BUFFER-FIELD('nromov'):BUFFER-VALUE ,
                 INPUT vhRowObject:BUFFER-FIELD('id_sucursal_ubicacion'):BUFFER-VALUE ,
                 INPUT INTEGER(SELF:SCREEN-VALUE),
                 OUTPUT viDesde ,
                 OUTPUT vihasta ).
    
      fi-desde:SCREEN-VALUE = STRING(viDesde).
      fi-hasta:SCREEN-VALUE = STRING(vihasta).
  
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-desde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-desde wWin
ON VALUE-CHANGED OF fi-desde IN FRAME fMain /* Desde */
DO:
    DEFINE VAR  hRowObject AS HANDLE NO-UNDO.

    hRowObject = DYNAMIC-FUNCTION('getRowObject' IN h_dLoteUbicacion).
    fi-hasta:SCREEN-VALUE = STRING ( INTEGER(fi-desde:SCREEN-VALUE) + INTEGER(fi-hasta:SCREEN-VALUE) - 1 ).
    RUN validaDesdeHasta IN h_dLoteUbicacion (hRowObject:BUFFER-FIELD('id_empresa'):BUFFER-VALUE , 
                                    hRowObject:BUFFER-FIELD('id_sucursal'):BUFFER-VALUE ,
                                    hRowObject:BUFFER-FIELD('id_tipotambor'):BUFFER-VALUE ,
                                    hRowObject:BUFFER-FIELD('nromov'):BUFFER-VALUE ,
                                    hRowObject:BUFFER-FIELD('id_sucursal_ubicacion'):BUFFER-VALUE ,
                                    INTEGER(fi-cantidad:SCREEN-VALUE) , 
                                    INTEGER(fi-desde:SCREEN-VALUE) , 
                                    INTEGER(fi-hasta:SCREEN-VALUE)
                                   ).
    IF RETURN-VALUE <> "" THEN
    DO:
        MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
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
             INPUT  'dsucursalstock.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedsucursalstockUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dsucursalstock ).
       RUN repositionObject IN h_dsucursalstock ( 15.52 , 135.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bsucursalstock.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bsucursalstock ).
       RUN repositionObject IN h_bsucursalstock ( 9.57 , 22.00 ) NO-ERROR.
       RUN resizeObject IN h_bsucursalstock ( 5.00 , 127.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dmovimientotambor.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedmovimientotamborUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dmovimientotambor ).
       RUN repositionObject IN h_dmovimientotambor ( 17.67 , 136.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'vmovimientotambor.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNamevmovimientotamborLogicalObjectNamevmovimientotamborPhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInityesObjectLayout':U ,
             OUTPUT h_vmovimientotambor ).
       RUN repositionObject IN h_vmovimientotambor ( 15.76 , 46.00 ) NO-ERROR.
       /* Size in AB:  ( 9.10 , 82.00 ) */

       RUN constructObject (
             INPUT  'dloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dloteubicacion ).
       RUN repositionObject IN h_dloteubicacion ( 19.57 , 136.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bloteubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bloteubicacion ).
       RUN repositionObject IN h_bloteubicacion ( 2.67 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_bloteubicacion ( 5.95 , 95.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtipomovimiento.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtipomovimientoUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtipomovimiento ).
       RUN repositionObject IN h_dtipomovimiento ( 21.48 , 135.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'btipomovimiento.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btipomovimiento ).
       RUN repositionObject IN h_btipomovimiento ( 2.67 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_btipomovimiento ( 5.71 , 53.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHideyesDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 1.00 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.24 , 21.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 136.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 22.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionVerticalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 9.10 , 153.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 5.95 , 5.60 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableioSubModulesTableIOTypeSaveSupportedLinksTableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsAdd,CopyHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsAdd,CopyNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 25.29 , 67.60 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 34.20 ) NO-ERROR.

       /* Links to SmartDataObject h_dsucursalstock. */
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_dsucursalstock ).

       /* Links to SmartDataBrowser h_bsucursalstock. */
       RUN addLink ( h_dsucursalstock , 'Data':U , h_bsucursalstock ).

       /* Links to SmartDataViewer h_vmovimientotambor. */
       RUN addLink ( h_dmovimientotambor , 'Data':U , h_vmovimientotambor ).
       RUN addLink ( h_vmovimientotambor , 'Update':U , h_dmovimientotambor ).
       RUN addLink ( h_dyntoolbar-2 , 'Tableio':U , h_vmovimientotambor ).

       /* Links to SmartDataObject h_dloteubicacion. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dloteubicacion ).

       /* Links to SmartDataBrowser h_bloteubicacion. */
       RUN addLink ( h_dloteubicacion , 'Data':U , h_bloteubicacion ).

       /* Links to SmartDataObject h_dtipomovimiento. */
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_dtipomovimiento ).

       /* Links to SmartDataBrowser h_btipomovimiento. */
       RUN addLink ( h_dtipomovimiento , 'Data':U , h_btipomovimiento ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             h_dyntoolbar-4 , 'AFTER':U ).
       RUN adjustTabOrder ( h_bloteubicacion ,
             h_btipomovimiento , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar-3 ,
             h_bloteubicacion , 'AFTER':U ).
       RUN adjustTabOrder ( h_bsucursalstock ,
             h_dyntoolbar-3 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vmovimientotambor ,
             h_bsucursalstock , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar-2 ,
             fi-hasta:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustomRowChanged wWin 
PROCEDURE CustomRowChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR vcQuery      AS CHARACTER    NO-UNDO.
DEFINE VAR hRowObject   AS HANDLE       NO-UNDO.
DEFINE VAR hField       AS HANDLE       NO-UNDO.
DEFINE VAR hField1      AS HANDLE       NO-UNDO.

hRowObject  = DYNAMIC-FUNCTION('getRowObject' IN h_dtipomovimiento).
hField      = hRowObject:BUFFER-FIELD('suc_origen').
hField1     = hRowObject:BUFFER-FIELD('suc_destino').


IF hField:BUFFER-VALUE <> "" THEN
DO:
    vcQuery = 'FOR EACH lotes_ubicacion WHERE CAN-DO(' + '"' + hField:BUFFER-VALUE +  '"'  + ' , string(lotes_ubicacion.id_sucursal_ubicacion) ) , FIRST SUCURSALES OF LOTES_UBICACION NO-LOCK'.
    DYNAMIC-FUNCTION ('setqueryWhere' IN h_dLoteUbicacion , vcQuery).
    DYNAMIC-FUNCTION ('openQuery' IN h_dLoteUbicacion ).
END.

IF hField1:BUFFER-VALUE <> "" THEN
DO:
    vcQuery = 'FOR EACH sucursales WHERE CAN-DO(' + '"' + hField1:BUFFER-VALUE +  '"'  + ' , string(sucursales.id_sucursal) ) '.
    DYNAMIC-FUNCTION ('setqueryWhere' IN h_dSucursalStock , vcQuery).
    DYNAMIC-FUNCTION ('openQuery' IN h_dSucursalStock ).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilitaMovimiento wWin 
PROCEDURE deshabilitaMovimiento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR vcQuery    AS CHARACTER NO-UNDO.
  vcQuery = 'for each movimientos_tambores where movimientos_tambores.nromov = ? no-lock'.
  {set queryWhere vcQuery h_dMovimientoTambor}.
  DYNAMIC-FUNCTION ('openQuery' IN h_dmovimientoTambor).

  fi-hasta:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE. 
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
  DISPLAY fi-cantidad fi-desde fi-hasta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fi-cantidad fi-desde fi-hasta RECT-10 RECT-12 RECT-2 RECT-3 RECT-5 
         RECT-6 RECT-8 RECT-9 
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

  /* Code placed here will execute AFTER standard behavior.    */

  /* RUN deshabilitar IN h_vMovimientoTambor. */


  RUN Ajustes IN h_dtipoMovimiento.

  SUBSCRIBE 'CustomRowChanged' IN h_btipomovimiento.

  RUN deshabilitaMovimiento.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paintBrowser wWin 
PROCEDURE paintBrowser :
/*------------------------------------------------------------------------------
  Purpose:     este procedure no hace nada lo convoca el browser de lote_ubicacion
               pero en esta pantalla no hace nada.
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getbrowseLoteUbicacionHandle wWin 
FUNCTION getbrowseLoteUbicacionHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN h_bLoteUbicacion.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteUbicacionHandle wWin 
FUNCTION getLoteUbicacionHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN h_dLoteUbicacion.   /* Function return value. */

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

