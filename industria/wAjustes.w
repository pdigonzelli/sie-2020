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
&Scoped-Define ENABLED-OBJECTS fi-cantidad fi-desde fi-hasta RECT-12 ~
RECT-13 RECT-6 
&Scoped-Define DISPLAYED-OBJECTS fiLote fiOrigen fiDestino fi-cantidad ~
fi-desde fi-hasta 

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
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vmovimientotambor AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "src/adm2/image/commit.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Transferir >>" 
     SIZE 19 BY 4.29 TOOLTIP "Transferir"
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

DEFINE VARIABLE fiDestino AS CHARACTER FORMAT "X(256)":U 
     LABEL "Destino" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fiLote AS CHARACTER FORMAT "XXXXXX/XXXX":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fiOrigen AS CHARACTER FORMAT "X(256)":U 
     LABEL "Origen" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 7.14.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 6.67.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 1.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiLote AT ROW 9.29 COL 10 COLON-ALIGNED
     fiOrigen AT ROW 10.43 COL 10 COLON-ALIGNED
     fiDestino AT ROW 11.57 COL 10 COLON-ALIGNED
     fi-cantidad AT ROW 12.71 COL 10 COLON-ALIGNED
     fi-desde AT ROW 13.91 COL 10 COLON-ALIGNED
     fi-hasta AT ROW 15.1 COL 10 COLON-ALIGNED
     BUTTON-1 AT ROW 10.76 COL 39
     RECT-12 AT ROW 9.1 COL 2
     RECT-13 AT ROW 2.19 COL 2
     RECT-6 AT ROW 1 COL 2
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
   Design Page: 2
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Movimientos"
         HEIGHT             = 15.48
         WIDTH              = 157
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
/* SETTINGS FOR FILL-IN fiDestino IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiDestino:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiLote IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiLote:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fiOrigen IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fiOrigen:READ-ONLY IN FRAME fMain        = TRUE.

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
       RUN repositionObject IN h_dsucursalstock ( 3.14 , 55.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dmovimientotambor.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedmovimientotamborUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dmovimientotambor ).
       RUN repositionObject IN h_dmovimientotambor ( 5.29 , 56.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dloteubicacion ).
       RUN repositionObject IN h_dloteubicacion ( 7.19 , 56.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dtipomovimiento.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtipomovimientoUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtipomovimiento ).
       RUN repositionObject IN h_dtipomovimiento ( 9.10 , 55.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'btipomovimiento.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btipomovimiento ).
       RUN repositionObject IN h_btipomovimiento ( 2.43 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_btipomovimiento ( 6.19 , 55.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHideyesDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-4 ).
       RUN repositionObject IN h_dyntoolbar-4 ( 1.00 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-4 ( 1.24 , 21.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Origen|Destino|Datos Ajuste' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 1.00 , 61.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 15.24 , 97.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2,1') NO-ERROR.

       /* Links to SmartDataObject h_dsucursalstock. */
       RUN addLink ( h_dynfilter-2 , 'Filter':U , h_dsucursalstock ).
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_dsucursalstock ).

       /* Links to SmartDataObject h_dloteubicacion. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dloteubicacion ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dloteubicacion ).

       /* Links to SmartDataObject h_dtipomovimiento. */
       RUN addLink ( h_dyntoolbar-4 , 'Navigation':U , h_dtipomovimiento ).

       /* Links to SmartDataBrowser h_btipomovimiento. */
       RUN addLink ( h_dtipomovimiento , 'Data':U , h_btipomovimiento ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'bloteubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bloteubicacion ).
       RUN repositionObject IN h_bloteubicacion ( 6.24 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_bloteubicacion ( 9.52 , 88.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_sucursal_ubicacion,id_loteOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_loteLote SAMIid_sucursal_ubicacionDepositoFieldToolTipsFieldHelpIdsid_lote0id_sucursal_ubicacion0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.19 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 3.71 , 88.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionVerticalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 2.19 , 151.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 13.57 , 5.60 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bloteubicacion. */
       RUN addLink ( h_dloteubicacion , 'Data':U , h_bloteubicacion ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'bsucursalstock.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bsucursalstock ).
       RUN repositionObject IN h_bsucursalstock ( 6.48 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_bsucursalstock ( 9.29 , 88.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_sucursalOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsid_sucursal0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-2 ).
       RUN repositionObject IN h_dynfilter-2 ( 2.19 , 62.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-2 ( 3.71 , 88.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionVerticalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 2.19 , 151.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 13.57 , 5.60 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bsucursalstock. */
       RUN addLink ( h_dsucursalstock , 'Data':U , h_bsucursalstock ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'vmovimientotambor.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNamevmovimientotamborLogicalObjectNamevmovimientotamborPhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInityesObjectLayout':U ,
             OUTPUT h_vmovimientotambor ).
       RUN repositionObject IN h_vmovimientotambor ( 3.62 , 68.00 ) NO-ERROR.
       /* Size in AB:  ( 9.10 , 82.00 ) */

       /* Links to SmartDataViewer h_vmovimientotambor. */
       RUN addLink ( h_dmovimientotambor , 'Data':U , h_vmovimientotambor ).
       RUN addLink ( h_vmovimientotambor , 'Update':U , h_dmovimientotambor ).

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
  DISPLAY fiLote fiOrigen fiDestino fi-cantidad fi-desde fi-hasta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fi-cantidad fi-desde fi-hasta RECT-12 RECT-13 RECT-6 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showSources wWin 
PROCEDURE showSources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  fiLote:SCREEN-VALUE IN FRAME fMain    = DYNAMIC-FUNCTION('columnValue' IN h_dLoteUbicacion, 'lote').
  fiOrigen:SCREEN-VALUE IN FRAME fMain  = DYNAMIC-FUNCTION('columnValue' IN h_dLoteUbicacion, 'Deposito').
  fiDestino:SCREEN-VALUE IN FRAME fMain = DYNAMIC-FUNCTION('columnValue' IN h_dSucursalStock, 'nombre').

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

