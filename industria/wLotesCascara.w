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
&Scoped-Define ENABLED-OBJECTS btnAdd-2 btnAdd btnRemove fiTotal fiAdd ~
fiRemove RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS fiTotal fiAdd fiRemove fiBolsasRestantes 

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
DEFINE VARIABLE h_blotescascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bproduccioncascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brproduccionlotecascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotescascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dproduccioncascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drproduccionlotecascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vlotescascara AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "src/adm2/image/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "(+) >>" 
     SIZE 5.2 BY 1.19.

DEFINE BUTTON btnAdd-2 
     IMAGE-UP FILE "src/adm2/image/rollback.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "(+) >>" 
     SIZE 5.2 BY 1.19.

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "src/adm2/image/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "(-) <<" 
     SIZE 5.2 BY 1.19.

DEFINE VARIABLE fiAdd AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiBolsasRestantes AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bolsas Restantes para Completar Lote" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiRemove AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotal AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.38.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 9.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.38.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 4.05.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 1.67.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 4.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnAdd-2 AT ROW 14.91 COL 63.6
     btnAdd AT ROW 16.48 COL 63.6
     btnRemove AT ROW 18.05 COL 63.6
     fiTotal AT ROW 15.05 COL 67.4 COLON-ALIGNED NO-LABEL
     fiAdd AT ROW 16.62 COL 67.4 COLON-ALIGNED NO-LABEL
     fiRemove AT ROW 18.24 COL 67.4 COLON-ALIGNED NO-LABEL
     fiBolsasRestantes AT ROW 22.48 COL 137.4 RIGHT-ALIGNED
     RECT-1 AT ROW 16.38 COL 63
     RECT-2 AT ROW 2.43 COL 56
     RECT-3 AT ROW 17.95 COL 63
     RECT-4 AT ROW 11.95 COL 1
     RECT-5 AT ROW 22.19 COL 82
     RECT-6 AT ROW 1 COL 1
     RECT-7 AT ROW 14.81 COL 63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 141.4 BY 23.14.


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
         TITLE              = "Lotes de Cascara"
         HEIGHT             = 23.14
         WIDTH              = 141.4
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

/* SETTINGS FOR FILL-IN fiBolsasRestantes IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Lotes de Cascara */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Lotes de Cascara */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd wWin
ON CHOOSE OF btnAdd IN FRAME fMain /* (+) >> */
DO: 
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  viCant = DYNAMIC-FUNCTION('columnValue' IN h_dProduccionCascara, 'bolsas_disponibles').
  IF (INTEGER(fiAdd:SCREEN-VALUE) > 0 AND INTEGER(fiAdd:SCREEN-VALUE) <= viCant)   THEN DO:
    RUN addQty IN h_dLotesCascara (INTEGER(fiAdd:SCREEN-VALUE), 
                                   DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'id_empresa'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'id_sucursal'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'nromov'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_dProduccionCascara, 'id_sucursal'), 
                                   DYNAMIC-FUNCTION('columnValue' IN h_dProduccionCascara, 'id_produccion'), 
                                   "+").
    RUN refreshRow IN h_dLotesCascara.
    RUN refreshRow IN h_dProduccionCascara.
    DYNAMIC-FUNCTION('openQuery' IN h_dRProduccionLoteCascara).
    fiBolsasRestantes:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('getCantidadRestante' IN h_dLotesCascara)).
    fiAdd:SCREEN-VALUE = "".
    fiRemove:SCREEN-VALUE = "".
  END.
  ELSE DO:
    MESSAGE "La cantidad debe estar entre 1 y " + STRING(viCant) + " o esta produccion ya tiene el total de las bolsas comprometidas." VIEW-AS ALERT-BOX TITLE "Cantidad Erronea".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd-2 wWin
ON CHOOSE OF btnAdd-2 IN FRAME fMain /* (+) >> */
DO: 
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  viCant = INTEGER(fiTotal:SCREEN-VALUE).
  IF INTEGER(fiTotal:SCREEN-VALUE) > 0 THEN DO:
    RUN setProduccionesLoteCompleto IN h_dProduccionCascara (DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'id_empresa'),
                                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'id_sucursal'),
                                                             DYNAMIC-FUNCTION('columnValue' IN h_dLotesCascara, 'nromov'),
                                                             viCant).
    RUN refreshRow IN h_dLotesCascara.
    DYNAMIC-FUNCTION('openQuery' IN h_dProduccionCascara).
    DYNAMIC-FUNCTION('openQuery' IN h_dRProduccionLoteCascara).
    fiBolsasRestantes:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('getCantidadRestante' IN h_dLotesCascara)).
    fiAdd:SCREEN-VALUE = "".
    fiRemove:SCREEN-VALUE = "".
    fiTotal:SCREEN-VALUE = "".
  END.
  ELSE DO:
    MESSAGE "La cantidad debemayor que 0." VIEW-AS ALERT-BOX TITLE "Cantidad Erronea".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove wWin
ON CHOOSE OF btnRemove IN FRAME fMain /* (-) << */
DO:
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  viCant = DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'cantidad').
  IF (INTEGER(fiRemove:SCREEN-VALUE) > 0 AND INTEGER(fiRemove:SCREEN-VALUE) <= viCant)   THEN DO:
    RUN addQty IN h_dLotesCascara (INTEGER(fiRemove:SCREEN-VALUE), 
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'id_empresa_lote'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'id_sucursal_lote'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'nromov_lote'),
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'id_sucursal_prod'), 
                                   DYNAMIC-FUNCTION('columnValue' IN h_drProduccionLoteCascara, 'id_produccion_prod'), 
                                   "-").
    RUN refreshRow IN h_dLotesCascara.
    DYNAMIC-FUNCTION('openQuery' IN h_dProduccionCascara).
    DYNAMIC-FUNCTION('openQuery' IN h_drProduccionLoteCascara).
    fiBolsasRestantes:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('getCantidadRestante' IN h_dLotesCascara)).
    fiRemove:SCREEN-VALUE = "".
    fiAdd:SCREEN-VALUE = "".
  END.
  ELSE DO:
    MESSAGE "La cantidad debe estar entre 1 y " + STRING(viCant) VIEW-AS ALERT-BOX TITLE "Cantidad Erronea".
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
             INPUT  'dlotescascara.wDB-AWARE':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotescascaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dlotescascara ).
       RUN repositionObject IN h_dlotescascara ( 19.48 , 62.80 ) NO-ERROR.
       /* Size in AB:  ( 2.57 , 16.80 ) */

       RUN constructObject (
             INPUT  'vlotescascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vlotescascara ).
       RUN repositionObject IN h_vlotescascara ( 2.91 , 57.00 ) NO-ERROR.
       /* Size in AB:  ( 8.29 , 82.80 ) */

       RUN constructObject (
             INPUT  'blotescascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotescascara ).
       RUN repositionObject IN h_blotescascara ( 6.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_blotescascara ( 5.71 , 54.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'drproduccionlotecascara.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_produccion_cascara_lote.nromov_lote,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrproduccionlotecascaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drproduccionlotecascara ).
       RUN repositionObject IN h_drproduccionlotecascara ( 12.91 , 71.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 9.00 ) */

       RUN constructObject (
             INPUT  'brproduccionlotecascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brproduccionlotecascara ).
       RUN repositionObject IN h_brproduccionlotecascara ( 11.95 , 82.00 ) NO-ERROR.
       RUN resizeObject IN h_brproduccionlotecascara ( 10.00 , 59.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dproduccioncascara.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsproduccion_cascara.id_sucursal,id_sucursalRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedproduccioncascaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dproduccioncascara ).
       RUN repositionObject IN h_dproduccioncascara ( 12.91 , 61.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bproduccioncascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bproduccioncascara ).
       RUN repositionObject IN h_bproduccioncascara ( 16.24 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bproduccioncascara ( 7.86 , 59.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda1SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsPrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 56.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 86.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_lote,anio,id_lote_clienteOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth15DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_loteLoteid_sucursalSucursalid_lote_clienteLote ClienteFieldToolTipsFieldHelpIdsanio0id_lote0id_sucursal0id_lote_cliente0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-2 ).
       RUN repositionObject IN h_dynfilter-2 ( 1.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-2 ( 4.43 , 52.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsFecha,id_produccionOperatorStyleImplicitOperatorViewAsCombo-boxOperator>=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFechaFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsFecha0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 12.19 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 3.71 , 57.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dlotescascara. */
       RUN addLink ( h_dynfilter-2 , 'Filter':U , h_dlotescascara ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dlotescascara ).

       /* Links to SmartDataViewer h_vlotescascara. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_vlotescascara ).
       RUN addLink ( h_vlotescascara , 'Update':U , h_dlotescascara ).
       RUN addLink ( h_dyntoolbar , 'TableIo':U , h_vlotescascara ).

       /* Links to SmartDataBrowser h_blotescascara. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_blotescascara ).

       /* Links to SmartDataObject h_drproduccionlotecascara. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_drproduccionlotecascara ).

       /* Links to SmartDataBrowser h_brproduccionlotecascara. */
       RUN addLink ( h_drproduccionlotecascara , 'Data':U , h_brproduccionlotecascara ).

       /* Links to SmartDataObject h_dproduccioncascara. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_dproduccioncascara ).
       RUN addLink ( h_dynfilter , 'Filter':U , h_dproduccioncascara ).

       /* Links to SmartDataBrowser h_bproduccioncascara. */
       RUN addLink ( h_dproduccioncascara , 'Data':U , h_bproduccioncascara ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vlotescascara ,
             h_dynfilter-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_brproduccionlotecascara ,
             h_blotescascara , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynfilter ,
             h_brproduccionlotecascara , 'AFTER':U ).
       RUN adjustTabOrder ( h_bproduccioncascara ,
             fiTotal:HANDLE IN FRAME fMain , 'AFTER':U ).
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
  DISPLAY fiTotal fiAdd fiRemove fiBolsasRestantes 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE btnAdd-2 btnAdd btnRemove fiTotal fiAdd fiRemove RECT-1 RECT-2 RECT-3 
         RECT-4 RECT-5 RECT-6 RECT-7 
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

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

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
    
  APPLY "CLOSE":U TO THIS-PROCEDURE.
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

