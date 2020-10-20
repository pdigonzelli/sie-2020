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
DEFINE VARIABLE chProg  AS COM-HANDLE     NO-UNDO.


DEFINE TEMP-TABLE ttTraza
  RCODE-INFORMATION
  FIELD fechal       AS DATE COLUMN-LABEL "Fecha Creacion"
  FIELD planta       AS INTEGER COLUMN-LABEL "Planta Produccion"
  FIELD lote_sami    AS INTEGER COLUMN-LABEL "Lote Sami"
  FIELD anio_lote    AS INTEGER COLUMN-LABEL "Anio Lote"
  FIELD lote_cliente AS CHARACTER COLUMN-LABEL "Lote Cliente"
  FIELD cantidadl    AS INTEGER COLUMN-LABEL "Cantidad Produccion"
  FIELD nro_remito   AS INTEGER COLUMN-LABEL "Nro Rto Int"
  FIELD nro_comp     AS CHARACTER COLUMN-LABEL "Nro Comprobante"
  FIELD id_suc_ori   AS INTEGER COLUMN-LABEL "Cod Suc Ori"
  FIELD desde        AS CHARACTER COLUMN-LABEL "Suc Desde"
  FIELD id_suc_des   AS INTEGER COLUMN-LABEL "Cod Suc Des"
  FIELD hasta        AS CHARACTER COLUMN-LABEL "Suc Hasta"
  FIELD fechar       AS DATE COLUMN-LABEL "Fecha Rto"
  FIELD cantidadr    AS INTEGER COLUMN-LABEL "Cantidad Remito"
  FIELD kilosr       AS INTEGER COLUMN-LABEL "Kilos"
  FIELD qty_lote     AS INTEGER COLUMN-LABEL "Cantidad Tot Lote"
  FIELD nromov       AS INTEGER COLUMN-LABEL "Nromov".

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
&Scoped-Define ENABLED-OBJECTS BUTTON-1 RECT-1 RECT-2 RECT-3 

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

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_blotescascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bloteubicacioncascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brlotecascararemito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotescascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drlotecascararemito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Button 1" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 133 BY 11.91.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 9.05.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 7.91 COL 51
     RECT-1 AT ROW 10.29 COL 1
     RECT-2 AT ROW 1 COL 68
     RECT-3 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 133.2 BY 21.29.


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
         TITLE              = "Trazabilidad Lotes Cascara"
         HEIGHT             = 21.29
         WIDTH              = 133.2
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 8.86
       COLUMN          = 51
       HEIGHT          = 1.91
       WIDTH           = 8
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 10.52
       COLUMN          = 2
       HEIGHT          = 11.43
       WIDTH           = 131
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
      CtrlFrame-2:MOVE-AFTER(BUTTON-1:HANDLE IN FRAME fMain).
      CtrlFrame:MOVE-AFTER(CtrlFrame-2).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Trazabilidad Lotes Cascara */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Trazabilidad Lotes Cascara */
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
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  
  RUN fillTree.

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
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlotescascaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dlotescascara ).
       RUN repositionObject IN h_dlotescascara ( 7.67 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'drlotecascararemito.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_lote_cascara_remito.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrlotecascararemitoUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drlotecascararemito ).
       RUN repositionObject IN h_drlotecascararemito ( 7.67 , 25.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldslotes_ubicacion.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dloteubicacion ).
       RUN repositionObject IN h_dloteubicacion ( 7.67 , 36.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda3,Banda1SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdate,Filter,Customaction,PrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Lote|Remitos - Lotes|Stock' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 1.24 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 8.57 , 64.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_sucursal,id_lote,anioOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_loteLoteid_sucursalCod.SucursalFieldToolTipsFieldHelpIdsanio0id_lote0id_sucursal0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.67 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.43 , 64.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dlotescascara. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dlotescascara ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dlotescascara ).

       /* Links to SmartDataObject h_drlotecascararemito. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_drlotecascararemito ).

       /* Links to SmartDataObject h_dloteubicacion. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_dloteubicacion ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'blotescascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotescascara ).
       RUN repositionObject IN h_blotescascara ( 2.43 , 70.00 ) NO-ERROR.
       RUN resizeObject IN h_blotescascara ( 7.14 , 62.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_blotescascara. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_blotescascara ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'brlotecascararemito.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brlotecascararemito ).
       RUN repositionObject IN h_brlotecascararemito ( 2.43 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_brlotecascararemito ( 7.38 , 62.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_brlotecascararemito. */
       RUN addLink ( h_drlotecascararemito , 'Data':U , h_brlotecascararemito ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'bloteubicacioncascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bloteubicacioncascara ).
       RUN repositionObject IN h_bloteubicacioncascara ( 2.43 , 70.00 ) NO-ERROR.
       RUN resizeObject IN h_bloteubicacioncascara ( 7.14 , 62.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bloteubicacioncascara. */
       RUN addLink ( h_dloteubicacion , 'Data':U , h_bloteubicacioncascara ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterFilter wWin 
PROCEDURE afterFilter :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER pcFieldNames  AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcFieldValues AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcOperators   AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT-OUTPUT pcFieldNames, INPUT-OUTPUT pcFieldValues, INPUT-OUTPUT pcOperators).


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

OCXFile = SEARCH( "wTrazaLotes.wrx":U ).
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
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wTrazaLotes.wrx":U SKIP(1)
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
  /*Boton de llamada a dialog de referencias*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "expTrazaAction", 
                          "tlbTraza", 
                          "Exportacion a Excel", 
                          "print.bmp", 
                          "tlbTraza", 
                          "BANDA3").


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
  ENABLE BUTTON-1 RECT-1 RECT-2 RECT-3 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTree wWin 
PROCEDURE fillTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE chNode   AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE chRoot   AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE vcLotes  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcLote   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcEntry  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcRKey   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcPKey   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcLKey   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcUKey   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcText   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcProds  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcProd   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcRemis  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcRemi   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcUbis   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcUbi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cText    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cImg     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE viNroMov AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCant    AS INTEGER    NO-UNDO.


  vcLotes = DYNAMIC-FUNCTION('getArrayLotes' IN h_dLotesCascara).
  
  /*chProg:MIN = 0.
  chProg:MAX = NUM-ENTRIES(vcLotes, ",").*/
  chTree:Nodes:CLEAR().
  DO i = 1 TO NUM-ENTRIES(vcLotes, ","):
    /*chProg:VALUE = i.*/
    vcEntry  = ENTRY(i, vcLotes, ",").
    vcLote   = ENTRY(2, vcEntry, CHR(14)) + " - " + ENTRY(3, vcEntry, CHR(14)).
    vcLKey   = STRING(i) + ENTRY(2, vcEntry, CHR(14)).
    chNode   = chTree:Nodes:ADD(, , vcLKey, vcLote, "lote").
    viNroMov = INTEGER(ENTRY(1, vcEntry, CHR(14))).
    
    /*cuelgo las producciones*/
    vcProds = DYNAMIC-FUNCTION('getArrayProducciones' IN h_drProduccionLoteCascara, viNroMov).
    cText   = "Producciones x(" + STRING(NUM-ENTRIES(vcProds, CHR(14))) + ")".
    vcPKey  = "Prod" + vcLKey.
    chNode  = chTree:Nodes:ADD(vcLKey, 4, vcPKey, cText, "prods").    
    DO j = 1 TO NUM-ENTRIES(vcProds, CHR(14)):
      vcProd = ENTRY(j, vcProds, CHR(14)).
      vcText = "Prod: " + ENTRY(1, vcProd) + " - Fecha: " + ENTRY(2, vcProd) + " - Cantidad: " + ENTRY(3, vcProd).
      vcKey  = "P" + STRING(i) + STRING(j) + ENTRY(1, vcProd).
      chNode = chTree:Nodes:ADD(vcPKey, 4, vcKey, vcText, "prod").
    END.
    
    /*cuelgo los remitos*/
    vcRemis = DYNAMIC-FUNCTION('getArrayRemitos' IN h_drLoteCascaraRemito, viNroMov).
    cText   = "Remitos x(" + STRING(NUM-ENTRIES(vcRemis, CHR(14))) + ")".
    vcRKey  = "Remi" + vcLKey.    
    chNode  = chTree:Nodes:ADD(vcLKey, 4, vcRKey, cText, "remi").    
    DO j = 1 TO NUM-ENTRIES(vcRemis, CHR(14)):
      vcRemi = ENTRY(j, vcRemis, CHR(14)).
      cImg = "remito".
      IF ENTRY(7, vcRemi) = "Manual" THEN 
        cImg = "manual".
      IF ENTRY(6, vcRemi) = "Anulado" THEN
        cImg = "anulado".
      vcText = "Remito: " + ENTRY(1, vcRemi) + 
               " - Fecha: " + ENTRY(2, vcRemi) + 
               " - de: " + ENTRY(4, vcRemi) + 
               " - a: " + ENTRY(5, vcRemi) + 
               " - Cantidad: " + ENTRY(3, vcRemi) + 
               " - Estado: " + ENTRY(6, vcRemi) + 
               " - TipoMov: " + ENTRY(7, vcRemi).
      vcKey  = "R" + STRING(i) + STRING(j) + ENTRY(1, vcRemi).
      chNode = chTree:Nodes:ADD(vcRKey, 4, vcKey, vcText, cImg).
    END.

    /*cuelgo lotes_ubicacion*/
    vcUKey = "Stoc" + vcLKey.
    chRoot = chTree:Nodes:ADD(vcLKey, 4, vcUKey, "Ubicacion", "stoc").
    vcUbis = DYNAMIC-FUNCTION('getArrayUbicacion' IN h_dLotesCascara, viNroMov).
    DO j = 1 TO NUM-ENTRIES(vcUbis, CHR(14)):
      vcUbi = ENTRY(j, vcUbis, CHR(14)).
      vcText = "en " + ENTRY(1, vcUbi) + " -> " + ENTRY(2, vcUbi) + " bolsas".
      vcKey  = "U" + STRING(i) + STRING(j) + ENTRY(1, vcUbi).
      chNode = chTree:Nodes:ADD(vcUKey, 4, vcKey, vcText, "loteubi").
      iCant = iCant + INTEGER(ENTRY(2, vcUbi)).
    END.
    chRoot:TEXT = chRoot:TEXT + " x(" + STRING(iCant) + ")".
    iCant = 0.
    
  
  END.
  /*chProg:VALUE = 0.*/


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

  chTree  = chCtrlFrame:TreeView.
  chImage = chCtrlFrame-2:ImageList.
  /*chProg  = chCtrlFrame-3:ProgressBar.*/
  chTree:ImageList = chImage.
  

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "expTrazaAction").
  SUBSCRIBE TO "tlbTraza" IN h_dyntoolbar.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbTraza wWin 
PROCEDURE tlbTraza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.

  RUN wdParams.w (OUTPUT iSuc, 
                  OUTPUT dDes, 
                  OUTPUT dHas).

  IF dDes <> ? AND dHas <> ? THEN DO:    
    RUN fillttTraza IN h_dLotesCascara (INPUT-OUTPUT TABLE ttTraza, 
                                        iSuc, 
                                        dDes, 
                                        dHas).
    
    RUN generateExcel.p (INPUT TABLE ttTraza,
                         INPUT " Despachos Lotes de Cascara",
                         INPUT " ",
                         INPUT 7,
                         INPUT 8,
                         INPUT "Century Gothic",
                         INPUT 7).
  END.
  ELSE
    MESSAGE "Complete los parametros." VIEW-AS ALERT-BOX INFO BUTTONS OK.



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

