&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 

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
DEFINE VARIABLE h_bloteubicacioncascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brlotecascararemito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brproduccionlotecascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotescascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drlotecascararemito AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drproduccionlotecascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 21.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 15.95.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 6.24 COL 56
     RECT-3 AT ROW 1 COL 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 131.4 BY 23.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 4
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Trazabilidad Lotes Cascara"
         HEIGHT             = 23.33
         WIDTH              = 131.4
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
       RUN repositionObject IN h_dlotescascara ( 22.19 , 14.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldslotes_ubicacion.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dloteubicacion ).
       RUN repositionObject IN h_dloteubicacion ( 22.19 , 46.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'drproduccionlotecascara.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_produccion_cascara_lote.nromov_lote,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrproduccionlotecascaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drproduccionlotecascara ).
       RUN repositionObject IN h_drproduccionlotecascara ( 22.19 , 24.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 10.80 ) */

       RUN constructObject (
             INPUT  'drlotecascararemito.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_lote_cascara_remito.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrlotecascararemitoUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drlotecascararemito ).
       RUN repositionObject IN h_drlotecascararemito ( 22.19 , 35.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_sucursal,id_lote,anioOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsid_loteLoteid_sucursalCod.SucursalFieldToolTipsFieldHelpIdsanio0id_lote0id_sucursal0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.24 , 57.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.43 , 74.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Lote|Producciones|Remitos|Ubicacion' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 6.48 , 57.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 15.48 , 74.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dlotescascara. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dlotescascara ).

       /* Links to SmartDataObject h_dloteubicacion. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_dloteubicacion ).

       /* Links to SmartDataObject h_drproduccionlotecascara. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_drproduccionlotecascara ).

       /* Links to SmartDataObject h_drlotecascararemito. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_drlotecascararemito ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'blotescascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_blotescascara ).
       RUN repositionObject IN h_blotescascara ( 7.67 , 58.00 ) NO-ERROR.
       RUN resizeObject IN h_blotescascara ( 13.81 , 72.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_blotescascara. */
       RUN addLink ( h_dlotescascara , 'Data':U , h_blotescascara ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'brproduccionlotecascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brproduccionlotecascara ).
       RUN repositionObject IN h_brproduccionlotecascara ( 7.67 , 58.00 ) NO-ERROR.
       RUN resizeObject IN h_brproduccionlotecascara ( 14.05 , 71.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_brproduccionlotecascara. */
       RUN addLink ( h_drproduccionlotecascara , 'Data':U , h_brproduccionlotecascara ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'brlotecascararemito.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brlotecascararemito ).
       RUN repositionObject IN h_brlotecascararemito ( 7.67 , 58.00 ) NO-ERROR.
       RUN resizeObject IN h_brlotecascararemito ( 14.05 , 72.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_brlotecascararemito. */
       RUN addLink ( h_drlotecascararemito , 'Data':U , h_brlotecascararemito ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'bloteubicacioncascara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bloteubicacioncascara ).
       RUN repositionObject IN h_bloteubicacioncascara ( 7.67 , 58.00 ) NO-ERROR.
       RUN resizeObject IN h_bloteubicacioncascara ( 14.05 , 72.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bloteubicacioncascara. */
       RUN addLink ( h_dloteubicacion , 'Data':U , h_bloteubicacioncascara ).

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
  ENABLE RECT-1 RECT-2 RECT-3 
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
  DEFINE VARIABLE vcQry AS CHARACTER  NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
/*
  chTree = chCtrlFrame:TreeView.
  chImage = chCtrlFrame-2:ImageList.
  chTree:ImageList = chImage.
*/
/*
  vcQry = "general.lotes_ubicacion.id_sucursal_ubicacion <> 1 AND general.lotes_ubicacion.id_tipotambor = 11".
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dloteubicacion, vcQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dLoteUbicacion).
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
