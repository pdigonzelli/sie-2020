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

DEFINE OUTPUT PARAMETER pSubdVtas AS ROWID.
DEFINE OUTPUT PARAMETER pAuxItemsVenta AS ROWID.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-41 BUTTON-42 RECT-1 

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
DEFINE VARIABLE h_bauxitemsvtasproformas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bsubdvtasproformas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dauxitemsvtasproformas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsubdvtasproformas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-41 
     LABEL "Aceptar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-42 
     LABEL "Cancelar" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-41 AT ROW 21.95 COL 109
     BUTTON-42 AT ROW 21.95 COL 125
     RECT-1 AT ROW 21.48 COL 107
     "Lotes" VIEW-AS TEXT
          SIZE 130 BY .62 AT ROW 13.86 COL 4
          BGCOLOR 9 FGCOLOR 15 
     "Proformas" VIEW-AS TEXT
          SIZE 138 BY .62 AT ROW 6 COL 4
          BGCOLOR 2 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.4 BY 22.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Seleccion de Proforma y Lote"
         HEIGHT             = 22.57
         WIDTH              = 144.4
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 154.8
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 154.8
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
ON END-ERROR OF wWin /* Seleccion de Proforma y Lote */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Seleccion de Proforma y Lote */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-41
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-41 wWin
ON CHOOSE OF BUTTON-41 IN FRAME fMain /* Aceptar */
DO:
    pSubdVtas = DYNAMIC-FUNCTION('getRowid':U IN h_dsubdvtasproformas).
    pAuxItemsVenta = DYNAMIC-FUNCTION('getRowid':U IN h_dauxitemsvtasproformas).

    RUN exitObject.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-42
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-42 wWin
ON CHOOSE OF BUTTON-42 IN FRAME fMain /* Cancelar */
DO:
    pSubdVtas = ?.
    pAuxItemsVenta = ?.
    
    RUN exitObject.

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
             INPUT  'dsubdvtasproformas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedsubdvtasproformasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dsubdvtasproformas ).
       RUN repositionObject IN h_dsubdvtasproformas ( 2.67 , 100.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bsubdvtasproformas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bsubdvtasproformas ).
       RUN repositionObject IN h_bsubdvtasproformas ( 6.71 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_bsubdvtasproformas ( 6.67 , 139.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dauxitemsvtasproformas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsaux_items_venta.id_punto_venta,id_punto_venta,aux_items_venta.nromov,nromovRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedauxitemsvtasproformasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dauxitemsvtasproformas ).
       RUN repositionObject IN h_dauxitemsvtasproformas ( 13.38 , 134.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bauxitemsvtasproformas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bauxitemsvtasproformas ).
       RUN repositionObject IN h_bauxitemsvtasproformas ( 14.57 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_bauxitemsvtasproformas ( 6.67 , 139.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsfecha,nombre,nro_proformaOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesfechaRANGEFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsfecha0nombre0nro_proforma0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.00 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.43 , 79.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dsubdvtasproformas. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dsubdvtasproformas ).

       /* Links to SmartDataBrowser h_bsubdvtasproformas. */
       RUN addLink ( h_dsubdvtasproformas , 'Data':U , h_bsubdvtasproformas ).
       RUN addLink ( h_bsubdvtasproformas , 'Update':U , h_dsubdvtasproformas ).

       /* Links to SmartDataObject h_dauxitemsvtasproformas. */
       RUN addLink ( h_dsubdvtasproformas , 'Data':U , h_dauxitemsvtasproformas ).

       /* Links to SmartDataBrowser h_bauxitemsvtasproformas. */
       RUN addLink ( h_dauxitemsvtasproformas , 'Data':U , h_bauxitemsvtasproformas ).
       RUN addLink ( h_bauxitemsvtasproformas , 'Update':U , h_dauxitemsvtasproformas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynfilter ,
             BUTTON-41:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_bsubdvtasproformas ,
             h_dynfilter , 'AFTER':U ).
       RUN adjustTabOrder ( h_bauxitemsvtasproformas ,
             h_bsubdvtasproformas , 'AFTER':U ).
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
  ENABLE BUTTON-41 BUTTON-42 RECT-1 
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

