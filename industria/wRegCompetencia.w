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

DEFINE VARIABLE chGraphBar AS COM-HANDLE     NO-UNDO.

DEFINE  TEMP-TABLE ttCompetencia
  RCODE-INFORMATION
  FIELD semana          AS CHARACTER COLUMN-LABEL "Semana"
  FIELD puerto_origen   AS CHARACTER COLUMN-LABEL "Puerto Origen"
  FIELD compania        AS CHARACTER COLUMN-LABEL "Compa�ia"
  FIELD region          AS CHARACTER COLUMN-LABEL "Region"
  FIELD puerto_destino  AS CHARACTER COLUMN-LABEL "Puerto Destino"
  FIELD producto        AS CHARACTER COLUMN-LABEL "Producto"
  FIELD vapor           AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD fecha           AS DATE      COLUMN-LABEL "Fecha"
  FIELD cantidad        AS CHARACTER COLUMN-LABEL "Cantidad"
  FIELD anio            AS INTEGER   COLUMN-LABEL "Anio".

DEFINE TEMP-TABLE ttSamiVSOtros
  RCODE-INFORMATION
  FIELD region          AS CHARACTER COLUMN-LABEL "Region"
  FIELD puerto_destino  AS CHARACTER COLUMN-LABEL "Puerto Destino"
  FIELD cantidad_sami   AS INTEGER   COLUMN-LABEL "Cantidad Sami"
  FIELD cantidad_otros  AS INTEGER   COLUMN-LABEL "Cantidad Otros"
  FIELD cantidad_total  AS INTEGER   COLUMN-LABEL "Cantidad Total"
  FIELD porcentaje_sami AS INTEGER   COLUMN-LABEL "(%) Porc. Sami"
  FIELD anio            AS INTEGER   COLUMN-LABEL "Anio".

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
&Scoped-Define ENABLED-OBJECTS optAnio fiSemana btnFilter RECT-1 RECT-2 ~
RECT-3 
&Scoped-Define DISPLAYED-OBJECTS optAnio fiSemana 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createButtons wWin 
FUNCTION createButtons RETURNS CHARACTER
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
DEFINE VARIABLE h_bitemsregistroexportacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bregexp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsregistroexportacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dregexp AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vitemsregistroexportacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vregistroexportacion AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFilter 
     LABEL "Filtrar" 
     SIZE 15 BY 1.14 TOOLTIP "Filtrar".

DEFINE VARIABLE fiSemana AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Semana" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE optAnio AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "2004", 1,
"2005", 2,
"2004 + 2005", 3, 
"2006", 4
     SIZE 17 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.4 BY 4.29.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.4 BY 5.19.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     optAnio AT ROW 2.67 COL 57 NO-LABEL
     fiSemana AT ROW 2.91 COL 82 COLON-ALIGNED
     btnFilter AT ROW 2.91 COL 100
     RECT-1 AT ROW 2.43 COL 55.6
     RECT-2 AT ROW 6.76 COL 55.6
     RECT-3 AT ROW 2.43 COL 116
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.6 BY 22.38.


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
         TITLE              = "Registo Competencia"
         HEIGHT             = 21.33
         WIDTH              = 151
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Registo Competencia */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Registo Competencia */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter wWin
ON CHOOSE OF btnFilter IN FRAME fMain /* Filtrar */
DO:
  DEFINE VARIABLE cColumns   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOperators AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValues    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry       AS CHARACTER  NO-UNDO.

  cColumns    = "registro_exportacion.anio".
  cOperators  = "=".
  cValues     = "".

  CASE optAnio:SCREEN-VALUE:
    WHEN "1" THEN DO: 
      cValues = "2004".
    END.
    WHEN "2" THEN DO: 
      cValues = "2005".
    END.
    WHEN "3" THEN DO:
      cOperators  = ">=".
      cValues     = "2004".      
    END.
    WHEN "4" THEN DO: 
      cValues = "2006".
    END.
  END CASE.
  

  IF fiSemana:SCREEN-VALUE <> "0" THEN DO:
    cColumns    = cColumns + ",registro_exportacion.semana".
    cOperators  = cOperators + ",=".
    cValues     = cValues + CHR(1) + fiSemana:SCREEN-VALUE.
  END.
  IF fiSemana:SCREEN-VALUE = "0" THEN DO:
    DYNAMIC-FUNCTION('removeQuerySelection' IN h_dRegExp, "registro_exportacion.semana", "=").    
  END.


  DYNAMIC-FUNCTION('assignQuerySelection' IN h_dRegExp, cColumns, cValues, cOperators).    
  DYNAMIC-FUNCTION('openQuery' IN h_dRegExp).

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
             INPUT  'vregistroexportacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vregistroexportacion ).
       RUN repositionObject IN h_vregistroexportacion ( 6.95 , 56.60 ) NO-ERROR.
       /* Size in AB:  ( 3.62 , 50.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Function,Banda1SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsFilter,PrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 55.60 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 96.40 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Detalles' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 12.19 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 10.00 , 151.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1') NO-ERROR.

       /* Links to SmartDataViewer h_vregistroexportacion. */
       RUN addLink ( h_dregexp , 'Data':U , h_vregistroexportacion ).
       RUN addLink ( h_vregistroexportacion , 'Update':U , h_dregexp ).
       RUN addLink ( h_dyntoolbar , 'TableIo':U , h_vregistroexportacion ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'dregexp.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsregistro_exportacion.anio,anio,registro_exportacion.semana,semana,registro_exportacion.id_puerto_origen,id_puerto_origenRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedregexpUpdateFromSourcenoToggleDataTargetsyesOpenOnInitno':U ,
             OUTPUT h_dregexp ).
       RUN repositionObject IN h_dregexp ( 8.14 , 119.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bregexp.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bregexp ).
       RUN repositionObject IN h_bregexp ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bregexp ( 10.95 , 54.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ditemsregistroexportacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_registro_exportacion.anio,anio,items_registro_exportacion.semana,semana,items_registro_exportacion.id_puerto_origen,id_puerto_origenRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsregistroexportacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsregistroexportacion ).
       RUN repositionObject IN h_ditemsregistroexportacion ( 8.14 , 131.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemsregistroexportacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsregistroexportacion ).
       RUN repositionObject IN h_bitemsregistroexportacion ( 13.38 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsregistroexportacion ( 8.57 , 76.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vitemsregistroexportacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vitemsregistroexportacion ).
       RUN repositionObject IN h_vitemsregistroexportacion ( 15.05 , 80.00 ) NO-ERROR.
       /* Size in AB:  ( 6.81 , 68.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 13.38 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 67.20 ) NO-ERROR.

       /* Links to SmartDataObject h_dregexp. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dregexp ).

       /* Links to SmartDataBrowser h_bregexp. */
       RUN addLink ( h_dregexp , 'Data':U , h_bregexp ).

       /* Links to SmartDataObject h_ditemsregistroexportacion. */
       RUN addLink ( h_dregexp , 'Data':U , h_ditemsregistroexportacion ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_ditemsregistroexportacion ).

       /* Links to SmartDataBrowser h_bitemsregistroexportacion. */
       RUN addLink ( h_ditemsregistroexportacion , 'Data':U , h_bitemsregistroexportacion ).

       /* Links to SmartDataViewer h_vitemsregistroexportacion. */
       RUN addLink ( h_ditemsregistroexportacion , 'Data':U , h_vitemsregistroexportacion ).
       RUN addLink ( h_vitemsregistroexportacion , 'Update':U , h_ditemsregistroexportacion ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vitemsregistroexportacion ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 1 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects wWin 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  createButtons().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE drawGraph wWin 
PROCEDURE drawGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piSemana AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piPuerto AS INTEGER NO-UNDO.

DEFINE VARIABLE vcLista AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcRow   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE viData  AS INTEGER    NO-UNDO.
DEFINE VARIABLE i       AS INTEGER    NO-UNDO.

/*  
  vcLista = DYNAMIC-FUNCTION('getGraphEntry' IN h_dRegistroExportacion, piSemana, piPuerto).
  IF vcLista <> "" THEN DO:
    chGraphBar:NumSets = 1.
    chGraphBar:NumPoints = NUM-ENTRIES(vcLista) - 1 .
    DO i = 1 TO chGraphBar:NumPoints:
      vcRow = ENTRY(i, vcLista, CHR(14)).
      chGraphBar:LABEL(i) = SUBSTRING(ENTRY(1, vcRow, ","), 1, 4).
      chGraphBar:Data(i)  = ENTRY(2, vcRow, ",").
    END.
    
    chGraphBar:DrawMode = 2.       
  END.
  
    
*/

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
  DISPLAY optAnio fiSemana 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE optAnio fiSemana btnFilter RECT-1 RECT-2 RECT-3 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTempTable wWin 
PROCEDURE fillTempTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN getDataForTT IN h_dRegExp (INPUT-OUTPUT TABLE ttCompetencia).
  
  

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
/*
  chGraphBar = chCtrlFrame:Graph.

  RUN drawGraph (DYNAMIC-FUNCTION('columnValue' IN h_dRegistroExportacion, 'semana'), 
                 DYNAMIC-FUNCTION('columnValue' IN h_dRegistroExportacion, 'id_puerto_origen')). 
*/
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "addCustomerAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "addHarborAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "ExportAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "ExportActionComp").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "addRegionAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "ExportActionGraph").
  
  SUBSCRIBE TO "tlbAddCustomer" IN h_dyntoolbar.
  SUBSCRIBE TO "tlbAddHarbor" IN h_dyntoolbar.
  SUBSCRIBE TO "tlbExport" IN h_dyntoolbar.
  SUBSCRIBE TO "tlbExportComp" IN h_dyntoolbar.
  SUBSCRIBE TO "tlbAddRegion" IN h_dyntoolbar.
  SUBSCRIBE TO "tlbExportGraph" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  /*RUN initQuery.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initQuery wWin 
PROCEDURE initQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.

  cQry = "registro_exportacion.anio = 9".

  DYNAMIC-FUNCTION('setQueryWhere' IN h_dRegExp, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dRegExp).
  */
  
/*  MESSAGE
"QueryString : " DYNAMIC-FUNCTION('getquerystring':U IN h_dRegExp) SKIP
"QueryWhere : " DYNAMIC-FUNCTION('getquerywhere':U IN h_dRegExp) SKIP
"BaseQuery : " DYNAMIC-FUNCTION('getbasequery':U IN h_dRegExp).

/* DYNAMIC-FUNCTION('setBaseQuery':U IN h_dRegExp, */
/* "FOR EACH customer NO-LOCK, */
/* FIRST Order OF Customer OUTER-JOIN NO-LOCK"). */
*/

DYNAMIC-FUNCTION('setBaseQuery':U IN h_dRegExp, "FOR EACH registro_Exportacion WHERE registro_exportacion.anio = 2005 NO-LOCK").
DYNAMIC-FUNCTION('setQueryWhere':U IN h_dRegExp, "").
DYNAMIC-FUNCTION('openQuery':U IN h_dRegExp).

/*
MESSAGE
"QueryString : " DYNAMIC-FUNCTION('getquerystring':U IN h_dRegExp) SKIP
"QueryWhere : " DYNAMIC-FUNCTION('getquerywhere':U IN h_dRegExp) SKIP
"BaseQuery : " DYNAMIC-FUNCTION('getbasequery':U IN h_dRegExp).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAddCustomer wWin 
PROCEDURE tlbAddCustomer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wEmpresaExp.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAddHarbor wWin 
PROCEDURE tlbAddHarbor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wLugarDescarga.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbAddRegion wWin 
PROCEDURE tlbAddRegion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wrLugDesRegion.w.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExport wWin 
PROCEDURE tlbExport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillTempTable.

  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR viFila  AS INTEGER.
  DEFINE VAR vcFila  AS CHARACTER.
  DEFINE VAR vcRange AS CHARACTER.

  CREATE "Excel.Application" chExcelAplication.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\DinamicaCompetencia(RO).xls').
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).
  chWorkSheet:Range("A1:AM1"):Font:Bold = TRUE.
  
  chWorkSheet:Range("A1:AM1"):Font:size    = 8.
  chWorkSheet:Range("A2:AM6000"):Font:size = 8.
   
  chWorkSheet:Range("A1"):Value = "Semana".
  chWorkSheet:Range("A1:A1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("A"):ColumnWidth = 10.

  chWorkSheet:Range("B1"):Value = "Puerto Origen".
  chWorkSheet:Range("B1:B1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("B"):ColumnWidth = 10.

  chWorkSheet:Range("C1"):Value = "Compan�ia".
  chWorkSheet:Range("C1:C1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("C"):ColumnWidth = 10.

  chWorkSheet:Range("D1"):Value = "Region".
  chWorkSheet:Range("D1:D1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("D"):ColumnWidth = 10.

  chWorkSheet:Range("E1"):Value = "Puerto Destino".
  chWorkSheet:Range("E1:E1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("E"):ColumnWidth = 10.

  chWorkSheet:Range("F1"):Value = "Producto".
  chWorkSheet:Range("F1:F1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("F"):ColumnWidth = 10.

  chWorkSheet:Range("G1"):Value = "Vapor".
  chWorkSheet:Range("G1:G1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("G"):ColumnWidth = 10.

  chWorkSheet:Range("H1"):Value = "Fecha".
  chWorkSheet:Range("H1:H1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("H"):ColumnWidth = 10.

  chWorkSheet:Range("I1"):Value = "Cantidad".
  chWorkSheet:Range("I1:I1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("I"):ColumnWidth = 10.

  chWorkSheet:Range("J1"):Value = "Anio".
  chWorkSheet:Range("J1:J1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("J"):ColumnWidth = 10.

  viFila = 2.
  FOR EACH ttCompetencia NO-LOCK.
    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.semana.
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.puerto_origen.
    vcRange = "C" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.compania.
    vcRange = "D" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.region.
    vcRange = "E" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.puerto_destino.
    vcRange = "F" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.producto.
    vcRange = "G" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.vapor.
    vcRange = "H" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.fecha.
    vcRange = "I" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.cantidad.
    vcRange = "J" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.anio.

    viFila = viFila + 1.
  END.
  
  /*Actualizar Dinamicas*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(2).
  chWorkSheet:PivotTables("xCompania"):RefreshTable().
  chWorkSheet = chExcelAplication:Sheets:ITEM(3).
  chWorkSheet:PivotTables("xSemanaPuertoCompania"):RefreshTable().
  chWorkSheet = chExcelAplication:Sheets:ITEM(4).
  chWorkSheet:PivotTables("xSemanaVaporCompania"):RefreshTable().
  chWorkSheet = chExcelAplication:Sheets:ITEM(5).
  chWorkSheet:PivotTables("xSemanaCompania"):RefreshTable().
  chWorkSheet = chExcelAplication:Sheets:ITEM(6).
  chWorkSheet:PivotTables("xPuertoCompania"):RefreshTable().
  
  chExcelAplication:VISIBLE = TRUE.
  /***********Release Variables*****************/
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 
  
  /*
  RUN generateExcel.p (INPUT TABLE ttCompetencia,
                       INPUT " Competencia ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExportComp wWin 
PROCEDURE tlbExportComp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN getDataForTTCuadro IN h_dRegExp (INPUT-OUTPUT TABLE ttSamiVSOtros).

  RUN generateExcel.p (INPUT TABLE ttSamiVSOtros,
                       INPUT " Detalle Comparativo de Embarques ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExportGraph wWin 
PROCEDURE tlbExportGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillTempTable.

  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR viFila  AS INTEGER.
  DEFINE VAR vcFila  AS CHARACTER.
  DEFINE VAR vcRange AS CHARACTER.

  CREATE "Excel.Application" chExcelAplication.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\GraficoCompetencia(RO).xls').
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).
  chWorkSheet:Range("A1:AM1"):Font:Bold = TRUE.
  
  chWorkSheet:Range("A1:AM1"):Font:size    = 8.
  chWorkSheet:Range("A2:AM6000"):Font:size = 8.
   
  chWorkSheet:Range("A1"):Value = "Semana".
  chWorkSheet:Range("A1:A1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("A"):ColumnWidth = 10.

  chWorkSheet:Range("B1"):Value = "Puerto Origen".
  chWorkSheet:Range("B1:B1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("B"):ColumnWidth = 10.

  chWorkSheet:Range("C1"):Value = "Compan�ia".
  chWorkSheet:Range("C1:C1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("C"):ColumnWidth = 10.

  chWorkSheet:Range("D1"):Value = "Region".
  chWorkSheet:Range("D1:D1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("D"):ColumnWidth = 10.

  chWorkSheet:Range("E1"):Value = "Puerto Destino".
  chWorkSheet:Range("E1:E1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("E"):ColumnWidth = 10.

  chWorkSheet:Range("F1"):Value = "Producto".
  chWorkSheet:Range("F1:F1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("F"):ColumnWidth = 10.

  chWorkSheet:Range("G1"):Value = "Vapor".
  chWorkSheet:Range("G1:G1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("G"):ColumnWidth = 10.

  chWorkSheet:Range("H1"):Value = "Fecha".
  chWorkSheet:Range("H1:H1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("H"):ColumnWidth = 10.

  chWorkSheet:Range("I1"):Value = "Cantidad".
  chWorkSheet:Range("I1:I1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("I"):ColumnWidth = 10.

  chWorkSheet:Range("J1"):Value = "Anio".
  chWorkSheet:Range("J1:J1"):BorderAround(1,2,1,1).
  chWorkSheet:Columns("J"):ColumnWidth = 10.

  viFila = 2.
  FOR EACH ttCompetencia NO-LOCK.
    vcRange = "A" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.semana.
    vcRange = "B" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.puerto_origen.
    vcRange = "C" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.compania.
    vcRange = "D" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.region.
    vcRange = "E" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.puerto_destino.
    vcRange = "F" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.producto.
    vcRange = "G" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.vapor.
    vcRange = "H" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.fecha.
    vcRange = "I" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.cantidad.
    vcRange = "J" + STRING(viFila).
    chWorkSheet:Range(vcRange):VALUE = ttCompetencia.anio.

    viFila = viFila + 1.
  END.
  
  /*Actualizar Dinamicas*/
  chWorkSheet = chExcelAplication:Sheets:ITEM(3).
  chWorkSheet:PivotTables("grCompXRegion"):RefreshTable().
  
  chExcelAplication:VISIBLE = TRUE.
  /***********Release Variables*****************/
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 
  
  /*
  RUN generateExcel.p (INPUT TABLE ttCompetencia,
                       INPUT " Competencia ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createButtons wWin 
FUNCTION createButtons RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*Boton para Agregar Clientes*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "AddCustomerAction", 
                          "tlbAddCustomer", 
                          "Agregar Cliente", 
                          "507.bmp", 
                          "tlbAddCustomer", 
                          "FUNCTION").
  
  /*Boton para Agregar Puertos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "AddHarborAction", 
                          "tlbAddHarbor", 
                          "Agregar Puerto", 
                          "507.bmp", 
                          "tlbAddHarbor", 
                          "FUNCTION").

  /*Boton para Agregar Regiones*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "AddRegionAction", 
                          "tlbAddRegion", 
                          "Agregar Region", 
                          "507.bmp", 
                          "tlbAddRegion", 
                          "FUNCTION").

  /*Boton para exporta a Excel*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "ExportAction", 
                          "tlbExport", 
                          "Reportes Gerenciales", 
                          "excel.bmp", 
                          "tlbExport", 
                          "FUNCTION").

  /*Boton para exporta a Excel*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "ExportActionComp", 
                          "tlbExportComp", 
                          "Cuadro Comparativo", 
                          "charts.bmp", 
                          "tlbExportComp", 
                          "FUNCTION").

  /*Boton para exporta a Excel Graficos Comparativos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "ExportActionGraph", 
                          "tlbExportGraph", 
                          "Graficos Comparativo", 
                          "charts.bmp", 
                          "tlbExportGraph", 
                          "FUNCTION").

  RETURN "".   /* Function return value. */

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

