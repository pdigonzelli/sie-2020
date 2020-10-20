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

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE hRep AS HANDLE     NO-UNDO.


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
&Scoped-Define ENABLED-OBJECTS RECT-27 

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
DEFINE VARIABLE h_bitemsmovimientocamara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bmovimientocamara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_brmovcamaratambor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsmovimientocamara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dmovimientocamara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drmovcamaratambor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fcompomovcamara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vitemsmovmientocamara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vmovimientocamara AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 7.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-27 AT ROW 2.43 COL 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138.2 BY 22.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 3
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Movimientos de Camara"
         HEIGHT             = 22.67
         WIDTH              = 138.2
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
ON END-ERROR OF wWin /* Movimientos de Camara */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Movimientos de Camara */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addMovimientoCamara wWin 
PROCEDURE addMovimientoCamara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcRowIds AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cRows  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRowId AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE iRow  AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iEmp  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSMo  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTno  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dFec  AS DATE       NO-UNDO.
  DEFINE VARIABLE iMov  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iItm AS INTEGER    NO-UNDO.


  DO iRow = 1 TO NUM-ENTRIES(pcRowIds): /*on error undo, leave*/
    cRowId = ENTRY(iRow, pcRowIds).
    cKey   = DYNAMIC-FUNCTION('getClaveTamborFromRowid' IN hLib, TO-ROWID(cRowId)).

    ASSIGN iEmp = INTEGER(ENTRY(1, cKey))
           iSuc = INTEGER(ENTRY(2, cKey))
           iTip = INTEGER(ENTRY(3, cKey))
           iNro = INTEGER(ENTRY(4, cKey))
           iTbo = INTEGER(ENTRY(5, cKey))
           iSMo = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_sucursal') 
           iTno = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_turno') 
           dFec = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'fecha') 
           iMov = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_movimiento_camara')
           iItm = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'item').
  
  
    /*MESSAGE cRowId SKIP cKey SKIP iNro SKIP iTbo VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    RUN addMovimientoCamara IN hLib (iEmp, iSuc, iTip, iNro, iTbo,
                                     iSMo, iTno, dFec, iMov, iItm).


  END.
  
  
  DYNAMIC-FUNCTION('openQuery' IN h_drMovCamaraTambor).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addMovimientoCamaraOld wWin 
PROCEDURE addMovimientoCamaraOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phSdo     AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER phBrowser AS HANDLE     NO-UNDO.



  DEFINE VARIABLE cRows AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSMo  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTno  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dFec  AS DATE       NO-UNDO.
  DEFINE VARIABLE iMov  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iItm AS INTEGER    NO-UNDO.

  
  cRows = DYNAMIC-FUNCTION('getSelectedRows' IN phBrowser).

  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION('fetchRow' IN phSdo, INTEGER(ENTRY(iRow,cRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,id_tambor').
    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".

    ASSIGN iEmp = INTEGER(ENTRY(2, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(3, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(4, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(5, cFields, CHR(1)))
           iTbo = INTEGER(ENTRY(6, cFields, CHR(1)))
           iSMo = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_sucursal') 
           iTno = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_turno') 
           dFec = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'fecha') 
           iMov = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_movimiento_camara')
           iItm = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'item').



    RUN addMovimientoCamara IN hLib (iEmp, iSuc, iTip, iNro, iTbo,
                                     iSMo, iTno, dFec, iMov, iItm).
    
  END.


  DYNAMIC-FUNCTION('openQuery' IN h_drMovCamaraTambor).



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
             INPUT  'dmovimientocamara.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedmovimientocamaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dmovimientocamara ).
       RUN repositionObject IN h_dmovimientocamara ( 2.43 , 128.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bmovimientocamara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bmovimientocamara ).
       RUN repositionObject IN h_bmovimientocamara ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bmovimientocamara ( 8.81 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ditemsmovimientocamara.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_movimiento_camara.id_sucursal,id_sucursal,items_movimiento_camara.id_turno,id_turno,items_movimiento_camara.id_movimiento_camara,id_movimiento_camara,items_movimiento_camara.fecha,fechaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsmovimientocamaraUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsmovimientocamara ).
       RUN repositionObject IN h_ditemsmovimientocamara ( 4.33 , 128.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,Banda1,Banda2SubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsPrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 68.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 71.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsfecha,id_sucursal,id_movimiento_camara,nro_planilla,id_turnoOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsDesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.67 , 70.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 6.71 , 59.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Planilla|Movimientos|Tambores' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 10.05 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 13.57 , 138.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2') NO-ERROR.

       /* Links to SmartDataObject h_dmovimientocamara. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dmovimientocamara ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dmovimientocamara ).

       /* Links to SmartDataBrowser h_bmovimientocamara. */
       RUN addLink ( h_dmovimientocamara , 'Data':U , h_bmovimientocamara ).

       /* Links to SmartDataObject h_ditemsmovimientocamara. */
       RUN addLink ( h_dmovimientocamara , 'Data':U , h_ditemsmovimientocamara ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_ditemsmovimientocamara ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vmovimientocamara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vmovimientocamara ).
       RUN repositionObject IN h_vmovimientocamara ( 11.71 , 27.00 ) NO-ERROR.
       /* Size in AB:  ( 8.24 , 80.40 ) */

       /* Links to SmartDataViewer h_vmovimientocamara. */
       RUN addLink ( h_dmovimientocamara , 'Data':U , h_vmovimientocamara ).
       RUN addLink ( h_vmovimientocamara , 'Update':U , h_dmovimientocamara ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vmovimientocamara ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'bitemsmovimientocamara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsmovimientocamara ).
       RUN repositionObject IN h_bitemsmovimientocamara ( 11.48 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsmovimientocamara ( 11.67 , 60.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vitemsmovmientocamara.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vitemsmovmientocamara ).
       RUN repositionObject IN h_vitemsmovmientocamara ( 13.38 , 68.00 ) NO-ERROR.
       /* Size in AB:  ( 9.86 , 66.60 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 11.48 , 68.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 67.20 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bitemsmovimientocamara. */
       RUN addLink ( h_ditemsmovimientocamara , 'Data':U , h_bitemsmovimientocamara ).

       /* Links to SmartDataViewer h_vitemsmovmientocamara. */
       RUN addLink ( h_ditemsmovimientocamara , 'Data':U , h_vitemsmovmientocamara ).
       RUN addLink ( h_vitemsmovmientocamara , 'Update':U , h_ditemsmovimientocamara ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vitemsmovmientocamara ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'drmovcamaratambor.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_mov_camara_tambor.id_sucursal_movimiento,id_sucursal,r_mov_camara_tambor.id_turno,id_turno,r_mov_camara_tambor.fecha,fecha,r_mov_camara_tambor.id_movimiento_camara,id_movimiento_camara,r_mov_camara_tambor.item,itemRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrmovcamaratamborUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drmovcamaratambor ).
       RUN repositionObject IN h_drmovcamaratambor ( 6.24 , 129.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 9.20 ) */

       RUN constructObject (
             INPUT  'brmovcamaratambor.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brmovcamaratambor ).
       RUN repositionObject IN h_brmovcamaratambor ( 11.48 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_brmovcamaratambor ( 11.67 , 58.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'fcompomovcamara.r':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fcompomovcamara ).
       RUN repositionObject IN h_fcompomovcamara ( 11.48 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 11.81 , 76.60 ) */

       /* Links to SmartDataObject h_drmovcamaratambor. */
       RUN addLink ( h_ditemsmovimientocamara , 'Data':U , h_drmovcamaratambor ).

       /* Links to SmartDataBrowser h_brmovcamaratambor. */
       RUN addLink ( h_drmovcamaratambor , 'Data':U , h_brmovcamaratambor ).

       /* Links to SmartFrame h_fcompomovcamara. */
       RUN addLink ( h_folder , 'Page':U , h_fcompomovcamara ).

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

  RUN filterTambores.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delMovimientoCamara wWin 
PROCEDURE delMovimientoCamara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRows AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSMo  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTno  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dFec  AS DATE       NO-UNDO.
  DEFINE VARIABLE iMov  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iItm  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCam  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTmo  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFil  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRid  AS CHARACTER  NO-UNDO.

  
  cRows = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_brMovCamaraTambor).
  
  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
    cRid = ENTRY(iRow, cRows).
    cKey = DYNAMIC-FUNCTION('getClaveTamborFromRowId' IN h_drMovCamaraTambor, TO-ROWID(cRid)).

    ASSIGN iEmp = INTEGER(ENTRY(1, cKey))
           iSuc = INTEGER(ENTRY(2, cKey))
           iTip = INTEGER(ENTRY(3, cKey))
           iNro = INTEGER(ENTRY(4, cKey))
           iTbo = INTEGER(ENTRY(5, cKey))
           iSMo = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_sucursal') 
           iTno = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_turno') 
           dFec = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'fecha') 
           iMov = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_movimiento_camara')
           iItm = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'item')
           iCam = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_camara')
           iTmo = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_tipo_movimiento').

    RUN delMovimientoCamara IN hLib (iEmp, iSuc, iTip, iNro, iTbo,
                                     iSMo, iTno, dFec, iMov, iItm).


    IF iTmo = 3 THEN DO:
      cFil = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'nro_fila_origen').
      cCol = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'nro_columna_origen').

      RUN setMovimientoCamaraTambor IN hLib (iEmp, iSuc, iTip, iNro, iTbo, 
                                             iEmp, iSMo, iCam, cFil, cCol).
    END.
    
  END.


  DYNAMIC-FUNCTION('openQuery' IN h_drMovCamaraTambor).

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
  ENABLE RECT-27 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterTambores wWin 
PROCEDURE filterTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCam AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol AS CHARACTER  NO-UNDO.

  ASSIGN iTip = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_tipo_movimiento')
         iSuc = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_sucursal')
         iEmp = 1
         iCam = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'id_camara')
         cFil = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'nro_fila_origen') 
         cCol = DYNAMIC-FUNCTION('columnValue' IN h_dItemsMovimientoCamara, 'nro_columna_origen').

  IF iTip = 1 THEN DO: /*ingreso*/
    ASSIGN iCam = 0
           cFil = ""
           cCol = "".
  END.

  IF INTEGER(DYNAMIC-FUNCTION('getCurrentPage')) = 3 THEN
    RUN refreshData IN h_fCompoMovCamara (iEmp, 
                                          iSuc, 
                                          iCam, 
                                          cFil, 
                                          cCol).

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

  RUN libTamboresIndustria.p PERSISTENT SET hLib.
  RUN libReportes.p PERSISTENT SET hRep.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "excelAction").  
  SUBSCRIBE TO "tlbExcel" IN h_dyntoolbar.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExcel wWin 
PROCEDURE tlbExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN callReporteCamara IN hRep (DYNAMIC-FUNCTION('columnValue' IN h_dMovimientoCamara, 'id_sucursal'), 0).
  RUN exportExcelCamara IN hRep.

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

