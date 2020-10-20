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
&Scoped-Define ENABLED-OBJECTS RECT-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b000cobranzaspermisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b000facturaspermisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b000gastosventapermisosembarqu AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b000itemsventapermisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b000permisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000cobranzaspermisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000facturaspermisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000gastosventapermisosembarqu AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000itemsventapermisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000permisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v000cobranzaspermisosembarque AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v000permisosembarque AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 5.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-5 AT ROW 2.33 COL 68
     "Filtrar" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.24 COL 69
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 141.6 BY 24.48.


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
         TITLE              = "Permisos de Embarque"
         HEIGHT             = 24.38
         WIDTH              = 141.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 165
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 165
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Permisos de Embarque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Permisos de Embarque */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
             INPUT  'd000permisosembarque.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000permisosembarqueUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000permisosembarque ).
       RUN repositionObject IN h_d000permisosembarque ( 1.71 , 131.00 ) NO-ERROR.
       /* Size in AB:  ( 1.81 , 10.80 ) */

       RUN constructObject (
             INPUT  'b000permisosembarque.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b000permisosembarque ).
       RUN repositionObject IN h_b000permisosembarque ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_b000permisosembarque ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsFilterHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 68.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 74.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_orden_entrega,id_permiso_embarque,id_aduana,anioOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsfecha0id_orden_entrega0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.43 , 69.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 5.57 , 71.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Permiso|Factura y Gastos|Cobranzas' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 7.67 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 17.62 , 141.00 ) NO-ERROR.

       /* Links to SmartDataObject h_d000permisosembarque. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_d000permisosembarque ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_d000permisosembarque ).

       /* Links to SmartDataBrowser h_b000permisosembarque. */
       RUN addLink ( h_d000permisosembarque , 'Data':U , h_b000permisosembarque ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'v000permisosembarque.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_v000permisosembarque ).
       RUN repositionObject IN h_v000permisosembarque ( 8.86 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 16.24 , 129.00 ) */

       /* Links to SmartDataViewer h_v000permisosembarque. */
       RUN addLink ( h_d000permisosembarque , 'Data':U , h_v000permisosembarque ).
       RUN addLink ( h_v000permisosembarque , 'Update':U , h_d000permisosembarque ).
       RUN addLink ( h_dyntoolbar , 'Tableio':U , h_v000permisosembarque ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'd000facturaspermisosembarque.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldspermisos_embarque.id_permiso_embarque,id_permiso_embarqueRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000facturaspermisosembarqueUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000facturaspermisosembarque ).
       RUN repositionObject IN h_d000facturaspermisosembarque ( 11.00 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'b000facturaspermisosembarque.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b000facturaspermisosembarque ).
       RUN repositionObject IN h_b000facturaspermisosembarque ( 8.86 , 39.00 ) NO-ERROR.
       RUN resizeObject IN h_b000facturaspermisosembarque ( 7.86 , 92.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'd000itemsventapermisosembarque.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_venta.id_punto_venta,id_punto_venta,subd_vtas.nro_comp,nro_compRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000itemsventapermisosembarqueUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000itemsventapermisosembarque ).
       RUN repositionObject IN h_d000itemsventapermisosembarque ( 14.57 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'b000itemsventapermisosembarque.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b000itemsventapermisosembarque ).
       RUN repositionObject IN h_b000itemsventapermisosembarque ( 16.95 , 82.00 ) NO-ERROR.
       RUN resizeObject IN h_b000itemsventapermisosembarque ( 7.86 , 49.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'd000gastosventapermisosembarque.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_gastos_permiso_embarque.id_permiso_embarque,id_permiso_embarque,r_gastos_permiso_embarque.anio,anioRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000gastosventapermisosembarqueUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000gastosventapermisosembarqu ).
       RUN repositionObject IN h_d000gastosventapermisosembarqu ( 12.67 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'b000gastosventapermisosembarque.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b000gastosventapermisosembarqu ).
       RUN repositionObject IN h_b000gastosventapermisosembarqu ( 16.95 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_b000gastosventapermisosembarqu ( 7.86 , 71.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 9.10 , 6.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 31.00 ) NO-ERROR.

       /* Links to SmartDataObject h_d000facturaspermisosembarque. */
       RUN addLink ( h_d000permisosembarque , 'Data':U , h_d000facturaspermisosembarque ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_d000facturaspermisosembarque ).

       /* Links to SmartDataBrowser h_b000facturaspermisosembarque. */
       RUN addLink ( h_d000facturaspermisosembarque , 'Data':U , h_b000facturaspermisosembarque ).

       /* Links to SmartDataObject h_d000itemsventapermisosembarque. */
       RUN addLink ( h_d000facturaspermisosembarque , 'Data':U , h_d000itemsventapermisosembarque ).

       /* Links to SmartDataBrowser h_b000itemsventapermisosembarque. */
       RUN addLink ( h_d000itemsventapermisosembarque , 'Data':U , h_b000itemsventapermisosembarque ).

       /* Links to SmartDataObject h_d000gastosventapermisosembarqu. */
       RUN addLink ( h_d000permisosembarque , 'Data':U , h_d000gastosventapermisosembarqu ).

       /* Links to SmartDataBrowser h_b000gastosventapermisosembarqu. */
       RUN addLink ( h_d000gastosventapermisosembarqu , 'Data':U , h_b000gastosventapermisosembarqu ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'd000cobranzaspermisosembarque.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_cobranzas_pe.id_permiso_embarque,id_permiso_embarqueRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000cobranzaspermisosembarqueUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000cobranzaspermisosembarque ).
       RUN repositionObject IN h_d000cobranzaspermisosembarque ( 9.33 , 5.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'b000cobranzaspermisosembarque.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b000cobranzaspermisosembarque ).
       RUN repositionObject IN h_b000cobranzaspermisosembarque ( 11.00 , 29.00 ) NO-ERROR.
       RUN resizeObject IN h_b000cobranzaspermisosembarque ( 7.86 , 89.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'v000cobranzaspermisosembarque.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_v000cobranzaspermisosembarque ).
       RUN repositionObject IN h_v000cobranzaspermisosembarque ( 19.10 , 39.00 ) NO-ERROR.
       /* Size in AB:  ( 5.48 , 70.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsCopyHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsCopyNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-3 ).
       RUN repositionObject IN h_dyntoolbar-3 ( 9.33 , 30.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-3 ( 1.24 , 88.00 ) NO-ERROR.

       /* Links to SmartDataObject h_d000cobranzaspermisosembarque. */
       RUN addLink ( h_d000permisosembarque , 'Data':U , h_d000cobranzaspermisosembarque ).
       RUN addLink ( h_dyntoolbar-3 , 'Navigation':U , h_d000cobranzaspermisosembarque ).

       /* Links to SmartDataBrowser h_b000cobranzaspermisosembarque. */
       RUN addLink ( h_d000cobranzaspermisosembarque , 'Data':U , h_b000cobranzaspermisosembarque ).

       /* Links to SmartDataViewer h_v000cobranzaspermisosembarque. */
       RUN addLink ( h_d000cobranzaspermisosembarque , 'Data':U , h_v000cobranzaspermisosembarque ).
       RUN addLink ( h_v000cobranzaspermisosembarque , 'Update':U , h_d000cobranzaspermisosembarque ).
       RUN addLink ( h_dyntoolbar-3 , 'TableIo':U , h_v000cobranzaspermisosembarque ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

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
  ENABLE RECT-5 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
DEF VAR xcColumns AS CHARACTER INITIAL "Name,Caption,Image,Type,OnChoose,AccessType,Parent".

&SCOP dlmt + CHR(1) +
  /*Boton de Impresion*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "PrintAction", 
                          "tlbPrint", 
                          "Imprimir", 
                          "print.bmp", 
                          "tlbPrint", 
                          "FUNCTION").

  RUN SUPER.

/* Code placed here will execute AFTER standard behavior. */
/*
/* create a button in toolbar for my action */
DYNAMIC-FUNCTION( 'createToolbar':U IN h_dyntoolbar, "myButtonAction" ).
*/
/* enable my action */
DYNAMIC-FUNCTION( 'enableActions' IN h_dyntoolbar, "PrintAction" ).

/* subscribe the container to act at my action */
SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.
RUN paintRows IN h_b000PermisosEmbarque.
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
  DEFINE VAR lista_reportes AS CHARACTER  NO-UNDO.
  DEFINE VAR cresult        AS CHARACTER  NO-UNDO.
  DEFINE VAR r              AS ROWID      NO-UNDO.
  DEFINE VAR v_contrato     AS CHAR       NO-UNDO.
  DEFINE VAR v_cantidad     AS INTEGER    NO-UNDO.
  DEFINE VAR v_lotes        AS CHARACTER  NO-UNDO.
  DEFINE VAR v_filtro       AS CHARACTER  NO-UNDO.
  DEFINE VAR v_fecha        AS CHARACTER  NO-UNDO.
  DEFINE VAR v_lote         AS CHARACTER  NO-UNDO.
  DEFINE VAR RB-MEMO-FILE   AS CHARACTER  NO-UNDO INITIAL "".
  DEFINE VAR v_semana_desde AS INTEGER    NO-UNDO.
  DEFINE VAR v_anio_desde   AS INTEGER    NO-UNDO.
  DEFINE VAR v_semana_hasta AS INTEGER    NO-UNDO.
  DEFINE VAR v_anio_hasta   AS INTEGER    NO-UNDO.
  
  lista_reportes = "Control PE total,Control PE total Facturas,Control PE rango semana,Cobranzas PE rango semana,PE Pendientes,Todos los PE c/fact,Todos los PE c/fact - Excell,Pedido de Derechos(Excell)".

  IF lista_reportes = "" THEN
      MESSAGE "No hay reportes disponibles" VIEW-AS ALERT-BOX.
  ELSE DO:
    RUN custom/support/cfun.w(INPUT lista_reportes, OUTPUT cresult).
      CASE cresult:
        WHEN "Control PE total" THEN DO:
          RUN p_calculo_control_pe.p.
          RUN p_reportes_9.p (INPUT "control_permisos_embarque",
                              INPUT "Reporte de PE",
                              INPUT "",
                              INPUT "").                        
        END.
        WHEN "Control PE total Facturas" THEN DO:
          RUN p_calculo_control_pe.p.
          RUN p_reportes_9.p (INPUT "control_pe_facturas",
                              INPUT "Reporte de PE",
                              INPUT "",
                              INPUT "").                        
        END.
        WHEN "Control PE rango semana" THEN DO:
          RUN wc_sel_rango_semana.w (OUTPUT v_semana_desde,
                                     OUTPUT v_anio_desde,
                                     OUTPUT v_semana_hasta,
                                     OUTPUT v_anio_hasta).
          IF v_semana_desde > 0 AND v_anio_desde > 0 AND v_semana_hasta > 0 AND v_anio_hasta > 0 THEN DO:
            RUN p_calculo_control_pe_semana.p (INPUT v_semana_desde,
                                               INPUT v_anio_desde,
                                               INPUT v_semana_hasta,
                                               INPUT v_anio_hasta).
            RUN p_reportes_9.p (INPUT "control_pe_facturas",
                                INPUT "Reporte de PE",
                                INPUT "",
                                INPUT "").                        
          END.
        END.
        WHEN "Cobranzas PE rango semana" THEN DO:
          RUN wc_sel_rango_semana.w (OUTPUT v_semana_desde,
                                     OUTPUT v_anio_desde,
                                     OUTPUT v_semana_hasta,
                                     OUTPUT v_anio_hasta).
            IF v_semana_desde > 0 AND v_anio_desde > 0 AND v_semana_hasta > 0 AND v_anio_hasta > 0 THEN DO:
              RUN p_cobranzas_pe_semana.p (INPUT v_semana_desde,
                                           INPUT v_anio_desde,
                                           INPUT v_semana_hasta,
                                           INPUT v_anio_hasta).
  
              RUN p_reportes_9.p (INPUT "control_pe_facturas_cobrar",
                                  INPUT "Reporte de Cobranzas de PE",
                                  INPUT "",
                                  INPUT "").                        
            END.                      
          END.
          WHEN "PE Pendientes" THEN DO:
            RUN p_pe_pendientes.p.
            RUN p_reportes_9.p (INPUT "control_pe_despachantes",
                                INPUT "Reporte PE Pendientes",
                                INPUT "",
                                INPUT "").                        
          END.
          WHEN "Todos los PE c/fact" THEN DO:
            RUN p_pe_pendientes_y_todos.p.
            RUN p_reportes_9.p (INPUT "control_pe_todos",
                                INPUT "Reporte PE Completo",
                                INPUT "",
                                INPUT "").                        
            END.
          WHEN "Todos los PE c/fact - Excell" THEN DO:
            RUN w_rep_excell_finanzas.w.                        
          END.
          WHEN "Pedido de Derechos(Excell)" THEN DO:
            RUN w_rep_excell_finanzas.w.                        
          END.
        END CASE.
      END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

