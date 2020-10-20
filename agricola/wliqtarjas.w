&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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

DEFINE INPUT PARAMETER p_sucursal AS INTEGER.


/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

{s_varsis.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-copias B-imprime RADIO-SET-4 B-actualizar ~
B-modificar B-eliminar 
&Scoped-Define DISPLAYED-OBJECTS FI-copias RADIO-SET-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bliqitemstarjas-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bliqtarjas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqitemstarjas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqtarjas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vliqitemstarjas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vliqtarjas AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-actualizar 
     LABEL "Actualizar" 
     SIZE 15 BY 1.19.

DEFINE BUTTON B-eliminar 
     LABEL "Eliminar" 
     SIZE 15 BY 1.19.

DEFINE BUTTON B-imprime 
     LABEL "Imprime Planilla" 
     SIZE 21 BY 1.19.

DEFINE BUTTON B-modificar 
     LABEL "Modificar" 
     SIZE 14 BY 1.19.

DEFINE VARIABLE FI-copias AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Nro copias" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.19 NO-UNDO.

DEFINE VARIABLE RADIO-SET-4 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "4 Items", 1,
"6 Items", 2,
"8 Items", 3
     SIZE 17 BY 2.14 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-copias AT ROW 1 COL 139 COLON-ALIGNED WIDGET-ID 86
     B-imprime AT ROW 1 COL 149.8 WIDGET-ID 8
     RADIO-SET-4 AT ROW 8.62 COL 56 NO-LABEL WIDGET-ID 76
     B-actualizar AT ROW 14.81 COL 29 WIDGET-ID 14
     B-modificar AT ROW 14.81 COL 46 WIDGET-ID 84
     B-eliminar AT ROW 14.81 COL 61 WIDGET-ID 16
     " Formato Impresi¢n" VIEW-AS TEXT
          SIZE 20 BY .95 AT ROW 7.43 COL 54 WIDGET-ID 82
          BGCOLOR 3 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.14
         SIZE 176.2 BY 31.43 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Captura planillas Tarjas"
         HEIGHT             = 31.57
         WIDTH              = 177.2
         MAX-HEIGHT         = 31.57
         MAX-WIDTH          = 217.6
         VIRTUAL-HEIGHT     = 31.57
         VIRTUAL-WIDTH      = 217.6
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Captura planillas Tarjas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Captura planillas Tarjas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-actualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-actualizar wWin
ON CHOOSE OF B-actualizar IN FRAME fMain /* Actualizar */
DO:
  RUN filtrar-planillas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-eliminar wWin
ON CHOOSE OF B-eliminar IN FRAME fMain /* Eliminar */
DO:
    DEF VAR X_empresa AS INTEGER.
  DEF VAR X_sucursal AS INTEGER.
  DEF VAR X_sector AS INTEGER.
  DEF VAR X_tipo AS INTEGER.
  DEF VAR X_nroplanilla AS INTEGER.
  DEF VAR X_fecha AS DATE.



x_empresa = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_empresa").
x_sucursal = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_sucursal").
x_sector = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_sector").
x_tipo = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_tipo_planilla").
x_nroplanilla = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "nro_planilla").
x_fecha = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "fecha").

  message "Esta seguro que desea eliminar la  planilla " + STRING(X_tipo) + " - " + STRING(x_nroplanilla) 
      view-as ALERT-BOX WARNING
      BUTTONS YES-NO
      UPDATE respuesta AS LOGICAL.    
 IF NOT respuesta  THEN RETURN.

/*

RUN pelimplanillas.p (INPUT X_empresa, INPUT X_sucursal, INPUT X_sector,
                     INPUT X_tipo, INPUT X_nroplanilla, INPUT X_fecha).
*/
RUN filtrar-planillas.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-imprime wWin
ON CHOOSE OF B-imprime IN FRAME fMain /* Imprime Planilla */
DO:
    DEF VAR X_empresa AS INTEGER.
    DEF VAR X_sucursal AS INTEGER.
    DEF VAR X_sector AS INTEGER.
    DEF VAR X_tipo AS INTEGER.
    DEF VAR X_nroplanilla AS INTEGER.
    DEF VAR x_items AS INTEGER NO-UNDO.

  x_empresa = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_empresa").
  x_sucursal = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_sucursal").
  x_sector = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_sector").
  x_tipo = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_tipo_planilla").
  x_nroplanilla = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "nro_planilla").

  x_items = INTEGER(radio-set-4:SCREEN-VALUE).


  RUN imprime-planilla ( INPUT x_empresa,
                       INPUT x_sucursal,
                       INPUT x_sector,
                       INPUT x_tipo,
                       INPUT x_nroplanilla,
                       INPUT X_items).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-modificar wWin
ON CHOOSE OF B-modificar IN FRAME fMain /* Modificar */
DO:
  DEF VAR X_empresa AS INTEGER.
  DEF VAR X_sucursal AS INTEGER.
  DEF VAR X_sector AS INTEGER.
  DEF VAR X_fecha AS DATE.
  DEF VAR X_proveedor AS INTEGER.
  DEF VAR X_origen AS INTEGER.
  DEF VAR X_grupo AS INTEGER.
  DEF VAR X_tipo AS INTEGER.

x_empresa = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_empresa").
x_sucursal = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_sucursal").
x_sector = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_sector").
x_fecha = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "fecha").
x_proveedor = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_proveedor").
x_origen = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_origen").
x_grupo = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_grupo").
x_tipo = DYNAMIC-FUNCTION("columnvalue" IN h_dliqtarjas, "id_tipo_planilla").

RUN wagregatarja.w (INPUT X_empresa, INPUT X_sucursal, INPUT X_sector,
                    INPUT X_fecha, INPUT X_proveedor, INPUT X_origen,   
                    INPUT x_grupo, INPUT X_tipo).
DYNAMIC-FUNCTION ('openQuery':U IN h_dliqitemstarjas). 


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
             INPUT  'bliqtarjas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bliqtarjas ).
       RUN repositionObject IN h_bliqtarjas ( 6.24 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bliqtarjas ( 8.10 , 49.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1':U) NO-ERROR.

       /* Links to SmartDataBrowser h_bliqtarjas. */
       RUN addLink ( h_dliqtarjas , 'Data':U , h_bliqtarjas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bliqtarjas ,
             B-imprime:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'dliqtarjas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedliqtarjasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqtarjas ).
       RUN repositionObject IN h_dliqtarjas ( 10.76 , 53.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'vliqtarjas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vliqtarjas ).
       RUN repositionObject IN h_vliqtarjas ( 2.43 , 75.00 ) NO-ERROR.
       /* Size in AB:  ( 10.48 , 101.00 ) */

       RUN constructObject (
             INPUT  'dliqitemstarjas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsliq_items_tarjas.id_empresa,id_empresa,liq_items_tarjas.id_sucursal,id_sucursal,liq_items_tarjas.id_sector,id_sector,liq_items_tarjas.fecha,fecha,liq_items_tarjas.id_proveedor,id_proveedor,liq_items_tarjas.id_origen,id_origen,liq_items_tarjas.id_grupo,id_grupo,liq_items_tarjas.id_tipo_planilla,id_tipo_planillaObjectNamedliqitemstarjasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqitemstarjas ).
       RUN repositionObject IN h_dliqitemstarjas ( 11.71 , 64.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'vliqitemstarjas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vliqitemstarjas ).
       RUN repositionObject IN h_vliqitemstarjas ( 14.81 , 80.00 ) NO-ERROR.
       /* Size in AB:  ( 16.19 , 97.00 ) */

       RUN constructObject (
             INPUT  'bliqitemstarjas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bliqitemstarjas-2 ).
       RUN repositionObject IN h_bliqitemstarjas-2 ( 16.48 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bliqitemstarjas-2 ( 14.05 , 74.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_sector,id_empresa,fechaOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsfecha0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.00 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.29 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsCopyFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,TableioTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsCopyHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 71.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 58.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,TableioTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNamedyntoolbarDisabledActionsHiddenActionsRESETHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-1 ).
       RUN repositionObject IN h_dyntoolbar-1 ( 13.38 , 99.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-1 ( 1.52 , 68.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dliqtarjas. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dliqtarjas ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dliqtarjas ).

       /* Links to SmartDataViewer h_vliqtarjas. */
       RUN addLink ( h_dliqtarjas , 'Data':U , h_vliqtarjas ).
       RUN addLink ( h_vliqtarjas , 'Update':U , h_dliqtarjas ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vliqtarjas ).

       /* Links to SmartDataObject h_dliqitemstarjas. */
       RUN addLink ( h_dliqtarjas , 'Data':U , h_dliqitemstarjas ).
       RUN addLink ( h_dyntoolbar-1 , 'Navigation':U , h_dliqitemstarjas ).

       /* Links to SmartDataViewer h_vliqitemstarjas. */
       RUN addLink ( h_dliqitemstarjas , 'Data':U , h_vliqitemstarjas ).
       RUN addLink ( h_vliqitemstarjas , 'Update':U , h_dliqitemstarjas ).
       RUN addLink ( h_dyntoolbar-1 , 'Tableio':U , h_vliqitemstarjas ).

       /* Links to SmartDataBrowser h_bliqitemstarjas-2. */
       RUN addLink ( h_dliqitemstarjas , 'Data':U , h_bliqitemstarjas-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynfilter ,
             FI-copias:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_dyntoolbar ,
             h_dynfilter , 'AFTER':U ).
       RUN adjustTabOrder ( h_vliqtarjas ,
             B-imprime:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar-1 ,
             RADIO-SET-4:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_vliqitemstarjas ,
             B-eliminar:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_bliqitemstarjas-2 ,
             h_vliqitemstarjas , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-sucursal wWin 
PROCEDURE devuelve-sucursal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER v-suc AS INTEGER.
v-suc = p_sucursal.
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
  DISPLAY FI-copias RADIO-SET-4 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE FI-copias B-imprime RADIO-SET-4 B-actualizar B-modificar B-eliminar 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filtrar-planillas wWin 
PROCEDURE filtrar-planillas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR cQuery AS CHARACTER.
DEFINE VAR xQuery AS CHARACTER. 
DEF VAR vtipo AS INTEGER.
DEF VAR vfechacierre AS DATE.

vtipo = 1.

FIND FIRST par_agricola WHERE par_agricola.id_usuario = 20 NO-LOCK NO-ERROR.
IF NOT AVAILABLE par_agricola THEN NEXT.
vfechacierre = par_agricola.fecha_cierre.

  
  cQuery = "liq_tarjas.id_sucursal = " + STRING(p_sucursal) + 
           " and liq_tarjas.fecha > date('" + string(vfechacierre) + "')" +
           " and liq_tarjas.id_tipo_planilla = " + STRING(vtipo).

  xQuery = "FOR EACH liq_tarjas WHERE " + cQuery + " NO-LOCK " + 
           " by liq_tarjas.nro_planilla INDEXED-REPOSITION ".



     {set queryWhere xQuery h_dliqtarjas}.
      DYNAMIC-FUNCTION ('openQuery':U IN h_dliqtarjas). 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime-planilla wWin 
PROCEDURE imprime-planilla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p_empresa AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_sucursal AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_sector AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_tipo AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_nro AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_items AS INTEGER NO-UNDO.
  
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "" NO-UNDO.
  DEFINE VAR v_filtro AS CHARACTER NO-UNDO.
  DEFINE VAR v-archivo AS CHARACTER NO-UNDO.
  DEFINE VAR v-copias AS INTEGER NO-UNDO.
            
            v-copias = INTEGER(fi-copias:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

            RB-MEMO-FILE  = vlc_dir_spool + string(next-value(next-spool),"99999999").
            
            
            v_filtro = "liq_tarjas.id_empresa = " + STRING(p_empresa) +
                       " and liq_tarjas.id_sucursal = " + STRING(p_sucursal) + 
                       " and liq_tarjas.id_sector = " + STRING(p_sector) + 
                       " and liq_tarjas.id_tipo_planilla = " + STRING(p_tipo) + 
                       " and liq_tarjas.nro_planilla = " + STRING(p_nro).
            

            CASE p_items:
              WHEN 1 THEN
                  v-archivo = "planilla_liqtarja-01".
              WHEN 2 THEN
                  MESSAGE "Opci¢n no habilitada" VIEW-AS ALERT-BOX WARNING.
                 /* v-archivo = "planilla_personal-01a". */
              WHEN 3 THEN
                  MESSAGE "Opci¢n no habilitada" VIEW-AS ALERT-BOX WARNING.
                 /* v-archivo = "planilla_personal-01b".*/
            END CASE.

            vlc_dir_fuentes = "z:\sistemas\sami\sistemas\".
            RUN  aderb\_prntrb2(
                 (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
                 v-archivo,                    /* RB-REPORT-NAME */
                 "",                             /* RB-DB-CONNECTION */
                 "O",                             /* RB-INCLUDE-RECORDS */
                 v_filtro,                              /* RB-FILTER */
                 RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                 "?",                             /* RB-PRINT-DESTINATION */
                 "?",  /* RB-PRINTER-NAME */
                 "",                              /* RB-PRINTER-PORT */
                 "",                              /* RB-OUTPUT-FILE */
                  v-copias,                              /* RB-NUMBER-COPIES  - zero */                  
                  0,                              /* RB-BEGIN-PAGE - zero */
                  0,                              /* RB-END-PAGE - zero */
                 no,                              /* RB-TEST-PATTERN */
                 "Planilla Tarja Personal",         /* RB-WINDOW-TITLE */
                 yes,                           /* RB-DISPLAY-ERRORS */
                 yes,                           /* RB-DISPLAY-STATUS */
                 no,                              /* RB-NO-WAIT */
                 "", /* RB-OTHER-PARAMETERS */
                 ""
                 ).   

            OS-DELETE VALUE(RB-MEMO-FILE). 





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

  RUN filtrar-planillas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectPage wWin 
PROCEDURE selectPage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER piPageNum AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT piPageNum).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

