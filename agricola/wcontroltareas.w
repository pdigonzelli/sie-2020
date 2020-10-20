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
DEFINE INPUT PARAMETER p_sector AS INTEGER.
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
&Scoped-Define ENABLED-OBJECTS B-imprime RADIO-SET-1 B-filtrar B-cargar ~
B-actualizar B-eliminar B-agregar FI-horainicio FI-horafin B-cargar-2 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 FI-horainicio FI-horafin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bcontroltareas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemscontroltareas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcontroltareas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemscontroltareas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcontroltareas-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vitemscontroltareas AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-actualizar 
     LABEL "Actualizar" 
     SIZE 15 BY 1.19.

DEFINE BUTTON B-agregar 
     LABEL "Modificar Planilla" 
     SIZE 19 BY 1.19.

DEFINE BUTTON B-cargar 
     LABEL "1er Intervalo" 
     SIZE 20 BY 1.14.

DEFINE BUTTON B-cargar-2 
     LABEL "2do Intervalo" 
     SIZE 20 BY 1.14.

DEFINE BUTTON B-eliminar 
     LABEL "Eliminar Planilla" 
     SIZE 19 BY 1.19.

DEFINE BUTTON B-filtrar 
     LABEL "Filtrar" 
     SIZE 17 BY 1.43.

DEFINE BUTTON B-imprime 
     LABEL "Imprime Planilla" 
     SIZE 21 BY 1.19.

DEFINE VARIABLE FI-horafin AS CHARACTER FORMAT "99:99:99":U 
     LABEL "Hora Fin" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-horainicio AS CHARACTER FORMAT "99:99:99":U 
     LABEL "Hora Inicio" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Planilla 1", 1,
"Planilla 2", 2,
"Planilla 3", 3,
"Planilla 4", 4
     SIZE 19 BY 3.81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-imprime AT ROW 1 COL 168 WIDGET-ID 8
     RADIO-SET-1 AT ROW 1.95 COL 67 NO-LABEL WIDGET-ID 18
     B-filtrar AT ROW 6.24 COL 66 WIDGET-ID 22
     B-cargar AT ROW 13.38 COL 172 WIDGET-ID 6
     B-actualizar AT ROW 13.62 COL 35 WIDGET-ID 14
     B-eliminar AT ROW 13.62 COL 53 WIDGET-ID 16
     B-agregar AT ROW 13.62 COL 74 WIDGET-ID 12
     FI-horainicio AT ROW 13.86 COL 111 COLON-ALIGNED WIDGET-ID 2
     FI-horafin AT ROW 13.86 COL 139 COLON-ALIGNED WIDGET-ID 4
     B-cargar-2 AT ROW 14.76 COL 172 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.14
         SIZE 195.2 BY 31.43 WIDGET-ID 100.


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
         TITLE              = "Captura planillas"
         HEIGHT             = 31.57
         WIDTH              = 196
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
ON END-ERROR OF wWin /* Captura planillas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Captura planillas */
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


&Scoped-define SELF-NAME B-agregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-agregar wWin
ON CHOOSE OF B-agregar IN FRAME fMain /* Modificar Planilla */
DO:
  DEF VAR X_empresa AS INTEGER.
  DEF VAR X_sucursal AS INTEGER.
  DEF VAR X_sector AS INTEGER.
  DEF VAR X_tipo AS INTEGER.
  DEF VAR X_nroplanilla AS INTEGER.
  DEF VAR X_fecha AS DATE.

x_empresa = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_empresa").
x_sucursal = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_sucursal").
x_sector = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_sector").
x_tipo = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_tipo_planilla").
x_nroplanilla = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "nro_planilla").
x_fecha = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "fecha").

RUN wagregaplanilla.w (INPUT X_empresa, INPUT X_sucursal, INPUT X_sector,
                       INPUT X_tipo, INPUT X_nroplanilla, INPUT X_fecha).
DYNAMIC-FUNCTION ('openQuery':U IN h_ditemscontroltareas). 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-cargar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-cargar wWin
ON CHOOSE OF B-cargar IN FRAME fMain /* 1er Intervalo */
DO:
  DEF VAR xRow AS ROWID NO-UNDO.
  DEF VAR v_hora_inicio AS CHARACTER NO-UNDO.
  DEF VAR v_hora_fin AS CHARACTER NO-UNDO.

  xRow = DYNAMIC-FUNCTION('getrowid':U IN h_dcontroltareas).
  IF xRow = ? THEN RETURN.

  v_hora_inicio = fi-horainicio:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  v_hora_fin = fi-horafin:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  RUN p_cargar_horario.p ( INPUT 1, INPUT xRow, INPUT v_hora_inicio, INPUT v_hora_fin).
  DYNAMIC-FUNCTION ('openQuery':U IN h_ditemscontroltareas). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-cargar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-cargar-2 wWin
ON CHOOSE OF B-cargar-2 IN FRAME fMain /* 2do Intervalo */
DO:
  DEF VAR xRow AS ROWID NO-UNDO.
  DEF VAR v_hora_inicio AS CHARACTER NO-UNDO.
  DEF VAR v_hora_fin AS CHARACTER NO-UNDO.

  xRow = DYNAMIC-FUNCTION('getrowid':U IN h_dcontroltareas).
  IF xRow = ? THEN RETURN.

  v_hora_inicio = fi-horainicio:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  v_hora_fin = fi-horafin:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  RUN p_cargar_horario.p (INPUT 2, INPUT xRow, INPUT v_hora_inicio, INPUT v_hora_fin).
  DYNAMIC-FUNCTION ('openQuery':U IN h_ditemscontroltareas). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-eliminar wWin
ON CHOOSE OF B-eliminar IN FRAME fMain /* Eliminar Planilla */
DO:
    DEF VAR X_empresa AS INTEGER.
  DEF VAR X_sucursal AS INTEGER.
  DEF VAR X_sector AS INTEGER.
  DEF VAR X_tipo AS INTEGER.
  DEF VAR X_nroplanilla AS INTEGER.
  DEF VAR X_fecha AS DATE.



x_empresa = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_empresa").
x_sucursal = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_sucursal").
x_sector = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_sector").
x_tipo = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_tipo_planilla").
x_nroplanilla = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "nro_planilla").
x_fecha = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "fecha").

  message "Esta seguro que desea eliminar la  planilla " + STRING(X_tipo) + " - " + STRING(x_nroplanilla) 
      view-as ALERT-BOX WARNING
      BUTTONS YES-NO
      UPDATE respuesta AS LOGICAL.    
 IF NOT respuesta  THEN RETURN.



RUN pelimplanillas.p (INPUT X_empresa, INPUT X_sucursal, INPUT X_sector,
                     INPUT X_tipo, INPUT X_nroplanilla, INPUT X_fecha).

RUN filtrar-planillas.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-filtrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-filtrar wWin
ON CHOOSE OF B-filtrar IN FRAME fMain /* Filtrar */
DO:
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

  x_empresa = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_empresa").
  x_sucursal = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_sucursal").
  x_sector = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_sector").
  x_tipo = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "id_tipo_planilla").
  x_nroplanilla = DYNAMIC-FUNCTION("columnvalue" IN h_dcontroltareas, "nro_planilla").

  RUN imprime-planilla ( INPUT x_empresa,
                       INPUT x_sucursal,
                       INPUT x_sector,
                       INPUT x_tipo,
                       INPUT x_nroplanilla).


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
             INPUT  'bcontroltareas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bcontroltareas ).
       RUN repositionObject IN h_bcontroltareas ( 1.91 , 5.00 ) NO-ERROR.
       RUN resizeObject IN h_bcontroltareas ( 10.95 , 57.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1':U) NO-ERROR.

       /* Links to SmartDataBrowser h_bcontroltareas. */
       RUN addLink ( h_dcontroltareas , 'Data':U , h_bcontroltareas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bcontroltareas ,
             B-imprime:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'dcontroltareas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcontroltareasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcontroltareas ).
       RUN repositionObject IN h_dcontroltareas ( 8.38 , 82.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'vcontroltareas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcontroltareas-2 ).
       RUN repositionObject IN h_vcontroltareas-2 ( 2.67 , 95.00 ) NO-ERROR.
       /* Size in AB:  ( 10.48 , 94.00 ) */

       RUN constructObject (
             INPUT  'ditemscontroltareas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsitems_control_tareas.id_empresa,id_empresa,items_control_tareas.id_sucursal,id_sucursal,items_control_tareas.id_sector,id_sector,items_control_tareas.fecha,fecha,items_control_tareas.id_proveedor,id_proveedor,items_control_tareas.id_origen,id_origen,items_control_tareas.id_grupo,id_grupo,items_control_tareas.id_tipo_planilla,id_tipo_planillaObjectNameditemscontroltareasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ditemscontroltareas ).
       RUN repositionObject IN h_ditemscontroltareas ( 9.81 , 76.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemscontroltareas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemscontroltareas ).
       RUN repositionObject IN h_bitemscontroltareas ( 15.29 , 2.20 ) NO-ERROR.
       RUN resizeObject IN h_bitemscontroltareas ( 14.29 , 97.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vitemscontroltareas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vitemscontroltareas ).
       RUN repositionObject IN h_vitemscontroltareas ( 17.10 , 101.00 ) NO-ERROR.
       /* Size in AB:  ( 14.33 , 91.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsAdd,Copy,DeleteFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,TableioTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsAdd,Copy,DeleteHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 95.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 67.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsAddFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNamedyntoolbarDisabledActionsAddHiddenActionsRESET,UPDATEHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-1 ).
       RUN repositionObject IN h_dyntoolbar-1 ( 15.14 , 101.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-1 ( 1.52 , 68.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dcontroltareas. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dcontroltareas ).

       /* Links to SmartDataViewer h_vcontroltareas-2. */
       RUN addLink ( h_dcontroltareas , 'Data':U , h_vcontroltareas-2 ).
       RUN addLink ( h_vcontroltareas-2 , 'Update':U , h_dcontroltareas ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vcontroltareas-2 ).

       /* Links to SmartDataObject h_ditemscontroltareas. */
       RUN addLink ( h_dcontroltareas , 'Data':U , h_ditemscontroltareas ).
       RUN addLink ( h_dyntoolbar-1 , 'Navigation':U , h_ditemscontroltareas ).

       /* Links to SmartDataBrowser h_bitemscontroltareas. */
       RUN addLink ( h_ditemscontroltareas , 'Data':U , h_bitemscontroltareas ).

       /* Links to SmartDataViewer h_vitemscontroltareas. */
       RUN addLink ( h_ditemscontroltareas , 'Data':U , h_vitemscontroltareas ).
       RUN addLink ( h_vitemscontroltareas , 'Update':U , h_ditemscontroltareas ).
       RUN addLink ( h_dyntoolbar-1 , 'Tableio':U , h_vitemscontroltareas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             B-imprime:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_vcontroltareas-2 ,
             RADIO-SET-1:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar-1 ,
             B-cargar-2:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_bitemscontroltareas ,
             h_dyntoolbar-1 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vitemscontroltareas ,
             h_bitemscontroltareas , 'AFTER':U ).
    END. /* Page 1 */

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
  DISPLAY RADIO-SET-1 FI-horainicio FI-horafin 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE B-imprime RADIO-SET-1 B-filtrar B-cargar B-actualizar B-eliminar 
         B-agregar FI-horainicio FI-horafin B-cargar-2 
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

vtipo = INTEGER(radio-set-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

FIND FIRST par_agricola WHERE par_agricola.id_usuario = 20 NO-LOCK NO-ERROR.
IF NOT AVAILABLE par_agricola THEN NEXT.

  
  cQuery = "control_tareas.id_sucursal = " + STRING(p_sucursal) + 
           " and control_tareas.id_sector = " + STRING(p_sector) + 
           " and control_tareas.fecha > date('" + string(par_agricola.fecha_cierre) + "')" +
           " and control_tareas.id_tipo_planilla = " + STRING(vtipo).

  xQuery = "FOR EACH control_tareas WHERE " + cQuery + " NO-LOCK, " + 
           "FIRST proveedores WHERE proveedores.id_proveedor = control_tareas.id_empresa OUTER-JOIN NO-LOCK," +
           "FIRST sectores_agricolas OF control_tareas OUTER-JOIN NO-LOCK," +
           "FIRST sucursales OF control_tareas OUTER-JOIN NO-LOCK by control_tareas.nro_planilla INDEXED-REPOSITION ".



     {set queryWhere xQuery h_dcontroltareas}.
      DYNAMIC-FUNCTION ('openQuery':U IN h_dcontroltareas). 


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
  
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "" NO-UNDO.
  DEFINE VAR v_filtro AS CHARACTER NO-UNDO.

            
            

            RB-MEMO-FILE  = vlc_dir_spool + string(next-value(next-spool),"99999999").
            
            
            v_filtro = "control_tareas.id_empresa = " + STRING(p_empresa) +
                       " and control_tareas.id_sucursal = " + STRING(p_sucursal) + 
                       " and control_tareas.id_sector = " + STRING(p_sector) + 
                       " and control_tareas.id_tipo_planilla = " + STRING(p_tipo) + 
                       " and control_tareas.nro_planilla = " + STRING(p_nro).
            
            RUN  aderb\_prntrb2(
                 (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
                 "planilla_personal-01",                    /* RB-REPORT-NAME */
                 "",                             /* RB-DB-CONNECTION */
                 "O",                             /* RB-INCLUDE-RECORDS */
                 v_filtro,                              /* RB-FILTER */
                 RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                 "?",                             /* RB-PRINT-DESTINATION */
                 "?",  /* RB-PRINTER-NAME */
                 "",                              /* RB-PRINTER-PORT */
                 "",                              /* RB-OUTPUT-FILE */
                  0,                              /* RB-NUMBER-COPIES  - zero */                  
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

