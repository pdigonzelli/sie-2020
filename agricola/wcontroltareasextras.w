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

DEFINE INPUT PARAMETER p_empresa AS INTEGER.
DEFINE INPUT PARAMETER p_suc AS INTEGER.
DEFINE INPUT PARAMETER p_sector AS INTEGER.
DEFINE INPUT PARAMETER p_tipo_planilla AS INTEGER.
DEFINE INPUT PARAMETER p_nro_planilla AS INTEGER.
DEFINE INPUT PARAMETER p_fecha AS DATE.


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
&Scoped-Define ENABLED-OBJECTS B-imprimir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bitemscontroltareasextras-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcontroltareas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemscontroltareasextras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vcontroltareas-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vitemscontroltareasextras AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-imprimir 
     LABEL "Imprimir" 
     SIZE 18 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-imprimir AT ROW 14.33 COL 53 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.14
         SIZE 173.8 BY 18.1 WIDGET-ID 100.


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
         TITLE              = "Conformacion planilla  HS. EXTRAS"
         HEIGHT             = 18.24
         WIDTH              = 174.2
         MAX-HEIGHT         = 29.33
         MAX-WIDTH          = 217.6
         VIRTUAL-HEIGHT     = 29.33
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
ON END-ERROR OF wWin /* Conformacion planilla  HS. EXTRAS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Conformacion planilla  HS. EXTRAS */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-imprimir wWin
ON CHOOSE OF B-imprimir IN FRAME fMain /* Imprimir */
DO:

    RUN imprime-planilla ( INPUT p_empresa,
                          INPUT p_suc,
                          INPUT p_sector,
                          INPUT p_tipo_planilla,
                          INPUT p_nro_planilla).

  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualiza-items wWin 
PROCEDURE actualiza-items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      DYNAMIC-FUNCTION ('openQuery':U IN h_ditemscontroltareasextras). 

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
             INPUT  'dcontroltareas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedcontroltareasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dcontroltareas ).
       RUN repositionObject IN h_dcontroltareas ( 13.86 , 18.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       /* Initialize other pages that this page requires. */
       RUN initPages ('1':U) NO-ERROR.

       /* Links to SmartDataObject h_dcontroltareas. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dcontroltareas ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vcontroltareas.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vcontroltareas-2 ).
       RUN repositionObject IN h_vcontroltareas-2 ( 2.67 , 7.00 ) NO-ERROR.
       /* Size in AB:  ( 10.48 , 94.00 ) */

       RUN constructObject (
             INPUT  'ditemscontroltareasextras.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsitems_control_tareas.id_empresa,id_empresa,items_control_tareas.id_sucursal,id_sucursal,items_control_tareas.id_sector,id_sector,items_control_tareas.fecha,fecha,items_control_tareas.id_proveedor,id_proveedor,items_control_tareas.id_origen,id_origen,items_control_tareas.id_grupo,id_grupo,items_control_tareas.id_tipo_planilla,id_tipo_planillaObjectNameditemscontroltareasextrasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ditemscontroltareasextras ).
       RUN repositionObject IN h_ditemscontroltareasextras ( 14.10 , 35.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemscontroltareasextras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemscontroltareasextras-2 ).
       RUN repositionObject IN h_bitemscontroltareasextras-2 ( 2.43 , 104.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemscontroltareasextras-2 ( 9.05 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vitemscontroltareasextras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vitemscontroltareasextras ).
       RUN repositionObject IN h_vitemscontroltareasextras ( 14.10 , 78.00 ) NO-ERROR.
       /* Size in AB:  ( 4.10 , 94.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsAdd,Copy,DeleteFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsAdd,Copy,DeleteHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 7.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 67.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeSaveSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNamedyntoolbarDisabledActionsHiddenActionsRESET,UPDATEHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-1 ).
       RUN repositionObject IN h_dyntoolbar-1 ( 11.95 , 104.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-1 ( 1.52 , 68.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vcontroltareas-2. */
       RUN addLink ( h_dcontroltareas , 'Data':U , h_vcontroltareas-2 ).
       RUN addLink ( h_vcontroltareas-2 , 'Update':U , h_dcontroltareas ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vcontroltareas-2 ).

       /* Links to SmartDataObject h_ditemscontroltareasextras. */
       RUN addLink ( h_dcontroltareas , 'Data':U , h_ditemscontroltareasextras ).
       RUN addLink ( h_dyntoolbar-1 , 'Navigation':U , h_ditemscontroltareasextras ).

       /* Links to SmartDataBrowser h_bitemscontroltareasextras-2. */
       RUN addLink ( h_ditemscontroltareasextras , 'Data':U , h_bitemscontroltareasextras-2 ).

       /* Links to SmartDataViewer h_vitemscontroltareasextras. */
       RUN addLink ( h_ditemscontroltareasextras , 'Data':U , h_vitemscontroltareasextras ).
       RUN addLink ( h_vitemscontroltareasextras , 'Update':U , h_ditemscontroltareasextras ).
       RUN addLink ( h_dyntoolbar-1 , 'Tableio':U , h_vitemscontroltareasextras ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             B-imprimir:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_bitemscontroltareasextras-2 ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_vcontroltareas-2 ,
             h_bitemscontroltareasextras-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar-1 ,
             h_vcontroltareas-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vitemscontroltareasextras ,
             h_dcontroltareas , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-nro-planilla wWin 
PROCEDURE devuelve-nro-planilla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER v_nro_planilla AS INTEGER NO-UNDO.

v_nro_planilla = p_nro_planilla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-total-extras wWin 
PROCEDURE devuelve-total-extras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER p_nro AS INTEGER.
DEF INPUT PARAMETER p_empresa AS INTEGER.
DEF INPUT PARAMETER p_legajo AS INTEGER.
DEFINE OUTPUT PARAMETER p_total AS INTEGER.

DEF VAR v_desde AS DATE.
DEF VAR v_hasta AS DATE.


DEFINE BUFFER b_control FOR CONTROL_tareas.
DEFINE BUFFER b_items FOR items_CONTROL_tareas.

v_desde = DATE("01/" + STRING(MONTH(p_fecha),"99") + "/" + STRING(YEAR(p_fecha),"9999")).

FOR EACH b_items WHERE b_items.fecha >= v_desde AND
    b_items.fecha <= p_fecha AND b_items.id_empresa = p_empresa AND b_items.legajo = p_legajo NO-LOCK, FIRST b_control OF b_items WHERE
     b_control.nro_planilla <> p_nro NO-LOCK:
    p_total = p_total + b_items.cant_hs_extras.
END.

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
  ENABLE B-imprimir 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filtrar-planilla wWin 
PROCEDURE filtrar-planilla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR cQuery AS CHARACTER.
DEFINE VAR xQuery AS CHARACTER. 


  
  cQuery = "control_tareas.id_sucursal = " + STRING(p_suc) + 
           " and control_tareas.id_sector = " + STRING(p_sector) + 
           " and control_tareas.id_tipo_planilla = " + STRING(p_tipo_planilla) + 
           " and control_tareas.nro_planilla = " + STRING(p_nro_planilla). 

  xQuery = "FOR EACH control_tareas WHERE " + cQuery + " NO-LOCK, " + 
           "FIRST proveedores WHERE proveedores.id_proveedor = control_tareas.id_empresa OUTER-JOIN NO-LOCK," +
           "FIRST sectores_agricolas OF control_tareas OUTER-JOIN NO-LOCK," +
           "FIRST sucursales OF control_tareas OUTER-JOIN NO-LOCK by control_tareas.fecha INDEXED-REPOSITION ".



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
  DEF INPUT PARAMETER v_empresa AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER v_sucursal AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER v_sector AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER v_tipo AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER v_nro AS INTEGER NO-UNDO.
  
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "" NO-UNDO.
  DEFINE VAR v_filtro AS CHARACTER NO-UNDO.

            
            

            RB-MEMO-FILE  = vlc_dir_spool + string(next-value(next-spool),"99999999").
            
            v_filtro = "control_tareas.id_empresa = " + STRING(v_empresa) +
                       " and control_tareas.id_sucursal = " + STRING(v_sucursal) + 
                       " and control_tareas.id_sector = " + STRING(v_sector) + 
                       " and control_tareas.id_tipo_planilla = " + STRING(v_tipo) + 
                       " and control_tareas.nro_planilla = " + STRING(v_nro).
            
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

  RUN filtrar-planilla.
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

