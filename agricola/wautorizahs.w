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
DEF INPUT PARAMETER p_suc AS INTEGER.


/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}
{s_inicio.i }

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
&Scoped-Define ENABLED-OBJECTS b-imprime FI-desde B-filtrar B-control ~
FI-hasta 
&Scoped-Define DISPLAYED-OBJECTS FI-desde FI-hasta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bliqitemstarjas01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqitemstarjas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vautorizahs AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-control 
     LABEL "Para Control" 
     SIZE 16 BY 1.19.

DEFINE BUTTON B-filtrar 
     LABEL "Filtrar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b-imprime 
     LABEL "Para Firmar" 
     SIZE 16 BY 1.19.

DEFINE VARIABLE FI-desde AS DATE FORMAT "99/99/99":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-hasta AS DATE FORMAT "99/99/99":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     b-imprime AT ROW 1.24 COL 127 WIDGET-ID 2
     FI-desde AT ROW 1.71 COL 25 COLON-ALIGNED WIDGET-ID 4
     B-filtrar AT ROW 1.71 COL 51 WIDGET-ID 8
     B-control AT ROW 2.67 COL 127 WIDGET-ID 10
     FI-hasta AT ROW 2.95 COL 25 COLON-ALIGNED WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.6 BY 18.91 WIDGET-ID 100.


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
         TITLE              = "Autorizaci¢n Hs Adicionales"
         HEIGHT             = 18.91
         WIDTH              = 151.6
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 151.6
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 151.6
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
ON END-ERROR OF wWin /* Autorizaci¢n Hs Adicionales */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Autorizaci¢n Hs Adicionales */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-control
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-control wWin
ON CHOOSE OF B-control IN FRAME fMain /* Para Control */
DO:
  RUN imprime-control.
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


&Scoped-define SELF-NAME b-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-imprime wWin
ON CHOOSE OF b-imprime IN FRAME fMain /* Para Firmar */
DO:
  RUN imprime-reporte.
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
             INPUT  'dliqitemstarjas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedliqitemstarjasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqitemstarjas ).
       RUN repositionObject IN h_dliqitemstarjas ( 1.24 , 105.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bliqitemstarjas01.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bliqitemstarjas01 ).
       RUN repositionObject IN h_bliqitemstarjas01 ( 4.33 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_bliqitemstarjas01 ( 9.29 , 144.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vautorizahs.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vautorizahs ).
       RUN repositionObject IN h_vautorizahs ( 16.00 , 44.00 ) NO-ERROR.
       /* Size in AB:  ( 1.52 , 37.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsAdd,Copy,DeleteFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,TableioTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsAdd,Copy,DeleteHiddenActionsReset,UndoChangeHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 14.33 , 36.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 67.20 ) NO-ERROR.

       /* Links to SmartDataObject h_dliqitemstarjas. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dliqitemstarjas ).

       /* Links to SmartDataBrowser h_bliqitemstarjas01. */
       RUN addLink ( h_dliqitemstarjas , 'Data':U , h_bliqitemstarjas01 ).

       /* Links to SmartDataViewer h_vautorizahs. */
       RUN addLink ( h_dliqitemstarjas , 'Data':U , h_vautorizahs ).
       RUN addLink ( h_vautorizahs , 'Update':U , h_dliqitemstarjas ).
       RUN addLink ( h_dyntoolbar , 'Tableio':U , h_vautorizahs ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bliqitemstarjas01 ,
             FI-hasta:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar ,
             h_bliqitemstarjas01 , 'AFTER':U ).
       RUN adjustTabOrder ( h_vautorizahs ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY FI-desde FI-hasta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE b-imprime FI-desde B-filtrar B-control FI-hasta 
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
DEF VAR vfechacierre AS DATE.
DEF VAR vfechadesde AS DATE.
DEF VAR vfechahasta AS DATE.

/*FIND FIRST par_agricola WHERE par_agricola.id_usuario = 20 NO-LOCK NO-ERROR.
IF NOT AVAILABLE par_agricola THEN NEXT.
vfechacierre = par_agricola.fecha_cierre.

vfechacierre = DATE("31/08/2013"). */

  vfechadesde = DATE(fi-desde:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  vfechahasta = DATE(fi-hasta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  
  cQuery = "liq_items_tarjas.id_sucursal = " + STRING(p_suc) +
           " and liq_items_tarjas.fecha >= date('" + string(vfechadesde) + "')" +
           " and liq_items_tarjas.fecha <= date('" + STRING(vfechahasta) + "')" +
           " and liq_items_tarjas.hs_adicionales_tareas_trabajadas <> 0".

  xQuery = "FOR EACH liq_items_tarjas WHERE " + cQuery + " NO-LOCK, " + 
      "FIRST liq_tareas OF liq_items_tarjas OUTER-JOIN NO-LOCK," +
      " FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_tarjas.id_empresa" +
      " AND liq_legajos.legajo = liq_items_tarjas.legajo OUTER-JOIN NO-LOCK INDEXED-REPOSITION".

     {set queryWhere xQuery h_dliqitemstarjas}.
      DYNAMIC-FUNCTION ('openQuery':U IN h_dliqitemstarjas). 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime-control wWin 
PROCEDURE imprime-control :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  define var v_filtro as character initial "".
  define var v_general as character.
  DEF VAR vfechadesde AS DATE.
  DEF VAR vfechahasta AS DATE.


   vfechadesde = DATE(fi-desde:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   vfechahasta = DATE(fi-hasta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  /* vlc_dir_fuentes = "z:\sistemas\sami\sistemas\".*/

        v_filtro = "liq_items_tarjas.id_sucursal = " + STRING(p_suc) +
                   " and liq_items_tarjas.fecha >= date('" + STRING(vfechadesde) + "')" +
                   " and liq_items_tarjas.fecha <= date('" + STRING(vfechahasta) + "')" +
                   " and liq_items_tarjas.hs_adicionales_tareas_trabajadas <> 0".
        
        RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.
        
         RUN  aderb\_prntrb2(
               (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
               "tarja_autoriza_hs_control", /* RB-REPORT-NAME */
               "",                             /* RB-DB-CONNECTION */
               "O",                             /* RB-INCLUDE-RECORDS */
               v_filtro,                              /* RB-FILTER */
               RB-MEMO-FILE,                              /* RB-MEMO-FILE */
               "D",                             /* RB-PRINT-DESTINATION */
               "?",                              /* RB-PRINTER-NAME */
               "",                              /* RB-PRINTER-PORT */
               "",                              /* RB-OUTPUT-FILE */
                1,                              /* RB-NUMBER-COPIES  - zero */                  
                0,                              /* RB-BEGIN-PAGE - zero */
                0,                              /* RB-END-PAGE - zero */
               no,                              /* RB-TEST-PATTERN */
               "Nomenclador de tareas",         /* RB-WINDOW-TITLE */
               yes,                           /* RB-DISPLAY-ERRORS */
               yes,                           /* RB-DISPLAY-STATUS */
               no,                              /* RB-NO-WAIT */
               "v_general = " + string(vfechadesde) + "|" + STRING(vfechahasta) + "|" /* RB-OTHER-PARAMETERS */,
               ""
               ).   
        
        os-delete value(RB-MEMO-FILE).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime-reporte wWin 
PROCEDURE imprime-reporte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  define var v_filtro as character initial "".
  define var v_general as character.
  DEF VAR vfechadesde AS DATE.
  DEF VAR vfechahasta AS DATE.


   vfechadesde = DATE(fi-desde:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   vfechahasta = DATE(fi-hasta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

   /*vlc_dir_fuentes = "z:\sistemas\sami\sistemas\". */

        v_filtro = "liq_items_tarjas.id_sucursal = " + STRING(p_suc) +
                   " and liq_items_tarjas.fecha >= date('" + STRING(vfechadesde) + "')" +
                   " and liq_items_tarjas.fecha <= date('" + STRING(vfechahasta) + "')" +
                   " and liq_items_tarjas.hs_adicionales_tareas_trabajadas <> 0".
        
        RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.
        
         RUN  aderb\_prntrb2(
               (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
               "tarja_autoriza_hs", /* RB-REPORT-NAME */
               "",                             /* RB-DB-CONNECTION */
               "O",                             /* RB-INCLUDE-RECORDS */
               v_filtro,                              /* RB-FILTER */
               RB-MEMO-FILE,                              /* RB-MEMO-FILE */
               "D",                             /* RB-PRINT-DESTINATION */
               "?",                              /* RB-PRINTER-NAME */
               "",                              /* RB-PRINTER-PORT */
               "",                              /* RB-OUTPUT-FILE */
                1,                              /* RB-NUMBER-COPIES  - zero */                  
                0,                              /* RB-BEGIN-PAGE - zero */
                0,                              /* RB-END-PAGE - zero */
               no,                              /* RB-TEST-PATTERN */
               "Nomenclador de tareas",         /* RB-WINDOW-TITLE */
               yes,                           /* RB-DISPLAY-ERRORS */
               yes,                           /* RB-DISPLAY-STATUS */
               no,                              /* RB-NO-WAIT */
               "v_general = " + string(vfechadesde) + "|" + STRING(vfechahasta) + "|" /* RB-OTHER-PARAMETERS */,
               ""
               ).   
        
        os-delete value(RB-MEMO-FILE).

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

  fi-desde:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "01/" + STRING(MONTH(TODAY),"99") + "/" + STRING(YEAR(TODAY)).
  fi-hasta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "01/" + STRING(MONTH(TODAY),"99") + "/" + STRING(YEAR(TODAY)).

  RUN filtrar-planillas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

