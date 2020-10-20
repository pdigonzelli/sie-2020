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

/* Local Variable Definitions ---                                       */
DEFINE VAR vsector AS INTEGER NO-UNDO.
DEFINE VAR vsucursal AS INTEGER NO-UNDO.
DEFINE VAR vfecha AS DATE NO-UNDO.

{src/adm2/widgetprto.i}

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
&Scoped-Define ENABLED-OBJECTS FI-sector B-filtrar FI-sucursal FI-fecha 
&Scoped-Define DISPLAYED-OBJECTS FI-sector nombre-sector FI-sucursal ~
nombre-sucursal FI-fecha 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_breservahoras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dreservashoras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vreservahoras AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-filtrar 
     LABEL "Filtrar" 
     SIZE 17 BY 1.43.

DEFINE VARIABLE FI-fecha AS DATE FORMAT "99/99/99":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-sucursal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.19
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE nombre-sucursal AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-sector AT ROW 1.48 COL 20 COLON-ALIGNED WIDGET-ID 6
     nombre-sector AT ROW 1.48 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     B-filtrar AT ROW 1.95 COL 77 WIDGET-ID 16
     FI-sucursal AT ROW 3.14 COL 20 COLON-ALIGNED WIDGET-ID 8
     nombre-sucursal AT ROW 3.14 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     FI-fecha AT ROW 4.81 COL 20.4 COLON-ALIGNED WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.8 BY 24.86 WIDGET-ID 100.


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
         TITLE              = "Reserva de Horas"
         HEIGHT             = 24.86
         WIDTH              = 107.8
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
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN nombre-sector IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sucursal IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Reserva de Horas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Reserva de Horas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-filtrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-filtrar wWin
ON CHOOSE OF B-filtrar IN FRAME fMain /* Filtrar */
DO:
  RUN filtrar-reservas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-sector
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON GO OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON LEAVE OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON MOUSE-SELECT-DBLCLICK OF FI-sector IN FRAME fMain /* Sector */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
    RUN adm2/support/gConsultas.w (INPUT "bsectoresagricola.w",
                                   INPUT "dsectoresagricola.w",
                                   INPUT "id_sector",
                                   INPUT "" ,
                                   OUTPUT xfieldResult).

    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:
           fi-sector:SCREEN-VALUE = xfieldResult.     
           RUN descriptivos.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON U1 OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sucursal wWin
ON LEAVE OF FI-sucursal IN FRAME fMain /* Sucursal */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sucursal wWin
ON MOUSE-SELECT-DBLCLICK OF FI-sucursal IN FRAME fMain /* Sucursal */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
    RUN adm2/support/gConsultas.w (INPUT "bsucursales.w",
                                   INPUT "dsucursales.w",
                                   INPUT "id_sucursal",
                                   INPUT "sucursales.id_tipo_sucursal = 19" ,
                                   OUTPUT xfieldResult).

    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:
           fi-sucursal:SCREEN-VALUE = xfieldResult.     
           RUN descriptivos.
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
             INPUT  'dreservashoras.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedreservashorasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dreservashoras ).
       RUN repositionObject IN h_dreservashoras ( 2.91 , 95.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'breservahoras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNames?UpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_breservahoras ).
       RUN repositionObject IN h_breservahoras ( 7.29 , 12.00 ) NO-ERROR.
       RUN resizeObject IN h_breservahoras ( 6.67 , 77.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vreservahoras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vreservahoras ).
       RUN repositionObject IN h_vreservahoras ( 16.19 , 14.00 ) NO-ERROR.
       /* Size in AB:  ( 8.67 , 79.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 14.33 , 10.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 67.20 ) NO-ERROR.

       /* Links to SmartDataObject h_dreservashoras. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dreservashoras ).

       /* Links to SmartDataBrowser h_breservahoras. */
       RUN addLink ( h_dreservashoras , 'Data':U , h_breservahoras ).

       /* Links to SmartDataViewer h_vreservahoras. */
       RUN addLink ( h_dreservashoras , 'Data':U , h_vreservahoras ).
       RUN addLink ( h_vreservahoras , 'Update':U , h_dreservashoras ).
       RUN addLink ( h_dyntoolbar , 'Tableio':U , h_vreservahoras ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_breservahoras ,
             FI-fecha:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_dyntoolbar ,
             h_breservahoras , 'AFTER':U ).
       RUN adjustTabOrder ( h_vreservahoras ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos wWin 
PROCEDURE descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 find first sucursales where sucursales.id_sucursal = INTEGER(fi-sucursal:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
 if available sucursales Then
          nombre-sucursal:screen-value = sucursales.nombre.
       ELSE
         nombre-sucursal:screen-value = "".


find first sectores_agricolas where sectores_agricolas.id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME} )  no-lock no-error .
if available sectores_agricolas then 
    nombre-sector:screen-value   = string(sectores_agricolas.descripcion).
    else
    nombre-sector:screen-value  = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-valores wWin 
PROCEDURE devuelve-valores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER psector AS INTEGER.
DEF OUTPUT PARAMETER psuc AS INTEGER.

 psector = INTEGER(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 psuc = INTEGER(fi-sucursal:SCREEN-VALUE IN FRAME {&FRAME-NAME}).


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
  DISPLAY FI-sector nombre-sector FI-sucursal nombre-sucursal FI-fecha 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE FI-sector B-filtrar FI-sucursal FI-fecha 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filtrar-reservas wWin 
PROCEDURE filtrar-reservas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR cQuery AS CHARACTER.
DEFINE VAR xQuery AS CHARACTER. 

 vsector = INTEGER(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 vsucursal = INTEGER(fi-sucursal:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 vfecha = DATE(fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).


  cQuery = "reserva_horas.id_sector = " + STRING(vsector) + 
           " and reserva_horas.id_sucursal = " + STRING(vsucursal) +
           " and reserva_horas.fecha >= date('" + string(vfecha) + "')".

  xQuery = "FOR EACH reserva_horas WHERE " + cQuery + " NO-LOCK INDEXED-REPOSITION ".

     {set queryWhere xQuery h_dreservashoras}.
      DYNAMIC-FUNCTION ('openQuery':U IN h_dreservashoras). 


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

  fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
  FIND FIRST usuarios_sucursales WHERE usuarios_sucursales.id_usuario = userid("userdb") no-lock NO-ERROR.
  IF AVAILABLE usuarios_sucursales THEN
      fi-sucursal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(usuarios_sucursales.id_sucursal).
  FIND LAST par_agricola WHERE par_agricola.id_usuario = 20 NO-LOCK NO-ERROR.
  IF AVAILABLE par_agricola THEN
     fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(par_agricola.fecha + 1).
  RUN descriptivos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

