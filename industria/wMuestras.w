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

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bmuestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dmuestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 17
         WIDTH              = 80
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
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
             INPUT  'dmuestras.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedmuestrasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dmuestras ).
       RUN repositionObject IN h_dmuestras ( 1.00 , 69.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  '../industria/bmuestras.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bmuestras ).
       RUN repositionObject IN h_bmuestras ( 6.71 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bmuestras ( 6.67 , 66.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 67.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_muestraOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsid_muestra0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.43 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 3.71 , 78.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dmuestras. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dmuestras ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dmuestras ).

       /* Links to SmartDataBrowser h_bmuestras. */
       RUN addLink ( h_dmuestras , 'Data':U , h_bmuestras ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynfilter ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_bmuestras ,
             h_dynfilter , 'AFTER':U ).
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
  VIEW FRAME fMain IN WINDOW wWin.
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

