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
&Scoped-Define ENABLED-OBJECTS RECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b001qprotocoloslote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d001qprotocoloslote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 87 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RECT-1 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87.8 BY 20.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Protocolos y Lotes"
         HEIGHT             = 20.52
         WIDTH              = 87.8
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Protocolos y Lotes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Protocolos y Lotes */
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
             INPUT  'd001qprotocoloslote.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed001qprotocolosloteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d001qprotocoloslote ).
       RUN repositionObject IN h_d001qprotocoloslote ( 7.43 , 65.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'b001qprotocoloslote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_b001qprotocoloslote ).
       RUN repositionObject IN h_b001qprotocoloslote ( 8.86 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_b001qprotocoloslote ( 12.14 , 87.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda1SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 87.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_lote,Anio-2,id_protocolo,id_articuloOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsid_lote0id_protocolo0Anio-20DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.67 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 5.57 , 85.00 ) NO-ERROR.

       /* Links to SmartDataObject h_d001qprotocoloslote. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_d001qprotocoloslote ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_d001qprotocoloslote ).

       /* Links to SmartDataBrowser h_b001qprotocoloslote. */
       RUN addLink ( h_d001qprotocoloslote , 'Data':U , h_b001qprotocoloslote ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynfilter ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_b001qprotocoloslote ,
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
  ENABLE RECT-1 
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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.

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
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".


DEFINE VARIABLE v_o_f AS CHARACTER  NO-UNDO.

FIND FIRST protocolos 
     WHERE protocolos.id_protocolo = DYNAMIC-FUNCTION('columnValue' IN h_d001qProtocolosLote, 'id_protocolo')
       AND protocolos.anio         = DYNAMIC-FUNCTION('columnValue' IN h_d001qProtocolosLote, 'anio')
     NO-ERROR.


FIND FIRST lotes_jugo 
     WHERE lotes_jugo.nromov = DYNAMIC-FUNCTION('columnValue' IN h_d001qprotocoloslote, 'nromov')
   NO-LOCK NO-ERROR.
                    IF AVAILABLE lotes_jugo THEN DO:
                        FOR EACH tambores_industria OF lotes_jugo
                                                    WHERE tambores_industria.id_tambor = 
                                                            protocolos.desde_tambor
                                                    NO-LOCK.
                            FIND FIRST contratos WHERE contratos.id_contrato = 
                                                       tambores_industria.id_contrato_of 
                                                   AND contratos.id_tipo_contrato = 
                                                       tambores_industria.id_tipocontrato_of
                                                   AND contratos.anio = 
                                                       tambores_industria.anio_of
                                                 NO-LOCK NO-ERROR.
                            IF AVAILABLE contratos THEN DO:
                                v_o_f = STRING(contratos.orden_fabricacion) + "/" + 
                                        SUBSTRING(STRING(YEAR(contratos.fecha)),3,2).
                            END.
                        END.
                        /*by facundo para el desglose por envases para lotes de pepsi 17/07/2006*/
                        DEFINE BUFFER buTam FOR tambores_industria.
                        DEFINE VARIABLE j     AS INTEGER    NO-UNDO.
                        DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
                        DEFINE VARIABLE d     AS INTEGER    NO-UNDO.
                        DEFINE VARIABLE h     AS INTEGER    NO-UNDO.
                        DEFINE VARIABLE cDes  AS CHARACTER  NO-UNDO.

                        FOR EACH buTam 
                            OF lotes_jugo
                            BREAK BY buTam.id_envase
                            BY buTam.id_tambor.
                          j = j + 1.
                          i = buTam.id_tambor.
                          IF LAST-OF(buTam.id_envase) THEN DO:
                            FIND FIRST envases_prod OF buTam NO-LOCK NO-ERROR.
                            d = i - j + 1.
                            h = i.
                            cDes = cDes + "From " + envases_prod.abreviatura_ingles + " " + STRING(d) + " To " + STRING(h) + ", " + STRING(j) + " Units" + CHR(10).
                            i = 0.
                            j = 0.                            
                          END.
                        END.

                        ASSIGN protocolos.desglose_envases = cDes.

                    END.
                          RUN  aderb\_prntrb2(
                           "..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                           "protocolo_jugo",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           "protocolos.id_protocolo = " + STRING(protocolos.id_protocolo) + " and " +
                                              "protocolos.anio = " + STRING(protocolos.anio) + " and " +
                                              "protocolos.id_articulo = " + STRING(protocolos.id_articulo),                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Protocolo Jugo",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           NO,                              /* RB-NO-WAIT */
                           "v_general = " + v_o_f + ";" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).

                          /*
                    run p_reportes_9.p (input "protocolo_jugo",
                                        input "Protocolos",
                                        input "protocolos.id_protocolo = " + STRING(protocolos.id_protocolo) + " and " +
                                              "protocolos.anio = " + STRING(protocolos.anio) + " and " +
                                              "protocolos.id_articulo = " + STRING(protocolos.id_articulo) ,
                                        input v_o_f + ";").   */                 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

