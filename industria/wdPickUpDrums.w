&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER piSucUbi AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER piTipTam AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER pcReturn AS CHARACTER  NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiUbi fiLote fiAnio fiArti fiTamborDesde ~
fiTamborHasta BUTTON-3 RADIO-SET-1 Btn_OK Btn_Cancel BUTTON-5 RECT-34 ~
RECT-35 
&Scoped-Define DISPLAYED-OBJECTS fiUbi fiLote fiAnio fiArti fiTamborDesde ~
fiTamborHasta RADIO-SET-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bttcabeceralote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttcabeceralote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Buscar" 
     SIZE 25 BY 1.67.

DEFINE BUTTON BUTTON-5 
     LABEL "Limpiar" 
     SIZE 25 BY 1.67.

DEFINE VARIABLE fiAnio AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiArti AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "52: Turbio; 53: Claro; 71: Pulpa; 30: Pulpa Aseptica" NO-UNDO.

DEFINE VARIABLE fiLote AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiTamborDesde AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Tambor" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiTamborHasta AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiUbi AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Ubicacion" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "85: Despachado; 91: Facturado; 95: Famailla; 96: Lavalle" NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "por Lote", "BY anio BY id_lote",
"por Articulo", "BY id_articulo",
"Año", "BY anio"
     SIZE 20 BY 3 NO-UNDO.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 6.67.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45.2 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiUbi AT ROW 2.91 COL 11 COLON-ALIGNED
     fiLote AT ROW 4.14 COL 11 COLON-ALIGNED
     fiAnio AT ROW 5.33 COL 11 COLON-ALIGNED
     fiArti AT ROW 6.52 COL 11 COLON-ALIGNED
     fiTamborDesde AT ROW 7.71 COL 11 COLON-ALIGNED
     fiTamborHasta AT ROW 7.71 COL 22 COLON-ALIGNED NO-LABEL
     BUTTON-3 AT ROW 2.91 COL 56
     RADIO-SET-1 AT ROW 3.62 COL 105 NO-LABEL
     Btn_OK AT ROW 18.14 COL 99
     Btn_Cancel AT ROW 18.14 COL 115
     BUTTON-5 AT ROW 5.76 COL 56
     RECT-34 AT ROW 2.43 COL 1
     RECT-35 AT ROW 2.43 COL 85
     "Ordenar" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 3.62 COL 93
     SPACE(27.20) SKIP(15.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Seleccionar Tambores"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   Custom                                                               */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Seleccionar Tambores */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
  pcReturn = DYNAMIC-FUNCTION('getSelectedRows' IN h_dttCabeceraLote).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 gDialog
ON CHOOSE OF BUTTON-3 IN FRAME gDialog /* Buscar */
DO:
  RUN fillTbsPickUp IN h_dttCabeceraLote (IF piSucUbi = 0 THEN INTEGER(fiUbi:SCREEN-VALUE) ELSE piSucUbi,
                                          piTipTam,
                                          0, 
                                          INTEGER(fiLote:SCREEN-VALUE),
                                          INTEGER(fiAnio:SCREEN-VALUE),
                                          INTEGER(fiArti:SCREEN-VALUE),
                                          INTEGER(fiTamborDesde:SCREEN-VALUE),
                                          INTEGER(fiTamborHasta:SCREEN-VALUE)).
  
  APPLY "VALUE-CHANGED" TO radio-set-1 IN FRAME gDialog.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 gDialog
ON CHOOSE OF BUTTON-5 IN FRAME gDialog /* Limpiar */
DO:
  ASSIGN
        fiLote:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = "0"
        fiAnio:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = "0"
        fiArti:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = "0"
        fiTamborDesde:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
        fiTamborHasta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
        .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 gDialog
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME gDialog
DO:
  DEFINE VARIABLE cSrt AS CHARACTER  NO-UNDO.

  cSrt = SELF:SCREEN-VALUE.

  DYNAMIC-FUNCTION('setQuerySort' IN h_dttCabeceraLote, cSrt).
  DYNAMIC-FUNCTION('openQuery' IN h_dttCabeceraLote).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
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
             INPUT  'dttcabeceralote.wDB-AWARE':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedttcabeceraloteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dttcabeceralote ).
       RUN repositionObject IN h_dttcabeceralote ( 5.76 , 88.00 ) NO-ERROR.
       /* Size in AB:  ( 2.14 , 10.00 ) */

       RUN constructObject (
             INPUT  'bttcabeceralote.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bttcabeceralote ).
       RUN repositionObject IN h_bttcabeceralote ( 9.57 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bttcabeceralote ( 8.33 , 129.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda1SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdate,PrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 129.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dttcabeceralote. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dttcabeceralote ).

       /* Links to SmartDataBrowser h_bttcabeceralote. */
       RUN addLink ( h_dttcabeceralote , 'Data':U , h_bttcabeceralote ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             Btn_Cancel:HANDLE , 'AFTER':U ).
       RUN adjustTabOrder ( h_bttcabeceralote ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY fiUbi fiLote fiAnio fiArti fiTamborDesde fiTamborHasta RADIO-SET-1 
      WITH FRAME gDialog.
  ENABLE fiUbi fiLote fiAnio fiArti fiTamborDesde fiTamborHasta BUTTON-3 
         RADIO-SET-1 Btn_OK Btn_Cancel BUTTON-5 RECT-34 RECT-35 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExit gDialog 
PROCEDURE tlbExit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  APPLY "END-ERROR":U TO SELF.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

