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
DEFINE INPUT  PARAMETER piSucursal AS INTEGER      NO-UNDO.
DEFINE INPUT  PARAMETER piTipoTamb AS INTEGER      NO-UNDO.
DEFINE INPUT  PARAMETER prRowId    AS ROWID        NO-UNDO.    /*cabecera a la cual se asociaran los tambores seleccionados, lote, carga, remito*/
DEFINE INPUT  PARAMETER pcOrigen   AS CHARACTER    NO-UNDO.
DEFINE OUTPUT PARAMETER pcAux      AS CHARACTER    NO-UNDO.


/* Local Variable Definitions ---                                       */

DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.

DEFINE VARIABLE hApp    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibRto AS HANDLE     NO-UNDO.
DEFINE VARIABLE lFlg    AS LOGICAL    NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS radFilter btnFilter lstFilter cmbOps fiValue ~
btnAddClause fiOk fiCancel btnReset chkAll btnDelClause fiCount fiRows ~
RECT-33 RECT-6 
&Scoped-Define DISPLAYED-OBJECTS radFilter lstFilter cmbOps fiValue chkAll ~
fiCount fiRows 

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

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_btamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtamboresindustria AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddClause 
     IMAGE-UP FILE "src/adm2/image/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Agregar" 
     SIZE 5.2 BY 1.19 TOOLTIP "Agregar".

DEFINE BUTTON btnDelClause 
     IMAGE-UP FILE "src/adm2/image/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Quitar" 
     SIZE 5.2 BY 1.19 TOOLTIP "Qutar".

DEFINE BUTTON btnFilter 
     IMAGE-UP FILE "src/adm2/image/filter.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Filtrar" 
     SIZE 5.2 BY 1.19.

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "src/adm2/image/reset.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Deshacer" 
     SIZE 5.2 BY 1.19 TOOLTIP "Deshacer".

DEFINE BUTTON fiCancel 
     LABEL "Cancelar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON fiOk 
     LABEL "Aceptar" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbOps AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS ">=","<=","=" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fiCount AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tambores Filtrados" 
      VIEW-AS TEXT 
     SIZE 5 BY .62 NO-UNDO.

DEFINE VARIABLE fiRows AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filas Seleccionadas" 
      VIEW-AS TEXT 
     SIZE 7 BY .62 NO-UNDO.

DEFINE VARIABLE fiValue AS CHARACTER FORMAT "x(256)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 NO-UNDO.

DEFINE VARIABLE radFilter AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sucursal Prod.", 1,
"Nro. Produccion/Lote", 2,
"Tambor", 3,
"Articulo", 4,
"A�o", 5
     SIZE 26 BY 4.52 NO-UNDO.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 1.19.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 119 BY 5.24.

DEFINE VARIABLE lstFilter AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 3.1 NO-UNDO.

DEFINE VARIABLE chkAll AS LOGICAL INITIAL no 
     LABEL "Seleccionar Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     radFilter AT ROW 2.67 COL 2 NO-LABEL
     btnFilter AT ROW 3.14 COL 107
     lstFilter AT ROW 3.14 COL 75 NO-LABEL
     cmbOps AT ROW 4.1 COL 39 COLON-ALIGNED NO-LABEL
     fiValue AT ROW 4.1 COL 49.6 COLON-ALIGNED NO-LABEL
     btnAddClause AT ROW 3.14 COL 69
     fiOk AT ROW 25.52 COL 88
     fiCancel AT ROW 25.52 COL 105
     btnReset AT ROW 5.1 COL 107
     chkAll AT ROW 25.71 COL 26 RIGHT-ALIGNED
     btnDelClause AT ROW 5 COL 69
     fiCount AT ROW 25.76 COL 31
     fiRows AT ROW 25.76 COL 77 COLON-ALIGNED
     RECT-33 AT ROW 25.52 COL 1
     RECT-6 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.6 BY 25.86
         CANCEL-BUTTON fiCancel.


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
         TITLE              = "Origen Tambores"
         HEIGHT             = 25.86
         WIDTH              = 119.6
         MAX-HEIGHT         = 34.33
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.33
         VIRTUAL-WIDTH      = 204.8
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
       btnAddClause:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnDelClause:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnFilter:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnReset:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR TOGGLE-BOX chkAll IN FRAME fMain
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN fiCount IN FRAME fMain
   ALIGN-L                                                              */
ASSIGN 
       fiCount:AUTO-RESIZE IN FRAME fMain      = TRUE.

ASSIGN 
       fiRows:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Origen Tambores */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Origen Tambores */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  IF VALID-HANDLE(hApp) THEN DO:      
    hApp:DISCONNECT().
    DELETE OBJECT hApp.
  END.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddClause
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddClause wWin
ON CHOOSE OF btnAddClause IN FRAME fMain /* Agregar */
DO:
  RUN addClause.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelClause
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelClause wWin
ON CHOOSE OF btnDelClause IN FRAME fMain /* Quitar */
DO:
  lstFilter:DELETE(lstFilter:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter wWin
ON CHOOSE OF btnFilter IN FRAME fMain /* Filtrar */
DO:
  RUN setFilter(TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset wWin
ON CHOOSE OF btnReset IN FRAME fMain /* Deshacer */
DO:
  RUN setFilter(FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME chkAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL chkAll wWin
ON VALUE-CHANGED OF chkAll IN FRAME fMain /* Seleccionar Todos */
DO:
  IF LOGICAL(SELF:SCREEN-VALUE) = TRUE THEN
    RUN selectAll IN h_bTamboresIndustria (TRUE).
  ELSE
    RUN refreshRow IN h_dTamboresIndustria.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCancel wWin
ON CHOOSE OF fiCancel IN FRAME fMain /* Cancelar */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOk wWin
ON CHOOSE OF fiOk IN FRAME fMain /* Aceptar */
DO:
  DEFINE VARIABLE cRows   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDes    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFirst  AS LOGICAL    NO-UNDO.


  cRows = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bTamboresIndustria).
 
  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:

    cFields = DYNAMIC-FUNCTION('getDatosTambor' IN hLib, TO-ROWID(ENTRY(iRow, cRows))).

    ASSIGN iEmp = INTEGER(ENTRY(1, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(2, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(3, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(4, cFields, CHR(1)))
           iTam = INTEGER(ENTRY(5, cFields, CHR(1))).

    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".
    IF pcOrigen = "carga" THEN DO:
      RUN setCargaToTambor IN hLib (prRowId,
                                    iEmp, 
                                    iSuc, 
                                    iTip, 
                                    iNro, 
                                    iTam).        
    END.  
    IF pcOrigen = "jugo" THEN DO:
      RUN setLoteJugoToTambor IN hLib (prRowId,
                                       iEmp, 
                                       iSuc, 
                                       iTip, 
                                       iNro, 
                                       iTam).
    END.

  END.  
  
  IF pcOrigen = "remitos" THEN DO:    
    RUN setRangos.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
  RUN postOpenQuery.

  
END.

/*
  IF pcOrigen = "carga" THEN DO:
    /*me manda un mail cada vez que se rebatchean tambores en cargas*/
    RUN ..\industria\sendMail.p("",                               
                                2,                                
                                "Rebatcheo de Tambores en carga",          
                                STRING(prRowId) + CHR(10) + CHR(13) + STRING(iNro), 
                                "facundoj@sa-sanmiguel.com", 
                                "").                         

  END.  
  */





/*


      IF NOT lFirst THEN DO:
        iDes   = iTam.
        lFirst = TRUE.
      END.
      iHas = iTam.
      pcAux = STRING(iEmp) + CHR(1) + 
              STRING(iSuc) + CHR(1) + 
              STRING(iTip) + CHR(1) + 
              STRING(iNro) + CHR(1) + 
              STRING(iDes) + CHR(1) + 
              STRING(iHas) + CHR(1) + 
              ENTRY(6,  cFields, CHR(1)) + CHR(1) + 
              ENTRY(7,  cFields, CHR(1)) + CHR(1) + 
              ENTRY(8,  cFields, CHR(1)) + CHR(1) + 
              ENTRY(9,  cFields, CHR(1)) + CHR(1) + 
              ENTRY(10, cFields, CHR(1)) + CHR(1) + 
              ENTRY(11, cFields, CHR(1)) + CHR(1) +
              ENTRY(12, cFields, CHR(1)) + CHR(1) + 
              ENTRY(13, cFields, CHR(1)).
              
    END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiValue wWin
ON ANY-KEY OF fiValue IN FRAME fMain
DO:

  IF LAST-KEY = 13 THEN 
    RUN addClause.

  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aceptaranterior wWin 
PROCEDURE aceptaranterior :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRows   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDes    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFirst  AS LOGICAL    NO-UNDO.


  cRows = DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bTamboresIndustria).
 
  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION('getClaveLote' IN hLib, TO-ROWID(ENTRY(iRow, cRows))).
    ASSIGN iEmp = INTEGER(ENTRY(1, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(2, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(3, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(4, cFields, CHR(1)))
           iTam = INTEGER(ENTRY(5, cFields, CHR(1))).

    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".
    IF pcOrigen = "carga" THEN DO:
      RUN setCargaToTambor IN hLib (prRowId,
                                    iEmp, 
                                    iSuc, 
                                    iTip, 
                                    iNro, 
                                    iTam).        
    END.  
    IF pcOrigen = "jugo" THEN DO:
      RUN setLoteJugoToTambor IN hLib (prRowId,
                                       iEmp, 
                                       iSuc, 
                                       iTip, 
                                       iNro, 
                                       iTam).
    END.

    IF pcOrigen = "remitos" THEN DO:
      IF NOT lFirst THEN DO:
        iDes   = iTam.
        lFirst = TRUE.
      END.
      iHas = iTam.
      pcAux = STRING(iEmp) + "," + 
              STRING(iSuc) + "," + 
              STRING(iTip) + "," + 
              STRING(iNro) + "," + 
              STRING(iDes) + "," + 
              STRING(iHas) + "," + 
              ENTRY(7,  cFields, CHR(1)) + "," + 
              ENTRY(8,  cFields, CHR(1)) + "," + 
              ENTRY(9,  cFields, CHR(1)) + "," + 
              ENTRY(10, cFields, CHR(1)) + "," + 
              ENTRY(11, cFields, CHR(1)) + "," + 
              ENTRY(12, cFields, CHR(1)) + "," +
              ENTRY(13, cFields, CHR(1)) + "," + 
              ENTRY(14, cFields, CHR(1)).
              
    END.
    

  END.  
  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
  RUN postOpenQuery.
  
  IF pcOrigen = "remitos" THEN DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addClause wWin 
PROCEDURE addClause :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFilterStr AS CHARACTER  NO-UNDO.

  CASE INTEGER(radFilter:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    WHEN 1 THEN 
      cFilterStr = "id_sucursal;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 2 THEN
      cFilterStr = "id_lote;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.
    WHEN 3 THEN
      cFilterStr = "id_tambor;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.    
    WHEN 4 THEN
      cFilterStr = "id_articulo;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.    
    WHEN 5 THEN
      cFilterStr = "anio;" + cmbOps:SCREEN-VALUE + ";" + fiValue:SCREEN-VALUE.    
  END CASE.
  
  lstFilter:ADD-LAST(cFilterStr).
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
             INPUT  'dtamboresindustria.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch1440CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtamboresindustriaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtamboresindustria ).
       RUN repositionObject IN h_dtamboresindustria ( 5.29 , 31.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'btamboresindustria.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_btamboresindustria ).
       RUN repositionObject IN h_btamboresindustria ( 7.67 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_btamboresindustria ( 17.62 , 119.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 119.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dtamboresindustria. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dtamboresindustria ).

       /* Links to SmartDataBrowser h_btamboresindustria. */
       RUN addLink ( h_dtamboresindustria , 'Data':U , h_btamboresindustria ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             radFilter:HANDLE IN FRAME fMain , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

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

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectApp wWin 
PROCEDURE connectApp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  hApp = DYNAMIC-FUNCTION('connectApp' IN hLibCom).

  IF VALID-HANDLE(hApp) THEN 
    RUN libTamboresIndustria.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
  ELSE 
    RUN libTamboresIndustria.p PERSISTENT SET hLib .

  RUN libRemitos.p PERSISTENT SET hLibRto.

  lFlg = VALID-HANDLE(hApp).


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
  DISPLAY radFilter lstFilter cmbOps fiValue chkAll fiCount fiRows 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE radFilter btnFilter lstFilter cmbOps fiValue btnAddClause fiOk 
         fiCancel btnReset chkAll btnDelClause fiCount fiRows RECT-33 RECT-6 
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

  RUN connectApp.
  RUN initQuery.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initQuery wWin 
PROCEDURE initQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSrt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  cQry = " tambores_industria.id_estado <> 8 AND"  +  /*no incluye tambores reprocesados en lotes*/
         " tambores_industria.id_estado <> 3 AND"  +  /*no incluye tambores en proceso de rebatcheo*/
         " tambores_industria.id_estado <> 10 AND" +  /*no incluye tambores rerocesados en cargas*/
         " tambores_industria.id_sucursal_ubicacion = " + STRING(piSucursal) + " ".
  

  CASE piTipoTamb:
    WHEN 77 THEN DO:
      cArt = DYNAMIC-FUNCTION('getArticulosMPJugo' IN hLib, prRowId, pcOrigen).
      IF cArt <> "" THEN DO:      
        cQry = cQry + "AND (".
        DO i = 1 TO NUM-ENTRIES(cArt):
          cQry = cQry + "tambores_industria.id_articulo = " + ENTRY(i, cArt) + " OR ".
        END.
        cQry = SUBSTRING(cQry, 1, LENGTH(cQry) - 4).
        cQry = cQry + ") AND tambores_industria.id_tipotambor <> 9".
      END.
      ELSE /*para que no traiga nada si no encuentra*/
        cQry = cQry + "AND tambores_industria.id_tipotambor = 99". 
    END.

    WHEN 3  THEN DO:
      IF pcOrigen = "remitos" THEN DO:
        cAux = DYNAMIC-FUNCTION('getDatosOERemito' IN hLibRto, prRowId).
        cQry = cQry + "AND tambores_industria.id_locacion_ubicacion = 4 AND tambores_industria.id_tipotambor = " + STRING(piTipoTamb).
        IF ENTRY(1, cAux, CHR(1)) <> "0" THEN
          cQry = cQry + " AND tambores_industria.id_orden_entrega = " + ENTRY(1, cAux, CHR(1)) + " AND tambores_industria.item_oe = " + ENTRY(2, cAux, CHR(1)).
      END.
      ELSE
        cQry = cQry + "AND tambores_industria.id_estado = 2 AND tambores_industria.id_locacion_ubicacion = 4 AND tambores_industria.id_tipotambor = " + STRING(piTipoTamb).
    END.

    OTHERWISE
      cQry = cQry + "AND tambores_industria.id_locacion_ubicacion = 4 AND tambores_industria.id_tipotambor = " + STRING(piTipoTamb).
  END CASE.


  cSrt = "BY tambores_industria.anio DESC BY tambores_industria.id_articulo DESC BY tambores_industria.id_lote BY tambores_industria.id_tambor".
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dTamboresIndustria, cQry).
  DYNAMIC-FUNCTION('setQuerySort' IN h_dTamboresIndustria, cSrt).
  DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE numSelectedRows wWin 
PROCEDURE numSelectedRows :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piRows AS INTEGER    NO-UNDO.

  fiRows:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(piRows).

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postOpenQuery wWin 
PROCEDURE postOpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  fiCount:SCREEN-VALUE IN FRAME fMain = DYNAMIC-FUNCTION('getRowsNumber' IN h_dTamboresIndustria).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFilter wWin 
PROCEDURE setFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER plAction AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cColumns   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValues    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOperators AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCond      AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  IF plAction THEN DO: /*filtrar*/
    DO i = 1 TO NUM-ENTRIES(lstFilter:LIST-ITEMS IN FRAME fMain):
      cCond = ENTRY(i, lstFilter:LIST-ITEMS).
      ASSIGN cColumns   = cColumns + "," + ENTRY(1, cCond, ";")
             cOperators = cOperators + "," + ENTRY(2, cCond, ";")
             cValues    = cValues + CHR(1) + ENTRY(3, cCond, ";").    
    END.
  
    cColumns   = SUBSTRING(cColumns, 2, LENGTH(cColumns)).
    cOperators = SUBSTRING(cOperators, 2, LENGTH(cOperators)).
    cValues    = SUBSTRING(cValues, 2, LENGTH(cValues)).
  
    DYNAMIC-FUNCTION('assignQuerySelection' IN h_dTamboresIndustria, cColumns, cValues, cOperators).
    DYNAMIC-FUNCTION('openQuery' IN h_dTamboresIndustria).
  END.
  ELSE DO: /*quitar filtro*/
    cQry = lstFilter:LIST-ITEMS.
    DO i = 1 TO NUM-ENTRIES(cQry):
      lstFilter:DELETE(ENTRY(i, cQry)).
    END.
    RUN initQuery.
  END.
    
  RUN postOpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRangos wWin 
PROCEDURE setRangos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
  pcAux = DYNAMIC-FUNCTION('getRangos' IN hLib, DYNAMIC-FUNCTION('getSelectedRowIds' IN h_bTamboresIndustria)).

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

