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

DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.


DEFINE VARIABLE chTreePrev AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chTreeCurr AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chTreeNext AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chImage    AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chNode     AS COM-HANDLE     NO-UNDO.

DEFINE VARIABLE iSelectedWeek AS INTEGER    NO-UNDO.


  
DEFINE TEMP-TABLE ttInal
  RCODE-INFORMATION
  FIELD id_orden_entrega AS INTEGER   COLUMN-LABEL "OE"
  FIELD cliente          AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD destino_final    AS CHARACTER COLUMN-LABEL "Destino"
  FIELD producto         AS CHARACTER COLUMN-LABEL "Producto"
  FIELD tambores         AS INTEGER   COLUMN-LABEL "Tambores"
  FIELD kilos_netos      AS DECIMAL   COLUMN-LABEL "Kilos"
  .

DEFINE TEMP-TABLE ttEmbarques
  RCODE-INFORMATION
  FIELD id_orden_entrega  AS INTEGER   COLUMN-LABEL "OE"
  FIELD cliente           AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD producto          AS CHARACTER COLUMN-LABEL "Produto"
  FIELD agencia           AS CHARACTER COLUMN-LABEL "Agencia"
  FIELD vapor             AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD cantidad          AS INTEGER   COLUMN-LABEL "Qty"
  FIELD tipo_contenedor   AS CHARACTER COLUMN-LABEL "Tipo Cont"
  FIELD booking           AS CHARACTER COLUMN-LABEL "Booking"
  FIELD destino           AS CHARACTER COLUMN-LABEL "Destino"
  FIELD cutoff            AS DATE      COLUMN-LABEL "Cut Off"
  FIELD Inal              AS CHARACTER COLUMN-LABEL "Inal"
  FIELD orden_retiro      AS CHARACTER COLUMN-LABEL "Orden Retiro"
  FIELD reserva_espacio   AS CHARACTER COLUMN-LABEL "Reserva Espacio"
  FIELD asociacion        AS CHARACTER COLUMN-LABEL "Asociacion con Tbs"
  FIELD remito            AS CHARACTER COLUMN-LABEL "Remito"
  FIELD fax               AS CHARACTER COLUMN-LABEL "Fax"
  FIELD observaciones     AS CHARACTER COLUMN-LABEL "Observaciones"
  .

{windows.i}

&scop GWL_EXSTYLE         -20
&scop WS_EX_LAYERED       524288
&SCOPED-DEFINE LWA_COLORKEY 1
&scop LWA_ALPHA           2
&scop WS_EX_TRANSPARENT   32

DEFINE VARIABLE thisHWND AS INTEGER NO-UNDO.

PROCEDURE SetWindowLongA EXTERNAL "user32.dll":
  def INPUT PARAM HWND AS LONG.     
  def INPUT PARAM nIndex AS LONG.   
  def INPUT PARAM dwNewLong AS LONG.
  DEF RETURN PARAM stat AS LONG.
END.

PROCEDURE SetLayeredWindowAttributes EXTERNAL "user32.dll":
  def INPUT PARAM HWND AS LONG.
  def INPUT PARAM crKey AS LONG.
  def INPUT PARAM bAlpha AS SHORT.
  def INPUT PARAM dwFlagsas AS LONG.
  DEF RETURN PARAM stat AS SHORT.
END.

PROCEDURE GetWindowLongA EXTERNAL "user32.dll":
  def INPUT PARAM HWND AS LONG.
  def INPUT PARAM nIndex AS LONG.   
  DEF RETURN PARAM flgs AS LONG.
END.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-49 SLIDER-1 fiSemana RECT-34 
&Scoped-Define DISPLAYED-OBJECTS SLIDER-1 fiSemana 

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

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-fMain 
       MENU-ITEM m_Query_Constructor LABEL "Query Constructor".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-4 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-4 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "src/adm2/image/filter.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 5.8 BY .95.

DEFINE VARIABLE fiSemana AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Semana" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 1.24.

DEFINE VARIABLE SLIDER-1 AS INTEGER INITIAL 180 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 255 HORIZONTAL 
     TIC-MARKS NONE 
     SIZE 19 BY 1.43 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-49 AT ROW 1.14 COL 56
     SLIDER-1 AT ROW 1 COL 22 NO-LABEL
     fiSemana AT ROW 1.1 COL 44.8 COLON-ALIGNED
     RECT-34 AT ROW 1 COL 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.2 BY 21.33.


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
         TITLE              = "Control OEs"
         HEIGHT             = 21.33
         WIDTH              = 62.2
         MAX-HEIGHT         = 33
         MAX-WIDTH          = 203.2
         VIRTUAL-HEIGHT     = 33
         VIRTUAL-WIDTH      = 203.2
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

{prowinapi/windows.i}
{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME fMain
                                                                        */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 2.43
       COLUMN          = 1
       HEIGHT          = 6.43
       WIDTH           = 62
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-4 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 8.14
       COLUMN          = 53
       HEIGHT          = 2.14
       WIDTH           = 9
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 8.86
       COLUMN          = 1
       HEIGHT          = 6.67
       WIDTH           = 62
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 15.52
       COLUMN          = 1
       HEIGHT          = 6.67
       WIDTH           = 62
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
      CtrlFrame-4:NAME = "CtrlFrame-4":U .
/* CtrlFrame-4 OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
      CtrlFrame:MOVE-AFTER(fiSemana:HANDLE IN FRAME fMain).
      CtrlFrame-4:MOVE-AFTER(CtrlFrame).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame-4).
      CtrlFrame-3:MOVE-AFTER(CtrlFrame-2).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Control OEs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Control OEs */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 wWin
ON CHOOSE OF BUTTON-49 IN FRAME fMain /* ... */
DO:
  DEFINE VARIABLE iSem AS INTEGER    NO-UNDO.

  iSem = INTEGER(fiSemana:SCREEN-VALUE).

  RUN fillTreeView (chTreePrev, iSem - 1).
  RUN fillTreeView (chTreeCurr, iSem).
  RUN fillTreeView (chTreeNext, iSem + 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Click
PROCEDURE CtrlFrame.TreeView.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  iSelectedWeek = INTEGER(chTreePrev:NODES(1):TAG).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.DblClick
PROCEDURE CtrlFrame.TreeView.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN editDocumento(chTreePrev).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 wWin OCX.Click
PROCEDURE CtrlFrame-2.TreeView.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  
 iSelectedWeek = INTEGER(chTreeCurr:NODES(1):TAG).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 wWin OCX.DblClick
PROCEDURE CtrlFrame-2.TreeView.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN editDocumento(chTreeCurr).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 wWin OCX.Click
PROCEDURE CtrlFrame-3.TreeView.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  iSelectedWeek = INTEGER(chTreeNext:NODES(1):TAG).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 wWin OCX.DblClick
PROCEDURE CtrlFrame-3.TreeView.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN editDocumento(chTreeNext).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Query_Constructor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Query_Constructor wWin
ON CHOOSE OF MENU-ITEM m_Query_Constructor /* Query Constructor */
DO:
    DEFINE VAR xSDOS        AS CHARACTER    NO-UNDO.
    DEFINE VAR i            AS INTEGER      NO-UNDO.
    DEFINE VAR iPage        AS INTEGER      NO-UNDO.
    DEFINE VAR iActualPage  AS INTEGER      NO-UNDO.
    DEFINE VAR xDataSource  AS CHARACTER    NO-UNDO.
    DEFINE VAR hDataSource  AS HANDLE       NO-UNDO.

    xSDOS = DYNAMIC-FUNCTION ('getSDO').
    {get CurrentPage iActualPage}.

    DO i = 1 TO NUM-ENTRIES(xSDOS):
        qh = WIDGET-HANDLE(ENTRY(i,xSDOS)).
        {get ObjectPage iPage qh}.
        {get DataSource xDataSource qh}.
        hDataSource = WIDGET-HANDLE(xDataSource).
        IF ( iPage = iActualPage OR iPage = 0 ) AND NOT valid-handle(hDataSource)THEN
            RUN adm2/support/wquery.w ( INPUT qh ).
        ELSE
            MESSAGE 'No puede ejecutar consulta en el detalle' VIEW-AS ALERT-BOX WARNING.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-1 wWin
ON VALUE-CHANGED OF SLIDER-1 IN FRAME fMain
DO:
  RUN makeTransparent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */



/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


MAIN-BLOCK:
  
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.

  RUN GetParent IN hpapi (wWin:HWND, OUTPUT thishwnd).
  
  DEF VAR stat AS INTEGER NO-UNDO.
  DEF VAR flgs AS INTEGER NO-UNDO.
  
  RUN GetWindowLongA(thishwnd, {&GWL_EXSTYLE}, OUTPUT flgs) .
  RUN SetWindowLongA(thishwnd, {&GWL_EXSTYLE}, flgs + {&WS_EX_LAYERED},
                     OUTPUT stat).

  RUN MakeTransparent.

  {src/adm2/windowmn.i}
  
  DEFINE VARIABLE hStyle AS HANDLE NO-UNDO.
    RUN WinStyle.p PERSISTENT SET hStyle.
    RUN AddPaletteStyle IN hStyle ({&window-name}:HWND).
    DELETE PROCEDURE hStyle.

 IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

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
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsBanda1SubModulesTableIOTypeSupportedLinksToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 22.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "wFlowOE.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
    chCtrlFrame-4 = CtrlFrame-4:COM-HANDLE
    UIB_S = chCtrlFrame-4:LoadControls( OCXFile, "CtrlFrame-4":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wFlowOE.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects wWin 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  DEFINE VARIABLE hLibCom AS HANDLE.
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  RUN addToolbarButton IN hLibCom (h_dynToolbar, 
                                   "exportExcelAction", 
                                   "EstadosOE", 
                                   "Estados OEs", 
                                   "excel.bmp",
                                   "tlbExcel", 
                                   "Banda1").


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editDocumento wWin 
PROCEDURE editDocumento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pchTree AS COM-HANDLE     NO-UNDO.

  DEFINE VARIABLE cDat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.

  IF pchTree:SELECTEDITEM:TAG = "" THEN RETURN.


  RUN wdDocumentoOE.w (pchTree:SELECTEDITEM:TAG, OUTPUT cDat).

  cTxt = pchTree:SELECTEDITEM:TEXT.
  IF cDat <> "" THEN DO:
    
    cIco = "no".
    IF ENTRY(2, cDat) <> ""  THEN
      cIco = "pedido".
    
    IF ENTRY(3, cDat) <> ""  THEN
      cIco = "si".

    cTxt = ENTRY(1, cDat) + " pedido el: " + ENTRY(2, cDat) + " cumplido el: " + ENTRY(3, cDat).
    
  END.
  ELSE DO: 
  
    pchTree:selectedITEM:IMAGE = "no".
  END.

  pchTree:selectedITEM:IMAGE = cIco.
  /*pchTree:SELECTEDITEM:TEXT  = cTxt.*/



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
  DISPLAY SLIDER-1 fiSemana 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BUTTON-49 SLIDER-1 fiSemana RECT-34 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeView wWin 
PROCEDURE fillTreeView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pchTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER piWeek  AS INTEGER    NO-UNDO.

  DEFINE VARIABLE dDesde  AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta  AS DATE       NO-UNDO.
  DEFINE VARIABLE cRgo    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  DEFINE VARIABLE chOpen  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chClose AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOE    AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.


  pchTree:NODES:CLEAR().

  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, piWeek, YEAR(TODAY)).
  dDesde = DATE(ENTRY(1, cRgo)).
  dHasta = DATE(ENTRY(2, cRgo)).

  i = 1.

  
  chNode  = pchTree:Nodes:ADD(, , "Semana" + STRING(piWeek), "Semana "  + STRING(piWeek), "semana").  
  chOpen  = pchTree:Nodes:ADD(chNode:KEY, 4, "Pendientes", "Pendientes", "pendientes").
  chClose = pchTree:Nodes:ADD(chNode:KEY, 4, "Cerradas", "Embarques", "cerradas").

  chNode:TAG = STRING(piWeek).


  FOR EACH items_contratos
      WHERE items_contratos.semana_entrega = piWeek
        AND items_contratos.anio = YEAR(dDesde)
      NO-LOCK.

    FIND FIRST contratos OF items_contratos NO-LOCK NO-ERROR.
    FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
        
    cKey = "parte" + items_contratos.id_contrato + "-" + STRING(items_contratos.ITEM).
    cTxt = items_contratos.id_contrato + " Parte: " + STRING(items_contratos.ITEM) + " Cliente: " + clientes.razon_social.

    IF items_contratos.pendiente THEN DO:      
      chNode = pchTree:Nodes:ADD(chOpen:KEY, 4, cKey, cTxt, "contrato").
    END.
      
  END.


  FOR EACH orden_entrega
      WHERE orden_entrega.fecha_embarque  >= dDesde
        AND orden_entrega.fecha_embarque  <= dHasta
      BY orden_entrega.id_orden_entrega.

    FOR EACH items_orden_entrega 
            OF orden_entrega 
          NO-LOCK.

        FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
        
        cCli   = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NONE".
        cKey   = "orden" + items_orden_entrega.id_contrato + "-" + STRING(items_orden_entrega.ITEM) + "-" + STRING(items_orden_entrega.id_orden_entrega) + "-" + STRING(items_orden_entrega.ITEM_oe).
        cTxt   = STRING(items_orden_entrega.id_orden_entrega) + " - " + 
                 STRING(items_orden_entrega.ITEM_oe) + 
                 " cliente: " + cCli + 
                 " contrato: " + items_orden_entrega.id_contrato + " parte: " + STRING(items_orden_entrega.item) + 
                 " Por " + STRING(items_orden_entrega.cantidad_tambores) + " en semana " + STRING(items_orden_entrega.semana_entrega)  .
        chOE   = pchTree:Nodes:ADD(chClose:KEY, 4, cKey, cTxt, "oe").
  
      
        FOR EACH documentos_oe
            WHERE documentos_oe.id_orden_entrega = items_orden_entrega.id_orden_entrega
            NO-LOCK.
  
          cIco = "no".
          IF documentos_oe.fecha_pedido <> ? AND documentos_oe.fecha_cumplido <> ? THEN cIco = "si".
          IF documentos_oe.fecha_pedido = ? AND documentos_oe.fecha_cumplido = ? THEN cIco = "no".
          IF documentos_oe.fecha_pedido <> ? AND documentos_oe.fecha_cumplido = ? THEN cIco = "pedido".
          IF documentos_oe.fecha_pedido = ? AND documentos_oe.fecha_cumplido <> ? THEN cIco = "no".
          
          cTxt   = documentos_oe.descripcion.
          IF documentos_oe.fecha_pedido <> ? THEN cTxt = cTxt + " pedido el: " + STRING(documentos_oe.fecha_pedido).
          IF documentos_oe.fecha_cumplido <> ? THEN cTxt = cTxt + " cumplido el: " + STRING(documentos_oe.fecha_cumplido).
          
          cKey   = "documento" + STRING(documentos_oe.id_orden_entrega) + STRING(items_orden_entrega.ITEM_oe) + STRING(documentos_oe.id_documento_oe).
  
          chNode     = pchTree:Nodes:ADD(chOE:KEY, 4, cKey, cTxt, cIco).
          chNode:TAG = STRING(documentos_oe.id_documento_oe).
  
        END.
  
  
  
      END.

  END.

      


  RELEASE documentos_oe.



  /*********************************************/
/*
  FOR EACH items_contratos
      WHERE items_contratos.semana_entrega >= piWeek
        AND items_contratos.anio = YEAR(dDesde)
      NO-LOCK.

    FIND FIRST contratos OF items_contratos NO-LOCK NO-ERROR.
    FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
        
    cKey = "parte" + items_contratos.id_contrato + "-" + STRING(items_contratos.ITEM).
    cTxt = items_contratos.id_contrato + " - " + STRING(items_contratos.ITEM) + " " + clientes.razon_social.
    
    IF items_contratos.pendiente THEN DO:      
      chNode = pchTree:Nodes:ADD(chOpen:KEY, 4, cKey, cTxt, "contrato").

    END.
    ELSE DO:
      chNode = pchTree:Nodes:ADD(chClose:KEY, 4, cKey, cTxt, "contrato").
    END.

    FOR FIRST items_orden_entrega 
          OF items_contratos 
        NO-LOCK.
      
      cKey   = "orden" + items_orden_entrega.id_contrato + "-" + STRING(items_orden_entrega.ITEM) + "-" + STRING(items_orden_entrega.id_orden_entrega).
      cTxt   = STRING(items_orden_entrega.id_orden_entrega) + " Por " + STRING(items_orden_entrega.cantidad_tambores) + " en semana " + STRING(items_orden_entrega.semana_entrega).
      chOE   = pchTree:Nodes:ADD(chNode:KEY, 4, cKey, cTxt, "oe").

      FOR EACH documentos_oe
          WHERE documentos_oe.id_orden_entrega = items_orden_entrega.id_orden_entrega
          NO-LOCK.

        cIco = "no".
        IF documentos_oe.fecha_pedido <> ? AND documentos_oe.fecha_cumplido <> ? THEN cIco = "si".
        IF documentos_oe.fecha_pedido = ? AND documentos_oe.fecha_cumplido = ? THEN cIco = "no".
        IF documentos_oe.fecha_pedido <> ? AND documentos_oe.fecha_cumplido = ? THEN cIco = "pedido".
        IF documentos_oe.fecha_pedido = ? AND documentos_oe.fecha_cumplido <> ? THEN cIco = "no".
        
        cTxt   = documentos_oe.descripcion.
        IF documentos_oe.fecha_pedido <> ? THEN cTxt = cTxt + " pedido el: " + STRING(documentos_oe.fecha_pedido).
        IF documentos_oe.fecha_cumplido <> ? THEN cTxt = cTxt + " cumplido el: " + STRING(documentos_oe.fecha_cumplido).
        
        cKey   = "documento" + STRING(documentos_oe.id_orden_entrega) + STRING(items_orden_entrega.ITEM) + STRING(documentos_oe.id_documento_oe).

        chNode     = pchTree:Nodes:ADD(chOE:KEY, 4, cKey, cTxt, cIco).
        chNode:TAG = STRING(documentos_oe.id_documento_oe).

      END.



    END.


      
  END.
*/  

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

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exitAction").  
  SUBSCRIBE TO "tlbExit" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "printAction").  
  SUBSCRIBE TO "tlbPrint" IN h_dyntoolbar.

  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "exportExcelAction").  
  SUBSCRIBE TO "tlbExcel" IN h_dyntoolbar.

  chTreePrev = chCtrlFrame:TreeView.
  chTreeCurr = chCtrlFrame-2:TreeView.
  chTreeNext = chCtrlFrame-3:TreeView.
  chImage    = chCtrlFrame-4:ImageList.

  chTreePrev:ImageList = chImage.
  chTreeCurr:ImageList = chImage.
  chTreeNext:ImageList = chImage.

  DEFINE VARIABLE hLibCom AS HANDLE.
  DEFINE VARIABLE iSem    AS INTEGER    NO-UNDO.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  iSem = DYNAMIC-FUNCTION('getNroSemana' IN hLibCom, TODAY).

  RUN fillTreeView (chTreePrev, iSem - 1).
  RUN fillTreeView (chTreeCurr, iSem).
  RUN fillTreeView (chTreeNext, iSem + 1).




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE makeTransparent wWin 
PROCEDURE makeTransparent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR stat AS INTEGER NO-UNDO.
  DO WITH FRAME {&FRAME-NAME} :
    ASSIGN slider-1.
    RUN SetLayeredWindowAttributes(thisHwnd, 
                                   color-table:GET-RGB-VALUE(11), 
                                   slider-1, 
                                   2, 
                                   OUTPUT stat).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTopMost wWin 
PROCEDURE setTopMost :
/*------------------------------------------------------------------------------
  Purpose:     prevents overlapping on other applications
  Parameters:  logical TopMost : yes = switch TopMost on
                                 no  = switch TopMost off
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER TopMost AS LOGICAL NO-UNDO.
 
  DEFINE VARIABLE hNonClient AS INTEGER NO-UNDO.
  DEFINE VARIABLE hwndInsertAfter AS INTEGER NO-UNDO.
  DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
 
  hNonClient = GetParent({&window-name}:HWND).
  IF TopMost THEN 
     hwndInsertAfter = {&HWND_TOPMOST}.
  ELSE
     hwndInsertAfter = {&HWND_NOTOPMOST}.
 
  RUN SetWindowPos IN hpApi
    ( hNonClient,
      hwndInsertAfter,
      0,0,0,0,    /* x,y,width,height : will be ignored */
      {&SWP_NOMOVE} + {&SWP_NOSIZE} + {&SWP_NOACTIVATE},
      OUTPUT ReturnValue
    ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExcel wWin 
PROCEDURE tlbExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iWeek   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRgo    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDocS   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDocN   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dDesde  AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta  AS DATE       NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.

  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  
  RELEASE documentos_oe.

  iWeek  = iSelectedWeek.
  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iWeek, YEAR(TODAY)).
  dDesde = DATE(ENTRY(1, cRgo)).
  dHasta = DATE(ENTRY(2, cRgo)).

  FOR EACH ttEmbarques.
    DELETE ttEmbarques.
  END.

  FOR EACH orden_entrega
      WHERE orden_entrega.fecha_embarque >= dDesde
        AND orden_entrega.fecha_embarque <= dHasta
      NO-LOCK, 
      EACH items_orden_entrega 
        OF orden_entrega
      NO-LOCK.

    FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST calidades OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST agencias WHERE agencias.id_agencia =  orden_entrega.id_agencia NO-LOCK NO-ERROR.
    FIND FIRST vapores OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST destinos OF orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST tipo_contenedor OF items_orden_entrega NO-LOCK NO-ERROR.

    CREATE ttEmbarques.    
    ASSIGN ttEmbarques.id_orden_entrega = items_orden_entrega.id_orden_entrega
           ttEmbarques.cliente          = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NONE"
           ttEmbarques.producto         = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
           ttEmbarques.agencia          = IF AVAILABLE agencias THEN agencias.descripcion ELSE "NONE"
           ttEmbarques.vapor            = IF AVAILABLE vapores THEN vapores.descripcion ELSE "NONE"
           ttEmbarques.cantidad         = items_orden_entrega.contenedores
           ttEmbarques.tipo_contenedor  = IF AVAILABLE tipo_contenedor THEN tipo_contenedor.descripcion ELSE "NONE"
           ttEmbarques.booking          = orden_entrega.booking
           ttEmbarques.destino          = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE"
           ttEmbarques.cutoff           = orden_entrega.fecha_cut_off
           ttEmbarques.observaciones    = orden_entrega.observaciones
           .

    FOR EACH documentos_oe
          OF orden_entrega
        NO-LOCK.
      cDocS = "".
      IF documentos_oe.fecha_cumplido <> ? THEN 
        cDocS = "X".
      CASE documentos_oe.id_tipo_documento.
        WHEN 1 THEN ttEmbarques.inal        = cDocS.
        WHEN 2 THEN ttEmbarques.orden       = cDocS.
        WHEN 3 THEN ttEmbarques.reserva     = cDocS.
        WHEN 4 THEN ttEmbarques.asociacion  = cDocS.
        WHEN 5 THEN ttEmbarques.remito      = cDocS.
        WHEN 6 THEN ttEmbarques.fax         = cDocS.
      END CASE.
    END.
  
    

  END.
  
  RUN generateExcel.p (TABLE ttEmbarques,
                       " Estados OEs ",
                       " ",
                       7,
                       8,
                       "Arial",
                       8).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExit wWin 
PROCEDURE tlbExit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

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
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cRgo    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dDesde  AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta  AS DATE       NO-UNDO.
  DEFINE VARIABLE iWeek   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil    AS DECIMAL    NO-UNDO.


  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  iWeek  = INTEGER(chTreeCurr:NODES(1):TAG).
  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iWeek, YEAR(TODAY)).
  dDesde = DATE(ENTRY(1, cRgo)).
  dHasta = DATE(ENTRY(2, cRgo)).

  FOR EACH ttInal.
    DELETE ttInal.
  END.

  RELEASE documentos_oe.

  FOR EACH documentos_oe
      WHERE documentos_oe.fecha_cumplido    = ?
        AND documentos_oe.id_tipo_documento = 1, 
      EACH orden_entrega 
        OF documentos_oe 
      WHERE orden_entrega.fecha_embarque >= dDesde
      NO-LOCK, 
      FIRST items_orden_entrega 
         OF orden_entrega 
      WHERE (items_orden_entrega.id_articulo = 52 OR items_orden_entrega.id_articulo = 53 OR items_orden_entrega.id_articulo = 71) 
      NO-LOCK .

    FIND FIRST destinos OF orden_entrega NO-LOCK NO-ERROR.
    
    FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST calidades OF items_orden_entrega NO-LOCK NO-ERROR.

    /*estimacion de kilos */
    fKil = 0.
    IF AVAILABLE calidades THEN DO:
      IF calidades.descripcion MATCHES "*400*" THEN
        fKil = 250 * items_orden_entrega.tambores_pedidos.
      
      IF calidades.descripcion MATCHES "*480*" THEN
        fKil = 250 * items_orden_entrega.tambores_pedidos.
      
      IF calidades.descripcion MATCHES "*500*" THEN
        fKil = 260 * items_orden_entrega.tambores_pedidos.

      IF calidades.descripcion MATCHES "*pulp*" THEN
        fKil = 19.60 * items_orden_entrega.tambores_pedidos.

    END.

    CREATE ttInal.
    ASSIGN ttInal.id_orden_entrega  = documentos_oe.id_orden_entrega
           ttInal.cliente           = IF AVAILABLE clientes THEN clientes.razon_social ELSE ""
           ttInal.destino_final     = IF AVAILABLE destinos THEN destinos.descripcion ELSE ""
           ttInal.producto          = IF AVAILABLE calidades THEN calidades.descripcion ELSE ""
           ttInal.tambores          = items_orden_entrega.tambores_pedidos
           ttInal.kilos_netos       = fKil
           .

    /*marca como pedidos los inales cuando tira el listado*/
    ASSIGN documentos_oe.fecha_pedido   = TODAY
           documentos_oe.fecha_cumplido = TODAY.
           
        
  END.

  RUN generateExcel.p (INPUT TABLE ttInal,
                         INPUT " Inales a Solicitar ",
                         INPUT " ",
                         INPUT 7,
                         INPUT 8,
                         INPUT "Arial",
                         INPUT 8).

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
