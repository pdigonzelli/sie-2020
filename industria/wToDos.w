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

DEFINE VARIABLE chTreeView  AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chImage     AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chNode      AS COM-HANDLE     NO-UNDO.

DEFINE VARIABLE iWeekPrev AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWeekCurr AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWeekNext AS INTEGER    NO-UNDO.

DEFINE VARIABLE hUsr AS HANDLE     NO-UNDO.

/*----- memptr for use with API call -----*/
def var mpt_mousexy as memptr no-undo.

/*----- define space for 2 long integers -----*/
set-size(mpt_mousexy) = 8.

/*----- result code from API call -----*/
def var rc as int no-undo.

def var WasInFrame as logical no-undo initial no.



/*DLL Calls*/
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

PROCEDURE GetCursorPos EXTERNAL "user32":
  DEFINE INPUT  PARAMETER  lpPoint     AS LONG. /* memptr */
  DEFINE RETURN PARAMETER  ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE ScreenToClient EXTERNAL "user32" :
   DEFINE INPUT  PARAMETER hWnd        AS LONG.
   DEFINE INPUT  PARAMETER lpPoint     AS LONG. /* memptr */
   DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.


/*para el menu contextual sobre el ocx treeview */
PROCEDURE SendMessageA EXTERNAL "USER32.DLL":
       DEFINE INPUT PARAMETER win-handle AS LONG NO-UNDO.
       DEFINE INPUT PARAMETER win-msg    AS LONG NO-UNDO.
       DEFINE INPUT PARAMETER win-param1 AS LONG NO-UNDO.
       DEFINE INPUT PARAMETER win-param2 AS LONG NO-UNDO.
   END PROCEDURE.

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
&Scoped-Define ENABLED-OBJECTS fiSemanaDesde fiSemanaHasta fiAnio ~
btnExcel-2 SLIDER-1 btnFilter btnRefresh btnExcel btnExit btnPrint btn1 ~
btn2 btn3 RECT-35 
&Scoped-Define DISPLAYED-OBJECTS fiSemanaDesde fiSemanaHasta fiAnio ~
SLIDER-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUserLib wWin 
FUNCTION getUserLib RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queryName wWin 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-4 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-4 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn1  NO-FOCUS FLAT-BUTTON
     LABEL "Semana" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn2  NO-FOCUS FLAT-BUTTON
     LABEL "Semana" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn3  NO-FOCUS FLAT-BUTTON
     LABEL "Semana" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnExcel 
     IMAGE-UP FILE "src/adm2/image/excel.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 5.8 BY .95.

DEFINE BUTTON btnExcel-2 
     IMAGE-UP FILE "src/adm2/image/1026.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 5.8 BY .95.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "src/adm2/image/exit.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 5.8 BY .95.

DEFINE BUTTON btnFilter 
     IMAGE-UP FILE "src/adm2/image/filter.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 5.8 BY .95.

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "src/adm2/image/print.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 5.8 BY .95.

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "src/adm2/image/299.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 5.8 BY .95 TOOLTIP "Recargar Datos (Refrescar)".

DEFINE VARIABLE fiAnio AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiSemanaDesde AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Semana" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiSemanaHasta AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86.6 BY 1.19.

DEFINE VARIABLE SLIDER-1 AS INTEGER INITIAL 195 
     VIEW-AS SLIDER MIN-VALUE 50 MAX-VALUE 255 HORIZONTAL 
     TIC-MARKS NONE 
     SIZE 21 BY 2.38 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiSemanaDesde AT ROW 1.1 COL 56.8 COLON-ALIGNED
     fiSemanaHasta AT ROW 1.1 COL 64.2 COLON-ALIGNED NO-LABEL
     fiAnio AT ROW 1.1 COL 71.6 COLON-ALIGNED NO-LABEL
     btnExcel-2 AT ROW 1.1 COL 13.4
     SLIDER-1 AT ROW 7.81 COL 66.6 NO-LABEL
     btnFilter AT ROW 1.1 COL 81.2
     btnRefresh AT ROW 1.1 COL 19.4
     btnExcel AT ROW 1.1 COL 1.4
     btnExit AT ROW 1.1 COL 25.8
     btnPrint AT ROW 1.1 COL 7.2
     btn1 AT ROW 9.05 COL 2
     btn2 AT ROW 9.05 COL 18.8
     btn3 AT ROW 9.05 COL 35
     RECT-35 AT ROW 1 COL 1
     SPACE(0.00) SKIP(8.01)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         TITLE              = "To Do's"
         HEIGHT             = 9.38
         WIDTH              = 87.2
         MAX-HEIGHT         = 32.91
         MAX-WIDTH          = 203.6
         VIRTUAL-HEIGHT     = 32.91
         VIRTUAL-WIDTH      = 203.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = 26
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
   Size-to-Fit                                                          */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE
       FRAME fMain:RESIZABLE        = TRUE.

ASSIGN 
       SLIDER-1:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 2.24
       COLUMN          = 1
       HEIGHT          = 6.62
       WIDTH           = 86.6
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-4 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 8.29
       COLUMN          = 51
       HEIGHT          = 1.91
       WIDTH           = 8
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 8.76
       COLUMN          = 60
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
      CtrlFrame-4:NAME = "CtrlFrame-4":U .
/* CtrlFrame-4 OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(fiAnio:HANDLE IN FRAME fMain).
      CtrlFrame-4:MOVE-AFTER(SLIDER-1:HANDLE IN FRAME fMain).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame-4).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* To Do's */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* To Do's */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* To Do's */
DO:
  RUN onWindowResize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESTORED OF wWin /* To Do's */
DO:
  /*RUN onWindowResize.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn1 wWin
ON CHOOSE OF btn1 IN FRAME fMain /* Semana */
DO:
  IF VALID-HANDLE(hUsr) THEN
    RUN clearTreeView.
    RUN fillTreeView IN hUsr (chTreeView, 
                              STRING(iWeekPrev) + "," + STRING(fiAnio:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2 wWin
ON CHOOSE OF btn2 IN FRAME fMain /* Semana */
DO:
  IF VALID-HANDLE(hUsr) THEN DO:
    RUN clearTreeView.
    RUN fillTreeView IN hUsr (chTreeView, 
                              STRING(iWeekCurr) + "," + STRING(fiAnio:SCREEN-VALUE)).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn3 wWin
ON CHOOSE OF btn3 IN FRAME fMain /* Semana */
DO:
  IF VALID-HANDLE(hUsr) THEN
    RUN clearTreeView.
    RUN fillTreeView IN hUsr (chTreeView, 
                              STRING(iWeekNext) + "," + STRING(fiAnio:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcel wWin
ON CHOOSE OF btnExcel IN FRAME fMain /* ... */
DO:
  IF VALID-HANDLE(hUsr) THEN
    RUN buttonAction1 IN hUsr (chTreeView:SelectedItem:TAG) NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExcel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcel-2 wWin
ON CHOOSE OF btnExcel-2 IN FRAME fMain /* ... */
DO:
  IF VALID-HANDLE(hUsr) THEN
    RUN buttonAction3 IN hUsr (chTreeView:SelectedItem:TAG) NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit wWin
ON CHOOSE OF btnExit IN FRAME fMain /* ... */
DO:
  APPLY "WINDOW-CLOSE":U TO wWin.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter wWin
ON CHOOSE OF btnFilter IN FRAME fMain /* ... */
DO:
  DEFINE VARIABLE iSemDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cArgs  AS CHARACTER  NO-UNDO.

  ASSIGN  iSemDes   = INTEGER(fiSemanaDesde:SCREEN-VALUE)
          iSemHas   = INTEGER(fiSemanaHasta:SCREEN-VALUE)
          iWeekPrev = iSemDes - 1
          iWeekCurr = iSemDes
          iWeekNext = iSemDes + 1 . 

  ASSIGN  btn1:LABEL IN FRAME {&FRAME-NAME} = "Semana " + STRING(iWeekPrev)
          btn2:LABEL IN FRAME {&FRAME-NAME} = "Semana " + STRING(iWeekCurr)
          btn3:LABEL IN FRAME {&FRAME-NAME} = "Semana " + STRING(iWeekNext).

  IF VALID-HANDLE(hUsr) THEN DO:
    cArgs = STRING(iSemDes) + "," + STRING(iSemHas) + "," + fiAnio:SCREEN-VALUE.
    RUN clearTreeView.
    RUN fillTreeView IN hUsr (chTreeView, cArgs).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint wWin
ON CHOOSE OF btnPrint IN FRAME fMain /* ... */
DO:
  IF VALID-HANDLE(hUsr) THEN
    RUN buttonAction2 IN hUsr (chTreeView:SelectedItem:TAG) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh wWin
ON CHOOSE OF btnRefresh IN FRAME fMain /* ... */
DO:
  APPLY "CHOOSE" TO btnFilter IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.DblClick
PROCEDURE CtrlFrame.TreeView.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  IF VALID-HANDLE(hUsr) THEN
    RUN nodeDoubleClick IN hUsr (chTreeView:SELECTEDITEM, "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.MouseUp
PROCEDURE CtrlFrame.TreeView.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.

IF p-Button = 2 THEN
       RUN SendMessageA (SELF:HWND, 517, 0, 0).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.NodeClick
PROCEDURE CtrlFrame.TreeView.NodeClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.
  
  IF VALID-HANDLE(hUsr) THEN DO:
    RUN nodeClick IN hUsr (p-Node, "").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 wWin OCX.Tick
PROCEDURE CtrlFrame-2.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  
/*process events.*/

def var InFrame as logical no-undo.

/*----- Win32 API call -----*/
run GetCursorPos(INPUT GET-POINTER-VALUE(mpt_mousexy),
                 OUTPUT rc) NO-ERROR.
/*
/*----- show the mouse X/Y coordinates -----*/
assign fil_x:screen-value in frame {&frame-name} = string(get-long(mpt_mousexy,1))
       fil_y:screen-value                        = string(get-long(mpt_mousexy,5)).
*/
/* also show the client coordinates relative to frame {&frame-name} */
run ScreenToClient (frame {&frame-name}:hWnd,
                    INPUT GET-POINTER-VALUE(mpt_mousexy),
                    OUTPUT rc) NO-ERROR.
/*
assign fil_x_cl:screen-value in frame {&frame-name} = string(get-long(mpt_mousexy,1))
       fil_y_cl:screen-value    
                           = string(get-long(mpt_mousexy,5)).
*/
/* test if mouse is over the frame */
InFrame = (     get-long(mpt_mousexy,1)>=0
           and get-long(mpt_mousexy,1)<=frame {&frame-name}:width-pixels 
           and get-long(mpt_mousexy,5)>=0
           and get-long(mpt_mousexy,5)<=frame {&frame-name}:height-pixels).

if InFrame<>WasInFrame then 
   if not WasInFrame then do:
      WasInFrame = yes.
      run FrameMouseEnter.
   end.
   else do:
      WasInFrame = no.
      run FrameMouseLeave.
   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-1 wWin
ON VALUE-CHANGED OF SLIDER-1 IN FRAME fMain
DO:
  RUN makeTransparent (slider-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/*{src/adm2/windowmn.i}*/
/*    
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.           
                                             */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.*/

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


MAIN-BLOCK:
  
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
  /*RUN CONTROL_load.*/
  RUN ENABLE_UI.

  RUN GetParent IN hpapi (wWin:HWND, OUTPUT thishwnd).
  
  DEF VAR stat AS INTEGER NO-UNDO.
  DEF VAR flgs AS INTEGER NO-UNDO.
    
  RUN GetWindowLongA(thishwnd, {&GWL_EXSTYLE}, OUTPUT flgs) .
  RUN SetWindowLongA(thishwnd, {&GWL_EXSTYLE}, flgs + {&WS_EX_LAYERED},OUTPUT stat).

  RUN initializeObject NO-ERROR.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  

END.


  
  /*estilo paleta*/
  /*DEFINE VARIABLE hStyle AS HANDLE NO-UNDO.
  RUN WinStyle.p PERSISTENT SET hStyle.
  RUN AddPaletteStyle IN hStyle ({&window-name}:HWND).
  DELETE PROCEDURE hStyle.*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearTreeView wWin 
PROCEDURE clearTreeView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  chTreeView:Nodes:CLEAR().

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

OCXFile = SEARCH( "wtodos.wrx":U ).
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
    chCtrlFrame-4 = CtrlFrame-4:COM-HANDLE
    UIB_S = chCtrlFrame-4:LoadControls( OCXFile, "CtrlFrame-4":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wtodos.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY fiSemanaDesde fiSemanaHasta fiAnio SLIDER-1 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fiSemanaDesde fiSemanaHasta fiAnio btnExcel-2 SLIDER-1 btnFilter 
         btnRefresh btnExcel btnExit btnPrint btn1 btn2 btn3 RECT-35 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frameMouseEnter wWin 
PROCEDURE frameMouseEnter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN makeTransparent (255).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frameMouseLeave wWin 
PROCEDURE frameMouseLeave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN makeTransparent (slider-1).
 
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

  RUN SUPER.

  RUN setEnviroment.
  RUN makeTransparent (slider-1).

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
  DEFINE INPUT  PARAMETER piValue AS INTEGER    NO-UNDO.

  DEFINE VARIABLE stat AS INTEGER    NO-UNDO.

  DO WITH FRAME {&FRAME-NAME} :
    ASSIGN slider-1
           .
    RUN SetLayeredWindowAttributes(thisHwnd, 
                                   COLOR-TABLE:GET-RGB-VALUE(11), 
                                   piValue, 
                                   2, 
                                   OUTPUT stat).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onWindowResize wWin 
PROCEDURE onWindowResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  FRAME fMain:MAX-WIDTH-PIXELS      = wWin:WIDTH-PIXELS.
  FRAME fMain:MAX-HEIGHT-PIXELS     = wWin:WIDTH-PIXELS.
  FRAME fMain:VIRTUAL-WIDTH-PIXELS  = wWin:WIDTH-PIXELS.
  FRAME fMain:VIRTUAL-HEIGHT-PIXELS = wWin:WIDTH-PIXELS.

  FRAME fMain:WIDTH-PIXELS  = wWin:WIDTH-PIXELS.
  FRAME fMain:HEIGHT-PIXELS = wWin:HEIGHT-PIXELS.

  rect-35:WIDTH-PIXELS IN FRAME {&FRAME-NAME} = FRAME fMain:WIDTH-PIXELS.
  CtrlFrame:WIDTH-PIXELS                      = FRAME fMain:WIDTH-PIXELS - 2 NO-ERROR.
  ctrlFrame:HEIGHT-PIXELS                     = FRAME fMain:HEIGHT-PIXELS - 54.
  
  btn1:Y      = CtrlFrame:Y + CtrlFrame:HEIGHT-PIXELS + 2.
  btn2:Y      = btn1:Y.
  btn3:Y      = btn1:Y.
  slider-1:Y  = btn1:Y - 26.
  slider-1:X  = FRAME fMain:WIDTH-PIXELS - slider-1:WIDTH-PIXELS - 2.

  btnFilter:X = FRAME fMain:WIDTH-PIXELS - btnFilter:WIDTH-PIXELS - 2.
  fiAnio:X    = btnFilter:X - 48.
  fiSemanaHasta:X  = fiAnio:X - 38.
  fiSemanaDesde:X  = fiSemanaHasta:X - 38.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repositionWindow wWin 
PROCEDURE repositionWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  wWin:X = SESSION:WIDTH-PIXELS - wWin:WIDTH-PIXELS - 10.
  wWin:Y = CURRENT-WINDOW:Y + CURRENT-WINDOW:HEIGHT-PIXELS * 1.7.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEnviroment wWin 
PROCEDURE setEnviroment :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLibCom AS HANDLE.
  DEFINE VARIABLE iSemHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cArgs   AS CHARACTER  NO-UNDO.

  hUsr = getUserLib().
  wWin:RESIZE = TRUE.
  RUN setPopupMenu.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  ASSIGN  iSemDes      = DYNAMIC-FUNCTION('getNroSemana' IN hLibCom, TODAY)
          iSemHas      = iSemDes
          iWeekPrev = iSemDes - 1
          iWeekCurr = iSemDes
          iWeekNext = iSemDes + 1  
          .

  chTreeView            = chCtrlFrame:TreeView.
  chImage               = chCtrlFrame-4:ImageList.
  chTreeView:ImageList  = chImage.


  ASSIGN  btn1:LABEL IN FRAME {&FRAME-NAME}             = "Semana " + STRING(iWeekPrev)
          btn2:LABEL IN FRAME {&FRAME-NAME}             = "Semana " + STRING(iWeekCurr)
          btn3:LABEL IN FRAME {&FRAME-NAME}             = "Semana " + STRING(iWeekNext)        
          fiSemanaDesde:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iSemDes)
          fiSemanaHasta:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(iSemHas)
          fiAnio:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(YEAR(TODAY))
          slider-1:SCREEN-VALUE                         = "180"
          .
          
 
  IF VALID-HANDLE(hUsr) THEN DO:

    RUN repositionWindow.
     
    wWin:TITLE = "To Do's" + " - " + 
                 DYNAMIC-FUNCTION('getUsuario' IN hUsr) + " - " +
                 DYNAMIC-FUNCTION('getLibName' IN hUsr).

    cArgs = STRING(iSemDes) + "," + STRING(iSemHas) + "," + fiAnio:SCREEN-VALUE.
    RUN clearTreeView.
    /*  
    RUN fillTreeView IN hUsr (chTreeView, cArgs).*/

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPopupMenu wWin 
PROCEDURE setPopupMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hMnu AS HANDLE     NO-UNDO.

  IF NOT VALID-HANDLE(hUsr) THEN RETURN.

  hMnu = DYNAMIC-FUNCTION('getPopupMenu' IN hUsr, hMnu).

  IF VALID-HANDLE(hMnu) THEN DO:
    THIS-PROCEDURE:POPUP-MENU = hMnu:HANDLE.
    CtrlFrame:POPUP-MENU      = hMnu:HANDLE.
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUserLib wWin 
FUNCTION getUserLib RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cUsrLib AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  IF USERID('userdb') <> 'computos' AND USERID('userdb') <> "" THEN
    cUsrLib = "..\industria\lib" + SUBSTRING(USERID("userdb"), 3) + ".r".
  ELSE
    cUsrLib = "..\industria\librvelez.r".

  IF SEARCH(cUsrLib) <> ? THEN  
    hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, cUsrLib).

  DELETE OBJECT hLibCom.

  RETURN hLib.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

