&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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

DEFINE VARIABLE vhLib AS HANDLE     NO-UNDO.

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

DEFINE VARIABLE chStatus AS COM-HANDLE     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btn01 RECT-2 RECT-3 btn02 RECT-4 RECT-5 ~
btn03 fiProg btn04 btn05 btn06 btn07 btn08 btn09 btn10 btn11 btn12 btn13 ~
btnExit btnRun 
&Scoped-Define DISPLAYED-OBJECTS fiProg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD exploreMenu wWin 
FUNCTION exploreMenu RETURNS CHARACTER
  (INPUT pcLetraInicial  AS CHARACTER, 
   INPUT pcItem          AS CHARACTER, 
   INPUT piItemPrimero   AS INTEGER,
   INPUT-OUTPUT pcMenu   AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPhater wWin 
FUNCTION getPhater RETURNS CHARACTER
  (INPUT pcLetra        AS CHARACTER, 
   INPUT pcItem         AS CHARACTER,
   INPUT-OUTPUT pcPath  AS CHARACTER )  FORWARD.

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

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn01  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn02  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05 TOOLTIP "Producciones de Aceites".

DEFINE BUTTON btn03  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn04  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn05  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn06  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn07  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn08  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn09  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn10  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn11  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn12  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btn13  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.8 BY 1.05.

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "src/adm2/image/exit.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 40" 
     SIZE 4.8 BY 1.05 TOOLTIP "Salir".

DEFINE BUTTON btnRun 
     IMAGE-UP FILE "src/adm2/image/299.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnexit 2" 
     SIZE 4.8 BY 1.05.

DEFINE VARIABLE fiProg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Programa" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 1.24.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 1.24.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.4 BY 1.24.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.4 BY 1.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn01 AT ROW 1.1 COL 1.6
     btn02 AT ROW 1.1 COL 6.6
     btn03 AT ROW 1.1 COL 11.8
     fiProg AT ROW 2.43 COL 10 COLON-ALIGNED
     btn04 AT ROW 1.1 COL 16.8
     btn05 AT ROW 1.1 COL 21.8
     btn06 AT ROW 1.1 COL 27.4
     btn07 AT ROW 1.1 COL 32.4
     btn08 AT ROW 1.1 COL 37.4
     btn09 AT ROW 1.1 COL 42.4
     btn10 AT ROW 1.1 COL 48.4
     btn11 AT ROW 1.1 COL 53.4
     btn12 AT ROW 1.1 COL 58.4
     btn13 AT ROW 1.1 COL 63.4
     btnExit AT ROW 1.1 COL 69.2
     btnRun AT ROW 2.38 COL 56
     RECT-2 AT ROW 1 COL 1
     RECT-3 AT ROW 1 COL 68.6
     RECT-4 AT ROW 1 COL 26.6
     RECT-5 AT ROW 1 COL 47.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.2 BY 3.62.


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
         TITLE              = "Sistema de Industria"
         HEIGHT             = 3.62
         WIDTH              = 77.2
         MAX-HEIGHT         = 34.33
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.33
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
       btn09:PRIVATE-DATA IN FRAME fMain     = 
                "wRemitos.w".

ASSIGN 
       btn10:PRIVATE-DATA IN FRAME fMain     = 
                "w_consulta_stock_rapido_fam.w".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Sistema de Industria */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Sistema de Industria */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn01 wWin
ON CHOOSE OF btn01 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn02
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn02 wWin
ON CHOOSE OF btn02 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn03
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn03 wWin
ON CHOOSE OF btn03 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn04
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn04 wWin
ON CHOOSE OF btn04 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn05
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn05 wWin
ON CHOOSE OF btn05 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn06
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn06 wWin
ON CHOOSE OF btn06 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn07
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn07 wWin
ON CHOOSE OF btn07 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn08
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn08 wWin
ON CHOOSE OF btn08 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn09
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn09 wWin
ON CHOOSE OF btn09 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn10 wWin
ON CHOOSE OF btn10 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn11 wWin
ON CHOOSE OF btn11 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn12 wWin
ON CHOOSE OF btn12 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn13 wWin
ON CHOOSE OF btn13 IN FRAME fMain
DO:
  RUN onMenuChoice IN hLib (SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit wWin
ON CHOOSE OF btnExit IN FRAME fMain /* Button 40 */
DO:  
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRun wWin
ON CHOOSE OF btnRun IN FRAME fMain /* btnexit 2 */
DO:
  IF fiProg:SCREEN-VALUE = "_edit.p" AND USERID('userdb') <> "y_facundoj"  THEN DO:
    MESSAGE "Ud. No esta autorizado para ejecutar esta operacion"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  RUN VALUE(fiProg:SCREEN-VALUE) NO-ERROR.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiProg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiProg wWin
ON RETURN OF fiProg IN FRAME fMain /* Programa */
DO:
  APPLY "choose" TO btnRun IN FRAME {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createButtons wWin 
PROCEDURE createButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableToolbarButtons wWin 
PROCEDURE enableToolbarButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcUsuario AS CHARACTER  NO-UNDO.

  btn01:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn01:PRIVATE-DATA, pcUsuario).
  btn02:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn02:PRIVATE-DATA, pcUsuario).
  btn03:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn03:PRIVATE-DATA, pcUsuario).
  btn04:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn04:PRIVATE-DATA, pcUsuario).
  btn05:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn05:PRIVATE-DATA, pcUsuario).

  btn06:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn06:PRIVATE-DATA, pcUsuario).
  btn07:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn07:PRIVATE-DATA, pcUsuario).
  btn08:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn08:PRIVATE-DATA, pcUsuario).
  btn09:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn09:PRIVATE-DATA, pcUsuario).

  btn10:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn10:PRIVATE-DATA, pcUsuario).
  btn11:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn11:PRIVATE-DATA, pcUsuario).
  btn12:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn12:PRIVATE-DATA, pcUsuario).
  /*btn13:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, btn13:PRIVATE-DATA, pcUsuario).*/
  
  /*btnTrazabilidad:SENSITIVE IN FRAME fMain = DYNAMIC-FUNCTION('canRun' IN hLib, "wTrazaIndust.w", pcUsuario).*/

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
  DISPLAY fiProg 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE btn01 RECT-2 RECT-3 btn02 RECT-4 RECT-5 btn03 fiProg btn04 btn05 btn06 
         btn07 btn08 btn09 btn10 btn11 btn12 btn13 btnExit btnRun 
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
  /* RUN splashdemo.p.*/

  RUN SUPER.


  /*reposiciona pantalla*/
  CURRENT-WINDOW:X = SESSION:WIDTH-PIXELS - CURRENT-WINDOW:WIDTH-PIXELS - 10.
  CURRENT-WINDOW:Y = 10.

  MESSAGE PROPATH VIEW-AS ALERT-BOX.  
  RUN libApiMenu.p PERSISTENT SET hLib.
  RUN setEnviroment.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE launchToDos wWin 
PROCEDURE launchToDos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*
  RUN wToDos.w PERSISTENT SET hPalette.
  RUN SetTopMost IN hPalette(YES) NO-ERROR.
  */
  /*RUN initializeObject IN hPalette NO-ERROR.*/
  

  RUN onMenuChoice IN hLib ("wToDos.w").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE menuAction wWin 
PROCEDURE menuAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcMenu AS CHARACTER  NO-UNDO.
  MESSAGE pcMenu VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuItemChoose wWin 
PROCEDURE MenuItemChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cMenuItemName AS CHARACTER NO-UNDO.

MESSAGE "Item Selected:" SKIP(1)
"'" + cMenuItemName + "'" VIEW-AS ALERT-BOX INFORMATION.

IF cMenuItemName = 'FileExit' THEN QUIT.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButtonsProperties wWin 
PROCEDURE setButtonsProperties :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cProps   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cProp    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cToolTip AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cProg    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cImage   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFile    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER    NO-UNDO.

  cFile = USERID("userdb") + ".toolbar".
  cProps = DYNAMIC-FUNCTION('getToolbarProperties' IN hLib, cFile). /*traigo la configuracion de los botones propia para el usuario logueado, sino traigo por defecto del .toolbar*/


  DO i = 1 TO NUM-ENTRIES(cProps, CHR(10)):
    cProp = ENTRY(i, cProps, CHR(10)).
    
    ASSIGN cLabel   = ENTRY(2, cProp, CHR(1))
           cToolTip = ENTRY(3, cProp, CHR(1))
           cProg    = ENTRY(5, cProp, CHR(1)) 
           cImage   = "src\adm2\image\" + ENTRY(4, cProp, CHR(1)) NO-ERROR.
    
    CASE i:
      WHEN 1 THEN DO:
        ASSIGN btn01:LABEL IN FRAME fMain        = cLabel
               btn01:TOOLTIP IN FRAME fMain      = cToolTip
               btn01:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn01:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 2 THEN DO:
        ASSIGN btn02:LABEL IN FRAME fMain        = cLabel
               btn02:TOOLTIP IN FRAME fMain      = cToolTip
               btn02:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn02:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 3 THEN DO:
        ASSIGN btn03:LABEL IN FRAME fMain        = cLabel
               btn03:TOOLTIP IN FRAME fMain      = cToolTip
               btn03:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn03:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 4 THEN DO:
        ASSIGN btn04:LABEL IN FRAME fMain        = cLabel
               btn04:TOOLTIP IN FRAME fMain      = cToolTip
               btn04:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn04:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 5 THEN DO:
        ASSIGN btn05:LABEL IN FRAME fMain        = cLabel
               btn05:TOOLTIP IN FRAME fMain      = cToolTip
               btn05:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn05:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 6 THEN DO:
        ASSIGN btn06:LABEL IN FRAME fMain        = cLabel
               btn06:TOOLTIP IN FRAME fMain      = cToolTip
               btn06:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn06:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 7 THEN DO:
        ASSIGN btn07:LABEL IN FRAME fMain        = cLabel
               btn07:TOOLTIP IN FRAME fMain      = cToolTip
               btn07:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn07:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 8 THEN DO:
        ASSIGN btn08:LABEL IN FRAME fMain        = cLabel
               btn08:TOOLTIP IN FRAME fMain      = cToolTip
               btn08:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn08:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 9 THEN DO:
        ASSIGN btn09:LABEL IN FRAME fMain        = cLabel
               btn09:TOOLTIP IN FRAME fMain      = cToolTip
               btn09:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn09:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 10 THEN DO:
        ASSIGN btn10:LABEL IN FRAME fMain        = cLabel
               btn10:TOOLTIP IN FRAME fMain      = cToolTip
               btn10:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn10:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 11 THEN DO:
        ASSIGN btn11:LABEL IN FRAME fMain        = cLabel
               btn11:TOOLTIP IN FRAME fMain      = cToolTip
               btn11:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn11:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 12 THEN DO:
        ASSIGN btn12:LABEL IN FRAME fMain        = cLabel
               btn12:TOOLTIP IN FRAME fMain      = cToolTip
               btn12:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn12:LOAD-IMAGE-UP(cImage).
      END.

      WHEN 13 THEN DO:
        ASSIGN btn13:LABEL IN FRAME fMain        = cLabel
               btn13:TOOLTIP IN FRAME fMain      = cToolTip
               btn13:PRIVATE-DATA IN FRAME fMain = cProg.           
               btn13:LOAD-IMAGE-UP(cImage).
      END.
    END CASE.
    
    

  END.



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
  DEFINE VARIABLE cUsuario AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFile    AS CHARACTER  NO-UNDO.

  SESSION:SUPPRESS-WARNINGS   = YES.
  cUsuario                    = SUBSTRING(USERID("userdb"), 3).
  
  RUN setMenuWindowHandle IN hLib (CURRENT-WINDOW).


  PROCESS EVENTS.

  /*menu*/
  DEFINE VARIABLE hMenu AS HANDLE     NO-UNDO.  
  hMenu = DYNAMIC-FUNCTION('populateMenu' IN hLib).
  DYNAMIC-FUNCTION('displayMenu' IN hLib, CURRENT-WINDOW, hMenu). 

  /*barra de botones*/
  RUN setButtonsProperties.

  /*permisos barra de botones*/
  RUN enableToolbarButtons(cUsuario).

  /*todo�s de usuario*/
  cFile = "..\industria\lib" + SUBSTRING(USERID('userdb'), 3) + ".r".

  IF SEARCH(cFile) <> ? THEN DO:
    RUN launchToDos.
  END.
  

  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRunningProc wWin 
PROCEDURE setRunningProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcProgName AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcFileName AS CHARACTER  NO-UNDO.
  /*
  chStatus:panels("prg"):TEXT         = pcProgName.
  chStatus:panels("prg"):TOOLTIPTEXT  = pcFileName.
  */  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbLotes wWin 
PROCEDURE tlbLotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wLotesJugo.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbTraza wWin 
PROCEDURE tlbTraza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wTrazaIndust.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE treeWalk wWin 
PROCEDURE treeWalk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER Grp AS WIDGET-HANDLE.
  DEF INPUT PARAMETER IntFlag AS INT.

  DEF VAR Flw AS WIDGET-HANDLE.

DO:
    Flw=Grp:FIRST-CHILD.
        DO WHILE (Flw <> ?).
            MESSAGE INTEGER(Grp) Grp:NAME Grp:TYPE SKIP
                    INTEGER(Flw) Flw:NAME Flw:TYPE SKIP
                    IntFlag.

            CASE IntFlag:
                WHEN 0 THEN RUN TreeWalk (Flw,1).
                WHEN 1 THEN RUN TreeWalk (Flw,2).
            END CASE.

            Flw=Flw:NEXT-SIBLING.       
        END.
    Grp=Grp:NEXT-SIBLING.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION exploreMenu wWin 
FUNCTION exploreMenu RETURNS CHARACTER
  (INPUT pcLetraInicial  AS CHARACTER, 
   INPUT pcItem          AS CHARACTER, 
   INPUT piItemPrimero   AS INTEGER,
   INPUT-OUTPUT pcMenu   AS CHARACTER):


  DEFINE VARIABLE viItemPrimero   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE c               AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMenu          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcItem          AS CHARACTER  NO-UNDO.

  vcMenu = pcMenu.
  FIND FIRST par_menu_grupos WHERE par_menu_grupos.letra_inicial  = pcLetraInicial
                               AND par_menu_grupos.ITEM_menu      = pcItem
                             NO-LOCK NO-ERROR.
  IF AVAILABLE par_menu_grupos THEN DO:
    viItemPrimero = INTEGER(par_menu_grupos.accion_seleccion) NO-ERROR.
    vcItem        = par_menu_grupos.ITEM_posterior.
    
    IF NOT ERROR-STATUS:ERROR AND viItemPrimero <> 0 THEN DO:  /*es una carpeta */
      vcMenu        = pcMenu + "\" + par_menu_grupos.dato_menu.
      viItemPrimero = INTEGER(par_menu_grupos.accion_seleccion).
      vcItem        = STRING(viItemPrimero, "999").
    END.      
    ELSE DO:  /*es un programa*/
      viItemPrimero = piItemPrimero.
    /*  
     DEFINE VAR dbg AS LOGICAL.
     dbg = DEBUGGER:INITIATE().
     dbg = DEBUGGER:SET-BREAK().
    */


      RUN createProgressShortCut IN vhLib ({&WINDOW-NAME}, 
                                           "\" + vcMenu + "\", 
                                           par_menu_grupos.dato_menu,
                                           par_menu_grupos.accion_seleccion_gui).
    END.
      
      
  END.
  IF par_menu_grupos.ITEM_posterior = STRING(piItemPrimero, "999") THEN DO:  /*recorrio todos los hijos*/
    RETURN pcMenu.  
  END.
  ELSE DO:
    /*
    DEFINE VAR dbg AS LOGICAL.
    dbg = DEBUGGER:INITIATE().
    dbg = DEBUGGER:SET-BREAK().
    */
    c = exploreMenu(pcLetraInicial, vcItem, viItemPrimero, INPUT-OUTPUT vcMenu).
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPhater wWin 
FUNCTION getPhater RETURNS CHARACTER
  (INPUT pcLetra        AS CHARACTER, 
   INPUT pcItem         AS CHARACTER,
   INPUT-OUTPUT pcPath  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  dado un nodo cualquiera busca las carpetas de las que cuelga hasta 
            llegar al primer nivel 
    Notes:  
------------------------------------------------------------------------------*/
  
  DEFINE VARIABLE i      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vcItem AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE c      AS CHARACTER  NO-UNDO.
  DEFINE BUFFER pm FOR par_menu_grupos.
  /*
  DEFINE VAR dbg AS LOGICAL.
  dbg = DEBUGGER:INITIATE().
  dbg = DEBUGGER:SET-BREAK().
  */
  /*busco el item*/
  FIND FIRST par_menu_grupos WHERE par_menu_grupos.letra_inicial = pcLetra
                               AND par_menu_grupo.ITEM_menu      = pcItem
                             NO-LOCK NO-ERROR.
  /*armo condicion de parada*/
  i = INTEGER(par_menu_grupos.accion_seleccion) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR AND i <> 0 THEN  DO: /*es una carpeta */
    /*pcPath    = pcPath + " - " + par_menu_grupos.dato_menu.*/
  END.
  IF NOT ERROR-STATUS:ERROR AND i <> 0 AND INTEGER(par_menu_grupos.ITEM_menu) <= 10 THEN DO:  
    RETURN pcPath.
  END.
  vcItem = par_menu_grupos.ITEM_anterior.
  IF AVAILABLE par_menu_grupos THEN DO:
    /*busco un item cuyo valor de accion_seleccion_gui = item_menu, es decir, la carpeta padre*/
    FIND FIRST pm WHERE pm.letra_inicial        = par_menu_grupos.letra_inicial
                    AND pm.accion_seleccion_gui = par_menu_grupos.ITEM_menu
                  NO-LOCK NO-ERROR.
    IF AVAILABLE pm THEN DO: /*es la carpeta padre*/
      pcPath    = pm.dato_menu + "\" + pcPath.
      vcItem    = pm.ITEM_anterior.
    END.
    c = getPhater(pcLetra, vcItem, pcPath).
  END.
  ELSE
    RETURN "Error".

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

