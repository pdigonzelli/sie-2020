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
DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.


/* Local Variable Definitions ---                                       */

DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.

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
&Scoped-Define ENABLED-OBJECTS fiFrLit fiBx2020 fiAcidezP fiLitros ~
BUTTON-35 fiBxCorreg fiBx2020400 fiAcidezGPL fiAcidezP400 fiKilos ~
fiCoef2020 fiRatio fiCoef2020400 fiBxCorreg400 fiCoefPEspBxCorr400 ~
fiKilos400 Btn_OK Btn_Cancel fiSolidos fiLitrosCga fiBxBuscado ~
fiBxExistente fiKilosTambor BUTTON-48 fiFrSSBxB fiFrSSBxE fiFrLitConc ~
fiFrPEBxE fiFrKilConc fiFrKilosTam fiFrCantTbs Btn_OK-2 RECT-32 RECT-33 ~
RECT-34 RECT-35 RECT-36 RECT-37 RECT-38 RECT-39 
&Scoped-Define DISPLAYED-OBJECTS fiFrLit fiBx2020 fiAcidezP fiLitros ~
fiBxCorreg fiBx2020400 fiAcidezGPL fiAcidezP400 fiKilos fiCoef2020 fiRatio ~
fiCoef2020400 fiBxCorreg400 fiCoefPEspBxCorr400 fiKilos400 fiSolidos ~
fiLitrosCga fiBxBuscado fiBxExistente fiKilosTambor fiFrSSBxB fiFrSSBxE ~
fiFrLitConc fiFrPEBxE fiFrKilConc fiFrKilosTam fiFrCantTbs 

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


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK-2 
     LABEL "Tabla Solidos" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-35 
     LABEL "Calcular" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-48 
     LABEL "Calcular" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiAcidezGPL AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Acidez GPL" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiAcidezP AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Acidez %" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiAcidezP400 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Acidez % a 400" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiBx2020 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Brix 2020" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiBx2020400 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Brix 2020 a 400" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiBxBuscado AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Brix Buscado" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiBxCorreg AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Brix Corr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiBxCorreg400 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Brix Correg a 400" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiBxExistente AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Brix Existente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiCoef2020 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Coef S.S. 2020" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiCoef2020400 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Coef S.S 2020 a 400" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiCoefPEspBxCorr400 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Coef PE Bx Corr a 400" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFrCantTbs AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFrKilConc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFrKilosTam AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFrLit AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFrLitConc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFrPEBxE AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFrSSBxB AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiFrSSBxE AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiKilos AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Kilos" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiKilos400 AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE fiKilosTambor AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Kilos Tambor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiLitros AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Litros" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiLitrosCga AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Litros" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiRatio AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Ratio" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiSolidos AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Solidos" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 30.4 BY .19.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 136 BY 3.57.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 33 BY .19.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55 BY 7.62.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 43 BY 7.62.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 14 BY .19.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 7.62.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 136 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiFrLit AT ROW 15.57 COL 29.6 COLON-ALIGNED NO-LABEL
     fiBx2020 AT ROW 1.24 COL 11 COLON-ALIGNED
     fiAcidezP AT ROW 2.43 COL 11 COLON-ALIGNED
     fiLitros AT ROW 3.62 COL 11 COLON-ALIGNED
     BUTTON-35 AT ROW 5.76 COL 7
     fiBxCorreg AT ROW 1.24 COL 61 COLON-ALIGNED
     fiBx2020400 AT ROW 1.24 COL 119 COLON-ALIGNED
     fiAcidezGPL AT ROW 2.43 COL 61 COLON-ALIGNED
     fiAcidezP400 AT ROW 2.43 COL 119 COLON-ALIGNED
     fiKilos AT ROW 3.62 COL 61 COLON-ALIGNED
     fiCoef2020 AT ROW 3.62 COL 119 COLON-ALIGNED
     fiRatio AT ROW 4.81 COL 61 COLON-ALIGNED
     fiCoef2020400 AT ROW 4.81 COL 119 COLON-ALIGNED
     fiBxCorreg400 AT ROW 6 COL 119 COLON-ALIGNED
     fiCoefPEspBxCorr400 AT ROW 7.19 COL 119 COLON-ALIGNED
     fiKilos400 AT ROW 10.95 COL 98 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 21.71 COL 106
     Btn_Cancel AT ROW 21.71 COL 122
     fiSolidos AT ROW 6 COL 55 COLON-ALIGNED
     fiLitrosCga AT ROW 14.33 COL 14 COLON-ALIGNED
     fiBxBuscado AT ROW 15.52 COL 14 COLON-ALIGNED
     fiBxExistente AT ROW 16.71 COL 14 COLON-ALIGNED
     fiKilosTambor AT ROW 17.91 COL 14 COLON-ALIGNED
     BUTTON-48 AT ROW 19.57 COL 8
     fiFrSSBxB AT ROW 15.57 COL 49.2 COLON-ALIGNED NO-LABEL
     fiFrSSBxE AT ROW 16.86 COL 39.4 COLON-ALIGNED NO-LABEL
     fiFrLitConc AT ROW 16.29 COL 67.8 COLON-ALIGNED NO-LABEL
     fiFrPEBxE AT ROW 16.29 COL 86.4 COLON-ALIGNED NO-LABEL
     fiFrKilConc AT ROW 15.57 COL 106.2 COLON-ALIGNED NO-LABEL
     fiFrKilosTam AT ROW 16.86 COL 106.2 COLON-ALIGNED NO-LABEL
     fiFrCantTbs AT ROW 16.24 COL 125 COLON-ALIGNED NO-LABEL
     Btn_OK-2 AT ROW 21.71 COL 89
     RECT-32 AT ROW 11.29 COL 31.6
     RECT-33 AT ROW 9.1 COL 1
     RECT-34 AT ROW 16.48 COL 32
     RECT-35 AT ROW 1 COL 81
     RECT-36 AT ROW 1 COL 37
     RECT-37 AT ROW 16.48 COL 108.2
     RECT-38 AT ROW 1 COL 1
     RECT-39 AT ROW 13.38 COL 1
     "*" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 11.19 COL 63
          FONT 6
     "SS Brix Existente" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.91 COL 39
     "Lts Concentrado" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 15.52 COL 70
     "PE Bx Exist" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 15.52 COL 90
     "Kgs Conc" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 14.81 COL 109
     "Kgs/Tbor" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 17.91 COL 110
     "Tbs Neces" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 15.29 COL 124
     "coefPEsp(bx correg a 400) =" VIEW-AS TEXT
          SIZE 32.6 BY .62 AT ROW 11 COL 65
          FONT 6
     "Kgs 400 =" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 11.05 COL 19
          FONT 6
     "Formula Calculo Kilos 400 GPL" VIEW-AS TEXT
          SIZE 39 BY .62 AT ROW 8.86 COL 2
          FONT 6
     "Litros * coefSS(Bx2020)" VIEW-AS TEXT
          SIZE 28.6 BY .62 AT ROW 10.52 COL 33.4
          FONT 6
     "coefSS(bx 2020 a 400)" VIEW-AS TEXT
          SIZE 28.2 BY .62 AT ROW 11.67 COL 32.8
          FONT 6
     "*" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 15.62 COL 47
          FONT 6
     "=" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 16.24 COL 66.2
          FONT 6
     "*" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 16.33 COL 85.2
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136.2 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     "=" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 16.24 COL 104
          FONT 6
     "=" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 16.24 COL 123.2
          FONT 6
     "Formula Calculo Volumen Carga" VIEW-AS TEXT
          SIZE 40 BY .62 AT ROW 13.14 COL 2
          FONT 6
     "SS Brix Buscado" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 14.81 COL 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136.2 BY 22.


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
         TITLE              = "Calculos Analiticos"
         HEIGHT             = 22
         WIDTH              = 136.2
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
/* SETTINGS FOR FRAME fMain
   Custom                                                               */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Calculos Analiticos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON MOUSE-SELECT-CLICK OF wWin /* Calculos Analiticos */
DO:
      MESSAGE CURRENT-WINDOW:TITLE
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Calculos Analiticos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel wWin
ON CHOOSE OF Btn_Cancel IN FRAME fMain /* Cancel */
DO:
  RUN beforeExit.
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* OK */
DO:
  RUN beforeExit.
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK-2 wWin
ON CHOOSE OF Btn_OK-2 IN FRAME fMain /* Tabla Solidos */
DO:
  RUN wBrix.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-35 wWin
ON CHOOSE OF BUTTON-35 IN FRAME fMain /* Calcular */
DO:
  DEFINE VARIABLE dCorr AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fBrx  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cFile AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLine AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVal  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPos  AS INTEGER    NO-UNDO.


  dKi4 = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, DECIMAL(fiBx2020:SCREEN-VALUE), 
                                                    DECIMAL(fiAcidezP:SCREEN-VALUE), 
                                                    DECIMAL(fiLitros:SCREEN-VALUE), 
                                                    TRUE).

  fiKilos400:SCREEN-VALUE IN FRAME fMain = STRING(ROUND(dKi4, 2)).

  
  cFile = "..\industria\analisis.conf".
  INPUT FROM VALUE(cFile).
  REPEAT :
    IMPORT UNFORMATTED cLine.
    IF LENGTH(cLine) = 0 THEN DO:  
      /*continua si encuentra una linea en blanco*/
      NEXT.
    END.
    iPos = INDEX(cLine, "#").
    IF iPos > 0 THEN DO:        
      /*continuo con la regla siguiente si esta el caracter de comentario #*/
      NEXT.
    END.

    cVal = cVal + ENTRY(2, cLine, "=") + CHR(1).
  END.


  ASSIGN fiBx2020400:SCREEN-VALUE         = ENTRY(1, cVal, CHR(1))
         fiAcidezP400:SCREEN-VALUE        = ENTRY(2, cVal, CHR(1))
         fiCoef2020:SCREEN-VALUE          = ENTRY(3, cVal, CHR(1))
         fiCoef2020400:SCREEN-VALUE       = ENTRY(4, cVal, CHR(1))
         fiBxCorreg400:SCREEN-VALUE       = ENTRY(5, cVal, CHR(1))
         fiCoefPEspBxCorr400:SCREEN-VALUE = ENTRY(6, cVal, CHR(1)).


  dCorr = DYNAMIC-FUNCTION('getCoefCorreccionAcidez' IN hLib, DECIMAL(fiAcidezP:SCREEN-VALUE)).
  fBrx  = ROUND(DECIMAL(fiBx2020:SCREEN-VALUE) + dCorr, 2).


  fiBxCorreg:SCREEN-VALUE   = STRING(fBrx).  /*DYNAMIC-FUNCTION('getBrixCorregido' IN hLib, DECIMAL(fiBx2020:SCREEN-VALUE), DECIMAL(fiAcidezP:SCREEN-VALUE)).*/
  fiAcidezGPL:SCREEN-VALUE  = DYNAMIC-FUNCTION('getAcidezGPL' IN hLib, DECIMAL(fiBx2020:SCREEN-VALUE), DECIMAL(fiAcidezP:SCREEN-VALUE)).
  fiKilos:SCREEN-VALUE      = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, DECIMAL(fiAcidezP:SCREEN-VALUE), DECIMAL(fiBx2020:SCREEN-VALUE), DECIMAL(fiLitros:SCREEN-VALUE)).
  fiRatio:SCREEN-VALUE      = DYNAMIC-FUNCTION('getRatio' IN hLib, DECIMAL(fiBx2020:SCREEN-VALUE), DECIMAL(fiAcidezP:SCREEN-VALUE)).
  fiSolidos:SCREEN-VALUE    = DYNAMIC-FUNCTION('getSolidosSolubles' IN hLib, fBrx , DECIMAL(fiLitros:SCREEN-VALUE)).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-48
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-48 wWin
ON CHOOSE OF BUTTON-48 IN FRAME fMain /* Calcular */
DO:
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iTam AS INTEGER    NO-UNDO.


  fiFrSSBxB:SCREEN-VALUE = DYNAMIC-FUNCTION('getCoefSolidosSolubles' IN hLib, DECIMAL(fiBxBuscado:SCREEN-VALUE)). 
  fiFrSSBxE:SCREEN-VALUE = DYNAMIC-FUNCTION('getCoefSolidosSolubles' IN hLib, DECIMAL(fiBxExistente:SCREEN-VALUE)).
  fiFrPEBxE:SCREEN-VALUE = DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hLib, DECIMAL(fiBxExistente:SCREEN-VALUE)).

  dLit = (DECIMAL(fiLitrosCga:SCREEN-VALUE) * DECIMAL(fiFrSSBxB:SCREEN-VALUE)) / DECIMAL(fiFrSSBxE:SCREEN-VALUE).
  dKil = dLit * DECIMAL(fiFrPEBxE:SCREEN-VALUE).
  iTam = ROUND(dKil / DECIMAL(fiFrKilosTam:SCREEN-VALUE), 0).

  fiFrLitConc:SCREEN-VALUE = STRING(dLit).
  fiFrKilConc:SCREEN-VALUE = STRING(dKil).
  fiFrCantTbs:SCREEN-VALUE = STRING(iTam).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKilosTambor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKilosTambor wWin
ON LEAVE OF fiKilosTambor IN FRAME fMain /* Kilos Tambor */
DO:
  fiFrKilosTam:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLitrosCga
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLitrosCga wWin
ON LEAVE OF fiLitrosCga IN FRAME fMain /* Litros */
DO:
  fiFrLit:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeExit wWin 
PROCEDURE beforeExit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libApiMenu.p').
  DELETE OBJECT hLibCom.

  RUN cleanUpInfoInParent IN hLib.

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
  DISPLAY fiFrLit fiBx2020 fiAcidezP fiLitros fiBxCorreg fiBx2020400 fiAcidezGPL 
          fiAcidezP400 fiKilos fiCoef2020 fiRatio fiCoef2020400 fiBxCorreg400 
          fiCoefPEspBxCorr400 fiKilos400 fiSolidos fiLitrosCga fiBxBuscado 
          fiBxExistente fiKilosTambor fiFrSSBxB fiFrSSBxE fiFrLitConc fiFrPEBxE 
          fiFrKilConc fiFrKilosTam fiFrCantTbs 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE fiFrLit fiBx2020 fiAcidezP fiLitros BUTTON-35 fiBxCorreg fiBx2020400 
         fiAcidezGPL fiAcidezP400 fiKilos fiCoef2020 fiRatio fiCoef2020400 
         fiBxCorreg400 fiCoefPEspBxCorr400 fiKilos400 Btn_OK Btn_Cancel 
         fiSolidos fiLitrosCga fiBxBuscado fiBxExistente fiKilosTambor 
         BUTTON-48 fiFrSSBxB fiFrSSBxE fiFrLitConc fiFrPEBxE fiFrKilConc 
         fiFrKilosTam fiFrCantTbs Btn_OK-2 RECT-32 RECT-33 RECT-34 RECT-35 
         RECT-36 RECT-37 RECT-38 RECT-39 
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

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  

  RUN libTamboresIndustria.p PERSISTENT SET hLib.


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

