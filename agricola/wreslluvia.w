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

{src/adm2/widgetprto.i}

{s_varsis.i}

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
&Scoped-Define ENABLED-OBJECTS Btn_OK FI-periodo RADIO-SET-1 Btn_Cancel ~
fi-mes 
&Scoped-Define DISPLAYED-OBJECTS FI-periodo RADIO-SET-1 fi-mes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Imprimir" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi-mes AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-periodo AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Mensual", 1,
"Anual", 2
     SIZE 21 BY 2.05 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.95 COL 64 WIDGET-ID 4
     FI-periodo AT ROW 2.43 COL 13 COLON-ALIGNED WIDGET-ID 10
     RADIO-SET-1 AT ROW 2.91 COL 40 NO-LABEL WIDGET-ID 36
     Btn_Cancel AT ROW 3.14 COL 64.2 WIDGET-ID 2
     fi-mes AT ROW 3.86 COL 13 COLON-ALIGNED WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.8 BY 5.1 WIDGET-ID 100.


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
         TITLE              = "Planilla LLuvia"
         HEIGHT             = 5.24
         WIDTH              = 83.4
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Planilla LLuvia */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Planilla LLuvia */
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
  APPLY "window-close" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* Imprimir */
DO:
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  define var v_filtro as character initial "".
  define var v_general as character.
  DEFINE VAR v_tipo_planilla AS INTEGER.
  DEFINE VAR v_reporte AS CHARACTER.

  v_tipo_planilla = INTEGER(radio-set-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  CASE v_tipo_planilla:
      WHEN 1 THEN 
      do: 
          v_reporte = "planilla_lluvia".
          v_filtro = "planilla_lluvia.anio = " + fi-periodo:SCREEN-VALUE +
                     " and planilla_lluvia.mes = " + fi-mes:SCREEN-VALUE.
      END.
      WHEN 2 THEN 
      DO:   
          v_reporte = "planilla_lluvia_anual".
          v_filtro = "planilla_lluvia.anio = " + fi-periodo:SCREEN-VALUE.
         
      END.
  END CASE.

 /* vlc_dir_fuentes = "z:\sistemas\sami\sistemas\". */

RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.

    RUN  aderb\_prntrb2(
         (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
          v_reporte, /* RB-REPORT-NAME */
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
          "Planilla LLuvia",         /* RB-WINDOW-TITLE */
          yes,                           /* RB-DISPLAY-ERRORS */
          yes,                           /* RB-DISPLAY-STATUS */
          no,                              /* RB-NO-WAIT */
          "" /* RB-OTHER-PARAMETERS */,
          ""
          ).   
os-delete value(RB-MEMO-FILE).


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
  DISPLAY FI-periodo RADIO-SET-1 fi-mes 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK FI-periodo RADIO-SET-1 Btn_Cancel fi-mes 
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

  /* Code placed here will execute AFTER standard behavior.    */

  fi-periodo:screen-value in frame {&FRAME-NAME} = string(year(today)).
  fi-mes:screen-value in frame {&FRAME-NAME} = string(MONTH(today)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

