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
&Scoped-Define ENABLED-OBJECTS Btn_generar fi-empresa T-empresa Btn_Cancel ~
Fi-sector T-sector Fi-tipo-liq T-tipo T-planilla FI-desde-fecha RADIO-SET-2 ~
FI-hasta-fecha FI-salida 
&Scoped-Define DISPLAYED-OBJECTS fi-empresa nombre-empresa T-empresa ~
Fi-sector nombre-sector T-sector Fi-tipo-liq T-tipo FI-nro-planilla ~
T-planilla FI-desde-fecha RADIO-SET-2 FI-hasta-fecha FI-salida 

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

DEFINE BUTTON Btn_generar AUTO-GO 
     LABEL "&Generar" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-desde-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emp Contratista" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE FI-hasta-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-nro-planilla AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Nro planilla" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-salida AS CHARACTER FORMAT "X(256)":U INITIAL "z:~\temp~\repgral01.txt" 
     LABEL "Archivo de salida" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .95 NO-UNDO.

DEFINE VARIABLE Fi-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE Fi-tipo-liq AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Liq" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Persona", 1,
"Para Liquidacion", 2,
"Resumen x Planilla", 3,
"Excel dias trabajados", 4,
"Excel dias trabajados unificado", 5
     SIZE 36 BY 5 NO-UNDO.

DEFINE VARIABLE T-empresa AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE T-planilla AS LOGICAL INITIAL yes 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-sector AS LOGICAL INITIAL no 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-tipo AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_generar AT ROW 1.48 COL 93 WIDGET-ID 4
     fi-empresa AT ROW 2.43 COL 22 COLON-ALIGNED WIDGET-ID 32
     nombre-empresa AT ROW 2.43 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     T-empresa AT ROW 2.48 COL 79 WIDGET-ID 54
     Btn_Cancel AT ROW 2.91 COL 93 WIDGET-ID 2
     Fi-sector AT ROW 3.71 COL 22 COLON-ALIGNED WIDGET-ID 34
     nombre-sector AT ROW 3.86 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     T-sector AT ROW 3.86 COL 67 WIDGET-ID 48
     Fi-tipo-liq AT ROW 5.19 COL 22.4 COLON-ALIGNED WIDGET-ID 46
     T-tipo AT ROW 5.29 COL 32 WIDGET-ID 52
     FI-nro-planilla AT ROW 5.29 COL 55 COLON-ALIGNED WIDGET-ID 40
     T-planilla AT ROW 5.29 COL 67 WIDGET-ID 50
     FI-desde-fecha AT ROW 6.95 COL 21 COLON-ALIGNED WIDGET-ID 6
     RADIO-SET-2 AT ROW 7.67 COL 71 NO-LABEL WIDGET-ID 36
     FI-hasta-fecha AT ROW 8.14 COL 21 COLON-ALIGNED WIDGET-ID 12
     FI-salida AT ROW 9.81 COL 21 COLON-ALIGNED WIDGET-ID 14
     "  Filtros" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 28
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.4 BY 12.67 WIDGET-ID 100.


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
         TITLE              = "Resumen de Tarjas - Excel"
         HEIGHT             = 12.67
         WIDTH              = 110.4
         MAX-HEIGHT         = 35.86
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 35.86
         VIRTUAL-WIDTH      = 256
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
/* SETTINGS FOR FILL-IN FI-nro-planilla IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-empresa IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sector IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Resumen de Tarjas - Excel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Resumen de Tarjas - Excel */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_generar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_generar wWin
ON CHOOSE OF Btn_generar IN FRAME fMain /* Generar */
DO:

          CASE integer(radio-set-2:SCREEN-VALUE):
            WHEN 1 THEN
            DO:
                /* z:\sistemas\sami\sistemas\gui\agricola\ */
                run p_liqtarjas01.p (input integer(fi-sector:screen-value),
                                    input integer(fi-empresa:screen-value),
                                    INPUT fi-tipo-liq:SCREEN-VALUE,
                                    input date(fi-desde-fecha:screen-value),
                                    input date(fi-hasta-fecha:screen-value),
                                    input fi-salida:screen-value). 
            END.
            WHEN 2 THEN
            DO:
                run p_liqtarjas02.p (input integer(fi-sector:screen-value),
                                    input integer(fi-empresa:screen-value),
                                    INPUT fi-tipo-liq:SCREEN-VALUE,
                                    input date(fi-desde-fecha:screen-value),
                                    input date(fi-hasta-fecha:screen-value),
                                    input fi-salida:screen-value). 
            END.
              WHEN 3 THEN
              DO:
                  RUN imprime-reporte.
              END.
              WHEN 4 THEN
              DO:
                  RUN p_liqdiastarjas.p (INPUT integer(fi-empresa:screen-value),
                                         input date(fi-desde-fecha:screen-value),
                                         input date(fi-hasta-fecha:screen-value),
                                         input fi-salida:screen-value). 

              END.
              WHEN 5 THEN
              DO:
                  RUN p_liqdiastarjasuni.p (INPUT integer(fi-empresa:screen-value),
                                         input date(fi-desde-fecha:screen-value),
                                         input date(fi-hasta-fecha:screen-value),
                                         input fi-salida:screen-value). 

              END.

          END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresa wWin
ON MOUSE-SELECT-DBLCLICK OF fi-empresa IN FRAME fMain /* Emp Contratista */
DO:
     DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bliqempresas.w",
                                 INPUT "dliqempresas.w",
                                 INPUT "id_empresa_liq",
                                 INPUT "", 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       fi-empresa:SCREEN-VALUE = xfieldResult. 
       RUN descriptivos.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fi-sector
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON GO OF Fi-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON LEAVE OF Fi-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON MOUSE-SELECT-DBLCLICK OF Fi-sector IN FRAME fMain /* Sector */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bliqsectores.w",
                                 INPUT "dliqsectores.w",
                                 INPUT "id_sector_liq",
                                 INPUT "liq_sectores.id_empresa_liq = " + fi-empresa:SCREEN-VALUE, 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       fi-sector:SCREEN-VALUE = xfieldResult. 
       RUN descriptivos.
  END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON U1 OF Fi-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-empresa wWin
ON VALUE-CHANGED OF T-empresa IN FRAME fMain /* Todas */
DO:
    if t-empresa:screen-value = 'yes' Then
       fi-empresa:sensitive = false.
   Else
       fi-empresa:sensitive = true.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-planilla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-planilla wWin
ON VALUE-CHANGED OF T-planilla IN FRAME fMain /* Todas */
DO:
    if t-planilla:screen-value = 'yes' Then
       fi-nro-planilla:sensitive = false.
   Else
       fi-nro-planilla:sensitive = true.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-sector
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-sector wWin
ON VALUE-CHANGED OF T-sector IN FRAME fMain /* Todos */
DO:
    if t-sector:screen-value = 'yes' Then
       fi-sector:sensitive = false.
   Else
       fi-sector:sensitive = true.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-tipo wWin
ON VALUE-CHANGED OF T-tipo IN FRAME fMain /* Todas */
DO:
    if t-tipo:screen-value = 'yes' Then
     fi-tipo-liq:sensitive = false.
 Else
     fi-tipo-liq:sensitive = true.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos wWin 
PROCEDURE descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST liq_empresas WHERE liq_empresas.id_empresa_liq = integer(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_empresas THEN
    nombre-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = liq_empresas.descripcion.
  ELSE
   nombre-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

FIND FIRST liq_sectores WHERE liq_sectores.id_empresa_liq = integer(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND 
    liq_sectores.id_sector_liq = integer(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
   IF AVAILABLE liq_sectores THEN
       nombre-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME} = liq_sectores.descripcion.
     ELSE
      nombre-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


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
  DISPLAY fi-empresa nombre-empresa T-empresa Fi-sector nombre-sector T-sector 
          Fi-tipo-liq T-tipo FI-nro-planilla T-planilla FI-desde-fecha 
          RADIO-SET-2 FI-hasta-fecha FI-salida 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_generar fi-empresa T-empresa Btn_Cancel Fi-sector T-sector 
         Fi-tipo-liq T-tipo T-planilla FI-desde-fecha RADIO-SET-2 
         FI-hasta-fecha FI-salida 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime-reporte wWin 
PROCEDURE imprime-reporte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  define var v_filtro as character initial "".
  define var v_general as character.
  define var v_zona as character.
  define var v_titulo as character format "x(60)".


     FOR EACH liq_items_tarjas WHERE
           liq_items_tarjas.fecha >= DATE(fi-desde-fecha:screen-value IN FRAME {&FRAME-NAME}) AND
           liq_items_tarjas.fecha <= DATE(fi-hasta-fecha:screen-value IN FRAME {&FRAME-NAME}) AND
           liq_items_tarjas.id_tarea <> 0:
        FIND FIRST resumen_compensado WHERE 
             resumen_compensado.id_empresa = liq_items_tarjas.id_empresa AND
             resumen_compensado.fecha = liq_items_tarjas.fecha AND
             resumen_compensado.legajo = liq_items_tarjas.legajo NO-LOCK NO-ERROR.
         IF AVAILABLE resumen_compensado THEN
         DO:
             IF (resumen_compensado.hs_categoria - resumen_compensado.hs_trab_norm) > 0  THEN
               ASSIGN liq_items_tarjas.cant_hs_compensa = (resumen_compensado.hs_categoria - resumen_compensado.hs_trab_norm).
              ELSE
               ASSIGN liq_items_tarjas.cant_hs_compensa = 0.
         END.
     END.



      v_filtro = "liq_items_tarjas.fecha >= date('" + fi-desde-fecha:screen-value IN FRAME {&FRAME-NAME} + "')" + 
           " and liq_items_tarjas.fecha <= date('" + fi-hasta-fecha:screen-value IN FRAME {&FRAME-NAME} + "')" +
           " and liq_items_tarjas.id_tarea <> 0".
           
      IF INTEGER(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <> 0 THEN
      DO:
          v_filtro = v_filtro + " and liq_items_tarjas.id_empresa = " + fi-empresa:screen-value in frame {&frame-name}.   
      END.


      IF INTEGER(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <> 0 THEN
      DO:
          v_filtro = v_filtro + " and liq_items_tarjas.id_sector = " + fi-sector:screen-value in frame {&frame-name}.   
      END.


RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.

  /*  vlc_dir_fuentes = "z:\sistemas\sami\sistemas\".  */
    RUN  aderb\_prntrb2(
         (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
          "liq_planilla_tarja", /* RB-REPORT-NAME */
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
          "Resumen Planilla x Persona",         /* RB-WINDOW-TITLE */
          yes,                           /* RB-DISPLAY-ERRORS */
          yes,                           /* RB-DISPLAY-STATUS */
          no,                              /* RB-NO-WAIT */
          "v_general = " + fi-desde-fecha:screen-value + ";" + fi-hasta-fecha:screen-value +
          ";" + v_titulo
          /* RB-OTHER-PARAMETERS */,
          ""
          ).   
os-delete value(RB-MEMO-FILE).

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

  fi-sector:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
  fi-empresa:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
fi-desde-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
fi-hasta-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

