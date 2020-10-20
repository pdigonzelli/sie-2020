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
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel FI-empresa T-empresa ~
FI-sector T-sector FI-sucursal T-sucursal FI-id_origen FI-id_proveedor ~
T-fincas FI-grupo T-grupo T-legajos FI-desde-fecha RADIO-SET-1 T-excel ~
FI-hasta-fecha FI-impresora B-cambiar 
&Scoped-Define DISPLAYED-OBJECTS FI-empresa nombre-empresa T-empresa ~
FI-sector nombre-sector T-sector FI-sucursal nombre-sucursal T-sucursal ~
FI-id_origen FI-id_proveedor nombre-finca T-fincas FI-grupo nombre-grupo ~
T-grupo FI-legajo nombre-legajo T-legajos FI-desde-fecha RADIO-SET-1 ~
T-excel FI-hasta-fecha FI-impresora 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-cambiar 
     LABEL "Cambiar" 
     SIZE 15 BY 1.19.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Imprimir" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-desde-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Empresa Cosechera" 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .95 NO-UNDO.

DEFINE VARIABLE FI-grupo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Grupo Tarea" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-hasta-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-id_origen AS CHARACTER FORMAT "X(256)":U 
     LABEL "Finca" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE FI-id_proveedor AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Productor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-impresora AS CHARACTER FORMAT "X(256)":U 
     LABEL "Impresora" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-legajo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Legajo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.24 NO-UNDO.

DEFINE VARIABLE FI-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-sucursal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-finca AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE nombre-grupo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-legajo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.19
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE nombre-sucursal AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1.19 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Persona", 1,
"Por Lote", 2
     SIZE 25 BY 1.91 NO-UNDO.

DEFINE VARIABLE T-empresa AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-excel AS LOGICAL INITIAL no 
     LABEL "Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-fincas AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-grupo AS LOGICAL INITIAL no 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-legajos AS LOGICAL INITIAL yes 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-sector AS LOGICAL INITIAL no 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-sucursal AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1 COL 102 WIDGET-ID 4
     Btn_Cancel AT ROW 2.19 COL 102 WIDGET-ID 2
     FI-empresa AT ROW 2.43 COL 21.2 COLON-ALIGNED WIDGET-ID 8
     nombre-empresa AT ROW 2.43 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     T-empresa AT ROW 2.67 COL 77 WIDGET-ID 60
     FI-sector AT ROW 3.62 COL 21 COLON-ALIGNED WIDGET-ID 14
     nombre-sector AT ROW 3.62 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     T-sector AT ROW 4.1 COL 77 WIDGET-ID 62
     FI-sucursal AT ROW 5.05 COL 21 COLON-ALIGNED WIDGET-ID 42
     nombre-sucursal AT ROW 5.05 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     T-sucursal AT ROW 5.52 COL 77 WIDGET-ID 64
     FI-id_origen AT ROW 6.95 COL 21 COLON-ALIGNED WIDGET-ID 22
     FI-id_proveedor AT ROW 6.95 COL 44 COLON-ALIGNED WIDGET-ID 24
     nombre-finca AT ROW 6.95 COL 58 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     T-fincas AT ROW 6.95 COL 109 WIDGET-ID 66
     FI-grupo AT ROW 8.14 COL 21 COLON-ALIGNED WIDGET-ID 56
     nombre-grupo AT ROW 8.14 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     T-grupo AT ROW 8.14 COL 109 WIDGET-ID 68
     FI-legajo AT ROW 10.05 COL 21 COLON-ALIGNED WIDGET-ID 44
     nombre-legajo AT ROW 10.05 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     T-legajos AT ROW 10.52 COL 86 WIDGET-ID 46
     FI-desde-fecha AT ROW 11.95 COL 21 COLON-ALIGNED WIDGET-ID 6
     RADIO-SET-1 AT ROW 12.43 COL 57 NO-LABEL WIDGET-ID 50
     T-excel AT ROW 12.67 COL 86 WIDGET-ID 54
     FI-hasta-fecha AT ROW 13.14 COL 21 COLON-ALIGNED WIDGET-ID 10
     FI-impresora AT ROW 14.57 COL 19 COLON-ALIGNED WIDGET-ID 34
     B-cambiar AT ROW 14.57 COL 83 WIDGET-ID 32
     "  Filtros" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 28
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.8 BY 15.24 WIDGET-ID 100.


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
         TITLE              = "Control Destajo"
         HEIGHT             = 15.24
         WIDTH              = 124.8
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
/* SETTINGS FOR FILL-IN FI-legajo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-empresa IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-finca IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-grupo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-legajo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sector IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sucursal IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Control Destajo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Control Destajo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-cambiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-cambiar wWin
ON CHOOSE OF B-cambiar IN FRAME fMain /* Cambiar */
DO:
  system-dialog printer-setup.
  fi-impresora:screen-value in frame {&frame-name} = session:printer-name.
  
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
  define var v_zona as character.
  define var v_titulo as character format "x(60)".
  DEFINE VAR v_nombre AS CHARACTER.

      v_filtro = "items_control_tareas.id_empresa = " + fi-empresa:screen-value    +
           " and items_control_tareas.id_sector = " + fi-sector:screen-value  +
           " and items_control_tareas.fecha >= date('" + fi-desde-fecha:screen-value  + "')" + 
           " and items_control_tareas.fecha <= date('" + fi-hasta-fecha:screen-value  + "')" +
           " and items_control_tareas.id_tarea <> 0".
           
      IF INTEGER(fi-sucursal:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <> 0 THEN
      DO:
          v_filtro = v_filtro + " and items_control_tareas.id_sucursal = " + fi-sucursal:screen-value in frame {&frame-name}.   
      END.

      IF INTEGER(fi-legajo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <> 0 THEN
      DO:
          v_filtro = v_filtro + " and items_control_tareas.legajo = " + fi-legajo:screen-value in frame {&frame-name}.   
      END.


CASE INTEGER(radio-set-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
    WHEN 1 THEN v_nombre = "control_destajo".
    WHEN 2 THEN v_nombre = "control_destajo_lote".
END CASE.

IF t-excel:SCREEN-VALUE = "yes" THEN
DO:
    run p_excel_destajo.p (INPUT INTEGER(fi-empresa:SCREEN-VALUE),
                           INPUT logical(t-empresa:SCREEN-VALUE), 
                           input integer(fi-sector:screen-value),
                           INPUT logical(t-sector:SCREEN-VALUE), 
                           input integer(fi-sucursal:screen-value),
                           INPUT logical(t-sucursal:SCREEN-VALUE), 
                           input integer(fi-id_origen:screen-value),
                           input integer(fi-id_proveedor:screen-value),
                           INPUT logical(t-fincas:SCREEN-VALUE), 
                           input integer(fi-grupo:screen-value),
                           INPUT logical(t-grupo:SCREEN-VALUE), 
                           input date(fi-desde-fecha:screen-value),
                           input date(fi-hasta-fecha:screen-value),
                           input "z:\temp\repor01.txt").


END.
ELSE
DO:
RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.

    RUN  aderb\_prntrb2(
         (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
          v_nombre, /* RB-REPORT-NAME */
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
          "Control Destajo",         /* RB-WINDOW-TITLE */
          yes,                           /* RB-DISPLAY-ERRORS */
          yes,                           /* RB-DISPLAY-STATUS */
          no,                              /* RB-NO-WAIT */
          "v_general = " + fi-desde-fecha:screen-value + ";" + fi-hasta-fecha:screen-value +
          ";" + v_titulo
          /* RB-OTHER-PARAMETERS */,
          ""
          ).   
os-delete value(RB-MEMO-FILE).
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-empresa wWin
ON MOUSE-SELECT-DBLCLICK OF FI-empresa IN FRAME fMain /* Empresa Cosechera */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
    RUN adm2/support/gConsultas.w (INPUT "brprovactiv.w",
                                   INPUT "drprovactiv.w",
                                   INPUT "id_proveedor",
                                   INPUT "r_prov_activ.id_actividad = 6" ,
                                   OUTPUT xfieldResult).

    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:

           fi-empresa:SCREEN-VALUE = xfieldResult.     
           RUN descriptivos.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-grupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-grupo wWin
ON LEAVE OF FI-grupo IN FRAME fMain /* Grupo Tarea */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-grupo wWin
ON MOUSE-SELECT-DBLCLICK OF FI-grupo IN FRAME fMain /* Grupo Tarea */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.
  RUN adm2/support/gConsultas.w (INPUT "bgrupostareas.w",
                                 INPUT "dgrupostareas.w",
                                 INPUT "id_grupo_tarea",
                                 INPUT "" ,
                                 OUTPUT xfieldResult).

  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
         fi-grupo:SCREEN-VALUE = xfieldResult.     
         RUN descriptivos.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-id_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-id_origen wWin
ON MOUSE-SELECT-DBLCLICK OF FI-id_origen IN FRAME fMain /* Finca */
DO:
 define var r as rowid no-undo.
 
 run wselfinca.w (output r).
 find FIRST origenes where rowid(origenes) = r no-lock no-error.
 if available origenes then 
 do:
   fi-id_proveedor:screen-value = string(origenes.id_proveedor).
   fi-id_origen:screen-value = string(origenes.id_origen).
   nombre-finca:screen-value = origenes.descripcion.
 end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-legajo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-legajo wWin
ON LEAVE OF FI-legajo IN FRAME fMain /* Legajo */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-legajo wWin
ON MOUSE-SELECT-DBLCLICK OF FI-legajo IN FRAME fMain /* Legajo */
DO:
  DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.
  RUN adm2/support/gConsultas.w (INPUT "bpersonalfinca.w",
                                 INPUT "dpersonalfinca.w",
                                 INPUT "legajo",
                                 INPUT "personal_finca.id_empresa_cosechera = " + fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " by personal_finca.nombre",
                                 OUTPUT xfieldResult).

  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
         fi-legajo:SCREEN-VALUE = xfieldResult.     
         RUN descriptivos.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-sector
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON GO OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON LEAVE OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON MOUSE-SELECT-DBLCLICK OF FI-sector IN FRAME fMain /* Sector */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
    RUN adm2/support/gConsultas.w (INPUT "bsectoresagricola.w",
                                   INPUT "dsectoresagricola.w",
                                   INPUT "id_sector",
                                   INPUT "" ,
                                   OUTPUT xfieldResult).

    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:
           fi-sector:SCREEN-VALUE = xfieldResult.     
           RUN descriptivos.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON U1 OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sucursal wWin
ON LEAVE OF FI-sucursal IN FRAME fMain /* Sucursal */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sucursal wWin
ON MOUSE-SELECT-DBLCLICK OF FI-sucursal IN FRAME fMain /* Sucursal */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
    RUN adm2/support/gConsultas.w (INPUT "bsucursales.w",
                                   INPUT "dsucursales.w",
                                   INPUT "id_sucursal",
                                   INPUT "sucursales.id_tipo_sucursal = 19" ,
                                   OUTPUT xfieldResult).

    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:
           fi-sucursal:SCREEN-VALUE = xfieldResult.     
           RUN descriptivos.
    END.

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


&Scoped-define SELF-NAME T-fincas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-fincas wWin
ON VALUE-CHANGED OF T-fincas IN FRAME fMain /* Todas */
DO:
   if t-fincas:screen-value = 'yes' Then
        ASSIGN fi-id_origen:sensitive = false
               fi-id_proveedor:SENSITIVE = FALSE.
    Else
        ASSIGN fi-id_origen:sensitive = true
               fi-id_proveedor:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-grupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-grupo wWin
ON VALUE-CHANGED OF T-grupo IN FRAME fMain /* Todos */
DO:
   if t-grupo:screen-value = 'yes' Then
        fi-grupo:sensitive = false.
    Else
        fi-grupo:sensitive = true.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-legajos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-legajos wWin
ON VALUE-CHANGED OF T-legajos IN FRAME fMain /* Todos */
DO:
    if t-legajos:screen-value = 'yes' Then
        fi-legajo:sensitive = false.
    Else
        fi-legajo:sensitive = true.

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


&Scoped-define SELF-NAME T-sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-sucursal wWin
ON VALUE-CHANGED OF T-sucursal IN FRAME fMain /* Todas */
DO:
   if t-sucursal:screen-value = 'yes' Then
        fi-sucursal:sensitive = false.
    Else
        fi-sucursal:sensitive = true.
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
 find first proveedores where proveedores.id_proveedor = INTEGER(fi-empresa:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
 if available proveedores Then
      nombre-empresa:screen-value = proveedores.nombre.
   ELSE
     nombre-empresa:screen-value = "".


find first sectores_agricolas where sectores_agricolas.id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME} )  no-lock no-error .
if available sectores_agricolas then 
    nombre-sector:screen-value   = string(sectores_agricolas.descripcion).
    else
    nombre-sector:screen-value  = ''.

    find first sucursales where sucursales.id_sucursal = INTEGER(fi-sucursal:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
    if available sucursales Then
             nombre-sucursal:screen-value = sucursales.nombre.
          ELSE
            nombre-sucursal:screen-value = "".

FIND FIRST personal_finca WHERE personal_finca.id_empresa_cosechera = INTEGER(fi-empresa:screen-value in frame {&FRAME-NAME}) AND
           personal_finca.legajo = INTEGER(fi-legajo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE personal_finca THEN
      nombre-legajo:SCREEN-VALUE = personal_finca.nombre.
   ELSE
      nombre-legajo:SCREEN-VALUE = "".



find first grupos_tareas where grupos_tareas.id_grupo = INTEGER(fi-grupo:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
      if available grupos_tareas Then
           nombre-grupo:screen-value = grupos_tareas.descripcion.
        ELSE
          nombre-grupo:screen-value = "".


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
  DISPLAY FI-empresa nombre-empresa T-empresa FI-sector nombre-sector T-sector 
          FI-sucursal nombre-sucursal T-sucursal FI-id_origen FI-id_proveedor 
          nombre-finca T-fincas FI-grupo nombre-grupo T-grupo FI-legajo 
          nombre-legajo T-legajos FI-desde-fecha RADIO-SET-1 T-excel 
          FI-hasta-fecha FI-impresora 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK Btn_Cancel FI-empresa T-empresa FI-sector T-sector FI-sucursal 
         T-sucursal FI-id_origen FI-id_proveedor T-fincas FI-grupo T-grupo 
         T-legajos FI-desde-fecha RADIO-SET-1 T-excel FI-hasta-fecha 
         FI-impresora B-cambiar 
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

  fi-empresa:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
  fi-desde-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
  fi-hasta-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
  fi-sector:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
  fi-sucursal:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
  fi-legajo:load-mouse-pointer("glove") in frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

