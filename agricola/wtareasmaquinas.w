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
&Scoped-Define ENABLED-OBJECTS Btn_OK FI-empresa Btn_Cancel FI-sector ~
RADIO-SET-1 fi-nro FI-desde-fecha RADIO-SET-2 FI-hasta-fecha FI-carpeta 
&Scoped-Define DISPLAYED-OBJECTS FI-empresa nombre-empresa FI-sector ~
nombre-sector RADIO-SET-1 fi-nro FI-desde-fecha RADIO-SET-2 FI-hasta-fecha ~
FI-carpeta 

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

DEFINE VARIABLE FI-carpeta AS CHARACTER FORMAT "X(256)":U INITIAL "z:~\temp~\" 
     LABEL "Carpeta" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE FI-desde-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Empresa Cosechera" 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .95 NO-UNDO.

DEFINE VARIABLE FI-hasta-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-nro AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Nro" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.19
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Tractor", 1,
"Maquina", 2
     SIZE 16 BY 1.67 NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Detalle", 1,
"Excel", 2
     SIZE 24 BY 1.43 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.24 COL 79 WIDGET-ID 4
     FI-empresa AT ROW 2.43 COL 21.2 COLON-ALIGNED WIDGET-ID 8
     nombre-empresa AT ROW 2.43 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     Btn_Cancel AT ROW 2.43 COL 79 WIDGET-ID 2
     FI-sector AT ROW 3.62 COL 21 COLON-ALIGNED WIDGET-ID 14
     nombre-sector AT ROW 3.62 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     RADIO-SET-1 AT ROW 5.29 COL 24 NO-LABEL WIDGET-ID 20
     fi-nro AT ROW 5.52 COL 46 COLON-ALIGNED WIDGET-ID 12
     FI-desde-fecha AT ROW 7.19 COL 21 COLON-ALIGNED WIDGET-ID 6
     RADIO-SET-2 AT ROW 7.67 COL 47 NO-LABEL WIDGET-ID 24
     FI-hasta-fecha AT ROW 8.38 COL 21 COLON-ALIGNED WIDGET-ID 10
     FI-carpeta AT ROW 9.81 COL 21 COLON-ALIGNED WIDGET-ID 30
     "  Filtros" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 28
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.6 BY 11.57 WIDGET-ID 100.


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
         TITLE              = "Tareas x Tractor/Maquina"
         HEIGHT             = 11.57
         WIDTH              = 98.6
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" wWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
ON END-ERROR OF wWin /* Tareas x Tractor/Maquina */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Tareas x Tractor/Maquina */
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
  define var v_zona as character.
  define var v_titulo as character format "x(60)".

if fi-sector:screen-value in frame {&frame-name} <> "0" Then
      v_filtro = "items_control_tareas.id_empresa = " + fi-empresa:screen-value    +
           " and items_control_tareas.id_sector = " + fi-sector:screen-value  +
           " and items_control_tareas.fecha >= date('" + fi-desde-fecha:screen-value  + "')" + 
           " and items_control_tareas.fecha <= date('" + fi-hasta-fecha:screen-value  + "')" +
           " and items_control_tareas.id_tarea <> 0".
   Else
      v_filtro = "items_control_tareas.id_empresa = " + fi-empresa:screen-value    +
           " and items_control_tareas.fecha >= date('" + fi-desde-fecha:screen-value  + "')" + 
           " and items_control_tareas.fecha <= date('" + fi-hasta-fecha:screen-value  + "')" +
           " and items_control_tareas.id_tarea <> 0".
           

case radio-set-1:screen-value :
   when "1" Then
    do:
     v_filtro = v_filtro + " and items_control_tareas.nro_tractor = " + fi-nro:screen-value in frame {&frame-name}.   
     v_titulo = "Nro de tractor: " + fi-nro:screen-value in frame {&frame-name}. 
    end. 
   when "2" Then
    do:
     v_filtro = v_filtro + " and items_control_tareas.nro_maquina = " + fi-nro:screen-value in frame {&frame-name}.   
     v_titulo = "Nro de Maquina: " + fi-nro:screen-value in frame {&frame-name}. 
    end. 
end case.   

RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.

case radio-set-2:screen-value:
  when "1"  Then
  do:
    RUN  aderb\_prntrb2(
         (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
          "tareas_por_maquina", /* RB-REPORT-NAME */
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
          "Tarja x Tractor/Maquina",         /* RB-WINDOW-TITLE */
          yes,                           /* RB-DISPLAY-ERRORS */
          yes,                           /* RB-DISPLAY-STATUS */
          no,                              /* RB-NO-WAIT */
          "v_general = " + fi-desde-fecha:screen-value + ";" + fi-hasta-fecha:screen-value +
          ";" + v_titulo
          /* RB-OTHER-PARAMETERS */,
          ""
          ).   
   end.
   when "2" Then
    do:
        run p_excel_maquinarias.p (input integer(fi-sector:screen-value in frame {&frame-name}),
                                   input date(fi-desde-fecha:screen-value),
                                   input date(fi-hasta-fecha:screen-value),
                                   INPUT fi-carpeta:SCREEN-VALUE).
    end.
end.   
os-delete value(RB-MEMO-FILE).


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
  DISPLAY FI-empresa nombre-empresa FI-sector nombre-sector RADIO-SET-1 fi-nro 
          FI-desde-fecha RADIO-SET-2 FI-hasta-fecha FI-carpeta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK FI-empresa Btn_Cancel FI-sector RADIO-SET-1 fi-nro 
         FI-desde-fecha RADIO-SET-2 FI-hasta-fecha FI-carpeta 
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

