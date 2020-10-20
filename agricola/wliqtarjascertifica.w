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
&Scoped-Define ENABLED-OBJECTS Btn_OK fi-empresa Btn_Cancel Fi-sector ~
FI-desde-fecha RADIO-SET-3 FI-hasta-fecha FI-cant-copias RADIO-SET-2 ~
Fi-tipo-liq FI-impresora B-cambiar 
&Scoped-Define DISPLAYED-OBJECTS fi-empresa nombre-empresa nombre-sector ~
Fi-sector FI-desde-fecha RADIO-SET-3 FI-hasta-fecha FI-cant-copias ~
RADIO-SET-2 Fi-tipo-liq FI-impresora 

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

DEFINE VARIABLE FI-cant-copias AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "Cant. copias" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.19 NO-UNDO.

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

DEFINE VARIABLE FI-impresora AS CHARACTER FORMAT "X(256)":U 
     LABEL "Impresora" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1.19 NO-UNDO.

DEFINE VARIABLE Fi-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE Fi-tipo-liq AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Liq" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pantalla", 1,
"Impresora", 2
     SIZE 24 BY 1.67 NO-UNDO.

DEFINE VARIABLE RADIO-SET-3 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Certificacion", 1,
"Cert. RH", 2
     SIZE 24 BY 1.81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.48 COL 86 WIDGET-ID 6
     fi-empresa AT ROW 2.67 COL 19 COLON-ALIGNED WIDGET-ID 38
     nombre-empresa AT ROW 2.67 COL 29.8 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     Btn_Cancel AT ROW 2.67 COL 86 WIDGET-ID 4
     nombre-sector AT ROW 3.86 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     Fi-sector AT ROW 3.95 COL 19 COLON-ALIGNED WIDGET-ID 40
     FI-desde-fecha AT ROW 5.29 COL 21 COLON-ALIGNED WIDGET-ID 10
     RADIO-SET-3 AT ROW 5.76 COL 54 NO-LABEL WIDGET-ID 48
     FI-hasta-fecha AT ROW 6.48 COL 21 COLON-ALIGNED WIDGET-ID 14
     FI-cant-copias AT ROW 9.33 COL 21 COLON-ALIGNED WIDGET-ID 8
     RADIO-SET-2 AT ROW 9.33 COL 41 NO-LABEL WIDGET-ID 28
     Fi-tipo-liq AT ROW 9.57 COL 76 COLON-ALIGNED WIDGET-ID 46
     FI-impresora AT ROW 11.24 COL 21 COLON-ALIGNED WIDGET-ID 16
     B-cambiar AT ROW 11.24 COL 86 WIDGET-ID 2
     "Opciones de impresión" VIEW-AS TEXT
          SIZE 77 BY .71 AT ROW 8.14 COL 4 WIDGET-ID 34
          BGCOLOR 1 FGCOLOR 15 
     "  Filtros" VIEW-AS TEXT
          SIZE 79 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 36
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.2 BY 12.33 WIDGET-ID 100.


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
         TITLE              = "Certificacion de Tareas"
         HEIGHT             = 12.52
         WIDTH              = 108.2
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
ON END-ERROR OF wWin /* Certificacion de Tareas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Certificacion de Tareas */
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* Imprimir */
DO:
  define var v_filtro as character initial "".

  CASE fi-tipo-liq:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
      WHEN "" THEN
          v_filtro = "liq_items_tarjas.id_empresa = " + fi-empresa:screen-value    +
               " and liq_items_tarjas.id_sector = " + fi-sector:screen-value    +
               " and liq_items_tarjas.fecha >= date('" + fi-desde-fecha:screen-value  + "')" + 
               " and liq_items_tarjas.fecha <= date('" + fi-hasta-fecha:screen-value  + "')" +
               " and liq_items_tarjas.id_tarea <> 0 " +
               " and liq_items_tarjas.id_tipo_planilla <> 4".  
      OTHERWISE 
          v_filtro = "liq_items_tarjas.id_empresa = " + fi-empresa:screen-value    +
               " and liq_items_tarjas.id_sector = " + fi-sector:screen-value    +
               " and liq_items_tarjas.fecha >= date('" + fi-desde-fecha:screen-value  + "')" + 
               " and liq_items_tarjas.fecha <= date('" + fi-hasta-fecha:screen-value  + "')" +
               " and liq_items_tarjas.id_tarea <> 0 " +
               " and liq_items_tarjas.id_tipo_planilla <> 4" +
               " and liq_legajos.tipo_liquidacion = '" + fi-tipo-liq:SCREEN-VALUE + "'".  

  END CASE.

   run imprime-reporte (input v_filtro).  
 
   
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
  DISPLAY fi-empresa nombre-empresa nombre-sector Fi-sector FI-desde-fecha 
          RADIO-SET-3 FI-hasta-fecha FI-cant-copias RADIO-SET-2 Fi-tipo-liq 
          FI-impresora 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK fi-empresa Btn_Cancel Fi-sector FI-desde-fecha RADIO-SET-3 
         FI-hasta-fecha FI-cant-copias RADIO-SET-2 Fi-tipo-liq FI-impresora 
         B-cambiar 
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
define input parameter v_filtro as character.

DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
define var v_precios like liq_tarifas_agricola.precio EXTENT 12.
define var v_comision like liq_tarifas_agricola.precio EXTENT 12.
define var v_iva like liq_tarifas_agricola.precio EXTENT 12.
define var i as integer.
define var v_fecha_tarifa as date.
define var v_tipo_impresion as character.
define var v_imprime_finca as integer.
define var v_imprime_grupo as integer.
define var v_sector as integer.
define var v_tipo_liq as integer.
define var v_anio as integer.
define var v_mes as integer.
define var v_quincena as integer.
define var v_nro_comp as character format "x(20)".
define var v_empresa as integer.
DEFINE VAR v_archivo AS CHARACTER.


v_tipo_liq = 1.
v_fecha_tarifa = date(fi-desde-fecha:screen-value in frame {&frame-name}).
v_anio = year(v_fecha_tarifa).
v_mes = month(v_fecha_tarifa).
v_empresa = integer(fi-empresa:screen-value).
v_sector = integer(fi-sector:screen-value).


case day(v_fecha_tarifa):
   when 1 Then v_quincena = 1.
   when 16 Then v_quincena = 2.
   otherwise 
     do:
        message "Error en la fecha de inicio de quincena" view-as alert-box.
        return. 
     end.   
end case.

v_imprime_grupo = 0.

v_imprime_finca = 1.

case radio-set-2:screen-value:
  when "1" Then
    v_tipo_impresion = "D".
  when "2" Then
    v_tipo_impresion = "".  
end case.

CASE integer(radio-set-3:SCREEN-VALUE):
    WHEN 1 THEN
        v_archivo = "liq_res_tarja_certifica_02".
    WHEN 2 THEN
        v_archivo = "liq_res_tarja_certifica_01".
END CASE.

RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.

/*v_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}). */
 

/*  vlc_dir_fuentes = "z:\sistemas\sami\sistemas\". */ 
     
  RUN  aderb\_prntrb2(
       (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
       v_archivo, /* RB-REPORT-NAME */
       "",                             /* RB-DB-CONNECTION */
       "O",                             /* RB-INCLUDE-RECORDS */
       v_filtro,                              /* RB-FILTER */
       RB-MEMO-FILE,                              /* RB-MEMO-FILE */
       v_tipo_impresion,                             /* RB-PRINT-DESTINATION */
       fi-impresora:screen-value,       /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        1,                              /* RB-NUMBER-COPIES  - zero */                  
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "Certificacion",         /* RB-WINDOW-TITLE */
       yes,                           /* RB-DISPLAY-ERRORS */
       yes,                           /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "v_general = " + fi-desde-fecha:screen-value + "|" + fi-hasta-fecha:screen-value + "|" +
                    STRING(v_imprime_finca) + "|" + STRING(v_imprime_grupo) + "|"
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

