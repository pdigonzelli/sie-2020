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
&Scoped-Define ENABLED-OBJECTS Btn_OK Fi-sector Btn_Cancel FI-empresa ~
T-todos FI-desde-fecha RADIO-SET-1 FI-hasta-fecha FI-cant-copias ~
RADIO-SET-2 FI-impresora B-cambiar 
&Scoped-Define DISPLAYED-OBJECTS Fi-sector nombre-sector FI-empresa ~
nombre-empresa T-todos FI-desde-fecha RADIO-SET-1 FI-hasta-fecha ~
FI-cant-copias RADIO-SET-2 FI-impresora 

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

DEFINE VARIABLE FI-empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Empresa Cosechera" 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .95 NO-UNDO.

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

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Certificacion", 1,
"Resumen x zona", 2
     SIZE 26 BY 2.14 NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pantalla", 1,
"Impresora", 2
     SIZE 24 BY 1.67 NO-UNDO.

DEFINE VARIABLE T-todos AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.48 COL 86 WIDGET-ID 6
     Fi-sector AT ROW 2.67 COL 21 COLON-ALIGNED WIDGET-ID 18
     nombre-sector AT ROW 2.67 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     Btn_Cancel AT ROW 2.67 COL 86 WIDGET-ID 4
     FI-empresa AT ROW 4.1 COL 21.2 COLON-ALIGNED WIDGET-ID 12
     nombre-empresa AT ROW 4.1 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     T-todos AT ROW 4.1 COL 76 WIDGET-ID 32
     FI-desde-fecha AT ROW 5.29 COL 21 COLON-ALIGNED WIDGET-ID 10
     RADIO-SET-1 AT ROW 5.76 COL 51 NO-LABEL WIDGET-ID 24
     FI-hasta-fecha AT ROW 6.48 COL 21 COLON-ALIGNED WIDGET-ID 14
     FI-cant-copias AT ROW 9.33 COL 21 COLON-ALIGNED WIDGET-ID 8
     RADIO-SET-2 AT ROW 9.33 COL 41 NO-LABEL WIDGET-ID 28
     FI-impresora AT ROW 11.24 COL 21 COLON-ALIGNED WIDGET-ID 16
     B-cambiar AT ROW 11.24 COL 86 WIDGET-ID 2
     "  Filtros" VIEW-AS TEXT
          SIZE 79 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 36
          BGCOLOR 1 FGCOLOR 15 
     "Opciones de impresión" VIEW-AS TEXT
          SIZE 77 BY .71 AT ROW 8.14 COL 4 WIDGET-ID 34
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

if t-todos:screen-value = 'yes' Then
   do:
       message "Opcion no instalada, seleccione una empresa" view-as alert-box.
       return.
   end.        
   Else
   do:
      v_filtro = "items_control_tareas.id_empresa = " + fi-empresa:screen-value    +
           " and items_control_tareas.id_sector = " + fi-sector:screen-value    +
           " and items_control_tareas.fecha >= date('" + fi-desde-fecha:screen-value  + "')" + 
           " and items_control_tareas.fecha <= date('" + fi-hasta-fecha:screen-value  + "')" +
           " and items_control_tareas.id_tarea <> 0 ".  
       run imprime-reporte (input v_filtro). 
   end.
   
 
   
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON U1 OF Fi-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-todos wWin
ON VALUE-CHANGED OF T-todos IN FRAME fMain /* Todas */
DO:
  if t-todos:screen-value = 'yes' Then
     do:
        fi-empresa:sensitive = false.
        nombre-empresa:sensitive = false.
     end.
    Else
     do:
        fi-empresa:sensitive = true.
        nombre-empresa:sensitive = false.
     end. 
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
  DISPLAY Fi-sector nombre-sector FI-empresa nombre-empresa T-todos 
          FI-desde-fecha RADIO-SET-1 FI-hasta-fecha FI-cant-copias RADIO-SET-2 
          FI-impresora 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK Fi-sector Btn_Cancel FI-empresa T-todos FI-desde-fecha 
         RADIO-SET-1 FI-hasta-fecha FI-cant-copias RADIO-SET-2 FI-impresora 
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
define var v_precios like tarifas_agricola.precio EXTENT 12.
define var v_comision like tarifas_agricola.precio EXTENT 12.
define var v_iva like tarifas_agricola.precio EXTENT 12.
define var i as integer.
define var v_fecha_tarifa as date.
define var v_tipo_impresion as character.
define var v_imprime_finca as integer.
define var v_sector as integer.
define var v_tipo_liq as integer.
define var v_anio as integer.
define var v_mes as integer.
define var v_quincena as integer.
define var v_nro_comp as character format "x(20)".
define var v_empresa as integer.

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

v_nro_comp = "".
find first liquidacion_empresa where 
   liquidacion_empresa.id_tipo_liq = v_tipo_liq and
   liquidacion_empresa.id_empresa = v_empresa and
   liquidacion_empresa.id_sector = v_sector and
   liquidacion_empresa.anio = v_anio and
   liquidacion_empresa.mes = v_mes and
   liquidacion_empresa.quincena = v_quincena no-lock no-error.
 if available liquidacion_empresa Then
    do:
        v_nro_comp = string(liquidacion_empresa.id_punto_venta,"9999") + "-" +
        string(liquidacion_empresa.nro_comp,"99999999").
    end. 
   Else
    do:
     message "Liquidacion de Empresas no disponible" view-as alert-box.
    end.   



case radio-set-1:screen-value:
  when "1" Then
    v_imprime_finca = 1.
  when "2" Then
    v_imprime_finca = 0.  
end case.

case radio-set-2:screen-value:
  when "1" Then
    v_tipo_impresion = "D".
  when "2" Then
    v_tipo_impresion = "".  
end case.



RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.

v_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}).
 
case v_sector:
   when 1 or when 5 or when 6 or when 7 Then
     do:

        i = 0.
        for each unidades_liquidacion where orden_cert_agricola <> 0 no-lock by orden_cert_agricola:
          i = i + 1.
          if i > 10 Then leave.
          find first tarifas_agricola where tarifas_agricola.id_empresa = 
          integer(fi-empresa:screen-value) and tarifas_agricola.id_unidad_liquidacion =
          unidades_liquidacion.id_unidad_liquidacion and tarifas_agricola.fecha = v_fecha_tarifa no-lock no-error.
          if available tarifas_agricola Then
            do:
             v_precios[i] = tarifas_agricola.precio. 
             v_comision[i] = tarifas_agricola.comision. 
             find first r_unidades_impuestos where 
                 r_unidades_impuestos.id_unidad_liquidacion = unidades_liquidacion.id_unidad_liquidacion and
                 r_unidades_impuestos.id_sector = v_sector and
                 r_unidades_impuestos.id_impuesto = 1 no-lock no-error.
             if available r_unidades_impuestos Then
                v_iva[i] = r_unidades_impuestos.porcentaje. 
                    
            end. 
          /* Else
             message "No definida la tarifa para la unidad " + string(unidades_liquidacion.id_unidad_liquidacion)
             view-as alert-box. */
        end.


     RUN  aderb\_prntrb2(
       (vlc_dir_objetos) +  "gui\agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
       "res_finca_3eros_total_v", /* RB-REPORT-NAME */
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
       string(v_precios[1]) + "|" + string(v_precios[2]) + "|" +
       string(v_precios[3]) + "|" + string(v_precios[4]) + "|" +
       string(v_precios[5]) + "|" + string(v_precios[6]) + "|" +
       string(v_precios[7]) + "|" + string(v_precios[8]) + "|" +
       string(v_precios[9]) + "|" + string(v_precios[10]) + "|" +
       string(v_comision[1]) + "|" + string(v_comision[2]) + "|" +
       string(v_comision[3]) + "|" + string(v_comision[4]) + "|" +
       string(v_comision[5]) + "|" + string(v_comision[6]) + "|" +
       string(v_comision[7]) + "|" + string(v_comision[8]) + "|" +
       string(v_comision[9]) + "|" + string(v_comision[10]) + "|" +
       string(v_iva[1]) + "|" + string(v_iva[2]) + "|" +
       string(v_iva[3]) + "|" + string(v_iva[4]) + "|" +
       string(v_iva[5]) + "|" + string(v_iva[6]) + "|" +
       string(v_iva[7]) + "|" + string(v_iva[8]) + "|" +
       string(v_iva[9]) + "|" + string(v_iva[10]) + "|" + 
       string(v_imprime_finca) + "|" + v_nro_comp + "|"
       /* RB-OTHER-PARAMETERS */,
       ""
       ). 
     end.
    when 2 Then
      do:
        i = 0.
        for each unidades_liquidacion where orden_cert_cosecha <> 0 no-lock by orden_cert_cosecha:
          i = i + 1.
          if i > 12 Then leave.
          find first tarifas_agricola where tarifas_agricola.id_empresa = 
          integer(fi-empresa:screen-value) and tarifas_agricola.id_unidad_liquidacion =
          unidades_liquidacion.id_unidad_liquidacion and tarifas_agricola.fecha = v_fecha_tarifa no-lock no-error.
          if available tarifas_agricola Then
            do:
             v_precios[i] = tarifas_agricola.precio. 
             v_comision[i] = tarifas_agricola.comision. 
             find first r_unidades_impuestos where 
                 r_unidades_impuestos.id_unidad_liquidacion = unidades_liquidacion.id_unidad_liquidacion and
                 r_unidades_impuestos.id_sector = v_sector and
                 r_unidades_impuestos.id_impuesto = 1 no-lock no-error.
             if available r_unidades_impuestos Then
                v_iva[i] = r_unidades_impuestos.porcentaje. 
                    
            end. 
        end.
      
       RUN  aderb\_prntrb2(
       (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
       "res_finca_3eros_total_v_cos", /* RB-REPORT-NAME */
       "",                             /* RB-DB-CONNECTION */
       "O",                             /* RB-INCLUDE-RECORDS */
       v_filtro,                        /* RB-FILTER */
       RB-MEMO-FILE,                   /* RB-MEMO-FILE */
       v_tipo_impresion,               /* RB-PRINT-DESTINATION */
       fi-impresora:screen-value,      /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        1,                              /* RB-NUMBER-COPIES  - zero */                  
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "Certificacion - Cosecha",         /* RB-WINDOW-TITLE */
       yes,                           /* RB-DISPLAY-ERRORS */
       yes,                           /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "v_general = " + fi-desde-fecha:screen-value + "|" + fi-hasta-fecha:screen-value + "|" +
       string(v_precios[1]) + "|" + string(v_precios[2]) + "|" +
       string(v_precios[3]) + "|" + string(v_precios[4]) + "|" +
       string(v_precios[5]) + "|" + string(v_precios[6]) + "|" +
       string(v_precios[7]) + "|" + string(v_precios[8]) + "|" +
       string(v_precios[9]) + "|" + string(v_precios[10]) + "|" +
       string(v_precios[11]) + "|" +
       string(v_comision[1]) + "|" + string(v_comision[2]) + "|" +
       string(v_comision[3]) + "|" + string(v_comision[4]) + "|" +
       string(v_comision[5]) + "|" + string(v_comision[6]) + "|" +
       string(v_comision[7]) + "|" + string(v_comision[8]) + "|" +
       string(v_comision[9]) + "|" + string(v_comision[10]) + "|" +
       string(v_comision[11]) + "|" +
       string(v_iva[1]) + "|" + string(v_iva[2]) + "|" +
       string(v_iva[3]) + "|" + string(v_iva[4]) + "|" +
       string(v_iva[5]) + "|" + string(v_iva[6]) + "|" +
       string(v_iva[7]) + "|" + string(v_iva[8]) + "|" +
       string(v_iva[9]) + "|" + string(v_iva[10]) + "|" +  
       string(v_iva[11]) + "|" + v_nro_comp + "|"
       /* RB-OTHER-PARAMETERS */,
       ""
       ).   

      end. 
 end case.    

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

