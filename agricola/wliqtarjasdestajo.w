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
&Scoped-Define ENABLED-OBJECTS Btn_OK Fi-sector Btn_Cancel fi-empresa ~
T-todos FI-desde-fecha FI-hasta-fecha 
&Scoped-Define DISPLAYED-OBJECTS Fi-sector nombre-sector fi-empresa ~
nombre-empresa T-todos FI-desde-fecha FI-hasta-fecha 

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

DEFINE VARIABLE Fi-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE T-todos AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.48 COL 86 WIDGET-ID 4
     Fi-sector AT ROW 2.67 COL 19 COLON-ALIGNED WIDGET-ID 34
     nombre-sector AT ROW 2.67 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     Btn_Cancel AT ROW 2.67 COL 86 WIDGET-ID 2
     fi-empresa AT ROW 4.1 COL 19 COLON-ALIGNED WIDGET-ID 32
     nombre-empresa AT ROW 4.1 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     T-todos AT ROW 4.33 COL 78 WIDGET-ID 18
     FI-desde-fecha AT ROW 5.52 COL 21 COLON-ALIGNED WIDGET-ID 6
     FI-hasta-fecha AT ROW 6.71 COL 21 COLON-ALIGNED WIDGET-ID 10
     "  Filtros" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 20
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.8 BY 7.57 WIDGET-ID 100.


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
         TITLE              = "Certificacion Destajos"
         HEIGHT             = 7.76
         WIDTH              = 102.8
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
ON END-ERROR OF wWin /* Certificacion Destajos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Certificacion Destajos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* Imprimir */
DO:
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  define var v_filtro as character initial "".
  define var v_fecha_desde as date.
  define var v_fecha_tarifa as date.
  define var v_nro_reporte as integer.
  
  
v_fecha_desde = date(fi-desde-fecha:screen-value).  
v_nro_reporte = RANDOM(1,1000000).

/*

for each rb_tarifas_destajo /*where rb_tarifas_destajo.id_reporte = v_nro_reporte*/:
   delete rb_tarifas_destajo.
end.

if t-todos:screen-value = "yes" Then
  run genera-tarifas-gral (input v_nro_reporte). 
 Else
  run genera-tarifas (input v_nro_reporte). 



RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.

      RUN  aderb\_prntrb2(
      (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
       "liq_res_destajo_total_v", /* RB-REPORT-NAME */
       "",                              /* RB-DB-CONNECTION */
       "O",                             /* RB-INCLUDE-RECORDS */
       v_filtro,                        /* RB-FILTER */
       RB-MEMO-FILE,                    /* RB-MEMO-FILE */
       "D",                             /* RB-PRINT-DESTINATION */
       "?",                              /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        1,                              /* RB-NUMBER-COPIES  - zero */                  
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "Resumen Destajo",         /* RB-WINDOW-TITLE */
       yes,                           /* RB-DISPLAY-ERRORS */
       yes,                           /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "v_general = " + fi-desde-fecha:screen-value + "|" + fi-hasta-fecha:screen-value + "|"
       /* RB-OTHER-PARAMETERS */,
       ""
       ).
  
os-delete value(RB-MEMO-FILE).

for each rb_tarifas_destajo where rb_tarifas_destajo.id_reporte = v_nro_reporte:
   delete rb_tarifas_destajo.
end.
*/

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


&Scoped-define SELF-NAME T-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-todos wWin
ON VALUE-CHANGED OF T-todos IN FRAME fMain /* Todas */
DO:
  if t-todos:screen-value = 'yes' Then
    do:
       fi-empresa:screen-value = "".
       nombre-empresa:screen-value = "".
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
  DISPLAY Fi-sector nombre-sector fi-empresa nombre-empresa T-todos 
          FI-desde-fecha FI-hasta-fecha 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK Fi-sector Btn_Cancel fi-empresa T-todos FI-desde-fecha 
         FI-hasta-fecha 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genera-tarifas wWin 
PROCEDURE genera-tarifas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter v_nro_reporte as integer.
define var v_fecha_tarifa as date.
define var v_precio_tarifa like liq_tarifas_destajo.precio.
define var v_sector as integer.


v_sector = integer(fi-sector:screen-valu in frame {&frame-name}).
  

for each liq_items_control_tareas where
         liq_items_control_tareas.id_empresa = integer(fi-empresa:screen-value in frame {&frame-name}) and
         liq_items_control_tareas.id_sector = integer(fi-sector:screen-value) and 
         liq_items_control_tareas.fecha >= date(fi-desde-fecha:screen-value) and 
         liq_items_control_tareas.fecha <= date(fi-hasta-fecha:screen-value) and
         liq_items_control_tareas.id_tarea <> 0 and liq_items_control_tareas.id_tipo_planilla <> 4 AND
         liq_items_control_tareas.id_unidad_liquidacion = 4 and
         (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea > 118)  no-lock: 
         
          if day(liq_items_control_tareas.fecha) <= 15 Then
                   v_fecha_tarifa = date("01/" + string(month(liq_items_control_tareas.fecha),"99") + "/" +
                                   string(year(liq_items_control_tareas.fecha),"9999")).
                 Else
                   v_fecha_tarifa = date("16/" + string(month(liq_items_control_tareas.fecha),"99") + "/" +
                                   string(year(liq_items_control_tareas.fecha),"9999")).
 
          find first liq_tarifas_destajo where 
                    liq_tarifas_destajo.id_empresa = liq_items_control_tareas.id_empresa and
                    liq_tarifas_destajo.id_proveedor = liq_items_control_tareas.id_proveedor and
                    liq_tarifas_destajo.id_origen = liq_items_control_tareas.id_origen and
                    liq_tarifas_destajo.id_lote = liq_items_control_tareas.id_lote and
                    liq_tarifas_destajo.id_tarea = liq_items_control_tareas.id_tarea and
                    liq_tarifas_destajo.fecha = v_fecha_tarifa no-lock no-error. 
          if available liq_tarifas_destajo Then          
                  v_precio_tarifa = liq_tarifas_destajo.precio.                          
              Else
                  v_precio_tarifa = 0.
                  
         find first rb_tarifas_destajo where 
            rb_tarifas_destajo.id_reporte = v_nro_reporte and
            rb_tarifas_destajo.id_empresa = liq_items_control_tareas.id_empresa and
            rb_tarifas_destajo.id_sector = liq_items_control_tareas.id_sector and
            rb_tarifas_destajo.id_proveedor = liq_items_control_tareas.id_proveedor and
            rb_tarifas_destajo.id_origen = liq_items_control_tareas.id_origen and
            rb_tarifas_destajo.id_lote = liq_items_control_tareas.id_lote and
            rb_tarifas_destajo.id_tarea = liq_items_control_tareas.id_tarea and
            rb_tarifas_destajo.precio = v_precio_tarifa no-error.
         if not available rb_tarifas_destajo Then
            do:
               create rb_tarifas_destajo.
               assign rb_tarifas_destajo.id_reporte = v_nro_reporte.                  .
               buffer-copy liq_items_control_tareas except cantidad to rb_tarifas_destajo.
               assign rb_tarifas_destajo.precio = v_precio_tarifa.
               find first r_unidades_impuestos where r_unidades_impuestos.id_unidad_liquidacion = 4 and
                          r_unidades_impuestos.id_sector = v_sector and
                          r_unidades_impuestos.id_impuesto = 1 no-lock no-error.
               if available r_unidades_impuestos Then
                  rb_tarifas_destajo.iva = r_unidades_impuestos.porcentaje.                                 
            end.
          rb_tarifas_destajo.cantidad = rb_tarifas_destajo.cantidad + liq_items_control_tareas.cantidad.
end.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genera-tarifas-gral wWin 
PROCEDURE genera-tarifas-gral :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter v_nro_reporte as integer.
define var v_fecha_tarifa as date.
define var v_precio_tarifa like liq_tarifas_destajo.precio.
define var v_sector as integer.


v_sector = integer(fi-sector:screen-valu in frame {&frame-name}).
  

for each liq_items_control_tareas where
         liq_items_control_tareas.id_empresa > 1 and
         liq_items_control_tareas.id_sector = integer(fi-sector:screen-value in frame {&frame-name}) and 
         liq_items_control_tareas.fecha >= date(fi-desde-fecha:screen-value) and 
         liq_items_control_tareas.fecha <= date(fi-hasta-fecha:screen-value) and
         liq_items_control_tareas.id_tarea <> 0 and liq_items_control_tareas.id_tipo_planilla <> 4 AND 
         liq_items_control_tareas.id_unidad_liquidacion = 4 and
         (liq_items_control_tareas.id_tarea < 107 or liq_items_control_tareas.id_tarea > 118)  no-lock: 
        
         if day(liq_items_control_tareas.fecha) <= 15 Then
                   v_fecha_tarifa = date("01/" + string(month(liq_items_control_tareas.fecha),"99") + "/" +
                                   string(year(liq_items_control_tareas.fecha),"9999")).
                 Else
                   v_fecha_tarifa = date("16/" + string(month(liq_items_control_tareas.fecha),"99") + "/" +
                                   string(year(liq_items_control_tareas.fecha),"9999")).
 
          find first liq_tarifas_destajo where 
                    liq_tarifas_destajo.id_empresa = liq_items_control_tareas.id_empresa and
                    liq_tarifas_destajo.id_proveedor = liq_items_control_tareas.id_proveedor and
                    liq_tarifas_destajo.id_origen = liq_items_control_tareas.id_origen and
                    liq_tarifas_destajo.id_lote = liq_items_control_tareas.id_lote and
                    liq_tarifas_destajo.id_tarea = liq_items_control_tareas.id_tarea and
                    liq_tarifas_destajo.fecha = v_fecha_tarifa no-lock no-error. 
          if available liq_tarifas_destajo Then          
                  v_precio_tarifa = liq_tarifas_destajo.precio.                          
              Else
                  v_precio_tarifa = 0.
        
         find first rb_tarifas_destajo where 
            rb_tarifas_destajo.id_reporte = v_nro_reporte and
            rb_tarifas_destajo.id_empresa = liq_items_control_tareas.id_empresa and
            rb_tarifas_destajo.id_sector = liq_items_control_tareas.id_sector and
            rb_tarifas_destajo.id_proveedor = liq_items_control_tareas.id_proveedor and
            rb_tarifas_destajo.id_origen = liq_items_control_tareas.id_origen and
            rb_tarifas_destajo.id_lote = liq_items_control_tareas.id_lote and
            rb_tarifas_destajo.id_tarea = liq_items_control_tareas.id_tarea and
            rb_tarifas_destajo.precio = v_precio_tarifa no-error.
         if not available rb_tarifas_destajo Then
            do:
               create rb_tarifas_destajo.
               assign rb_tarifas_destajo.id_reporte = v_nro_reporte.                  .
               buffer-copy liq_items_control_tareas except cantidad to rb_tarifas_destajo.
               assign rb_tarifas_destajo.precio = v_precio_tarifa.
                  
               find first r_unidades_impuestos where r_unidades_impuestos.id_unidad_liquidacion = 4 and
                          r_unidades_impuestos.id_sector = v_sector and 
                          r_unidades_impuestos.id_impuesto = 1 no-lock no-error.
                   if available r_unidades_impuestos Then
                    rb_tarifas_destajo.iva = r_unidades_impuestos.porcentaje.                                 
            end.
          rb_tarifas_destajo.cantidad = rb_tarifas_destajo.cantidad + liq_items_control_tareas.cantidad.
end.



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

