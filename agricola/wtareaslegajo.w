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

    define temp-table t-personal
       FIELD id_empresa LIKE items_control_tareas.id_empresa 
       field legajo like items_control_tareas.legajo
       field nombre like items_control_tareas.nombre
       field dni_cuil like items_control_tareas.dni_cuil
       field id_concepto like r_tareas_unidades.id_concepto
       field cantidad like items_control_tareas.cantidad
       field id_centro_abacus like personal_finca.id_centro_abacus.

      DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
      define var v_nro_reporte as integer.

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
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel Fi-sector FI-sucursal ~
FI-desde-fecha FI-hasta-fecha fi-empresa T-todos T-fincas FI-impresora ~
B-cambiar 
&Scoped-Define DISPLAYED-OBJECTS Fi-sector nombre-sector FI-sucursal ~
nombre-sucursal FI-desde-fecha FI-hasta-fecha fi-empresa nombre-empresa ~
FI-legajo T-todos FI-id_origen FI-id_proveedor nombre-finca T-fincas ~
FI-impresora 

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

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emp Contratista" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE FI-hasta-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-id_origen AS CHARACTER FORMAT "X(256)":U 
     LABEL "Finca" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .95 NO-UNDO.

DEFINE VARIABLE FI-id_proveedor AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE FI-impresora AS CHARACTER FORMAT "X(256)":U 
     LABEL "Impresora" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-legajo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Legajo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE Fi-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE FI-sucursal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-finca AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sucursal AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .95 NO-UNDO.

DEFINE VARIABLE T-fincas AS LOGICAL INITIAL yes 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE T-todos AS LOGICAL INITIAL yes 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.24 COL 91 WIDGET-ID 6
     Btn_Cancel AT ROW 2.43 COL 91 WIDGET-ID 4
     Fi-sector AT ROW 2.67 COL 21 COLON-ALIGNED WIDGET-ID 22
     nombre-sector AT ROW 2.67 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     FI-sucursal AT ROW 3.86 COL 21 COLON-ALIGNED WIDGET-ID 24
     nombre-sucursal AT ROW 3.86 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     FI-desde-fecha AT ROW 5.52 COL 21 COLON-ALIGNED WIDGET-ID 8
     FI-hasta-fecha AT ROW 6.71 COL 21 COLON-ALIGNED WIDGET-ID 12
     fi-empresa AT ROW 8.14 COL 21 COLON-ALIGNED WIDGET-ID 10
     nombre-empresa AT ROW 8.14 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     FI-legajo AT ROW 9.57 COL 21 COLON-ALIGNED WIDGET-ID 20
     T-todos AT ROW 9.57 COL 39 WIDGET-ID 36
     FI-id_origen AT ROW 11 COL 21 COLON-ALIGNED WIDGET-ID 14
     FI-id_proveedor AT ROW 11 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     nombre-finca AT ROW 11 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     T-fincas AT ROW 11 COL 93 WIDGET-ID 34
     FI-impresora AT ROW 12.67 COL 21 COLON-ALIGNED WIDGET-ID 18
     B-cambiar AT ROW 12.67 COL 86 WIDGET-ID 2
     "  Filtros" VIEW-AS TEXT
          SIZE 81 BY .71 AT ROW 1.48 COL 3 WIDGET-ID 38
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.8 BY 14.19 WIDGET-ID 100.


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
         TITLE              = "Resumen Tarja Personal x Legajo"
         HEIGHT             = 14.19
         WIDTH              = 120.8
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
/* SETTINGS FOR FILL-IN FI-id_origen IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-id_proveedor IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-legajo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-empresa IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-finca IN FRAME fMain
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
ON END-ERROR OF wWin /* Resumen Tarja Personal x Legajo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Resumen Tarja Personal x Legajo */
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
 DEF VAR v_sector AS INTEGER.
 DEF VAR v_sucursal AS INTEGER.
 DEF VAR v_fecha_desde AS DATE.
 DEF VAR v_fecha_hasta AS DATE.
 DEF VAR v_empresa AS INTEGER.

 v_sector = INTEGER(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_fecha_desde = DATE(fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_fecha_hasta = DATE(fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_empresa = INTEGER(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 

 RUN p_actualiza_datos.p (INPUT v_sector, INPUT v_sucursal, 
                          INPUT v_fecha_desde, INPUT v_fecha_hasta,
                          INPUT v_empresa).

 case integer(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
   WHEN 1 THEN
     run generar-tarja-agricola.
   WHEN 2 THEN
     run generar-tarja-cosecha.
   OTHERWISE
     run generar-tarja-agricola.
 end case.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresa wWin
ON LEAVE OF fi-empresa IN FRAME fMain /* Emp Contratista */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresa wWin
ON MOUSE-SELECT-DBLCLICK OF fi-empresa IN FRAME fMain /* Emp Contratista */
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


&Scoped-define SELF-NAME FI-id_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-id_origen wWin
ON MOUSE-SELECT-DBLCLICK OF FI-id_origen IN FRAME fMain /* Finca */
DO:
 define var r as rowid no-undo.
 run c_fincas_propias.w (output r).
 find origenes where rowid(origenes) = r no-lock no-error.
 if available origenes then 
 do:
   fi-id_proveedor:screen-value = string(origenes.id_proveedor).
   fi-id_origen:screen-value = string(origenes.id_origen).
   nombre-finca:screen-value = origenes.descripcion.
 end.

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


&Scoped-define SELF-NAME FI-sucursal
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


&Scoped-define SELF-NAME T-fincas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-fincas wWin
ON VALUE-CHANGED OF T-fincas IN FRAME fMain /* Todas */
DO:
   if t-fincas:screen-value = 'yes' Then
    do:
    fi-id_origen:screen-value = "".
    nombre-finca:screen-value = "".
    fi-id_origen:sensitive = false.
    fi-id_proveedor:screen-value = "".
    end.
   Else
    fi-id_origen:sensitive = true.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-todos wWin
ON VALUE-CHANGED OF T-todos IN FRAME fMain /* Todos */
DO:
  if t-todos:screen-value = 'yes' Then
    fi-legajo:sensitive = false.
   Else
    fi-legajo:sensitive = true.
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

 
 find first sucursales where sucursales.id_sucursal = INTEGER(fi-sucursal:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
 if available sucursales Then
          nombre-sucursal:screen-value = sucursales.nombre.
       ELSE
         nombre-sucursal:screen-value = "".


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
  DISPLAY Fi-sector nombre-sector FI-sucursal nombre-sucursal FI-desde-fecha 
          FI-hasta-fecha fi-empresa nombre-empresa FI-legajo T-todos 
          FI-id_origen FI-id_proveedor nombre-finca T-fincas FI-impresora 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK Btn_Cancel Fi-sector FI-sucursal FI-desde-fecha FI-hasta-fecha 
         fi-empresa T-todos T-fincas FI-impresora B-cambiar 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generar-tarja-agricola wWin 
PROCEDURE generar-tarja-agricola :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define var v_general as character.
  define var j as integer.

  def var v_codigo as integer.
  def var v_codigo-1 as integer.
  def var v_codigo-2 as integer.
  def var v_resumen as character.
  define var v_cargo like personal_finca.id_cargo.
  define var v_centro like personal_finca.id_centro_abacus.
  DEF VAR v-sector AS INTEGER.
  DEF VAR v-sucursal AS INTEGER.


  v-sector = INTEGER(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v-sucursal = INTEGER(fi-sucursal:SCREEN-VALUE IN FRAME {&FRAME-NAME}).


if fi-empresa:screen-value in frame {&FRAME-NAME} = "" Then
   do:
     message "Debe ingresar una empresa" view-as alert-box.
     return.
   end.

if fi-sucursal:screen-value in frame {&FRAME-NAME} = "" Then
   do:
     message "Debe ingresar una sucursal" view-as alert-box.
     return.
   end.

 v_nro_reporte = RANDOM(1,1000000).

 for each t-personal:
   delete t-personal.
 end.
 
 for each rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte AND
     rb_tarja.id_empresa = INTEGER(fi-empresa:SCREEN-VALUE) AND
     rb_tarja.id_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE) AND
     rb_tarja.id_sector = INTEGER(fi-sector:SCREEN-VALUE):
   for each rb_items_tarja of rb_tarja:
      delete rb_items_tarja.
   end.
   delete rb_tarja.
 end.      
 

if t-fincas:screen-value = 'yes' or integer(fi-empresa:screen-value) <> 1 Then
 do:
 for each items_control_tareas where 
      items_control_tareas.id_empresa = integer(fi-empresa:screen-value) and 
      items_control_tareas.id_sucursal = integer(fi-sucursal:screen-value) and 
      items_control_tareas.fecha >= date(fi-desde-fecha:screen-value) and 
      items_control_tareas.fecha <= date(fi-hasta-fecha:screen-value) and
      items_control_tarea.id_sector = integer(fi-sector:screen-value) AND
      items_control_tareas.id_tipo_planilla <> 4 AND
      (items_control_tareas.cant_jornal <> 0 OR items_control_tareas.cant_horas <> 0 OR items_control_tareas.cantidad <> 0) 
      no-lock by items_control_tareas.id_empresa by items_control_tareas.legajo
      by items_control_tareas.nombre by items_control_tareas.fecha :

      if items_control_tareas.id_tarea = 0 Then next.

      find first personal_finca where personal_finca.id_empresa_cosechera =
       items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.

       if available personal_finca Then
         do:
          v_cargo = personal_finca.id_cargo.
          v_centro = personal_finca.id_centro_abacus.
         end. 
         Else
         do:
          v_cargo = 0. 
          v_centro = 0.
         end. 
        find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
       ASSIGN  v_codigo = 0
               v_codigo-1 = 0
               v_codigo-2 = 0.

     {calcula-agricola.i}
     
     FIND FIRST rb_items_tarja WHERE
         rb_items_tarja.id_reporte = v_nro_reporte AND
         rb_items_tarja.id_empresa = items_control_tareas.id_empresa AND
         rb_items_tarja.id_sucursal = items_control_tareas.id_sucursal AND
         rb_items_tarja.id_sector = items_control_tareas.id_sector AND
         rb_items_tarja.legajo = items_control_tareas.legajo AND
         rb_items_tarja.nombre = items_control_tareas.nombre AND
         rb_items_tarja.fecha = items_control_tareas.fecha AND
         rb_items_tarja.id_tarea = items_control_tareas.id_tarea AND
         rb_items_tarja.id_proveedor = items_control_tareas.id_proveedor AND 
         rb_items_tarja.id_origen = items_control_tareas.id_origen AND
         rb_items_tarja.id_lote = items_control_tareas.id_lote and
         rb_items_tarja.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion  NO-ERROR.
     IF NOT AVAILABLE rb_items_tarja THEN
     DO:
         create rb_items_tarja.
         assign rb_items_tarja.id_reporte = v_nro_reporte
                rb_items_tarja.id_empresa = items_control_tareas.id_empresa
                rb_items_tarja.id_sucursal = items_control_tareas.id_sucursal
                rb_items_tarja.id_sector = items_control_tareas.id_sector
                rb_items_tarja.legajo = items_control_tareas.legajo
                rb_items_tarja.nombre = items_control_tareas.nombre
                rb_items_tarja.fecha = items_control_tareas.fecha
                rb_items_tarja.id_tarea = items_control_tareas.id_tarea 
                rb_items_tarja.id_proveedor = items_control_tareas.id_proveedor 
                rb_items_tarja.id_origen = items_control_tareas.id_origen 
                rb_items_tarja.id_lote = items_control_tareas.id_lote
                rb_items_tarja.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion
                rb_items_tarja.nro_tractor = items_control_tareas.nro_tractor
                rb_items_tarja.nro_maquina = items_control_tareas.nro_maquina.
     END.
          ASSIGN      
                rb_items_tarja.cant_jornal = rb_items_tarja.cant_jornal + items_control_tareas.cant_jornal
                rb_items_tarja.cant_horas = rb_items_tarja.cant_horas + items_control_tareas.cant_horas
                rb_items_tarja.cantidad = rb_items_tarja.cantidad + items_control_tareas.cantidad.

            
    find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
    if available tareas Then
       do:
         rb_items_tarja.nombre_tarea = tareas.abreviatura.
         rb_items_tarja.nombre_largo_tarea = tareas.descripcion.
       end.  

    find first origenes of items_control_tareas no-lock no-error.
    if available origenes Then
       rb_items_tarja.nombre_finca = origenes.descripcion.

    find first lotes_plantacion where lotes_plantacion.id_proveedor =  items_control_tareas.id_proveedor and
         lotes_plantacion.id_origen =  items_control_tareas.id_origen and
         lotes_plantacion.id_lote =  items_control_tareas.id_lote 
     no-lock no-error.
     
    if available lotes_plantacion Then
       rb_items_tarja.nombre_lote = lotes_plantacion.descripcion.
    
    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       rb_items_tarja.id_concepto[1] = v_codigo.
       find first t-personal where 
       t-personal.id_empresa = items_control_tareas.id_empresa AND
       t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign 
                    t-personal.id_empresa = items_control_tareas.id_empresa
                    t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_jornal_norm.
       end.
    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       rb_items_tarja.id_concepto[2] = v_codigo-1.
       find first t-personal where 
       t-personal.id_empresa = items_control_tareas.id_empresa AND
       t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = items_control_tareas.id_empresa 
                    t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       rb_items_tarja.id_concepto[3] = v_codigo-2.
       find first t-personal where 
       t-personal.id_empresa = items_control_tareas.id_empresa AND
       t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = items_control_tareas.id_empresa 
                    t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_centro.
          end. 
       t-personal.cantidad = t-personal.cantidad + items_control_tareas.cantidad.
       
       find first unidades_liquidacion where unidades_liquidacion.id_unidad_liquidacion = 
       items_control_tareas.id_unidad_liquidacion no-lock no-error.
       if available unidades_liquidacion Then
          rb_items_tarja.nombre_unidad = unidades_liquidacion.abreviatura.
     end. 
    end.
   end.   
 end. 
Else
 do:

  for each items_control_tareas where 
      items_control_tareas.id_empresa = integer(fi-empresa:screen-value) and 
      items_control_tareas.id_sucursal = integer(fi-sucursal:screen-value) and 
      items_control_tareas.fecha >= date(fi-desde-fecha:screen-value) and 
      items_control_tareas.fecha <= date(fi-hasta-fecha:screen-value) and
      items_control_tareas.id_sector = integer(fi-sector:screen-value) AND
      items_control_tareas.id_tipo_planilla <> 4 AND
      (items_control_tareas.cant_jornal <> 0 OR items_control_tareas.cant_horas <> 0 OR items_control_tareas.cantidad <> 0) 
      no-lock, first personal_finca where personal_finca.id_empresa_cosechera =
      items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock
      by items_control_tareas.id_empresa by items_control_tareas.legajo
      by items_control_tareas.nombre by items_control_tareas.fecha :
      if items_control_tareas.id_tarea = 0 Then next.
      
          ASSIGN v_cargo = personal_finca.id_cargo
                 v_centro = personal_finca.id_centro_abacus.
        
        find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
        ASSIGN v_codigo = 0
               v_codigo-1 = 0
               v_codigo-2 = 0.

     {calcula-agricola.i}    










     create rb_items_tarja.
      assign rb_items_tarja.id_reporte = v_nro_reporte
          rb_items_tarja.id_empresa = items_control_tareas.id_empresa
          rb_items_tarja.id_sucursal = items_control_tareas.id_sucursal
          rb_items_tarja.id_sector = items_control_tareas.id_sector
            rb_items_tarja.legajo = items_control_tareas.legajo
            rb_items_tarja.nombre = items_control_tareas.nombre
            rb_items_tarja.fecha = items_control_tareas.fecha
            rb_items_tarja.id_tarea = items_control_tareas.id_tarea 
            rb_items_tarja.id_proveedor = items_control_tareas.id_proveedor 
            rb_items_tarja.id_origen = items_control_tareas.id_origen 
            rb_items_tarja.id_lote = items_control_tareas.id_lote
            rb_items_tarja.cant_jornal = items_control_tareas.cant_jornal_norm
            rb_items_tarja.cant_horas = items_control_tareas.cant_horas
            rb_items_tarja.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion 
            rb_items_tarja.cantidad = items_control_tareas.cantidad
            rb_items_tarja.nro_tractor = items_control_tareas.nro_tractor
            rb_items_tarja.nro_maquina = items_control_tareas.nro_maquina.
            
    find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
    if available tareas Then
       do:
         rb_items_tarja.nombre_tarea = tareas.abreviatura.
         rb_items_tarja.nombre_largo_tarea = tareas.descripcion.
       end.  

    find first origenes of items_control_tareas no-lock no-error.
    if available origenes Then
       rb_items_tarja.nombre_finca = origenes.descripcion.

    find first lotes_plantacion where lotes_plantacion.id_proveedor =  items_control_tareas.id_proveedor and
         lotes_plantacion.id_origen =  items_control_tareas.id_origen and
         lotes_plantacion.id_lote =  items_control_tareas.id_lote 
     no-lock no-error.
     
    if available lotes_plantacion Then
       rb_items_tarja.nombre_lote = lotes_plantacion.descripcion.


    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       rb_items_tarja.id_concepto[1] = v_codigo.
       find first t-personal where 
       t-personal.id_empresa = items_control_tareas.id_empresa AND
       t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = items_control_tareas.id_empresa 
                    t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_jornal_norm.
       end.
    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       rb_items_tarja.id_concepto[2] = v_codigo-1.
       find first t-personal where 
       t-personal.id_empresa = items_control_tareas.id_empresa AND
       t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = items_control_tareas.id_empresa 
                    t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       rb_items_tarja.id_concepto[3] = v_codigo-2.
       find first t-personal where 
       t-personal.id_empresa = items_control_tareas.id_empresa AND
       t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.id_empresa = items_control_tareas.id_empresa 
                    t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_centro.
          end. 
       t-personal.cantidad = t-personal.cantidad + items_control_tareas.cantidad.
       find first unidades_liquidacion where unidades_liquidacion.id_unidad_liquidacion = 
       items_control_tareas.id_unidad_liquidacion no-lock no-error.
       if available unidades_liquidacion Then
          rb_items_tarja.nombre_unidad = unidades_liquidacion.abreviatura.
     end. 
    end.
   end.   
 end.  

j = 0.
for each t-personal no-lock break by  t-personal.legajo by t-personal.nombre 
   by t-personal.id_concepto:
   find first rb_tarja where rb_tarja.id_reporte = v_nro_reporte and
   rb_tarja.id_empresa = t-personal.id_empresa AND
   rb_tarja.id_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE) AND
   rb_tarja.id_sector = INTEGER(fi-sector:SCREEN-VALUE) AND
   rb_tarja.legajo = t-personal.legajo and
    rb_tarja.nombre = t-personal.nombre no-error.
   if not available rb_tarja Then
      do:
         create rb_tarja.
         rb_tarja.id_reporte = v_nro_reporte.
         rb_tarja.id_empresa = integer(fi-empresa:screen-value).
         rb_tarja.id_sucursal = integer(fi-sucursal:screen-value).
         rb_tarja.id_sector = integer(fi-sector:screen-value).
         rb_tarja.legajo = t-personal.legajo.
         find first personal_finca where personal_finca.id_empresa_cosechera =
         integer(fi-empresa:screen-value) and personal_finca.legajo = t-personal.legajo no-lock no-error.
         if available personal_finca Then
            do:
              rb_tarja.nombre = t-personal.nombre.
              rb_tarja.dni_cuil = personal_finca.dni_cuil.
              rb_tarja.id_cargo = personal_finca.id_cargo.
              rb_tarja.id_centro_abacus = personal_finca.id_centro_abacus.
              
              find first centros_costos_abacus where centros_costos_abacus.id_centro_abacus = rb_tarja.id_centro_abacus no-lock
              no-error.    
              if available centros_costos_abacus Then
                 rb_tarja.nombre_centro_abacus = centros_costos_abacus.descripcion.   
            end.  
           Else
             do:
               rb_tarja.nombre = t-personal.nombre.
               rb_tarja.dni_cuil = t-personal.dni_cuil.
             end.
         j = 0.
      end. 
      j = j + 1.
      
      if j <= 20 Then
      do:
      rb_tarja.id_concepto[j] = t-personal.id_concepto.  
      rb_tarja.valor_concepto[j] =  t-personal.cantidad. 
      find first conceptos_abacus where conceptos_abacus.id_concepto = t-personal.id_concepto 
           no-lock no-error.
      if available conceptos_abacus Then     
         rb_tarja.nombre_concepto[j] = conceptos_abacus.descripcion.
      end.
      Else
        message rb_tarja.legajo rb_tarja.nombre view-as alert-box.   
end.

find first rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte and
    rb_tarja.id_empresa = integer(fi-empresa:screen-value) AND
    rb_tarja.id_sucursal = integer(fi-sucursal:screen-value) AND
    rb_tarja.id_sector = integer(fi-sector:screen-value) no-lock no-error.
if available rb_tarja Then
  run imprime-reporte.
Else
  message "No hay datos registrados para esta selección" view-as alert-box
  title "Atención".  


  for each rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte AND
      rb_tarja.id_empresa = INTEGER(fi-empresa:SCREEN-VALUE) AND
      rb_tarja.id_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE) AND
      rb_tarja.id_sector = INTEGER(fi-sector:SCREEN-VALUE):
    for each rb_items_tarja of rb_tarja:
       delete rb_items_tarja.
    end.
    delete rb_tarja.
  end.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generar-tarja-cosecha wWin 
PROCEDURE generar-tarja-cosecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define var v_general as character.
  define var j as integer.

  def var v_codigo as integer.
  def var v_codigo-1 as integer.
  def var v_codigo-2 as integer.
  def var v_resumen as character.
  define var v_cargo like personal_finca.id_cargo.
  define var v_centro like personal_finca.id_centro_abacus.

if fi-empresa:screen-value in frame {&FRAME-NAME} = "" Then
   do:
     message "Debe ingresar una empresa" view-as alert-box.
     return.
   end.
   
if fi-sucursal:screen-value in frame {&FRAME-NAME} = "" Then
   do:
     message "Debe ingresar una sucursal" view-as alert-box.
     return.
   end.

 v_nro_reporte = RANDOM(1,1000000).
   
   
 for each t-personal:
   delete t-personal.
 end.
 
 for each rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte AND
     rb_tarja.id_empresa = INTEGER(fi-empresa:SCREEN-VALUE) AND
     rb_tarja.id_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE) AND
     rb_tarja.id_sector = INTEGER(fi-sector:SCREEN-VALUE):
   for each rb_items_tarja of rb_tarja:
      delete rb_items_tarja.
   end.
   delete rb_tarja.
 end.      
 

if t-fincas:screen-value = 'yes' or integer(fi-empresa:screen-value) <> 1 Then
 do:
 for each items_control_tareas where 
      items_control_tareas.id_empresa = integer(fi-empresa:screen-value) and 
      items_control_tareas.id_sucursal = integer(fi-sucursal:screen-value) and 
      items_control_tareas.fecha >= date(fi-desde-fecha:screen-value) and 
      items_control_tareas.fecha <= date(fi-hasta-fecha:screen-value) and
      items_control_tarea.id_sector = integer(fi-sector:screen-value) AND
      items_control_tareas.id_tipo_planilla <> 4 AND
     (items_control_tareas.cant_jornal <> 0 OR items_control_tareas.cant_horas <> 0 OR items_control_tareas.cantidad <> 0) 
      no-lock by items_control_tareas.id_empresa by items_control_tareas.legajo
      by items_control_tareas.nombre by items_control_tareas.fecha :

      if items_control_tareas.id_tarea = 0 and items_control_tareas.cantidad = 0 Then next.
      find first personal_finca where personal_finca.id_empresa_cosechera =
       items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.

       if available personal_finca Then
         do:
          v_cargo = personal_finca.id_cargo.
          v_centro = personal_finca.id_centro_abacus.
         end. 
         Else
         do:
          v_cargo = 0. 
          v_centro = 0.
         end. 
        find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.
       
     {calcula-cosecha.i}


          
     create rb_items_tarja.
     assign rb_items_tarja.id_reporte = v_nro_reporte
         rb_items_tarja.id_empresa = items_control_tareas.id_empresa
         rb_items_tarja.id_sucursal = items_control_tareas.id_sucursal
         rb_items_tarja.id_sector = items_control_tareas.id_sector
            rb_items_tarja.legajo = items_control_tareas.legajo
            rb_items_tarja.nombre = items_control_tareas.nombre
            rb_items_tarja.fecha = items_control_tareas.fecha
            rb_items_tarja.id_tarea = items_control_tareas.id_tarea 
            rb_items_tarja.id_proveedor = items_control_tareas.id_proveedor 
            rb_items_tarja.id_origen = items_control_tareas.id_origen 
            rb_items_tarja.id_lote = items_control_tareas.id_lote
            rb_items_tarja.cant_jornal = items_control_tareas.cant_jornal
            rb_items_tarja.cant_horas = items_control_tareas.cant_horas
            rb_items_tarja.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion 
            rb_items_tarja.cantidad = items_control_tareas.cantidad
            rb_items_tarja.nro_tractor = items_control_tareas.nro_tractor
            rb_items_tarja.nro_maquina = items_control_tareas.nro_maquina.
            
    find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
    if available tareas Then
       do:
         rb_items_tarja.nombre_tarea = tareas.abreviatura.
         rb_items_tarja.nombre_largo_tarea = tareas.descripcion.
       end.  

    find first origenes of items_control_tareas no-lock no-error.
    if available origenes Then
       rb_items_tarja.nombre_finca = origenes.descripcion.

    find first lotes_plantacion where lotes_plantacion.id_proveedor =  items_control_tareas.id_proveedor and
         lotes_plantacion.id_origen =  items_control_tareas.id_origen and
         lotes_plantacion.id_lote =  items_control_tareas.id_lote 
     no-lock no-error.
     
    if available lotes_plantacion Then
       rb_items_tarja.nombre_lote = lotes_plantacion.descripcion.
        
    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       rb_items_tarja.id_concepto[1] = v_codigo.
       find first t-personal where
       t-personal.id_empresa = items_control_tareas.id_empresa AND
       t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign 
                    t-personal.id_empresa = items_control_tareas.id_empresa 
                    t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_jornal.
       end.
    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       rb_items_tarja.id_concepto[2] = v_codigo-1.
       find first t-personal where
           t-personal.id_empresa = items_control_tareas.id_empresa AND
           t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign
                 t-personal.id_empresa = items_control_tareas.id_empresa 
                 t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       rb_items_tarja.id_concepto[3] = v_codigo-2.
       find first t-personal where 
           t-personal.id_empresa = items_control_tareas.id_empresa AND
           t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign 
                 t-personal.id_empresa = items_control_tareas.id_empresa 
                 t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_centro.
          end. 
       t-personal.cantidad = t-personal.cantidad + items_control_tareas.cantidad.
       
       find first unidades_liquidacion where unidades_liquidacion.id_unidad_liquidacion = 
       items_control_tareas.id_unidad_liquidacion no-lock no-error.
       if available unidades_liquidacion Then
          rb_items_tarja.nombre_unidad = unidades_liquidacion.abreviatura.
     end. 
    end.
   end.   
 end. 
Else
 do:
  for each items_control_tareas where 
      items_control_tareas.id_empresa = integer(fi-empresa:screen-value) and 
      items_control_tareas.id_sucursal = integer(fi-sucursal:screen-value) and 
      items_control_tareas.fecha >= date(fi-desde-fecha:screen-value) and 
      items_control_tareas.fecha <= date(fi-hasta-fecha:screen-value) and
      items_control_tareas.id_sector = integer(fi-sector:screen-value) AND
      items_control_tareas.id_tipo_planilla <> 4 AND
      (items_control_tareas.cant_jornal <> 0 OR items_control_tareas.cant_horas <> 0 OR items_control_tareas.cantidad <> 0) 
      no-lock, first personal_finca where personal_finca.id_empresa_cosechera =
      items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock,
      first centros_costos_abacus where centros_costos_abacus.id_centro_abacus = personal_finca.id_centro_abacus and
      centros_costos_abacus.id_proveedor = integer(fi-id_proveedor:screen-value) and
      centros_costos_abacus.id_origen = integer(fi-id_origen:screen-value) no-lock
      by items_control_tareas.id_empresa by items_control_tareas.legajo
      by items_control_tareas.nombre by items_control_tareas.fecha :
      
      if items_control_tareas.id_tarea = 0 and items_control_tareas.cantidad = 0 Then next.
      
          v_cargo = personal_finca.id_cargo.
          v_centro = personal_finca.id_centro_abacus.
        
        find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
        if available tareas Then
        do:
        v_codigo = 0.
        v_codigo-1 = 0.
        v_codigo-2 = 0.
        /***Jornal****/
       if items_control_tareas.cant_jornal <> 0  Then
          v_codigo = tareas.id_concepto[14].
       Else
        v_codigo = 9999. 

      /*****Horas********/
      if items_control_tareas.cant_horas <> 0  Then
        if v_cargo <> 42 Then
         do:
          if items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0 Then
             v_codigo-1 = tareas.id_concepto[15].
           Else  
             v_codigo-1 = tareas.id_concepto[2].
         end.
       Else
           v_codigo-1 = tareas.id_concepto[15].
      Else 
         v_codigo-1 = 9999.

     /****Otros******/    
     if items_control_tareas.cantidad <> 0 Then
       do:
       if items_control_tareas.id_unidad_liquidacion <> 0 Then 
         v_codigo-2 = tareas.id_concepto[items_control_tareas.id_unidad_liquidacion].
       end.  
      Else         
        v_codigo-2 = 9999.
    
     create rb_items_tarja.
      assign rb_items_tarja.id_reporte = v_nro_reporte
            rb_items_tarja.id_empresa = items_control_tareas.id_empresa
            rb_items_tarja.id_sucursal = items_control_tareas.id_sucursal
            rb_items_tarja.id_sector = items_control_tareas.id_sector
            rb_items_tarja.legajo = items_control_tareas.legajo
            rb_items_tarja.nombre = items_control_tareas.nombre
            rb_items_tarja.fecha = items_control_tareas.fecha
            rb_items_tarja.id_tarea = items_control_tareas.id_tarea 
            rb_items_tarja.id_proveedor = items_control_tareas.id_proveedor 
            rb_items_tarja.id_origen = items_control_tareas.id_origen 
            rb_items_tarja.id_lote = items_control_tareas.id_lote
            rb_items_tarja.cant_jornal = items_control_tareas.cant_jornal
            rb_items_tarja.cant_horas = items_control_tareas.cant_horas
            rb_items_tarja.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion 
            rb_items_tarja.cantidad = items_control_tareas.cantidad
            rb_items_tarja.nro_tractor = items_control_tareas.nro_tractor
            rb_items_tarja.nro_maquina = items_control_tareas.nro_maquina.
            
    find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.
    if available tareas Then
       do:
         rb_items_tarja.nombre_tarea = tareas.abreviatura.
         rb_items_tarja.nombre_largo_tarea = tareas.descripcion.
       end.  

    find first origenes of items_control_tareas no-lock no-error.
    if available origenes Then
       rb_items_tarja.nombre_finca = origenes.descripcion.

    find first lotes_plantacion where lotes_plantacion.id_proveedor =  items_control_tareas.id_proveedor and
         lotes_plantacion.id_origen =  items_control_tareas.id_origen and
         lotes_plantacion.id_lote =  items_control_tareas.id_lote 
     no-lock no-error.
     
    if available lotes_plantacion Then
       rb_items_tarja.nombre_lote = lotes_plantacion.descripcion.
        
    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       rb_items_tarja.id_concepto[1] = v_codigo.
       find first t-personal where
           t-personal.id_empresa = items_control_tareas.id_empresa AND
           t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign 
                 t-personal.id_empresa = items_control_tareas.id_empresa 
                 t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_jornal.
       end.
    

    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       rb_items_tarja.id_concepto[2] = v_codigo-1.
       find first t-personal where 
           t-personal.id_empresa = items_control_tareas.id_empresa AND
           t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign 
                 t-personal.id_empresa = items_control_tareas.id_empresa 
                 t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       rb_items_tarja.id_concepto[3] = v_codigo-2.
       find first t-personal where 
           t-personal.id_empresa = items_control_tareas.id_empresa AND
           t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign 
                 t-personal.id_empresa = items_control_tareas.id_empresa 
                 t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.dni_cuil = items_control_tareas.dni_cuil
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_centro.
          end. 
       t-personal.cantidad = t-personal.cantidad + items_control_tareas.cantidad.
       find first unidades_liquidacion where unidades_liquidacion.id_unidad_liquidacion = 
       items_control_tareas.id_unidad_liquidacion no-lock no-error.
       if available unidades_liquidacion Then
          rb_items_tarja.nombre_unidad = unidades_liquidacion.abreviatura.
     end. 
    end.
   end.   
 end.  
j = 0.
for each t-personal no-lock break by  t-personal.legajo by t-personal.nombre 
   by t-personal.id_concepto:
   find first rb_tarja where rb_tarja.id_reporte = v_nro_reporte and
    rb_tarja.id_empresa = t-personal.id_empresa AND
    rb_tarja.id_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE) AND
    rb_tarja.id_sector = INTEGER(fi-sector:SCREEN-VALUE) AND
   rb_tarja.legajo = t-personal.legajo and
    rb_tarja.nombre = t-personal.nombre no-error.
   if not available rb_tarja Then
      do:
         create rb_tarja.
         rb_tarja.id_reporte = v_nro_reporte.
         rb_tarja.id_empresa = t-personal.id_empresa. 
         rb_tarja.id_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE).
         rb_tarja.id_sector = INTEGER(fi-sector:SCREEN-VALUE). 
         rb_tarja.legajo = t-personal.legajo.
         find first personal_finca where personal_finca.id_empresa_cosechera =
         integer(fi-empresa:screen-value) and personal_finca.legajo = t-personal.legajo no-lock no-error.
         if available personal_finca Then
            do:
              rb_tarja.nombre = t-personal.nombre.
              rb_tarja.dni_cuil = personal_finca.dni_cuil.
              rb_tarja.id_cargo = personal_finca.id_cargo.
              rb_tarja.id_centro_abacus = personal_finca.id_centro_abacus.
              
              find first centros_costos_abacus where centros_costos_abacus.id_centro_abacus = rb_tarja.id_centro_abacus no-lock
              no-error.    
              if available centros_costos_abacus Then
                 rb_tarja.nombre_centro_abacus = centros_costos_abacus.descripcion.   
            end.  
           Else
             do:
               rb_tarja.nombre = t-personal.nombre.
               rb_tarja.dni_cuil = t-personal.dni_cuil.
             end.
         j = 0.
      end. 
      j = j + 1.
      
      if j <= 20 Then
      do:
      rb_tarja.id_concepto[j] = t-personal.id_concepto.  
      rb_tarja.valor_concepto[j] =  t-personal.cantidad. 
      find first conceptos_abacus where conceptos_abacus.id_concepto = t-personal.id_concepto 
           no-lock no-error.
      if available conceptos_abacus Then     
         rb_tarja.nombre_concepto[j] = conceptos_abacus.descripcion.
      end.
      Else
        message rb_tarja.legajo rb_tarja.nombre view-as alert-box.   
end.

find first rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte and
    rb_tarja.id_empresa = integer(fi-empresa:screen-value) AND
    rb_tarja.id_sucursal = integer(fi-sucursal:screen-value) AND
    rb_tarja.id_sector = integer(fi-sector:screen-value) no-lock no-error.
if available rb_tarja Then
  run imprime-reporte.
Else
  message "No hay datos registrados para esta selección" view-as alert-box
  title "Atención".  


  for each rb_tarja WHERE rb_tarja.id_reporte = v_nro_reporte AND
      rb_tarja.id_empresa = INTEGER(fi-empresa:SCREEN-VALUE) AND
      rb_tarja.id_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE) AND
      rb_tarja.id_sector = INTEGER(fi-sector:SCREEN-VALUE):
    for each rb_items_tarja of rb_tarja:
       delete rb_items_tarja.
    end.
    delete rb_tarja.
  end.      

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
  define var v_filtro as character initial "".
  define var v_nombre_empresa as character.

    v_filtro = "rb_tarja.id_reporte = " + string(v_nro_reporte) + " and " +
        "rb_tarja.id_empresa = " + (fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + " and " +
        "rb_tarja.id_sucursal = " + (fi-sucursal:SCREEN-VALUE) + " and " +
        "rb_tarja.id_sector = " + (fi-sector:SCREEN-VALUE).

  find first proveedores where 
       proveedores.id_proveedor = integer(fi-empresa:screen-value in frame {&frame-name}) no-lock no-error.
  if available proveedores Then
     v_nombre_empresa = proveedores.nombre.  

    RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(v_nro_reporte) + '.TXT'.
    RUN  aderb\_prntrb2(
              (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
       "resumen_tareas_legajo2", /* RB-REPORT-NAME */
       "",                             /* RB-DB-CONNECTION */
       "O",                             /* RB-INCLUDE-RECORDS */
       v_filtro,                              /* RB-FILTER */
       RB-MEMO-FILE,                              /* RB-MEMO-FILE */
       "D",                             /* RB-PRINT-DESTINATION */
       fi-impresora:screen-value in frame {&frame-name} ,       /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        1,                              /* RB-NUMBER-COPIES  - zero */                  
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "Tarja Personal",         /* RB-WINDOW-TITLE */
       yes,                           /* RB-DISPLAY-ERRORS */
       yes,                           /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "v_general = " + fi-desde-fecha:screen-value in frame {&FRAME-NAME} + ";" + fi-hasta-fecha:screen-value in frame {&FRAME-NAME} + ";" +
       v_nombre_empresa + ";"
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
fi-sucursal:load-mouse-pointer("glove") in frame {&FRAME-NAME}.

fi-id_origen:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
fi-desde-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
fi-hasta-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
fi-impresora:screen-value in frame {&frame-name} = session:printer-name.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

