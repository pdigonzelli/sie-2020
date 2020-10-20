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

    define var archivo-spool as character no-undo.


    define temp-table t-control
       field id_sucursal like control_tareas.id_sucursal 
       field legajo like items_control_tareas.legajo
       field nombre like items_control_tareas.nombre
       field id_tarea like items_control_tareas.id_tarea
       field id_unidad_liquidacion like items_control_tarea.id_unidad_liquidacion
       field cant_jornal like items_control_tareas.cantidad
       field cant_hs like items_control_tareas.cantidad
       field cant_otros like items_control_tareas.cantidad
       field nro_planilla like control_tareas.nro_planilla.

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
&Scoped-Define ENABLED-OBJECTS RECT-3 FI-sucursal Fi-sector fi-empresa ~
FI-desde-fecha FI-hasta-fecha B-ok B-cancel 
&Scoped-Define DISPLAYED-OBJECTS FI-sucursal nombre-sucursal Fi-sector ~
nombre-sector fi-empresa nombre-empresa FI-desde-fecha FI-hasta-fecha 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.43.

DEFINE BUTTON B-ok 
     LABEL "OK" 
     SIZE 14 BY 1.43.

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

DEFINE VARIABLE FI-sucursal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sucursal AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-sucursal AT ROW 2.43 COL 25 COLON-ALIGNED WIDGET-ID 10
     nombre-sucursal AT ROW 2.43 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     Fi-sector AT ROW 3.62 COL 25 COLON-ALIGNED WIDGET-ID 8
     nombre-sector AT ROW 3.62 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fi-empresa AT ROW 4.81 COL 25 COLON-ALIGNED WIDGET-ID 4
     nombre-empresa AT ROW 4.81 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     FI-desde-fecha AT ROW 6.24 COL 25 COLON-ALIGNED WIDGET-ID 2
     FI-hasta-fecha AT ROW 7.43 COL 25 COLON-ALIGNED WIDGET-ID 6
     B-ok AT ROW 11 COL 49 WIDGET-ID 28
     B-cancel AT ROW 11 COL 65 WIDGET-ID 30
     "  Filtros" VIEW-AS TEXT
          SIZE 79 BY .71 AT ROW 1.48 COL 6 WIDGET-ID 26
          BGCOLOR 1 FGCOLOR 15 
     RECT-3 AT ROW 1.24 COL 3 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.4 BY 12.1 WIDGET-ID 100.


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
         TITLE              = "Reportes"
         HEIGHT             = 12.67
         WIDTH              = 91.4
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
/* SETTINGS FOR FILL-IN nombre-sucursal IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Reportes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Reportes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-cancel wWin
ON CHOOSE OF B-cancel IN FRAME fMain /* Cancel */
DO:
  APPLY "window-close" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ok wWin
ON CHOOSE OF B-ok IN FRAME fMain /* OK */
DO:
    case integer(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
     WHEN 1 OR WHEN 7 THEN
       run ejecucion-agricola.
     WHEN 2 THEN
       run ejecucion-cosecha.
     WHEN 5 THEN
       run ejecucion-agricola.
    end case.    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-empresa
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
ON GO OF FI-sucursal IN FRAME fMain /* Sucursal */
DO:
    run descriptivos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sucursal wWin
ON LEAVE OF FI-sucursal IN FRAME fMain /* Sucursal */
DO:
    run descriptivos.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sucursal wWin
ON U1 OF FI-sucursal IN FRAME fMain /* Sucursal */
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
find first sectores_agricolas where sectores_agricolas.id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME} )  no-lock no-error .
if available sectores_agricolas then 
nombre-sector:screen-value   = string(sectores_agricolas.descripcion).
else
nombre-sector:screen-value  = ''.

find first sucursales where sucursales.id_sucursal = integer(fi-sucursal:screen-value in frame {&FRAME-NAME} )  no-lock no-error .
if available sucursales then 
nombre-sucursal:screen-value   = string(sucursales.nombre).
else
nombre-sucursal:screen-value  = ''.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ejecucion-agricola wWin 
PROCEDURE ejecucion-agricola :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var v_fecha_desde as date.
define var v_fecha_hasta as date.
define var v_codigo as integer.
define var v_codigo-1 as integer.
define var v_codigo-2 as integer.
define var v_empresa as character format "x(30)".
define var v_id_empresa as integer.
define var v_id_sector as integer.
define var v_id_sucursal as integer.

define var v_cargo like personal_finca.id_cargo.
define var v_centro like personal_finca.id_centro_abacus.
define var v_tractor as integer.

v_id_empresa = integer(fi-empresa:screen-value in frame {&FRAME-NAME}).
v_id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}).
v_id_sucursal = integer(fi-sucursal:screen-value in frame {&FRAME-NAME}).
v_fecha_desde = date(fi-desde-fecha:screen-value in frame {&FRAME-NAME}).
v_fecha_hasta = date(fi-hasta-fecha:screen-value in frame {&FRAME-NAME}).




for each t-control:
  delete t-control. 
end.

find first proveedores where proveedores.id_proveedor = v_id_empresa no-lock no-error.
if available proveedores Then
   v_empresa = proveedores.nombre.
 Else     
   v_empresa = "".
   


for each control_tareas no-lock where
      control_tareas.id_empresa = v_id_empresa and
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta and
      control_tareas.id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}) and
      (if v_id_sucursal <> 0 Then control_tareas.id_sucursal = v_id_sucursal Else true)
      ,each items_control_tareas of control_tareas no-lock by items_control_tareas.id_sucursal by items_control_tareas.legajo :
      
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

    {calcula-agricola.i}          
        
    if v_codigo <> 9999 and v_codigo = 0 and items_control_tareas.id_tarea <> 0 Then
       do:
       find first t-control where  t-control.legajo = items_control_tareas.legajo and
       t-control.nombre = items_control_tareas.nombre and
       t-control.id_tarea = items_control_tareas.id_tarea and
       t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion and
       t-control.id_sucursal = control_tareas.id_sucursal and
       t-control.nro_planilla = control_tareas.nro_planilla no-lock no-error.
       if not available t-control Then
          do:
             create t-control.
             assign t-control.legajo = items_control_tareas.legajo
                    t-control.nombre = items_control_tareas.nombre
                    t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion
                    t-control.id_tarea = items_control_tareas.id_tarea
                    t-control.id_sucursal = control_tareas.id_sucursal
                    t-control.nro_planilla = control_tareas.nro_planilla.
          end. 
          t-control.cant_jornal = t-control.cant_jornal + items_control_tareas.cant_jornal_norm.
       end.
    

    if v_codigo-1 <> 9999 and v_codigo-1 = 0 and items_control_tareas.id_tarea <> 0 Then
     do: 
       find first t-control where  t-control.legajo = items_control_tareas.legajo and
       t-control.nombre = items_control_tareas.nombre and
       t-control.id_tarea = items_control_tareas.id_tarea and
       t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion and
       t-control.id_sucursal = control_tareas.id_sucursal and
       t-control.nro_planilla = control_tareas.nro_planilla  
       no-lock no-error.
       if not available t-control Then
          do:
             create t-control.
             assign t-control.legajo = items_control_tareas.legajo
                    t-control.nombre = items_control_tareas.nombre
                    t-control.id_tarea = items_control_tareas.id_tarea
                    t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion
                    t-control.id_sucursal = control_tareas.id_sucursal
                    t-control.nro_planilla = control_tareas.nro_planilla.
          end. 
          t-control.cant_hs = t-control.cant_hs + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 = 0 and items_control_tareas.id_tarea <> 0 Then
     do:
       find first t-control where 
       t-control.legajo = items_control_tareas.legajo and
       t-control.nombre = items_control_tareas.nombre and
       t-control.id_tarea = items_control_tareas.id_tarea and
       t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion and
       t-control.id_sucursal = control_tareas.id_sucursal and
       t-control.nro_planilla = control_tareas.nro_planilla no-lock no-error.
       if not available t-control Then
          do:
             create t-control.
             assign t-control.legajo = items_control_tareas.legajo
                    t-control.nombre = items_control_tareas.nombre
                    t-control.id_tarea = items_control_tareas.id_tarea
                    t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion
                    t-control.id_sucursal = control_tareas.id_sucursal
                    t-control.nro_planilla = control_tareas.nro_planilla.
          end. 
          t-control.cant_otros = t-control.cant_otros + items_control_tareas.cantidad.
   end.      
  end.
 end.   

RUN p_impctrl.p (INPUT TABLE t-control).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ejecucion-cosecha wWin 
PROCEDURE ejecucion-cosecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var v_fecha_desde as date.
define var v_fecha_hasta as date.
define var v_codigo as integer.
define var v_codigo-1 as integer.
define var v_codigo-2 as integer.
define var v_empresa as character format "x(30)".
define var v_id_empresa as integer.
define var v_id_sector as integer.
define var v_id_sucursal as integer.
define var v_cargo like personal_finca.id_cargo.
define var v_centro like personal_finca.id_centro_abacus.
define var v_tractor as integer.

v_id_empresa = integer(fi-empresa:screen-value in frame {&FRAME-NAME}).
v_id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}).
v_id_sucursal = integer(fi-sucursal:screen-value in frame {&FRAME-NAME}).
v_fecha_desde = date(fi-desde-fecha:screen-value in frame {&FRAME-NAME}).
v_fecha_hasta = date(fi-hasta-fecha:screen-value in frame {&FRAME-NAME}).




for each t-control:
  delete t-control. 
end.

find first proveedores where proveedores.id_proveedor = v_id_empresa no-lock no-error.
if available proveedores Then
   v_empresa = proveedores.nombre.
 Else     
   v_empresa = "".
   


for each control_tareas no-lock where
      control_tareas.id_empresa = v_id_empresa and
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta and
      control_tareas.id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}) and
      (if v_id_sucursal <> 0 Then control_tareas.id_sucursal = v_id_sucursal Else true)
      ,each items_control_tareas of control_tareas no-lock by items_control_tareas.id_sucursal by items_control_tareas.legajo:
      
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
        
    if v_codigo <> 9999 and v_codigo = 0 and items_control_tareas.id_tarea <> 0 Then
       do:
       find first t-control where  t-control.legajo = items_control_tareas.legajo and
       t-control.nombre = items_control_tareas.nombre and
       t-control.id_tarea = items_control_tareas.id_tarea and
       t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion and
       t-control.id_sucursal = control_tareas.id_sucursal and
       t-control.nro_planilla = control_tareas.nro_planilla no-lock no-error.
       if not available t-control Then
          do:
             create t-control.
             assign t-control.legajo = items_control_tareas.legajo
                    t-control.nombre = items_control_tareas.nombre
                    t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion
                    t-control.id_tarea = items_control_tareas.id_tarea
                    t-control.id_sucursal = control_tareas.id_sucursal
                    t-control.nro_planilla = control_tareas.nro_planilla.
          end. 
          t-control.cant_jornal = t-control.cant_jornal + items_control_tareas.cant_jornal.
       end.
    

    if v_codigo-1 <> 9999 and v_codigo-1 = 0 and items_control_tareas.id_tarea <> 0 Then
     do: 
       find first t-control where  t-control.legajo = items_control_tareas.legajo and
       t-control.nombre = items_control_tareas.nombre and
       t-control.id_tarea = items_control_tareas.id_tarea and
       t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion and
       t-control.id_sucursal = control_tareas.id_sucursal and
       t-control.nro_planilla = control_tareas.nro_planilla  
       no-lock no-error.
       if not available t-control Then
          do:
             create t-control.
             assign t-control.legajo = items_control_tareas.legajo
                    t-control.nombre = items_control_tareas.nombre
                    t-control.id_tarea = items_control_tareas.id_tarea
                    t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion
                    t-control.id_sucursal = control_tareas.id_sucursal
                    t-control.nro_planilla = control_tareas.nro_planilla.
          end. 
          t-control.cant_hs = t-control.cant_hs + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 = 0 and items_control_tareas.id_tarea <> 0 Then
     do:
       find first t-control where 
       t-control.legajo = items_control_tareas.legajo and
       t-control.nombre = items_control_tareas.nombre and
       t-control.id_tarea = items_control_tareas.id_tarea and
       t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion and
       t-control.id_sucursal = control_tareas.id_sucursal and
       t-control.nro_planilla = control_tareas.nro_planilla no-lock no-error.
       if not available t-control Then
          do:
             create t-control.
             assign t-control.legajo = items_control_tareas.legajo
                    t-control.nombre = items_control_tareas.nombre
                    t-control.id_tarea = items_control_tareas.id_tarea
                    t-control.id_unidad_liquidacion = items_control_tareas.id_unidad_liquidacion
                    t-control.id_sucursal = control_tareas.id_sucursal
                    t-control.nro_planilla = control_tareas.nro_planilla.
          end. 
          t-control.cant_otros = t-control.cant_otros + items_control_tareas.cantidad.
   end.      
  end.
 end.   
      
 RUN p_impctrl.p (INPUT TABLE t-control).


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
  DISPLAY FI-sucursal nombre-sucursal Fi-sector nombre-sector fi-empresa 
          nombre-empresa FI-desde-fecha FI-hasta-fecha 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-3 FI-sucursal Fi-sector fi-empresa FI-desde-fecha FI-hasta-fecha 
         B-ok B-cancel 
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

