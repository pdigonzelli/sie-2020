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


define temp-table t-personal
       field legajo like items_control_tareas.legajo
       field nombre like items_control_tareas.nombre
       field id_concepto like r_tareas_unidades.id_concepto
       field cantidad like items_control_tareas.cantidad
       field id_centro_abacus like personal_finca.id_centro_abacus.


define temp-table t-resumen
       field id_centro_abacus like personal_finca.id_centro_abacus
       field id_concepto like r_tareas_unidades.id_concepto
       field cantidad like items_control_tareas.cantidad.

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
FI-desde-fecha T-opcion-1 T-opcion-2 FI-hasta-fecha T-opcion-3 B-ok ~
B-cancel 
&Scoped-Define DISPLAYED-OBJECTS FI-sucursal nombre-sucursal Fi-sector ~
nombre-sector fi-empresa nombre-empresa FI-desde-fecha T-opcion-1 ~
T-opcion-2 FI-hasta-fecha T-opcion-3 

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

DEFINE VARIABLE T-opcion-1 AS LOGICAL INITIAL yes 
     LABEL "Detalle de legajos" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE T-opcion-2 AS LOGICAL INITIAL yes 
     LABEL "Resumen x Centro de Costo" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1.19 NO-UNDO.

DEFINE VARIABLE T-opcion-3 AS LOGICAL INITIAL yes 
     LABEL "Resumen Gral" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-sucursal AT ROW 2.43 COL 25 COLON-ALIGNED WIDGET-ID 10
     nombre-sucursal AT ROW 2.43 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     Fi-sector AT ROW 3.62 COL 25 COLON-ALIGNED WIDGET-ID 8
     nombre-sector AT ROW 3.62 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fi-empresa AT ROW 4.81 COL 25 COLON-ALIGNED WIDGET-ID 4
     nombre-empresa AT ROW 4.81 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     FI-desde-fecha AT ROW 6.24 COL 25 COLON-ALIGNED WIDGET-ID 2
     T-opcion-1 AT ROW 6.24 COL 54 WIDGET-ID 20
     T-opcion-2 AT ROW 7.19 COL 54 WIDGET-ID 22
     FI-hasta-fecha AT ROW 7.43 COL 25 COLON-ALIGNED WIDGET-ID 6
     T-opcion-3 AT ROW 8.14 COL 54 WIDGET-ID 24
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
  WHEN 1 or when 5 OR WHEN 7 THEN
    run ejecucion-agricola.
  WHEN 2 THEN
    run ejecucion-cosecha.
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
define var v_cargo like personal_finca.id_cargo.
define var v_centro like personal_finca.id_centro_abacus.
define var v_id_sector as integer.
define var v_sector as character.
define var v_id_sucursal as integer.
define var v_nombre_sucursal as character.
define var v_horas as decimal.


v_id_sucursal = integer(fi-sucursal:screen-value in frame {&FRAME-NAME}).
v_id_empresa = integer(fi-empresa:screen-value in frame {&FRAME-NAME}).
v_id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}).
v_fecha_desde = date(fi-desde-fecha:screen-value in frame {&FRAME-NAME}).
v_fecha_hasta = date(fi-hasta-fecha:screen-value in frame {&FRAME-NAME}).

form
    skip(1)
    "Centro de Costo" t-personal.id_centro_abacus space(5)
    centros_costos_abacus.descripcion 
    skip
    with frame fcentro down no-box no-labels width 100 use-text stream-io.   
   


form 
    t-personal.legajo column-label "Legajo"
    personal_finca.nombre format "x(30)" column-label "Nombre"
    t-personal.id_concepto column-label "Concepto"
    conceptos_abacus.descripcion format "x(25)" column-label ""
    t-personal.cantidad column-label "Cantidad"
    with frame detalle down  width 100 use-text stream-io.   


form
    t-resumen.id_centro_abacus column-label "CCosto"
    centros_costos_abacus.descripcion format "x(20)"
    t-resumen.id_concepto column-label "Concepto"
    conceptos_abacus.descripcion
    t-resumen.cantidad column-label "Cantidad"
    with frame fresumen down  width 100 use-text stream-io.   


form
    t-resumen.id_concepto column-label "Concepto"
    conceptos_abacus.descripcion
    t-resumen.cantidad column-label "Cantidad"
    with frame fresgral down  width 100 use-text stream-io.   
   

for each t-resumen:
  delete t-resumen.
end.  
   
for each t-personal:
  delete t-personal. 
end.


find first proveedores where proveedores.id_proveedor = v_id_empresa no-lock no-error.
if available proveedores Then
   v_empresa = proveedores.nombre.
 Else     
   v_empresa = "".
   
find first sectores_agricolas where sectores_agricolas.id_sector = v_id_sector no-lock no-error.
if available sectores_agricola Then
   v_sector = sectores_agricola.descripcion.
 Else
   v_sector = "".  


for each control_tareas no-lock where 
      control_tareas.id_empresa = v_id_empresa and 
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta and
      control_tareas.id_sucursal = v_id_sucursal and
      control_tareas.id_sector = v_id_sector
      ,each items_control_tareas of control_tareas 
      no-lock 
      by items_control_tareas.id_empresa by items_control_tareas.legajo
      :
      
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
        
    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_jornal.
       end.
    

    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cantidad.

   end.      
  end.
 end.   

/************************************************************/
/* Se excluyen del contado de dias las tareas con sector 0  */


for each control_tareas no-lock where control_tareas.id_empresa = 1 and 
      control_tareas.id_sucursal = v_id_sucursal and
      control_tareas.id_sector = v_id_sector and
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta 
      ,each items_control_tareas of control_tareas where
       items_control_tareas.id_tarea <> 0 no-lock,
       first tareas where tareas.id_tarea = items_control_tareas.id_tarea and 
       (tareas.id_sector <> 0) no-lock 
       break by items_control_tareas.id_empresa by 
       items_control_tareas.legajo by items_control_tareas.fecha:
      
      
      
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
        
        
              /************************************************************/ 
              /* Llevo a horas todas las tareas realizadas ***************/
            
              if items_control_tareas.cant_jornal <> 0 Then
                 v_horas = v_horas + (items_control_tareas.cant_jornal * 8).

              if items_control_tareas.cant_horas <> 0 Then
                 v_horas = v_horas + items_control_tareas.cant_horas.
                 
              if items_control_tareas.cantidad <> 0 Then
                 do:
                    case items_control_tareas.id_unidad_liquidacion:
                        when 1 or when 5 or when 10 or when 11 or when 14 Then
                           v_horas = v_horas + (items_control_tareas.cant_jornal * 8).
                        when 2 or when 3 or when 7 or when 15 or when 17
                               or when 20 or when 21 or when 22 or when 23
                               or when 24 Then
                           v_horas = v_horas + items_control_tareas.cant_horas.
                        otherwise 
                           v_horas = v_horas + 8. 
                    end case.   
                 end.   
               /************************************************************/   


        
        
           if last-of(items_control_tareas.fecha) Then
              do:
                 v_codigo = 941.
                 find first t-personal where t-personal.legajo = items_control_tareas.legajo and
                            t-personal.nombre = items_control_tareas.nombre and
                            t-personal.id_concepto = v_codigo no-lock no-error.
                 if not available t-personal Then
                 do:
                    create t-personal.
                    assign t-personal.legajo = items_control_tareas.legajo
                           t-personal.nombre = items_control_tareas.nombre
                           t-personal.id_concepto = v_codigo 
                           t-personal.id_centro_abacus = v_centro.
                 end. 
                 if v_horas > 8 Then v_horas = 8.
                 t-personal.cantidad = t-personal.cantidad + (v_horas / 8).
                 v_horas = 0.
               end.
end.

      

for each t-personal where
  (t-personal.id_concepto <> 11 and
   t-personal.id_concepto <> 491 and
   t-personal.id_concepto <> 173) no-lock: 
  find first t-resumen where t-resumen.id_centro_abacus = t-personal.id_centro_abacus and
             t-resumen.id_concepto = t-personal.id_concepto no-lock no-error.
  if not available t-resumen Then
    do:
       create t-resumen.
       t-resumen.id_centro_abacus = t-personal.id_centro_abacus.
       t-resumen.id_concepto = t-personal.id_concepto.
    end.
  t-resumen.cantidad = t-resumen.cantidad + t-personal.cantidad.
end.

find first sucursales where sucursales.id_sucursal = v_id_sucursal no-lock no-error.
if available sucursales Then
   v_nombre_sucursal = sucursales.nombre.



RUN p_imprep.p (INPUT TABLE t-personal, 
                INPUT TABLE t-resumen,
                INPUT t-opcion-1:SCREEN-VALUE,
                INPUT t-opcion-2:SCREEN-VALUE,
                INPUT t-opcion-3:SCREEN-VALUE,
                INPUT v_id_empresa).

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
define var v_cargo like personal_finca.id_cargo.
define var v_centro like personal_finca.id_centro_abacus.
define var v_sector as character.
define var v_id_sucursal as integer.
define var v_nombre_sucursal as character format "x(30)".
define var v_horas as decimal.


v_id_sucursal = integer(fi-sucursal:screen-value in frame {&FRAME-NAME}).
v_id_empresa = integer(fi-empresa:screen-value in frame {&FRAME-NAME}).
v_id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}).
v_fecha_desde = date(fi-desde-fecha:screen-value in frame {&FRAME-NAME}).
v_fecha_hasta = date(fi-hasta-fecha:screen-value in frame {&FRAME-NAME}).

form
    skip(1)
    "Centro de Costo" t-personal.id_centro_abacus space(5)
    centros_costos_abacus.descripcion 
    skip
    with frame fcentro down no-box no-labels width 100 use-text stream-io.   
   


form 
    t-personal.legajo column-label "Legajo"
    personal_finca.nombre format "x(30)" column-label "Nombre"
    t-personal.id_concepto column-label "Concepto"
    conceptos_abacus.descripcion format "x(25)" column-label ""
    t-personal.cantidad column-label "Cantidad"
    with frame detalle down  width 100 use-text stream-io.   


form
    t-resumen.id_centro_abacus column-label "CCosto"
    centros_costos_abacus.descripcion format "x(20)"
    t-resumen.id_concepto column-label "Concepto"
    conceptos_abacus.descripcion
    t-resumen.cantidad column-label "Cantidad"
    with frame fresumen down  width 100 use-text stream-io.   


form
    t-resumen.id_concepto column-label "Concepto"
    conceptos_abacus.descripcion
    t-resumen.cantidad column-label "Cantidad"
    with frame fresgral down  width 100 use-text stream-io.   
   

for each t-resumen:
  delete t-resumen.
end.  
   
for each t-personal:
  delete t-personal. 
end.


find first proveedores where proveedores.id_proveedor = v_id_empresa no-lock no-error.
if available proveedores Then
   v_empresa = proveedores.nombre.
 Else     
   v_empresa = "".
   
find first sectores_agricolas where sectores_agricolas.id_sector = v_id_sector no-lock no-error.
if available sectores_agricola Then
   v_sector = sectores_agricola.descripcion.
 Else
   v_sector = "".  

for each control_tareas no-lock where 
      control_tareas.id_empresa = v_id_empresa and 
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta and
      control_tareas.id_sucursal = v_id_sucursal and
      control_tareas.id_sector = v_id_sector
      ,each items_control_tareas of control_tareas 
      no-lock by items_control_tareas.id_empresa by items_control_tareas.legajo
      :
      
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
        
    if v_codigo <> 9999 and v_codigo <> 0 Then
       do:
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and   
       t-personal.id_concepto = v_codigo no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.id_concepto = v_codigo 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_jornal.
       end.
    

    if v_codigo-1 <> 9999 and v_codigo-1 <> 0 Then
     do: 
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and 
       t-personal.id_concepto = v_codigo-1 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.id_concepto = v_codigo-1 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cant_horas.
     end.
     
    if v_codigo-2 <> 9999 and v_codigo-2 <> 0 Then
     do:
       find first t-personal where t-personal.legajo = items_control_tareas.legajo and
       t-personal.nombre = items_control_tareas.nombre and  
       t-personal.id_concepto = v_codigo-2 no-lock no-error.
       if not available t-personal Then
          do:
             create t-personal.
             assign t-personal.legajo = items_control_tareas.legajo
                    t-personal.nombre = items_control_tareas.nombre
                    t-personal.id_concepto = v_codigo-2 
                    t-personal.id_centro_abacus = v_centro.
          end. 
          t-personal.cantidad = t-personal.cantidad + items_control_tareas.cantidad.

   end.      
  end.
 end.   
      



/************************************************************/
/* Se excluyen del contado de dias las tareas con sector 0  */


for each control_tareas no-lock where control_tareas.id_empresa = 1 and 
      control_tareas.id_sucursal = v_id_sucursal and
      control_tareas.id_sector = v_id_sector and
      control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta 
      ,each items_control_tareas of control_tareas where
       items_control_tareas.id_tarea <> 0 no-lock,
       first tareas where tareas.id_tarea = items_control_tareas.id_tarea and 
       (tareas.id_sector <> 0) no-lock 
       break by items_control_tareas.id_empresa by 
       items_control_tareas.legajo by items_control_tareas.fecha:
      
      
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
        
        
                
              /************************************************************/ 
              /* Llevo a horas todas las tareas realizadas ***************/
            
              if items_control_tareas.cant_jornal <> 0 Then
                 v_horas = v_horas + (items_control_tareas.cant_jornal * 8).

              if items_control_tareas.cant_horas <> 0 Then
                 v_horas = v_horas + items_control_tareas.cant_horas.
                 
              if items_control_tareas.cantidad <> 0 Then
                 do:
                    case items_control_tareas.id_unidad_liquidacion:
                        when 1 or when 5 or when 10 or when 11 or when 14 Then
                           v_horas = v_horas + (items_control_tareas.cant_jornal * 8).
                        when 2 or when 3 or when 7 or when 15 or when 17
                               or when 20 or when 21 or when 22 or when 23
                               or when 24 Then
                           v_horas = v_horas + items_control_tareas.cant_horas.
                        otherwise 
                           v_horas = v_horas + 8. 
                    end case.   
                 end.   
               /************************************************************/   



        
        
        
           if last-of(items_control_tareas.fecha) Then
              do:
                  v_codigo = 941.
                  find first t-personal where t-personal.legajo = items_control_tareas.legajo and
                        t-personal.nombre = items_control_tareas.nombre and
                        t-personal.id_concepto = v_codigo no-lock no-error.
                        if not available t-personal Then
                           do:
                              create t-personal.
                              assign t-personal.legajo = items_control_tareas.legajo
                                     t-personal.nombre = items_control_tareas.nombre
                                     t-personal.id_concepto = v_codigo 
                                     t-personal.id_centro_abacus = v_centro.
                           end. 
                           if v_horas > 8 Then v_horas = 8.
                           t-personal.cantidad = t-personal.cantidad + (v_horas / 8).
                           v_horas = 0.
               end.
 
end.



for each t-personal 
  where (t-personal.id_concepto <> 11 and
        t-personal.id_concepto <> 491 and
        t-personal.id_concepto <> 173) no-lock:
  find first t-resumen where t-resumen.id_centro_abacus = t-personal.id_centro_abacus and
             t-resumen.id_concepto = t-personal.id_concepto 
             no-lock no-error.
  if not available t-resumen Then
    do:
       create t-resumen.
       t-resumen.id_centro_abacus = t-personal.id_centro_abacus.
       t-resumen.id_concepto = t-personal.id_concepto.
    end.
  t-resumen.cantidad = t-resumen.cantidad + t-personal.cantidad.
end.


find first sucursales where sucursales.id_sucursal = v_id_sucursal no-lock no-error.
if available sucursales Then
   v_nombre_sucursal = sucursales.nombre.




RUN p_imprepcos.p (INPUT TABLE t-personal,
                INPUT TABLE t-resumen,
                INPUT t-opcion-1:SCREEN-VALUE,
                INPUT t-opcion-2:SCREEN-VALUE,
                INPUT t-opcion-3:SCREEN-VALUE,
                INPUT v_id_empresa). 

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
          nombre-empresa FI-desde-fecha T-opcion-1 T-opcion-2 FI-hasta-fecha 
          T-opcion-3 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-3 FI-sucursal Fi-sector fi-empresa FI-desde-fecha T-opcion-1 
         T-opcion-2 FI-hasta-fecha T-opcion-3 B-ok B-cancel 
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

