&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dliqitemscontroltareas.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dliqitemscontroltareas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.legajo RowObject.id_tarea ~
RowObject.id_lote RowObject.id_turno RowObject.cant_jornal_norm ~
RowObject.cant_hs_norm RowObject.cant_hs_compensa RowObject.cant_hs_extras ~
RowObject.cantidad RowObject.id_unidad_liquidacion ~
RowObject.cantidad_adicional RowObject.cantidad_adicional-1 ~
RowObject.nro_maquina RowObject.nro_tractor RowObject.ajuste_categoria ~
RowObject.compensa_hs 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.legajo RowObject.dni_cuil ~
RowObject.id_tarea RowObject.id_lote RowObject.id_turno ~
RowObject.cant_jornal_norm RowObject.cant_hs_norm ~
RowObject.cant_hs_compensa RowObject.cant_hs_extras RowObject.cantidad ~
RowObject.id_unidad_liquidacion RowObject.cantidad_adicional ~
RowObject.cantidad_adicional-1 RowObject.nro_maquina RowObject.nro_tractor ~
RowObject.ajuste_categoria RowObject.compensa_hs RowObject.id_proveedor ~
RowObject.id_origen RowObject.id_reserva RowObject.id_empresa ~
RowObject.id_sector RowObject.id_sucursal RowObject.nombre 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS nombre-tarea nombre-cat-tarea FI-cat-tarea ~
FI-categoria nombre-lote nombre-unidad nombre-diferencial 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-cat-tarea AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cat Tarea" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .95 NO-UNDO.

DEFINE VARIABLE FI-categoria AS CHARACTER FORMAT "X(256)":U 
     LABEL "Categoria" 
     VIEW-AS FILL-IN 
     SIZE 52 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-cat-tarea AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-diferencial AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-lote AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-tarea AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-unidad AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.legajo AT ROW 1 COL 18 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     RowObject.dni_cuil AT ROW 2.19 COL 80 COLON-ALIGNED WIDGET-ID 82
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RowObject.id_tarea AT ROW 4.62 COL 13 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     nombre-tarea AT ROW 4.57 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     nombre-cat-tarea AT ROW 5.76 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     FI-cat-tarea AT ROW 5.76 COL 62 COLON-ALIGNED WIDGET-ID 86
     FI-categoria AT ROW 3.38 COL 19 COLON-ALIGNED WIDGET-ID 84
     RowObject.id_lote AT ROW 5.81 COL 13.2 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     nombre-lote AT ROW 5.81 COL 21.2 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     RowObject.id_turno AT ROW 3.38 COL 80 COLON-ALIGNED WIDGET-ID 100
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RowObject.cant_jornal_norm AT ROW 6.95 COL 13 COLON-ALIGNED WIDGET-ID 96
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cant_hs_norm AT ROW 8.14 COL 13 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cant_hs_compensa AT ROW 7.05 COL 42.6 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cant_hs_extras AT ROW 8.19 COL 42.2 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cantidad AT ROW 9.81 COL 13.4 COLON-ALIGNED WIDGET-ID 28
          LABEL "Cantidad"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.id_unidad_liquidacion AT ROW 9.81 COL 49 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     nombre-unidad AT ROW 9.81 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     RowObject.cantidad_adicional AT ROW 14.62 COL 41 COLON-ALIGNED WIDGET-ID 62
          LABEL "Cant.Adic 1"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cantidad_adicional-1 AT ROW 15.81 COL 41 COLON-ALIGNED WIDGET-ID 66
          LABEL "Cant.Adic 2"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.nro_maquina AT ROW 12.19 COL 14 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     RowObject.nro_tractor AT ROW 13.38 COL 14 COLON-ALIGNED WIDGET-ID 26 FORMAT ">>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.ajuste_categoria AT ROW 12.19 COL 40 WIDGET-ID 52
          LABEL "Ajuste Categoria"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     RowObject.compensa_hs AT ROW 12.29 COL 72 WIDGET-ID 48
          LABEL "Compensa Hs"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     RowObject.id_proveedor AT ROW 7.05 COL 76 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.id_origen AT ROW 8.05 COL 76 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.id_reserva AT ROW 13.19 COL 70 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_empresa AT ROW 1 COL 53.4 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_sector AT ROW 15.76 COL 68 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.id_sucursal AT ROW 14.57 COL 78 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.nombre AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 80
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     nombre-diferencial AT ROW 11 COL 57 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     " Datos Adicionales" VIEW-AS TEXT
          SIZE 19 BY 1.19 AT ROW 14.57 COL 2 WIDGET-ID 60
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dliqitemscontroltareas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dliqitemscontroltareas.i}
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 16.05
         WIDTH              = 97.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.ajuste_categoria IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cantidad IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cantidad_adicional IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cantidad_adicional-1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.compensa_hs IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.dni_cuil IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.dni_cuil:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN FI-cat-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-categoria IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_empresa IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_empresa:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_origen IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_origen:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_proveedor IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_proveedor:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_reserva IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_sector IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_sector:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_sucursal IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_sucursal:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-cat-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-diferencial IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-unidad IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.nro_tractor IN FRAME F-Main
   EXP-FORMAT                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RowObject.id_lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_lote vTableWin
ON LEAVE OF RowObject.id_lote IN FRAME F-Main /* Lote */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_lote vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_lote IN FRAME F-Main /* Lote */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "blotesplantacion.w",
                                 INPUT "dlotesplantacion.w",
                                 INPUT "id_lote",
                                 INPUT "lotes_plantacion.id_proveedor = " + rowObject.id_proveedor:SCREEN-VALUE + 
                                       " and lotes_plantacion.id_origen = " + rowObject.id_origen:SCREEN-VALUE +
                                       " and lotes_plantacion.estado = yes", 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       RowObject.id_lote:SCREEN-VALUE = xfieldResult. 
       RUN fieldModified (SELF:HANDLE).
       RUN descriptivos.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_reserva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_reserva vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_reserva IN FRAME F-Main /* Reserva */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
RUN adm2/support/gConsultas.w (INPUT "breservahoras.w",
                               INPUT "dreservashoras.w",
                               INPUT "id_reserva",
                               INPUT "reserva_horas.id_empresa = " + RowObject.id_empresa:SCREEN-VALUE +
                                     " and reserva_horas.id_sector = " + RowObject.id_sector:SCREEN-VALUE +
                                     " and reserva_horas.id_sucursal = " + RowObject.id_sucursal:SCREEN-VALUE +
                                     " and reserva_horas.legajo = " + RowObject.legajo:SCREEN-VALUE +
                                     " and reserva_horas.cant_horas <> reserva_horas.cant_hs_consumidas",
                               OUTPUT xfieldResult).

IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:

       RowObject.id_reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = xfieldResult.     
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_tarea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tarea vTableWin
ON LEAVE OF RowObject.id_tarea IN FRAME F-Main /* Tarea */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tarea vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_tarea IN FRAME F-Main /* Tarea */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
    
    RUN adm2/support/gConsultas.w (INPUT "btareas.w",
                                   INPUT "dtareas.w",
                                   INPUT "id_tarea",
                                   INPUT "tareas.vigente = yes", 
                                   OUTPUT xfieldResult).
    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:
         RowObject.id_tarea:SCREEN-VALUE = xfieldResult. 
         RUN fieldModified (SELF:HANDLE).
         RUN descriptivos.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_unidad_liquidacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_unidad_liquidacion vTableWin
ON LEAVE OF RowObject.id_unidad_liquidacion IN FRAME F-Main /* Unid. Liquidacion */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_unidad_liquidacion vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_unidad_liquidacion IN FRAME F-Main /* Unid. Liquidacion */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "brtareasunidades.w",
                               INPUT "drtareasunidades.w",
                               INPUT "id_unidad_liquidacion",
                               INPUT "r_tareas_unidades.id_tarea = " + rowObject.id_tarea:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                               OUTPUT xfieldResult).
IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
     RowObject.id_unidad_liquidacion:SCREEN-VALUE = xfieldResult. 
     RUN fieldModified (SELF:HANDLE).
     RUN descriptivos.
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.legajo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.legajo vTableWin
ON LEAVE OF RowObject.legajo IN FRAME F-Main /* Legajo */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.legajo vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.legajo IN FRAME F-Main /* Legajo */
DO:
     DEFINE VAR v-legajo AS INTEGER.  

      run wsellegajos.w (INPUT integer(RowObject.id_empresa:SCREEN-VALUE),
                               OUTPUT v-legajo).
       
      RowObject.legajo:SCREEN-VALUE = string(v-legajo). 
      RUN fieldModified (SELF:HANDLE).
      RUN descriptivos.
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
 RUN descriptivos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
 RUN descriptivos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable vTableWin 
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).

  /* Code placed here will execute AFTER standard behavior.    */
  RUN descriptivos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos vTableWin 
PROCEDURE descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = INTEGER(RowObject.id_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
    AND liq_legajos.legajo = integer(RowObject.legajo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_legajos THEN
DO:
   RowObject.nombre:SCREEN-VALUE = liq_legajos.apellido_nombre.
   FIND FIRST liq_categorias OF liq_legajos NO-LOCK NO-ERROR.
   IF AVAILABLE liq_categorias THEN
   DO:
      fi-categoria:SCREEN-VALUE = liq_categorias.descripcion.
   END.
     ELSE
      ASSIGN fi-categoria:SCREEN-VALUE = "".

END.
ELSE
  ASSIGN  RowObject.nombre:SCREEN-VALUE = ""
          fi-categoria:SCREEN-VALUE = "".


FIND FIRST unidades_liquidacion WHERE unidades_liquidacion.id_unidad_liquidacion = INTEGER(RowObject.id_unidad_liquidacion:SCREEN-VALUE) NO-LOCK NO-ERROR.
IF AVAILABLE unidades_liquidacion THEN nombre-unidad:SCREEN-VALUE = unidades_liquidacion.abreviatura.
     ELSE nombre-unidad:SCREEN-VALUE = "".





FIND FIRST agricola.lotes_plantacion WHERE lotes_plantacion.id_proveedor = INTEGER(RowObject.id_proveedor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
        lotes_plantacion.id_origen = INTEGER(RowObject.id_origen:SCREEN-VALUE) and
        lotes_plantacion.id_lote = INTEGER(RowObject.id_lote:SCREEN-VALUE)NO-LOCK NO-ERROR.
IF AVAILABLE lotes_plantacion THEN nombre-lote:SCREEN-VALUE = lotes_plantacion.descripcion.
         ELSE nombre-lote:SCREEN-VALUE = "".

FIND FIRST tareas WHERE tareas.id_tarea = INTEGER(RowObject.id_tarea:SCREEN-VALUE) NO-LOCK NO-ERROR.
IF AVAILABLE tareas THEN 
   DO:
      ASSIGN nombre-tarea:SCREEN-VALUE = tareas.descripcion.
   END.
  ELSE 
  DO:
    ASSIGN nombre-tarea:SCREEN-VALUE = "" .
    RowObject.id_lote:SENSITIVE = NO.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RowObject.id_tarea:LOAD-MOUSE-POINTER("glove") IN FRAME {&FRAME-NAME}.
  RowObject.id_lote:LOAD-MOUSE-POINTER("glove") IN FRAME {&FRAME-NAME}.
  RowObject.id_unidad_liquidacion:LOAD-MOUSE-POINTER("glove") IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
 RUN descriptivos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

