&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dliqitemstarjas.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dliqitemstarjas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.legajo RowObject.id_tarea ~
RowObject.id_lote RowObject.cant_horas RowObject.cantidad ~
RowObject.cantidad_adicional RowObject.cantidad_adicional-1 ~
RowObject.nro_maquina RowObject.nro_tractor RowObject.id_turno 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.legajo RowObject.id_tarea ~
RowObject.id_lote RowObject.cant_horas RowObject.cant_hs_norm ~
RowObject.cant_hs_compensa RowObject.cant_hs_extras RowObject.cantidad ~
RowObject.cantidad_adicional RowObject.cantidad_adicional-1 ~
RowObject.nro_maquina RowObject.nro_tractor RowObject.id_proveedor ~
RowObject.id_origen RowObject.id_reserva RowObject.id_empresa ~
RowObject.id_sector RowObject.id_sucursal RowObject.nombre ~
RowObject.dni_cuil RowObject.id_diferencial RowObject.id_turno ~
RowObject.id_codigo_abacus RowObject.id_codigo_abacus_diferencial ~
RowObject.id_codigo_abacus_cantidad RowObject.hs_acond_finca ~
RowObject.hs_plus_tareas_automatico RowObject.hs_plus_tareas_trabajadas ~
RowObject.hs_adicionales_tareas_trabajadas 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-id_ccategoria_liq FI-id_cconvenio_liq ~
nombre-abacus nombre-abacus-diferencial nombre-turno nombre-cat-tarea ~
nombre-diferencial FI-cat-tarea FI-categoria nombre-tarea nombre-lote ~
nombre-abacus-cantidad 

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
     LABEL "Cat" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .95 NO-UNDO.

DEFINE VARIABLE FI-id_ccategoria_liq AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-id_cconvenio_liq AS CHARACTER FORMAT "X(256)":U 
     LABEL "Conv." 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-abacus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-abacus-cantidad AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-abacus-diferencial AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-cat-tarea AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-diferencial AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-lote AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-tarea AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 77.8 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-turno AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13.8 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FI-id_ccategoria_liq AT ROW 3.38 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     FI-id_cconvenio_liq AT ROW 3.38 COL 6 COLON-ALIGNED WIDGET-ID 132
     nombre-abacus AT ROW 8 COL 57 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     nombre-abacus-diferencial AT ROW 10.48 COL 57.4 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     nombre-turno AT ROW 3.38 COL 82.2 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     nombre-cat-tarea AT ROW 5.76 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     nombre-diferencial AT ROW 10.38 COL 24.6 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     FI-cat-tarea AT ROW 5.76 COL 62 COLON-ALIGNED WIDGET-ID 86
     FI-categoria AT ROW 3.38 COL 34 COLON-ALIGNED WIDGET-ID 84
     nombre-tarea AT ROW 4.57 COL 18.2 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     RowObject.legajo AT ROW 1 COL 18 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.id_tarea AT ROW 4.57 COL 5.8 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_lote AT ROW 5.81 COL 13.2 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     nombre-lote AT ROW 5.81 COL 21.2 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     RowObject.cant_horas AT ROW 6.95 COL 17.8 COLON-ALIGNED WIDGET-ID 122
          LABEL "Hs Trabajadas"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.cant_hs_norm AT ROW 8 COL 17.6 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cant_hs_compensa AT ROW 14.57 COL 55.8 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     RowObject.cant_hs_extras AT ROW 14.57 COL 80.6 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.cantidad AT ROW 9.1 COL 17.8 COLON-ALIGNED WIDGET-ID 28
          LABEL "Cantidad"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.cantidad_adicional AT ROW 16.05 COL 31.8 COLON-ALIGNED WIDGET-ID 62
          LABEL "Cant.Adic 1"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cantidad_adicional-1 AT ROW 16 COL 60.6 COLON-ALIGNED WIDGET-ID 66
          LABEL "Cant.Adic 2"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.nro_maquina AT ROW 6.71 COL 48 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     RowObject.nro_tractor AT ROW 6.76 COL 79 COLON-ALIGNED WIDGET-ID 26 FORMAT ">>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_proveedor AT ROW 16 COL 85 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.id_origen AT ROW 16 COL 87 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.id_reserva AT ROW 16 COL 87 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RowObject.id_empresa AT ROW 1 COL 53.4 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_sector AT ROW 16 COL 88 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RowObject.id_sucursal AT ROW 16 COL 87 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.nombre AT ROW 2.19 COL 19 COLON-ALIGNED WIDGET-ID 80
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.dni_cuil AT ROW 2.19 COL 78.6 COLON-ALIGNED WIDGET-ID 82
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RowObject.id_diferencial AT ROW 10.29 COL 17.6 COLON-ALIGNED WIDGET-ID 88
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.id_turno AT ROW 3.38 COL 78.4 COLON-ALIGNED WIDGET-ID 96
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
     RowObject.id_codigo_abacus AT ROW 7.91 COL 48 COLON-ALIGNED WIDGET-ID 102
          LABEL "Cod.RHPRO" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 7.8 BY 1
     RowObject.id_codigo_abacus_diferencial AT ROW 10.38 COL 47.6 COLON-ALIGNED WIDGET-ID 104
          LABEL "Cod.Dif" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     RowObject.id_codigo_abacus_cantidad AT ROW 9.24 COL 48 COLON-ALIGNED WIDGET-ID 114
          VIEW-AS FILL-IN 
          SIZE 7.8 BY 1
     nombre-abacus-cantidad AT ROW 9.33 COL 57 COLON-ALIGNED NO-LABEL WIDGET-ID 118
     RowObject.hs_acond_finca AT ROW 11.95 COL 27.4 COLON-ALIGNED WIDGET-ID 124
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.hs_plus_tareas_automatico AT ROW 12 COL 79.8 COLON-ALIGNED WIDGET-ID 128
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.hs_plus_tareas_trabajadas AT ROW 13.14 COL 27 COLON-ALIGNED WIDGET-ID 130
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.hs_adicionales_tareas_trabajadas AT ROW 13.24 COL 79.8 COLON-ALIGNED WIDGET-ID 126
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     " Datos Adicionales" VIEW-AS TEXT
          SIZE 19 BY 1.19 AT ROW 16 COL 2 WIDGET-ID 60
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dliqitemstarjas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dliqitemstarjas.i}
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
         HEIGHT             = 16.95
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

/* SETTINGS FOR FILL-IN RowObject.cantidad IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cantidad_adicional IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cantidad_adicional-1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cant_horas IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cant_hs_compensa IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.cant_hs_extras IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.cant_hs_norm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.dni_cuil IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.dni_cuil:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN FI-cat-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-categoria IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-id_ccategoria_liq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-id_cconvenio_liq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.hs_acond_finca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.hs_adicionales_tareas_trabajadas IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.hs_plus_tareas_automatico IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.hs_plus_tareas_trabajadas IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_codigo_abacus IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN RowObject.id_codigo_abacus_cantidad IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_codigo_abacus_diferencial IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN RowObject.id_diferencial IN FRAME F-Main
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
ASSIGN 
       RowObject.id_reserva:HIDDEN IN FRAME F-Main           = TRUE.

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
/* SETTINGS FOR FILL-IN nombre-abacus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-abacus-cantidad IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-abacus-diferencial IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-cat-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-diferencial IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-turno IN FRAME F-Main
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
  RUN limpiar-registro.
  RUN limpiar-campos.
  RUN descriptivos.
  RUN habilita-campos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tarea vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_tarea IN FRAME F-Main /* Tarea */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
    
    RUN limpiar-registro.
    RUN limpiar-campos.


    RUN adm2/support/gConsultas.w (INPUT "bliqtareas.w",
                                   INPUT "dliqtareas.w",
                                   INPUT "id_tarea",
                                   INPUT "liq_tareas.vigente = yes", 
                                   OUTPUT xfieldResult).
    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:
         RowObject.id_tarea:SCREEN-VALUE = xfieldResult. 
         RUN fieldModified (SELF:HANDLE).
         RUN descriptivos.
    END.
    RUN habilita-campos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tarea vTableWin
ON RETURN OF RowObject.id_tarea IN FRAME F-Main /* Tarea */
DO:
  RUN limpiar-registro.
  RUN limpiar-campos.
  RUN descriptivos.
  RUN habilita-campos.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_turno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_turno vTableWin
ON LEAVE OF RowObject.id_turno IN FRAME F-Main /* Turno */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_turno vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_turno IN FRAME F-Main /* Turno */
DO:
  DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bturnostarjas.w",
                                 INPUT "dturnostarjas.w",
                                 INPUT "id_turno",
                                 INPUT "", 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       RowObject.id_turno:SCREEN-VALUE = xfieldResult. 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord vTableWin 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  RUN limpiar-registro.
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
DEF BUFFER bconceptos FOR liq_conceptos.
DEF BUFFER bcon01 FOR liq_conceptos.
DEF BUFFER bcon02 FOR liq_conceptos.

DEF VAR v_categoria AS INTEGER.



FIND FIRST liq_tareas WHERE liq_tareas.id_tarea = INTEGER(RowObject.id_tarea:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_tareas THEN nombre-tarea:SCREEN-VALUE = liq_tareas.descripcion.
                        ELSE nombre-tarea:SCREEN-VALUE = "".

FIND FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = INTEGER(RowObject.id_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
    AND liq_legajos.legajo = integer(RowObject.legajo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_legajos THEN
DO:
   RowObject.nombre:SCREEN-VALUE = liq_legajos.apellido_nombre.
   fi-id_cconvenio_liq:SCREEN-VALUE = string(liq_legajos.id_cconvenio_liq).
   fi-id_ccategoria_liq:SCREEN-VALUE = string(liq_legajos.id_ccategoria_liq).
   
   FIND FIRST liq_ccategoriasliq WHERE
       liq_ccategoriasliq.id_categoria = liq_legajos.id_ccategoria_liq NO-LOCK NO-ERROR.
   IF AVAILABLE liq_ccategoriasliq THEN
   DO:
      fi-categoria:SCREEN-VALUE = liq_ccategoriasliq.descripcion.

      v_categoria = 0.
      FIND FIRST liq_tareas WHERE liq_tareas.id_tarea = INTEGER(RowObject.id_tarea:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
      IF AVAILABLE liq_tareas THEN 
      DO:
            IF liq_tareas.id_codigo_abacus <> 0 AND liq_tareas.habilita_cantidad = YES THEN
                 RowObject.id_codigo_abacus_cantidad:SCREEN-VALUE = STRING(liq_tareas.id_codigo_abacus).


          ASSIGN nombre-tarea:SCREEN-VALUE = liq_tareas.descripcion.

          IF liq_tareas.id_categoria <> 10 THEN
              v_categoria = liq_tareas.id_categoria.
            ELSE
            DO:
                IF integer(RowObject.nro_maquina:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 0 and
                   integer(RowObject.nro_tractor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 0 THEN
                    v_categoria = 2.
                 ELSE 
                    v_categoria = 3.
            END.
      
          IF (liq_tareas.id_codigo_abacus = 0 OR (liq_tareas.id_codigo_abacus <> 0 AND liq_tareas.habilita_horas)) and
             liq_tareas.id_tarea  <> 0 and
              v_categoria <> 0 THEN
            DO:
                 FIND FIRST r_cat_pers_tareas_rhpro WHERE
                     r_cat_pers_tareas_rhpro.id_cconvenio_liq = liq_legajos.id_cconvenio_liq AND
                     r_cat_pers_tareas_rhpro.id_ccategoria_liq = liq_legajos.id_ccategoria_liq AND
                     r_cat_pers_tareas_rhpro.id_categoria_tarea = v_categoria NO-LOCK NO-ERROR.
                 IF AVAILABLE r_cat_pers_tareas_rhpro THEN
                 DO:
                    ASSIGN RowObject.id_codigo_abacus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(r_cat_pers_tareas_rhpro.id_codigo_rhpro)
                           RowObject.id_diferencial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(r_cat_pers_tareas_rhpro.id_diferencial)
                           RowObject.id_codigo_abacus_diferencial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(r_cat_pers_tareas_rhpro.id_codigo_rhpro_diferencial).

                    /*IF liq_tareas.cal_hs_adic = YES THEN
                        RowObject.id_codigo_abacus_adicional:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(r_cat_pers_tareas.id_codigo_abacus_adicional).*/
                 END.
                 ELSE
                 DO:
                         MESSAGE "Falta relacion categoria/legajo vs categoria tarea" SKIP
                                 "Cat legajo " liq_legajos.id_cconvenio_liq liq_legajos.id_ccategoria_liq SKIP 
                                 "Avise a administracion x favor" VIEW-AS ALERT-BOX WARNING.
                 END.
             END.
             ELSE
             DO:
                 IF liq_tareas.id_codigo_abacus <> 0 AND liq_tareas.id_tarea <> 0 AND liq_tareas.habilita_horas = NO THEN
                     ASSIGN RowObject.id_codigo_abacus_cantidad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(liq_tareas.id_codigo_abacus).

             END.
       END. /* end liq_tareas */
         ELSE
          ASSIGN  nombre-tarea:SCREEN-VALUE = ""
                  fi-categoria:SCREEN-VALUE = "".
   
   END. /* end liq_ccategoriasliq */
    ELSE
      ASSIGN  fi-categoria:SCREEN-VALUE = "".


  FIND FIRST categorias_tareas OF liq_tareas NO-LOCK NO-ERROR.
     IF AVAILABLE categorias_tareas THEN
         ASSIGN  fi-cat-tarea:SCREEN-VALUE = string(categorias_tareas.id_categoria)
                 nombre-cat-tarea:SCREEN-VALUE = categorias_tareas.descripcion.
      ELSE
         ASSIGN fi-cat-tarea:SCREEN-VALUE = "0"
                nombre-cat-tarea:SCREEN-VALUE = "".

END. /* end liq_legajos */


FIND FIRST turnos_tarjas WHERE turnos_tarjas.id_turno = INTEGER(RowObject.id_turno:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE turnos_tarjas THEN nombre-turno:SCREEN-VALUE = turnos_tarjas.descripcion.
                           ELSE nombre-turno:SCREEN-VALUE = "".


FIND FIRST agricola.lotes_plantacion WHERE lotes_plantacion.id_proveedor = INTEGER(RowObject.id_proveedor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
        lotes_plantacion.id_origen = INTEGER(RowObject.id_origen:SCREEN-VALUE) and
        lotes_plantacion.id_lote = INTEGER(RowObject.id_lote:SCREEN-VALUE)NO-LOCK NO-ERROR.
IF AVAILABLE lotes_plantacion THEN nombre-lote:SCREEN-VALUE = lotes_plantacion.descripcion.
         ELSE nombre-lote:SCREEN-VALUE = "".

FIND FIRST liq_conceptos WHERE liq_conceptos.id_concepto = INTEGER(RowObject.id_codigo_abacus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF AVAILABLE liq_conceptos THEN nombre-abacus:SCREEN-VALUE = liq_conceptos.descripcion.
      ELSE nombre-abacus:SCREEN-VALUE = "".

FIND FIRST bconceptos WHERE bconceptos.id_concepto = INTEGER(RowObject.id_codigo_abacus_diferencial:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE bconceptos THEN nombre-abacus-diferencial:SCREEN-VALUE = bconceptos.descripcion.
   ELSE nombre-abacus-diferencial:SCREEN-VALUE = "".


 FIND FIRST diferenciales WHERE diferenciales.id_diferencial = INTEGER(RowObject.id_diferencial:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAILABLE diferenciales THEN 
             nombre-diferencial:SCREEN-VALUE = diferenciales.descripcion.
             ELSE 
             nombre-diferencial:SCREEN-VALUE = "".

 FIND FIRST bcon01 WHERE bcon01.id_concepto = INTEGER(RowObject.id_codigo_abacus_cantidad:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
 IF AVAILABLE bcon01 THEN nombre-abacus-cantidad:SCREEN-VALUE = bcon01.descripcion.
                ELSE nombre-abacus-cantidad:SCREEN-VALUE = "".

 /*FIND FIRST bcon02 WHERE bcon02.id_concepto = INTEGER(RowObject.id_codigo_abacus_adicional:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
 IF AVAILABLE bcon02 THEN nombre-abacus-adicional:SCREEN-VALUE = bcon02.descripcion.
                     ELSE nombre-abacus-adicional:SCREEN-VALUE = "".
 */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
RUN habilita-campos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-campos vTableWin 
PROCEDURE habilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST liq_tareas WHERE liq_tareas.id_tarea = INTEGER(RowObject.id_tarea:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
 IF AVAILABLE liq_tareas THEN 
    DO:
       IF liq_tareas.id_tipo_dato = 1 THEN RowObject.id_lote:SENSITIVE = NO.
                                      ELSE RowObject.id_lote:SENSITIVE = YES.

       IF liq_tareas.habilita_turno THEN RowObject.id_turno:SENSITIVE = YES.
                                    ELSE RowObject.id_turno:SENSITIVE = NO.

       IF liq_tareas.habilita_horas THEN RowObject.cant_horas:SENSITIVE = YES.
                                 ELSE RowObject.cant_horas:SENSITIVE = NO.


       IF liq_tareas.habilita_cantidad THEN RowObject.cantidad:SENSITIVE = YES.
                                       ELSE RowObject.cantidad:SENSITIVE = NO.


       IF liq_tareas.tiene_maquina THEN 
                                     RowObject.nro_maquina:SENSITIVE = YES.
                                   ELSE RowObject.nro_maquina:SENSITIVE = NO.

       IF liq_tareas.tiene_tractor THEN RowObject.nro_tractor:SENSITIVE = YES.
                                   ELSE RowObject.nro_tractor:SENSITIVE = NO.


       IF liq_tareas.cant_adicional-1 THEN RowObject.cantidad_adicional:SENSITIVE = YES.
                                      ELSE RowObject.cantidad_adicional:SENSITIVE = NO.

       IF liq_tareas.cant_adic-2 THEN RowObject.cantidad_adicional-1:SENSITIVE = YES.
                                      ELSE RowObject.cantidad_adicional-1:SENSITIVE = NO.

 END.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE limpiar-campos vTableWin 
PROCEDURE limpiar-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN RowObject.nro_tractor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
         RowObject.nro_maquina:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
         RowObject.id_turno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE limpiar-registro vTableWin 
PROCEDURE limpiar-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN RowObject.cant_horas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.cant_hs_norm:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.id_codigo_abacus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
         RowObject.cantidad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.id_codigo_abacus_cantidad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
         RowObject.hs_acond_finca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.hs_plus_tareas_automatico:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.hs_plus_tareas_trabajadas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.hs_adicionales_tareas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.cant_hs_compensa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.cant_hs_extra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.cantidad_adicional:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00"
         RowObject.cantidad_adicional-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

