&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dliqtareas.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dliqtareas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.id_tarea RowObject.descripcion ~
RowObject.abreviatura RowObject.id_sector RowObject.id_grupo_tarea ~
RowObject.id_grupo_consulta RowObject.id_tipo_dato ~
RowObject.id_categoria_tarea RowObject.vigente RowObject.habilita_turno ~
RowObject.varios_lotes RowObject.liquida_horas RowObject.habilita_horas ~
RowObject.id_codigo_abacus RowObject.habilita_cantidad ~
RowObject.genera_hs_auto RowObject.tiene_tractor RowObject.pulverizacion ~
RowObject.tiene_maquina RowObject.id_tipo_automatico ~
RowObject.cant_adicional-1 RowObject.id_tipo_cant-1 ~
RowObject.id_tipo_excedente RowObject.cant_adic-2 RowObject.id_tipo_cant-2 ~
RowObject.cant_ha RowObject.de_ha RowObject.no_compensa ~
RowObject.codigo_sap 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.id_tarea RowObject.descripcion ~
RowObject.abreviatura RowObject.id_sector RowObject.id_grupo_tarea ~
RowObject.id_grupo_consulta RowObject.id_tipo_dato ~
RowObject.id_categoria_tarea RowObject.vigente RowObject.habilita_turno ~
RowObject.varios_lotes RowObject.liquida_horas RowObject.habilita_horas ~
RowObject.id_codigo_abacus RowObject.habilita_cantidad ~
RowObject.genera_hs_auto RowObject.tiene_tractor RowObject.pulverizacion ~
RowObject.tiene_maquina RowObject.id_tipo_automatico ~
RowObject.cant_adicional-1 RowObject.id_tipo_cant-1 ~
RowObject.id_tipo_excedente RowObject.cant_adic-2 RowObject.id_tipo_cant-2 ~
RowObject.cant_ha RowObject.de_ha RowObject.no_compensa ~
RowObject.codigo_sap 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS nombre-sector nombre-grupo-tarea ~
nombre-tipo-dato nombre-grupo-consulta nombre-categoria-tarea nombre-abacus ~
nombre-automatico nombre-cant1 nombre-excedente nombre-cant2 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE nombre-abacus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-automatico AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.8 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-cant1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-cant2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-categoria-tarea AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-excedente AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-grupo-consulta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-grupo-tarea AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-tipo-dato AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_tarea AT ROW 1 COL 17 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.descripcion AT ROW 2.14 COL 17 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 94 BY 1
     RowObject.abreviatura AT ROW 3.14 COL 17 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     RowObject.id_sector AT ROW 3.14 COL 52 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     nombre-sector AT ROW 3.14 COL 59 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     RowObject.id_grupo_tarea AT ROW 4.33 COL 17 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     nombre-grupo-tarea AT ROW 4.33 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     nombre-tipo-dato AT ROW 5.29 COL 97 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     RowObject.id_grupo_consulta AT ROW 5.52 COL 17 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     nombre-grupo-consulta AT ROW 5.52 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     RowObject.id_tipo_dato AT ROW 5.52 COL 89 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.id_categoria_tarea AT ROW 6.71 COL 17 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     nombre-categoria-tarea AT ROW 6.71 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     RowObject.vigente AT ROW 6.86 COL 70 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     RowObject.habilita_turno AT ROW 6.95 COL 78 WIDGET-ID 52
          LABEL "Habilita Turno?"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     RowObject.varios_lotes AT ROW 6.95 COL 102 WIDGET-ID 80
          LABEL "Vs Lotes"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.liquida_horas AT ROW 7.91 COL 78 WIDGET-ID 28
          LABEL "Liquida Hs"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .81
     RowObject.habilita_horas AT ROW 8.14 COL 4 WIDGET-ID 62
          LABEL "Habilita Hs"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     RowObject.id_codigo_abacus AT ROW 8.86 COL 45 COLON-ALIGNED WIDGET-ID 54
          LABEL "Cod.RHPRO" FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     nombre-abacus AT ROW 8.86 COL 57 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     RowObject.habilita_cantidad AT ROW 9.33 COL 4 WIDGET-ID 40
          LABEL "Habilita Cantidad"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     RowObject.genera_hs_auto AT ROW 10.29 COL 68 WIDGET-ID 66
          LABEL "Genera Hs Autom ticas?"
          VIEW-AS TOGGLE-BOX
          SIZE 28 BY .81
     RowObject.tiene_tractor AT ROW 10.43 COL 27 WIDGET-ID 26
          LABEL "Tractor"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.pulverizacion AT ROW 10.43 COL 42 WIDGET-ID 82
          LABEL "Pulverizaci¢n"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     RowObject.tiene_maquina AT ROW 10.52 COL 4 WIDGET-ID 24
          LABEL "Maquina"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.id_tipo_automatico AT ROW 11.48 COL 82 COLON-ALIGNED WIDGET-ID 74
          LABEL "Tipo Automatico"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     nombre-automatico AT ROW 11.48 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     RowObject.cant_adicional-1 AT ROW 11.95 COL 4 WIDGET-ID 30
          LABEL "Cant. Adic 1"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .81
     RowObject.id_tipo_cant-1 AT ROW 11.95 COL 33 COLON-ALIGNED WIDGET-ID 84
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     nombre-cant1 AT ROW 11.95 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     RowObject.id_tipo_excedente AT ROW 12.67 COL 81.8 COLON-ALIGNED WIDGET-ID 68
          LABEL "Tipo Exedente"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     nombre-excedente AT ROW 12.67 COL 88.6 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     RowObject.cant_adic-2 AT ROW 13.14 COL 4 WIDGET-ID 32
          LABEL "Cant. Adic 2"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81
     RowObject.id_tipo_cant-2 AT ROW 13.14 COL 33 COLON-ALIGNED WIDGET-ID 86
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     nombre-cant2 AT ROW 13.19 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     RowObject.cant_ha AT ROW 13.86 COL 93 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RowObject.de_ha AT ROW 13.86 COL 106.6 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     RowObject.no_compensa AT ROW 14.57 COL 4 WIDGET-ID 78
          LABEL "No Compensa"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     RowObject.codigo_sap AT ROW 15.52 COL 93 COLON-ALIGNED WIDGET-ID 92
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     "   REGLA" VIEW-AS TEXT
          SIZE 14 BY .95 AT ROW 13.86 COL 72 WIDGET-ID 46
          BGCOLOR 3 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dliqtareas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dliqtareas.i}
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
         HEIGHT             = 15.91
         WIDTH              = 122.8.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.cant_adic-2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.cant_adicional-1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.genera_hs_auto IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.habilita_cantidad IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.habilita_horas IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.habilita_turno IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_codigo_abacus IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.id_tipo_automatico IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_tipo_excedente IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.liquida_horas IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nombre-abacus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-automatico IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-cant1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-cant2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-categoria-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-excedente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-grupo-consulta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-grupo-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sector IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-tipo-dato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.no_compensa IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.pulverizacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.tiene_maquina IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.tiene_tractor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.varios_lotes IN FRAME F-Main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME RowObject.id_categoria_tarea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_categoria_tarea vTableWin
ON LEAVE OF RowObject.id_categoria_tarea IN FRAME F-Main /* Categoria Tarea */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_categoria_tarea vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_categoria_tarea IN FRAME F-Main /* Categoria Tarea */
DO:

 DEFINE VAR hfield AS HANDLE NO-UNDO.
 DEFINE VAR xFieldResult AS CHARACTER.

 RUN adm2/support/gConsultas.w (INPUT "bcategoriastareas.w",
                               INPUT "dcategoriastareas.w",
                               INPUT "id_categoria_tarea",
                               INPUT "", 
                               OUTPUT xfieldResult).

 IF xFieldResult <> "" AND xFieldResult <> ? THEN
 DO:
     RowObject.id_categoria_tarea:SCREEN-VALUE = xfieldResult. 
     RUN fieldModified (SELF:HANDLE).
     RUN descriptivos.
 END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_codigo_abacus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_codigo_abacus vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_codigo_abacus IN FRAME F-Main /* Cod.RHPRO */
DO:
DEFINE VAR hfield AS HANDLE NO-UNDO.
DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "bliqconceptos.w",
                            INPUT "dliqconceptos.w",
                            INPUT "id_concepto",
                            INPUT "by descripcion", 
                            OUTPUT xfieldResult).

IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
  RowObject.id_codigo_abacus:SCREEN-VALUE = xfieldResult. 
  RUN fieldModified (SELF:HANDLE).
  RUN descriptivos.
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_grupo_consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_grupo_consulta vTableWin
ON LEAVE OF RowObject.id_grupo_consulta IN FRAME F-Main /* Grupo Consulta */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_grupo_consulta vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_grupo_consulta IN FRAME F-Main /* Grupo Consulta */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "bliqgruposconsultas.w",
                               INPUT "dliqgruposconsultas.w",
                               INPUT "id_grupo_consulta",
                               INPUT "", 
                               OUTPUT xfieldResult).
IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
     RowObject.id_grupo_consulta:SCREEN-VALUE = xfieldResult. 
     RUN fieldModified (SELF:HANDLE).
     RUN descriptivos.
END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_grupo_tarea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_grupo_tarea vTableWin
ON LEAVE OF RowObject.id_grupo_tarea IN FRAME F-Main /* Grupo */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_grupo_tarea vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_grupo_tarea IN FRAME F-Main /* Grupo */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bliqgrupostareas.w",
                                 INPUT "dliqgrupostareas.w",
                                 INPUT "id_grupo_tarea",
                                 INPUT "", 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       RowObject.id_grupo_tarea:SCREEN-VALUE = xfieldResult. 
       RUN fieldModified (SELF:HANDLE).
       RUN descriptivos.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_sector
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_sector vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_sector IN FRAME F-Main /* Cod.Sector */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bliqsectores.w",
                                 INPUT "dliqsectores.w",
                                 INPUT "id_sector_liq",
                                 INPUT "liq_sectores.id_empresa = 101", 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       RowObject.id_sector:SCREEN-VALUE = xfieldResult. 
       RUN descriptivos.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_tipo_automatico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tipo_automatico vTableWin
ON LEAVE OF RowObject.id_tipo_automatico IN FRAME F-Main /* Tipo Automatico */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tipo_automatico vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_tipo_automatico IN FRAME F-Main /* Tipo Automatico */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "btipoautomatico.w",
                            INPUT "dtipoautomatico.w",
                            INPUT "id_tipo_automatico",
                            INPUT "", 
                            OUTPUT xfieldResult).

IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
  RowObject.id_tipo_automatico:SCREEN-VALUE = xfieldResult. 
  RUN fieldModified (SELF:HANDLE).
  RUN descriptivos.
END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_tipo_cant-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tipo_cant-1 vTableWin
ON LEAVE OF RowObject.id_tipo_cant-1 IN FRAME F-Main /* Tipo Cant 1 */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tipo_cant-1 vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_tipo_cant-1 IN FRAME F-Main /* Tipo Cant 1 */
DO:
DEFINE VAR hfield AS HANDLE NO-UNDO.
DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "btipocantadic.w",
                          INPUT "dtipocantadic.w",
                          INPUT "id_tipo_cantidad",
                          INPUT "", 
                          OUTPUT xfieldResult).

IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
RowObject.id_tipo_cant-1:SCREEN-VALUE = xfieldResult. 
RUN fieldModified (SELF:HANDLE).
RUN descriptivos.
END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_tipo_cant-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tipo_cant-2 vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_tipo_cant-2 IN FRAME F-Main /* Tipo Cant 2 */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "btipocantadic.w",
                            INPUT "dtipocantadic.w",
                            INPUT "id_tipo_cantidad",
                            INPUT "", 
                            OUTPUT xfieldResult).

  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
  RowObject.id_tipo_cant-2:SCREEN-VALUE = xfieldResult. 
  RUN fieldModified (SELF:HANDLE).
  RUN descriptivos.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_tipo_dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tipo_dato vTableWin
ON LEAVE OF RowObject.id_tipo_dato IN FRAME F-Main /* Tipo */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tipo_dato vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_tipo_dato IN FRAME F-Main /* Tipo */
DO:
  DEFINE VAR hfield AS HANDLE NO-UNDO.
 DEFINE VAR xFieldResult AS CHARACTER.

 RUN adm2/support/gConsultas.w (INPUT "btipodato.w",
                               INPUT "dtipodato.w",
                               INPUT "id_tipo_dato",
                               INPUT "", 
                               OUTPUT xfieldResult).

 IF xFieldResult <> "" AND xFieldResult <> ? THEN
 DO:
     RowObject.id_tipo_dato:SCREEN-VALUE = xfieldResult. 
     RUN fieldModified (SELF:HANDLE).
     RUN descriptivos.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_tipo_excedente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tipo_excedente vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_tipo_excedente IN FRAME F-Main /* Tipo Exedente */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "btipoexcedente.w",
                              INPUT "dtipoexcedente.w",
                              INPUT "id_tipo_excedente",
                              INPUT "", 
                              OUTPUT xfieldResult).

  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
    RowObject.id_tipo_excedente:SCREEN-VALUE = xfieldResult. 
    RUN fieldModified (SELF:HANDLE).
    RUN descriptivos.
  END.

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

FIND FIRST liq_grupos_tareas WHERE liq_grupos_tareas.id_grupo_tarea = INTEGER(RowObject.id_grupo_tarea:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_grupos_tareas THEN nombre-grupo-tarea:SCREEN-VALUE = liq_grupos_tareas.descripcion.
                    ELSE nombre-grupo-tarea:SCREEN-VALUE = "".

FIND FIRST liq_grupos_consultas WHERE liq_grupos_consultas.id_grupo_consulta = INTEGER(RowObject.id_grupo_consulta:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_grupos_consultas THEN nombre-grupo-consulta:SCREEN-VALUE = liq_grupos_consultas.descripcion.
        ELSE nombre-grupo-consulta:SCREEN-VALUE = "".

FIND FIRST tipo_dato WHERE tipo_dato.id_tipo_dato = INTEGER(RowObject.id_tipo_dato:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAILABLE tipo_dato THEN nombre-tipo-dato:SCREEN-VALUE = tipo_dato.descripcion.
                         ELSE nombre-tipo-dato:SCREEN-VALUE = "".

FIND FIRST categorias_tareas WHERE categorias_tareas.id_categoria_tarea = INTEGER(RowObject.id_categoria_tarea:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAILABLE categorias_tareas THEN nombre-categoria-tarea:SCREEN-VALUE = categorias_tareas.descripcion.
  ELSE nombre-categoria-tarea:SCREEN-VALUE = "".


FIND FIRST liq_conceptos WHERE liq_conceptos.id_concepto = INTEGER(RowObject.id_codigo_abacus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_conceptos THEN nombre-abacus:SCREEN-VALUE = liq_conceptos.descripcion.
        ELSE nombre-abacus:SCREEN-VALUE = "".

 
FIND FIRST liq_sectores WHERE liq_sectores.id_sector = INTEGER(RowObject.id_sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
 IF AVAILABLE liq_sectores THEN nombre-sector:SCREEN-VALUE = liq_sectores.descripcion.
       ELSE nombre-sector:SCREEN-VALUE = "".

FIND FIRST tipo_cant_adicional WHERE tipo_cant_adicional.id_tipo_cantidad = INTEGER(RowObject.id_tipo_cant-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE tipo_cant_adicional THEN nombre-cant1:SCREEN-VALUE = tipo_cant_adicional.descripcion.
      ELSE nombre-cant1:SCREEN-VALUE = "".

FIND FIRST tipo_cant_adicional WHERE tipo_cant_adicional.id_tipo_cantidad = INTEGER(RowObject.id_tipo_cant-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE tipo_cant_adicional THEN nombre-cant2:SCREEN-VALUE = tipo_cant_adicional.descripcion.
      ELSE nombre-cant2:SCREEN-VALUE = "".


            
FIND FIRST tipo_excedente WHERE tipo_excedente.id_tipo_excedente = INTEGER(RowObject.id_tipo_excedente:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE tipo_excedente THEN nombre-excedente:SCREEN-VALUE = tipo_excedente.descripcion.
        ELSE nombre-excedente:SCREEN-VALUE = "".

FIND FIRST tipo_automatico WHERE tipo_automatico.id_tipo_automatico = INTEGER(RowObject.id_tipo_automatico:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE tipo_automatico THEN nombre-automatico:SCREEN-VALUE = tipo_automatico.descripcion.
        ELSE nombre-automatico:SCREEN-VALUE = "".

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

