&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
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
DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.

DEFINE VARIABLE dKilClaro  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKilTurbio AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKilPulpa  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKi4Claro  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKi4Turbio AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKi4Pulpa  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKilOtros  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKi4Otros  AS DECIMAL    NO-UNDO.


{adm2/support/customColors.i}

DEFINE TEMP-TABLE ttTransfer
  FIELD sucursal            AS CHARACTER 
  FIELD fecha               AS DATE
  FIELD fecha_finalizacion  AS DATE
  FIELD c_fecha             AS DATE
  FIELD id_lote             AS INTEGER
  FIELD anio                AS INTEGER
  FIELD codigo_lote         AS CHARACTER
  FIELD tambores            AS INTEGER
  FIELD kilos               AS DECIMAL
  FIELD contrato            AS CHARACTER
  FIELD id_articulo         AS INTEGER
  FIELD articulo            AS CHARACTER
  FIELD id_calidad          AS INTEGER
  FIELD calidad             AS CHARACTER
  FIELD id_envase           AS INTEGER
  FIELD envase              AS CHARACTER
  FIELD kilos400            AS DECIMAL
  FIELD orden_fabricacion   AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF

&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lotes_jugo

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio anio_of Balanza_usada Calibracion composicion_lote control_calidad~
 Control_pesas c_fecha c_hora c_usuario estado estado_lote Fecha~
 Fecha_comienzo Fecha_comienzo_envase Fecha_finalizacion~
 Fecha_finalizacion_envase Hora_comienzo Hora_comienzo_envase~
 Hora_finalizacion Hora_fin_envase id_articulo id_calidad id_contrato_of~
 id_empresa id_envase id_envio_of id_legajo_capataz id_lote id_lote_nuevo~
 id_orden_entrega id_sucursal id_tanque id_tipocontrato_of id_tipolimon~
 id_tipotambor item_oe item_of Jugo_pomelo microbiologia nromov~
 observaciones Peso_neto Pulpa quimico_control_calidad quimico_microbiologia~
 cantidad_envases_nuevo cantidad_tambores_recup id_condicion_origen~
 kilos_jugo_linea capataz envasadores etiqueta_adicional~
 etiq_adicional_descripcion hora_aprobacion id_camara id_tanque_sobrante~
 concentracion_mesh codigo_lote fecha_control_calidad fecha_microbiologia~
 fecha_pesticida fecha_sensorial pesticida sensorial
&Scoped-define ENABLED-FIELDS-IN-lotes_jugo anio anio_of Balanza_usada ~
Calibracion composicion_lote control_calidad Control_pesas c_fecha c_hora ~
c_usuario estado estado_lote Fecha Fecha_comienzo Fecha_comienzo_envase ~
Fecha_finalizacion Fecha_finalizacion_envase Hora_comienzo ~
Hora_comienzo_envase Hora_finalizacion Hora_fin_envase id_articulo ~
id_calidad id_contrato_of id_empresa id_envase id_envio_of ~
id_legajo_capataz id_lote id_lote_nuevo id_orden_entrega id_sucursal ~
id_tanque id_tipocontrato_of id_tipolimon id_tipotambor item_oe item_of ~
Jugo_pomelo microbiologia nromov observaciones Peso_neto Pulpa ~
quimico_control_calidad quimico_microbiologia cantidad_envases_nuevo ~
cantidad_tambores_recup id_condicion_origen kilos_jugo_linea capataz ~
envasadores etiqueta_adicional etiq_adicional_descripcion hora_aprobacion ~
id_camara id_tanque_sobrante concentracion_mesh codigo_lote ~
fecha_control_calidad fecha_microbiologia fecha_pesticida fecha_sensorial ~
pesticida sensorial 
&Scoped-Define DATA-FIELDS  anio anio_of Balanza_usada Calibracion composicion_lote control_calidad~
 Control_pesas c_fecha c_hora c_usuario estado estado_lote Fecha~
 Fecha_comienzo Fecha_comienzo_envase Fecha_finalizacion~
 Fecha_finalizacion_envase Hora_comienzo Hora_comienzo_envase~
 Hora_finalizacion Hora_fin_envase id_articulo id_calidad id_contrato_of~
 id_empresa id_envase id_envio_of id_legajo_capataz id_lote id_lote_nuevo~
 id_orden_entrega id_sucursal id_tanque id_tipocontrato_of id_tipolimon~
 id_tipotambor item_oe item_of Jugo_pomelo microbiologia nromov~
 observaciones Peso_neto Pulpa quimico_control_calidad quimico_microbiologia~
 Articulo cantidad_envases_nuevo cantidad_tambores_recup id_condicion_origen~
 kilos_jugo_linea capataz envasadores etiqueta_adicional Calidad~
 etiq_adicional_descripcion hora_aprobacion id_camara id_tanque_sobrante~
 concentracion_mesh Envase codigo_lote fecha_control_calidad~
 fecha_microbiologia fecha_pesticida Sucursal fecha_sensorial Tambores~
 pesticida ControlCalidad sensorial Quimico ControlMicro QuimicoMicro Kilos~
 Kilos400 Litros Contrato OrdenFabricacion NroCtrlAjuste Galones
&Scoped-define DATA-FIELDS-IN-lotes_jugo anio anio_of Balanza_usada ~
Calibracion composicion_lote control_calidad Control_pesas c_fecha c_hora ~
c_usuario estado estado_lote Fecha Fecha_comienzo Fecha_comienzo_envase ~
Fecha_finalizacion Fecha_finalizacion_envase Hora_comienzo ~
Hora_comienzo_envase Hora_finalizacion Hora_fin_envase id_articulo ~
id_calidad id_contrato_of id_empresa id_envase id_envio_of ~
id_legajo_capataz id_lote id_lote_nuevo id_orden_entrega id_sucursal ~
id_tanque id_tipocontrato_of id_tipolimon id_tipotambor item_oe item_of ~
Jugo_pomelo microbiologia nromov observaciones Peso_neto Pulpa ~
quimico_control_calidad quimico_microbiologia cantidad_envases_nuevo ~
cantidad_tambores_recup id_condicion_origen kilos_jugo_linea capataz ~
envasadores etiqueta_adicional etiq_adicional_descripcion hora_aprobacion ~
id_camara id_tanque_sobrante concentracion_mesh codigo_lote ~
fecha_control_calidad fecha_microbiologia fecha_pesticida fecha_sensorial ~
pesticida sensorial 
&Scoped-Define MANDATORY-FIELDS  id_calidad id_lote id_sucursal id_camara
&Scoped-Define APPLICATION-SERVICE asindustria
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dlotesjugo.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH lotes_jugo ~
      WHERE lotes_jugo.anio >= 2005 NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH lotes_jugo ~
      WHERE lotes_jugo.anio >= 2005 NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main lotes_jugo
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main lotes_jugo


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addRow dTables  _DB-REQUIRED
FUNCTION addRow RETURNS CHARACTER
  ( INPUT pcViewColList AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dTotTurbio dTables  _DB-REQUIRED
FUNCTION dTotTurbio RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalidad dTables  _DB-REQUIRED
FUNCTION getCalidad RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadesOrigen dTables  _DB-REQUIRED
FUNCTION getCantidadesOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadTambores dTables  _DB-REQUIRED
FUNCTION getCantidadTambores RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlAjuste dTables  _DB-REQUIRED
FUNCTION getControlAjuste RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosContrato dTables  _DB-REQUIRED
FUNCTION getDatosContrato RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosGraficoProduccion dTables  _DB-REQUIRED
FUNCTION getDatosGraficoProduccion RETURNS CHARACTER
  (piArticulo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEstado dTables  _DB-REQUIRED
FUNCTION getEstado RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEstadoMicrobiologia dTables  _DB-REQUIRED
FUNCTION getEstadoMicrobiologia RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGalones dTables  _DB-REQUIRED
FUNCTION getGalones RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos400Lote dTables  _DB-REQUIRED
FUNCTION getKilos400Lote RETURNS DECIMAL
  (plAction AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosLote dTables  _DB-REQUIRED
FUNCTION getKilosLote RETURNS DECIMAL
  (plAction AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrdenFabricacion dTables  _DB-REQUIRED
FUNCTION getOrdenFabricacion RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProducto dTables  _DB-REQUIRED
FUNCTION getProducto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuimicoId dTables  _DB-REQUIRED
FUNCTION getQuimicoId RETURNS INTEGER
  (INPUT pcUser AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuimicoMicro dTables  _DB-REQUIRED
FUNCTION getQuimicoMicro RETURNS CHARACTER
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuimicoName dTables  _DB-REQUIRED
FUNCTION getQuimicoName RETURNS CHARACTER
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowId dTables  _DB-REQUIRED
FUNCTION getRowId RETURNS ROWID
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTot400Claro dTables  _DB-REQUIRED
FUNCTION getTot400Claro RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTot400Otros dTables  _DB-REQUIRED
FUNCTION getTot400Otros RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTot400Pulpa dTables  _DB-REQUIRED
FUNCTION getTot400Pulpa RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTot400Turbio dTables  _DB-REQUIRED
FUNCTION getTot400Turbio RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotClaro dTables  _DB-REQUIRED
FUNCTION getTotClaro RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotKilos dTables  _DB-REQUIRED
FUNCTION getTotKilos RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotKilos400 dTables  _DB-REQUIRED
FUNCTION getTotKilos400 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotOtros dTables  _DB-REQUIRED
FUNCTION getTotOtros RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotPulpa dTables  _DB-REQUIRED
FUNCTION getTotPulpa RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotTurbio dTables  _DB-REQUIRED
FUNCTION getTotTurbio RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      lotes_jugo SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Partition: asindustria
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "general.lotes_jugo"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "general.lotes_jugo.anio >= 2005"
     _FldNameList[1]   > general.lotes_jugo.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > general.lotes_jugo.anio_of
"anio_of" "anio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[3]   > general.lotes_jugo.Balanza_usada
"Balanza_usada" "Balanza_usada" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[4]   > general.lotes_jugo.Calibracion
"Calibracion" "Calibracion" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[5]   > general.lotes_jugo.composicion_lote
"composicion_lote" "composicion_lote" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[6]   > general.lotes_jugo.control_calidad
"control_calidad" "control_calidad" ? ? "logical" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[7]   > general.lotes_jugo.Control_pesas
"Control_pesas" "Control_pesas" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.4 yes
     _FldNameList[8]   > general.lotes_jugo.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[9]   > general.lotes_jugo.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[10]   > general.lotes_jugo.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[11]   > general.lotes_jugo.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[12]   > general.lotes_jugo.estado_lote
"estado_lote" "estado_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[13]   > general.lotes_jugo.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[14]   > general.lotes_jugo.Fecha_comienzo
"Fecha_comienzo" "Fecha_comienzo" ? ? "date" ? ? ? ? ? ? yes ? no 16.2 yes
     _FldNameList[15]   > general.lotes_jugo.Fecha_comienzo_envase
"Fecha_comienzo_envase" "Fecha_comienzo_envase" ? ? "date" ? ? ? ? ? ? yes ? no 24.4 yes
     _FldNameList[16]   > general.lotes_jugo.Fecha_finalizacion
"Fecha_finalizacion" "Fecha_finalizacion" ? ? "date" ? ? ? ? ? ? yes ? no 17.6 yes
     _FldNameList[17]   > general.lotes_jugo.Fecha_finalizacion_envase
"Fecha_finalizacion_envase" "Fecha_finalizacion_envase" ? ? "date" ? ? ? ? ? ? yes ? no 25.8 yes
     _FldNameList[18]   > general.lotes_jugo.Hora_comienzo
"Hora_comienzo" "Hora_comienzo" ? ? "character" ? ? ? ? ? ? yes ? no 17.2 yes
     _FldNameList[19]   > general.lotes_jugo.Hora_comienzo_envase
"Hora_comienzo_envase" "Hora_comienzo_envase" ? ? "character" ? ? ? ? ? ? yes ? no 24.8 yes
     _FldNameList[20]   > general.lotes_jugo.Hora_finalizacion
"Hora_finalizacion" "Hora_finalizacion" ? ? "character" ? ? ? ? ? ? yes ? no 18.6 yes
     _FldNameList[21]   > general.lotes_jugo.Hora_fin_envase
"Hora_fin_envase" "Hora_fin_envase" ? ? "character" ? ? ? ? ? ? yes ? no 18 yes
     _FldNameList[22]   > general.lotes_jugo.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[23]   > general.lotes_jugo.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[24]   > general.lotes_jugo.id_contrato_of
"id_contrato_of" "id_contrato_of" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[25]   > general.lotes_jugo.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[26]   > general.lotes_jugo.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[27]   > general.lotes_jugo.id_envio_of
"id_envio_of" "id_envio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[28]   > general.lotes_jugo.id_legajo_capataz
"id_legajo_capataz" "id_legajo_capataz" ? ? "integer" ? ? ? ? ? ? yes ? no 17.2 yes
     _FldNameList[29]   > general.lotes_jugo.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[30]   > general.lotes_jugo.id_lote_nuevo
"id_lote_nuevo" "id_lote_nuevo" ? ? "character" ? ? ? ? ? ? yes ? no 11.2 yes
     _FldNameList[31]   > general.lotes_jugo.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[32]   > general.lotes_jugo.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[33]   > general.lotes_jugo.id_tanque
"id_tanque" "id_tanque" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[34]   > general.lotes_jugo.id_tipocontrato_of
"id_tipocontrato_of" "id_tipocontrato_of" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[35]   > general.lotes_jugo.id_tipolimon
"id_tipolimon" "id_tipolimon" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[36]   > general.lotes_jugo.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[37]   > general.lotes_jugo.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[38]   > general.lotes_jugo.item_of
"item_of" "item_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[39]   > general.lotes_jugo.Jugo_pomelo
"Jugo_pomelo" "Jugo_pomelo" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[40]   > general.lotes_jugo.microbiologia
"microbiologia" "microbiologia" ? ? "logical" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[41]   > general.lotes_jugo.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[42]   > general.lotes_jugo.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
     _FldNameList[43]   > general.lotes_jugo.Peso_neto
"Peso_neto" "Peso_neto" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[44]   > general.lotes_jugo.Pulpa
"Pulpa" "Pulpa" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[45]   > general.lotes_jugo.quimico_control_calidad
"quimico_control_calidad" "quimico_control_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[46]   > general.lotes_jugo.quimico_microbiologia
"quimico_microbiologia" "quimico_microbiologia" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[47]   > "_<CALC>"
"getProducto()" "Articulo" "Articulo" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[48]   > general.lotes_jugo.cantidad_envases_nuevo
"cantidad_envases_nuevo" "cantidad_envases_nuevo" ? ? "integer" ? ? ? ? ? ? yes ? no 24.6 yes
     _FldNameList[49]   > general.lotes_jugo.cantidad_tambores_recup
"cantidad_tambores_recup" "cantidad_tambores_recup" ? ? "integer" ? ? ? ? ? ? yes ? no 24 yes
     _FldNameList[50]   > general.lotes_jugo.id_condicion_origen
"id_condicion_origen" "id_condicion_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 19 yes
     _FldNameList[51]   > general.lotes_jugo.kilos_jugo_linea
"kilos_jugo_linea" "kilos_jugo_linea" ? ? "decimal" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[52]   > general.lotes_jugo.capataz
"capataz" "capataz" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[53]   > general.lotes_jugo.envasadores
"envasadores" "envasadores" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[54]   > general.lotes_jugo.etiqueta_adicional
"etiqueta_adicional" "etiqueta_adicional" ? ? "logical" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[55]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[56]   > general.lotes_jugo.etiq_adicional_descripcion
"etiq_adicional_descripcion" "etiq_adicional_descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[57]   > general.lotes_jugo.hora_aprobacion
"hora_aprobacion" "hora_aprobacion" ? ? "character" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[58]   > general.lotes_jugo.id_camara
"id_camara" "id_camara" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[59]   > general.lotes_jugo.id_tanque_sobrante
"id_tanque_sobrante" "id_tanque_sobrante" ? ? "integer" ? ? ? ? ? ? yes ? no 26 yes
     _FldNameList[60]   > general.lotes_jugo.concentracion_mesh
"concentracion_mesh" "concentracion_mesh" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.6 yes
     _FldNameList[61]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[62]   > general.lotes_jugo.codigo_lote
"codigo_lote" "codigo_lote" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[63]   > general.lotes_jugo.fecha_control_calidad
"fecha_control_calidad" "fecha_control_calidad" ? ? "date" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[64]   > general.lotes_jugo.fecha_microbiologia
"fecha_microbiologia" "fecha_microbiologia" ? ? "date" ? ? ? ? ? ? yes ? no 18.8 yes
     _FldNameList[65]   > general.lotes_jugo.fecha_pesticida
"fecha_pesticida" "fecha_pesticida" ? ? "date" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[66]   > "_<CALC>"
"getSucursal()" "Sucursal" "Sucursal" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[67]   > general.lotes_jugo.fecha_sensorial
"fecha_sensorial" "fecha_sensorial" ? ? "date" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[68]   > "_<CALC>"
"getCantidadTambores()" "Tambores" "Tambores" ">>>>9" "Integer" ? ? ? ? ? ? no ? no 9.4 no
     _FldNameList[69]   > general.lotes_jugo.pesticida
"pesticida" "pesticida" ? ? "logical" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[70]   > "_<CALC>"
"getEstado()" "ControlCalidad" "ControlCalidad" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[71]   > general.lotes_jugo.sensorial
"sensorial" "sensorial" ? ? "logical" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[72]   > "_<CALC>"
"getQuimicoName()" "Quimico" "Quimico" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[73]   > "_<CALC>"
"getEstadoMicrobiologia()" "ControlMicro" "ControlMicro" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[74]   > "_<CALC>"
"getQuimicoMicro()" "QuimicoMicro" "QuimicoMicro" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[75]   > "_<CALC>"
"getKilosLote(FALSE)" "Kilos" "Kilos" ">>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.8 no
     _FldNameList[76]   > "_<CALC>"
"getKilos400Lote(TRUE)" "Kilos400" "Kilos400" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[77]   > "_<CALC>"
"getKilosLote(FALSE)" "Litros" "Litros" ">>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10 no
     _FldNameList[78]   > "_<CALC>"
"getDatosContrato()" "Contrato" "Contrato" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[79]   > "_<CALC>"
"getOrdenFabricacion()" "OrdenFabricacion" "OrdenFabricacion" ">>>9" "Integer" ? ? ? ? ? ? no ? no 16.8 no
     _FldNameList[80]   > "_<CALC>"
"getControlAjuste()" "NroCtrlAjuste" "NroCtrlAjuste" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[81]   > "_<CALC>"
"getGalones()" "Galones" "Galontes" ">>>,>>>,>>9.999999" "Decimal" ? ? ? ? ? ? no ? no 19.8 no
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE beginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lTieneRemito AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lTieneReproc AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lExiste      AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE hCont     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE ibgcolor  AS INTEGER    NO-UNDO.  
  DEFINE VARIABLE hTam      AS HANDLE     NO-UNDO.

  {get ContainerSource hCont}.


  
  FIND LAST RowObjUpd NO-ERROR.


  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
    IF rowObjUpd.id_articulo = 0 THEN
      RETURN "Error en articulo".
  
    IF rowObjUpd.id_condicion_origen = 0 THEN
      RETURN "Error condicion de origenes del lote".
  
    lExiste = DYNAMIC-FUNCTION('getLoteExistente' IN hLib, rowObjUpd.id_sucursal,
                                                           3, 
                                                           rowObjUpd.id_lote,
                                                           rowObjUpd.anio,
                                                           rowObjUpd.id_articulo).
    IF lExiste THEN 
      RETURN "Lote Existente".
  
  
    
    ASSIGN rowObjUpd.id_tipotambor  = 3
           rowObjUpd.nromov         = NEXT-VALUE(nromov)
           rowObjUpd.estado_lote    = 1
           rowObjUpd.anio           = YEAR(rowObjUpd.fecha)
           rowObjUpd.c_usuario      = USERID("userdb")
           rowObjUpd.c_fecha        = TODAY
           rowObjUpd.c_hora         = STRING(TIME,"HH:MM:SS").
  
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_tipotambor,nromov,estado_lote,anio,c_usuario,c_fecha,c_hora".  
  END.
  

  /*** UPDATE ***/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".

    IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wLotesJugo.w" THEN DO:
      RUN setArticuloCalidad.
    END. /*if cont = lotes_jugo*/

    
    IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wControlCalidadJugo.w" THEN DO:
      RUN setControlCalidad.
    END. /*if cont = wControlCalidad.w*/


    ASSIGN rowObjUpd.c_usuario               = USERID("usedrb")
           rowObjUpd.c_fecha                 = TODAY
           rowObjUpd.c_hora                  = STRING(TIME,"HH:MM:SS")
           .
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".
  
    
  END. /*feach rowObjUpd.rowMod = U ...*/

  
  
  /*borrado*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D".
  
    /*si tiene remitos u oes*/
    lTieneRemito = DYNAMIC-FUNCTION('getLoteTieneRemitos' IN hLib, rowObjUpd.id_empresa, 
                                                                   rowObjUpd.id_sucursal, 
                                                                   rowObjUpd.id_tipotambor,
                                                                   rowObjUpd.nromov).
    IF lTieneRemito THEN DO:
      MESSAGE "Imposible Eliminar al cual se la han hecho remitos" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN "ADM-ERROR".
    END.
  
    /*si tiene reprocesos*/
    lTieneReproc = DYNAMIC-FUNCTION('getLoteTieneReprocesos' IN hLib, rowObjUpd.id_empresa, 
                                                                      rowObjUpd.id_sucursal, 
                                                                      rowObjUpd.id_tipotambor,
                                                                      rowObjUpd.nromov).
    IF lTieneReproc THEN DO:
      MESSAGE "Imposible Eliminar que tiene tambores que fueron reprocesados" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN "ADM-ERROR".
    END.
  
    /*agrego entrada en el spool de mails a depositos*/
    RUN addMailingSpoolEntry IN hLib (rowObjUpd.id_empresa,
                                      rowObjUpd.id_sucursal,
                                      rowObjUpd.id_tipotambor,
                                      rowObjUpd.nromov, 
                                      "delete").
  
    /*elimino punto de envase*/
    RUN deletePuntoEnvase IN hLib (rowObjUpd.id_empresa,
                                   rowObjUpd.id_sucursal,
                                   rowObjUpd.id_tipotambor,
                                   rowObjUpd.nromov).
  
    
  
    /*arrastre*/
    FOR EACH arrastre_lote WHERE arrastre_lote.id_empresa    = rowObjUpd.id_empresa
                             AND arrastre_lote.id_sucursal   = rowObjUpd.id_sucursal
                             AND arrastre_lote.id_tipotambor = rowObjUpd.id_tipotambor
                             AND arrastre_lote.nromov        = rowObjUpd.nromov.
      /*elimino tambores*/
      RUN deleteDrumsFromBatch IN hLib (arrastre_lote.id_empresa, 
                                        arrastre_lote.id_sucursal, 
                                        arrastre_lote.id_tipotambor, 
                                        arrastre_lote.nromov, 
                                        TRUE).    
      DELETE arrastre_lote.
    END.
  
    /*sobrante*/
    FOR EACH sobrante WHERE sobrante.id_empresa    = rowObjUpd.id_empresa
                        AND sobrante.id_sucursal   = rowObjUpd.id_sucursal
                        AND sobrante.id_tipotambor = rowObjUpd.id_tipotambor
                        AND sobrante.nromov        = rowObjUpd.nromov.
      /*elimino tambores*/
      RUN deleteDrumsFromBatch IN hLib (sobrante.id_empresa, 
                                        sobrante.id_sucursal, 
                                        sobrante.id_tipotambor, 
                                        sobrante.nromov, 
                                        TRUE).    
      DELETE sobrante.
    END.
  
  
    /*libero origenes*/
    RUN removeOrigenes IN hLib (rowObjUpd.id_empresa, 
                                rowObjUpd.id_sucursal, 
                                rowObjUpd.id_tipotambor, 
                                rowObjUpd.nromov).
    /*elimino tambores*/
    RUN deleteDrumsFromBatch IN hLib (rowObjUpd.id_empresa, 
                                      rowObjUpd.id_sucursal, 
                                      rowObjUpd.id_tipotambor, 
                                      rowObjUpd.nromov, 
                                      TRUE).
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcTots dTables  _DB-REQUIRED
PROCEDURE calcTots :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE BUFFER buRow FOR rowObject.

  ASSIGN dKilClaro  = 0
         dKi4Claro  = 0
         dKilTurbio = 0
         dKi4Turbio = 0
         dKilPulpa  = 0
         dKi4Pulpa  = 0 
         dKilOtros  = 0
         dKi4Otros  = 0
         .

  FOR EACH buRow
      WHERE buRow.id_articulo = 53
      NO-LOCK.
    dKilClaro = dKilClaro + buRow.kilos.
    dKi4Claro = dKi4Claro + buRow.kilos400.
  END.

  FOR EACH buRow
      WHERE buRow.id_articulo = 52
      NO-LOCK.
    dKilTurbio = dKilTurbio + buRow.kilos.
    dKi4Turbio = dKi4Turbio + buRow.kilos400.
  END.

  FOR EACH buRow
      WHERE buRow.id_articulo = 71
      NO-LOCK.
    dKilPulpa = dKilPulpa + buRow.kilos.
    dKi4Pulpa = dKi4Pulpa + buRow.kilos400.
  END.

  FOR EACH buRow
      WHERE (buRow.id_articulo <> 52 AND buRow.id_articulo <> 53 AND buRow.id_articulo <> 71)
      NO-LOCK.
    dKilOtros = dKilOtros + buRow.kilos.
    dKi4Otros = dKi4Otros + buRow.kilos400.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.Articulo = (getProducto())
         rowObject.Calidad = (getCalidad())
         rowObject.Contrato = (getDatosContrato())
         rowObject.ControlCalidad = (getEstado())
         rowObject.ControlMicro = (getEstadoMicrobiologia())
         rowObject.Envase = (getEnvase())
         rowObject.Galones = (getGalones())
         rowObject.Kilos = (getKilosLote(FALSE))
         rowObject.Kilos400 = (getKilos400Lote(TRUE))
         rowObject.Litros = (getKilosLote(FALSE))
         rowObject.NroCtrlAjuste = (getControlAjuste())
         rowObject.OrdenFabricacion = (getOrdenFabricacion())
         rowObject.Quimico = (getQuimicoName())
         rowObject.QuimicoMicro = (getQuimicoMicro())
         rowObject.Sucursal = (getSucursal())
         rowObject.Tambores = (getCantidadTambores())
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE endTransactionValidate dTables  _DB-REQUIRED
PROCEDURE endTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPosUpdate dTables  _DB-REQUIRED
PROCEDURE especialPosUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER  xCase   AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPreCreate dTables  _DB-REQUIRED
PROCEDURE especialPreCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xCase AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPreUpdate dTables  _DB-REQUIRED
PROCEDURE especialPreUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER xCase   AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fechaValidate dTables  _DB-REQUIRED
PROCEDURE fechaValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF DATE(pcValor) <> TODAY AND DATE(pcValor) <> TODAY - 1 THEN 
    RETURN "No puede cargar un lote con fecha anterior al " + STRING(TODAY - 1).
  ELSE
    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fieldsWithProblems dTables  _DB-REQUIRED
PROCEDURE fieldsWithProblems :
/*------------------------------------------------------------------------------
  Purpose: In this procedure we check all conditions of BUssines Logic that the
  RowObject can give us.Returns a comma separated list in wich each element is a
  pair of field + chr(3)+  color we want to mark as irregular ones
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cList AS CHARACTER NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getField dTables  _DB-REQUIRED
PROCEDURE getField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xFieldName AS CHARACTER NO-UNDO.

DEFINE VAR hBuffer AS HANDLE NO-UNDO.
DEFINE VAR hField  AS HANDLE NO-UNDO.

hBuffer = BUFFER RowObject:HANDLE.
IF VALID-HANDLE(hBuffer) THEN
DO:
    hField = hBuffer:BUFFER-FIELD(xFieldName).
    IF VALID-HANDLE(hField) THEN
    DO:
        RETURN hField:BUFFER-VALUE().
    END.
END.

RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hora_comienzoValidate dTables  _DB-REQUIRED
PROCEDURE hora_comienzoValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF INTEGER(SUBSTRING(pcValor,1,2)) > 24 OR INTEGER(SUBSTRING(pcValor,4,2)) > 60 THEN 
    RETURN "Debe ingresar una hora v�lida".
  ELSE
    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hora_finalizacionValidate dTables  _DB-REQUIRED
PROCEDURE hora_finalizacionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF INTEGER(SUBSTRING(pcValor,1,2)) > 24 OR INTEGER(SUBSTRING(pcValor,4,2)) > 60 THEN 
    RETURN "Debe ingresar una hora v�lida".
  ELSE 
    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hora_fin_envaseValidate dTables  _DB-REQUIRED
PROCEDURE hora_fin_envaseValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF INTEGER(SUBSTRING(pcValor,1,2)) > 24 OR INTEGER(SUBSTRING(pcValor,4,2)) > 60 THEN 
    RETURN "Debe ingresar una hora v�lida".
  ELSE 
    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_articuloValidate dTables  _DB-REQUIRED
PROCEDURE id_articuloValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  
  IF INTEGER(pcValor) = 0 THEN 
    RETURN "Error en articulo".
  ELSE 
    RETURN "".




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_calidadValidate dTables  _DB-REQUIRED
PROCEDURE id_calidadValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.
  DEFINE BUFFER buLote FOR lotes_jugo.

  IF rowObject.id_articulo = 53 OR rowObject.id_articulo = 52 THEN DO:
    FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = rowObject.id_articulo
                                     AND r_productos_calidad.id_calidad  = INTEGER(pcValor)
                                   NO-LOCK NO-ERROR.
    IF NOT AVAILABLE r_productos_calidad THEN
      RETURN "Ingrese la Calidad Correcta" .
    ELSE
      RETURN "".

  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_condicion_origenValidate dTables  _DB-REQUIRED
PROCEDURE id_condicion_origenValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF INTEGER(pcValor) = 0 THEN
    RETURN "Especifique de que forma se creara el lote. (jugo linea o jugo linea + reproceso)".
  ELSE
    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_sucursalValidate dTables  _DB-REQUIRED
PROCEDURE id_sucursalValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF INTEGER(pcValor) = 0 THEN
    RETURN "Error. Valor de Sucursal Invalido.".
  ELSE
    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/



  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  /*
  DEFINE VAR xDataSource AS CHARACTER NO-UNDO.
  {get DataSource xDataSource}.
  IF xDataSource <> ? THEN
  DO:
      {set AutoCommit NO}.
      {set CommitSource xDataSource}.
  END.
  */

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE peso_netoValidate dTables  _DB-REQUIRED
PROCEDURE peso_netoValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF INTEGER(pcValor) <= 0 THEN
    RETURN "Por favor ingrese el peso neto de los tambores que van conformar el lote".
  ELSE
    RETURN "".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postTransactionValidate dTables  _DB-REQUIRED
PROCEDURE postTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postUpdate dTables  _DB-REQUIRED
PROCEDURE postUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer   AS HANDLE NO-UNDO.
DEFINE VAR hField                AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preCreate dTables  _DB-REQUIRED
PROCEDURE preCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preUpdate dTables  _DB-REQUIRED
PROCEDURE preUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setArticuloCalidad dTables  _DB-REQUIRED
PROCEDURE setArticuloCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF rowObjUpd.id_articulo = 0 THEN
    RETURN "Error en articulo".

  IF rowObjUpd.id_condicion_origen = 0 THEN
    RETURN "Error condicion de origenes del lote".

  FIND FIRST envases_prod WHERE envases_prod.id_envase = rowObjUpd.id_envase 
                          NO-LOCK NO-ERROR.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = rowObjUpd.id_empresa
                                AND tambores_industria.id_sucursal   = rowObjUpd.id_sucursal
                                AND tambores_industria.id_tipotambor = rowObjUpd.id_tipotambor
                                AND tambores_industria.nromov        = rowObjUpd.nromov.
      
    ASSIGN tambores_industria.id_articulo    = rowObjUpd.id_articulo
           tambores_industria.id_calidad     = rowObjUpd.id_calidad
           tambores_industria.fecha          = rowObjUpd.fecha
           tambores_industria.id_lote        = rowObjUpd.id_lote
           tambores_industria.id_envase      = rowObjUpd.id_envase
           tambores_industria.kilos_tambor   = rowObjUpd.peso_neto
           tambores_industria.tara           = IF AVAILABLE envases_prod THEN envases_prod.tara ELSE 0.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setControlCalidad dTables  _DB-REQUIRED
PROCEDURE setControlCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none> plEstado = true ==> lote aprobado, false ==> desaprobado  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iEstadoLote  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEstadoTamb  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iQuimico     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cUser        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lChoice      AS LOGICAL    NO-UNDO.
  

  cUser     = USERID("userdb").
  cUser     = REPLACE(cUser, "y_", "").
  iQuimico  = getQuimicoId(cUser).

  IF rowObjUpd.CONTROL_calidad AND rowObjUpd.microbiologia THEN
    ASSIGN iEstadoLote = 3
           iEstadoTamb = 11.
  ELSE 
    ASSIGN iEstadoLote = 4
           iEstadoTamb = 2.

  ASSIGN rowObjUpd.estado_lote             = iEstadoLote
         rowObjUpd.quimico_control_calidad = getQuimicoId(USERID("userdb"))
         rowObjUpd.quimico_microbiologia   = getQuimicoId(USERID("userdb")).

  rowObjUpd.changedFields = rowObjUpd.changedFields + ",estado_lote,quimico_control_calidad,quimico_microbiologia".

  FOR EACH tambores_industria
      WHERE tambores_industria.id_empresa    = rowObjUpd.id_empresa
        AND tambores_industria.id_sucursal   = rowObjUpd.id_sucursal
        AND tambores_industria.id_tipotambor = rowObjUpd.id_tipotambor
        AND tambores_industria.nromov        = rowObjUpd.nromov.

    RUN setEstadoTambor IN hLib (tambores_industria.id_empresa,
                                 tambores_industria.id_sucursal,
                                 tambores_industria.id_tipotambor,
                                 tambores_industria.nromov,
                                 tambores_industria.id_tambor,
                                 iEstadoTamb).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKilos dTables  _DB-REQUIRED
PROCEDURE setKilos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hRep AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCoe AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buRo FOR rowObject.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.



  FOR EACH buRo.
    
    cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hRep, buRo.id_empresa, 
                                                          buRo.id_sucursal, 
                                                          buRo.id_tipotambor, 
                                                          buRo.nromov).
    IF cAnl = "" OR cAnl = ? OR cAnl = "Aceite" THEN NEXT.
    
    /*dLit = dKil / (DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hTam, DECIMAL(ENTRY(4, cAnl, CHR(1))))).*/
    dKi4 = DYNAMIC-FUNCTION('getKilos400GPL' IN hTam, DECIMAL(ENTRY(3, cAnl, CHR(1))),
                                                      DECIMAL(ENTRY(2, cAnl, CHR(1))), 
                                                      buRo.litros, 
                                                      FALSE).
    
    ASSIGN /*buRo.kilos    = dKil*/
           buRo.kilos400 = dKi4
           dKil               = 0
           dKi4               = 0
           dLit               = 0
           .

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tieDrumsToBatch dTables  _DB-REQUIRED
PROCEDURE tieDrumsToBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcDrumsList  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER lAction      AS LOGICAL    NO-UNDO. /*true asocia, false desvincula*/

  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRowId  AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE cTo     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSubj   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBody   AS CHARACTER  NO-UNDO.

  cTo   = "lmunoz@sa-sanmiguel.com,facundoj@sa-sanmiguel.com".
  cSubj = "asignacion de origenes de lote".

  FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa    = piEmpresa
                          AND lotes_jugo.id_sucursal   = piSucursal
                          AND lotes_jugo.id_tipotambor = piTipoTambor
                          AND lotes_jugo.nromov        = piNroMov
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:  
    DO i = 1 TO NUM-ENTRIES(pcDrumsList, CHR(10)):
      cRowId = ENTRY(i, pcDrumsList, CHR(10)).
      FIND FIRST tambores_industria WHERE ROWID(tambores_industria) = TO-ROWID(cRowId) 
                                    NO-ERROR.
      IF AVAILABLE tambores_industria THEN DO:
        
        IF lAction THEN DO: /*vincular tambor a lote*/
          RUN setLoteDestinoToTambor IN hLib (tambores_industria.id_empresa, 
                                              tambores_industria.id_sucursal, 
                                              tambores_industria.id_tipotambor,
                                              tambores_industria.nromov, 
                                              tambores_industria.id_tambor, 
                                              lotes_jugo.id_empresa, 
                                              lotes_jugo.id_sucursal, 
                                              lotes_jugo.id_tipotambor, 
                                              lotes_jugo.nromov, 
                                              TRUE).

        END.
        ELSE DO: /*desvincular tambor de lote*/
          RUN setLoteDestinoToTambor IN hLib(tambores_industria.id_empresa, 
                                             tambores_industria.id_sucursal, 
                                             tambores_industria.id_tipotambor,
                                             tambores_industria.nromov, 
                                             tambores_industria.id_tambor, 
                                             0, 
                                             0, 
                                             0, 
                                             0, 
                                             TRUE).
        END.
      END.
    END.
/*
    /*me manda un mail cada vez que se rebatchean tambores*/
    RUN ..\industria\sendMail.p("",                               
                                2,                                
                                "Rebatcheo de Tambores",          
                                pcDrumsList + CHR(10) + CHR(13) + STRING(piNroMov), 
                                "facundoj@sa-sanmiguel.com", 
                                "").                         
*/                                


  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toExcelLotesAntiguos dTables  _DB-REQUIRED
PROCEDURE toExcelLotesAntiguos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piAnio AS INTEGER    NO-UNDO.

  DEFINE VARIABLE dDesde AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta AS DATE       NO-UNDO.
  DEFINE VARIABLE fKil   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fKi4   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iTam   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cAnl   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hRep   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hTam   AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  FOR EACH ttTransfer.
    DELETE ttTransfer.
  END.
  
  dDesde = DATE('01/01/' + STRING(piAnio)).
  dHasta = DATE('31/12/2004').

  FOR EACH tambores_industria
      WHERE tambores_industria.fecha        >= dDesde
        AND tambores_industria.fecha        <= dHasta
        AND tambores_industria.id_tipotambor = 3
      BREAK BY tambores_industria.nromov.

    fKil = fKil + tambores_industria.kilos_tambor.
    iTam = iTam + 1.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
    
        /*calculo kilos 400*/
      cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hRep, tambores_industria.id_empresa, 
                                                            tambores_industria.id_sucursal, 
                                                            tambores_industria.id_tipotambor, 
                                                            tambores_industria.nromov).
      IF cAnl = "" OR cAnl = ? OR cAnl = "Aceite" THEN NEXT.
      

      fKi4 = DYNAMIC-FUNCTION('getKilos400' IN hRep, tambores_industria.id_tipotambor,
                                                     tambores_industria.id_articulo,
                                                     tambores_industria.id_calidad,
                                                     fKil).
      
 
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST sucursales OF tambores_industria NO-LOCK NO-ERROR.
  
      CREATE ttTransfer.
      ASSIGN ttTransfer.sucursal            = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NONE"
             ttTransfer.fecha               = tambores_industria.fecha
             ttTransfer.c_fecha             = tambores_industria.c_fecha
             ttTransfer.id_lote             = tambores_industria.id_lote
             ttTransfer.anio                = tambores_industria.anio
             ttTransfer.codigo_lote         = tambores_industria.codigo_lote
             ttTransfer.tambores            = iTam
             ttTransfer.kilos               = fKil
             ttTransfer.contrato            = tambores_industria.id_contrato_of
             ttTransfer.id_articulo         = tambores_industria.id_articulo
             ttTransfer.articulo            = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
             ttTransfer.id_calidad          = tambores_industria.id_calidad
             ttTransfer.calidad             = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"  
             ttTransfer.id_envase           = tambores_industria.id_envase
             ttTransfer.envase              = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE"
             ttTransfer.kilos400            = fKi4
             .

      iTam = 0.
      fKil = 0.
      fKi4 = 0.
    END.
  END.

    RUN generateExcel.p (INPUT TABLE ttTransfer,
                       INPUT " Lotes de Jugo " + STRING(piAnio),
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Arial",
                       INPUT 8).
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferToExcel dTables  _DB-REQUIRED
PROCEDURE transferToExcel :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcFieldList   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER plIncludeObj  AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER plUseExisting AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER piMaxRecords  AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /*RUN SUPER( INPUT pcFieldList, INPUT plIncludeObj, INPUT plUseExisting, INPUT piMaxRecords).*/

  DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hRep AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.


  DEFINE BUFFER buRow FOR rowObject.

  FOR EACH ttTransfer.
    DELETE ttTransfer.
  END.

  FOR EACH buRow
      NO-LOCK.
    /*calculo kilos 400*/
    cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hRep, buRow.id_empresa, 
                                                          buRow.id_sucursal, 
                                                          buRow.id_tipotambor, 
                                                          buRow.nromov).
    IF cAnl = "" OR cAnl = ? OR cAnl = "Aceite" THEN NEXT.
    
    /*dLit = dKil / (DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hTam, DECIMAL(ENTRY(4, cAnl, CHR(1))))).*/
    /*
    dKi4 = DYNAMIC-FUNCTION('getKilos400GPL' IN hTam, DECIMAL(ENTRY(3, cAnl, CHR(1))),
                                                      DECIMAL(ENTRY(2, cAnl, CHR(1))), 
                                                      buRow.litros, 
                                                      FALSE).
    */
    dKi4 = DYNAMIC-FUNCTION('getKilos400' IN hRep, buRow.id_tipotambor,
                                                   buRow.id_articulo,
                                                   buRow.id_calidad,
                                                   buRow.kilos).
    
    ASSIGN buRow.kilos400 = dKi4
           dKi4           = 0
           .


    CREATE ttTransfer.
    ASSIGN ttTransfer.sucursal            = buRow.sucursal
           ttTransfer.fecha               = buRow.fecha
           ttTransfer.fecha_finalizacion  = buRow.fecha_finalizacion
           ttTransfer.c_fecha             = buRow.c_fecha
           ttTransfer.id_lote             = buRow.id_lote
           ttTransfer.anio                = buRow.anio
           ttTransfer.codigo_lote         = buRow.codigo_lote
           ttTransfer.tambores            = buRow.tambores
           ttTransfer.kilos               = buRow.kilos
           ttTransfer.contrato            = buRow.contrato
           ttTransfer.id_articulo         = buRow.id_articulo
           ttTransfer.articulo            = buRow.articulo
           ttTransfer.id_calidad          = buRow.id_calidad
           ttTransfer.calidad             = buRow.calidad  
           ttTransfer.id_envase           = buRow.id_envase
           ttTransfer.envase              = buRow.envase
           ttTransfer.kilos400            = buRow.kilos400
           ttTransfer.orden_fabricacion   = buRow.ordenfabricacion
           .
  END.


  RUN generateExcel.p (INPUT TABLE ttTransfer,
                       INPUT " Lotes de Jugo ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Arial",
                       INPUT 8).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addRow dTables  _DB-REQUIRED
FUNCTION addRow RETURNS CHARACTER
  ( INPUT pcViewColList AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RETURN SUPER( INPUT pcViewColList ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dTotTurbio dTables  _DB-REQUIRED
FUNCTION dTotTurbio RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKilTurbio.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalidad dTables  _DB-REQUIRED
FUNCTION getCalidad RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST calidades WHERE calidades.id_calidad = rowObject.id_calidad
                    NO-LOCK NO-ERROR.
  IF AVAILABLE calidades THEN
    RETURN calidades.descripcion.
  ELSE 
    RETURN "".


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadesOrigen dTables  _DB-REQUIRED
FUNCTION getCantidadesOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino    = rowObject.id_empresa
                                AND tambores_industria.id_sucursal_destino   = rowObject.id_sucursal
                                AND tambores_industria.id_tipotambor_destino = rowObject.id_tipotambor
                                AND tambores_industria.nromov_destino        = rowObject.nromov
                              BREAK BY tambores_industria.id_tipotambor.
    i = i + 1.
    IF LAST-OF(tambores_industria.id_tipotambor) THEN DO:
      cRet = cRet + STRING(tambores_industria.id_tipotambor) + "," + STRING(i) + CHR(10).
      i = 0.
    END.
  END.
  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadTambores dTables  _DB-REQUIRED
FUNCTION getCantidadTambores RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE l    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fCoe AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hRep AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  hTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hRep, rowObject.id_empresa, 
                                                        rowObject.id_sucursal, 
                                                        rowObject.id_tipotambor, 
                                                        rowObject.nromov).
  
  fCoe = (DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hTam, DECIMAL(ENTRY(4, cAnl, CHR(1))))).

  
  
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = rowObject.id_empresa
                                AND tambores_industria.id_sucursal   = rowObject.id_sucursal
                                AND tambores_industria.id_tipotambor = rowObject.id_tipotambor
                                AND tambores_industria.nromov        = rowObject.nromov
                              NO-LOCK.
    i = i + 1.    
    k = k + tambores_industria.kilos_tambor.
    l = l + (tambores_industria.kilos_tambor / fCoe).
  END.

  ASSIGN rowObject.kilos  = k
         rowObject.litros = l.
  

  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlAjuste dTables  _DB-REQUIRED
FUNCTION getControlAjuste RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FOR FIRST registro_post_cierre
      WHERE registro_post_cierre.id_empresa    = rowObject.id_empresa
        AND registro_post_cierre.id_sucursal   = rowObject.id_sucursal
        AND registro_post_cierre.id_tipotambor = rowObject.id_tipotambor
        AND registro_post_cierre.nromov        = rowObject.nromov
      NO-LOCK.
    
    cRet = registro_post_cierre.serial_control.

  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosContrato dTables  _DB-REQUIRED
FUNCTION getDatosContrato RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST tambores_industria
      WHERE tambores_industria.nromov       = rowObject.nromov
        AND tambores_industria.id_contrato <> ?
      NO-LOCK.
    cRet = tambores_industria.id_contrato.
      
  END.
  

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosGraficoProduccion dTables  _DB-REQUIRED
FUNCTION getDatosGraficoProduccion RETURNS CHARACTER
  (piArticulo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buRow FOR rowObject.


  FOR EACH buRow
      WHERE (IF piArticulo <> 0 THEN buRow.id_articulo = piArticulo ELSE TRUE)
      BREAK BY buRow.fecha.
    k = k + buRow.kilos.
    IF LAST-OF(buRow.fecha) THEN DO:
      cRet = cRet + STRING(buRow.fecha) + "," + STRING(k) + CHR(10).
      k    = 0.
    END.
  END.
  
  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST envases_prod WHERE envases_prod.id_envase = rowObject.id_envase
                    NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEstado dTables  _DB-REQUIRED
FUNCTION getEstado RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  cRet = "".

  IF rowObject.CONTROL_calidad THEN
    cRet = "APROBADO".
  ELSE
    cRet = "NO APROBADO".
  
  
  /*
  IF rowObject.estado_lote = 3 THEN 
    cRet = "APROBADO".

  IF rowObject.estado_lote = 4 THEN
    cRet = "DESAPROBADO".

  IF rowObject.estado_lote = 5 THEN 
    cRet = "APROBADO AMBOS".
  */  

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEstadoMicrobiologia dTables  _DB-REQUIRED
FUNCTION getEstadoMicrobiologia RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  cRet = "".

  IF rowObject.microbiologia THEN
    cRet = "APROBADO".
  ELSE
    cRet = "NO APROBADO".

  
  /*
  IF rowObject.estado_lote = 3 THEN 
    cRet = "APROBADO".

  IF rowObject.estado_lote = 5 THEN 
    cRet = "APROBADO AMBOS".

  IF rowObject.estado_lote = 4 THEN
    cRet = "DESAPROBADO".
  */

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGalones dTables  _DB-REQUIRED
FUNCTION getGalones RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT VALID-HANDLE(hLib) THEN RETURN 0.00.
  
  RETURN DYNAMIC-FUNCTION('getGalonesLote' IN hLib, rowObject.id_empresa,
                                                    rowObject.id_sucursal,
                                                    rowObject.id_tipotambor,
                                                    rowObject.nromov).


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos400Lote dTables  _DB-REQUIRED
FUNCTION getKilos400Lote RETURNS DECIMAL
  (plAction AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  IF NOT plAction THEN RETURN 0.00.

  fKil = DYNAMIC-FUNCTION('getKilos400Lote' IN hLib, rowObject.id_empresa,
                                                     rowObject.id_sucursal,
                                                     rowObject.id_tipotambor,
                                                     rowObject.nromov).

  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosLote dTables  _DB-REQUIRED
FUNCTION getKilosLote RETURNS DECIMAL
  (plAction AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  IF NOT plAction THEN RETURN 0.00.

  fKil = DYNAMIC-FUNCTION('getKilosLote' IN hLib, rowObject.id_empresa,
                                                  rowObject.id_sucursal,
                                                  rowObject.id_tipotambor,
                                                  rowObject.nromov).

  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrdenFabricacion dTables  _DB-REQUIRED
FUNCTION getOrdenFabricacion RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  FOR FIRST tambores_industria
      WHERE tambores_industria.nromov       = rowObject.nromov
        AND tambores_industria.id_contrato <> ?
      NO-LOCK.
    FOR FIRST contratos
        WHERE contratos.id_contrato = tambores_industria.id_contrato_of
        NO-LOCK.
        iRet = contratos.orden_fabricacion.
    END.
      
  END.
  

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProducto dTables  _DB-REQUIRED
FUNCTION getProducto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = rowObject.id_articulo
                                  NO-LOCK NO-ERROR.
  IF AVAILABLE productos_terminados THEN
    RETURN productos_terminados.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuimicoId dTables  _DB-REQUIRED
FUNCTION getQuimicoId RETURNS INTEGER
  (INPUT pcUser AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  FIND FIRST quimicos WHERE quimicos.usuario = pcUser NO-LOCK NO-ERROR.
  IF AVAILABLE quimicos THEN DO:
    iRet = quimicos.id_quimico.
  END.
  
  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuimicoMicro dTables  _DB-REQUIRED
FUNCTION getQuimicoMicro RETURNS CHARACTER
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST quimicos WHERE quimicos.id_quimico = rowObject.quimico_microbiologia
                    NO-LOCK NO-ERROR.
  IF AVAILABLE quimicos THEN
    RETURN quimicos.nombre.
  ELSE                                        
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuimicoName dTables  _DB-REQUIRED
FUNCTION getQuimicoName RETURNS CHARACTER
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST quimicos WHERE quimicos.id_quimico = rowObject.quimico_control_calidad
                    NO-LOCK NO-ERROR.
  IF AVAILABLE quimicos THEN
    RETURN quimicos.nombre.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowId dTables  _DB-REQUIRED
FUNCTION getRowId RETURNS ROWID
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.

  RETURN ROWID(rowObject).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_sucursal
                       NO-LOCK NO-ERROR.
  IF AVAILABLE sucursales THEN 
    RETURN sucursales.abreviatura.
  ELSE
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTot400Claro dTables  _DB-REQUIRED
FUNCTION getTot400Claro RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKi4Claro.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTot400Otros dTables  _DB-REQUIRED
FUNCTION getTot400Otros RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKi4Otros.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTot400Pulpa dTables  _DB-REQUIRED
FUNCTION getTot400Pulpa RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKi4Pulpa.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTot400Turbio dTables  _DB-REQUIRED
FUNCTION getTot400Turbio RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKi4Turbio.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotClaro dTables  _DB-REQUIRED
FUNCTION getTotClaro RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKilClaro.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotKilos dTables  _DB-REQUIRED
FUNCTION getTotKilos RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKilClaro + dKilTurbio + dKilPulpa + dKilOtros.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotKilos400 dTables  _DB-REQUIRED
FUNCTION getTotKilos400 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKi4Claro + dKi4Turbio + dKi4Pulpa + dKi4Otros.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotOtros dTables  _DB-REQUIRED
FUNCTION getTotOtros RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKilOtros.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotPulpa dTables  _DB-REQUIRED
FUNCTION getTotPulpa RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKilPulpa.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotTurbio dTables  _DB-REQUIRED
FUNCTION getTotTurbio RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN dKilTurbio.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

