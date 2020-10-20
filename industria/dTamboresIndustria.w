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
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.


{adm2/support/customColors.i}

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
&Scoped-define INTERNAL-TABLES tambores_industria

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Anio anio_of citral c_fecha c_fecha_rep c_hora c_hora_rep c_usuario~
 c_usuario_rep estado Fecha Fecha_cierre galones_tambor id_articulo~
 id_calidad id_contrato_of id_empresa id_empresa_destino~
 id_empresa_ubicacion id_envase id_envio_of id_etiqueta~
 id_locacion_ubicacion id_lote id_lote_deposito id_lote_nuevo~
 id_orden_entrega id_posicion_ubicacion id_proveedor id_release_delivery~
 id_sucursal id_sucursal_destino id_sucursal_remito id_sucursal_ubicacion~
 id_tambor id_tipocontrato_of id_tipotambor id_tipotambor_destino~
 id_tipo_movsto indice_tambor item_factura item_oe item_of~
 item_release_delivery kilos_agregados kilos_tambor kilos_tambor_old Nro~
 nromov nromov_destino nro_remito tara id_estado fecha_reproceso~
 nro_precinto id_camara id_empresa_camara id_sucursal_camara~
 nro_columna_camara nro_fila_camara id_tipo_precinto codigo_lote~
 id_devolucion anio_devolucion origen_water
&Scoped-define ENABLED-FIELDS-IN-tambores_industria Anio anio_of citral ~
c_fecha c_fecha_rep c_hora c_hora_rep c_usuario c_usuario_rep estado Fecha ~
Fecha_cierre galones_tambor id_articulo id_calidad id_contrato_of ~
id_empresa id_empresa_destino id_empresa_ubicacion id_envase id_envio_of ~
id_etiqueta id_locacion_ubicacion id_lote id_lote_deposito id_lote_nuevo ~
id_orden_entrega id_posicion_ubicacion id_proveedor id_release_delivery ~
id_sucursal id_sucursal_destino id_sucursal_remito id_sucursal_ubicacion ~
id_tambor id_tipocontrato_of id_tipotambor id_tipotambor_destino ~
id_tipo_movsto indice_tambor item_factura item_oe item_of ~
item_release_delivery kilos_agregados kilos_tambor kilos_tambor_old Nro ~
nromov nromov_destino nro_remito tara id_estado fecha_reproceso ~
nro_precinto id_camara id_empresa_camara id_sucursal_camara ~
nro_columna_camara nro_fila_camara id_tipo_precinto codigo_lote ~
id_devolucion anio_devolucion origen_water 
&Scoped-Define DATA-FIELDS  Anio EstadoTambor anio_of citral c_fecha c_fecha_rep c_hora c_hora_rep~
 SucursalLote c_usuario c_usuario_rep estado Fecha Fecha_cierre~
 galones_tambor id_articulo id_calidad Ubicacion id_contrato_of id_empresa~
 id_empresa_destino id_empresa_ubicacion id_envase id_envio_of id_etiqueta~
 id_locacion_ubicacion Calidad id_lote id_lote_deposito id_lote_nuevo~
 id_orden_entrega id_posicion_ubicacion id_proveedor id_release_delivery~
 id_sucursal Envase id_sucursal_destino id_sucursal_remito~
 id_sucursal_ubicacion id_tambor id_tipocontrato_of id_tipotambor~
 id_tipotambor_destino id_tipo_movsto indice_tambor item_factura item_oe~
 item_of item_release_delivery kilos_agregados kilos_tambor kilos_tambor_old~
 Nro nromov nromov_destino nro_remito tara id_estado fecha_reproceso~
 nro_precinto id_camara id_empresa_camara id_sucursal_camara~
 nro_columna_camara nro_fila_camara id_tipo_precinto codigo_lote~
 id_devolucion anio_devolucion origen_water Articulo Litros TipoPrecinto
&Scoped-define DATA-FIELDS-IN-tambores_industria Anio anio_of citral ~
c_fecha c_fecha_rep c_hora c_hora_rep c_usuario c_usuario_rep estado Fecha ~
Fecha_cierre galones_tambor id_articulo id_calidad id_contrato_of ~
id_empresa id_empresa_destino id_empresa_ubicacion id_envase id_envio_of ~
id_etiqueta id_locacion_ubicacion id_lote id_lote_deposito id_lote_nuevo ~
id_orden_entrega id_posicion_ubicacion id_proveedor id_release_delivery ~
id_sucursal id_sucursal_destino id_sucursal_remito id_sucursal_ubicacion ~
id_tambor id_tipocontrato_of id_tipotambor id_tipotambor_destino ~
id_tipo_movsto indice_tambor item_factura item_oe item_of ~
item_release_delivery kilos_agregados kilos_tambor kilos_tambor_old Nro ~
nromov nromov_destino nro_remito tara id_estado fecha_reproceso ~
nro_precinto id_camara id_empresa_camara id_sucursal_camara ~
nro_columna_camara nro_fila_camara id_tipo_precinto codigo_lote ~
id_devolucion anio_devolucion origen_water 
&Scoped-Define MANDATORY-FIELDS  id_calidad id_empresa_destino id_lote id_sucursal id_sucursal_destino~
 id_sucursal_ubicacion id_tambor id_camara id_empresa_camara~
 id_sucursal_camara
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dtamboresindustria.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH tambores_industria NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH tambores_industria NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main tambores_industria
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main tambores_industria


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArticulo dTables  _DB-REQUIRED
FUNCTION getArticulo RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadKilos dTables  _DB-REQUIRED
FUNCTION getCantidadKilos RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDistribCamara dTables  _DB-REQUIRED
FUNCTION getDistribCamara RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEstadoTambor dTables  _DB-REQUIRED
FUNCTION getEstadoTambor RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLitros dTables  _DB-REQUIRED
FUNCTION getLitros RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getResumenTambores dTables  _DB-REQUIRED
FUNCTION getResumenTambores RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowsNumber dTables  _DB-REQUIRED
FUNCTION getRowsNumber RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedRowIds dTables  _DB-REQUIRED
FUNCTION getSelectedRowIds RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER)  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursalUbicacion dTables  _DB-REQUIRED
FUNCTION getSucursalUbicacion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTipoPrecinto dTables  _DB-REQUIRED
FUNCTION getTipoPrecinto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getViewerHandle dTables  _DB-REQUIRED
FUNCTION getViewerHandle RETURNS HANDLE
  (INPUT pcViewerName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      tambores_industria SCROLLING.
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
     _TblList          = "general.tambores_industria"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.tambores_industria.Anio
"Anio" "Anio" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[2]   > "_<CALC>"
"getEstadoTambor()" "EstadoTambor" "EstadoTambor" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[3]   > general.tambores_industria.anio_of
"anio_of" "anio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[4]   > general.tambores_industria.citral
"citral" "citral" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[5]   > general.tambores_industria.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[6]   > general.tambores_industria.c_fecha_rep
"c_fecha_rep" "c_fecha_rep" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[7]   > general.tambores_industria.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[8]   > general.tambores_industria.c_hora_rep
"c_hora_rep" "c_hora_rep" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[9]   > "_<CALC>"
"getSucursal()" "SucursalLote" "SucursalLote" "x(10)" "character" ? ? ? ? ? ? no ? no 12.4 no
     _FldNameList[10]   > general.tambores_industria.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[11]   > general.tambores_industria.c_usuario_rep
"c_usuario_rep" "c_usuario_rep" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[12]   > general.tambores_industria.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[13]   > general.tambores_industria.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[14]   > general.tambores_industria.Fecha_cierre
"Fecha_cierre" "Fecha_cierre" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[15]   > general.tambores_industria.galones_tambor
"galones_tambor" "galones_tambor" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[16]   > general.tambores_industria.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[17]   > general.tambores_industria.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 7 yes
     _FldNameList[18]   > "_<CALC>"
"getSucursalUbicacion()" "Ubicacion" "Ubicacion" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no
     _FldNameList[19]   > general.tambores_industria.id_contrato_of
"id_contrato_of" "id_contrato_of" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[20]   > general.tambores_industria.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[21]   > general.tambores_industria.id_empresa_destino
"id_empresa_destino" "id_empresa_destino" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.2 yes
     _FldNameList[22]   > general.tambores_industria.id_empresa_ubicacion
"id_empresa_ubicacion" "id_empresa_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[23]   > general.tambores_industria.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[24]   > general.tambores_industria.id_envio_of
"id_envio_of" "id_envio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[25]   > general.tambores_industria.id_etiqueta
"id_etiqueta" "id_etiqueta" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[26]   > general.tambores_industria.id_locacion_ubicacion
"id_locacion_ubicacion" "id_locacion_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[27]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[28]   > general.tambores_industria.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[29]   > general.tambores_industria.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[30]   > general.tambores_industria.id_lote_nuevo
"id_lote_nuevo" "id_lote_nuevo" ? ? "character" ? ? ? ? ? ? yes ? no 11.2 yes
     _FldNameList[31]   > general.tambores_industria.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[32]   > general.tambores_industria.id_posicion_ubicacion
"id_posicion_ubicacion" "id_posicion_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[33]   > general.tambores_industria.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[34]   > general.tambores_industria.id_release_delivery
"id_release_delivery" "id_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[35]   > general.tambores_industria.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.6 yes
     _FldNameList[36]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[37]   > general.tambores_industria.id_sucursal_destino
"id_sucursal_destino" "id_sucursal_destino" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.2 yes
     _FldNameList[38]   > general.tambores_industria.id_sucursal_remito
"id_sucursal_remito" "id_sucursal_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[39]   > general.tambores_industria.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.2 yes
     _FldNameList[40]   > general.tambores_industria.id_tambor
"id_tambor" "id_tambor" ? ? "integer" ? ? ? ? ? ? yes ? yes 12 yes
     _FldNameList[41]   > general.tambores_industria.id_tipocontrato_of
"id_tipocontrato_of" "id_tipocontrato_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[42]   > general.tambores_industria.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[43]   > general.tambores_industria.id_tipotambor_destino
"id_tipotambor_destino" "id_tipotambor_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[44]   > general.tambores_industria.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[45]   > general.tambores_industria.indice_tambor
"indice_tambor" "indice_tambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[46]   > general.tambores_industria.item_factura
"item_factura" "item_factura" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[47]   > general.tambores_industria.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[48]   > general.tambores_industria.item_of
"item_of" "item_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[49]   > general.tambores_industria.item_release_delivery
"item_release_delivery" "item_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 20.2 yes
     _FldNameList[50]   > general.tambores_industria.kilos_agregados
"kilos_agregados" "kilos_agregados" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[51]   > general.tambores_industria.kilos_tambor
"kilos_tambor" "kilos_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[52]   > general.tambores_industria.kilos_tambor_old
"kilos_tambor_old" "kilos_tambor_old" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[53]   > general.tambores_industria.Nro
"Nro" "Nro" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[54]   > general.tambores_industria.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[55]   > general.tambores_industria.nromov_destino
"nromov_destino" "nromov_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[56]   > general.tambores_industria.nro_remito
"nro_remito" "nro_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[57]   > general.tambores_industria.tara
"tara" "tara" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[58]   > general.tambores_industria.id_estado
"id_estado" "id_estado" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[59]   > general.tambores_industria.fecha_reproceso
"fecha_reproceso" "fecha_reproceso" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[60]   > general.tambores_industria.nro_precinto
"nro_precinto" "nro_precinto" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[61]   > general.tambores_industria.id_camara
"id_camara" "id_camara" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[62]   > general.tambores_industria.id_empresa_camara
"id_empresa_camara" "id_empresa_camara" ? ? "integer" ? ? ? ? ? ? yes ? yes 19 yes
     _FldNameList[63]   > general.tambores_industria.id_sucursal_camara
"id_sucursal_camara" "id_sucursal_camara" ? ? "integer" ? ? ? ? ? ? yes ? yes 18.8 yes
     _FldNameList[64]   > general.tambores_industria.nro_columna_camara
"nro_columna_camara" "nro_columna_camara" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[65]   > general.tambores_industria.nro_fila_camara
"nro_fila_camara" "nro_fila_camara" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[66]   > general.tambores_industria.id_tipo_precinto
"id_tipo_precinto" "id_tipo_precinto" ? ? "integer" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[67]   > general.tambores_industria.codigo_lote
"codigo_lote" "codigo_lote" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[68]   > general.tambores_industria.id_devolucion
"id_devolucion" "id_devolucion" ? ? "integer" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[69]   > general.tambores_industria.anio_devolucion
"anio_devolucion" "anio_devolucion" ? ? "integer" ? ? ? ? ? ? yes ? no 15.6 yes
     _FldNameList[70]   > general.tambores_industria.origen_water
"origen_water" "origen_water" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[71]   > "_<CALC>"
"getArticulo()" "Articulo" "Articulo" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[72]   > "_<CALC>"
"getLitros()" "Litros" "Litros" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[73]   > "_<CALC>"
"getTipoPrecinto()" "TipoPrecinto" "TipoPrecinto" "x(10)" "character" ? ? ? ? ? ? no ? no 12 no
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

FIND LAST RowObjUpd NO-ERROR.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A".
  RUN createTamborCero.  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTamborCero dTables  _DB-REQUIRED
PROCEDURE createTamborCero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCont   AS HANDLE     NO-UNDO.

  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.

  
  {get ContainerSource hCont}.

  /*para pantalla de consulta jugo*/
  IF VALID-HANDLE(hCont) AND (hCont:FILE-NAME MATCHES "*fTamboresProduccionJugo.w" ) THEN DO:
    cKey = DYNAMIC-FUNCTION('getKeyProduccion' IN hCont).
    ASSIGN iEmp = INTEGER(ENTRY(1, cKey))
           iSuc = INTEGER(ENTRY(2, cKey))
           iTip = INTEGER(ENTRY(3, cKey))
           iNro = INTEGER(ENTRY(4, cKey)).
  END.
  ELSE DO:
    hSource = DYNAMIC-FUNCTION('getDataSource').
    ASSIGN iEmp = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_empresa')
           iSuc = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal')
           iTip = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_tipotambor')
           iNro = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nromov').
  END.


    
  /*esto esta horrible lo hago para poder usar el esquema de transacciones del sdo*/
  ASSIGN rowObjUpd.id_empresa    = iEmp
         rowObjUpd.id_sucursal   = iSuc
         rowObjUpd.id_tipotambor = iTip
         rowObjUpd.nromov        = iNro
         rowObjUpd.id_tambor     = 0
         rowObjUpd.id_etiqueta   = IF rowObjUpd.id_sucursal = 96 THEN NEXT-VALUE(tambores)
                                                                 ELSE NEXT-VALUE(tambores_famailla).
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_empresa,id_sucursal,id_tipotambor,nromov,id_etiqueta".
           

  

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
         rowObject.Articulo = (getArticulo())
         rowObject.Calidad = (getCalidad())
         rowObject.Envase = (getEnvase())
         rowObject.EstadoTambor = (getEstadoTambor())
         rowObject.Litros = (getLitros())
         rowObject.SucursalLote = (getSucursal())
         rowObject.TipoPrecinto = (getTipoPrecinto())
         rowObject.Ubicacion = (getSucursalUbicacion())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_envaseValidate dTables  _DB-REQUIRED
PROCEDURE id_envaseValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF pcValor = "0" THEN
    RETURN "Debe ingresar un valor para envase".
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
  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  DEFINE VARIABLE cSort AS CHARACTER  NO-UNDO.
  cSort = "BY anio BY id_lote BY id_tambor".
  DYNAMIC-FUNCTION('setQuerySort', cSort).

  
  
  
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kilos_tamborValidate dTables  _DB-REQUIRED
PROCEDURE kilos_tamborValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  IF DECIMAL(pcValor) = 0 THEN
    RETURN "Debe Ingersar un valor para Kilos".
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
  DEFINE VARIABLE hCont   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  
  FIND LAST RowObjUpd NO-ERROR.
  
  {get ContainerSource hCont}.

  IF VALID-HANDLE(hCont) AND (hCont:FILE-NAME MATCHES "*fTamboresProduccionJugo.w" ) THEN DO:
    RUN refreshSource IN hCont.
  END.
  ELSE DO:
    FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A".
      hSource = DYNAMIC-FUNCTION('getDataSource').
      RUN refreshRow IN hSource.
    END.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDevolucionTambor dTables  _DB-REQUIRED
PROCEDURE setDevolucionTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcRows   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piDevol  AS INTEGER    NO-UNDO.  
  DEFINE INPUT  PARAMETER piAnioDe AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piUbic   AS INTEGER    NO-UNDO. /*ubicacion en la que se encontraba el lote antes de ser devuelto*/
  DEFINE INPUT  PARAMETER plAction AS LOGICAL    NO-UNDO. /*true marca como devuelto false retorna tambor*/ 

  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iUbi AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRows   AS CHARACTER  NO-UNDO.
  
  cRows = pcRows.

  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow', INTEGER(ENTRY(iRow,cRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,id_tambor,id_sucursal_ubicacion,id_lote,anio,id_articulo').
    
    ASSIGN iEmp = INTEGER(ENTRY(2, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(3, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(4, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(5, cFields, CHR(1)))
           iTbo = INTEGER(ENTRY(6, cFields, CHR(1)))
           iUbi = INTEGER(ENTRY(7, cFields, CHR(1)))
           iLot = INTEGER(ENTRY(8, cFields, CHR(1)))
           iAno = INTEGER(ENTRY(9, cFields, CHR(1)))
           iArt = INTEGER(ENTRY(10, cFields, CHR(1)))
           .

    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".

    IF plAction THEN
      RUN setEstadoDevolucionTambor IN hLib (iEmp, iSuc, iTip, iNro, iTbo, piDevol, piAnioDe, iSuc).
    ELSE
      RUN setEstadoDevolucionTambor IN hLib (iEmp, iSuc, iTip, iNro, iTbo, 0, 0, piUbic) .
  END.

  
  /*actualizo info de lote en lote_devolucion*/
  
  FOR FIRST lote_devolucion 
    WHERE lote_devolucion.id_devolucion   = piDevol
      AND lote_devolucion.anio_devolucion = piAnioDe.

    IF plAction THEN
      ASSIGN lote_devolucion.id_sucursal_ubicacion = piUbic
             lote_devolucion.id_empresa            = iEmp
             lote_devolucion.id_sucursal           = iSuc
             lote_devolucion.id_tipotambor         = iTip
             lote_devolucion.nromov                = iNro
             lote_devolucion.id_lote               = iLot
             lote_devolucion.anio                  = iAno
             lote_devolucion.id_articulo           = iArt
             .
    /*
    ELSE     
      ASSIGN lote_devolucion.id_sucursal_ubicacion = 0
             lote_devolucion.id_empresa            = 0
             lote_devolucion.id_sucursal           = 0
             lote_devolucion.id_tipotambor         = 0
             lote_devolucion.nromov                = 0
             lote_devolucion.id_lote               = 0
             lote_devolucion.anio                  = 0
             lote_devolucion.id_articulo           = 0
             .
   */

  END.

  /*actualizo lotes_ubicacion*/
  RUN updateLoteUbicacion IN hLib (iEmp, iSuc, iTip, iNro, iSuc).

  /*rutina de cacha?*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEstado dTables  _DB-REQUIRED
PROCEDURE setEstado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcRows   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piEstado AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRows   AS CHARACTER  NO-UNDO.
  
  cRows = pcRows.

  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow', INTEGER(ENTRY(iRow,cRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,id_tambor').
    
    ASSIGN iEmp = INTEGER(ENTRY(2, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(3, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(4, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(5, cFields, CHR(1)))
           iTbo = INTEGER(ENTRY(6, cFields, CHR(1))).

    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".
    
    RUN setEstadoTambor IN hLib (iEmp, iSuc, iTip, iNro, iTbo, piEstado) .
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNroPrecinto dTables  _DB-REQUIRED
PROCEDURE setNroPrecinto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcPre AS CHARACTER  NO-UNDO.

  RUN setNroPrecinto IN hLib (piEmp,
                              piSuc,
                              piTip,
                              piNro,
                              piDes,
                              piHas,
                              pcPre).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelection dTables  _DB-REQUIRED
PROCEDURE setSelection :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER plSelect AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE iSelect AS INTEGER    NO-UNDO.

  IF plSelect THEN 
    iSelect = 1.
  ELSE
    iSelect = 0.

  FIND CURRENT rowObject NO-ERROR.

  FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = rowObject.id_empresa
                                  AND tambores_industria.id_sucursal   = rowObject.id_sucursal 
                                  AND tambores_industria.id_tipotambor = rowObject.id_tipotambor
                                  AND tambores_industria.nromov        = rowObject.nromov
                                  AND tambores_industria.id_tambor     = rowObject.id_tambor
                                NO-ERROR.
  IF AVAILABLE tambores_industria THEN DO:
    ASSIGN tambores_industria.nro = iSelect.
  END.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTipoPrecinto dTables  _DB-REQUIRED
PROCEDURE setTipoPrecinto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTPr AS INTEGER    NO-UNDO.

  RUN setTipoPrecinto IN hLib (piEmp,
                               piSuc,
                               piTip,
                               piNro,
                               piDes,
                               piHas,
                               piTpr).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArticulo dTables  _DB-REQUIRED
FUNCTION getArticulo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST productos_terminados WHERE productos_terminados.id_articulo = rowObject.id_articulo NO-LOCK.
    cRet = productos_terminados.descripcion.
  END.
  
  RETURN cRet.


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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST calidades WHERE calidades.id_calidad = rowObject.id_calidad NO-LOCK.
    cRet = calidades.descripcion.
  END.
  
  RETURN cRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadKilos dTables  _DB-REQUIRED
FUNCTION getCantidadKilos RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve la suma de los kilos que tiene el rowObject
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buRowObject FOR rowObject.
  
  DEFINE VARIABLE i    AS INTEGER INITIAL 0   NO-UNDO.
  DEFINE VARIABLE k    AS INTEGER INITIAL 0   NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR EACH buRowObject NO-LOCK.
    i = i + 1.
    k = k + buRowObject.kilos_tambor.
  END.

  cRet = STRING(i) + "," + STRING(k).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDistribCamara dTables  _DB-REQUIRED
FUNCTION getDistribCamara RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/  
  DEFINE VARIABLE hQry AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLoc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAno AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTbs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCam AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lIn  AS LOGICAL    NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.
  
  cQry = DYNAMIC-FUNCTION('getQueryWhere').
  cQry = REPLACE(cQry, "BY tambores_industria.id_lote", "BY tambores_industria.id_camara BY tambores_industria.nro_fila_camara BY tambores_industria.nro_columna_camara BY tambores_industria.nromov BY tambores_industria.id_lote").
  cQry = REPLACE(cQry, "tambores_industria", "buTam").
  
  CREATE QUERY hQry.  
  hQry:SET-BUFFERS(BUFFER buTam:HANDLE).
  hQry:QUERY-PREPARE(cQry).
  hQry:QUERY-OPEN().
  hQry:GET-FIRST().  
  IF hQry:QUERY-OFF-END THEN LEAVE.

  iNro = buTam.nromov.
  cLoc = STRING(buTam.id_camara) + buTam.nro_fila_camara + buTam.nro_columna_camara + STRING(buTam.nromov)  .

  

  REPEAT:      
    i = i + 1.
    hQry:GET-NEXT().  
    IF hQry:QUERY-OFF-END THEN LEAVE.    
  
    
    IF cLoc <> STRING(buTam.id_camara) + buTam.nro_fila_camara + buTam.nro_columna_camara + STRING(buTam.nromov) THEN DO:      
       
      cRet = cRet + cLot      + CHR(1) + 
                    cAno      + CHR(1) + 
                    cArt      + CHR(1) + 
                    cCal      + CHR(1) + 
                    cEnv      + CHR(1) + 
                    cFil      + CHR(1) + 
                    cCol      + CHR(1) +
                    STRING(i) + CHR(1) + 
                    cNro      + CHR(1) + 
                    cCam      + CHR(1) +  /*aqui va la descripcion de la camara*/
                    cSuc      + CHR(1) + 
                    cCam      + CHR(10).          
          
      i = 0.
      lIn = TRUE.
    END.

    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = buTam.id_articulo NO-LOCK NO-ERROR.
    FIND FIRST calidades WHERE calidades.id_calidad = buTam.id_calidad NO-LOCK NO-ERROR.
    FIND FIRST envases_prod WHERE envases_prod.id_envase = buTam.id_envase NO-LOCK NO-ERROR.

    ASSIGN cLot = STRING(buTam.id_lote)
       cAno = STRING(buTam.anio)
       cArt = productos_terminados.abreviatura
       cCal = calidades.descripcion
       cEnv = envases_prod.descripcion
       cFil = buTam.nro_fila_camara
       cCol = buTam.nro_columna_camara
       cNro = STRING(buTam.nromov)
       cCam = STRING(buTam.id_camara)
       cSuc = STRING(buTam.id_sucursal_ubicacion).

    cLoc = STRING(buTam.id_camara) + buTam.nro_fila_camara + buTam.nro_columna_camara + STRING(buTam.nromov).
    /*iNro = buTam.nromov.*/
    /*cLoc = STRING(iNro) + STRING(buTam.id_camara) + buTam.nro_fila_camara + buTam.nro_columna_camara.*/
  END.

  IF NOT lIn AND i > 0 THEN DO: /*solo un lote*/
    cRet = cRet + cLot      + CHR(1) + 
                  cAno      + CHR(1) + 
                  cArt      + CHR(1) + 
                  cCal      + CHR(1) + 
                  cEnv      + CHR(1) + 
                  cFil      + CHR(1) + 
                  cCol      + CHR(1) +
                  STRING(i) + CHR(1) + 
                  cNro      + CHR(1) + 
                  cCam      + CHR(1) +  /*aqui va la descripcion de la camara*/
                  cSuc      + CHR(1) + 
                  cCam      + CHR(10). 
  END.

  

  
  hQry:QUERY-CLOSE().
  DELETE OBJECT hQry.


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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST envases_prod WHERE envases_prod.id_envase = rowObject.id_envase NO-LOCK.
    cRet = envases_prod.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEstadoTambor dTables  _DB-REQUIRED
FUNCTION getEstadoTambor RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.

  FIND FIRST estados_tambor WHERE estados_tambor.id_estado = rowObject.id_estado
                            NO-LOCK NO-ERROR.
  IF AVAILABLE estados_tambor THEN
    RETURN estados_tambor.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLitros dTables  _DB-REQUIRED
FUNCTION getLitros RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  dLit = DYNAMIC-FUNCTION('getLitrosTambor' IN hLib, rowObject.id_empresa, 
                                                     rowObject.id_sucursal,
                                                     rowObject.id_tipotambor,
                                                     rowObject.nromov,
                                                     rowObject.kilos_tambor) NO-ERROR.
  
  RETURN dLit.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getResumenTambores dTables  _DB-REQUIRED
FUNCTION getResumenTambores RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE crlf AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE l    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE k4   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBx  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAc  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCo  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAn  AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE hLibRep AS HANDLE     NO-UNDO.
  RUN libReportes.p PERSISTENT SET hLibRep.

  DEFINE BUFFER ro FOR rowObject.

  crlf = CHR(13) + CHR(10).

  FOR EACH ro BREAK BY ro.nromov.
    ASSIGN i = i + 1
           k = k + ro.kilos_tambor
           l = l + ro.litros.
    IF LAST-OF(ro.nromov) THEN DO:
     
      cAn = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLibRep, ro.id_empresa,
                                                              ro.id_sucursal,
                                                              ro.id_tipotambor,
                                                              ro.nromov).

      
      IF cAn <> "" AND cAn <> ? AND cAn <> "Aceite" THEN DO:
        dBx = DECIMAL(ENTRY(3, cAn, CHR(1))).
        dAc = DECIMAL(ENTRY(2, cAn, CHR(1))).
        k4  = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, dBx, dAc, l, FALSE).
        dCo = DYNAMIC-FUNCTION('getCoefConversion400' IN hLib, ro.id_tipotambor, ro.id_articulo, ro.id_calidad).
        IF ro.id_articulo = 42 THEN DO: /*jugo dulce*/
          k4 = k.
        END.
      END.

      cRet = cRet + STRING(ro.id_lote)        + CHR(1) + 
                    STRING(ro.anio)           + CHR(1) + 
                    ro.articulo               + CHR(1) + 
                    ro.calidad                + CHR(1) + 
                    ro.envase                 + CHR(1) + 
                    STRING(i)                 + CHR(1) + 
                    STRING(k)                 + CHR(1) + 
                    STRING(l)                 + CHR(1) + 
                    ro.ubicacion              + CHR(1) +
                    STRING(k4)                + CHR(1) +
                    STRING(dBx)               + CHR(1) + 
                    STRING(dAc)               + CHR(1) + 
                    STRING(dCo)               + CHR(1) +
                    STRING(ro.id_empresa)     + CHR(1) + 
                    STRING(ro.id_sucursal)    + CHR(1) + 
                    STRING(ro.id_tipotambor)  + CHR(1) + 
                    STRING(ro.nromov)         + CHR(10).

      ASSIGN i  = 0
             k  = 0
             l  = 0
             k4 = 0.

    END.
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowsNumber dTables  _DB-REQUIRED
FUNCTION getRowsNumber RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buRowObject FOR rowObject.
  DEFINE VARIABLE i AS INTEGER INITIAL 0   NO-UNDO.

  FOR EACH buRowObject NO-LOCK.
    i = i + 1.
  END.

  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedRowIds dTables  _DB-REQUIRED
FUNCTION getSelectedRowIds RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO. 
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo    AS INTEGER    NO-UNDO.


  DO iRow = 1 TO NUM-ENTRIES(pcRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow', INTEGER(ENTRY(iRow,pcRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,id_tambor').
    ASSIGN iEmp = INTEGER(ENTRY(2, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(3, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(4, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(5, cFields, CHR(1)))
           iTbo = INTEGER(ENTRY(6, cFields, CHR(1))).

    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".
    FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = iEmp
                                    AND tambores_industria.id_sucursal   = iSuc
                                    AND tambores_industria.id_tipotambor = iTip
                                    AND tambores_industria.nromov        = iNro
                                    AND tambores_industria.id_tambor     = iTbo
                                  NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      cRet = cRet + STRING(ROWID(tambores_industria)) + CHR(10).
    END.
  END.
  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1). 
  RETURN cRet.  
   
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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_sucursal NO-LOCK.
    cRet = sucursales.abreviatura.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursalUbicacion dTables  _DB-REQUIRED
FUNCTION getSucursalUbicacion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_sucursal_ubicacion NO-LOCK.
    cRet = sucursales.abreviatura.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTipoPrecinto dTables  _DB-REQUIRED
FUNCTION getTipoPrecinto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST tipo_precinto WHERE tipo_precinto.id_tipo_precinto = rowObject.id_tipo_precinto NO-LOCK.
    cRet = tipo_precinto.descripcion.
  END.
  
  RETURN cRet.
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getViewerHandle dTables  _DB-REQUIRED
FUNCTION getViewerHandle RETURNS HANDLE
  (INPUT pcViewerName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTargets AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hTarget  AS HANDLE      NO-UNDO.
  DEFINE VARIABLE iCounter AS INTEGER    NO-UNDO.
  
  cTargets = DYNAMIC-FUNCTION('getDataTarget').
  
  DO iCounter = 1 TO NUM-ENTRIES(cTargets):
    hTarget = WIDGET-HANDLE(ENTRY(iCounter, cTargets)).
    IF DYNAMIC-FUNCTION('getObjectName' IN hTarget) = pcViewerName THEN LEAVE.
  END.
  
  IF VALID-HANDLE(hTarget) AND DYNAMIC-FUNCTION('getObjectName' IN hTarget) = pcViewerName THEN
    RETURN hTarget.
    /*MESSAGE "hTarget now holds the handle of the 'bCustomer' object" VIEW-AS ALERT-BOX.*/
  ELSE
    RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

