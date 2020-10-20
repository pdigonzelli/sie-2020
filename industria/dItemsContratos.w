&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
&Scoped-define INTERNAL-TABLES items_contratos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_contrato item anio id_tipo_contrato id_articulo id_calidad id_envase~
 semana_entrega anio_semana_entrega adelanto arribo_estimado cajas_pallets~
 calibres cantidad cantidad_pallet cert_fito comision_broker~
 comision_broker_viejo comision_otros comision_otros_viejo consignacion~
 contramarca cotizacion_base cotizacion_origen c_fecha c_hora c_usuario~
 descuento destino_final detalle_clientes embarque_estimado estado fecha~
 id_articulo_cliente1 id_articulo_cliente2 id_articulo_cliente3 id_caract~
 id_categoria id_clausula id_cliente_final id_cond_pago_cliente~
 id_confeccion_final id_deposito_final id_destino id_empacador~
 id_forma_carga id_from id_instrumento_pago id_marca id_medida_pallet~
 id_moneda_base id_moneda_local id_moneda_origen id_po_cliente1~
 id_po_cliente2 id_po_cliente3 id_puerto_ent id_puerto_sal id_tipo_empaque~
 id_tipo_envase id_tipo_esquinero id_tipo_pallet id_tipo_tratamiento~
 id_tipo_unidad_venta id_tipo_unidad_venta_origen id_tipo_venta id_vapor~
 id_variedad interes kilos_cajas marca_tambores numero_release1~
 numero_release2 numero_release3 obleas observaciones obs_comerciales~
 otra_identificacion papel_sulfito pendiente precio_base precio_base_calculo~
 precio_comision precio_local precio_origen precio_venta reutiliza_caja~
 saldo semana_entrega_hasta
&Scoped-define ENABLED-FIELDS-IN-items_contratos id_contrato item anio ~
id_tipo_contrato id_articulo id_calidad id_envase semana_entrega ~
anio_semana_entrega adelanto arribo_estimado cajas_pallets calibres ~
cantidad cantidad_pallet cert_fito comision_broker comision_broker_viejo ~
comision_otros comision_otros_viejo consignacion contramarca ~
cotizacion_base cotizacion_origen c_fecha c_hora c_usuario descuento ~
destino_final detalle_clientes embarque_estimado estado fecha ~
id_articulo_cliente1 id_articulo_cliente2 id_articulo_cliente3 id_caract ~
id_categoria id_clausula id_cliente_final id_cond_pago_cliente ~
id_confeccion_final id_deposito_final id_destino id_empacador ~
id_forma_carga id_from id_instrumento_pago id_marca id_medida_pallet ~
id_moneda_base id_moneda_local id_moneda_origen id_po_cliente1 ~
id_po_cliente2 id_po_cliente3 id_puerto_ent id_puerto_sal id_tipo_empaque ~
id_tipo_envase id_tipo_esquinero id_tipo_pallet id_tipo_tratamiento ~
id_tipo_unidad_venta id_tipo_unidad_venta_origen id_tipo_venta id_vapor ~
id_variedad interes kilos_cajas marca_tambores numero_release1 ~
numero_release2 numero_release3 obleas observaciones obs_comerciales ~
otra_identificacion papel_sulfito pendiente precio_base precio_base_calculo ~
precio_comision precio_local precio_origen precio_venta reutiliza_caja ~
saldo semana_entrega_hasta 
&Scoped-Define DATA-FIELDS  id_contrato OrdenFabricacion Tipo Calidad Envase Articulo item anio~
 id_tipo_contrato id_articulo id_calidad id_envase semana_entrega~
 anio_semana_entrega adelanto arribo_estimado cajas_pallets calibres~
 cantidad cantidad_pallet cert_fito comision_broker comision_broker_viejo~
 comision_otros comision_otros_viejo consignacion contramarca~
 cotizacion_base cotizacion_origen c_fecha c_hora c_usuario descuento~
 destino_final detalle_clientes embarque_estimado estado fecha~
 id_articulo_cliente1 id_articulo_cliente2 id_articulo_cliente3 id_caract~
 id_categoria id_clausula id_cliente_final id_cond_pago_cliente~
 id_confeccion_final id_deposito_final id_destino id_empacador~
 id_forma_carga id_from id_instrumento_pago id_marca id_medida_pallet~
 id_moneda_base id_moneda_local id_moneda_origen id_po_cliente1~
 id_po_cliente2 id_po_cliente3 id_puerto_ent id_puerto_sal id_tipo_empaque~
 id_tipo_envase id_tipo_esquinero id_tipo_pallet id_tipo_tratamiento~
 id_tipo_unidad_venta id_tipo_unidad_venta_origen id_tipo_venta id_vapor~
 id_variedad interes kilos_cajas marca_tambores numero_release1~
 numero_release2 numero_release3 obleas observaciones obs_comerciales~
 otra_identificacion papel_sulfito pendiente precio_base precio_base_calculo~
 precio_comision precio_local precio_origen precio_venta reutiliza_caja~
 saldo semana_entrega_hasta Cumplidos CondVenta Moneda
&Scoped-define DATA-FIELDS-IN-items_contratos id_contrato item anio ~
id_tipo_contrato id_articulo id_calidad id_envase semana_entrega ~
anio_semana_entrega adelanto arribo_estimado cajas_pallets calibres ~
cantidad cantidad_pallet cert_fito comision_broker comision_broker_viejo ~
comision_otros comision_otros_viejo consignacion contramarca ~
cotizacion_base cotizacion_origen c_fecha c_hora c_usuario descuento ~
destino_final detalle_clientes embarque_estimado estado fecha ~
id_articulo_cliente1 id_articulo_cliente2 id_articulo_cliente3 id_caract ~
id_categoria id_clausula id_cliente_final id_cond_pago_cliente ~
id_confeccion_final id_deposito_final id_destino id_empacador ~
id_forma_carga id_from id_instrumento_pago id_marca id_medida_pallet ~
id_moneda_base id_moneda_local id_moneda_origen id_po_cliente1 ~
id_po_cliente2 id_po_cliente3 id_puerto_ent id_puerto_sal id_tipo_empaque ~
id_tipo_envase id_tipo_esquinero id_tipo_pallet id_tipo_tratamiento ~
id_tipo_unidad_venta id_tipo_unidad_venta_origen id_tipo_venta id_vapor ~
id_variedad interes kilos_cajas marca_tambores numero_release1 ~
numero_release2 numero_release3 obleas observaciones obs_comerciales ~
otra_identificacion papel_sulfito pendiente precio_base precio_base_calculo ~
precio_comision precio_local precio_origen precio_venta reutiliza_caja ~
saldo semana_entrega_hasta 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.id_articulo_cliente1 = items_contratos.id_articulo_cliente[1]~
  rowObject.id_articulo_cliente2 = items_contratos.id_articulo_cliente[2]~
  rowObject.id_articulo_cliente3 = items_contratos.id_articulo_cliente[3]~
  rowObject.id_po_cliente1 = items_contratos.id_po_cliente[1]~
  rowObject.id_po_cliente2 = items_contratos.id_po_cliente[2]~
  rowObject.id_po_cliente3 = items_contratos.id_po_cliente[3]~
  rowObject.numero_release1 = items_contratos.numero_release[1]~
  rowObject.numero_release2 = items_contratos.numero_release[2]~
  rowObject.numero_release3 = items_contratos.numero_release[3]
&Scoped-Define DATA-FIELD-DEFS "dItemsContratos.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_contratos NO-LOCK ~
    BY items_contratos.anio ~
       BY items_contratos.item INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_contratos NO-LOCK ~
    BY items_contratos.anio ~
       BY items_contratos.item INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_contratos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_contratos


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCondVta dTables  _DB-REQUIRED
FUNCTION getCondVta RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMoneda dTables  _DB-REQUIRED
FUNCTION getMoneda RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOF dTables  _DB-REQUIRED
FUNCTION getOF RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTbsAsoc dTables  _DB-REQUIRED
FUNCTION getTbsAsoc RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTipoContrato dTables  _DB-REQUIRED
FUNCTION getTipoContrato RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD openQuery dTables  _DB-REQUIRED
FUNCTION openQuery RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_contratos
    FIELDS(items_contratos.id_contrato
      items_contratos.item
      items_contratos.anio
      items_contratos.id_tipo_contrato
      items_contratos.id_articulo
      items_contratos.id_calidad
      items_contratos.id_envase
      items_contratos.semana_entrega
      items_contratos.anio_semana_entrega
      items_contratos.adelanto
      items_contratos.arribo_estimado
      items_contratos.cajas_pallets
      items_contratos.calibres
      items_contratos.cantidad
      items_contratos.cantidad_pallet
      items_contratos.cert_fito
      items_contratos.comision_broker
      items_contratos.comision_broker_viejo
      items_contratos.comision_otros
      items_contratos.comision_otros_viejo
      items_contratos.consignacion
      items_contratos.contramarca
      items_contratos.cotizacion_base
      items_contratos.cotizacion_origen
      items_contratos.c_fecha
      items_contratos.c_hora
      items_contratos.c_usuario
      items_contratos.descuento
      items_contratos.destino_final
      items_contratos.detalle_clientes
      items_contratos.embarque_estimado
      items_contratos.estado
      items_contratos.fecha
      items_contratos.id_articulo_cliente[1]
      items_contratos.id_articulo_cliente[2]
      items_contratos.id_articulo_cliente[3]
      items_contratos.id_caract
      items_contratos.id_categoria
      items_contratos.id_clausula
      items_contratos.id_cliente_final
      items_contratos.id_cond_pago_cliente
      items_contratos.id_confeccion_final
      items_contratos.id_deposito_final
      items_contratos.id_destino
      items_contratos.id_empacador
      items_contratos.id_forma_carga
      items_contratos.id_from
      items_contratos.id_instrumento_pago
      items_contratos.id_marca
      items_contratos.id_medida_pallet
      items_contratos.id_moneda_base
      items_contratos.id_moneda_local
      items_contratos.id_moneda_origen
      items_contratos.id_po_cliente[1]
      items_contratos.id_po_cliente[2]
      items_contratos.id_po_cliente[3]
      items_contratos.id_puerto_ent
      items_contratos.id_puerto_sal
      items_contratos.id_tipo_empaque
      items_contratos.id_tipo_envase
      items_contratos.id_tipo_esquinero
      items_contratos.id_tipo_pallet
      items_contratos.id_tipo_tratamiento
      items_contratos.id_tipo_unidad_venta
      items_contratos.id_tipo_unidad_venta_origen
      items_contratos.id_tipo_venta
      items_contratos.id_vapor
      items_contratos.id_variedad
      items_contratos.interes
      items_contratos.kilos_cajas
      items_contratos.marca_tambores
      items_contratos.numero_release[1]
      items_contratos.numero_release[2]
      items_contratos.numero_release[3]
      items_contratos.obleas
      items_contratos.observaciones
      items_contratos.obs_comerciales
      items_contratos.otra_identificacion
      items_contratos.papel_sulfito
      items_contratos.pendiente
      items_contratos.precio_base
      items_contratos.precio_base_calculo
      items_contratos.precio_comision
      items_contratos.precio_local
      items_contratos.precio_origen
      items_contratos.precio_venta
      items_contratos.reutiliza_caja
      items_contratos.saldo
      items_contratos.semana_entrega_hasta) SCROLLING.
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
     _TblList          = "general.items_contratos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = "USED, USED,,"
     _OrdList          = "general.items_contratos.anio|yes,general.items_contratos.item|yes"
     _FldNameList[1]   > general.items_contratos.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[2]   > "_<CALC>"
"getOF()" "OrdenFabricacion" "OrdenFabricacion" "x(5)" "character" ? ? ? ? ? ? no ? no 16.4 no
     _FldNameList[3]   > "_<CALC>"
"getTipoContrato()" "Tipo" "Tipo" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[4]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[5]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[6]   > "_<CALC>"
"getArticulo()" "Articulo" "Articulo" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[7]   > general.items_contratos.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[8]   > general.items_contratos.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[9]   > general.items_contratos.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[10]   > general.items_contratos.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[11]   > general.items_contratos.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[12]   > general.items_contratos.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[13]   > general.items_contratos.semana_entrega
"semana_entrega" "semana_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 18.6 yes
     _FldNameList[14]   > general.items_contratos.anio_semana_entrega
"anio_semana_entrega" "anio_semana_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[15]   > general.items_contratos.adelanto
"adelanto" "adelanto" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes
     _FldNameList[16]   > general.items_contratos.arribo_estimado
"arribo_estimado" "arribo_estimado" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[17]   > general.items_contratos.cajas_pallets
"cajas_pallets" "cajas_pallets" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.6 yes
     _FldNameList[18]   > general.items_contratos.calibres
"calibres" "calibres" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[19]   > general.items_contratos.cantidad
"cantidad" "cantidad" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes
     _FldNameList[20]   > general.items_contratos.cantidad_pallet
"cantidad_pallet" "cantidad_pallet" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.2 yes
     _FldNameList[21]   > general.items_contratos.cert_fito
"cert_fito" "cert_fito" ? ? "logical" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[22]   > general.items_contratos.comision_broker
"comision_broker" "comision_broker" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[23]   > general.items_contratos.comision_broker_viejo
"comision_broker_viejo" "comision_broker_viejo" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[24]   > general.items_contratos.comision_otros
"comision_otros" "comision_otros" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[25]   > general.items_contratos.comision_otros_viejo
"comision_otros_viejo" "comision_otros_viejo" ? ? "integer" ? ? ? ? ? ? yes ? no 14 yes
     _FldNameList[26]   > general.items_contratos.consignacion
"consignacion" "consignacion" ? ? "logical" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[27]   > general.items_contratos.contramarca
"contramarca" "contramarca" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[28]   > general.items_contratos.cotizacion_base
"cotizacion_base" "cotizacion_base" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[29]   > general.items_contratos.cotizacion_origen
"cotizacion_origen" "cotizacion_origen" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[30]   > general.items_contratos.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[31]   > general.items_contratos.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[32]   > general.items_contratos.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[33]   > general.items_contratos.descuento
"descuento" "descuento" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes
     _FldNameList[34]   > general.items_contratos.destino_final
"destino_final" "destino_final" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[35]   > general.items_contratos.detalle_clientes
"detalle_clientes" "detalle_clientes" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[36]   > general.items_contratos.embarque_estimado
"embarque_estimado" "embarque_estimado" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[37]   > general.items_contratos.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[38]   > general.items_contratos.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[39]   > general.items_contratos.id_articulo_cliente[1]
"id_articulo_cliente[1]" "id_articulo_cliente1" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[40]   > general.items_contratos.id_articulo_cliente[2]
"id_articulo_cliente[2]" "id_articulo_cliente2" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[41]   > general.items_contratos.id_articulo_cliente[3]
"id_articulo_cliente[3]" "id_articulo_cliente3" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[42]   > general.items_contratos.id_caract
"id_caract" "id_caract" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes
     _FldNameList[43]   > general.items_contratos.id_categoria
"id_categoria" "id_categoria" ? ? "integer" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[44]   > general.items_contratos.id_clausula
"id_clausula" "id_clausula" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[45]   > general.items_contratos.id_cliente_final
"id_cliente_final" "id_cliente_final" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[46]   > general.items_contratos.id_cond_pago_cliente
"id_cond_pago_cliente" "id_cond_pago_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[47]   > general.items_contratos.id_confeccion_final
"id_confeccion_final" "id_confeccion_final" ? ? "integer" ? ? ? ? ? ? yes ? no 15.8 yes
     _FldNameList[48]   > general.items_contratos.id_deposito_final
"id_deposito_final" "id_deposito_final" ? ? "integer" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[49]   > general.items_contratos.id_destino
"id_destino" "id_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[50]   > general.items_contratos.id_empacador
"id_empacador" "id_empacador" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[51]   > general.items_contratos.id_forma_carga
"id_forma_carga" "id_forma_carga" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[52]   > general.items_contratos.id_from
"id_from" "id_from" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[53]   > general.items_contratos.id_instrumento_pago
"id_instrumento_pago" "id_instrumento_pago" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[54]   > general.items_contratos.id_marca
"id_marca" "id_marca" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes
     _FldNameList[55]   > general.items_contratos.id_medida_pallet
"id_medida_pallet" "id_medida_pallet" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[56]   > general.items_contratos.id_moneda_base
"id_moneda_base" "id_moneda_base" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[57]   > general.items_contratos.id_moneda_local
"id_moneda_local" "id_moneda_local" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[58]   > general.items_contratos.id_moneda_origen
"id_moneda_origen" "id_moneda_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[59]   > general.items_contratos.id_po_cliente[1]
"id_po_cliente[1]" "id_po_cliente1" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[60]   > general.items_contratos.id_po_cliente[2]
"id_po_cliente[2]" "id_po_cliente2" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[61]   > general.items_contratos.id_po_cliente[3]
"id_po_cliente[3]" "id_po_cliente3" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[62]   > general.items_contratos.id_puerto_ent
"id_puerto_ent" "id_puerto_ent" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[63]   > general.items_contratos.id_puerto_sal
"id_puerto_sal" "id_puerto_sal" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[64]   > general.items_contratos.id_tipo_empaque
"id_tipo_empaque" "id_tipo_empaque" ? ? "integer" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[65]   > general.items_contratos.id_tipo_envase
"id_tipo_envase" "id_tipo_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[66]   > general.items_contratos.id_tipo_esquinero
"id_tipo_esquinero" "id_tipo_esquinero" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[67]   > general.items_contratos.id_tipo_pallet
"id_tipo_pallet" "id_tipo_pallet" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[68]   > general.items_contratos.id_tipo_tratamiento
"id_tipo_tratamiento" "id_tipo_tratamiento" ? ? "integer" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[69]   > general.items_contratos.id_tipo_unidad_venta
"id_tipo_unidad_venta" "id_tipo_unidad_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[70]   > general.items_contratos.id_tipo_unidad_venta_origen
"id_tipo_unidad_venta_origen" "id_tipo_unidad_venta_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[71]   > general.items_contratos.id_tipo_venta
"id_tipo_venta" "id_tipo_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 13.6 yes
     _FldNameList[72]   > general.items_contratos.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[73]   > general.items_contratos.id_variedad
"id_variedad" "id_variedad" ? ? "integer" ? ? ? ? ? ? yes ? no 3.8 yes
     _FldNameList[74]   > general.items_contratos.interes
"interes" "interes" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes
     _FldNameList[75]   > general.items_contratos.kilos_cajas
"kilos_cajas" "kilos_cajas" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[76]   > general.items_contratos.marca_tambores
"marca_tambores" "marca_tambores" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[77]   > general.items_contratos.numero_release[1]
"numero_release[1]" "numero_release1" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[78]   > general.items_contratos.numero_release[2]
"numero_release[2]" "numero_release2" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[79]   > general.items_contratos.numero_release[3]
"numero_release[3]" "numero_release3" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[80]   > general.items_contratos.obleas
"obleas" "obleas" ? ? "logical" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[81]   > general.items_contratos.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[82]   > general.items_contratos.obs_comerciales
"obs_comerciales" "obs_comerciales" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[83]   > general.items_contratos.otra_identificacion
"otra_identificacion" "otra_identificacion" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[84]   > general.items_contratos.papel_sulfito
"papel_sulfito" "papel_sulfito" ? ? "logical" ? ? ? ? ? ? yes ? no 13 yes
     _FldNameList[85]   > general.items_contratos.pendiente
"pendiente" "pendiente" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[86]   > general.items_contratos.precio_base
"precio_base" "precio_base" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[87]   > general.items_contratos.precio_base_calculo
"precio_base_calculo" "precio_base_calculo" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[88]   > general.items_contratos.precio_comision
"precio_comision" "precio_comision" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[89]   > general.items_contratos.precio_local
"precio_local" "precio_local" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[90]   > general.items_contratos.precio_origen
"precio_origen" "precio_origen" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[91]   > general.items_contratos.precio_venta
"precio_venta" "precio_venta" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[92]   > general.items_contratos.reutiliza_caja
"reutiliza_caja" "reutiliza_caja" ? ? "logical" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[93]   > general.items_contratos.saldo
"saldo" "saldo" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes
     _FldNameList[94]   > general.items_contratos.semana_entrega_hasta
"semana_entrega_hasta" "semana_entrega_hasta" ? ? "integer" ? ? ? ? ? ? yes ? no 24.4 yes
     _FldNameList[95]   > "_<CALC>"
"getTbsAsoc()" "Cumplidos" "Cumplidos" ">>>,>>>,>>9" "Integer" ? ? ? ? ? ? no ? no 12 no
     _FldNameList[96]   > "_<CALC>"
"getCondVta()" "CondVenta" "CondVenta" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no
     _FldNameList[97]   > "_<CALC>"
"getMoneda()" "Moneda" "Moneda" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
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
DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.
DEFINE VARIABLE cCont AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iTipo AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAnio AS INTEGER    NO-UNDO.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
  {get ContainerSource hCont}.
   
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wItemsContratos.w*" THEN DO:
    cCont = DYNAMIC-FUNCTION('getParamContrato' IN hCont).
    iTipo = DYNAMIC-FUNCTION('getParamTipoContrato' IN hCont).
    iAnio = DYNAMIC-FUNCTION('getParamAnioContrato' IN hCont).
  END.

  ASSIGN rowObjUpd.id_contrato      = cCont
         rowObjUpd.id_tipo_contrato = iTipo
         rowObjUpd.anio             = iAnio
         rowObjUpd.c_usuario        = USERID("userdb")
         rowObjUpd.c_fecha          = TODAY  
         rowObjUpd.c_hora           = STRING(TIME,"HH:MM:SS").
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora,id_contrato,id_tipo_contrato,anio".
  
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
         rowObject.Articulo = (getArticulo())
         rowObject.Calidad = (getCalidad())
         rowObject.CondVenta = (getCondVta())
         rowObject.Cumplidos = (getTbsAsoc())
         rowObject.Envase = (getEnvase())
         rowObject.Moneda = (getMoneda())
         rowObject.OrdenFabricacion = (getOF())
         rowObject.Tipo = (getTipoContrato())
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
FIND LAST RowObjUpd NO-ERROR.

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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArticulo dTables  _DB-REQUIRED
FUNCTION getArticulo RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCondVta dTables  _DB-REQUIRED
FUNCTION getCondVta RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST clausulas WHERE clausulas.id_clausula = rowObject.id_clausula
                       NO-LOCK NO-ERROR.
  IF AVAILABLE clausulas THEN 
    RETURN clausulas.descripcion.
  ELSE
    RETURN "NO INFO".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMoneda dTables  _DB-REQUIRED
FUNCTION getMoneda RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
    FOR FIRST tipo_moneda 
        WHERE tipo_moneda.id_moneda = rowObject.id_moneda_origen
        NO-LOCK.
      cRet = tipo_moneda.descripcion.
    END.
    

  
  RETURN cRet.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOF dTables  _DB-REQUIRED
FUNCTION getOF RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST contratos WHERE contratos.id_contrato      = rowObject.id_contrato
                         AND contratos.id_tipo_contrato = rowObject.id_tipo_contrato
                         AND contratos.anio             = rowObject.anio
                    NO-LOCK NO-ERROR.
  IF AVAILABLE contratos THEN
    RETURN STRING(contratos.orden_fabricacion).
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTbsAsoc dTables  _DB-REQUIRED
FUNCTION getTbsAsoc RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_contrato          = rowObject.id_contrato
        AND tambores_industria.ITEM_of              = rowObject.ITEM
        AND tambores_industria.id_tipocontrato_of   = rowObject.id_tipo_contrato
        AND tambores_industria.anio_of              = rowObject.anio
      NO-LOCK.
    i = i + 1.
  END.

  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTipoContrato dTables  _DB-REQUIRED
FUNCTION getTipoContrato RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST tipos_contrato WHERE tipos_contrato.id_tipo_contrato = rowObject.id_tipo_contrato
                    NO-LOCK NO-ERROR.
  IF AVAILABLE tipos_contrato THEN
    RETURN tipos_contrato.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION openQuery dTables  _DB-REQUIRED
FUNCTION openQuery RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RETURN SUPER( ).

  PUBLISH 'ContratoChanged'.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

