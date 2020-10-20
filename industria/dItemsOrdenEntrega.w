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

DEFINE TEMP-TABLE ttPedidoFondos
  RCODE-INFORMATION
  FIELD despachante   AS CHARACTER COLUMN-LABEL "Despachante"
  FIELD oe            AS CHARACTER COLUMN-LABEL "OE"
  FIELD vapor         AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD Fecha         AS CHARACTER COLUMN-LABEL "Fecha"
  FIELD cliente       AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD producto      AS CHARACTER COLUMN-LABEL "Producto"
  FIELD destino       AS CHARACTER COLUMN-LABEL "Destino"
  FIELD condicion_vta AS CHARACTER COLUMN-LABEL "Condicion Vta"
  FIELD compania      AS CHARACTER COLUMN-LABEL "Cia. Maritima"
  FIELD contenedores  AS CHARACTER COLUMN-LABEL "Tot. Contenedores"
  FIELD gasto         AS CHARACTER COLUMN-LABEL "Gasto"
  FIELD importe       AS CHARACTER COLUMN-LABEL "Importe".

DEFINE TEMP-TABLE ttTransfer
    RCODE-INFORMATION
    FIELD id_orden_entrega  AS INTEGER COLUMN-LABEL "OE"
    FIELD ITEM_oe           AS INTEGER COLUMN-LABEL "Pte"
    FIELD anio              AS INTEGER COLUMN-LABEL "Anio"
    FIELD semana_entrega    AS INTEGER COLUMN-LABEL "Semana"
    FIELD producto          AS CHARACTER COLUMN-LABEL "Articulo"
    FIELD precioVenta       AS DECIMAL COLUMN-LABEL "Precio"
    FIELD Unidad            AS CHARACTER COLUMN-LABEL "Unidad"
    FIELD id_estado         AS INTEGER COLUMN-LABEL "Estado"
    FIELD id_contrato       AS CHARACTER COLUMN-LABEL "Contrato"
    FIELD cantidad          AS INTEGER COLUMN-LABEL "Cantidad"
    FIELD orden_fabricacion AS INTEGER COLUMN-LABEL "OF"
    FIELD cliente           AS CHARACTER COLUMN-LABEL "Cliente"
    .

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
&Scoped-define INTERNAL-TABLES items_orden_entrega

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio cliente cajas_x_pallets cantidad_pallets Producto cantidad_tambores~
 cerrado destino contenedores fob_ton condVta fob_ton_u$s fob_unitario~
 agencia id_articulo id_calidad despachante id_cliente id_condicion_venta~
 vapor id_contrato id_envase id_estado id_frigorifico id_instrumento_pago~
 id_marca id_moneda id_moneda_cambio id_orden_entrega id_programa_despacho~
 id_tipo_contenedor id_tipo_contrato id_tipo_orden_entrega id_tipo_pallet~
 id_tipo_plazo id_tipo_unidad_venta id_tipo_venta importe_comisiones~
 importe_derechos_exportacion importe_factura_dolar importe_fob_dolar~
 importe_origen importe_reintegro_fijo item item_oe item_programa_despacho~
 kgs_brutos_tambores kgs_netos_tambores kilos_x_caja modo_actualizacion~
 pendiente plazo precio_x_caja precio_x_galon semana_entrega~
 tambores_pedidos tipo_cambio total_cajas total_factura~
 valor_aduana_derechos valor_aduana_reintegro x_kilos fecha cert_fito~
 grados_brix observaciones total_galones cheque coeficiente cotizacion~
 c_fecha c_hora c_usuario id_empresa_ipp id_orden_ipp id_punto_emisor_ipp~
 item_ipp
&Scoped-define ENABLED-FIELDS-IN-items_orden_entrega anio cajas_x_pallets ~
cantidad_pallets cantidad_tambores cerrado contenedores fob_ton fob_ton_u$s ~
fob_unitario id_articulo id_calidad id_cliente id_condicion_venta ~
id_contrato id_envase id_estado id_frigorifico id_instrumento_pago id_marca ~
id_moneda id_moneda_cambio id_orden_entrega id_programa_despacho ~
id_tipo_contenedor id_tipo_contrato id_tipo_orden_entrega id_tipo_pallet ~
id_tipo_plazo id_tipo_unidad_venta id_tipo_venta importe_comisiones ~
importe_derechos_exportacion importe_factura_dolar importe_fob_dolar ~
importe_origen importe_reintegro_fijo item item_oe item_programa_despacho ~
kgs_brutos_tambores kgs_netos_tambores kilos_x_caja modo_actualizacion ~
pendiente plazo precio_x_caja precio_x_galon semana_entrega ~
tambores_pedidos tipo_cambio total_cajas total_factura ~
valor_aduana_derechos valor_aduana_reintegro x_kilos fecha cert_fito ~
grados_brix observaciones total_galones cheque coeficiente cotizacion ~
c_fecha c_hora c_usuario id_empresa_ipp id_orden_ipp id_punto_emisor_ipp ~
item_ipp 
&Scoped-Define DATA-FIELDS  anio cliente cajas_x_pallets cantidad_pallets Producto cantidad_tambores~
 cerrado destino contenedores fob_ton condVta fob_ton_u$s fob_unitario~
 agencia id_articulo id_calidad despachante id_cliente id_condicion_venta~
 vapor id_contrato id_envase id_estado id_frigorifico id_instrumento_pago~
 id_marca id_moneda id_moneda_cambio id_orden_entrega id_programa_despacho~
 id_tipo_contenedor id_tipo_contrato id_tipo_orden_entrega id_tipo_pallet~
 id_tipo_plazo id_tipo_unidad_venta id_tipo_venta importe_comisiones~
 importe_derechos_exportacion importe_factura_dolar importe_fob_dolar~
 importe_origen importe_reintegro_fijo item item_oe item_programa_despacho~
 kgs_brutos_tambores kgs_netos_tambores kilos_x_caja modo_actualizacion~
 pendiente plazo precio_x_caja precio_x_galon semana_entrega~
 tambores_pedidos tipo_cambio total_cajas total_factura~
 valor_aduana_derechos valor_aduana_reintegro x_kilos fecha cert_fito~
 grados_brix observaciones total_galones cheque coeficiente cotizacion~
 c_fecha c_hora c_usuario id_empresa_ipp id_orden_ipp id_punto_emisor_ipp~
 item_ipp PrecioVenta Unidad GastosItem UniTon UniKil UniGal UniLib Libras~
 Moneda Estado
&Scoped-define DATA-FIELDS-IN-items_orden_entrega anio cajas_x_pallets ~
cantidad_pallets cantidad_tambores cerrado contenedores fob_ton fob_ton_u$s ~
fob_unitario id_articulo id_calidad id_cliente id_condicion_venta ~
id_contrato id_envase id_estado id_frigorifico id_instrumento_pago id_marca ~
id_moneda id_moneda_cambio id_orden_entrega id_programa_despacho ~
id_tipo_contenedor id_tipo_contrato id_tipo_orden_entrega id_tipo_pallet ~
id_tipo_plazo id_tipo_unidad_venta id_tipo_venta importe_comisiones ~
importe_derechos_exportacion importe_factura_dolar importe_fob_dolar ~
importe_origen importe_reintegro_fijo item item_oe item_programa_despacho ~
kgs_brutos_tambores kgs_netos_tambores kilos_x_caja modo_actualizacion ~
pendiente plazo precio_x_caja precio_x_galon semana_entrega ~
tambores_pedidos tipo_cambio total_cajas total_factura ~
valor_aduana_derechos valor_aduana_reintegro x_kilos fecha cert_fito ~
grados_brix observaciones total_galones cheque coeficiente cotizacion ~
c_fecha c_hora c_usuario id_empresa_ipp id_orden_ipp id_punto_emisor_ipp ~
item_ipp 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "ditemsordenentrega.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_orden_entrega NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_orden_entrega NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_orden_entrega
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_orden_entrega


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calculaFobItemOE dTables  _DB-REQUIRED
FUNCTION calculaFobItemOE RETURNS DECIMAL
  (INPUT pOE     AS INTEGER ,     /* ROWID DE orden_entrega */
   INPUT pItemOE AS INTEGER,      /* ITEM DE LA OE */
   INPUT pTotalFactura AS DECIMAL /* IDEM ANTERIOR */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calculaGastosItemOE dTables  _DB-REQUIRED
FUNCTION calculaGastosItemOE RETURNS DECIMAL
  (INPUT rIPP    AS ROWID , 
   INPUT vOE     AS INTEGER ,
   INPUT vItemOE AS INTEGER ,
   INPUT pPallets AS INTEGER ,
   INPUT pClausula AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createItemOEPedidosPackingCompleto dTables  _DB-REQUIRED
FUNCTION createItemOEPedidosPackingCompleto RETURNS CHARACTER
  (INPUT prPedidoPacking AS ROWID,
   INPUT piOrdenEntrega AS INTEGER
   )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createItemOrdenEntrega dTables  _DB-REQUIRED
FUNCTION createItemOrdenEntrega RETURNS CHARACTER
  (INPUT rIPP    AS ROWID ,
   INPUT pOE     AS INTEGER ,
   INPUT pPallets AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deleteItemOrdenEntrega dTables  _DB-REQUIRED
FUNCTION deleteItemOrdenEntrega RETURNS ROWID
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAgencia dTables  _DB-REQUIRED
FUNCTION getAgencia RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCajasPorPallets dTables  _DB-REQUIRED
FUNCTION getCajasPorPallets RETURNS INTEGER
  ( INPUT pTipoPallet AS INTEGER,
    INPUT pEnvase AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadContenedores dTables  _DB-REQUIRED
FUNCTION getCantidadContenedores RETURNS DECIMAL
  ( INPUT pTipoPallet AS INTEGER ,
    INPUT pPallets AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCliente dTables  _DB-REQUIRED
FUNCTION getCliente RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCoeficienteAduana dTables  _DB-REQUIRED
FUNCTION getCoeficienteAduana RETURNS DECIMAL
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDespachante dTables  _DB-REQUIRED
FUNCTION getDespachante RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDestino dTables  _DB-REQUIRED
FUNCTION getDestino RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEstadoOE dTables  _DB-REQUIRED
FUNCTION getEstadoOE RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFobUniGal dTables  _DB-REQUIRED
FUNCTION getFobUniGal RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFobUniKil dTables  _DB-REQUIRED
FUNCTION getFobUniKil RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFobUniLib dTables  _DB-REQUIRED
FUNCTION getFobUniLib RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFobUniTon dTables  _DB-REQUIRED
FUNCTION getFobUniTon RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGastosItemOE dTables  _DB-REQUIRED
FUNCTION getGastosItemOE RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIPPAvailability dTables  _DB-REQUIRED
FUNCTION getIPPAvailability RETURNS LOGICAL
  ( INPUT pIPP AS ROWID,
    INPUT pPallets AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosBrutos dTables  _DB-REQUIRED
FUNCTION getKilosBrutos RETURNS DECIMAL
  ( INPUT pEnvase AS INTEGER,
    INPUT pTotalCajas AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosNetos dTables  _DB-REQUIRED
FUNCTION getKilosNetos RETURNS DECIMAL
  ( INPUT pEnvase AS INTEGER,
    INPUT pTotalCajas AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMonedaOrigen dTables  _DB-REQUIRED
FUNCTION getMonedaOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPesoLibras dTables  _DB-REQUIRED
FUNCTION getPesoLibras RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrecioPorCaja dTables  _DB-REQUIRED
FUNCTION getPrecioPorCaja RETURNS DECIMAL
  ( INPUT pCliente AS INTEGER,
    INPUT pArticulo AS INTEGER,
    INPUT pEnvase AS INTEGER,
    INPUT pCalibre AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrecioVenta dTables  _DB-REQUIRED
FUNCTION getPrecioVenta RETURNS DECIMAL
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidadVenta dTables  _DB-REQUIRED
FUNCTION getUnidadVenta RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVapor dTables  _DB-REQUIRED
FUNCTION getVapor RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD modifyCantidadPallets dTables  _DB-REQUIRED
FUNCTION modifyCantidadPallets RETURNS CHARACTER
  (INPUT rIPP   AS ROWID ,
   INPUT pCantidadPallets AS INTEGER,
   INPUT pSigno AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_orden_entrega SCROLLING.
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
         HEIGHT             = 1.52
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
     _TblList          = "general.items_orden_entrega"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.items_orden_entrega.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > "_<CALC>"
"getCliente()" "cliente" "cliente" "x(50)" "character" ? ? ? ? ? ? yes ? no 50 no
     _FldNameList[3]   > general.items_orden_entrega.cajas_x_pallets
"cajas_x_pallets" "cajas_x_pallets" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[4]   > general.items_orden_entrega.cantidad_pallets
"cantidad_pallets" "cantidad_pallets" ? ? "integer" ? ? ? ? ? ? yes ? no 15.2 yes
     _FldNameList[5]   > "_<CALC>"
"getProducto()" "Producto" "producto" "x(50)" "character" ? ? ? ? ? ? yes ? no 50 no
     _FldNameList[6]   > general.items_orden_entrega.cantidad_tambores
"cantidad_tambores" "cantidad_tambores" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[7]   > general.items_orden_entrega.cerrado
"cerrado" "cerrado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[8]   > "_<CALC>"
"getDestino()" "destino" "destino" "x(50)" "character" ? ? ? ? ? ? yes ? no 50 no
     _FldNameList[9]   > general.items_orden_entrega.contenedores
"contenedores" "contenedores" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[10]   > general.items_orden_entrega.fob_ton
"fob_ton" "fob_ton" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[11]   > "_<CALC>"
"getCondVta()" "condVta" "condVta" "x(15)" "character" ? ? ? ? ? ? yes ? no 15 no
     _FldNameList[12]   > general.items_orden_entrega.fob_ton_u$s
"fob_ton_u$s" "fob_ton_u$s" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[13]   > general.items_orden_entrega.fob_unitario
"fob_unitario" "fob_unitario" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[14]   > "_<CALC>"
"getAgencia()" "agencia" "agencia" "x(50)" "character" ? ? ? ? ? ? yes ? no 50 no
     _FldNameList[15]   > general.items_orden_entrega.id_articulo
"id_articulo" "id_articulo" ? ">>9" "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[16]   > general.items_orden_entrega.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[17]   > "_<CALC>"
"getDespachante()" "despachante" "despachante" "x(50)" "character" ? ? ? ? ? ? yes ? no 50 no
     _FldNameList[18]   > general.items_orden_entrega.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[19]   > general.items_orden_entrega.id_condicion_venta
"id_condicion_venta" "id_condicion_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[20]   > "_<CALC>"
"getVapor()" "vapor" "vapor" "x(15)" "character" ? ? ? ? ? ? yes ? no 15 no
     _FldNameList[21]   > general.items_orden_entrega.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[22]   > general.items_orden_entrega.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[23]   > general.items_orden_entrega.id_estado
"id_estado" "id_estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[24]   > general.items_orden_entrega.id_frigorifico
"id_frigorifico" "id_frigorifico" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[25]   > general.items_orden_entrega.id_instrumento_pago
"id_instrumento_pago" "id_instrumento_pago" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[26]   > general.items_orden_entrega.id_marca
"id_marca" "id_marca" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[27]   > general.items_orden_entrega.id_moneda
"id_moneda" "id_moneda" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[28]   > general.items_orden_entrega.id_moneda_cambio
"id_moneda_cambio" "id_moneda_cambio" ? ? "integer" ? ? ? ? ? ? yes ? no 15.2 yes
     _FldNameList[29]   > general.items_orden_entrega.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[30]   > general.items_orden_entrega.id_programa_despacho
"id_programa_despacho" "id_programa_despacho" ? ? "integer" ? ? ? ? ? ? yes ? no 22.2 yes
     _FldNameList[31]   > general.items_orden_entrega.id_tipo_contenedor
"id_tipo_contenedor" "id_tipo_contenedor" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[32]   > general.items_orden_entrega.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[33]   > general.items_orden_entrega.id_tipo_orden_entrega
"id_tipo_orden_entrega" "id_tipo_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[34]   > general.items_orden_entrega.id_tipo_pallet
"id_tipo_pallet" "id_tipo_pallet" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[35]   > general.items_orden_entrega.id_tipo_plazo
"id_tipo_plazo" "id_tipo_plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[36]   > general.items_orden_entrega.id_tipo_unidad_venta
"id_tipo_unidad_venta" "id_tipo_unidad_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[37]   > general.items_orden_entrega.id_tipo_venta
"id_tipo_venta" "id_tipo_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 4.6 yes
     _FldNameList[38]   > general.items_orden_entrega.importe_comisiones
"importe_comisiones" "importe_comisiones" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.2 yes
     _FldNameList[39]   > general.items_orden_entrega.importe_derechos_exportacion
"importe_derechos_exportacion" "importe_derechos_exportacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[40]   > general.items_orden_entrega.importe_factura_dolar
"importe_factura_dolar" "importe_factura_dolar" ? ? "decimal" ? ? ? ? ? ? yes ? no 20.4 yes
     _FldNameList[41]   > general.items_orden_entrega.importe_fob_dolar
"importe_fob_dolar" "importe_fob_dolar" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[42]   > general.items_orden_entrega.importe_origen
"importe_origen" "importe_origen" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[43]   > general.items_orden_entrega.importe_reintegro_fijo
"importe_reintegro_fijo" "importe_reintegro_fijo" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes
     _FldNameList[44]   > general.items_orden_entrega.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[45]   > general.items_orden_entrega.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[46]   > general.items_orden_entrega.item_programa_despacho
"item_programa_despacho" "item_programa_despacho" ? ? "integer" ? ? ? ? ? ? yes ? no 24.4 yes
     _FldNameList[47]   > general.items_orden_entrega.kgs_brutos_tambores
"kgs_brutos_tambores" "kgs_brutos_tambores" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[48]   > general.items_orden_entrega.kgs_netos_tambores
"kgs_netos_tambores" "kgs_netos_tambores" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[49]   > general.items_orden_entrega.kilos_x_caja
"kilos_x_caja" "kilos_x_caja" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[50]   > general.items_orden_entrega.modo_actualizacion
"modo_actualizacion" "modo_actualizacion" ? ? "logical" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[51]   > general.items_orden_entrega.pendiente
"pendiente" "pendiente" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[52]   > general.items_orden_entrega.plazo
"plazo" "plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[53]   > general.items_orden_entrega.precio_x_caja
"precio_x_caja" "precio_x_caja" ? ? "decimal" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[54]   > general.items_orden_entrega.precio_x_galon
"precio_x_galon" "precio_x_galon" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.8 yes
     _FldNameList[55]   > general.items_orden_entrega.semana_entrega
"semana_entrega" "semana_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 18.6 yes
     _FldNameList[56]   > general.items_orden_entrega.tambores_pedidos
"tambores_pedidos" "tambores_pedidos" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[57]   > general.items_orden_entrega.tipo_cambio
"tipo_cambio" "tipo_cambio" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[58]   > general.items_orden_entrega.total_cajas
"total_cajas" "total_cajas" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[59]   > general.items_orden_entrega.total_factura
"total_factura" "total_factura" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[60]   > general.items_orden_entrega.valor_aduana_derechos
"valor_aduana_derechos" "valor_aduana_derechos" ? ? "decimal" ? ? ? ? ? ? yes ? no 22.2 yes
     _FldNameList[61]   > general.items_orden_entrega.valor_aduana_reintegro
"valor_aduana_reintegro" "valor_aduana_reintegro" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.2 yes
     _FldNameList[62]   > general.items_orden_entrega.x_kilos
"x_kilos" "x_kilos" ? ? "logical" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[63]   > general.items_orden_entrega.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[64]   > general.items_orden_entrega.cert_fito
"cert_fito" "cert_fito" ? ? "logical" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[65]   > general.items_orden_entrega.grados_brix
"grados_brix" "grados_brix" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[66]   > general.items_orden_entrega.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[67]   > general.items_orden_entrega.total_galones
"total_galones" "total_galones" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[68]   > general.items_orden_entrega.cheque
"cheque" "cheque" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[69]   > general.items_orden_entrega.coeficiente
"coeficiente" "coeficiente" ? ? "decimal" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[70]   > general.items_orden_entrega.cotizacion
"cotizacion" "cotizacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[71]   > general.items_orden_entrega.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[72]   > general.items_orden_entrega.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[73]   > general.items_orden_entrega.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[74]   > general.items_orden_entrega.id_empresa_ipp
"id_empresa_ipp" "id_empresa_ipp" ? ? "integer" ? ? ? ? ? ? yes ? no 27.2 yes
     _FldNameList[75]   > general.items_orden_entrega.id_orden_ipp
"id_orden_ipp" "id_orden_ipp" ? ? "integer" ? ? ? ? ? ? yes ? no 25.8 yes
     _FldNameList[76]   > general.items_orden_entrega.id_punto_emisor_ipp
"id_punto_emisor_ipp" "id_punto_emisor_ipp" ? ? "integer" ? ? ? ? ? ? yes ? no 19.4 yes
     _FldNameList[77]   > general.items_orden_entrega.item_ipp
"item_ipp" "item_ipp" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[78]   > "_<CALC>"
"getPrecioVenta()" "PrecioVenta" "PrecioVenta" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[79]   > "_<CALC>"
"getUnidadVenta()" "Unidad" "Unidad" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no
     _FldNameList[80]   > "_<CALC>"
"getGastosItemOE()" "GastosItem" "GastosItem" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[81]   > "_<CALC>"
"getFobUniTon()" "UniTon" "UniTon" ">>>,>>>,>>9.9999" "Decimal" ? ? ? ? ? ? no ? no 17.4 no
     _FldNameList[82]   > "_<CALC>"
"getFobUniKil()" "UniKil" "UniKil" ">>>,>>>,>>9.9999" "Decimal" ? ? ? ? ? ? no ? no 17.4 no
     _FldNameList[83]   > "_<CALC>"
"getFobUniGal()" "UniGal" "UniGal" ">>>,>>>,>>9.9999" "Decimal" ? ? ? ? ? ? no ? no 17.4 no
     _FldNameList[84]   > "_<CALC>"
"getFobUniLib()" "UniLib" "UniLib" ">>>,>>>,>>9.9999" "Decimal" ? ? ? ? ? ? no ? no 17.4 no
     _FldNameList[85]   > "_<CALC>"
"getPesoLibras()" "Libras" "Libras" ">>>,>>>,>>9.9999" "Decimal" ? ? ? ? ? ? no ? no 17.4 no
     _FldNameList[86]   > "_<CALC>"
"getMonedaOrigen()" "Moneda" "Moneda" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[87]   > "_<CALC>"
"getEstadoOE()" "Estado" "Estado" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
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
  
  /*add*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C". 

    FOR FIRST items_contratos 
        WHERE items_contratos.id_contrato       = rowObjUpd.id_contrato
          /*AND items_contratos.id_tipo_contrato  = rowObjUpd.id_tipo_contrato*/
          /*AND items_contratos.anio              = rowObjUpd.anio*/
          AND items_contratos.ITEM              = rowObjUpd.ITEM.

      FIND FIRST contratos OF items_contratos NO-LOCK NO-ERROR.

      ASSIGN rowObjUpd.id_cliente           = IF AVAILABLE contratos THEN contratos.id_cliente ELSE 0
             rowObjUpd.id_moneda            = items_contratos.id_moneda_origen
             rowObjUpd.id_tipo_unidad_venta = items_contratos.id_tipo_unidad_venta_origen
             rowObjUpd.id_tipo_venta        = items_contratos.id_tipo_venta
             rowObjUpd.semana_entrega       = items_contratos.semana_entrega
             rowObjUpd.pendiente            = FALSE
             rowObjUpd.id_tipo_unidad_venta = items_contratos.id_tipo_unidad_venta
             rowObjUpd.importe_origen       = items_contratos.precio_origen
             rowObjUpd.contenedores         = IF rowObjUpd.contenedores = 0 THEN 1 ELSE rowObjUpd.contenedores
             items_contratos.pendiente      = FALSE
             items_contratos.semana_entrega = rowObjUpd.semana_entrega
             .
      rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_cliente,id_moneda,id_tipo_unidad_venta,id_tipo_venta,semana_entrega,id_tipo_unidad_venta,precio_origen,contenedores".


      /*poner fecha en documento_oe asociacion de tambores*/
      FOR FIRST documentos_oe
          WHERE documentos_oe.id_orden_entrega  = rowObjUpd.id_orden_entrega
            AND documentos_oe.id_tipo_documento = 4.
    
        ASSIGN documentos_oe.fecha_pedido = TODAY.
          
      END.
    END.

    
  
    ASSIGN rowObjUpd.c_usuario = USERID("userdb")
           rowObjUpd.c_fecha   = TODAY
           rowObjUpd.c_hora    = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".    
  END.
  
  /*update*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U". 
    FOR FIRST items_contratos 
        WHERE items_contratos.id_contrato       = rowObjUpd.id_contrato
          AND items_contratos.id_tipo_contrato  = rowObjUpd.id_tipo_contrato
          AND items_contratos.anio              = rowObjUpd.anio
          AND items_contratos.ITEM              = rowObjUpd.ITEM.

      ASSIGN rowObjUpd.c_usuario            = USERID("userdb")
             rowObjUpd.c_fecha              = TODAY
             rowObjUpd.c_hora               = STRING(TIME,"HH:MM:SS")
             items_contratos.pendiente      = FALSE
             items_contratos.semana_entrega = rowObjUpd.semana_entrega
             .
      rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".        
    END.
  END.
  
  /*delete*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D". 
  
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
         rowObject.agencia = (getAgencia())
         rowObject.cliente = (getCliente())
         rowObject.condVta = (getCondVta())
         rowObject.despachante = (getDespachante())
         rowObject.destino = (getDestino())
         rowObject.Estado = (getEstadoOE())
         rowObject.GastosItem = (getGastosItemOE())
         rowObject.Libras = (getPesoLibras())
         rowObject.Moneda = (getMonedaOrigen())
         rowObject.PrecioVenta = (getPrecioVenta())
         rowObject.Producto = (getProducto())
         rowObject.Unidad = (getUnidadVenta())
         rowObject.UniGal = (getFobUniGal())
         rowObject.UniKil = (getFobUniKil())
         rowObject.UniLib = (getFobUniLib())
         rowObject.UniTon = (getFobUniTon())
         rowObject.vapor = (getVapor())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTTPedidoFondos dTables  _DB-REQUIRED
PROCEDURE fillTTPedidoFondos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttPedidoFondos.

  FOR EACH ttPedidoFondos.
    DELETE ttPedidoFondos.
  END.

  FIND CURRENT rowObject NO-ERROR.
  FOR EACH gastos_items_orden_entrega WHERE gastos_items_orden_entrega.id_orden_entrega = rowObject.id_orden_entrega
                                        AND gastos_items_orden_entrega.ITEM_oe          = rowObject.ITEM_oe
                                      NO-LOCK. 
    FIND gastos_venta OF gastos_items_orden_entrega NO-LOCK NO-ERROR.
    CREATE ttPedidoFondos.
    ASSIGN ttPedidoFondos.despachante   = rowObject.despachante
           ttPedidoFondos.oe            = string(rowObject.id_orden_entrega)
           ttPedidoFondos.vapor         = rowObject.vapor
           ttPedidoFondos.fecha         = STRING(rowObject.fecha)
           ttPedidoFondos.cliente       = rowObject.cliente
           ttPedidoFondos.producto      = rowObject.producto
           ttPedidoFondos.destino       = rowObject.destino
           ttPedidoFondos.condicion_vta = rowObject.condvta
           ttPedidoFondos.compania      = rowObject.agencia
           ttPedidoFondos.contenedores  = STRING(rowObject.contenedores)
           ttPedidoFondos.gasto         = gastos_venta.descripcion
           ttPedidoFondos.importe       = STRING(gastos_items_orden_entrega.importe, ">>>>>>99.99").
  END.

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
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  FIND LAST RowObjUpd NO-ERROR.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p'). 
  DELETE OBJECT hLibCom.

  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C" OR rowObjUpd.rowMod = "U".
    /* entrada en tabla despachos */
    RUN createSubdDespacho IN hLib (rowObjUpd.id_orden_entrega, rowObjUpd.ITEM_oe).
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

  FOR EACH rowObject.
    CREATE ttTransfer.
    ASSIGN ttTransfer.id_orden_entrega  = rowObject.id_orden_entrega
           ttTransfer.ITEM_oe           = rowObject.ITEM_oe
           ttTransfer.anio              = rowObject.anio
           ttTransfer.semana_entrega    = rowObject.semana_entrega
           ttTransfer.producto          = rowObject.producto
           ttTransfer.precioVenta       = rowObject.precioVenta
           ttTransfer.unidad            = rowObject.Unidad
           ttTransfer.id_estado         = rowObject.id_estado
           ttTransfer.id_contrato       = rowObject.id_contrato
           ttTransfer.cantidad          = rowObject.tambores_pedidos
           ttTransfer.orden_fabricacion = 0
           ttTransfer.cliente           = rowObject.cliente
           .
  END.

  RUN generateExcel.p (INPUT TABLE ttTransfer,
                       INPUT " Ordenes Entrega ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Century Gothic",
                       INPUT 7).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calculaFobItemOE dTables  _DB-REQUIRED
FUNCTION calculaFobItemOE RETURNS DECIMAL
  (INPUT pOE     AS INTEGER ,     /* ROWID DE orden_entrega */
   INPUT pItemOE AS INTEGER,      /* ITEM DE LA OE */
   INPUT pTotalFactura AS DECIMAL /* IDEM ANTERIOR */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR vGastos AS DECIMAL NO-UNDO.
    DEFINE VAR vPallets AS INTEGER NO-UNDO.
    DEFINE VAR vTotalFactura AS DECIMAL NO-UNDO.
    DEFINE VAR vCantidadContenedores AS DECIMAL NO-UNDO.
    DEFINE VAR vFob AS DECIMAL.
    DEFINE BUFFER bbItemOE FOR items_orden_entrega.
    
    FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = pOE NO-LOCK NO-ERROR.
    IF AVAILABLE orden_entrega THEN DO:
        
        FIND FIRST bbItemOE OF orden_entrega
                            WHERE bbItemOE.ITEM_oe = pItemOE
                            NO-LOCK NO-ERROR.
        IF AVAILABLE bbItemOE THEN DO:
            IF vPallets = 0 THEN vPallets = bbItemOE.cantidad_pallets.
            IF vTotalFactura = 0 THEN vTotalFactura = bbItemOE.TOTAL_factura.
            
            FOR EACH r_clausulas_gastos_item_oe WHERE r_clausulas_gastos_item_oe.id_clausula = bbItemOE.id_condicion_venta 
                                                NO-LOCK.
                /* BUSCO TODOS LOS GASTOS SEGUN LA CONDICION DE VENTA QUE ME DETERMINARA QUE TENGO QUE RESTAR
                   AL PRECIO FINAL PARA OBTENER EL FOB */

                FIND FIRST gastos_items_orden_entrega OF bbItemOE
                                                      WHERE gastos_items_orden_entrega.id_gasto = r_clausulas_gastos_item_oe.id_gasto
                                                      NO-LOCK NO-ERROR.
                IF AVAILABLE gastos_items_orden_entrega THEN DO:
                    /* ENTRO A BUSCAR PRIMERO LOS GASTOS DE LAS PARTES DE LA OE */
                    vGastos = vGastos + gastos_items_orden_entrega.importe.
                END.
            END. 
            
            vFob = vTotalFactura - vGastos.

            RETURN vFob.
        END.
        ELSE DO:
            /* LA PRIMERA VEZ, CUANDO SE CREA EL ITEM OE , ENTRA POR EL ELSE.
               DEJO DISPONIBLE LA PRIMERA PARTA PARA CUANDO SE PRODUCEN FUTURAS
               MODIFICACIONES SOBRE EL ITEM OE */

        END.
    END.
    
    IF vFob = 0 THEN RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calculaGastosItemOE dTables  _DB-REQUIRED
FUNCTION calculaGastosItemOE RETURNS DECIMAL
  (INPUT rIPP    AS ROWID , 
   INPUT vOE     AS INTEGER ,
   INPUT vItemOE AS INTEGER ,
   INPUT pPallets AS INTEGER ,
   INPUT pClausula AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

/* ESTA FUNCION QUIZAS NO SEA USADA */

    DEFINE VAR vCajasPorPallets AS INTEGER NO-UNDO.
    DEFINE VAR vTotalCajas AS INTEGER NO-UNDO.
    DEFINE VAR vCantidadContenedores AS DECIMAL NO-UNDO.
    DEFINE VAR vFlete AS DECIMAL NO-UNDO.
    DEFINE VAR vSeguro AS DECIMAL NO-UNDO.
    
    FIND FIRST items_pedidos_packing WHERE ROWID(items_pedidos_packing) = rIPP NO-LOCK NO-ERROR.
    IF AVAILABLE items_pedidos_packing THEN DO:
        FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = vOE NO-LOCK NO-ERROR.
        IF AVAILABLE orden_entrega THEN DO:
            FIND FIRST pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
            
            FIND FIRST r_pallets_envase OF items_pedidos_packing NO-LOCK NO-ERROR.
            IF AVAILABLE r_pallets_envase THEN DO:
                vCajasPorPallets = r_pallets_envases.pallets.
                vTotalCajas      = vCajasPorPallets * pPallets.
            END.
            ELSE DO:
                vCajasPorPallets = 0.
                vTotalCajas      = 0.
            END.
            
            FIND FIRST r_clientes_envases_precios WHERE r_clientes_envases_precios.id_cliente   = pedidos_packing.id_cliente
                                                    AND r_clientes_envases_precios.id_articulo  = items_pedidos_packing.id_articulo
                                                    AND r_clientes_envases_precios.id_envase    = items_pedidos_packing.id_envase
                                                    NO-LOCK NO-ERROR.
            
            FIND FIRST r_pallets_contenedores WHERE r_pallets_contenedor.id_tipo_pallet     = items_pedidos_packing.id_tipo_pallet
                                                AND r_pallets_contenedor.id_tipo_contenedor = 6
                                                NO-LOCK NO-ERROR.
            IF AVAILABLE r_pallets_contenedores THEN DO:
                vCantidadContenedores = pPallets / general.r_pallets_contenedores.palletsxcontenedor.
            END.
            ELSE DO:
                vCantidadContenedores = 0.
            END.

            RUN p_calculo-gastos_item_oe.p(INPUT orden_entrega.id_orden_entrega,
                                           INPUT vItemOE,
                                           INPUT pClausula,
                                           INPUT vCAntidadContenedores).
            /*
            CASE pClausula:
                WHEN 1 THEN DO: /* FOB */
                
                END.
                WHEN 3 THEN DO: /* CIF */
                 /* PREGUNTAR A MARTIN COMO VAMOS A CALCULAR EL FLETE Y EL SEGURO */
                END.
                WHEN 15 THEN DO: /* CONSIGNACION */
                 /* PREGUNTAR BIEN A MARTIN CUANDO VA EL FLETE Y CUANDO NO */

                END.
            END CASE.
            */
        END.
    END.
  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createItemOEPedidosPackingCompleto dTables  _DB-REQUIRED
FUNCTION createItemOEPedidosPackingCompleto RETURNS CHARACTER
  (INPUT prPedidoPacking AS ROWID,
   INPUT piOrdenEntrega AS INTEGER
   ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR vRespuesta AS CHAR.
    
    FIND FIRST pedidos_packing WHERE ROWID(pedidos_packing) = prPedidoPacking
                                NO-LOCK NO-ERROR.
    IF AVAILABLE pedidos_packing THEN DO:
        FOR EACH items_pedidos_packing OF pedidos_packing NO-LOCK.

            vRespuesta = createItemOrdenEntrega(ROWID(items_pedidos_packing),
                                                piOrdenEntrega,
                                                items_pedidos_packing.cant_pallets).
        END.
    END.
    
    RETURN vRespuesta.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createItemOrdenEntrega dTables  _DB-REQUIRED
FUNCTION createItemOrdenEntrega RETURNS CHARACTER
  (INPUT rIPP    AS ROWID ,
   INPUT pOE     AS INTEGER ,
   INPUT pPallets AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE BUFFER bb_items FOR items_orden_entrega.
    DEFINE VAR vCajasPorPallets AS INTEGER NO-UNDO.
    DEFINE VAR vTotalCajas AS INTEGER NO-UNDO.
    DEFINE VAR vPrecioPorCaja AS DECIMAL NO-UNDO.
    DEFINE VAR vCantidadContenedores AS DECIMAL NO-UNDO.
    DEFINE VAR vFob AS DECIMAL NO-UNDO.
    DEFINE VAR vFobPorCaja AS DECIMAL NO-UNDO.
    DEFINE VAR vItemOE AS INTEGER NO-UNDO.
    DEFINE VAR vTotalFactura AS DECIMAL NO-UNDO.
    DEFINE VAR vKilosNetos AS DECIMAL NO-UNDO.
    DEFINE VAR vKilosBrutos AS DECIMAL NO-UNDO.
    DEFINE VAR vTara AS DECIMAL NO-UNDO.
    DEFINE VAR vCoef AS DECIMAL DECIMALS 6 NO-UNDO.
    DEFINE VAR vComision AS DECIMAL NO-UNDO.
    DEFINE VAR vDerechoAduana AS DECIMAL NO-UNDO.
    DEFINE VAR vReintegroAduana AS DECIMAL NO-UNDO.
    DEFINE VAR vDerecho AS DECIMAL NO-UNDO.
    DEFINE VAR vReintegro AS DECIMAL NO-UNDO.
    DEFINE VAR vPalletsDisponibles AS LOGICAL NO-UNDO.
    
    FIND FIRST items_pedidos_packing WHERE ROWID(items_pedidos_packing) = rIPP
                                     NO-LOCK NO-ERROR.
    IF AVAILABLE items_pedidos_packing AND items_pedidos_packing.cant_pallets >= pPallets THEN DO:
        vPalletsDisponibles = getIPPAvailability(INPUT rIPP,
                                                 INPUT pPallets).
        IF vPalletsDisponibles THEN DO:
        
            FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = pOE NO-LOCK NO-ERROR.
            IF AVAILABLE orden_entrega THEN DO:
                FIND FIRST pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
                
                vCajasPorPallets = getCajasPorPallets(INPUT items_pedidos_packing.id_tipo_pallet,
                                                      INPUT items_pedidos_packing.id_envase).
    
                vTotalCajas      = vCajasPorPallets * pPallets.
                
                vPrecioPorCaja = getPrecioPorCaja(INPUT pedidos_packing.id_cliente,
                                                  INPUT items_pedidos_packing.id_articulo,
                                                  INPUT items_pedidos_packing.id_envase,
                                                  INPUT items_pedidos_packing.calibre).
                
                vCantidadContenedores = getCantidadContenedores (INPUT items_pedidos_packing.id_tipo_pallet,
                                                                 INPUT pPallets ).
    
                FIND LAST bb_items OF orden_entrega NO-LOCK NO-ERROR.
                vItemOE = IF AVAILABLE bb_items THEN (bb_items.ITEM_oe + 1) ELSE 1.
                
    
                vTotalFactura = vTotalCajas * vPrecioPorCaja.
                
                vKilosNetos = getKilosNetos(INPUT items_pedidos_packing.id_envase,
                                            INPUT vTotalCajas).
                
                vKilosBrutos = getKilosBrutos(INPUT items_pedidos_packing.id_envase,
                                              INPUT vTotalCajas).
    
                vCoef = getCoeficienteAduana().
                vComision = 0.    /* PREGUNTAR A MARTIN COMO SE CALCULA ESTO */
                
                CREATE items_orden_entrega.
                ASSIGN items_orden_entrega.id_orden_entrega       = orden_entrega.id_orden_entrega
                       items_orden_entrega.id_tipo_orden_entrega  = orden_entrega.id_tipo_orden_entrega
                       items_orden_entrega.ITEM_oe                = vItemOE
                       items_orden_entrega.fecha                  = orden_entrega.fecha
                       items_orden_entrega.id_empresa_ipp         = items_pedidos_packing.id_empresa
                       items_orden_entrega.id_orden_ipp           = items_pedidos_packing.id_orden
                       items_orden_entrega.id_punto_emisor_ipp    = items_pedidos_packing.id_punto_emisor
                       items_orden_entrega.ITEM_ipp               = items_pedidos_packing.ITEM
                       items_orden_entrega.id_envase              = items_pedidos_packing.id_envase
                       items_orden_entrega.id_articulo            = items_pedidos_packing.id_articulo
                       items_orden_entrega.id_cliente             = IF AVAILABLE pedidos_packing THEN pedidos_packing.id_cliente ELSE 0
                       items_orden_entrega.semana                 = items_pedidos_packing.semana
                       items_orden_entrega.id_calidad             = items_pedidos_packing.id_variedad
                       items_orden_entrega.id_marca               = items_pedidos_packing.id_marca
                       items_orden_entrega.id_tipo_pallet         = items_pedidos_packing.id_tipo_pallet
                       items_orden_entrega.cantidad_pallets       = pPallets
                       items_orden_entrega.cajas_x_pallets        = vCajasPorPallets
                       items_orden_entrega.total_cajas            = vTotalCajas
                       items_orden_entrega.kgs_netos_tambores     = vKilosNetos
                       items_orden_entrega.kgs_brutos_tambores    = vKilosBrutos
                       items_orden_entrega.kilos_x_caja           = vKilosNetos / vTotalCajas
                       items_orden_entrega.precio_x_caja          = vPrecioPorCaja
                       items_orden_entrega.id_condicion_venta     = r_clientes_envases_precios.id_condicion_venta
                       items_orden_entrega.id_moneda              = r_clientes_envases_precios.id_moneda
                       items_orden_entrega.plazo                  = 60
                       items_orden_entrega.id_tipo_plazo          = 2 /* dias de B/L */
                       items_orden_entrega.id_tipo_contenedor     = 6 /* High ube */
                       items_orden_entrega.cert_fito              = TRUE
                       items_orden_entrega.contenedores           = vCantidadContenedores
                       items_orden_entrega.TOTAL_factura          = vTotalFactura.
                
                /* ESTE PROGRAMA p_calculo-gastos_item_oe.p ES EL QUE ESTA EN USO 
                   EN LAS OE DE INDUSTRIA LO REBICE Y TENDRIA QUE ANDAR SIN PROBLEMAS 
                   PARA LAS OE DE FF.
                   ADRIAN CAROSSO 12-02-2004 */
                RUN p_calculo-gastos_item_oe.p(INPUT orden_entrega.id_orden_entrega,
                                               INPUT items_orden_entrega.ITEM_oe,
                                               INPUT r_clientes_envases_precios.id_condicion_venta,
                                               INPUT vCantidadContenedores).
                
                vFob = calculaFobItemOE (INPUT pOE,
                                         INPUT vItemOE,
                                         INPUT vTotalFactura).
                
                vDerechoAduana = vFob * vCoef.
                vDerecho = vDerechoAduana * 0.05.
    
                FIND LAST porcentaje_reint_articulo WHERE porcentaje_reint_articulo.id_articulo = 
                                                          items_pedidos_packing.id_articulo
                                                    NO-LOCK NO-ERROR.
    
                vReintegroAduana = vFob - vComision.
                vReintegro = IF AVAILABLE porcentaje_reint_articulo THEN (vReintegroAduana * (porcentaje_reint_articulo.porcentaje / 100)) ELSE 0.
                
                ASSIGN items_orden_entrega.fob_ton                = vFob
                       items_orden_entrega.fob_unitario           = vFob / vTotalCajas
                       items_orden_entrega.coeficiente            = vCoef
                       items_orden_entrega.valor_aduana_derechos  = vDerechoAduana
                       items_orden_entrega.valor_aduana_reintegro = vReintegroAduana
                       items_orden_entrega.importe_derechos_exportacion = vDerecho
                       items_orden_entrega.importe_reintegro_fijo = vReintegro.
                
            END.
        END.
    END.
    ELSE MESSAGE "Ha elegido mas pallets que los que tiene la parte del pedido" VIEW-AS ALERT-BOX.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deleteItemOrdenEntrega dTables  _DB-REQUIRED
FUNCTION deleteItemOrdenEntrega RETURNS ROWID
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR rowidIPP AS ROWID NO-UNDO.
 
FOR EACH gastos_items_orden_entrega WHERE gastos_items_orden_entrega.id_orden_entrega = rowObject.id_orden_entrega
                                       AND gastos_items_orden_entrega.ITEM_oe = rowObject.ITEM_oe.
     DELETE gastos_items_orden_entrega.
 END.

 FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega   = RowObject.id_orden_entrega
                                  AND items_orden_entrega.ITEM_oe            = RowObject.ITEM_oe
                                NO-ERROR.
 IF AVAILABLE items_orden_entrega THEN DO:
     FIND FIRST items_pedidos_packing WHERE items_pedidos_packing.id_empresa        = items_orden_entrega.id_empresa_ipp
                                        AND items_pedidos_packing.id_orden          = items_orden_entrega.id_orden_ipp
                                        AND items_pedidos_packing.id_punto_emisor   = items_orden_entrega.id_punto_emisor_ipp
                                        AND items_pedidos_packing.ITEM              = items_orden_entrega.item_ipp
                                        NO-LOCK NO-ERROR.
     IF AVAILABLE items_pedidos_packing THEN DO:
         rowidIPP = ROWID(items_pedidos_packing).
     END.
        
     DELETE items_orden_entrega.
 END.

 DYNAMIC-FUNCTION('openQuery':U).
 
 RETURN rowidIPP.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAgencia dTables  _DB-REQUIRED
FUNCTION getAgencia RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  
  FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = rowObject.id_orden_entrega
                           NO-LOCK NO-ERROR.
  FIND FIRST agencias WHERE agencias.id_agencia = orden_entrega.id_agencia
                      NO-LOCK NO-ERROR.
  IF AVAILABLE agencias THEN
    RETURN agencias.descripcion.
  ELSE 
    RETURN "NO INFO".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCajasPorPallets dTables  _DB-REQUIRED
FUNCTION getCajasPorPallets RETURNS INTEGER
  ( INPUT pTipoPallet AS INTEGER,
    INPUT pEnvase AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR vCajasPorPallets AS INTEGER.

    FIND FIRST r_pallets_envase WHERE r_pallets_envase.id_tipo_pallet = pTipoPallet
                                  AND r_pallets_envase.id_envase      = pEnvase
                                 NO-LOCK NO-ERROR.
    vCajasPorPallets = IF AVAILABLE r_pallets_envase THEN r_pallets_envases.pallets ELSE 0.
    RETURN vCajasPorPallets.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadContenedores dTables  _DB-REQUIRED
FUNCTION getCantidadContenedores RETURNS DECIMAL
  ( INPUT pTipoPallet AS INTEGER ,
    INPUT pPallets AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR vCantidadContenedores AS DECIMAL.
    FIND FIRST r_pallets_contenedores WHERE r_pallets_contenedor.id_tipo_pallet     = pTipoPallet
                                        AND r_pallets_contenedor.id_tipo_contenedor = 6
                                        NO-LOCK NO-ERROR.
    vCantidadContenedores = IF AVAILABLE r_pallets_contenedores THEN (pPallets / r_pallets_contenedores.palletsxcontenedor) ELSE 0.
  RETURN vCantidadContenedores.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCliente dTables  _DB-REQUIRED
FUNCTION getCliente RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST items_contratos OF rowObject NO-LOCK NO-ERROR.
  FIND FIRST contratos OF items_contratos NO-LOCK NO-ERROR.
  FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
  IF AVAILABLE clientes THEN 
    RETURN clientes.razon_social.
  ELSE
    RETURN "NO INFO".


  
  
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCoeficienteAduana dTables  _DB-REQUIRED
FUNCTION getCoeficienteAduana RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR vCoef AS DECIMAL DECIMALS 6 NO-UNDO.

    FIND LAST coeficientes_aduana NO-LOCK NO-ERROR.
    vCoef = IF AVAILABLE coeficientes_aduana THEN coeficientes_aduana.coeficiente ELSE 0.
    RETURN vCoef.   /* Function return value. */

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
  FIND FIRST clausulas WHERE clausulas.id_clausula = rowObject.id_condicion_venta
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDespachante dTables  _DB-REQUIRED
FUNCTION getDespachante RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  
  FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = rowObject.id_orden_entrega
                           NO-LOCK NO-ERROR.
  FIND FIRST despachantes WHERE despachantes.id_despachante = orden_entrega.id_despachante
                          NO-LOCK NO-ERROR.
  IF AVAILABLE despachantes THEN
    RETURN despachantes.descripcion.
  ELSE
    RETURN "NO INFO".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDestino dTables  _DB-REQUIRED
FUNCTION getDestino RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = rowObject.id_orden_entrega
                           NO-LOCK NO-ERROR.
  FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = orden_entrega.id_lugdes
                            NO-LOCK NO-ERROR.
  IF AVAILABLE lugar_descarga THEN
    RETURN lugar_descarga.descripcion.
  ELSE
    RETURN "NO INFO".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEstadoOE dTables  _DB-REQUIRED
FUNCTION getEstadoOE RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST estados_oe 
      WHERE estados_oe.id_estado = items_orden_entrega.id_estado
      NO-LOCK.
    cRet = estados_oe.descripcion.
  END.

 

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFobUniGal dTables  _DB-REQUIRED
FUNCTION getFobUniGal RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fUni AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cDat AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  cDat = DYNAMIC-FUNCTION('getUnidadPrecioContrato' IN hLib, rowObject.id_contrato, 
                                                             rowObject.id_tipo_contrato,
                                                             rowObject.anio,
                                                             rowObject.ITEM).

  IF cDat = ?  THEN RETURN 0.00.

  IF INTEGER(ENTRY(1, cDat, CHR(1))) = 3 THEN 
    fRet = rowObject.fob_ton / rowObject.TOTAL_galones.

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFobUniKil dTables  _DB-REQUIRED
FUNCTION getFobUniKil RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fUni AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cDat AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  cDat = DYNAMIC-FUNCTION('getUnidadPrecioContrato' IN hLib, rowObject.id_contrato, 
                                                             rowObject.id_tipo_contrato,
                                                             rowObject.anio,
                                                             rowObject.ITEM).

  IF cDat = ?  THEN RETURN 0.00.

  IF ENTRY(1, cDat, CHR(1)) = "2" THEN 
    fRet = rowObject.fob_ton / rowObject.kgs_netos_tambores.

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFobUniLib dTables  _DB-REQUIRED
FUNCTION getFobUniLib RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fUni AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fLib AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cDat AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  cDat = DYNAMIC-FUNCTION('getUnidadPrecioContrato' IN hLib, rowObject.id_contrato, 
                                                             rowObject.id_tipo_contrato,
                                                             rowObject.anio,
                                                             rowObject.ITEM).


  IF cDat = ?  THEN RETURN 0.00.

  IF ENTRY(1, cDat, CHR(1)) = "4" THEN DO:
    fLib = rowObject.kgs_netos_tambores * 2.20462.
    fRet = rowObject.fob_ton / fLib.
  END.

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFobUniTon dTables  _DB-REQUIRED
FUNCTION getFobUniTon RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fUni AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cDat AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.
  
  fUni = rowObject.fob_ton / (rowObject.kgs_netos_tambores / 1000).

  RETURN fUni.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGastosItemOE dTables  _DB-REQUIRED
FUNCTION getGastosItemOE RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  FOR EACH gastos_items_orden_entrega
      WHERE gastos_items_orden_entrega.id_orden_entrega = rowObject.id_orden_entrega
        AND gastos_items_orden_entrega.ITEM_oe          = rowObject.ITEM_oe
      NO-LOCK.
    fRet = fRet + gastos_items_orden_entrega.importe.
  END.


  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIPPAvailability dTables  _DB-REQUIRED
FUNCTION getIPPAvailability RETURNS LOGICAL
  ( INPUT pIPP AS ROWID,
    INPUT pPallets AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE BUFFER bbIPP FOR items_pedidos_packing.
DEFINE BUFFER bbItemOE FOR items_orden_entrega.
DEFINE VAR vPalletsOE AS INTEGER.
DEFINE VAR vPalletsRestantes AS INTEGER.

FIND FIRST bbIPP WHERE ROWID(bbIPP) = pIPP NO-LOCK NO-ERROR.
IF AVAILABLE bbIPP THEN DO:
    FOR EACH bbItemOE WHERE bbItemOE.id_empresa_ipp         = bbIPP.id_empresa
                        AND bbItemOE.id_orden_ipp           = bbIPP.id_orden
                        AND bbItemOE.id_punto_emisor_ipp    = bbIPP.id_punto_emisor
                        AND bbItemOE.item_ipp               = bbIPP.ITEM
                        NO-LOCK.
        vPalletsOE = vPalletsOE + bbItemOE.cantidad_pallets.
    END.
    vPalletsRestantes = bbIPP.cant_pallets - vPalletsOE.

    IF vPalletsRestantes >= pPallets THEN DO: 
        RETURN TRUE.
    END.
    ELSE DO: 
        MESSAGE "No se puede asignar " pPallets " pallets porque quedan " 
                                       vPalletsRestantes " pallets disponibles para despachar."
                                       VIEW-AS ALERT-BOX.
        RETURN FALSE.
    END.
END.
ELSE DO:
    MESSAGE "No selecciono la parte del pedido packing" VIEW-AS ALERT-BOX.
    RETURN FALSE.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosBrutos dTables  _DB-REQUIRED
FUNCTION getKilosBrutos RETURNS DECIMAL
  ( INPUT pEnvase AS INTEGER,
    INPUT pTotalCajas AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR vKilosNetos AS DECIMAL NO-UNDO.
    DEFINE VAR vKilosBrutos AS DECIMAL NO-UNDO.
    DEFINE VAR vTara AS DECIMAL NO-UNDO.

    vKilosNetos = getKilosNetos(INPUT pEnvase,
                                INPUT pTotalCajas).

    FIND FIRST envases_prod WHERE envases_prod.id_envase = pEnvase NO-LOCK NO-ERROR.
    vTara  = (IF AVAILABLE envases_prod THEN (envases_prod.Tara + 0.5) ELSE 0) * pTotalCajas.
    vKilosBrutos = vKilosNetos + vTara.
    RETURN vKilosBrutos.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosNetos dTables  _DB-REQUIRED
FUNCTION getKilosNetos RETURNS DECIMAL
  ( INPUT pEnvase AS INTEGER,
    INPUT pTotalCajas AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR vKilosNetos AS DECIMAL NO-UNDO.

    FIND FIRST r_envases_prod WHERE r_envases_prod.id_envase = pEnvase NO-LOCK NO-ERROR.
    vKilosNetos = (IF AVAILABLE r_envases_prod THEN r_envases_prod.Kilos_nominal ELSE 0) * pTotalCajas.
    RETURN vKilosNetos.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMonedaOrigen dTables  _DB-REQUIRED
FUNCTION getMonedaOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST items_contratos 
      WHERE items_contratos.id_contrato = rowObject.id_contrato 
        AND items_contratos.ITEM        = rowObject.ITEM
      NO-LOCK.
    FOR FIRST tipo_moneda 
        WHERE tipo_moneda.id_moneda = items_contratos.id_moneda_origen
        NO-LOCK.
      cRet = tipo_moneda.descripcion.
    END.
    
  END.
  
  RETURN cRet.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPesoLibras dTables  _DB-REQUIRED
FUNCTION getPesoLibras RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fLib AS DECIMAL    NO-UNDO.

  fLib = rowObject.kgs_netos_tambores * 2.20462.


  RETURN fLib.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrecioPorCaja dTables  _DB-REQUIRED
FUNCTION getPrecioPorCaja RETURNS DECIMAL
  ( INPUT pCliente AS INTEGER,
    INPUT pArticulo AS INTEGER,
    INPUT pEnvase AS INTEGER,
    INPUT pCalibre AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR vPrecioPorCaja AS DECIMAL NO-UNDO.

    FIND FIRST r_clientes_envases_precios WHERE r_clientes_envases_precios.id_cliente   = pCliente
                                            AND r_clientes_envases_precios.id_articulo  = pArticulo
                                            AND r_clientes_envases_precios.id_envase    = pEnvase
                                            AND r_clientes_envases_precios.calibre      = ""
                                            NO-LOCK NO-ERROR.
    IF AVAILABLE r_clientes_envases_precios THEN DO:
        /* ENTRA EN ESTA OPCION SI EL PRECIO ES IGUAL PARA TODOS LOS CALIBRE */
        vPrecioPorCaja = r_clientes_envases_precios.precio.
    END.
    ELSE DO:
        FIND FIRST r_clientes_envases_precios WHERE r_clientes_envases_precios.id_cliente   = pCliente
                                                AND r_clientes_envases_precios.id_articulo  = pArticulo
                                                AND r_clientes_envases_precios.id_envase    = pEnvase
                                                AND r_clientes_envases_precios.calibre      = pCalibre
                                                NO-LOCK NO-ERROR.
        IF AVAILABLE r_clientes_envases_precios THEN DO:
            /* ENTRA EN ESTA OPCION SI EL PRECIO CAMBIAN SEGUN LOS CALIBRES */
            vPrecioPorCaja = r_clientes_envases_precios.precio.
        END.
    END.
    
    RETURN vPrecioPorCaja.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrecioVenta dTables  _DB-REQUIRED
FUNCTION getPrecioVenta RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  

  FIND FIRST items_contratos 
      WHERE items_contratos.id_contrato = rowObject.id_contrato 
        AND items_contratos.ITEM        = rowObject.ITEM
      NO-LOCK NO-ERROR.
  IF AVAILABLE items_contratos THEN
    dRet = items_contratos.precio_origen.
  ELSE
    dRet = 0.00.

  RETURN dRet.

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

  FIND FIRST productos_terminados WHERE productos_terminado.id_articulo = rowObject.id_articulo
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidadVenta dTables  _DB-REQUIRED
FUNCTION getUnidadVenta RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  cRet = ''.
  FOR FIRST items_contratos 
      WHERE items_contratos.id_contrato = rowObject.id_contrato 
        AND items_contratos.ITEM        = rowObject.ITEM
      NO-LOCK.
    FOR FIRST tipo_unidad_venta 
        WHERE tipo_unidad_venta.id_tipo_unidad = items_contratos.id_tipo_unidad_venta_origen
        NO-LOCK.
      cRet = tipo_unidad_venta.descripcion.
    END.
    
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVapor dTables  _DB-REQUIRED
FUNCTION getVapor RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR .

  FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = rowObject.id_orden_entrega
                           NO-LOCK NO-ERROR.
  FIND FIRST vapores WHERE vapores.id_vapor = orden_entrega.id_vapor
                     NO-LOCK NO-ERROR.
  IF AVAILABLE vapores  THEN 
    RETURN vapores.descripcion.
  ELSE
    RETURN "NO INFO".


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION modifyCantidadPallets dTables  _DB-REQUIRED
FUNCTION modifyCantidadPallets RETURNS CHARACTER
  (INPUT rIPP   AS ROWID ,
   INPUT pCantidadPallets AS INTEGER,
   INPUT pSigno AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR vPalletsNuevos AS INTEGER.
DEFINE VAR vCajasPorPallets AS INTEGER NO-UNDO.
DEFINE VAR vTotalCajas AS INTEGER NO-UNDO.
DEFINE VAR vPrecioPorCaja AS DECIMAL NO-UNDO.
DEFINE VAR vCantidadContenedores AS DECIMAL NO-UNDO.
DEFINE VAR vFob AS DECIMAL NO-UNDO.
DEFINE VAR vFobPorCaja AS DECIMAL NO-UNDO.
DEFINE VAR vItemOE AS INTEGER NO-UNDO.
DEFINE VAR vTotalFactura AS DECIMAL NO-UNDO.
DEFINE VAR vKilosNetos AS DECIMAL NO-UNDO.
DEFINE VAR vKilosBrutos AS DECIMAL NO-UNDO.
DEFINE VAR vTara AS DECIMAL NO-UNDO.
DEFINE VAR vCoef AS DECIMAL DECIMALS 6 NO-UNDO.
DEFINE VAR vComision AS DECIMAL NO-UNDO.
DEFINE VAR vDerechoAduana AS DECIMAL NO-UNDO.
DEFINE VAR vReintegroAduana AS DECIMAL NO-UNDO.
DEFINE VAR vDerecho AS DECIMAL NO-UNDO.
DEFINE VAR vReintegro AS DECIMAL NO-UNDO.
DEFINE VAR vPalletsDisponibles AS LOGICAL NO-UNDO.
DEFINE VAR vPalletsAvailability AS INTEGER NO-UNDO.


vPalletsNuevos = IF pSigno = "-" THEN (rowObject.cantidad_pallets - pCantidadPallets) ELSE (rowObject.cantidad_pallets + pCantidadPallets).
vPalletsAvailability = IF pSigno = "-" THEN (-1 * pCantidadPallets) ELSE pCantidadPallets.

/*
MESSAGE "rowObject.cantidad_pallets " rowObject.cantidad_pallets 
        " pCantidadPallets " pCantidadPallets
        " vPalletsNuevos " vPalletsNuevos VIEW-AS ALERT-BOX.
*/

IF vPalletsNuevos >= 0 THEN DO:
    FIND FIRST items_pedidos_packing WHERE ROWID(items_pedidos_packing) = rIPP
                                     NO-LOCK NO-ERROR.
    
    IF AVAILABLE items_pedidos_packing THEN DO:

        FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega   = rowObject.id_orden_entrega
                                         AND items_orden_entrega.ITEM_oe            = rowObject.ITEM_oe
                                        NO-ERROR.
        IF AVAILABLE items_orden_entrega AND
           items_pedidos_packing.id_empresa      = items_orden_entrega.id_empresa_ipp AND
           items_pedidos_packing.id_orden        = items_orden_entrega.id_orden_ipp AND
           items_pedidos_packing.id_punto_emisor = items_orden_entrega.id_punto_emisor_ipp AND
           items_pedidos_packing.ITEM            = items_orden_entrega.item_ipp  THEN DO:

            vPalletsDisponibles = getIPPAvailability(INPUT rIPP,
                                                     INPUT vPalletsAvailability).
            
            IF items_pedidos_packing.cant_pallets >= vPalletsNuevos AND 
               vPalletsDisponibles THEN DO:

            END.
            ELSE DO:
                MESSAGE "Esta tratando de agregar mas Pallets de los que tiene la Parte del Pedido Packing"
                        VIEW-AS ALERT-BOX.
            END.

                vCajasPorPallets = getCajasPorPallets(INPUT rowObject.id_tipo_pallet,
                                                      INPUT rowObject.id_envase).
    
                vTotalCajas      = vCajasPorPallets * vPalletsNuevos.
                
                vPrecioPorCaja = getPrecioPorCaja(INPUT rowObject.id_cliente,
                                                  INPUT rowObject.id_articulo,
                                                  INPUT rowObject.id_envase,
                                                  INPUT items_pedidos_packing.calibre).
                
                vCantidadContenedores = getCantidadContenedores (INPUT rowObject.id_tipo_pallet,
                                                                 INPUT vPalletsNuevos ).
    
                vTotalFactura = vTotalCajas * vPrecioPorCaja.
                
                vKilosNetos = getKilosNetos(INPUT rowObject.id_envase,
                                            INPUT vTotalCajas).
                
                vKilosBrutos = getKilosBrutos(INPUT rowObject.id_envase,
                                              INPUT vTotalCajas).
    
                vCoef = getCoeficienteAduana().
                vComision = 0.    
                
                ASSIGN items_orden_entrega.cantidad_pallets       = vPalletsNuevos
                       items_orden_entrega.cajas_x_pallets        = vCajasPorPallets
                       items_orden_entrega.total_cajas            = vTotalCajas
                       items_orden_entrega.kgs_netos_tambores     = vKilosNetos
                       items_orden_entrega.kgs_brutos_tambores    = vKilosBrutos
                       items_orden_entrega.kilos_x_caja           = vKilosNetos / vTotalCajas
                       items_orden_entrega.precio_x_caja          = vPrecioPorCaja
                       items_orden_entrega.contenedores           = vCantidadContenedores
                       items_orden_entrega.TOTAL_factura          = vTotalFactura.
                
                /* ESTE PROGRAMA p_calculo-gastos_item_oe.p ES EL QUE ESTA EN USO 
                   EN LAS OE DE INDUSTRIA LO REBICE Y TENDRIA QUE ANDAR SIN PROBLEMAS 
                   PARA LAS OE DE FF.
                   ADRIAN CAROSSO 12-02-2004 */
                RUN p_calculo-gastos_item_oe.p(INPUT rowObject.id_orden_entrega,
                                               INPUT rowObject.ITEM_oe,
                                               INPUT rowObject.id_condicion_venta,
                                               INPUT vCantidadContenedores).
                
                vFob = calculaFobItemOE (INPUT rowObject.id_orden_entrega,
                                         INPUT rowObject.ITEM_oe,
                                         INPUT vTotalFactura).
                
                vDerechoAduana = vFob * vCoef.
                vDerecho = vDerechoAduana * 0.05.
    
                FIND LAST porcentaje_reint_articulo WHERE porcentaje_reint_articulo.id_articulo = 
                                                          rowObject.id_articulo
                                                    NO-LOCK NO-ERROR.
    
                vReintegroAduana = vFob - vComision.
                vReintegro = IF AVAILABLE porcentaje_reint_articulo THEN (vReintegroAduana * (porcentaje_reint_articulo.porcentaje / 100)) ELSE 0.
                
                ASSIGN items_orden_entrega.fob_ton                = vFob
                       items_orden_entrega.fob_unitario           = vFob / vTotalCajas
                       items_orden_entrega.coeficiente            = vCoef
                       items_orden_entrega.valor_aduana_derechos  = vDerechoAduana
                       items_orden_entrega.valor_aduana_reintegro = vReintegroAduana
                       items_orden_entrega.importe_derechos_exportacion = vDerecho
                       items_orden_entrega.importe_reintegro_fijo = vReintegro.
            
            
            
        END.
        ELSE DO: 
            IF AVAILABLE items_orden_entrega THEN 
                MESSAGE "Esta tratando de agregar pallets de una parte del Pedido Packing diferente"
                    VIEW-AS ALERT-BOX.
            ELSE
                MESSAGE "No encontro el ItemOE" VIEW-AS ALERT-BOX.

        END.
    END.
    ELSE MESSAGE "No selecciono ninguna parte de pedido packing" VIEW-AS ALERT-BOX.
END.
ELSE MESSAGE "Los tambores que usted selecciono dejan 0 pallets" VIEW-AS ALERT-BOX.

RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

