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
  FIELD vapor         AS INTEGER   COLUMN-LABEL "Vapor"
  FIELD Fecha         AS INTEGER   COLUMN-LABEL "Fecha"
  FIELD cliente       AS INTEGER   COLUMN-LABEL "Cliente"
  FIELD producto      AS INTEGER   COLUMN-LABEL "Producto"
  FIELD destino       AS INTEGER   COLUMN-LABEL "Destino"
  FIELD condicion_vta AS INTEGER   COLUMN-LABEL "Condicion Vta"
  FIELD compania      AS INTEGER   COLUMN-LABEL "Cia. Maritima"
  FIELD contenedores  AS INTEGER   COLUMN-LABEL "Tot. Contenedores"
  FIELD gasto         AS INTEGER   COLUMN-LABEL "Gasto"
  FIELD importe       AS INTEGER   COLUMN-LABEL "Importe".

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
&Scoped-define INTERNAL-TABLES orden_entrega

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_orden_entrega anio id_tipo_orden_entrega cerrado fecha fecha_embarque~
 id_despachante id_destino id_estado id_vapor semana_embarque~
 id_condicion_venta contenedores semana_entrega id_agencia Clausula~
 cajas_x_pallets cantidad_tambores Agencia cheque cotizacion c_fecha c_hora~
 c_usuario fecha_arribo fob_ton fob_ton_u$s fob_unitario forma_transporte~
 grados_brix id_articulo id_calidad id_cliente id_contrato id_destino_final~
 id_frigorifico id_instrumento_pago id_lugdes id_marca id_moneda~
 id_tipo_contenedor id_tipo_contrato id_tipo_plazo id_tipo_unidad_venta~
 id_tipo_venta importe_comisiones importe_origen item kgs_brutos_tambores~
 kgs_netos_tambores modo_actualizacion observaciones pallets pedido_fondos~
 pendiente plazo precio_x_galon tambores_pedidos total_factura total_galones~
 booking fecha_cut_off
&Scoped-define ENABLED-FIELDS-IN-orden_entrega id_orden_entrega anio ~
id_tipo_orden_entrega cerrado fecha fecha_embarque id_despachante ~
id_destino id_estado id_vapor semana_embarque id_condicion_venta ~
contenedores semana_entrega id_agencia cajas_x_pallets cantidad_tambores ~
cheque cotizacion c_fecha c_hora c_usuario fecha_arribo fob_ton fob_ton_u$s ~
fob_unitario forma_transporte grados_brix id_articulo id_calidad id_cliente ~
id_contrato id_destino_final id_frigorifico id_instrumento_pago id_lugdes ~
id_marca id_moneda id_tipo_contenedor id_tipo_contrato id_tipo_plazo ~
id_tipo_unidad_venta id_tipo_venta importe_comisiones importe_origen item ~
kgs_brutos_tambores kgs_netos_tambores modo_actualizacion observaciones ~
pallets pedido_fondos pendiente plazo precio_x_galon tambores_pedidos ~
total_factura total_galones booking fecha_cut_off 
&Scoped-Define DATA-FIELDS  id_orden_entrega anio id_tipo_orden_entrega cerrado fecha fecha_embarque~
 id_despachante id_destino id_estado id_vapor semana_embarque~
 id_condicion_venta contenedores semana_entrega id_agencia Clausula~
 cajas_x_pallets cantidad_tambores Agencia cheque cotizacion c_fecha c_hora~
 c_usuario fecha_arribo fob_ton fob_ton_u$s fob_unitario forma_transporte~
 grados_brix id_articulo id_calidad id_cliente id_contrato id_destino_final~
 id_frigorifico id_instrumento_pago id_lugdes id_marca id_moneda~
 id_tipo_contenedor id_tipo_contrato id_tipo_plazo id_tipo_unidad_venta~
 id_tipo_venta importe_comisiones importe_origen item kgs_brutos_tambores~
 kgs_netos_tambores modo_actualizacion observaciones pallets pedido_fondos~
 pendiente plazo precio_x_galon tambores_pedidos total_factura total_galones~
 booking fecha_cut_off
&Scoped-define DATA-FIELDS-IN-orden_entrega id_orden_entrega anio ~
id_tipo_orden_entrega cerrado fecha fecha_embarque id_despachante ~
id_destino id_estado id_vapor semana_embarque id_condicion_venta ~
contenedores semana_entrega id_agencia cajas_x_pallets cantidad_tambores ~
cheque cotizacion c_fecha c_hora c_usuario fecha_arribo fob_ton fob_ton_u$s ~
fob_unitario forma_transporte grados_brix id_articulo id_calidad id_cliente ~
id_contrato id_destino_final id_frigorifico id_instrumento_pago id_lugdes ~
id_marca id_moneda id_tipo_contenedor id_tipo_contrato id_tipo_plazo ~
id_tipo_unidad_venta id_tipo_venta importe_comisiones importe_origen item ~
kgs_brutos_tambores kgs_netos_tambores modo_actualizacion observaciones ~
pallets pedido_fondos pendiente plazo precio_x_galon tambores_pedidos ~
total_factura total_galones booking fecha_cut_off 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dordenentrega.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH orden_entrega ~
      WHERE orden_entrega.fecha >= DATE("01/01/2006") ~
 AND orden_entrega.id_tipo_orden_entrega = 1 NO-LOCK ~
    BY orden_entrega.id_orden_entrega DESCENDING ~
       BY orden_entrega.semana_entrega DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH orden_entrega ~
      WHERE orden_entrega.fecha >= DATE("01/01/2006") ~
 AND orden_entrega.id_tipo_orden_entrega = 1 NO-LOCK ~
    BY orden_entrega.id_orden_entrega DESCENDING ~
       BY orden_entrega.semana_entrega DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main orden_entrega
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main orden_entrega


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAgencia dTables  _DB-REQUIRED
FUNCTION getAgencia RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCondicionVenta dTables  _DB-REQUIRED
FUNCTION getCondicionVenta RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFob dTables  _DB-REQUIRED
FUNCTION getFob RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroOE dTables  _DB-REQUIRED
FUNCTION getNextNroOE RETURNS INTEGER
  (piTipo AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalFactura dTables  _DB-REQUIRED
FUNCTION getTotalFactura RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalGastos dTables  _DB-REQUIRED
FUNCTION getTotalGastos RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      orden_entrega SCROLLING.
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
     _TblList          = "general.orden_entrega"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "general.orden_entrega.id_orden_entrega|no,general.orden_entrega.semana_entrega|no"
     _Where[1]         = "general.orden_entrega.fecha >= DATE(""01/01/2006"")
 AND general.orden_entrega.id_tipo_orden_entrega = 1"
     _FldNameList[1]   > general.orden_entrega.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[2]   > general.orden_entrega.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[3]   > general.orden_entrega.id_tipo_orden_entrega
"id_tipo_orden_entrega" "id_tipo_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[4]   > general.orden_entrega.cerrado
"cerrado" "cerrado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[5]   > general.orden_entrega.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[6]   > general.orden_entrega.fecha_embarque
"fecha_embarque" "fecha_embarque" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[7]   > general.orden_entrega.id_despachante
"id_despachante" "id_despachante" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[8]   > general.orden_entrega.id_destino
"id_destino" "id_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[9]   > general.orden_entrega.id_estado
"id_estado" "id_estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[10]   > general.orden_entrega.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[11]   > general.orden_entrega.semana_embarque
"semana_embarque" "semana_embarque" ? ? "integer" ? ? ? ? ? ? yes ? no 17.8 yes
     _FldNameList[12]   > general.orden_entrega.id_condicion_venta
"id_condicion_venta" "id_condicion_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[13]   > general.orden_entrega.contenedores
"contenedores" "contenedores" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[14]   > general.orden_entrega.semana_entrega
"semana_entrega" "semana_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 18.6 yes
     _FldNameList[15]   > general.orden_entrega.id_agencia
"id_agencia" "id_agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[16]   > "_<CALC>"
"getCondicionVenta()" "Clausula" "Clausula" "x(20)" "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[17]   > general.orden_entrega.cajas_x_pallets
"cajas_x_pallets" "cajas_x_pallets" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[18]   > general.orden_entrega.cantidad_tambores
"cantidad_tambores" "cantidad_tambores" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[19]   > "_<CALC>"
"getAgencia()" "Agencia" "Agencia" "x(25)" "character" ? ? ? ? ? ? yes ? no 25 no
     _FldNameList[20]   > general.orden_entrega.cheque
"cheque" "cheque" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[21]   > general.orden_entrega.cotizacion
"cotizacion" "cotizacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[22]   > general.orden_entrega.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[23]   > general.orden_entrega.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[24]   > general.orden_entrega.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[25]   > general.orden_entrega.fecha_arribo
"fecha_arribo" "fecha_arribo" ? ? "date" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[26]   > general.orden_entrega.fob_ton
"fob_ton" "fob_ton" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[27]   > general.orden_entrega.fob_ton_u$s
"fob_ton_u$s" "fob_ton_u$s" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[28]   > general.orden_entrega.fob_unitario
"fob_unitario" "fob_unitario" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[29]   > general.orden_entrega.forma_transporte
"forma_transporte" "forma_transporte" ? ? "logical" ? ? ? ? ? ? yes ? no 19.6 yes
     _FldNameList[30]   > general.orden_entrega.grados_brix
"grados_brix" "grados_brix" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[31]   > general.orden_entrega.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[32]   > general.orden_entrega.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[33]   > general.orden_entrega.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[34]   > general.orden_entrega.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[35]   > general.orden_entrega.id_destino_final
"id_destino_final" "id_destino_final" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[36]   > general.orden_entrega.id_frigorifico
"id_frigorifico" "id_frigorifico" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[37]   > general.orden_entrega.id_instrumento_pago
"id_instrumento_pago" "id_instrumento_pago" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[38]   > general.orden_entrega.id_lugdes
"id_lugdes" "id_lugdes" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[39]   > general.orden_entrega.id_marca
"id_marca" "id_marca" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[40]   > general.orden_entrega.id_moneda
"id_moneda" "id_moneda" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[41]   > general.orden_entrega.id_tipo_contenedor
"id_tipo_contenedor" "id_tipo_contenedor" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[42]   > general.orden_entrega.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[43]   > general.orden_entrega.id_tipo_plazo
"id_tipo_plazo" "id_tipo_plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[44]   > general.orden_entrega.id_tipo_unidad_venta
"id_tipo_unidad_venta" "id_tipo_unidad_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[45]   > general.orden_entrega.id_tipo_venta
"id_tipo_venta" "id_tipo_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 4.6 yes
     _FldNameList[46]   > general.orden_entrega.importe_comisiones
"importe_comisiones" "importe_comisiones" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.2 yes
     _FldNameList[47]   > general.orden_entrega.importe_origen
"importe_origen" "importe_origen" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[48]   > general.orden_entrega.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[49]   > general.orden_entrega.kgs_brutos_tambores
"kgs_brutos_tambores" "kgs_brutos_tambores" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[50]   > general.orden_entrega.kgs_netos_tambores
"kgs_netos_tambores" "kgs_netos_tambores" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[51]   > general.orden_entrega.modo_actualizacion
"modo_actualizacion" "modo_actualizacion" ? ? "logical" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[52]   > general.orden_entrega.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[53]   > general.orden_entrega.pallets
"pallets" "pallets" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[54]   > general.orden_entrega.pedido_fondos
"pedido_fondos" "pedido_fondos" ? ? "logical" ? ? ? ? ? ? yes ? no 17.2 yes
     _FldNameList[55]   > general.orden_entrega.pendiente
"pendiente" "pendiente" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[56]   > general.orden_entrega.plazo
"plazo" "plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[57]   > general.orden_entrega.precio_x_galon
"precio_x_galon" "precio_x_galon" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.8 yes
     _FldNameList[58]   > general.orden_entrega.tambores_pedidos
"tambores_pedidos" "tambores_pedidos" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[59]   > general.orden_entrega.total_factura
"total_factura" "total_factura" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[60]   > general.orden_entrega.total_galones
"total_galones" "total_galones" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[61]   > general.orden_entrega.booking
"booking" "booking" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[62]   > general.orden_entrega.fecha_cut_off
"fecha_cut_off" "fecha_cut_off" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
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
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  FIND LAST RowObjUpd NO-ERROR.
  
  /*add*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C". 

    RUN createDocumentosOE IN hLib (rowObjUpd.id_orden_entrega).
  
    ASSIGN rowObjUpd.anio      = YEAR(TODAY)
           rowObjUpd.c_usuario = USERID("userdb")
           rowObjUpd.c_fecha   = TODAY
           rowObjUpd.c_hora    = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",anio,c_usuario,c_fecha,c_hora".    
  END.
  
  /*update*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U". 

    FOR FIRST packing_list
        WHERE packing_list.id_orden_entrega = rowObjUpd.id_orden_entrega
        NO-LOCK.

      ASSIGN rowObjUpd.fecha_embarque = ventas.packing_list.fecha_salida_vapor
             rowObjUpd.fecha_arribo   = packing_list.fecha_salida_vapor + ventas.packing_list.dias_transito
             .

      rowObjUpd.changedFields = rowObjUpd.changedFields + ",fecha_embarque,fecha_arribo".
    END.

    RUN setSemanaOE IN hLib (rowObjUpd.id_orden_entrega, rowObjUpd.semana_embarque).

    ASSIGN rowObjUpd.c_usuario = USERID("userdb")
           rowObjUpd.c_fecha   = TODAY
           rowObjUpd.c_hora    = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".        
  END.
  
  /*delete*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D". 
    FOR EACH items_orden_entrega
        WHERE items_orden_entrega.id_orden_entrega = rowObjUpd.id_orden_entrega.
      FOR EACH gastos_items_orden_entrega
            OF items_orden_entrega.
        DELETE gastos_items_orden_entrega.
      END.
      DELETE items_orden_entrega.
    END.

    FOR EACH gastos_orden_entrega
        WHERE gastos_orden_entrega.id_orden_entrega = rowObjUpd.id_orden_entrega.
      DELETE gastos_orden_entrega.
    END.
  
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
         rowObject.Agencia = (getAgencia())
         rowObject.Clausula = (getCondicionVenta())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAgencia dTables  _DB-REQUIRED
FUNCTION getAgencia RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST agencias WHERE agencias.id_agencia = rowObject.id_agencia NO-LOCK.
    cRet = STRING(rowObject.id_agencia) + " - " + agencias.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCondicionVenta dTables  _DB-REQUIRED
FUNCTION getCondicionVenta RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = rowObject.id_orden_entrega 
                                NO-LOCK.
    FOR FIRST clausulas WHERE clausulas.id_clausula = items_orden_entrega.id_condicion_venta NO-LOCK.
      cRet = clausulas.descripcion.
    END.
  END.
  
                                      
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFob dTables  _DB-REQUIRED
FUNCTION getFob RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroOE dTables  _DB-REQUIRED
FUNCTION getNextNroOE RETURNS INTEGER
  (piTipo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  piTipo = 1 => exportacion; piTipo = 2 => mercado interno
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p'). 
  DELETE OBJECT hLibCom.

  RETURN DYNAMIC-FUNCTION('getNextNroOE' IN hLib, piTipo).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalFactura dTables  _DB-REQUIRED
FUNCTION getTotalFactura RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalGastos dTables  _DB-REQUIRED
FUNCTION getTotalGastos RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

