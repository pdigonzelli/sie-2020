&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
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

DEFINE VAR vIdOrdenEntrega AS INTEGER NO-UNDO.
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
&Scoped-define INTERNAL-TABLES items_orden_entrega calidades ~
productos_terminados contratos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio cantidad_tambores cerrado cert_fito cheque coeficiente contenedores~
 cotizacion c_fecha c_hora c_usuario fecha fob_ton fob_ton_u$s fob_unitario~
 grados_brix id_articulo id_calidad id_cliente id_condicion_venta~
 id_contrato id_envase id_estado id_frigorifico id_instrumento_pago id_marca~
 id_moneda id_moneda_cambio id_orden_entrega id_programa_despacho~
 id_punto_emisor_ipp id_tipo_contenedor id_tipo_contrato~
 id_tipo_orden_entrega id_tipo_plazo id_tipo_unidad_venta id_tipo_venta~
 importe_comisiones importe_derechos_exportacion importe_factura_dolar~
 importe_fob_dolar importe_origen importe_reintegro_fijo item item_oe~
 kgs_brutos_tambores kgs_netos_tambores modo_actualizacion observaciones~
 pendiente plazo precio_x_galon semana_entrega tambores_pedidos tipo_cambio~
 total_factura total_galones valor_aduana_derechos valor_aduana_reintegro~
 x_kilos
&Scoped-define ENABLED-FIELDS-IN-items_orden_entrega anio cantidad_tambores ~
cerrado cert_fito cheque coeficiente contenedores cotizacion c_fecha c_hora ~
c_usuario fecha fob_ton fob_ton_u$s fob_unitario grados_brix id_articulo ~
id_calidad id_cliente id_condicion_venta id_contrato id_envase id_estado ~
id_frigorifico id_instrumento_pago id_marca id_moneda id_moneda_cambio ~
id_orden_entrega id_programa_despacho id_punto_emisor_ipp ~
id_tipo_contenedor id_tipo_contrato id_tipo_orden_entrega id_tipo_plazo ~
id_tipo_unidad_venta id_tipo_venta importe_comisiones ~
importe_derechos_exportacion importe_factura_dolar importe_fob_dolar ~
importe_origen importe_reintegro_fijo item item_oe kgs_brutos_tambores ~
kgs_netos_tambores modo_actualizacion observaciones pendiente plazo ~
precio_x_galon semana_entrega tambores_pedidos tipo_cambio total_factura ~
total_galones valor_aduana_derechos valor_aduana_reintegro x_kilos 
&Scoped-Define DATA-FIELDS  anio cantidad_tambores cerrado cert_fito cheque coeficiente contenedores~
 cotizacion c_fecha c_hora c_usuario fecha fob_ton fob_ton_u$s fob_unitario~
 grados_brix id_articulo id_calidad id_cliente id_condicion_venta~
 id_contrato id_envase id_estado id_frigorifico id_instrumento_pago id_marca~
 id_moneda id_moneda_cambio id_orden_entrega id_programa_despacho~
 id_punto_emisor_ipp id_tipo_contenedor id_tipo_contrato~
 id_tipo_orden_entrega id_tipo_plazo id_tipo_unidad_venta id_tipo_venta~
 importe_comisiones importe_derechos_exportacion importe_factura_dolar~
 importe_fob_dolar importe_origen importe_reintegro_fijo item item_oe~
 kgs_brutos_tambores kgs_netos_tambores modo_actualizacion observaciones~
 pendiente plazo precio_x_galon semana_entrega tambores_pedidos tipo_cambio~
 total_factura total_galones valor_aduana_derechos valor_aduana_reintegro~
 x_kilos abreviatura abreviatura-2 orden_fabricacion Vapor Cliente
&Scoped-define DATA-FIELDS-IN-items_orden_entrega anio cantidad_tambores ~
cerrado cert_fito cheque coeficiente contenedores cotizacion c_fecha c_hora ~
c_usuario fecha fob_ton fob_ton_u$s fob_unitario grados_brix id_articulo ~
id_calidad id_cliente id_condicion_venta id_contrato id_envase id_estado ~
id_frigorifico id_instrumento_pago id_marca id_moneda id_moneda_cambio ~
id_orden_entrega id_programa_despacho id_punto_emisor_ipp ~
id_tipo_contenedor id_tipo_contrato id_tipo_orden_entrega id_tipo_plazo ~
id_tipo_unidad_venta id_tipo_venta importe_comisiones ~
importe_derechos_exportacion importe_factura_dolar importe_fob_dolar ~
importe_origen importe_reintegro_fijo item item_oe kgs_brutos_tambores ~
kgs_netos_tambores modo_actualizacion observaciones pendiente plazo ~
precio_x_galon semana_entrega tambores_pedidos tipo_cambio total_factura ~
total_galones valor_aduana_derechos valor_aduana_reintegro x_kilos 
&Scoped-define DATA-FIELDS-IN-calidades abreviatura 
&Scoped-define DATA-FIELDS-IN-productos_terminados abreviatura-2 
&Scoped-define DATA-FIELDS-IN-contratos orden_fabricacion 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.abreviatura-2 = productos_terminados.abreviatura
&Scoped-Define DATA-FIELD-DEFS "ditemsordenentregaindustria.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_orden_entrega NO-LOCK, ~
      EACH calidades OF items_orden_entrega NO-LOCK, ~
      EACH productos_terminados OF items_orden_entrega NO-LOCK, ~
      EACH contratos OF items_orden_entrega NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_orden_entrega NO-LOCK, ~
      EACH calidades OF items_orden_entrega NO-LOCK, ~
      EACH productos_terminados OF items_orden_entrega NO-LOCK, ~
      EACH contratos OF items_orden_entrega NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_orden_entrega calidades ~
productos_terminados contratos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_orden_entrega
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main calidades
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main productos_terminados
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main contratos


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCliente dTables  _DB-REQUIRED
FUNCTION getCliente RETURNS CHARACTER
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


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_orden_entrega, 
      calidades, 
      productos_terminados, 
      contratos SCROLLING.
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
     _TblList          = "general.items_orden_entrega,general.calidades OF general.items_orden_entrega,general.productos_terminados OF general.items_orden_entrega,industria.contratos OF general.items_orden_entrega"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.items_orden_entrega.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > general.items_orden_entrega.cantidad_tambores
"cantidad_tambores" "cantidad_tambores" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[3]   > general.items_orden_entrega.cerrado
"cerrado" "cerrado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[4]   > general.items_orden_entrega.cert_fito
"cert_fito" "cert_fito" ? ? "logical" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[5]   > general.items_orden_entrega.cheque
"cheque" "cheque" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[6]   > general.items_orden_entrega.coeficiente
"coeficiente" "coeficiente" ? ? "decimal" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[7]   > general.items_orden_entrega.contenedores
"contenedores" "contenedores" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[8]   > general.items_orden_entrega.cotizacion
"cotizacion" "cotizacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[9]   > general.items_orden_entrega.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[10]   > general.items_orden_entrega.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[11]   > general.items_orden_entrega.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[12]   > general.items_orden_entrega.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[13]   > general.items_orden_entrega.fob_ton
"fob_ton" "fob_ton" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[14]   > general.items_orden_entrega.fob_ton_u$s
"fob_ton_u$s" "fob_ton_u$s" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[15]   > general.items_orden_entrega.fob_unitario
"fob_unitario" "fob_unitario" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[16]   > general.items_orden_entrega.grados_brix
"grados_brix" "grados_brix" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[17]   > general.items_orden_entrega.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[18]   > general.items_orden_entrega.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[19]   > general.items_orden_entrega.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[20]   > general.items_orden_entrega.id_condicion_venta
"id_condicion_venta" "id_condicion_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
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
     _FldNameList[31]   > general.items_orden_entrega.id_punto_emisor_ipp
"id_punto_emisor_ipp" "id_punto_emisor_ipp" ? ? "integer" ? ? ? ? ? ? yes ? no 19.4 yes
     _FldNameList[32]   > general.items_orden_entrega.id_tipo_contenedor
"id_tipo_contenedor" "id_tipo_contenedor" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[33]   > general.items_orden_entrega.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[34]   > general.items_orden_entrega.id_tipo_orden_entrega
"id_tipo_orden_entrega" "id_tipo_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
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
     _FldNameList[46]   > general.items_orden_entrega.kgs_brutos_tambores
"kgs_brutos_tambores" "kgs_brutos_tambores" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[47]   > general.items_orden_entrega.kgs_netos_tambores
"kgs_netos_tambores" "kgs_netos_tambores" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[48]   > general.items_orden_entrega.modo_actualizacion
"modo_actualizacion" "modo_actualizacion" ? ? "logical" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[49]   > general.items_orden_entrega.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[50]   > general.items_orden_entrega.pendiente
"pendiente" "pendiente" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[51]   > general.items_orden_entrega.plazo
"plazo" "plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[52]   > general.items_orden_entrega.precio_x_galon
"precio_x_galon" "precio_x_galon" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.8 yes
     _FldNameList[53]   > general.items_orden_entrega.semana_entrega
"semana_entrega" "semana_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 18.6 yes
     _FldNameList[54]   > general.items_orden_entrega.tambores_pedidos
"tambores_pedidos" "tambores_pedidos" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[55]   > general.items_orden_entrega.tipo_cambio
"tipo_cambio" "tipo_cambio" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[56]   > general.items_orden_entrega.total_factura
"total_factura" "total_factura" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[57]   > general.items_orden_entrega.total_galones
"total_galones" "total_galones" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[58]   > general.items_orden_entrega.valor_aduana_derechos
"valor_aduana_derechos" "valor_aduana_derechos" ? ? "decimal" ? ? ? ? ? ? yes ? no 22.2 yes
     _FldNameList[59]   > general.items_orden_entrega.valor_aduana_reintegro
"valor_aduana_reintegro" "valor_aduana_reintegro" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.2 yes
     _FldNameList[60]   > general.items_orden_entrega.x_kilos
"x_kilos" "x_kilos" ? ? "logical" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[61]   > general.calidades.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[62]   > general.productos_terminados.abreviatura
"abreviatura" "abreviatura-2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[63]   > general.contratos.orden_fabricacion
"orden_fabricacion" "orden_fabricacion" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[64]   > "_<CALC>"
"getVapor()" "Vapor" "Vapor" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _FldNameList[65]   > "_<CALC>"
"getCliente()" "Cliente" "Cliente" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-datos dTables  _DB-REQUIRED
PROCEDURE carga-datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pOE AS ROWID NO-UNDO.
DEFINE VAR xQueryTambores AS CHAR NO-UNDO.
DEFINE BUFFER bbOE FOR orden_entrega.

FIND FIRST bbOE WHERE ROWID(bbOE) = pOE NO-LOCK NO-ERROR.
IF AVAILABLE bbOE THEN DO:
    vIdOrdenEntrega = bbOE.id_orden_entrega.
    xQueryTambores = "items_orden_entrega.id_orden_entrega = " + STRING(bbOE.id_orden_entrega) .

    {set queryWhere xQueryTambores}.
    DYNAMIC-FUNCTION ('openQuery':U).
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
         rowObject.Cliente = (getCliente())
         rowObject.Vapor = (getVapor())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-parte-contrato dTables  _DB-REQUIRED
PROCEDURE get-parte-contrato :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parte-contrato dTables  _DB-REQUIRED
PROCEDURE parte-contrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pItemOE AS INTEGER.

DEFINE VAR r AS ROWID.
DEFINE VAR vKilos AS DECIMAL.
DEFINE VAR vKilosBrutos AS DECIMAL.
DEFINE VAR vCantidadTambores AS DECIMAL.
DEFINE VAR vGall AS DECIMAL.
DEFINE VAR vGallBrix AS DECIMAL.
DEFINE VAR vTotalFactura AS DECIMAL.
DEFINE VAR hCon AS HANDLE.

DEFINE BUFFER bbItemsContrato FOR items_contratos.
DEFINE BUFFER bbContratos FOR contratos.

RUN wSeleccionItemsContratoOE.w( OUTPUT r).
FIND FIRST bbItemsContrato WHERE ROWID(bbItemsContrato) = r NO-LOCK NO-ERROR.
IF AVAILABLE bbItemsContrato THEN DO:

    FIND FIRST bbContratos OF bbItemsContrato no-lock no-error.
    IF AVAILABLE bbContratos THEN DO:

        rowObject.id_contrato           = bbContratos.id_contrato.
        rowObject.anio                  = bbContratos.anio.
        rowObject.item                  = bbItemsContrato.item.
        rowObject.id_tipo_contrato      = bbContratos.id_tipo_contrato.
        rowObject.semana_entrega        = bbItemsContrato.semana_entrega.
        rowObject.id_condicion_venta    = bbItemsContrato.id_clausula.
        rowObject.id_cliente            = bbContratos.id_cliente.
        rowObject.id_calidad            = bbItemsContrato.id_calidad.
        rowObject.id_articulo           = bbItemsContrato.id_articulo.
        rowObject.cantidad_tambores     = bbItemsContrato.cantidad.
        rowObject.tambores_pedidos      = bbItemsContrato.cantidad.
        rowObject.plazo                 = bbContratos.plazo.
        rowObject.id_tipo_plazo         = bbContratos.id_tipo_plazo.
        rowObject.id_instrumento_pago   = bbContratos.id_instrumento_pago.
        rowObject.cert_fito             = bbItemsContrato.cert_fito.
        
        RUN p_calcular-brix_item_oe.p (INPUT  vIdOrdenEntrega, 
                                       INPUT  pItemOE,
                                       OUTPUT vKilos,
                                       OUTPUT vKilosBrutos,
                                       OUTPUT vCantidadTambores,
                                       OUTPUT vGall,
                                       OUTPUT vGallBrix).
                                     
        rowObject.grados_brix           = vGallBrix.
        rowObject.kgs_netos_tambores    = vKilos.
        rowObject.kgs_brutos_tambores   = vKilosBrutos.
        rowObject.total_galones         = vGall.
        
        case bbItemsContrato.id_tipo_unidad_venta:
            when 1 then /* TONELADAS */ do:
                    vTotalFactura = ((vKilos / 1000) * bbItemsContrato.precio_origen).
                end.
            when 2 then /* KILOS */     do:
                    vTotalFactura = (vKilos * bbItemsContrato.precio_origen).                    
                end.
            when 3 then /* GALONES */   do:
                    vTotalFactura = (vGall * bbItemsContrato.precio_origen).
                end.
            when 4 then /* LIBRAS */    do:
                    vTotalFactura = ((vKilos * 2.20462) * bbItemsContrato.precio_origen).
                end.
       end case. 
               
       rowObject.total_factura  = vTotalFactura.
       
       hCon =  DYNAMIC-FUNCTION('getContainerHandle':U).

       RUN display-campos IN hCon.
       
       /*
       v_moneda         = items_contratos.id_moneda_origen.
       v_tipo_unidad    = items_contratos.id_tipo_unidad_venta_origen.
       v_precio         = items_contratos.precio_origen.
       v_tipo_venta     = items_contratos.id_tipo_venta.
         */
      END.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCliente dTables  _DB-REQUIRED
FUNCTION getCliente RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST clientes WHERE clientes.id_cliente = rowObject.id_cliente
                    NO-LOCK NO-ERROR.
  IF AVAILABLE clientes THEN
    RETURN clientes.razon_social.
  ELSE 
    RETURN "".

  

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

