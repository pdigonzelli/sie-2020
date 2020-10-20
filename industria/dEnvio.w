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

DEFINE  TEMP-TABLE ttReport
  RCODE-INFORMATION
  FIELD oe                AS CHARACTER COLUMN-LABEL "OE"
  FIELD id_cliente        AS CHARACTER COLUMN-LABEL "Cod.Cliente"
  FIELD cliente           AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD contrato          AS CHARACTER COLUMN-LABEL "Contrato"
  FIELD id_producto       AS CHARACTER COLUMN-LABEL "Cod.Producto"
  FIELD producto          AS CHARACTER COLUMN-LABEL "Producto"
  FIELD calidad           AS CHARACTER COLUMN-LABEL "Calidad"
  FIELD envase            AS CHARACTER COLUMN-LABEL "Envase"
  FIELD cantidad          AS CHARACTER COLUMN-LABEL "Cantidad"
  FIELD nro_lote          AS CHARACTER COLUMN-LABEL "Lote"
  FIELD id_vapor          AS CHARACTER COLUMN-LABEL "Cod.Vapor"
  FIELD vapor             AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD fecha_vapor       AS CHARACTER COLUMN-LABEL "Fecha Vapor"
  FIELD contenedor        AS CHARACTER COLUMN-LABEL "Contenedor"
  FIELD eta               AS CHARACTER COLUMN-LABEL "Eta"
  FIELD eta_real          AS CHARACTER COLUMN-LABEL "Eta Real"
  FIELD customs_entry     AS CHARACTER COLUMN-LABEL "Customs Entry"
  FIELD fda_released      AS CHARACTER COLUMN-LABEL "Fda Released"
  FIELD customs_released  AS CHARACTER COLUMN-LABEL "Customs Released"
  FIELD PICK_up           AS CHARACTER COLUMN-LABEL "Pick Up"
  FIELD delivered         AS CHARACTER COLUMN-LABEL "Delivered"
  FIELD id_destino        AS CHARACTER COLUMN-LABEL "Cod.Destino"
  FIELD destino           AS CHARACTER COLUMN-LABEL "Destino"
  FIELD factura           AS CHARACTER COLUMN-LABEL "Factura".

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
&Scoped-define INTERNAL-TABLES envios r_envio_oe orden_entrega ~
items_orden_entrega

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio c_fecha c_hora c_usuario descripcion_fechas_intermedias1~
 descripcion_fechas_intermedias2 descripcion_fechas_intermedias3~
 descripcion_fechas_intermedias4 descripcion_fechas_intermedias5~
 fechas_intermedias1 fechas_intermedias2 fechas_intermedias3~
 fechas_intermedias4 fechas_intermedias5 fecha_llegada_destino~
 fecha_salida_destino fecha_salida_origen id_destino id_envio id_origen~
 id_vapor semana fecha_custom_entry fecha_real_llegada~
 fecha_release_customer fecha_release_fda observaciones fecha_fda_notice~
 nro_fda_notice
&Scoped-define ENABLED-FIELDS-IN-envios anio c_fecha c_hora c_usuario ~
descripcion_fechas_intermedias1 descripcion_fechas_intermedias2 ~
descripcion_fechas_intermedias3 descripcion_fechas_intermedias4 ~
descripcion_fechas_intermedias5 fechas_intermedias1 fechas_intermedias2 ~
fechas_intermedias3 fechas_intermedias4 fechas_intermedias5 ~
fecha_llegada_destino fecha_salida_destino fecha_salida_origen id_destino ~
id_envio id_origen id_vapor semana fecha_custom_entry fecha_real_llegada ~
fecha_release_customer fecha_release_fda observaciones fecha_fda_notice ~
nro_fda_notice 
&Scoped-Define DATA-FIELDS  anio NroLote Envase Calidad Contrato Entrega Cliente DestinoFinal Producto~
 Carrier Vapor c_fecha c_hora c_usuario descripcion_fechas_intermedias1~
 descripcion_fechas_intermedias2 descripcion_fechas_intermedias3~
 descripcion_fechas_intermedias4 descripcion_fechas_intermedias5 origen~
 fechas_intermedias1 fechas_intermedias2 fechas_intermedias3~
 fechas_intermedias4 fechas_intermedias5 fecha_llegada_destino~
 fecha_salida_destino fecha_salida_origen id_destino id_envio id_origen~
 id_vapor semana fecha_custom_entry fecha_real_llegada~
 fecha_release_customer fecha_release_fda observaciones fecha_fda_notice~
 nro_fda_notice id_orden_entrega id_cliente id_articulo item_oe item~
 id_contrato destino
&Scoped-define DATA-FIELDS-IN-envios anio c_fecha c_hora c_usuario ~
descripcion_fechas_intermedias1 descripcion_fechas_intermedias2 ~
descripcion_fechas_intermedias3 descripcion_fechas_intermedias4 ~
descripcion_fechas_intermedias5 fechas_intermedias1 fechas_intermedias2 ~
fechas_intermedias3 fechas_intermedias4 fechas_intermedias5 ~
fecha_llegada_destino fecha_salida_destino fecha_salida_origen id_destino ~
id_envio id_origen id_vapor semana fecha_custom_entry fecha_real_llegada ~
fecha_release_customer fecha_release_fda observaciones fecha_fda_notice ~
nro_fda_notice 
&Scoped-define DATA-FIELDS-IN-r_envio_oe id_orden_entrega 
&Scoped-define DATA-FIELDS-IN-items_orden_entrega id_cliente id_articulo ~
item_oe item id_contrato 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST ~
  rowObject.descripcion_fechas_intermedias1 = envios.descripcion_fechas_intermedias[1]~
  rowObject.descripcion_fechas_intermedias2 = envios.descripcion_fechas_intermedias[2]~
  rowObject.descripcion_fechas_intermedias3 = envios.descripcion_fechas_intermedias[3]~
  rowObject.descripcion_fechas_intermedias4 = envios.descripcion_fechas_intermedias[4]~
  rowObject.descripcion_fechas_intermedias5 = envios.descripcion_fechas_intermedias[5]~
  rowObject.fechas_intermedias1 = envios.fechas_intermedias[1]~
  rowObject.fechas_intermedias2 = envios.fechas_intermedias[2]~
  rowObject.fechas_intermedias3 = envios.fechas_intermedias[3]~
  rowObject.fechas_intermedias4 = envios.fechas_intermedias[4]~
  rowObject.fechas_intermedias5 = envios.fechas_intermedias[5]
&Scoped-Define DATA-FIELD-DEFS "denvio.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH envios NO-LOCK, ~
      EACH r_envio_oe OF envios NO-LOCK, ~
      FIRST orden_entrega OF r_envio_oe NO-LOCK, ~
      FIRST items_orden_entrega OF orden_entrega NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH envios NO-LOCK, ~
      EACH r_envio_oe OF envios NO-LOCK, ~
      FIRST orden_entrega OF r_envio_oe NO-LOCK, ~
      FIRST items_orden_entrega OF orden_entrega NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main envios r_envio_oe orden_entrega ~
items_orden_entrega
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main envios
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main r_envio_oe
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main orden_entrega
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main items_orden_entrega


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalidad dTables  _DB-REQUIRED
FUNCTION getCalidad RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContrato dTables  _DB-REQUIRED
FUNCTION getContrato RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDeliveryCarrier dTables  _DB-REQUIRED
FUNCTION getDeliveryCarrier RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDestinoFinal dTables  _DB-REQUIRED
FUNCTION getDestinoFinal RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDireccionEntrega dTables  _DB-REQUIRED
FUNCTION getDireccionEntrega RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNroLote dTables  _DB-REQUIRED
FUNCTION getNroLote RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrigen dTables  _DB-REQUIRED
FUNCTION getOrigen RETURNS CHARACTER
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
      envios
    FIELDS(envios.anio
      envios.c_fecha
      envios.c_hora
      envios.c_usuario
      envios.descripcion_fechas_intermedias[1]
      envios.descripcion_fechas_intermedias[2]
      envios.descripcion_fechas_intermedias[3]
      envios.descripcion_fechas_intermedias[4]
      envios.descripcion_fechas_intermedias[5]
      envios.fechas_intermedias[1]
      envios.fechas_intermedias[2]
      envios.fechas_intermedias[3]
      envios.fechas_intermedias[4]
      envios.fechas_intermedias[5]
      envios.fecha_llegada_destino
      envios.fecha_salida_destino
      envios.fecha_salida_origen
      envios.id_destino
      envios.id_envio
      envios.id_origen
      envios.id_vapor
      envios.semana
      envios.fecha_custom_entry
      envios.fecha_real_llegada
      envios.fecha_release_customer
      envios.fecha_release_fda
      envios.observaciones
      envios.fecha_fda_notice
      envios.nro_fda_notice), 
      r_envio_oe
    FIELDS(r_envio_oe.id_orden_entrega), 
      orden_entrega
    FIELDS(), 
      items_orden_entrega
    FIELDS(items_orden_entrega.id_cliente
      items_orden_entrega.id_articulo
      items_orden_entrega.item_oe
      items_orden_entrega.item
      items_orden_entrega.id_contrato) SCROLLING.
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
     _TblList          = "general.envios,industria.r_envio_oe OF general.envios,general.orden_entrega OF general.r_envio_oe,general.items_orden_entrega OF general.orden_entrega"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = "USED, USED, FIRST USED, FIRST USED,,"
     _FldNameList[1]   > general.envios.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > "_<CALC>"
"getNroLote()" "NroLote" "NroLote" "XXXXXXXX/99" "character" ? ? ? ? ? ? no ? no 11 no
     _FldNameList[3]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[4]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[5]   > "_<CALC>"
"getContrato()" "Contrato" "Contrato" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[6]   > "_<CALC>"
"getDireccionEntrega()" "Entrega" "Entrega" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no
     _FldNameList[7]   > "_<CALC>"
"getCliente()" "Cliente" "Cliente" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[8]   > "_<CALC>"
"getDestinoFinal()" "DestinoFinal" "DestinoFinal" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[9]   > "_<CALC>"
"getProducto()" "Producto" "Producto" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[10]   > "_<CALC>"
"getDeliveryCarrier()" "Carrier" "Carrier" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[11]   > "_<CALC>"
"getVapor()" "Vapor" ? "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _FldNameList[12]   > general.envios.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[13]   > general.envios.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[14]   > general.envios.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[15]   > general.envios.descripcion_fechas_intermedias[1]
"descripcion_fechas_intermedias[1]" "descripcion_fechas_intermedias1" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[16]   > general.envios.descripcion_fechas_intermedias[2]
"descripcion_fechas_intermedias[2]" "descripcion_fechas_intermedias2" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[17]   > general.envios.descripcion_fechas_intermedias[3]
"descripcion_fechas_intermedias[3]" "descripcion_fechas_intermedias3" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[18]   > general.envios.descripcion_fechas_intermedias[4]
"descripcion_fechas_intermedias[4]" "descripcion_fechas_intermedias4" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[19]   > general.envios.descripcion_fechas_intermedias[5]
"descripcion_fechas_intermedias[5]" "descripcion_fechas_intermedias5" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[20]   > "_<CALC>"
"getOrigen()" "origen" "Origen" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no
     _FldNameList[21]   > general.envios.fechas_intermedias[1]
"fechas_intermedias[1]" "fechas_intermedias1" ? ? "date" ? ? ? ? ? ? yes ? no 18.4 yes
     _FldNameList[22]   > general.envios.fechas_intermedias[2]
"fechas_intermedias[2]" "fechas_intermedias2" ? ? "date" ? ? ? ? ? ? yes ? no 18.4 yes
     _FldNameList[23]   > general.envios.fechas_intermedias[3]
"fechas_intermedias[3]" "fechas_intermedias3" ? ? "date" ? ? ? ? ? ? yes ? no 18.4 yes
     _FldNameList[24]   > general.envios.fechas_intermedias[4]
"fechas_intermedias[4]" "fechas_intermedias4" ? ? "date" ? ? ? ? ? ? yes ? no 18.4 yes
     _FldNameList[25]   > general.envios.fechas_intermedias[5]
"fechas_intermedias[5]" "fechas_intermedias5" ? ? "date" ? ? ? ? ? ? yes ? no 18.4 yes
     _FldNameList[26]   > general.envios.fecha_llegada_destino
"fecha_llegada_destino" "fecha_llegada_destino" ? ? "date" ? ? ? ? ? ? yes ? no 22 yes
     _FldNameList[27]   > general.envios.fecha_salida_destino
"fecha_salida_destino" "fecha_salida_destino" ? ? "date" ? ? ? ? ? ? yes ? no 20.2 yes
     _FldNameList[28]   > general.envios.fecha_salida_origen
"fecha_salida_origen" "fecha_salida_origen" ? ? "date" ? ? ? ? ? ? yes ? no 19.2 yes
     _FldNameList[29]   > general.envios.id_destino
"id_destino" "id_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[30]   > general.envios.id_envio
"id_envio" "id_envio" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[31]   > general.envios.id_origen
"id_origen" "id_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[32]   > general.envios.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[33]   > general.envios.semana
"semana" "semana" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[34]   > general.envios.fecha_custom_entry
"fecha_custom_entry" "fecha_custom_entry" ? ? "date" ? ? ? ? ? ? yes ? no 19 yes
     _FldNameList[35]   > general.envios.fecha_real_llegada
"fecha_real_llegada" "fecha_real_llegada" ? ? "date" ? ? ? ? ? ? yes ? no 19.2 yes
     _FldNameList[36]   > general.envios.fecha_release_customer
"fecha_release_customer" "fecha_release_customer" "Fecha Released by Customer" ? "date" ? ? ? ? ? ? yes ? no 14.6 yes
     _FldNameList[37]   > general.envios.fecha_release_fda
"fecha_release_fda" "fecha_release_fda" "Fecha Released by FDA" ? "date" ? ? ? ? ? ? yes ? no 19.2 yes
     _FldNameList[38]   > general.envios.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
     _FldNameList[39]   > general.envios.fecha_fda_notice
"fecha_fda_notice" "fecha_fda_notice" "Fecha FDA Notice" ? "date" ? ? ? ? ? ? yes ? no 17.6 yes
     _FldNameList[40]   > general.envios.nro_fda_notice
"nro_fda_notice" "nro_fda_notice" "Nro FDA Notice" ? "character" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[41]   > general.r_envio_oe.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[42]   > general.items_orden_entrega.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 9 yes
     _FldNameList[43]   > general.items_orden_entrega.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[44]   > general.items_orden_entrega.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? no ? no 8.6 yes
     _FldNameList[45]   > general.items_orden_entrega.item
"item" "item" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[46]   > general.items_orden_entrega.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[47]   > "_<CALC>"
"getDestino()" "destino" "Destino" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.Calidad = (getCalidad())
         rowObject.Carrier = (getDeliveryCarrier())
         rowObject.Cliente = (getCliente())
         rowObject.Contrato = (getContrato())
         rowObject.destino = (getDestino())
         rowObject.DestinoFinal = (getDestinoFinal())
         rowObject.Entrega = (getDireccionEntrega())
         rowObject.Envase = (getEnvase())
         rowObject.NroLote = (getNroLote())
         rowObject.origen = (getOrigen())
         rowObject.Producto = (getProducto())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fecha_salida_destinoValidate dTables  _DB-REQUIRED
PROCEDURE fecha_salida_destinoValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcValorFecha AS CHARACTER NO-UNDO.
/*
  IF DATE(pcValorFecha) < RowObjUpd.fecha_llegada_destino THEN
    RETURN "Error en la fecha de Salida de Destino".
*/
  RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fecha_salida_origenValidate dTables  _DB-REQUIRED
PROCEDURE fecha_salida_origenValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcValorFecha AS CHARACTER NO-UNDO.

  IF DATE(pcValorFecha) < RowObjUpd.fecha_salida_origen THEN
    RETURN "Error en la fecha de Llegada al Destino".

  RETURN "".   /* Function return value. */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTTReport dTables  _DB-REQUIRED
PROCEDURE fillTTReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttReport.

  DEFINE BUFFER buRO FOR rowObject.
  
  FOR EACH buRO NO-LOCK.
    FIND FIRST contenedores WHERE contenedores.id_envio = buRO.id_envio
                            NO-LOCK NO-ERROR.
    
    CREATE ttReport.
    ASSIGN ttReport.oe                = STRING(buRO.id_orden_entrega)
           ttReport.id_cliente        = STRING(buRO.id_cliente)
           ttReport.cliente           = buRO.cliente
           ttReport.contrato          = buRO.Contrato
           ttReport.id_producto       = STRING(buRO.id_articulo)
           ttReport.producto          = buRO.producto
           ttReport.calidad           = Calidad
           ttReport.envase            = Envase
           ttReport.cantidad          = ""
           ttReport.nro_lote          = STRING(NroLote, "XXXXXX/XXXX")
           ttReport.id_vapor          = STRING(buRO.id_vapor)
           ttReport.vapor             = buRO.vapor
           ttReport.fecha_vapor       = ""
           ttReport.contenedor        = IF AVAILABLE contenedores THEN STRING(contenedores.nro_contenedor) ELSE ""
           ttReport.eta               = STRING(buRO.fecha_llegada_destino)
           ttReport.customs_entry     = STRING(buRO.fecha_custom_entry)
           ttReport.fda_released      = STRING(buRO.fecha_release_fda)
           ttReport.customs_released  = STRING(buRO.fecha_release_customer)
           ttReport.PICK_up           = IF AVAILABLE contenedores THEN STRING(contenedores.fecha_pickup) ELSE ""
           ttReport.delivered         = IF AVAILABLE contenedores THEN STRING(contenedores.fecha_entrega) ELSE ""
           ttReport.id_destino        = STRING(buRO.id_destino)
           ttReport.destino           = buRO.destino
           ttReport.factura           = "".

       

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalidad dTables  _DB-REQUIRED
FUNCTION getCalidad RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcRet AS CHARACTER  NO-UNDO.
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST r_envio_oe OF rowObject NO-LOCK NO-ERROR.
  FIND FIRST orden_entrega OF r_envio_oe NO-LOCK NO-ERROR.
  FIND FIRST tambores_industria OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
  IF AVAILABLE calidades THEN
    RETURN calidades.descripcion.
  ELSE
    RETURN "NO-INFO".

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
  DEFINE VARIABLE vcCliente AS CHARACTER INITIAL "No Cust. Info." NO-UNDO.
 
  FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = rowObject.id_orden_entrega NO-LOCK NO-ERROR.
  IF AVAILABLE items_orden_entrega THEN DO:
    FIND FIRST clientes WHERE clientes.id_cliente = items_orden_entrega.id_cliente NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
      vcCliente = clientes.razon_social.
    END.
  END.

  RETURN vcCliente.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContrato dTables  _DB-REQUIRED
FUNCTION getContrato RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.

  FIND FIRST packing_list OF envios NO-LOCK NO-ERROR.
  FIND FIRST contratos OF packing_list NO-LOCK NO-ERROR.
  IF AVAILABLE contratos  THEN DO:
    RETURN contratos.id_contrato.
  END.
  ELSE 
    RETURN "NO-INFO".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDeliveryCarrier dTables  _DB-REQUIRED
FUNCTION getDeliveryCarrier RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcCarrier AS CHARACTER INITIAL "No Carrier Info." NO-UNDO.

  FIND FIRST contratos WHERE contratos.id_contrato = rowObject.id_contrato NO-LOCK NO-ERROR.
  IF AVAILABLE contratos THEN DO:
    FIND FIRST contactos_industria WHERE contratos.id_delivery_carrier = contactos_industria.id_contacto NO-LOCK NO-ERROR.
    IF AVAILABLE contactos_industria THEN DO:
      vcCarrier = contactos_industria.razon_social.
    END.
  END.
  
  RETURN vcCarrier.

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
  FIND destinos OF RowObject NO-LOCK NO-ERROR.
  IF AVAILABLE destinos THEN
      RETURN destinos.descripcion.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDestinoFinal dTables  _DB-REQUIRED
FUNCTION getDestinoFinal RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcDestino AS CHARACTER INITIAL "No Final Dest. Info"  NO-UNDO.

  FIND FIRST items_contratos WHERE items_contratos.id_contrato = rowObject.id_contrato 
                               AND items_contratos.ITEM = rowObject.item
                             NO-LOCK NO-ERROR.
  IF AVAILABLE items_contratos THEN DO:
    FIND destinos WHERE items_contratos.id_destino = destinos.id_destino NO-LOCK NO-ERROR.
    IF AVAILABLE destinos THEN
      vcDestino = destinos.descripcion.
    END.

  RETURN vcDestino.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDireccionEntrega dTables  _DB-REQUIRED
FUNCTION getDireccionEntrega RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcDelivery AS CHARACTER INITIAL "No Delivery Address Info." NO-UNDO.

  FIND FIRST contratos WHERE contratos.id_contrato = rowObject.id_contrato NO-LOCK NO-ERROR.
  IF AVAILABLE contratos THEN DO:
    FIND FIRST contactos_industria WHERE contratos.id_direccion_descarga = contactos_industria.id_contacto NO-LOCK NO-ERROR.
    IF AVAILABLE contactos_industria THEN DO:
      vcDelivery = contactos_industria.direccion.
    END.
  END.
  
  RETURN vcDelivery. 

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
  FIND FIRST r_envio_oe OF rowObject NO-LOCK NO-ERROR.
  FIND FIRST orden_entrega OF r_envio_oe NO-LOCK NO-ERROR.
  FIND FIRST tambores_industria OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.descripcion.
  ELSE
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNroLote dTables  _DB-REQUIRED
FUNCTION getNroLote RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST r_envio_oe OF rowObject NO-LOCK NO-ERROR.
  FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = r_envio_oe.id_orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST tambores_industria OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST lotes_ubicacion OF tambores_industria NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_ubicacion THEN
    RETURN lotes_ubicacion.lote.
  ELSE
    RETURN "". 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrigen dTables  _DB-REQUIRED
FUNCTION getOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND lugar_descarga WHERE lugar_descarga.id_lugdes = RowObject.id_origen NO-LOCK NO-ERROR.
  IF AVAILABLE lugar_descarga  THEN
      RETURN lugar_descarga.descripcion.

  RETURN "".   /* Function return value. */

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
  DEFINE VARIABLE vcProducto AS CHARACTER INITIAL "No Product Info" NO-UNDO.

  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = rowObject.id_articulo NO-LOCK NO-ERROR.
  IF AVAILABLE productos_terminados THEN DO:
    vcProducto = productos_terminados.descripcion.
  END.

  RETURN vcProducto.

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
  FIND vapores OF RowObject NO-LOCK NO-ERROR.
  IF AVAILABLE vapores THEN
      RETURN vapores.descripcion.
  RETURN "Sin vapor".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

