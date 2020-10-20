&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
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
&Scoped-define INTERNAL-TABLES remitos confirmacion_remito

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_sucursal id_tipo_movsto nro nro_comprobante anio_of anio_produccion~
 bultos chofer c_fecha c_hora c_usuario estado fecha fecha_descarga~
 fecha_llegada fecha_proceso fecha_salida hora_descarga hora_llegada~
 hora_salida id_agencia id_ciaseg id_cliente id_cliente_remito id_destino~
 id_destino_final id_galpon id_lugdes id_operacion id_operacion_compra~
 id_orden_entrega id_proveedor id_punto_venta id_tipocomp_compra id_vapor~
 impresion item_oe mercado nromov_compra nro_contenedor nro_iascav~
 nro_orden_fab nro_ord_carga nro_per_embarque nro_precinto observaciones~
 observ_ordenentrega orden_embarque orden_fabricacion Pat_acopla pat_chasis~
 peso Peso_bruto Peso_neto Tara temp_llegada temp_salida tipo_remito~
 Valor_declarado FechaArribo
&Scoped-define ENABLED-FIELDS-IN-remitos id_sucursal id_tipo_movsto nro ~
nro_comprobante anio_of anio_produccion bultos chofer c_fecha c_hora ~
c_usuario estado fecha fecha_descarga fecha_llegada fecha_proceso ~
fecha_salida hora_descarga hora_llegada hora_salida id_agencia id_ciaseg ~
id_cliente id_cliente_remito id_destino id_destino_final id_galpon ~
id_lugdes id_operacion id_operacion_compra id_orden_entrega id_proveedor ~
id_punto_venta id_tipocomp_compra id_vapor impresion item_oe mercado ~
nromov_compra nro_contenedor nro_iascav nro_orden_fab nro_ord_carga ~
nro_per_embarque nro_precinto observaciones observ_ordenentrega ~
orden_embarque orden_fabricacion Pat_acopla pat_chasis peso Peso_bruto ~
Peso_neto Tara temp_llegada temp_salida tipo_remito Valor_declarado 
&Scoped-define ENABLED-FIELDS-IN-confirmacion_remito FechaArribo 
&Scoped-Define DATA-FIELDS  id_sucursal id_tipo_movsto nro nro_comprobante anio_of anio_produccion~
 bultos chofer c_fecha c_hora c_usuario estado fecha fecha_descarga~
 fecha_llegada fecha_proceso fecha_salida hora_descarga hora_llegada~
 hora_salida id_agencia id_ciaseg id_cliente id_cliente_remito id_destino~
 id_destino_final id_galpon id_lugdes id_operacion id_operacion_compra~
 id_orden_entrega id_proveedor id_punto_venta id_tipocomp_compra id_vapor~
 impresion item_oe mercado nromov_compra nro_contenedor nro_iascav~
 nro_orden_fab nro_ord_carga nro_per_embarque nro_precinto observaciones~
 observ_ordenentrega orden_embarque orden_fabricacion Pat_acopla pat_chasis~
 peso Peso_bruto Peso_neto Tara temp_llegada temp_salida tipo_remito~
 Valor_declarado LugarDescarga FechaArribo Destino
&Scoped-define DATA-FIELDS-IN-remitos id_sucursal id_tipo_movsto nro ~
nro_comprobante anio_of anio_produccion bultos chofer c_fecha c_hora ~
c_usuario estado fecha fecha_descarga fecha_llegada fecha_proceso ~
fecha_salida hora_descarga hora_llegada hora_salida id_agencia id_ciaseg ~
id_cliente id_cliente_remito id_destino id_destino_final id_galpon ~
id_lugdes id_operacion id_operacion_compra id_orden_entrega id_proveedor ~
id_punto_venta id_tipocomp_compra id_vapor impresion item_oe mercado ~
nromov_compra nro_contenedor nro_iascav nro_orden_fab nro_ord_carga ~
nro_per_embarque nro_precinto observaciones observ_ordenentrega ~
orden_embarque orden_fabricacion Pat_acopla pat_chasis peso Peso_bruto ~
Peso_neto Tara temp_llegada temp_salida tipo_remito Valor_declarado 
&Scoped-define DATA-FIELDS-IN-confirmacion_remito FechaArribo 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.FechaArribo = confirmacion_remito.fecha
&Scoped-Define DATA-FIELD-DEFS "dqrtosnoconfirmados.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH remitos ~
      WHERE remitos.fecha >= 11/09/2006 ~
  AND remitos.mercado <> 0 ~
 AND remitos.nro > 0 ~
 AND remitos.id_operacion = 311 ~
 AND remitos.fecha_proceso <> ? NO-LOCK, ~
      EACH confirmacion_remito WHERE confirmacion_remito.id_sucursal_remito = remitos.id_sucursal ~
  AND confirmacion_remito.id_tipo_movsto = remitos.id_tipo_movsto ~
  AND confirmacion_remito.nro_remito = remitos.nro ~
      AND confirmacion_remito.fecha = date("") OUTER-JOIN NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH remitos ~
      WHERE remitos.fecha >= 11/09/2006 ~
  AND remitos.mercado <> 0 ~
 AND remitos.nro > 0 ~
 AND remitos.id_operacion = 311 ~
 AND remitos.fecha_proceso <> ? NO-LOCK, ~
      EACH confirmacion_remito WHERE confirmacion_remito.id_sucursal_remito = remitos.id_sucursal ~
  AND confirmacion_remito.id_tipo_movsto = remitos.id_tipo_movsto ~
  AND confirmacion_remito.nro_remito = remitos.nro ~
      AND confirmacion_remito.fecha = date("") OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main remitos confirmacion_remito
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main remitos
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main confirmacion_remito


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getConfirmacionArribo dTables  _DB-REQUIRED
FUNCTION getConfirmacionArribo RETURNS DATE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLugDes dTables  _DB-REQUIRED
FUNCTION getLugDes RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      remitos, 
      confirmacion_remito SCROLLING.
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
     _TblList          = "general.remitos,industria.confirmacion_remito WHERE general.remitos ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _Where[1]         = "general.remitos.fecha >= 11/09/2006
  AND general.remitos.mercado <> 0
 AND general.remitos.nro > 0
 AND general.remitos.id_operacion = 311
 AND general.remitos.fecha_proceso <> ?"
     _JoinCode[2]      = "general.confirmacion_remito.id_sucursal_remito = general.remitos.id_sucursal
  AND general.confirmacion_remito.id_tipo_movsto = general.remitos.id_tipo_movsto
  AND general.confirmacion_remito.nro_remito = general.remitos.nro"
     _Where[2]         = "general.confirmacion_remito.fecha = date("""")"
     _FldNameList[1]   > general.remitos.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[2]   > general.remitos.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[3]   > general.remitos.nro
"nro" "nro" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[4]   > general.remitos.nro_comprobante
"nro_comprobante" "nro_comprobante" ? ? "character" ? ? ? ? ? ? yes ? no 13 yes
     _FldNameList[5]   > general.remitos.anio_of
"anio_of" "anio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[6]   > general.remitos.anio_produccion
"anio_produccion" "anio_produccion" ? ? "integer" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[7]   > general.remitos.bultos
"bultos" "bultos" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[8]   > general.remitos.chofer
"chofer" "chofer" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[9]   > general.remitos.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[10]   > general.remitos.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[11]   > general.remitos.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[12]   > general.remitos.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[13]   > general.remitos.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[14]   > general.remitos.fecha_descarga
"fecha_descarga" "fecha_descarga" ? ? "date" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[15]   > general.remitos.fecha_llegada
"fecha_llegada" "fecha_llegada" ? ? "date" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[16]   > general.remitos.fecha_proceso
"fecha_proceso" "fecha_proceso" ? ? "date" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[17]   > general.remitos.fecha_salida
"fecha_salida" "fecha_salida" ? ? "date" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[18]   > general.remitos.hora_descarga
"hora_descarga" "hora_descarga" ? ? "character" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[19]   > general.remitos.hora_llegada
"hora_llegada" "hora_llegada" ? ? "character" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[20]   > general.remitos.hora_salida
"hora_salida" "hora_salida" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[21]   > general.remitos.id_agencia
"id_agencia" "id_agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[22]   > general.remitos.id_ciaseg
"id_ciaseg" "id_ciaseg" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[23]   > general.remitos.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[24]   > general.remitos.id_cliente_remito
"id_cliente_remito" "id_cliente_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 13.6 yes
     _FldNameList[25]   > general.remitos.id_destino
"id_destino" "id_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 17 yes
     _FldNameList[26]   > general.remitos.id_destino_final
"id_destino_final" "id_destino_final" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[27]   > general.remitos.id_galpon
"id_galpon" "id_galpon" ? ? "integer" ? ? ? ? ? ? yes ? no 16.6 yes
     _FldNameList[28]   > general.remitos.id_lugdes
"id_lugdes" "id_lugdes" ? ? "integer" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[29]   > general.remitos.id_operacion
"id_operacion" "id_operacion" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[30]   > general.remitos.id_operacion_compra
"id_operacion_compra" "id_operacion_compra" ? ? "integer" ? ? ? ? ? ? yes ? no 20.4 yes
     _FldNameList[31]   > general.remitos.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[32]   > general.remitos.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? yes ? no 14.2 yes
     _FldNameList[33]   > general.remitos.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[34]   > general.remitos.id_tipocomp_compra
"id_tipocomp_compra" "id_tipocomp_compra" ? ? "integer" ? ? ? ? ? ? yes ? no 19.6 yes
     _FldNameList[35]   > general.remitos.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 15.6 yes
     _FldNameList[36]   > general.remitos.impresion
"impresion" "impresion" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes
     _FldNameList[37]   > general.remitos.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[38]   > general.remitos.mercado
"mercado" "mercado" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[39]   > general.remitos.nromov_compra
"nromov_compra" "nromov_compra" ? ? "integer" ? ? ? ? ? ? yes ? no 15.2 yes
     _FldNameList[40]   > general.remitos.nro_contenedor
"nro_contenedor" "nro_contenedor" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[41]   > general.remitos.nro_iascav
"nro_iascav" "nro_iascav" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes
     _FldNameList[42]   > general.remitos.nro_orden_fab
"nro_orden_fab" "nro_orden_fab" ? ? "character" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[43]   > general.remitos.nro_ord_carga
"nro_ord_carga" "nro_ord_carga" ? ? "integer" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[44]   > general.remitos.nro_per_embarque
"nro_per_embarque" "nro_per_embarque" ? ? "character" ? ? ? ? ? ? yes ? no 18.6 yes
     _FldNameList[45]   > general.remitos.nro_precinto
"nro_precinto" "nro_precinto" ? ? "character" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[46]   > general.remitos.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes
     _FldNameList[47]   > general.remitos.observ_ordenentrega
"observ_ordenentrega" "observ_ordenentrega" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes
     _FldNameList[48]   > general.remitos.orden_embarque
"orden_embarque" "orden_embarque" ? ? "integer" ? ? ? ? ? ? yes ? no 11.2 yes
     _FldNameList[49]   > general.remitos.orden_fabricacion
"orden_fabricacion" "orden_fabricacion" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[50]   > general.remitos.Pat_acopla
"Pat_acopla" "Pat_acopla" ? ? "character" ? ? ? ? ? ? yes ? no 16.8 yes
     _FldNameList[51]   > general.remitos.pat_chasis
"pat_chasis" "pat_chasis" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes
     _FldNameList[52]   > general.remitos.peso
"peso" "peso" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[53]   > general.remitos.Peso_bruto
"Peso_bruto" "Peso_bruto" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[54]   > general.remitos.Peso_neto
"Peso_neto" "Peso_neto" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[55]   > general.remitos.Tara
"Tara" "Tara" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[56]   > general.remitos.temp_llegada
"temp_llegada" "temp_llegada" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[57]   > general.remitos.temp_salida
"temp_salida" "temp_salida" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[58]   > general.remitos.tipo_remito
"tipo_remito" "tipo_remito" ? ? "logical" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[59]   > general.remitos.Valor_declarado
"Valor_declarado" "Valor_declarado" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[60]   > "_<CALC>"
"getLugDes()" "LugarDescarga" "LugarDescarga" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[61]   > general.confirmacion_remito.fecha
"fecha" "FechaArribo" "FechaArribo" "99/99/9999" "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[62]   > "_<CALC>"
"getDestino()" "Destino" "Destino" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
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
         rowObject.Destino = (getDestino())
         rowObject.LugarDescarga = (getLugDes())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getConfirmacionArribo dTables  _DB-REQUIRED
FUNCTION getConfirmacionArribo RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DATE       NO-UNDO.

  FOR FIRST confirmacion_remito
      WHERE confirmacion_remito.id_sucursal     = rowObject.id_sucursal
        AND confirmacion_remito.id_tipo_movsto  = rowObject.id_tipo_movsto
        AND confirmacion_remito.nro             = rowObject.nro
      NO-LOCK.
    dRet = confirmacion_remito.fecha.
  END.

  RETURN dRet.

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
  FIND FIRST destinos WHERE destinos.id_destino = rowObject.id_destino
                      NO-LOCK NO-ERROR.
  IF AVAILABLE destinos THEN
    RETURN destinos.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLugDes dTables  _DB-REQUIRED
FUNCTION getLugDes RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = rowObject.id_lugdes
                    NO-LOCK NO-ERROR.
  IF AVAILABLE lugar_descarga THEN
    RETURN lugar_descarga.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

