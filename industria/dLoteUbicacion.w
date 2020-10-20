&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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

DEFINE VAR hProg AS HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES lotes_ubicacion sucursales

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  cantidad id_empresa id_empresa_ubicacion id_locacion_ubicacion id_lote~
 id_lote_deposito id_posicion_ubicacion id_sucursal id_sucursal_ubicacion~
 id_tipotambor nromov calidad envase lote cantidad_comprometida saldo~
 id_articulo id_contrato id_tipo_contrato anio_contrato item_contrato~
 c_usuario c_hora c_fecha codigo_lote abreviatura
&Scoped-define ENABLED-FIELDS-IN-lotes_ubicacion cantidad id_empresa ~
id_empresa_ubicacion id_locacion_ubicacion id_lote id_lote_deposito ~
id_posicion_ubicacion id_sucursal id_sucursal_ubicacion id_tipotambor ~
nromov calidad envase lote cantidad_comprometida id_articulo id_contrato ~
id_tipo_contrato anio_contrato item_contrato c_usuario c_hora c_fecha ~
codigo_lote 
&Scoped-define ENABLED-FIELDS-IN-sucursales abreviatura 
&Scoped-Define DATA-FIELDS  cantidad LoteDeposito id_empresa id_empresa_ubicacion FechaProceso~
 id_locacion_ubicacion id_lote id_lote_deposito id_posicion_ubicacion~
 contenedor id_sucursal id_sucursal_ubicacion id_tipotambor nromov vapor~
 calidad envase lote cantidad_comprometida saldo id_tipo_sucursal Deposito~
 id_articulo id_contrato id_tipo_contrato anio_contrato galones~
 item_contrato c_usuario c_hora c_fecha codigo_lote abreviatura peso~
 Producto
&Scoped-define DATA-FIELDS-IN-lotes_ubicacion cantidad id_empresa ~
id_empresa_ubicacion id_locacion_ubicacion id_lote id_lote_deposito ~
id_posicion_ubicacion id_sucursal id_sucursal_ubicacion id_tipotambor ~
nromov calidad envase lote cantidad_comprometida id_articulo id_contrato ~
id_tipo_contrato anio_contrato item_contrato c_usuario c_hora c_fecha ~
codigo_lote 
&Scoped-define DATA-FIELDS-IN-sucursales id_tipo_sucursal Deposito ~
abreviatura 
&Scoped-Define MANDATORY-FIELDS  id_lote id_sucursal id_sucursal_ubicacion
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Deposito = sucursales.nombre
&Scoped-Define DATA-FIELD-DEFS "dLoteUbicacion.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH lotes_ubicacion ~
      WHERE lotes_ubicacion.id_sucursal_ubicacion <> 1 ~
  NO-LOCK, ~
      EACH sucursales WHERE sucursales.id_sucursal = lotes_ubicacion.id_sucursal_ubicacion NO-LOCK ~
    BY lotes_ubicacion.lote INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH lotes_ubicacion ~
      WHERE lotes_ubicacion.id_sucursal_ubicacion <> 1 ~
  NO-LOCK, ~
      EACH sucursales WHERE sucursales.id_sucursal = lotes_ubicacion.id_sucursal_ubicacion NO-LOCK ~
    BY lotes_ubicacion.lote INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main lotes_ubicacion sucursales
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main lotes_ubicacion
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main sucursales


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD controlCantidadBolsas dTables  _DB-REQUIRED
FUNCTION controlCantidadBolsas RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER,
   piUbi AS INTEGER, 
   piCan AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArrayLotesCascara dTables  _DB-REQUIRED
FUNCTION getArrayLotesCascara RETURNS CHARACTER
  ()  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadSucursal dTables  _DB-REQUIRED
FUNCTION getCantidadSucursal RETURNS INTEGER
  (INPUT piSucursal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContenedor dTables  _DB-REQUIRED
FUNCTION getContenedor RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFechaProceso dTables  _DB-REQUIRED
FUNCTION getFechaProceso RETURNS DATE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLote dTables  _DB-REQUIRED
FUNCTION getLote RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteDeposito dTables  _DB-REQUIRED
FUNCTION getLoteDeposito RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteIndustria dTables  _DB-REQUIRED
FUNCTION getLoteIndustria RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLotesSucursal dTables  _DB-REQUIRED
FUNCTION getLotesSucursal RETURNS CHARACTER
  (INPUT piSucursal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPeso dTables  _DB-REQUIRED
FUNCTION getPeso RETURNS DECIMAL
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSaldo dTables  _DB-REQUIRED
FUNCTION getSaldo RETURNS INTEGER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD openDesdeSucursal dTables 
FUNCTION openDesdeSucursal RETURNS CHARACTER
  ( INPUT piSucursal AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCantidad dTables  _DB-REQUIRED
FUNCTION setCantidad RETURNS INTEGER
  (INPUT piSucursal AS INTEGER, 
   INPUT piLote     AS INTEGER,
   INPUT piCantidad AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      lotes_ubicacion, 
      sucursales SCROLLING.
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
         HEIGHT             = 1.67
         WIDTH              = 46.4.
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
     _TblList          = "general.lotes_ubicacion,comercial.sucursales WHERE general.lotes_ubicacion ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ","
     _OrdList          = "general.lotes_ubicacion.lote|yes"
     _Where[1]         = "general.lotes_ubicacion.id_sucursal_ubicacion <> 1
 "
     _JoinCode[2]      = "comercial.sucursales.id_sucursal = general.lotes_ubicacion.id_sucursal_ubicacion"
     _FldNameList[1]   > general.lotes_ubicacion.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[2]   > "_<CALC>"
"getLoteDeposito()" "LoteDeposito" "LoteDeposito" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no
     _FldNameList[3]   > general.lotes_ubicacion.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[4]   > general.lotes_ubicacion.id_empresa_ubicacion
"id_empresa_ubicacion" "id_empresa_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[5]   > "_<CALC>"
"getFechaProceso()" "FechaProceso" ? "99/99/9999" "Date" ? ? ? ? ? ? no ? no 11.6 no
     _FldNameList[6]   > general.lotes_ubicacion.id_locacion_ubicacion
"id_locacion_ubicacion" "id_locacion_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[7]   > general.lotes_ubicacion.id_lote
"id_lote" "id_lote" ? ">,>>>,>>9" "integer" ? ? ? ? ? ? yes ? yes 9.6 yes
     _FldNameList[8]   > general.lotes_ubicacion.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[9]   > general.lotes_ubicacion.id_posicion_ubicacion
"id_posicion_ubicacion" "id_posicion_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[10]   > "_<CALC>"
"getContenedor()" "contenedor" ? "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[11]   > general.lotes_ubicacion.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[12]   > general.lotes_ubicacion.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.2 yes
     _FldNameList[13]   > general.lotes_ubicacion.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[14]   > general.lotes_ubicacion.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[15]   > "_<CALC>"
"getVapor()" "vapor" ? "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[16]   > general.lotes_ubicacion.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[17]   > general.lotes_ubicacion.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[18]   > general.lotes_ubicacion.lote
"lote" "lote" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[19]   > general.lotes_ubicacion.cantidad_comprometida
"cantidad_comprometida" "cantidad_comprometida" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[20]   > "_<CALC>"
"getSaldo()" "saldo" "Cantidad" ">>>9" "Integer" ? ? ? ? ? ? yes ? no 8.4 no
     _FldNameList[21]   > comercial.sucursales.id_tipo_sucursal
"id_tipo_sucursal" "id_tipo_sucursal" ? ? "integer" ? ? ? ? ? ? no ? no 6.8 yes
     _FldNameList[22]   > comercial.sucursales.nombre
"nombre" "Deposito" "Deposito" ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[23]   > general.lotes_ubicacion.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[24]   > general.lotes_ubicacion.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[25]   > general.lotes_ubicacion.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[26]   > general.lotes_ubicacion.anio_contrato
"anio_contrato" "anio_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[27]   > "_<CALC>"
"getGalones()" "galones" ? "zzz,zz9.99" "Decimal" ? ? ? ? ? ? no ? no 9.8 no
     _FldNameList[28]   > general.lotes_ubicacion.item_contrato
"item_contrato" "item_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[29]   > general.lotes_ubicacion.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[30]   > general.lotes_ubicacion.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[31]   > general.lotes_ubicacion.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[32]   > general.lotes_ubicacion.codigo_lote
"codigo_lote" "codigo_lote" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[33]   > comercial.sucursales.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[34]   > "_<CALC>"
"getPeso()" "peso" ? "zzz,zz9.99" "Decimal" ? ? ? ? ? ? no ? no 9.8 no
     _FldNameList[35]   > "_<CALC>"
"getProducto()" "Producto" "Producto" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _Design-Parent    is WINDOW dTables @ ( 1.19 , 2.2 )
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
  DEFINE VARIABLE iSeq AS INTEGER    NO-UNDO.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".
  /*actualizo item_ingreso_lote_ubicacion*/
  FIND FIRST ITEM_ingreso_lote_ubicacion WHERE ITEM_ingreso_lote_ubicacion.nromov = rowObjUpd.nromov
                                         NO-ERROR .
  IF AVAILABLE ITEM_ingreso_lote_ubicacion THEN DO:
    ASSIGN ITEM_ingreso_lote_ubicacion.id_lote_deposito = rowObjUpd.id_lote_deposito.
  END.
  /*actualizo items_release_delivery*/
  FIND FIRST items_release_delivery WHERE items_release_delivery.nromov = rowObjUpd.nromov
                                    NO-ERROR.
  IF AVAILABLE items_release_delivery THEN DO:
    ASSIGN items_release_delivery.id_lote_deposito = rowObjUpd.id_lote_deposito.
  END.
  /* para ingresos de terceros de cascara */
  IF rowObjUpd.id_tipotambor = 11 AND rowObjUpd.cantidad <> 0 THEN DO:
    DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hLibCom AS HANDLE.
    RUN libCommonFunctions.p PERSISTENT SET hLibCom.
    hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libProduccionCascara.p'). 
    DELETE OBJECT hLibCom.
    RUN getNextNroProduccion IN hLib (OUTPUT iSeq).
    CREATE produccion_cascara.
    ASSIGN produccion_cascara.id_empresa      = rowObjUpd.id_empresa
           produccion_cascara.id_sucursal     = rowObjUpd.id_sucursal
           produccion_cascara.id_tipotambor   = 12
           produccion_cascara.nromov          = NEXT-VALUE(nromov)
           produccion_cascara.id_articulo     = 54
           produccion_cascara.id_produccion   = iSeq
           produccion_cascara.anio            = YEAR(TODAY)
           produccion_cascara.cantidad_bolsas = rowObjUpd.cantidad
           .
    CREATE r_produccion_cascara_lote.
    ASSIGN  r_produccion_cascara_lote.cantidad              = rowObjUpd.cantidad
            r_produccion_cascara_lote.c_fecha               = TODAY 
            r_produccion_cascara_lote.c_hora                = STRING(TIME, "HH:MM:SS")
            r_produccion_cascara_lote.c_usuario             = USERID('userdb')
            r_produccion_cascara_lote.id_empresa_lote       = rowObjUpd.id_empresa
            r_produccion_cascara_lote.id_produccion_prod    = iSeq
            r_produccion_cascara_lote.id_sucursal_lote      = rowObjUpd.id_sucursal
            r_produccion_cascara_lote.id_sucursal_prod      = rowObjUpd.id_sucursal  
            r_produccion_cascara_lote.id_tipotambor_lote    = 11
            r_produccion_cascara_lote.id_turno_prod         = 0
            r_produccion_cascara_lote.nromov_lote           = rowObjUpd.nromov
            .

  END.

  ASSIGN rowObjUpd.c_usuario = USERID("user_db")
         rowObjUpd.c_fecha   = TODAY
         rowObjUpd.c_hora    = STRING(TIME, "HH:MM:SS").


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
         rowObject.contenedor = (getContenedor())
         rowObject.FechaProceso = (getFechaProceso())
         rowObject.galones = (getGalones())
         rowObject.LoteDeposito = (getLoteDeposito())
         rowObject.peso = (getPeso())
         rowObject.Producto = (getProducto())
         rowObject.saldo = (getSaldo())
         rowObject.vapor = (getVapor())
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE desdeSucursal dTables 
PROCEDURE desdeSucursal PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piSucursal AS INTEGER NO-UNDO.

DEFINE VAR cq AS CHARACTER NO-UNDO.

cq = "FOR EACH lotes_ubicacion where lotes_ubicacion.id_sucursal_ubicacion = " + STRING(piSucursal) + ", EACH sucursales WHERE sucursales.id_sucursal = lotes_ubicacion.id_sucursal_ubicacion "  + "  BY lotes_ubicacion.lote ". 
/*  
  "for each lotes_ubicacion where lotes_ubicacion.id_sucursal_ubicacion = " + STRING(piSucursal) +
     " by lotes_ubicacion.lote".*/
{set queryWhere cq}.

openQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFirstTambor dTables  _DB-REQUIRED
PROCEDURE getFirstTambor PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER ti FOR tambores_industria.

  FIND CURRENT RowObject NO-LOCK NO-ERROR.
  IF NOT AVAILABLE RowObject THEN
  RETURN "".   /* Function return value. */
  
  FIND lotes_jugo OF RowObject NO-LOCK NO-ERROR.
  IF NOT AVAILABLE lotes_jugo THEN
  DO:
        FIND lotes_aceite OF RowObject NO-LOCK NO-ERROR.
        IF NOT AVAILABLE lotes_aceite THEN 
            RETURN "NO-LOTE".
        ELSE
        DO:
            FIND FIRST ti OF lotes_jugo NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ti THEN
                RETURN "NO-TAMBOR".
        END.
  END.
  ELSE
      FIND FIRST ti OF lotes_jugo NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ti THEN
          RETURN "NO-TAMBOR".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getGalonesLote dTables  _DB-REQUIRED
PROCEDURE getGalonesLote PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER pdgall AS DECIMAL.

DEFINE VAR vdpeso AS DECIMAL NO-UNDO.

          FIND CURRENT RowOBject NO-LOCK NO-ERROR.

          IF AVAILABLE RowOBject THEN
          DO:
              IF RowOBject.id_tipotambor <> 3 THEN RETURN "No es lote de jugo".
              find last inspecciones_lote WHERE inspecciones_lote.id_empresa       =   RowObject.id_empresa AND 
                                                inspecciones_lote.id_sucursal      =   RowObject.id_sucursal AND
                                                inspecciones_lote.id_tipotambor    =   RowObject.id_tipotambor AND
                                                inspecciones_lote.nromov           =   RowObject.nromov NO-LOCK NO-ERROR.
              if available inspecciones_lote then
              DO:                                     
                  FOR EACH tambores_industria  WHERE  tambores_industria.nromov = RowObject.nromov AND
                                                      tambores_industria.id_sucursal_ubicacion = 
                                                      RowObject.id_sucursal_ubicacion NO-LOCK.
                      vdPeso = vdPeso + tambores_industria.kilos_tambor.
                  END.
                  find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
                  if available brix then 
                   do:
                      pdgall = (vdPeso / brix.pe) / 3.785.
                   end.
              END.
          END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPesoLote dTables  _DB-REQUIRED
PROCEDURE getPesoLote PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER pdPeso AS DECIMAL NO-UNDO.


FIND CURRENT RowOBject NO-LOCK NO-ERROR.
IF AVAILABLE RowOBject THEN DO:
    FOR EACH tambores_industria  WHERE tambores_industria.nromov = RowObject.nromov 
                                   AND tambores_industria.id_sucursal_ubicacion = RowObject.id_sucursal_ubicacion 
                                 NO-LOCK.
        pdPeso = pdPeso + tambores_industria.kilos_tambor.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPLEnvio dTables  _DB-REQUIRED
PROCEDURE getPLEnvio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER piIdEmpresa    AS INTEGER. 
DEFINE INPUT  PARAMETER piIdSucursal   AS INTEGER. 
DEFINE INPUT  PARAMETER piIdTipoTambor AS INTEGER. 
DEFINE INPUT  PARAMETER piNroMov       AS INTEGER.
DEFINE OUTPUT PARAMETER pcReturn       AS CHARACTER INITIAL ",,,,".

DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO INITIAL ",,,,".

FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = piIdEmpresa
                               AND tambores_industria.id_sucursal   = piIdSucursal 
                               AND tambores_industria.id_tipotambor = piIdTipoTambor
                               AND tambores_industria.nromov        = piNroMov
                             NO-LOCK.
  FOR FIRST items_packing_list WHERE items_packing_list.id_sucursal_remito = tambores_industria.id_sucursal_remito
                                 AND items_packing_list.id_tipo_movsto     = tambores_industria.id_tipo_movsto
                                 AND items_packing_list.nro                = tambores_industria.nro_remito
                                 AND items_packing_list.ITEM_remito        = tambores_industria.ITEM_factura
                               NO-LOCK.  
    FOR FIRST packing_list OF items_packing_list NO-LOCK.     
      ENTRY(1, cRet) = STRING(packing_list.id_packing_list).
      ENTRY(2, cRet) = STRING(items_packing_list.ITEM).
      ENTRY(3, cRet) = STRING(packing_list.id_vapor).
      ENTRY(4, cRet) = items_packing_list.nro_contenedor.
      ENTRY(5, cRet) = packing_list.nro_pack_list.
      /*DISP packing_list.nro_pack_list nro_contenedor id_vapor.*/
    END.
  END.
END.
 
pcReturn = cRet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVaporEnvio dTables  _DB-REQUIRED
PROCEDURE getVaporEnvio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  pcVapor              AS CHARACTER NO-UNDO.

DEFINE VARIABLE cPL AS CHARACTER  NO-UNDO INITIAL ",,,,".

RUN getPLEnvio (piEmpresa, piSucursal, piTipoTambor, piNroMov, OUTPUT cPL).

IF cPL <> ",,,," THEN DO:
    
    FIND FIRST vapores WHERE vapores.id_vapor = INTEGER(ENTRY(3, cPL)) NO-LOCK NO-ERROR.
    IF AVAILABLE vapores THEN DO:
        pcVapor = vapores.descripcion.
    END.
    ELSE pcVapor = "No se encontro Vapor".
END.
ELSE 
  pcVapor =  "".


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

  RUN libLotesUbicacion.p PERSISTENT SET hProg.
  THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hProg).

  /* Code placed here will execute AFTER standard behavior.    */
  /*
  DEFINE VAR xDataSource AS CHARACTER NO-UNDO.
  {get DataSource xDataSource}.
  IF xDataSource <> ? THEN
  DO:
      {set AutoCommit YES}.
      {set CommitSource xDataSource}.
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lnkFilterLU dTables  _DB-REQUIRED
PROCEDURE lnkFilterLU :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piIdSucursal AS INTEGER.
  
  DEFINE VARIABLE cWhere AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFlag  AS LOGICAL    NO-UNDO.

  cWhere = "FOR EACH lotes_ubicacion, EACH sucursales WHERE lotes_ubicacion.id_sucursal_ubicacion = " + STRING(piIdSucursal) + " AND comercial.sucursales.id_sucursal = general.lotes_ubicacion.id_sucursal_ubicacion AND lotes_ubicacion.cantidad > lotes_ubicacion.cantidad_comprometida AND sucursales.id_tipo_sucursal = 6 BY lotes_ubicacion.lote ".
    
  lFlag = DYNAMIC-FUNCTION('setQueryWhere', cWhere).
  IF lFlag THEN DO:
    DYNAMIC-FUNCTION('openQuery').
  END.
  
  /*  "WHERE id_sucursal_ubicacion = " + STRING(piIdSucursal) + " AND lotes_ubicacion.cantidad > lotes_ubicacion.cantidad_comprometida BY lotes_ubicacion.lote".*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryForContrato dTables  _DB-REQUIRED
PROCEDURE openQueryForContrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piSucursal     AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piArticulo     AS INTEGER   NO-UNDO.

DEFINE VAR cq AS CHARACTER NO-UNDO.

cq = "FOR EACH lotes_ubicacion WHERE ( lotes_ubicacion.cantidad > lotes_ubicacion.cantidad_comprometida ) and lotes_ubicacion.id_sucursal_ubicacion = " + STRING(piSucursal) + " and lotes_ubicacion.id_articulo = " + STRING(piArticulo) + " , FIRST sucursales WHERE sucursales.id_sucursal = lotes_ubicacion.id_sucursal_ubicacion "  + "  BY lotes_ubicacion.lote ". 

{set queryWhere cq}.

openQuery().




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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION controlCantidadBolsas dTables  _DB-REQUIRED
FUNCTION controlCantidadBolsas RETURNS LOGICAL
  (piEmp AS INTEGER,
   piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER,
   piUbi AS INTEGER, 
   piCan AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.

  FOR FIRST lotes_ubicacion
      WHERE lotes_ubicacion.id_empresa            = piEmp
        AND lotes_ubicacion.id_sucursal           = piSuc
        AND lotes_ubicacion.id_tipotambor         = piTip
        AND lotes_ubicacion.nromov                = piNro
        AND lotes_ubicacion.id_sucursal_ubicacion = piUbi
      NO-LOCK.

    iCan = lotes_ubicacion.cantidad.

  END.

  IF piCan > iCan THEN
    lRet = FALSE.
  ELSE 
    lRet = TRUE.

  RETURN lRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArrayLotesCascara dTables  _DB-REQUIRED
FUNCTION getArrayLotesCascara RETURNS CHARACTER
  () :
/*------------------------------------------------------------------------------
  Purpose:  devuelve una array separado por comas y chr(10) al final
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE BUFFER ro FOR rowObject.

  FOR EACH ro BY ro.id_sucursal_ubicacion BY ro.id_sucursal BY ro.id_lote.
    FIND FIRST sucursales WHERE ro.id_sucursal_ubicacion = sucursales.id_sucursal NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados WHERE ro.id_articulo = productos_terminados.id_articulo NO-LOCK NO-ERROR.
    FIND FIRST lotes_cascara WHERE ro.nromov = lotes_cascara.nromov NO-LOCK NO-ERROR.

    cArt = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "no se encontro articulo".
    cLot = IF AVAILABLE lotes_cascara THEN STRING(lotes_cascara.id_lote_cliente) ELSE "no se encontro lote cliente".

    cRet = cRet + sucursal.nombre + "," + 
                  "(" + STRING(ro.id_sucursal) + ") " + STRING(ro.id_lote) + "," + 
                  STRING(lotes_cascara.anio) + "," + 
                  cArt + "," + 
                  ro.calidad + "," + 
                  ro.envase + "," + 
                  cLot + "," + 
                  STRING(ro.cantidad) + "," + 
                  STRING(ro.cantidad * 50) + "," + 
                  STRING(ro.id_sucursal_ubicacion) + 
                  CHR(10).
  END.
  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).
  
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
  DEFINE BUFFER ti FOR tambores_industria.

  RUN getFirstTambor (BUFFER ti).
  IF RETURN-VALUE <> "" THEN
      RETURN "".

  FIND calidades OF ti NO-LOCK NO-ERROR.

  IF AVAILABLE calidades THEN
      RETURN calidades.descripcion.
  ELSE
      RETURN " No Encontro calidad " + STRING(ti.id_calidad).
 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadSucursal dTables  _DB-REQUIRED
FUNCTION getCantidadSucursal RETURNS INTEGER
  (INPUT piSucursal AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  FOR EACH lotes_ubicacion WHERE lotes_ubicacion.id_sucursal_ubicacion = piSucursal 
                             AND lotes_ubicacion.id_tipotambor         <> 11
                           NO-LOCK.
    viCant = viCant + lotes_ubicacion.cantidad.
  END.

  RETURN viCant.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContenedor dTables  _DB-REQUIRED
FUNCTION getContenedor RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

FIND CURRENT RowObject NO-LOCK.

FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = RowObject.id_empresa
                               AND tambores_industria.id_sucursal   = RowObject.id_sucursal 
                               AND tambores_industria.id_tipotambor = RowObject.id_tipotambor
                               AND tambores_industria.nromov        = RowObject.nromov
                               AND tambores_industria.id_sucursal_ubicacion = RowObject.id_sucursal_ubicacion NO-LOCK.
/*
  FOR FIRST items_packing_list WHERE items_packing_list.id_sucursal_remito = tambores_industria.id_sucursal_remito
                                 AND items_packing_list.id_tipo_movsto     = tambores_industria.id_tipo_movsto
                                 AND items_packing_list.nro                = tambores_industria.nro_remito
                                 AND items_packing_list.ITEM_remito        = tambores_industria.ITEM_factura
                               NO-LOCK.  
      RETURN items_packing_list.nro_contenedor.
  END.
*/  
END.
 
RETURN "Sin Contenedor".   /* Function return value. */

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
  DEFINE BUFFER ti FOR tambores_industria.

  RUN getFirstTambor (BUFFER ti).
  IF RETURN-VALUE <> "" THEN
      RETURN "".

  FIND envases_prod OF ti NO-LOCK NO-ERROR.

  IF AVAILABLE envases_prod THEN
      RETURN envases_prod.descripcion.
  ELSE
      RETURN "No encontro el envase " + STRING(ti.id_envase).

  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFechaProceso dTables  _DB-REQUIRED
FUNCTION getFechaProceso RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  FIND CURRENT RowObject NO-ERROR.


  FOR  LAST ITEM_ingreso_lote_ubicacion WHERE ITEM_ingreso_lote_ubicacion.id_empresa = RowObject.id_empresa AND 
                                              ITEM_ingreso_lote_ubicacion.id_sucursal = RowObject.id_sucursal AND
                                              ITEM_ingreso_lote_ubicacion.id_tipotambor = RowObject.id_tipotambor AND
                                              ITEM_ingreso_lote_ubicacion.nromov      = RowObject.nromov AND
                                              ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = RowObject.id_sucursal_ubicacion 
                                              NO-LOCK.
    FIND FIRST ingreso_lote_Ubicacion WHERE ingreso_lote_ubicacion.nromov_ingreso = ITEM_ingreso_lote_ubicacion.nromov_ingreso   NO-LOCK NO-ERROR.                    
    IF AVAILABLE ingreso_lote_ubicacion THEN
      RETURN ingreso_lote_ubicacion.fecha_proceso.
  END.

  RETURN ?.   /* Function return value. */

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
  DEFINE VAR vdgall AS DECIMAL NO-UNDO.
  RUN getGalonesLote (OUTPUT vdgall).

  RETURN vdgall.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLote dTables  _DB-REQUIRED
FUNCTION getLote RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR hLote AS HANDLE NO-UNDO.
  DEFINE VAR cLote AS CHARACTER FORMAT "9999/99" NO-UNDO.

  hLote = getLoteIndustria().

  IF valid-handle(hLote) AND hLote <> ? THEN
  DO:
      cLote = STRING(hLote:BUFFER-FIELD('id_lote'):BUFFER-VALUE ,'999999') + STRING ( hLote:BUFFER-FIELD('anio'):BUFFER-VALUE ,'9999' ).
  END.

  RETURN cLote.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteDeposito dTables  _DB-REQUIRED
FUNCTION getLoteDeposito RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO INITIAL "NO LOT # AVAILABLE".
  FIND CURRENT RowObject NO-ERROR.

  FOR FIRST item_ingreso_lote_ubicacion WHERE ITEM_ingreso_lote_ubicacion.id_empresa    = RowObject.id_empresa
                                          AND ITEM_ingreso_lote_ubicacion.id_sucursal   = RowObject.id_sucursal
                                          AND ITEM_ingreso_lote_ubicacion.id_tipotambor = RowObject.id_tipotambor
                                          AND ITEM_ingreso_lote_ubicacion.nromov        = RowObject.nromov
                                        NO-LOCK.
    cRet = ITEM_ingreso_lote_ubicacion.id_lote_deposito.
  END.

  RETURN cRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteIndustria dTables  _DB-REQUIRED
FUNCTION getLoteIndustria RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR hb     AS HANDLE       NO-UNDO.
  DEFINE VAR cq     AS CHARACTER    NO-UNDO.
  DEFINE VAR ret    AS LOGICAL      INITIAL FALSE NO-UNDO.
  
  FIND CURRENT RowObject NO-LOCK.

  cq = " where id_empresa = " + STRING( RowObject.id_empresa ) + " and " + 
       " id_sucursal = " + STRING( RowObject.id_sucursal ) + " and " +
       " id_tipotambor = " + STRING( RowObject.id_tipotambor ) + " and " +
       " nromov = " + STRING( RowObject.nromov ).

  hb = BUFFER lotes_jugo:HANDLE.
  ret = hb:FIND-FIRST(cq,6209,6090) NO-ERROR.
  IF NOT ret THEN
  DO:
      hb = BUFFER lotes_aceite:HANDLE.
      ret = hb:FIND-FIRST(cq,6209,6090) NO-ERROR.
  END.
  IF ret  THEN 
    RETURN hb.   /* Function return value. */
  ELSE
    RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLotesSucursal dTables  _DB-REQUIRED
FUNCTION getLotesSucursal RETURNS CHARACTER
  (INPUT piSucursal AS INTEGER) :

  DEFINE VARIABLE vcLista AS CHARACTER  NO-UNDO.
  
  FOR EACH lotes_ubicacion  WHERE lotes_ubicacion.id_sucursal_ubicacion = piSucursal
                              AND lotes_ubicacion.id_tipotambor         <> 11
                            NO-LOCK.
    FIND FIRST item_ingreso_lote_ubicacion WHERE lotes_ubicacion.nromov = item_ingreso_lote_ubicacion.nromov
                                           NO-LOCK NO-ERROR.
    vcLista = vcLista + item_ingreso_lote_ubicacion.id_lote_deposito + "," + 
              STRING(lotes_ubicacion.cantidad) + "," +
              STRING(lotes_ubicacion.id_lote) +
              CHR(14).
  END.

  vcLista = SUBSTRING(vcLista, 1, LENGTH(vcLista) - 1).

  RETURN vcLista.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPeso dTables  _DB-REQUIRED
FUNCTION getPeso RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dPeso AS DECIMAL    NO-UNDO.
  DEFINE BUFFER buTam FOR tambores_industria.

  FIND CURRENT rowObject NO-ERROR.
  FOR EACH buTam WHERE buTam.id_empresa            = rowObject.id_empresa
                   AND buTam.id_sucursal           = rowObject.id_sucursal
                   AND buTam.id_tipotambor         = rowObject.id_tipotambor
                   AND buTam.nromov                = rowObject.nromov
                   AND buTam.id_sucursal_ubicacion = rowObject.id_sucursal_ubicacion
                 NO-LOCK.
    dPeso = dPeso + buTam.kilos_tambor.    
  END.
  


/*  RUN getPesoLote (OUTPUT vdPeso).*/

  RETURN dPeso.   /* Function return value. */

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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST tambores_industria 
      WHERE tambores_industria.id_empresa = rowObject.id_empresa
        AND tambores_industria.id_sucursal = rowObject.id_sucursal
        AND tambores_industria.id_tipotambor = rowObject.id_tipotambor
        AND tambores_industria.nromov        = rowObject.nromov
      NO-LOCK.

    FOR FIRST productos_terminados
        OF tambores_industria
        NO-LOCK.
      cRet = productos_terminados.descripcion.
    END.

  END.

  FOR FIRST lotes_cascara 
      WHERE lotes_cascara.id_empresa = rowObject.id_empresa
        AND lotes_cascara.id_sucursal = rowObject.id_sucursal
        AND lotes_cascara.id_tipotambor = rowObject.id_tipotambor
        AND lotes_cascara.nromov        = rowObject.nromov
      NO-LOCK.

    FOR FIRST productos_terminados
        WHERE productos_terminados.id_articulo = lotes_cascara.id_articulo
        NO-LOCK.
      cRet = productos_terminados.descripcion.
    END.

  END.



  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSaldo dTables  _DB-REQUIRED
FUNCTION getSaldo RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
  viCant = rowObject.cantidad - rowObject.cantidad_comprometida.
  IF viCant < 0  THEN
    viCant = 0.


  RETURN viCant.

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
DEFINE VARIABLE cVapor AS CHARACTER  NO-UNDO.

FIND CURRENT RowObject NO-LOCK.

RUN getVaporEnvio (RowObject.id_empresa, 
                       RowObject.id_sucursal, 
                       RowObject.id_tipotambor, 
                       RowObject.nromov,
                       OUTPUT cVapor).

IF cVapor <> "" THEN
  RETURN cVapor.


RETURN "Sin Vapor".
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION openDesdeSucursal dTables 
FUNCTION openDesdeSucursal RETURNS CHARACTER
  ( INPUT piSucursal AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RUN desdeSucursal (piSucursal).

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCantidad dTables  _DB-REQUIRED
FUNCTION setCantidad RETURNS INTEGER
  (INPUT piSucursal AS INTEGER, 
   INPUT piLote     AS INTEGER,
   INPUT piCantidad AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  
  FOR FIRST lotes_ubicacion WHERE id_sucursal_ubicacion = piSucursal
                              AND id_lote               = piLote
                            NO-LOCK .
    ASSIGN cantidad_comprometida = piCantidad.
  END.

  RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}
