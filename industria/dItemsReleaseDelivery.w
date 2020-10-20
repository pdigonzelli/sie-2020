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

DEFINE VARIABLE hProg AS HANDLE     NO-UNDO.

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
&Scoped-define INTERNAL-TABLES items_release_delivery vapores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio_contrato cantidad contenedor c_fecha c_hora c_usuario id_contrato~
 id_empresa id_envase id_lote id_lote_deposito id_release_delivery~
 id_sucursal id_sucursal_ubicacion id_tipotambor id_tipo_contrato~
 id_unidad_medida id_vapor item_contrato item_release_delivery nromov~
 tambores kilos_brutos kilos_netos observaciones
&Scoped-define ENABLED-FIELDS-IN-items_release_delivery anio_contrato ~
cantidad contenedor c_fecha c_hora c_usuario id_contrato id_empresa ~
id_envase id_lote id_lote_deposito id_release_delivery id_sucursal ~
id_sucursal_ubicacion id_tipotambor id_tipo_contrato id_unidad_medida ~
id_vapor item_contrato item_release_delivery nromov tambores kilos_brutos ~
kilos_netos observaciones 
&Scoped-Define DATA-FIELDS  anio_contrato UsuarioConf cantidad FechaConfirmacion contenedor NroRelease~
 c_fecha Calidad c_hora Envase c_usuario Peso id_contrato Galones id_empresa~
 id_envase id_lote id_lote_deposito id_release_delivery id_sucursal~
 id_sucursal_ubicacion id_tipotambor id_tipo_contrato id_unidad_medida~
 id_vapor item_contrato item_release_delivery nromov tambores Vapor~
 kilos_brutos kilos_netos observaciones Cliente Broker Producto Destino~
 CondicionVta Plazos Moneda UnidadVta
&Scoped-define DATA-FIELDS-IN-items_release_delivery anio_contrato cantidad ~
contenedor c_fecha c_hora c_usuario id_contrato id_empresa id_envase ~
id_lote id_lote_deposito id_release_delivery id_sucursal ~
id_sucursal_ubicacion id_tipotambor id_tipo_contrato id_unidad_medida ~
id_vapor item_contrato item_release_delivery nromov tambores kilos_brutos ~
kilos_netos observaciones 
&Scoped-define DATA-FIELDS-IN-vapores Vapor 
&Scoped-Define MANDATORY-FIELDS  id_sucursal_ubicacion
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Vapor = vapores.descripcion
&Scoped-Define DATA-FIELD-DEFS "ditemsreleasedelivery.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_release_delivery NO-LOCK, ~
      EACH vapores OF items_release_delivery OUTER-JOIN NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_release_delivery NO-LOCK, ~
      EACH vapores OF items_release_delivery OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_release_delivery vapores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_release_delivery
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main vapores


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createItemReleaseFromLUbicacionContrato dTables  _DB-REQUIRED
FUNCTION createItemReleaseFromLUbicacionContrato RETURNS CHARACTER
  (INPUT hDataSource    AS HANDLE , 
   INPUT cRows          AS CHARACTER , 
   INPUT pcContrato     AS CHARACTER , 
   INPUT piItem         AS INTEGER,
   INPUT piTipoContrato AS INTEGER, 
   INPUT piAnioContrato AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBroker dTables  _DB-REQUIRED
FUNCTION getBroker RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCliente dTables  _DB-REQUIRED
FUNCTION getCliente RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDestino dTables  _DB-REQUIRED
FUNCTION getDestino RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFechaConfirmacion dTables  _DB-REQUIRED
FUNCTION getFechaConfirmacion RETURNS DATE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMoneda dTables  _DB-REQUIRED
FUNCTION getMoneda RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNroRelease dTables  _DB-REQUIRED
FUNCTION getNroRelease RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPlazos dTables  _DB-REQUIRED
FUNCTION getPlazos RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidadVta dTables  _DB-REQUIRED
FUNCTION getUnidadVta RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUsuario dTables  _DB-REQUIRED
FUNCTION getUsuario RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_release_delivery, 
      vapores SCROLLING.
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
     _TblList          = "general.items_release_delivery,comercial.vapores OF general.items_release_delivery"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _FldNameList[1]   > general.items_release_delivery.anio_contrato
"anio_contrato" "anio_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > "_<CALC>"
"getUsuario()" "UsuarioConf" "UsuarioConf" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[3]   > general.items_release_delivery.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[4]   > "_<CALC>"
"getFechaConfirmacion()" "FechaConfirmacion" "FechaConfirmacion" "99/99/9999" "Date" ? ? ? ? ? ? no ? no 18.2 no
     _FldNameList[5]   > general.items_release_delivery.contenedor
"contenedor" "contenedor" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[6]   > "_<CALC>"
"getNroRelease()" "NroRelease" "NroRelease" "zzzzz999" "Integer" ? ? ? ? ? ? no ? no 11.2 no
     _FldNameList[7]   > general.items_release_delivery.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[8]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[9]   > general.items_release_delivery.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[10]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[11]   > general.items_release_delivery.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[12]   > "_<CALC>"
"getPeso()" "Peso" "Peso" "zzzzz9,99" "Decimal" ? ? ? ? ? ? no ? no 9.2 no
     _FldNameList[13]   > general.items_release_delivery.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[14]   > "_<CALC>"
"getGalones()" "Galones" "Galones" "zzzzz9,99" "Decimal" ? ? ? ? ? ? no ? no 9.2 no
     _FldNameList[15]   > general.items_release_delivery.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[16]   > general.items_release_delivery.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[17]   > general.items_release_delivery.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[18]   > general.items_release_delivery.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[19]   > general.items_release_delivery.id_release_delivery
"id_release_delivery" "id_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[20]   > general.items_release_delivery.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[21]   > general.items_release_delivery.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? yes 21 yes
     _FldNameList[22]   > general.items_release_delivery.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 yes
     _FldNameList[23]   > general.items_release_delivery.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[24]   > general.items_release_delivery.id_unidad_medida
"id_unidad_medida" "id_unidad_medida" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[25]   > general.items_release_delivery.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[26]   > general.items_release_delivery.item_contrato
"item_contrato" "item_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[27]   > general.items_release_delivery.item_release_delivery
"item_release_delivery" "item_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 20.2 yes
     _FldNameList[28]   > general.items_release_delivery.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[29]   > general.items_release_delivery.tambores
"tambores" "tambores" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[30]   > comercial.vapores.descripcion
"descripcion" "Vapor" "Vapor" ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[31]   > general.items_release_delivery.kilos_brutos
"kilos_brutos" "kilos_brutos" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[32]   > general.items_release_delivery.kilos_netos
"kilos_netos" "kilos_netos" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[33]   > general.items_release_delivery.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
     _FldNameList[34]   > "_<CALC>"
"getCliente()" "Cliente" "Cliente" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[35]   > "_<CALC>"
"getBroker()" "Broker" "Broker" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[36]   > "_<CALC>"
"getProducto()" "Producto" "Producto" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[37]   > "_<CALC>"
"getDestino()" "Destino" "Destino" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[38]   > "_<CALC>"
"getCondVta()" "CondicionVta" "CondicionVta" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[39]   > "_<CALC>"
"getPlazos()" "Plazos" "Plazos" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[40]   > "_<CALC>"
"getMoneda()" "Moneda" "Moneda" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no
     _FldNameList[41]   > "_<CALC>"
"getUnidadVta()" "UnidadVta" "UnidadVta" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addItemFromLoteDeposito dTables  _DB-REQUIRED
PROCEDURE addItemFromLoteDeposito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piRelease      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pcContrato     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER piItemContrato AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piTipoContrato AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piAnioContrato AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piEmpresa      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piSucursal     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piTipoTambor   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piNroMov       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piSucUbi       AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piCantidad     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pcLoteDep      AS CHARACTER NO-UNDO.


  DEFINE VARIABLE cPL    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCont  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNroPL AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iVapor AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iItem  AS INTEGER    NO-UNDO.

  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fBru AS DECIMAL    NO-UNDO.


  FIND FIRST ITEM_ingreso_lote_ubicacion WHERE ITEM_ingreso_lote_ubicacion.id_empresa            = piEmpresa
                                           AND ITEM_ingreso_lote_ubicacion.id_sucursal           = piSucursal
                                           AND ITEM_ingreso_lote_ubicacion.id_tipotambor         = piTipoTambor
                                           AND ITEM_ingreso_lote_ubicacion.nromov                = piNroMov
                                           AND ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = piSucUbi
                                           AND ITEM_ingreso_lote_ubicacion.id_lote_deposito      = pcLoteDep
                                         NO-LOCK NO-ERROR.
  IF AVAILABLE ITEM_ingreso_lote_ubicacion THEN DO:
    /*control de cantidad*/
    IF piCantidad > ITEM_ingreso_lote_ubicacion.cantidad THEN DO:
      MESSAGE "La cantidad especificada es mayor a la cantidad de tambores disponibles en el lote deposito" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      RETURN.
    END.
    /*creo un nuevo item_release*/
    
    FIND FIRST tambores_industria WHERE tambores_industria.nromov = ITEM_ingreso_lote_ubicacion.nromov NO-LOCK NO-ERROR.
    FIND LAST items_release_delivery WHERE items_release_delivery.id_release_delivery = piRelease NO-LOCK NO-ERROR.
    IF AVAILABLE items_release_delivery THEN 
      iItem = items_release_delivery.ITEM_release + 1 .
    ELSE 
      iItem = 1.
   /*  
    RUN getPackingList IN hProg (piEmpresa, 
                                 piSucursal,
                                 piTipoTambor,
                                 piNroMov, 
                                 OUTPUT cPL).
    IF cPL <> ",,,," THEN DO:
      iVapor = INTEGER(ENTRY(3, cPL)).
      cCont  = ENTRY(4, cPL).
      cNroPL = ENTRY(5, cPL).
    END.*/

    IF AVAILABLE tambores_industria THEN DO:
      fKil = tambores_industria.kilos_tambor * piCantidad.
      fBru = (tambores_industria.kilos_tambor + tambores_industria.tara) * piCantidad.
    END.

    CREATE items_release_delivery.
    ASSIGN items_release_delivery.id_empresa            = piEmpresa
           items_release_delivery.id_sucursal           = piSucursal
           items_release_delivery.id_tipotambor         = piTipoTambor
           items_release_delivery.nromov                = piNroMov
           items_release_delivery.id_sucursal_ubicacion = piSucUbi
           items_release_delivery.id_lote_deposito      = pcLoteDep
           items_release_delivery.tambores              = piCantidad
           items_release_delivery.id_release_delivery   = piRelease
           items_release_delivery.ITEM_release_delivery = iItem
           items_release_delivery.id_lote               = IF AVAILABLE tambores_industria THEN tambores_industria.id_lote ELSE 0
           items_release_delivery.id_vapor              = iVapor
           items_release_delivery.contenedor            = cCont
           items_release_delivery.id_contrato           = pcContrato
           items_release_delivery.ITEM_contrato         = piItemContrato
           items_release_delivery.id_tipo_contrato      = piTipoContrato
           items_release_delivery.anio_contrato         = piAnioContrato
           items_release_delivery.kilos_netos           = fKil
           items_release_delivery.kilos_brutos          = fBru
           items_release_delivery.c_usuario             = USERID('userdb')
           items_release_delivery.c_fecha               = TODAY
           items_release_delivery.c_hora                = STRING(TIME,"HH:MM:SS").

    /*grabar clave de item_release en tambores_industria*/
    RUN setClaveItemRelease IN hProg (piEmpresa,        /*id_empresa*/
                                      piSucursal,       /*id_sucursal*/
                                      piTipoTambor,     /*id_tipotambor*/
                                      piNroMov,         /*nromov*/
                                      piSucUbi,         /*id_sucursal_ubicacion*/
                                      piRelease,        /*id_release*/
                                      iItem,            /*item_release*/
                                      piCantidad,       /*cantidad de tambores*/
                                      pcLoteDep,        /*id_lote_deposito*/
                                      TRUE).

    DYNAMIC-FUNCTION('openQuery').           

  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE beginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".
  RUN setClaveItemRelease IN hProg (RowObjUpd.id_empresa,            /*id_empresa*/
                                    RowObjUpd.id_sucursal,           /*id_sucursal*/
                                    RowObjUpd.id_tipotambor,         /*id_tipotambor*/
                                    RowObjUpd.nromov,                /*nromov*/
                                    RowObjUpd.id_sucursal_ubicacion, /*id_sucursal_ubicacion*/
                                    RowObjUpd.id_release_delivery,   /*id_release*/
                                    RowObjUpd.ITEM_release_delivery, /*item_release*/
                                    RowObjUpd.tambores,              /*cantidad de tambores*/
                                    RowObjUpd.id_lote_deposito,      /*nro lote deposito externo*/
                                    FALSE).                          /*desvincular*/
  
  RUN setClaveItemRelease IN hProg (RowObjUpd.id_empresa,            /*id_empresa*/
                                    RowObjUpd.id_sucursal,           /*id_sucursal*/
                                    RowObjUpd.id_tipotambor,         /*id_tipotambor*/
                                    RowObjUpd.nromov,                /*nromov*/
                                    RowObjUpd.id_sucursal_ubicacion, /*id_sucursal_ubicacion*/
                                    RowObjUpd.id_release_delivery,   /*id_release*/
                                    RowObjUpd.ITEM_release_delivery, /*item_release*/
                                    RowObjUpd.tambores,              /*cantidad de tambores*/
                                    RowObjUpd.id_lote_deposito,      /*nro lote deposito externo*/
                                    TRUE).                           /*desvincular*/


  RUN releaseLoteUbicacion IN hProg (RowObjUpd.id_empresa, 
                                     RowObjUpd.id_sucursal, 
                                     RowObjUpd.id_tipotambor, 
                                     RowObjUpd.nromov, 
                                     RowObjUpd.id_sucursal_ubicacion, 
                                     RowObjUpd.tambores).

  
END.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D".
  RUN setClaveItemRelease IN hProg (RowObjUpd.id_empresa,            /*id_empresa*/
                                    RowObjUpd.id_sucursal,           /*id_sucursal*/
                                    RowObjUpd.id_tipotambor,         /*id_tipotambor*/
                                    RowObjUpd.nromov,                /*nromov*/
                                    RowObjUpd.id_sucursal_ubicacion, /*id_sucursal_ubicacion*/
                                    RowObjUpd.id_release_delivery,   /*id_release*/
                                    RowObjUpd.ITEM_release_delivery, /*item_release*/
                                    RowObjUpd.tambores,              /*cantidad de tambores*/
                                    RowObjUpd.id_lote_deposito,      /*nro lote deposito externo*/
                                    FALSE). 

  /*
  FIND FIRST lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = rowObjUpd.id_empresa
                               AND lotes_ubicacion.id_sucursal           = rowObjUpd.id_sucursal
                               AND lotes_ubicacion.id_tipotambor         = rowObjUpd.id_tipotambor
                               AND lotes_ubicacion.nromov                = rowObjUpd.nromov
                               AND lotes_ubicacion.id_sucursal_ubicacion = rowObjUpd.id_sucursal_ubicacion.
  IF AVAILABLE lotes_ubicacion THEN DO:
    lotes_ubicacion.cantidad_comprometida = lotes_ubicacion.cantidad_comprometida - rowObjUpd.tambores.
  END.
  */  
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
         rowObject.Broker = (getBroker())
         rowObject.Calidad = (getCalidad())
         rowObject.Cliente = (getCliente())
         rowObject.CondicionVta = (getCondVta())
         rowObject.Destino = (getDestino())
         rowObject.Envase = (getEnvase())
         rowObject.FechaConfirmacion = (getFechaConfirmacion())
         rowObject.Galones = (getGalones())
         rowObject.Moneda = (getMoneda())
         rowObject.NroRelease = (getNroRelease())
         rowObject.Peso = (getPeso())
         rowObject.Plazos = (getPlazos())
         rowObject.Producto = (getProducto())
         rowObject.UnidadVta = (getUnidadVta())
         rowObject.UsuarioConf = (getUsuario())
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
FIND FIRST RowObjUpd NO-ERROR. 
/*
FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D".
  /*quitar clave de item_release en tambores_industria*/
  RUN setClaveItemRelease IN hProg (RowObjUpd.id_empresa,            /*id_empresa*/
                                    RowObjUpd.id_sucursal,           /*id_sucursal*/
                                    RowObjUpd.id_tipotambor,         /*id_tipotambor*/
                                    RowObjUpd.nromov,                /*nromov*/
                                    RowObjUpd.id_sucursal_ubicacion, /*id_sucursal_ubicacion*/
                                    RowObjUpd.id_release_delivery,   /*id_release*/
                                    RowObjUpd.ITEM_release_delivery, /*item_release*/
                                    RowObjUpd.tambores,              /*cantidad de tambores*/
                                    FALSE).                          /*desvincular*/

  RUN releaseLoteUbicacion IN hProg (RowObjUpd.id_empresa, 
                                     RowObjUpd.id_sucursal, 
                                     RowObjUpd.id_tipotambor, 
                                     RowObjUpd.nromov, 
                                     RowObjUpd.id_sucursal_ubicacion, 
                                     RowObjUpd.tambores).
END.
*/
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

  RUN libItemsReleaseDelivery.p PERSISTENT SET hProg.
  
  RUN SUPER.

  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createItemReleaseFromLUbicacionContrato dTables  _DB-REQUIRED
FUNCTION createItemReleaseFromLUbicacionContrato RETURNS CHARACTER
  (INPUT hDataSource    AS HANDLE , 
   INPUT cRows          AS CHARACTER , 
   INPUT pcContrato     AS CHARACTER , 
   INPUT piItem         AS INTEGER,
   INPUT piTipoContrato AS INTEGER, 
   INPUT piAnioContrato AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VAR iRow           AS INTEGER      NO-UNDO.
  DEFINE VAR cFields        AS CHARACTER    NO-UNDO.
  DEFINE VAR hDataSource1   AS HANDLE       NO-UNDO.
  DEFINE VAR hRowObject     AS HANDLE       NO-UNDO.
  DEFINE VAR hRowObject1    AS HANDLE       NO-UNDO.

  DEFINE VAR iSuc           AS INTEGER      NO-UNDO.
  DEFINE VAR iSeq           AS INTEGER      NO-UNDO.
  DEFINE VAR iItem          AS INTEGER      NO-UNDO.
  DEFINE VAR iVapor         AS INTEGER      NO-UNDO INITIAL 9999.
  DEFINE VAR cCont          AS CHARACTER    NO-UNDO.
  DEFINE VAR cNroPL         AS CHARACTER    NO-UNDO INITIAL 20.
  DEFINE VAR iLote          AS INTEGER      NO-UNDO.
  DEFINE VAR cPL            AS CHARACTER    NO-UNDO.
  DEFINE VAR cLote          AS CHARACTER    NO-UNDO.
  DEFINE VAR iCant          AS INTEGER      NO-UNDO.
  DEFINE VAR iComp          AS INTEGER      NO-UNDO.

  DEFINE BUFFER bItemRelease FOR ITEMs_release_delivery.


  {get dataSource hDataSource1}.
  hRowObject = DYNAMIC-FUNCTION('getRowObject' IN hDataSource1).

  IF hRowObject = ? THEN DO:
    RETURN "Error en la obtencion de Ingreso".
  END.
      
  /*encuentro proximo valor de item*/
  FIND LAST items_release_delivery WHERE items_release_delivery.id_release_delivery = INTEGER(hRowObject:BUFFER-FIELD('id_release_delivery'):BUFFER-VALUE)
                                     NO-LOCK NO-ERROR.
  IF AVAILABLE items_release_delivery THEN DO:
    iItem = ITEM_release_delivery + 1.
  END.
  ELSE DO:
    iItem = 1.
  END.
  
  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow' IN hDataSource , INTEGER(ENTRY(iRow,cRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,cantidad,id_lote_deposito,lote,id_sucursal_ubicacion,cantidad_comprometida').
    IF cFields EQ ? OR cFields = "" THEN DO: 
      RETURN-VALUE = "Error en cFields".
      RETURN "Error".
    END.

    /*
    /*elimino items anteriores del mismo lote*/
    FOR EACH bItemRelease WHERE bItemRelease.id_sucursal_ubicacion = INTEGER(ENTRY(9, cFields, CHR(1)))
                            AND bItemRelease.id_empresa            = INTEGER(ENTRY(2, cFields, CHR(1)))
                            AND bItemRelease.id_sucursal           = INTEGER(ENTRY(3, cFields, CHR(1)))
                            AND bItemRelease.id_tipotambor         = INTEGER(ENTRY(4, cFields, CHR(1)))
                            AND bItemRelease.nromov                = INTEGER(ENTRY(5, cFields, CHR(1))).
      DELETE bItemRelease.
    END.
    */
    /*obtengo datos de packing_list*/
    RUN getPackingList IN hProg (INPUT INTEGER(ENTRY(2, cFields, CHR(1))), 
                                 INPUT INTEGER(ENTRY(3, cFields, CHR(1))),
                                 INPUT INTEGER(ENTRY(4, cFields, CHR(1))),
                                 INPUT INTEGER(ENTRY(5, cFields, CHR(1))), 
                                 OUTPUT cPL).
     IF cPL <> ",,,," THEN DO:
       iVapor = INTEGER(ENTRY(3, cPL)).
       cCont  = ENTRY(4, cPL).
       cNroPL = ENTRY(5, cPL).
     END.

     RUN getLoteDeposito IN hProg (INPUT INTEGER(ENTRY(2, cFields, CHR(1))), 
                                   INPUT INTEGER(ENTRY(3, cFields, CHR(1))),
                                   INPUT INTEGER(ENTRY(4, cFields, CHR(1))),
                                   INPUT INTEGER(ENTRY(5, cFields, CHR(1))), 
                                   OUTPUT cLote).
    iCant = INTEGER(ENTRY(6, cFields, CHR(1))) - INTEGER(ENTRY(10, cFields, CHR(1))).
    /*creo item release*/
    CREATE items_release_delivery.
    ASSIGN items_release_delivery.id_release_delivery   = hRowObject:BUFFER-FIELD('id_release_delivery'):BUFFER-VALUE
           items_release_delivery.ITEM_release_delivery = iItem
           items_release_delivery.id_sucursal_ubicacion = INTEGER(ENTRY(9, cFields, CHR(1)))
           items_release_delivery.id_empresa            = INTEGER(ENTRY(2, cFields, CHR(1)))
           items_release_delivery.id_sucursal           = INTEGER(ENTRY(3, cFields, CHR(1)))
           items_release_delivery.id_tipotambor         = INTEGER(ENTRY(4, cFields, CHR(1)))
           items_release_delivery.nromov                = INTEGER(ENTRY(5, cFields, CHR(1)))
           items_release_delivery.tambores              = iCant
           items_release_delivery.id_lote_deposito      = cLote
           items_release_delivery.id_lote               = INTEGER(SUBSTRING(ENTRY(8, cFields, CHR(1)), 1, 6))
           items_release_delivery.id_vapor              = iVapor
           items_release_delivery.contenedor            = cCont
           items_release_delivery.id_contrato           = pcContrato
           items_release_delivery.ITEM_contrato         = piItem
           items_release_delivery.id_tipo_contrato      = piTipoContrato
           items_release_delivery.anio_contrato         = piAnioContrato.
    
    /*grabar clave de item_release en tambores_industria*/
    RUN setClaveItemRelease IN hProg (INTEGER(ENTRY(2, cFields, CHR(1))),                           /*id_empresa*/
                                      INTEGER(ENTRY(3, cFields, CHR(1))),                           /*id_sucursal*/
                                      INTEGER(ENTRY(4, cFields, CHR(1))),                           /*id_tipotambor*/
                                      INTEGER(ENTRY(5, cFields, CHR(1))),                           /*nromov*/
                                      INTEGER(ENTRY(9, cFields, CHR(1))),                           /*id_sucursal_ubicacion*/
                                      hRowObject:BUFFER-FIELD('id_release_delivery'):BUFFER-VALUE,  /*id_release*/
                                      iItem,                                                        /*item_release*/
                                      iCant,                                                        /*cantidad de tambores*/
                                      TRUE).  
                                                          
    
    
    /*vincular*/
    
    iItem = iItem + 1.
    
    /*actualizo la cantidad_comprometida en lote_ubicacion*/
    
    FOR FIRST lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = INTEGER(ENTRY(2, cFields, CHR(1)))
                                AND lotes_ubicacion.id_sucursal           = INTEGER(ENTRY(3, cFields, CHR(1)))
                                AND lotes_ubicacion.id_tipotambor         = INTEGER(ENTRY(4, cFields, CHR(1)))
                                AND lotes_ubicacion.nromov                = INTEGER(ENTRY(5, cFields, CHR(1)))
                                AND lotes_ubicacion.id_sucursal_ubicacion = INTEGER(ENTRY(9, cFields, CHR(1))).
      ASSIGN lotes_ubicacion.cantidad_comprometida = lotes_ubicacion.cantidad_comprometida + iCant.
    END.
    
    
  END.

  openQuery().
  DYNAMIC-FUNCTION('openQuery' IN hDataSource).
  
  DEFINE VARIABLE hData      AS HANDLE     NO-UNDO.

  hData = DYNAMIC-FUNCTION('getDataSource').
  RUN refreshRow IN hData.

  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBroker dTables  _DB-REQUIRED
FUNCTION getBroker RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  
  FIND FIRST contratos WHERE contratos.id_contrato      = rowObject.id_contrato
                         AND contratos.id_tipo_contrato = rowObject.id_tipo_contrato
                         AND contratos.anio             = rowObject.anio_contrato
                       NO-LOCK NO-ERROR.
  FIND FIRST contactos_industria WHERE contratos.id_broker = contactos_industria.id_contacto
                                 NO-LOCK NO-ERROR.
  IF AVAILABLE contactos_industria THEN 
    RETURN contactos_industria.razon_social.
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
  
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO INITIAL "".
  FIND CURRENT RowObject NO-ERROR.
  IF AVAILABLE RowObject THEN DO:
    RUN getCalidad IN hProg (RowObject.id_empresa, 
                             RowObject.id_sucursal, 
                             RowObject.id_tipotambor, 
                             RowObject.nromov, 
                             OUTPUT cRet).
    RETURN cRet.
  END.
 
  
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
  FIND FIRST release_delivery WHERE release_delivery.id_release_delivery = rowObject.id_release_delivery
                    NO-LOCK NO-ERROR.
  IF AVAILABLE release_delivery THEN DO:  
    FIND FIRST clientes WHERE clientes.id_cliente = RELEASE_delivery.id_cliente
                        NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
      RETURN clientes.razon_social.
  END.
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
  FIND FIRST items_contratos WHERE items_contratos.id_contrato      = rowObject.id_contrato
                               AND items_contratos.id_tipo_contrato = rowObject.id_tipo_contrato
                               AND items_contratos.anio             = rowObject.anio_contrato
                               AND items_contratos.ITEM             = rowObject.ITEM_contrato
                             NO-LOCK NO-ERROR.
  FIND FIRST clausulas OF items_contratos NO-LOCK NO-ERROR.
  IF AVAILABLE clausulas THEN 
    RETURN clausulas.descripcion.
  ELSE
    RETURN "".

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
  FIND FIRST items_contratos WHERE items_contratos.id_contrato      = rowObject.id_contrato
                               AND items_contratos.id_tipo_contrato = rowObject.id_tipo_contrato
                               AND items_contratos.anio             = rowObject.anio_contrato
                               AND items_contratos.ITEM             = rowObject.ITEM_contrato
                             NO-LOCK NO-ERROR.
  FIND FIRST destinos OF items_contratos
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  FIND CURRENT RowObject NO-ERROR.
  RUN getEnvase IN hProg (RowObject.id_empresa, 
                          RowObject.id_sucursal, 
                          RowObject.id_tipotambor, 
                          RowObject.nromov, 
                          OUTPUT cRet).
  RETURN cRet.
 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFechaConfirmacion dTables  _DB-REQUIRED
FUNCTION getFechaConfirmacion RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vdRet AS DATE    NO-UNDO.
  FIND CURRENT rowObject NO-ERROR.

  FIND FIRST RELEASE_delivery WHERE RELEASE_delivery.id_release_delivery = rowObject.id_release_delivery NO-LOCK NO-ERROR.
  IF AVAILABLE RELEASE_delivery THEN
    vdRet = RELEASE_delivery.fecha_confirmacion_entrega.

  RETURN vdRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGalones dTables  _DB-REQUIRED
FUNCTION getGalones RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
 
DEFINE VAR dPeso AS DECIMAL NO-UNDO.
  DEFINE VAR dGal  AS DECIMAL NO-UNDO.

  FIND CURRENT RowOBject NO-LOCK NO-ERROR.

  IF AVAILABLE RowOBject THEN DO:
    FIND LAST inspecciones_lote WHERE inspecciones_lote.id_empresa       =   RowObject.id_empresa AND 
                                      inspecciones_lote.id_sucursal      =   RowObject.id_sucursal AND
                                      inspecciones_lote.id_tipotambor    =   RowObject.id_tipotambor AND
                                      inspecciones_lote.nromov           =   RowObject.nromov 
                                NO-LOCK NO-ERROR.
    IF AVAILABLE inspecciones_lote THEN DO:                                     
      FOR EACH tambores_industria  WHERE  tambores_industria.nromov                = RowObject.nromov AND
                                          tambores_industria.id_sucursal_ubicacion = RowObject.id_sucursal_ubicacion 
                                   NO-LOCK.
        dPeso = dPeso + tambores_industria.kilos_tambor.
      END.
      FIND LAST brix WHERE brix.brix <= ROUND(inspecciones_lote.bx_correg,1) 
                     NO-LOCK NO-ERROR.
      IF AVAILABLE brix THEN DO:
        dGal = (dPeso / brix.pe) / 3.785.
      END.
    END.
  END.

  RETURN dGal.   /* Function return value. */

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
   FIND FIRST items_contratos WHERE items_contratos.id_contrato      = rowObject.id_contrato
                               AND items_contratos.id_tipo_contrato = rowObject.id_tipo_contrato
                               AND items_contratos.anio             = rowObject.anio_contrato
                               AND items_contratos.ITEM             = rowObject.ITEM_contrato
                             NO-LOCK NO-ERROR.
  FIND FIRST items_orden_entrega OF items_contratos NO-LOCK NO-ERROR.
  IF AVAILABLE items_orden_entrega THEN DO:
    FIND FIRST tipo_moneda OF items_orden_entrega NO-LOCK NO-ERROR.
    IF AVAILABLE tipo_moneda THEN 
      RETURN tipo_moneda.descripcion.
    ELSE 
      RETURN "".
  END.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNroRelease dTables  _DB-REQUIRED
FUNCTION getNroRelease RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viRet AS INTEGER    NO-UNDO.
  FIND CURRENT rowObject NO-ERROR.

  FIND FIRST RELEASE_delivery WHERE RELEASE_delivery.id_release_delivery = rowObject.id_release_delivery NO-LOCK NO-ERROR.
  IF AVAILABLE RELEASE_delivery THEN
    viRet = RELEASE_delivery.numero_release.

  RETURN viRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPeso dTables  _DB-REQUIRED
FUNCTION getPeso RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  DEFINE VAR dPeso AS DECIMAL NO-UNDO.

  FIND CURRENT RowOBject NO-LOCK NO-ERROR.
  IF AVAILABLE RowOBject THEN DO:
    FOR EACH tambores_industria  WHERE  tambores_industria.nromov                = RowObject.nromov AND
                                        tambores_industria.id_sucursal_ubicacion = RowObject.id_sucursal_ubicacion 
                                 NO-LOCK.
        dPeso = dPeso + tambores_industria.kilos_tambor.
    END.
  END.
  

  RETURN dPeso.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPlazos dTables  _DB-REQUIRED
FUNCTION getPlazos RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST contratos WHERE contratos.id_contrato      = rowObject.id_contrato
                         AND contratos.id_tipo_contrato = rowObject.id_tipo_contrato
                         AND contratos.anio             = rowObject.anio_contrato
                       NO-LOCK NO-ERROR.
  FIND FIRST tipos_plazo OF contratos NO-LOCK NO-ERROR.
  IF AVAILABLE tipos_plazo THEN 
    RETURN tipos_plazo.descripcion.
  ELSE
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProducto dTables  _DB-REQUIRED
FUNCTION getProducto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  trae la descripcion del producto del primer tambor, esto esta mal, 
            porque puedo tener un release con tambores de distintos productoes
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST tambores_industria WHERE tambores_industria.id_release_delivery   = rowObject.id_release_delivery
                                 AND tambores_industria.ITEM_release_delivery = rowObject.ITEM_release
                               NO-LOCK.
    FOR FIRST productos_terminados OF tambores_industria.
      cRet = productos_terminados.descripcion.
    END.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidadVta dTables  _DB-REQUIRED
FUNCTION getUnidadVta RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST items_contratos WHERE items_contratos.id_contrato      = rowObject.id_contrato
                               AND items_contratos.id_tipo_contrato = rowObject.id_tipo_contrato
                               AND items_contratos.anio             = rowObject.anio_contrato
                               AND items_contratos.ITEM             = rowObject.ITEM_contrato
                             NO-LOCK NO-ERROR.
  FIND FIRST tipo_unidad_venta OF items_contratos NO-LOCK NO-ERROR.
  IF AVAILABLE tipo_unidad_venta THEN 
    RETURN tipo_unidad_venta.descripcion.
  ELSE
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUsuario dTables  _DB-REQUIRED
FUNCTION getUsuario RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcRet AS CHARACTER NO-UNDO.
  FIND CURRENT rowObject NO-ERROR.

  FIND FIRST RELEASE_delivery WHERE RELEASE_delivery.id_release_delivery = rowObject.id_release_delivery NO-LOCK NO-ERROR.
  IF AVAILABLE RELEASE_delivery THEN
    vcRet = RELEASE_delivery.c_usuario.

  RETURN vcRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

