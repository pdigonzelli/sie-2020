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

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.


DEFINE TEMP-TABLE ttTraza
  RCODE-INFORMATION
  FIELD fechal       AS DATE COLUMN-LABEL "Fecha Creacion"
  FIELD planta       AS INTEGER COLUMN-LABEL "Planta Produccion"
  FIELD lote_sami    AS INTEGER COLUMN-LABEL "Lote Sami"
  FIELD anio_lote    AS INTEGER COLUMN-LABEL "Anio Lote"
  FIELD lote_cliente AS CHARACTER COLUMN-LABEL "Lote Cliente"
  FIELD cantidadl    AS INTEGER COLUMN-LABEL "Cantidad Produccion"
  FIELD nro_remito   AS INTEGER COLUMN-LABEL "Nro Rto Int"
  FIELD nro_comp     AS CHARACTER COLUMN-LABEL "Nro Comprobante"
  FIELD id_suc_ori   AS INTEGER COLUMN-LABEL "Cod Suc Ori"
  FIELD desde        AS CHARACTER COLUMN-LABEL "Suc Desde"
  FIELD id_suc_des   AS INTEGER COLUMN-LABEL "Cod Suc Des"
  FIELD hasta        AS CHARACTER COLUMN-LABEL "Suc Hasta"
  FIELD fechar       AS DATE COLUMN-LABEL "Fecha Rto"
  FIELD cantidadr    AS INTEGER COLUMN-LABEL "Cantidad Rto"
  FIELD kilosr       AS INTEGER COLUMN-LABEL "Kilos"
  FIELD qty_lote     AS INTEGER COLUMN-LABEL "Cantidad Tot Lote"
  FIELD nromov       AS INTEGER COLUMN-LABEL "Nromov".

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
&Scoped-define INTERNAL-TABLES lotes_cascara

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio c_fecha c_hora c_usuario Fecha id_articulo id_calidad id_empresa~
 id_envase id_lote id_sucursal id_tipotambor nromov observaciones~
 id_orden_entrega item_oe id_contrato item_contrato id_tipo_contrato~
 anio_contrato control_calidad usuario_control_calidad id_sucursal_remito~
 id_tipo_movsto nro_remito id_lote_cliente anio_cliente codigo_lote
&Scoped-define ENABLED-FIELDS-IN-lotes_cascara anio c_fecha c_hora ~
c_usuario Fecha id_articulo id_calidad id_empresa id_envase id_lote ~
id_sucursal id_tipotambor nromov observaciones id_orden_entrega item_oe ~
id_contrato item_contrato id_tipo_contrato anio_contrato control_calidad ~
usuario_control_calidad id_sucursal_remito id_tipo_movsto nro_remito ~
id_lote_cliente anio_cliente codigo_lote 
&Scoped-Define DATA-FIELDS  anio Sucursal c_fecha c_hora c_usuario Fecha Vapor id_articulo id_calidad~
 id_empresa id_envase Cantidad id_lote id_sucursal id_tipotambor nromov~
 observaciones id_orden_entrega item_oe id_contrato item_contrato~
 id_tipo_contrato anio_contrato control_calidad usuario_control_calidad~
 id_sucursal_remito id_tipo_movsto nro_remito id_lote_cliente anio_cliente~
 codigo_lote Producto
&Scoped-define DATA-FIELDS-IN-lotes_cascara anio c_fecha c_hora c_usuario ~
Fecha id_articulo id_calidad id_empresa id_envase id_lote id_sucursal ~
id_tipotambor nromov observaciones id_orden_entrega item_oe id_contrato ~
item_contrato id_tipo_contrato anio_contrato control_calidad ~
usuario_control_calidad id_sucursal_remito id_tipo_movsto nro_remito ~
id_lote_cliente anio_cliente codigo_lote 
&Scoped-Define MANDATORY-FIELDS  id_calidad id_lote id_sucursal id_sucursal_remito id_lote_cliente
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dlotescascara.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH lotes_cascara NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH lotes_cascara NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main lotes_cascara
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main lotes_cascara


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArrayLotes dTables  _DB-REQUIRED
FUNCTION getArrayLotes RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArrayUbicacion dTables  _DB-REQUIRED
FUNCTION getArrayUbicacion RETURNS CHARACTER
  (INPUT piNroMov AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadBolsasLote dTables  _DB-REQUIRED
FUNCTION getCantidadBolsasLote RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadReproceso dTables  _DB-REQUIRED
FUNCTION getCantidadReproceso RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadRestante dTables  _DB-REQUIRED
FUNCTION getCantidadRestante RETURNS INTEGER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlCalidadAprobado dTables  _DB-REQUIRED
FUNCTION getControlCalidadAprobado RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIdVapor dTables  _DB-REQUIRED
FUNCTION getIdVapor RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteDespachado dTables  _DB-REQUIRED
FUNCTION getLoteDespachado RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLotesSucursal dTables  _DB-REQUIRED
FUNCTION getLotesSucursal RETURNS CHARACTER
  ( INPUT piSucursal AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroLote dTables  _DB-REQUIRED
FUNCTION getNextNroLote RETURNS INTEGER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
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
      lotes_cascara
    FIELDS(lotes_cascara.anio
      lotes_cascara.c_fecha
      lotes_cascara.c_hora
      lotes_cascara.c_usuario
      lotes_cascara.Fecha
      lotes_cascara.id_articulo
      lotes_cascara.id_calidad
      lotes_cascara.id_empresa
      lotes_cascara.id_envase
      lotes_cascara.id_lote
      lotes_cascara.id_sucursal
      lotes_cascara.id_tipotambor
      lotes_cascara.nromov
      lotes_cascara.observaciones
      lotes_cascara.id_orden_entrega
      lotes_cascara.item_oe
      lotes_cascara.id_contrato
      lotes_cascara.item_contrato
      lotes_cascara.id_tipo_contrato
      lotes_cascara.anio_contrato
      lotes_cascara.control_calidad
      lotes_cascara.usuario_control_calidad
      lotes_cascara.id_sucursal_remito
      lotes_cascara.id_tipo_movsto
      lotes_cascara.nro_remito
      lotes_cascara.id_lote_cliente
      lotes_cascara.anio_cliente
      lotes_cascara.codigo_lote) SCROLLING.
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
     _TblList          = "general.lotes_cascara"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = "USED"
     _FldNameList[1]   > general.lotes_cascara.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > "_<CALC>"
"getSucursal()" "Sucursal" "Sucursal" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[3]   > general.lotes_cascara.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[4]   > general.lotes_cascara.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[5]   > general.lotes_cascara.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[6]   > general.lotes_cascara.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[7]   > "_<CALC>"
"getVapor()" "Vapor" "Vapor" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[8]   > general.lotes_cascara.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[9]   > general.lotes_cascara.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[10]   > general.lotes_cascara.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[11]   > general.lotes_cascara.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[12]   > "_<CALC>"
"getCantidadBolsasLote()" "Cantidad" "Cantidad" ">,>>9" "Integer" ? ? ? ? ? ? no ? no 8.4 no
     _FldNameList[13]   > general.lotes_cascara.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[14]   > general.lotes_cascara.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[15]   > general.lotes_cascara.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[16]   > general.lotes_cascara.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[17]   > general.lotes_cascara.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
     _FldNameList[18]   > general.lotes_cascara.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[19]   > general.lotes_cascara.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[20]   > general.lotes_cascara.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[21]   > general.lotes_cascara.item_contrato
"item_contrato" "item_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[22]   > general.lotes_cascara.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[23]   > general.lotes_cascara.anio_contrato
"anio_contrato" "anio_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[24]   > general.lotes_cascara.control_calidad
"control_calidad" "control_calidad" ? ? "logical" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[25]   > general.lotes_cascara.usuario_control_calidad
"usuario_control_calidad" "usuario_control_calidad" ? ? "character" ? ? ? ? ? ? yes ? no 22.4 yes
     _FldNameList[26]   > general.lotes_cascara.id_sucursal_remito
"id_sucursal_remito" "id_sucursal_remito" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[27]   > general.lotes_cascara.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[28]   > general.lotes_cascara.nro_remito
"nro_remito" "nro_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[29]   > general.lotes_cascara.id_lote_cliente
"id_lote_cliente" "id_lote_cliente" ? ? "integer" ? ? ? ? ? ? yes ? yes 13.4 yes
     _FldNameList[30]   > general.lotes_cascara.anio_cliente
"anio_cliente" "anio_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[31]   > general.lotes_cascara.codigo_lote
"codigo_lote" "codigo_lote" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[32]   > "_<CALC>"
"getProducto()" "Producto" "Producto" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addQty dTables  _DB-REQUIRED
PROCEDURE addQty :
/*------------------------------------------------------------------------------
  Purpose:     vincula o desvincula una produccion a un lote y registra en stock
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piQty        AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER piEmpresa    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER piSucLote    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER piNroMovLote AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER piSucPro     AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER piProduccion AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pcSigno      AS CHARACTER NO-UNDO.


IF pcSigno = "+" THEN DO:
  FIND r_produccion_cascara_lote WHERE r_produccion_cascara_lote.id_empresa_lote    = piEmpresa
                                   AND r_produccion_cascara_lote.id_sucursal_lote   = piSucLote
                                   AND r_produccion_cascara_lote.id_tipotambor      = 11
                                   AND r_produccion_cascara_lote.nromov_lote        = piNroMovLote
                                   AND r_produccion_cascara_lote.id_sucursal_prod   = piSucPro
                                   AND r_produccion_cascara_lote.id_produccion_prod = piProduccion
                                 NO-ERROR.
  IF AVAILABLE r_produccion_cascara_lote THEN DO:
    /*si existe sumo la cantidad parametro*/
    ASSIGN r_produccion_cascara_lote.cantidad = r_produccion_cascara_lote.cantidad + piQty.
  END.
  ELSE DO:
    /*si no existe creo el registro*/
    CREATE r_produccion_cascara_lote.
    ASSIGN r_produccion_cascara_lote.id_empresa_lote    = piEmpresa
           r_produccion_cascara_lote.id_sucursal_lote   = piSucLote
           r_produccion_cascara_lote.id_tipotambor_lote = 11
           r_produccion_cascara_lote.nromov_lote        = piNroMovLote
           r_produccion_cascara_lote.id_sucursal_prod   = piSucPro
           r_produccion_cascara_lote.id_produccion      = piProduccion
           r_produccion_cascara.cantidad                = piQty
           r_produccion_cascara.c_fecha                 = TODAY
           r_produccion_cascara.c_usuario               = USERID('userdb')
           r_produccion_cascara.c_hora                  = STRING(TIME, "HH:MM:SS")
           .
  END.
  /*doy de baja la produccion para que cuadre el stock*/
  RUN y_gstkmovdep_cascara.p (piEmpresa, 
                              piSucPro,
                              12,
                              piProduccion,
                              piSucLote * 10,
                              piSucLote,
                              1,
                              piQty,
                              35,
                              TODAY).
  
  RUN transferenciaLoteCascara IN hLib (piEmpresa, 
                                        piSucLote, 
                                        piNroMovLote,
                                        piSucLote * 10, 
                                        piSucLote, 
                                        piQty).
  /*
  RUN transferirLoteCascara (piEmpresa, 
                             piSucLote, 
                             piNroMovLote, 
                             piSucLote * 10, 
                             piSucLote, 
                             piQty, 
                             pcSigno, 
                             33).
  */                           
END.


IF pcSigno = "-" THEN DO:
  FIND r_produccion_cascara_lote WHERE r_produccion_cascara_lote.id_empresa_lote    = piEmpresa
                                   AND r_produccion_cascara_lote.id_sucursal_lote   = piSucLote
                                   AND r_produccion_cascara_lote.id_tipotambor      = 11
                                   AND r_produccion_cascara_lote.nromov_lote        = piNroMovLote
                                   AND r_produccion_cascara_lote.id_sucursal_prod   = piSucPro
                                   AND r_produccion_cascara_lote.id_produccion_prod = piProduccion
                                 NO-ERROR.
  IF AVAILABLE r_produccion_cascara_lote THEN DO:
    IF piQty >= r_produccion_cascara_lote.cantidad THEN DO:
      /*si la cantidad parametro es igual a la cantidad de la tabla borro el registro completo*/
      DELETE r_produccion_cascara_lote.
    END.
    ELSE DO:
      /*si la cantidad parametro es menor que la cantidad en la tabla entonces resto en este registro*/
      ASSIGN r_produccion_cascara_lote.cantidad = r_produccion_cascara_lote.cantidad - piQty.
    END.
  END.
  /*doy de alta la produccion*/
  RUN y_gstkmovdep_cascara.p (piEmpresa, 
                              piSucPro,
                              12,
                              piProduccion,
                              piSucLote,
                              piSucLote * 10,
                              1,
                              piQty,
                              35,
                              TODAY).
  
  RUN transferenciaLoteCascara IN hLib (piEmpresa, 
                                        piSucLote, 
                                        piNroMovLote,
                                        piSucLote, 
                                        piSucLote * 10, 
                                        piQty).

  /*RUN transferirLoteCascara (piEmpresa, piSucLote, piNroMovLote, piSucLote, piSucLote * 10, piQty, pcSigno, 33).*/
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

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".


  ASSIGN rowObjUpd.id_empresa    = 1
         rowObjUpd.id_tipotambor = 11
         rowObjUpd.nromov        = NEXT-VALUE(nromov)
         rowObjUpd.c_usuario     = USERID("userdb")
         rowObjUpd.c_fecha       = TODAY
         rowObjUpd.c_hora        = STRING(TIME, "HH:MM:SS")
         . 

  rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_empresa,id_tipotambor,nromov,c_usuario,c_fecha,c_hora,codigo_lote".
END.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".

  FIND FIRST lotes_cascara OF rowObjUpd NO-LOCK NO-ERROR.
  IF lotes_cascara.id_orden_entrega <> rowObjUpd.id_orden_entrega AND lotes_cascara.ITEM_oe <> rowObjUpd.ITEM_oe THEN DO:  /*edito campos de orden_entrega e item*/
    /*entra aqui solo para la ventana de asociar lotes con oes*/
    /*chequear que la oe sea de cascara*/
    FIND items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = rowObjUpd.id_orden_entrega
                               AND items_orden_entrega.ITEM_oe          = rowObjUpd.ITEM_oe
                               AND items_orden_entrega.id_articulo      = 54
                             NO-LOCK NO-ERROR.
    IF NOT AVAILABLE items_orden_entrega THEN 
      RETURN "Esta Parte de OE no es de Cascara".
  END.

  IF NOT getControlCalidadAprobado() THEN
    RETURN "Este lote no esta aprobado por el control de calidad.".
  
  ASSIGN rowObjUpd.c_usuario = USERID("userdb")
         rowObjUpd.c_fecha   = TODAY
         rowObjUpd.c_hora    = STRING(TIME, "HH:MM:SS").

  rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".
END.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D".
  /*eliminar las producciones que conforman el lote y devolver al stock las bolsas*/
  FOR EACH r_produccion_cascara_lote WHERE r_produccion_cascara_lote.id_empresa_lote    = rowObjUpd.id_empresa
                                       AND r_produccion_cascara_lote.id_sucursal_lote   = rowObjUpd.id_sucursal
                                       AND r_produccion_cascara_lote.id_tipotambor_lote = rowObjUpd.id_tipotambor
                                       AND r_produccion_cascara_lote.nromov_lote        = rowObjUpd.nromov.
    RUN addQty (r_produccion_cascara_lote.cantidad, 
                r_produccion_cascara_lote.id_empresa_lote,
                r_produccion_cascara_lote.id_sucursal_lote,
                r_produccion_cascara_lote.nromov_lote,
                r_produccion_cascara_lote.id_sucursal_prod, 
                r_produccion_cascara_lote.id_produccion_prod, 
                "-").
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
         rowObject.Cantidad = (getCantidadBolsasLote())
         rowObject.Producto = (getProducto())
         rowObject.Sucursal = (getSucursal())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillttTraza dTables  _DB-REQUIRED
PROCEDURE fillttTraza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttTraza.
  DEFINE INPUT  PARAMETER piSucursal AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdDesde    AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER pdHasta    AS DATE       NO-UNDO.

  
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAux   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAux2  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFlag  AS LOGICAL    NO-UNDO.
  DEFINE BUFFER buOri FOR sucursales.
  DEFINE BUFFER buDes FOR sucursales.

  FOR EACH ttTraza.
    DELETE ttTraza.
  END.


  FOR EACH lotes_cascara WHERE (IF piSucursal <> 0 THEN lotes_cascara.id_sucursal = piSucursal ELSE TRUE) 
                           AND lotes_cascara.fecha >= pdDesde
                           AND lotes_cascara.fecha <= pdHasta
                         NO-LOCK.
    lFlag = FALSE.
    iCount = 0.
    FOR EACH r_produccion_cascara_lote WHERE r_produccion_cascara_lote.id_empresa_lote    = lotes_cascara.id_empresa
                                         AND r_produccion_cascara_lote.id_sucursal_lote   = lotes_cascara.id_sucursal
                                         AND r_produccion_cascara_lote.id_tipotambor_lote = lotes_cascara.id_tipotambor
                                         AND r_produccion_cascara_lote.nromov_lote        = lotes_cascara.nromov
                                       NO-LOCK.
      iCount = iCount + r_produccion_cascara_lote.cantidad.
    END.
    iAux = iCount.


    FOR EACH r_lote_cascara_remito OF lotes_cascara NO-LOCK, 
      FIRST items_factura WHERE r_lote_cascara_remito.id_sucursal_remito = items_factura.id_sucursal
                            AND r_lote_cascara_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                            AND r_lote_cascara_remito.nro_remito         = items_factura.nro
                            AND r_lote_cascara_remito.ITEM_factura       = items_factura.ITEM
                          NO-LOCK, 
      FIRST remitos OF items_factura WHERE remitos.estado = TRUE
                                       AND remitos.fecha <= pdHasta
                                     NO-LOCK.
  
      FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
      FIND FIRST buOri OF remitos NO-LOCK NO-ERROR.
      FIND FIRST buDes OF lugar_descarga NO-LOCK NO-ERROR.
      
      lFlag  = TRUE.
      iCount = iCount - items_factura.cantidad.

      CREATE ttTraza.
      ASSIGN ttTraza.fechal       = lotes_cascara.fecha
             ttTraza.planta       = lotes_cascara.id_sucursal
             ttTraza.lote_sami    = lotes_cascara.id_lote
             ttTraza.anio_lote    = lotes_cascara.anio
             ttTraza.lote_cliente = STRING(lotes_cascara.id_lote_cliente)
             ttTraza.cantidadl    = iCount
             ttTraza.nro_remito   = remitos.nro
             ttTraza.nro_comp     = STRING(remitos.nro_comp, "9999-99999999")
             ttTraza.id_suc_ori   = remitos.id_sucursal
             ttTraza.desde        = buOri.abreviatura
             ttTraza.id_suc_des   = lugar_descarga.id_sucursal
             ttTraza.hasta        = buDes.abreviatura
             ttTraza.fechar       = remitos.fecha
             ttTraza.cantidadr    = items_factura.cantidad
             ttTraza.kilosr       = items_factura.cantidad * 50
             ttTraza.qty_lote     = iAux
             ttTraza.nromov       = lotes_cascara.nromov.
      IF iCount = 0 THEN 
        iCount = iAux.
    END.

    IF NOT lFlag THEN DO: /*lotes sin remitos*/
      FIND FIRST lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = lotes_cascara.id_empresa
                                   AND lotes_ubicacion.id_sucursal           = lotes_cascara.id_sucursal
                                   AND lotes_ubicacion.id_tipotambor         = lotes_cascara.id_tipotambor
                                   AND lotes_ubicacion.nromov                = lotes_cascara.nromov
                                   AND lotes_ubicacion.id_sucursal_ubicacion = lotes_cascara.id_sucursal
                                 NO-LOCK NO-ERROR.
      IF AVAILABLE lotes_ubicacion THEN DO:      
        CREATE ttTraza.
        ASSIGN ttTraza.fechal       = lotes_cascara.fecha
               ttTraza.planta       = lotes_cascara.id_sucursal
               ttTraza.lote_sami    = lotes_cascara.id_lote
               ttTraza.anio_lote    = lotes_cascara.anio
               ttTraza.lote_cliente = STRING(lotes_cascara.id_lote_cliente)
               ttTraza.cantidadl    = iCount
               ttTraza.nro_remito   = 0
               ttTraza.nro_comp     = ""
               ttTraza.id_suc_ori   = 0
               ttTraza.desde        = ""
               ttTraza.id_suc_des   = 0
               ttTraza.hasta        = ""
               ttTraza.fechar       = ?
               ttTraza.cantidadr    = 0
               ttTraza.kilosr       = 0
               ttTraza.qty_lote     = iCount
               ttTraza.nromov       = lotes_cascara.nromov.
      END.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_envaseValidate dTables  _DB-REQUIRED
PROCEDURE id_envaseValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValue  AS CHARACTER  NO-UNDO.

  IF pcValue = "" THEN
    RETURN "Envase Invalido".
  ELSE
    RETURN "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_orden_entregaValidate dTables  _DB-REQUIRED
PROCEDURE id_orden_entregaValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcItem AS CHARACTER NO-UNDO.
/*
  IF INTEGER(pcItem) <= 0 THEN
    RETURN "Error en Numero de OE".
  ELSE
    RETURN "".
*/
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

  RUN libLotesUbicacion.p PERSISTENT SET hLib.

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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setControlCalidad dTables  _DB-REQUIRED
PROCEDURE setControlCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER plEstado AS LOGICAL NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST lotes_cascara OF rowObject NO-ERROR.
  IF AVAILABLE lotes_cascara THEN DO:
    ASSIGN lotes_cascara.CONTROL_calidad = plEstado
           lotes_cascara.usuario_control_calidad = USERID("userdb").
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferirLoteCascara dTables  _DB-REQUIRED
PROCEDURE transferirLoteCascara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*1.- si existe en lote_ubicacion ==> modificar cantidad, sino ==> crear lote_ubicacion
  2.- llamar a la rutina de cacha para stock.*/
  DEFINE INPUT PARAMETER piEmpresa  AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucursal AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piNroMov   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucUbi   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucDes   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piCantidad AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER pcSigno    AS CHARACTER  NO-UNDO.
  DEFINE INPUT PARAMETER piTipoMov  AS INTEGER    NO-UNDO.

  DEFINE VARIABLE viSucOri AS INTEGER    NO-UNDO.
  DEFINE VARIABLE viSucDes AS INTEGER    NO-UNDO.

  DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
    FIND lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = piEmpresa 
                           AND lotes_ubicacion.id_sucursal           = piSucursal
                           AND lotes_ubicacion.id_tipotambor         = 11
                           AND lotes_ubicacion.nromov                = piNroMov
                           AND lotes_ubicacion.id_sucursal_ubicacion = piSucUbi
                         NO-ERROR.
    IF AVAILABLE lotes_ubicacion THEN DO:
      IF pcSigno = "+" THEN
        ASSIGN lotes_ubicacion.cantidad = lotes_ubicacion.cantidad + piCantidad.
      ELSE DO:
        ASSIGN lotes_ubicacion.cantidad = lotes_ubicacion.cantidad - piCantidad.
        IF lotes_ubicacion.cantidad <= 0 THEN
          DELETE lotes_ubicacion.
      END.
    END.
    ELSE DO:
      FIND lotes_cascara WHERE lotes_cascara.nromov = piNroMov NO-LOCK NO-ERROR.
      FIND calidades WHERE calidades.id_calidad = lotes_cascara.id_calidad NO-LOCK NO-ERROR.
      FIND envases_prod WHERE lotes_cascara.id_envase = envases_prod.id_envase NO-LOCK NO-ERROR.
      CREATE  lotes_ubicacion.
      ASSIGN  lotes_ubicacion.id_empresa            = piEmpresa
              lotes_ubicacion.id_sucursal           = piSucursal
              lotes_ubicacion.id_tipotambor         = 11
              lotes_ubicacion.nromov                = piNroMov
              lotes_ubicacion.id_sucursal_ubicacion = piSucDes
              lotes_ubicacion.cantidad              = piCantidad
              lotes_ubicacion.id_lote               = lotes_cascara.id_lote
              lotes_ubicacion.calidad               = calidades.descripcion
              lotes_ubicacion.envase                = envases_prod.descripcion
              lotes_ubicacion.lote                  = STRING(lotes_cascara.id_lote,"999999") + STRING(lotes_cascara.anio,"9999").
    END.
  
    IF piTipoMov = 33 THEN DO:
      viSucOri = viSucOri * 10.
    END.
    
    viSucOri = piSucUbi. 
    viSucDes = piSucDes.
    /*
    IF pcSigno = "-" AND piTipoMov <> 34 THEN DO: /*34 = reproceso no intercambiar las sucursales*/
      viSucOri = piSucDes.
      viSucDes = piSucUbi.
    END.
    */

    RUN y_gstkmovdep_cascara.p (piEmpresa, 
                                piSucursal,
                                11,
                                piNroMov,
                                viSucOri,
                                viSucDes,
                                1,
                                piCantidad,
                                piTipoMov,
                                TODAY).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArrayLotes dTables  _DB-REQUIRED
FUNCTION getArrayLotes RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcArray AS CHARACTER  NO-UNDO.
  DEFINE BUFFER buRO FOR rowObject.

  FOR EACH buRO NO-LOCK.
    vcArray = vcArray + STRING(buRO.nromov) + CHR(14) + STRING(buRO.id_lote) + "/" + STRING(buRO.anio) + CHR(14) + buRO.Sucursal + ",".
  END.
  vcArray = SUBSTR(vcArray, 1, LENGTH(vcArray) - 1).
  RETURN vcArray.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArrayUbicacion dTables  _DB-REQUIRED
FUNCTION getArrayUbicacion RETURNS CHARACTER
  (INPUT piNroMov AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcArray AS CHARACTER  NO-UNDO.
  DEFINE BUFFER buRO FOR rowObject.
  
  
  FOR EACH lotes_ubicacion WHERE lotes_ubicacion.nromov = piNroMov
                           NO-LOCK.
    FIND FIRST sucursales WHERE lotes_ubicacion.id_sucursal_ubicacion = sucursales.id_sucursal NO-LOCK NO-ERROR.
    vcArray = vcArray + sucursal.nombre + "," + STRING(lotes_ubicacion.cantidad) + CHR(14).
  END.
  vcArray = SUBSTRING(vcArray, 1, LENGTH(vcArray) - 1).
  RETURN vcArray.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadBolsasLote dTables  _DB-REQUIRED
FUNCTION getCantidadBolsasLote RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viCount AS INTEGER INITIAL 0   NO-UNDO.
  
  FOR EACH r_produccion_cascara_lote WHERE r_produccion_cascara_lote.id_empresa_lote    = rowObject.id_empresa
                                       AND r_produccion_cascara_lote.id_sucursal_lote   = rowObject.id_sucursal
                                       AND r_produccion_cascara_lote.id_tipotambor_lote = rowObject.id_tipotambor
                                       AND r_produccion_cascara_lote.nromov_lote        = rowObject.nromov
                                     NO-LOCK.
    viCount = viCount + r_produccion_cascara_lote.cantidad.
  END.
  

  RETURN viCount - getCantidadReproceso() .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadReproceso dTables  _DB-REQUIRED
FUNCTION getCantidadReproceso RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
  FOR EACH stock_historico_tambores WHERE stock_historico_tambores.id_empresa    = rowObject.id_empresa
                                      AND stock_historico_tambores.id_sucursal   = rowObject.id_sucursal
                                      AND stock_historico_tambores.id_tipotambor = rowObject.id_tipotambor
                                      AND stock_historico_tambores.nromov        = rowObject.nromov
                                      AND stock_historico_tambores.id_tipo_movi  = 34
                                      AND stock_historico_tambores.signo         = "-"
                                    NO-LOCK.
    viCant = viCant + tambor_hasta.
  END.

  RETURN viCant.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadRestante dTables  _DB-REQUIRED
FUNCTION getCantidadRestante RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 3682 - getCantidadBolsasLote().   /* Function return value. */

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
                             AND lotes_ubicacion.id_tipotambor         = 11
                           NO-LOCK.
    viCant = viCant + lotes_ubicacion.cantidad.
  END.

  RETURN viCant.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlCalidadAprobado dTables  _DB-REQUIRED
FUNCTION getControlCalidadAprobado RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*cofificar aqui la funcion que evalua la aprobacion del control de calidad*/

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIdVapor dTables  _DB-REQUIRED
FUNCTION getIdVapor RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND orden_entrega WHERE orden_entrega.id_orden_entrega = rowObject.id_orden_entrega NO-LOCK NO-ERROR.
  IF AVAILABLE orden_entrega THEN DO:
    RETURN orden_entrega.id_vapor.
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteDespachado dTables  _DB-REQUIRED
FUNCTION getLoteDespachado RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vlDespachado AS LOGICAL INITIAL FALSE   NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  FOR EACH lotes_ubicacion WHERE lotes_ubicacion.id_empresa    = rowObject.id_empresa
                             AND lotes_ubicacion.id_sucursal   = rowObject.id_sucursal
                             AND lotes_ubicacion.id_tipotambor = rowObject.id_tipotambor
                             AND lotes_ubicacion.nromov        = rowObject.nromov
                           NO-LOCK.
    IF lotes_ubicacion.id_sucursal <> 95 AND lotes_ubicacion.id_sucursal <> 96 AND lotes_ubicacion.id_sucursal <> 92 THEN DO:
      vlDespachado = TRUE.
      LEAVE.
    END.
  END.


  RETURN vlDespachado.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLotesSucursal dTables  _DB-REQUIRED
FUNCTION getLotesSucursal RETURNS CHARACTER
  ( INPUT piSucursal AS INTEGER ) :

  DEFINE VARIABLE vcLista AS CHARACTER  NO-UNDO.
  
  FOR EACH lotes_ubicacion  WHERE lotes_ubicacion.id_sucursal_ubicacion = piSucursal
                              AND lotes_ubicacion.id_tipotambor          = 11
                            NO-LOCK.
    vcLista = vcLista + STRING(lotes_ubicacion.id_lote) + "," + STRING(lotes_ubicacion.cantidad) + CHR(14).
  END.

  RETURN vcLista.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroLote dTables  _DB-REQUIRED
FUNCTION getNextNroLote RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iSeq AS INTEGER INITIAL 1   NO-UNDO.

  FOR EACH lotes_cascara BY lotes_cascara.id_lote DESC.
    iSeq = lotes_cascara.id_lote + 1.
    LEAVE.
  END.


  RETURN iSeq.   /* Function return value. */

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

  FOR FIRST productos_terminados 
      WHERE productos_terminados.id_articulo = rowObject.id_articulo
      NO-LOCK.
    cRet = productos_terminados.descripcion.
  END.

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
  FIND FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_sucursal NO-LOCK NO-ERROR.
  IF AVAILABLE sucursales THEN 
    RETURN sucursales.abreviatura.
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
  DEFINE VARIABLE vcVapor AS CHARACTER INITIAL "NO VESSELL INFO"  NO-UNDO.

  FIND orden_entrega OF rowObject NO-LOCK NO-ERROR.
  IF AVAILABLE orden_entrega THEN DO:
    FIND vapores OF orden_entrega NO-LOCK NO-ERROR.
    IF AVAILABLE vapores THEN
      vcVapor = vapores.descripcion.
  END.

  RETURN vcVapor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

