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
&Scoped-define INTERNAL-TABLES lotes_aceite

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  activo anio anio_of citral control_calidad c_fecha c_hora c_usuario~
 estado_lote fecha Fecha_comienzo Fecha_finalizacion id_articulo~
 id_contrato_of id_empresa id_envase id_envio_of id_lote id_lote_nuevo~
 id_orden id_orden_entrega id_sucursal id_tipocontrato_of id_tipotambor~
 item_oe item_of nromov nro_partida observaciones Peso_neto~
 quimico_control_calidad tanque origen_water codigo_lote
&Scoped-define ENABLED-FIELDS-IN-lotes_aceite activo anio anio_of citral ~
control_calidad c_fecha c_hora c_usuario estado_lote fecha Fecha_comienzo ~
Fecha_finalizacion id_articulo id_contrato_of id_empresa id_envase ~
id_envio_of id_lote id_lote_nuevo id_orden id_orden_entrega id_sucursal ~
id_tipocontrato_of id_tipotambor item_oe item_of nromov nro_partida ~
observaciones Peso_neto quimico_control_calidad tanque origen_water ~
codigo_lote 
&Scoped-Define DATA-FIELDS  activo Articulo anio anio_of citral control_calidad c_fecha c_hora~
 c_usuario estado_lote fecha Fecha_comienzo Fecha_finalizacion id_articulo~
 id_contrato_of id_empresa id_envase id_envio_of id_lote id_lote_nuevo~
 id_orden id_orden_entrega id_sucursal id_tipocontrato_of id_tipotambor~
 item_oe item_of nromov nro_partida observaciones Peso_neto~
 quimico_control_calidad tanque origen_water Envase codigo_lote Estado~
 Quimico SucLote Tambores Kilos
&Scoped-define DATA-FIELDS-IN-lotes_aceite activo anio anio_of citral ~
control_calidad c_fecha c_hora c_usuario estado_lote fecha Fecha_comienzo ~
Fecha_finalizacion id_articulo id_contrato_of id_empresa id_envase ~
id_envio_of id_lote id_lote_nuevo id_orden id_orden_entrega id_sucursal ~
id_tipocontrato_of id_tipotambor item_oe item_of nromov nro_partida ~
observaciones Peso_neto quimico_control_calidad tanque origen_water ~
codigo_lote 
&Scoped-Define MANDATORY-FIELDS  id_lote id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dlotesaceite.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH lotes_aceite NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH lotes_aceite NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main lotes_aceite
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main lotes_aceite


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadesOrigen dTables  _DB-REQUIRED
FUNCTION getCantidadesOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosGraficoProduccion dTables  _DB-REQUIRED
FUNCTION getDatosGraficoProduccion RETURNS CHARACTER
  (piArticulo AS INTEGER)  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEstado dTables  _DB-REQUIRED
FUNCTION getEstado RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos dTables  _DB-REQUIRED
FUNCTION getKilos RETURNS DECIMAL
  (plAction AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosProducto dTables  _DB-REQUIRED
FUNCTION getKilosProducto RETURNS DECIMAL
  (piArt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroOrden dTables  _DB-REQUIRED
FUNCTION getNextNroOrden RETURNS INTEGER
  (INPUT piLote AS INTEGER, 
   INPUT piAnio AS INTEGER, 
   INPUT piSuc  AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQtyTambores dTables  _DB-REQUIRED
FUNCTION getQtyTambores RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuimicoId dTables  _DB-REQUIRED
FUNCTION getQuimicoId RETURNS INTEGER
  (INPUT pcUser AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuimicoName dTables  _DB-REQUIRED
FUNCTION getQuimicoName RETURNS CHARACTER
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursalLote dTables  _DB-REQUIRED
FUNCTION getSucursalLote RETURNS CHARACTER
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
      lotes_aceite SCROLLING.
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
     _TblList          = "general.lotes_aceite"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.lotes_aceite.activo
"activo" "activo" ? ? "logical" ? ? ? ? ? ? yes ? no 6 yes
     _FldNameList[2]   > "_<CALC>"
"getArticulo()" "Articulo" "Articulo" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[3]   > general.lotes_aceite.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[4]   > general.lotes_aceite.anio_of
"anio_of" "anio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[5]   > general.lotes_aceite.citral
"citral" "citral" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[6]   > general.lotes_aceite.control_calidad
"control_calidad" "control_calidad" ? ? "logical" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[7]   > general.lotes_aceite.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[8]   > general.lotes_aceite.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[9]   > general.lotes_aceite.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[10]   > general.lotes_aceite.estado_lote
"estado_lote" "estado_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[11]   > general.lotes_aceite.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[12]   > general.lotes_aceite.Fecha_comienzo
"Fecha_comienzo" "Fecha_comienzo" ? ? "date" ? ? ? ? ? ? yes ? no 16.2 yes
     _FldNameList[13]   > general.lotes_aceite.Fecha_finalizacion
"Fecha_finalizacion" "Fecha_finalizacion" ? ? "date" ? ? ? ? ? ? yes ? no 17.6 yes
     _FldNameList[14]   > general.lotes_aceite.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[15]   > general.lotes_aceite.id_contrato_of
"id_contrato_of" "id_contrato_of" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[16]   > general.lotes_aceite.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[17]   > general.lotes_aceite.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[18]   > general.lotes_aceite.id_envio_of
"id_envio_of" "id_envio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[19]   > general.lotes_aceite.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[20]   > general.lotes_aceite.id_lote_nuevo
"id_lote_nuevo" "id_lote_nuevo" ? ? "character" ? ? ? ? ? ? yes ? no 11.2 yes
     _FldNameList[21]   > general.lotes_aceite.id_orden
"id_orden" "id_orden" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes
     _FldNameList[22]   > general.lotes_aceite.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[23]   > general.lotes_aceite.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[24]   > general.lotes_aceite.id_tipocontrato_of
"id_tipocontrato_of" "id_tipocontrato_of" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[25]   > general.lotes_aceite.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[26]   > general.lotes_aceite.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[27]   > general.lotes_aceite.item_of
"item_of" "item_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[28]   > general.lotes_aceite.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[29]   > general.lotes_aceite.nro_partida
"nro_partida" "nro_partida" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[30]   > general.lotes_aceite.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
     _FldNameList[31]   > general.lotes_aceite.Peso_neto
"Peso_neto" "Peso_neto" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[32]   > general.lotes_aceite.quimico_control_calidad
"quimico_control_calidad" "quimico_control_calidad" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[33]   > general.lotes_aceite.tanque
"tanque" "tanque" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[34]   > general.lotes_aceite.origen_water
"origen_water" "origen_water" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[35]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[36]   > general.lotes_aceite.codigo_lote
"codigo_lote" "codigo_lote" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[37]   > "_<CALC>"
"getEstado()" "Estado" "Estado" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[38]   > "_<CALC>"
"getQuimicoName()" "Quimico" "Quimico" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[39]   > "_<CALC>"
"getSucursalLote()" "SucLote" "SucLote" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[40]   > "_<CALC>"
"getQtyTambores()" "Tambores" "Tambores" ">>>9" "Integer" ? ? ? ? ? ? no ? no 9.4 no
     _FldNameList[41]   > "_<CALC>"
"getKilos(TRUE)" "Kilos" "Kilos" ">>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15 no
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
DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
DEFINE VARIABLE iLote   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAnio   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
DEFINE VARIABLE lExiste AS LOGICAL    NO-UNDO.



FIND LAST RowObjUpd NO-ERROR.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
  hSource = DYNAMIC-FUNCTION('getDataSource').
  iEmp    = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_empresa').
  iSuc    = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal').
  iAnio   = DYNAMIC-FUNCTION('columnValue' IN hSource, 'anio').
  iLote   = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_lote').

  /*
  lExiste = DYNAMIC-FUNCTION('getLoteExistente' IN hLib, iSuc,
                                                         6,
                                                         iLote, 
                                                         iAnio, 
                                                         rowObjUpd.id_articulo).
  */                                                       
  IF lExiste THEN DO:
    MESSAGE "Imposible crear un lote con el mismo nro y articulo en la misma sucursal" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN "ADM-ERROR".
  END.


  ASSIGN rowObjUpd.id_empresa      = iEmp
         rowObjUpd.id_sucursal     = iSuc
         rowObjUpd.id_tipotambor   = 6
         rowObjUpd.id_lote         = iLote
         rowObjUpd.nromov          = NEXT-VALUE(nromov)
         rowObjUpd.anio            = iAnio
         rowObjUpd.estado_lote     = 1
         rowObjUpd.nro_partida     = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nro_partida')
         rowObjUpd.id_orden        = getNextNroOrden(iLote, iAnio, iSuc)
         rowObjUpd.activo          = TRUE
         rowObjUpd.c_usuario       = USERID("userdb")
         rowObjUpd.c_fecha         = TODAY  
         rowObjUpd.c_hora          = STRING(TIME,"HH:MM:SS").
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_empresa,id_sucursal,id_tipotambor,id_lote,nromov,anio,estado_lote,nro_partida,id_orden,activo".
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".  
END.



FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".
  ASSIGN rowObjUpd.c_usuario  = USERID("userdb")
         rowObjUpd.c_fecha    = TODAY  
         rowObjUpd.c_hora     = STRING(TIME,"HH:MM:SS").
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".

  RUN setArticuloLote IN hLib (rowObjUpd.id_empresa,
                               rowObjUpd.id_sucursal, 
                               rowObjUpd.id_tipotambor, 
                               rowObjUpd.nromov, 
                               rowObjUpd.id_articulo).

  RUN setEnvaseLote IN hLib (rowObjUpd.id_empresa,
                             rowObjUpd.id_sucursal, 
                             rowObjUpd.id_tipotambor, 
                             rowObjUpd.nromov, 
                             rowObjUpd.id_envase).

  RUN setKilosLote IN hLib (rowObjUpd.id_empresa,
                            rowObjUpd.id_sucursal, 
                            rowObjUpd.id_tipotambor, 
                            rowObjUpd.nromov, 
                            rowObjUpd.peso_neto).

END.


FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D".
  
  /*agrego entrada en el spool de mails a depositos*/
  RUN addMailingSpoolEntry IN hLib (rowObjUpd.id_empresa,
                                    rowObjUpd.id_sucursal,
                                    rowObjUpd.id_tipotambor,
                                    rowObjUpd.nromov, 
                                    "delete").

  /*libero origenes*/
  RUN removeOrigenes IN hLib (rowObjUpd.id_empresa, 
                              rowObjUpd.id_sucursal, 
                              rowObjUpd.id_tipotambor, 
                              rowObjUpd.nromov).
  /*elimino tambores*/
  RUN deleteDrumsFromBatch IN hLib (rowObjUpd.id_empresa, 
                                    rowObjUpd.id_sucursal, 
                                    rowObjUpd.id_tipotambor, 
                                    rowObjUpd.nromov, 
                                    TRUE).
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
         rowObject.Envase = (getEnvase())
         rowObject.Estado = (getEstado())
         rowObject.Kilos = (getKilos(TRUE))
         rowObject.Quimico = (getQuimicoName())
         rowObject.SucLote = (getSucursalLote())
         rowObject.Tambores = (getQtyTambores())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTamboresProduccion dTables  _DB-REQUIRED
PROCEDURE getTamboresProduccion :
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

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
  RUN setReprocesoTambores.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prueba dTables  _DB-REQUIRED
PROCEDURE prueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hVwr AS HANDLE     NO-UNDO.

  hVwr = getViewerHandle("vLotesAceite").



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
  DEFINE INPUT  PARAMETER plEstado AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE iQuimico  AS INTEGER    NO-UNDO.

  iQuimico  = getQuimicoId(USERID("userdb")).
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR".
    FIND CURRENT RowObject NO-ERROR.
    
    RUN setEstadoControlCalidad IN hLib (rowObject.id_empresa, 
                                         rowObject.id_sucursal, 
                                         rowObject.id_tipotambor, 
                                         rowObject.nromov, 
                                         plEstado,
                                         iQuimico, 
                                         rowObject.id_articulo).

  END.

  RUN refreshRow.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setReprocesoTambores dTables  _DB-REQUIRED
PROCEDURE setReprocesoTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iNroMov AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOrden  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDesde  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAnio   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLote   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCant   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iKilos  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTara   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFirst  AS LOGICAL    NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE hViewer AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO.


  hSource = DYNAMIC-FUNCTION('getDataSource').
  
  iEmp    = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_empresa').
  iSuc    = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal').
  iAnio   = DYNAMIC-FUNCTION('columnValue' IN hSource, 'anio').
  iLote   = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_lote').
  
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
    FIND LAST lotes_aceite WHERE lotes_aceite.id_lote     =  iLote
                             AND lotes_aceite.anio        =  iAnio
                             AND lotes_aceite.id_sucursal =  iSuc
                             AND lotes_aceite.nromov      <> rowObjUpd.nromov
                           NO-ERROR.
    IF AVAILABLE lotes_aceite THEN DO:
      lotes_aceite.activo = FALSE.
      /*actualizo tambores de la etapa anterior*/
      RUN setLoteDestinoToTambores IN hLib (lotes_aceite.id_empresa, 
                                            lotes_aceite.id_sucursal, 
                                            lotes_aceite.id_tipotambor, 
                                            lotes_aceite.nromov, 
                                            rowObjUpd.id_empresa, 
                                            rowObjUpd.id_sucursal, 
                                            rowObjUpd.id_tipotambor, 
                                            rowObjUpd.nromov, 
                                            FALSE).  
      IF RETURN-VALUE <> "" THEN DO:
        MESSAGE "Error en la actualizacion de tambores setLoteDestinoToTambores()" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN "ADM-ERROR".
      END.

    END.
  END. /*transaction*/

  /*
  /*crear los tambores*/
  hViewer = getViewerHandle("vLotesAceite").
  IF VALID-HANDLE(hViewer) THEN DO:
    cRet   = DYNAMIC-FUNCTION('getDataForDrumsCreation' IN hViewer).
    iCant  = INTEGER(ENTRY(1, cRet)).
    iDesde = INTEGER(ENTRY(2, cRet)).
    iHasta = INTEGER(ENTRY(3, cRet)).
    iKilos = INTEGER(ENTRY(4, cRet)).
    iTara  = INTEGER(ENTRY(5, cRet)).
  END.
  ELSE
    MESSAGE "Imposible recuperar datos del viewer" VIEW-AS ALERT-BOX INFO BUTTONS OK.

  RUN createDrumsInBatch IN hLib (rowObjUpd.id_empresa,
                                  rowObjUpd.id_sucursal, 
                                  rowObjUpd.id_tipotambor, 
                                  rowObjUpd.nromov, 
                                  iCant, 
                                  iDesde, 
                                  iHasta, 
                                  iKilos, 
                                  iTara).
  IF RETURN-VALUE <> "" THEN DO:
    MESSAGE "Error en la creacion de tambores createDrumsInBatch()" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN "ADM-ERROR".
  END.
  
  RUN cancelRecord IN hViewer.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tieDrumsToBatch dTables  _DB-REQUIRED
PROCEDURE tieDrumsToBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcRows       AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piEmpresa    AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucursal   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTambor AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroMov     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plAction     AS LOGICAL    NO-UNDO. /*true asocia, false desvincula*/

  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo    AS INTEGER    NO-UNDO.  
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRowId  AS CHARACTER  NO-UNDO.

     
  DO iRow = 1 TO NUM-ENTRIES(pcRows, CHR(10)) ON ERROR UNDO, LEAVE:
    cRowId = ENTRY(iRow, pcRows, CHR(10)).
    FIND FIRST tambores_industria WHERE ROWID(tambores_industria) = TO-ROWID(cRowId) 
                                  NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      ASSIGN iEmp = tambores_industria.id_empresa 
             iSuc = tambores_industria.id_sucursal
             iTip = tambores_industria.id_tipotambor
             iNro = tambores_industria.nromov
             iTbo = tambores_industria.id_tambor.
      IF plAction THEN
        RUN setLoteDestinoToTambor IN hLib (iEmp, 
                                            iSuc, 
                                            iTip,
                                            iNro,
                                            iTbo,       
                                            piEmpresa,
                                            piSucursal, 
                                            piTipoTambor, 
                                            piNroMov,
                                            TRUE).
      ELSE
        RUN setLoteDestinoToTambor IN hLib (iEmp, 
                                            iSuc, 
                                            iTip,
                                            iNro,
                                            iTbo,       
                                            0,
                                            0, 
                                            0, 
                                            0,
                                            TRUE).
      IF RETURN-VALUE <> "" THEN DO:
        MESSAGE "Error en la asociacion de tambores setLoteDestinoToTambor()" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN "ADM-ERROR".
      END.
    END.
  END.



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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadesOrigen dTables  _DB-REQUIRED
FUNCTION getCantidadesOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
  FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino    = rowObject.id_empresa
                                AND tambores_industria.id_sucursal_destino   = rowObject.id_sucursal
                                AND tambores_industria.id_tipotambor_destino = rowObject.id_tipotambor
                                AND tambores_industria.nromov_destino        = rowObject.nromov
                              BREAK BY tambores_industria.id_tipotambor.
    i = i + 1.
    IF LAST-OF(tambores_industria.id_tipotambor) THEN DO:
      cRet = cRet + STRING(tambores_industria.id_tipotambor) + "," + STRING(i) + CHR(10).
      i = 0.
    END.
  END.
  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).
  
  RETURN cRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosGraficoProduccion dTables  _DB-REQUIRED
FUNCTION getDatosGraficoProduccion RETURNS CHARACTER
  (piArticulo AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buRow FOR rowObject.


  FOR EACH buRow
      WHERE (IF piArticulo <> 0 THEN buRow.id_articulo = piArticulo ELSE TRUE)
      BREAK BY buRow.fecha.
    k = k + buRow.kilos.
    IF LAST-OF(buRow.fecha) THEN DO:
      cRet = cRet + STRING(buRow.fecha) + "," + STRING(k) + CHR(10).
      k    = 0.
    END.
  END.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEstado dTables  _DB-REQUIRED
FUNCTION getEstado RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  cRet = "".
  IF rowObject.estado_lote = 5 THEN 
    cRet = "APROBADO".

  IF rowObject.estado_lote = 4 THEN
    cRet = "DESAPROBADO".

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos dTables  _DB-REQUIRED
FUNCTION getKilos RETURNS DECIMAL
  (plAction AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  IF NOT plAction THEN RETURN 0.00.

  fKil = DYNAMIC-FUNCTION('getKilosLote' IN hLib, rowObject.id_empresa,
                                                  rowObject.id_sucursal,
                                                  rowObject.id_tipotambor,
                                                  rowObject.nromov).

  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosProducto dTables  _DB-REQUIRED
FUNCTION getKilosProducto RETURNS DECIMAL
  (piArt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buRow FOR rowObject.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.


  FOR EACH buRow
      WHERE (IF piArt = 0 THEN (buRow.id_articulo <> 51 AND buRow.id_articulo <> 571 AND buRow.id_articulo <> 581) ELSE buRow.id_articulo = piArt)
      NO-LOCK.
    fKil = fKil + buRow.kilos.
    
  END.

  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroOrden dTables  _DB-REQUIRED
FUNCTION getNextNroOrden RETURNS INTEGER
  (INPUT piLote AS INTEGER, 
   INPUT piAnio AS INTEGER, 
   INPUT piSuc  AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER  INITIAL 1  NO-UNDO.


  FIND LAST lotes_aceite WHERE lotes_aceite.id_lote     = piLote
                           AND lotes_aceite.anio        = piAnio
                           AND lotes_aceite.id_sucursal = piSuc
                         NO-ERROR.
  IF AVAILABLE lotes_aceite THEN 
    iRet = lotes_aceite.id_orden + 1. 

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQtyTambores dTables  _DB-REQUIRED
FUNCTION getQtyTambores RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  

  FIND CURRENT rowObject NO-ERROR.
  FOR EACH tambores_industria WHERE tambores_industria.nromov = rowObject.nromov
                              NO-LOCK.
    i = i + 1.    
  END.



  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuimicoId dTables  _DB-REQUIRED
FUNCTION getQuimicoId RETURNS INTEGER
  (INPUT pcUser AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  FIND FIRST quimicos WHERE "y_" + quimicos.usuario = pcUser NO-LOCK NO-ERROR.
  IF AVAILABLE quimicos THEN DO:
    iRet = quimicos.id_quimico.
  END.
  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuimicoName dTables  _DB-REQUIRED
FUNCTION getQuimicoName RETURNS CHARACTER
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.

  RETURN rowObject.quimico_control_calidad.
  /*
  FIND FIRST quimicos WHERE quimicos.id_quimico = INTEGER(rowObject.quimico_control_calidad)
                      NO-LOCK NO-ERROR.
  IF AVAILABLE quimicos THEN
    RETURN quimicos.nombre.
  ELSE 
    RETURN "".
    
  */  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursalLote dTables  _DB-REQUIRED
FUNCTION getSucursalLote RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_sucursal
                    NO-LOCK NO-ERROR.
  IF AVAILABLE sucursales THEN
    RETURN sucursales.abreviatura.
  ELSE 
    RETURN "".

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

