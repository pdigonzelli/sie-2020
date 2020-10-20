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
&Scoped-define INTERNAL-TABLES tambores_industria

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  Anio anio_of citral c_fecha c_fecha_rep c_hora c_hora_rep c_usuario~
 c_usuario_rep estado Fecha Fecha_cierre galones_tambor id_articulo~
 id_calidad id_contrato_of id_empresa id_empresa_destino~
 id_empresa_ubicacion id_envase id_envio_of id_etiqueta~
 id_locacion_ubicacion id_lote id_lote_deposito id_lote_nuevo~
 id_orden_entrega id_posicion_ubicacion id_proveedor id_release_delivery~
 id_sucursal id_sucursal_destino id_sucursal_remito id_sucursal_ubicacion~
 id_tambor id_tipocontrato_of id_tipotambor id_tipotambor_destino~
 id_tipo_movsto indice_tambor item_factura item_oe item_of~
 item_release_delivery kilos_agregados kilos_tambor kilos_tambor_old Nro~
 nromov nromov_destino nro_remito tara Brix
&Scoped-define DATA-FIELDS-IN-tambores_industria Anio anio_of citral ~
c_fecha c_fecha_rep c_hora c_hora_rep c_usuario c_usuario_rep estado Fecha ~
Fecha_cierre galones_tambor id_articulo id_calidad id_contrato_of ~
id_empresa id_empresa_destino id_empresa_ubicacion id_envase id_envio_of ~
id_etiqueta id_locacion_ubicacion id_lote id_lote_deposito id_lote_nuevo ~
id_orden_entrega id_posicion_ubicacion id_proveedor id_release_delivery ~
id_sucursal id_sucursal_destino id_sucursal_remito id_sucursal_ubicacion ~
id_tambor id_tipocontrato_of id_tipotambor id_tipotambor_destino ~
id_tipo_movsto indice_tambor item_factura item_oe item_of ~
item_release_delivery kilos_agregados kilos_tambor kilos_tambor_old Nro ~
nromov nromov_destino nro_remito tara 
&Scoped-Define MANDATORY-FIELDS  id_calidad id_empresa_destino id_lote id_sucursal id_sucursal_destino~
 id_sucursal_ubicacion id_tambor
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dcompolotejugo.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH tambores_industria NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH tambores_industria NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main tambores_industria
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main tambores_industria


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArticulosMateriaPrima dTables  _DB-REQUIRED
FUNCTION getArticulosMateriaPrima RETURNS CHARACTER
  (INPUT piArticuloLote AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrix dTables  _DB-REQUIRED
FUNCTION getBrix RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowsNumber dTables  _DB-REQUIRED
FUNCTION getRowsNumber RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedRowIds dTables  _DB-REQUIRED
FUNCTION getSelectedRowIds RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      tambores_industria SCROLLING.
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
     _TblList          = "general.tambores_industria"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.tambores_industria.Anio
"Anio" "Anio" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[2]   > general.tambores_industria.anio_of
"anio_of" "anio_of" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[3]   > general.tambores_industria.citral
"citral" "citral" ? ? "decimal" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[4]   > general.tambores_industria.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[5]   > general.tambores_industria.c_fecha_rep
"c_fecha_rep" "c_fecha_rep" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[6]   > general.tambores_industria.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[7]   > general.tambores_industria.c_hora_rep
"c_hora_rep" "c_hora_rep" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[8]   > general.tambores_industria.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[9]   > general.tambores_industria.c_usuario_rep
"c_usuario_rep" "c_usuario_rep" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[10]   > general.tambores_industria.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? no ? no 7.4 yes
     _FldNameList[11]   > general.tambores_industria.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[12]   > general.tambores_industria.Fecha_cierre
"Fecha_cierre" "Fecha_cierre" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[13]   > general.tambores_industria.galones_tambor
"galones_tambor" "galones_tambor" ? ? "integer" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[14]   > general.tambores_industria.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[15]   > general.tambores_industria.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? yes 7 yes
     _FldNameList[16]   > general.tambores_industria.id_contrato_of
"id_contrato_of" "id_contrato_of" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[17]   > general.tambores_industria.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[18]   > general.tambores_industria.id_empresa_destino
"id_empresa_destino" "id_empresa_destino" ? ? "integer" ? ? ? ? ? ? no ? yes 8.2 yes
     _FldNameList[19]   > general.tambores_industria.id_empresa_ubicacion
"id_empresa_ubicacion" "id_empresa_ubicacion" ? ? "integer" ? ? ? ? ? ? no ? no 8.6 yes
     _FldNameList[20]   > general.tambores_industria.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[21]   > general.tambores_industria.id_envio_of
"id_envio_of" "id_envio_of" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[22]   > general.tambores_industria.id_etiqueta
"id_etiqueta" "id_etiqueta" ? ? "integer" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[23]   > general.tambores_industria.id_locacion_ubicacion
"id_locacion_ubicacion" "id_locacion_ubicacion" ? ? "integer" ? ? ? ? ? ? no ? no 7.4 yes
     _FldNameList[24]   > general.tambores_industria.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? no ? yes 10.2 yes
     _FldNameList[25]   > general.tambores_industria.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? no ? no 20 yes
     _FldNameList[26]   > general.tambores_industria.id_lote_nuevo
"id_lote_nuevo" "id_lote_nuevo" ? ? "character" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[27]   > general.tambores_industria.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[28]   > general.tambores_industria.id_posicion_ubicacion
"id_posicion_ubicacion" "id_posicion_ubicacion" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[29]   > general.tambores_industria.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? no ? no 9.8 yes
     _FldNameList[30]   > general.tambores_industria.id_release_delivery
"id_release_delivery" "id_release_delivery" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[31]   > general.tambores_industria.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? no ? yes 6.6 yes
     _FldNameList[32]   > general.tambores_industria.id_sucursal_destino
"id_sucursal_destino" "id_sucursal_destino" ? ? "integer" ? ? ? ? ? ? no ? yes 8.2 yes
     _FldNameList[33]   > general.tambores_industria.id_sucursal_remito
"id_sucursal_remito" "id_sucursal_remito" ? ? "integer" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[34]   > general.tambores_industria.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? no ? yes 8.2 yes
     _FldNameList[35]   > general.tambores_industria.id_tambor
"id_tambor" "id_tambor" ? ? "integer" ? ? ? ? ? ? no ? yes 12 yes
     _FldNameList[36]   > general.tambores_industria.id_tipocontrato_of
"id_tipocontrato_of" "id_tipocontrato_of" ? ? "integer" ? ? ? ? ? ? no ? no 7.4 yes
     _FldNameList[37]   > general.tambores_industria.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[38]   > general.tambores_industria.id_tipotambor_destino
"id_tipotambor_destino" "id_tipotambor_destino" ? ? "integer" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[39]   > general.tambores_industria.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[40]   > general.tambores_industria.indice_tambor
"indice_tambor" "indice_tambor" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[41]   > general.tambores_industria.item_factura
"item_factura" "item_factura" ? ? "integer" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[42]   > general.tambores_industria.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? no ? no 8.6 yes
     _FldNameList[43]   > general.tambores_industria.item_of
"item_of" "item_of" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[44]   > general.tambores_industria.item_release_delivery
"item_release_delivery" "item_release_delivery" ? ? "integer" ? ? ? ? ? ? no ? no 20.2 yes
     _FldNameList[45]   > general.tambores_industria.kilos_agregados
"kilos_agregados" "kilos_agregados" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[46]   > general.tambores_industria.kilos_tambor
"kilos_tambor" "kilos_tambor" ? ? "decimal" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[47]   > general.tambores_industria.kilos_tambor_old
"kilos_tambor_old" "kilos_tambor_old" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[48]   > general.tambores_industria.Nro
"Nro" "Nro" ? ? "integer" ? ? ? ? ? ? no ? no 9.4 yes
     _FldNameList[49]   > general.tambores_industria.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[50]   > general.tambores_industria.nromov_destino
"nromov_destino" "nromov_destino" ? ? "integer" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[51]   > general.tambores_industria.nro_remito
"nro_remito" "nro_remito" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[52]   > general.tambores_industria.tara
"tara" "tara" ? ? "decimal" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[53]   > "_<CALC>"
"getBrix()" "Brix" "Brix" ">>>,99" "Decimal" ? ? ? ? ? ? no ? no 6.6 no
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
         rowObject.Brix = (getBrix())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArticulosMateriaPrima dTables  _DB-REQUIRED
FUNCTION getArticulosMateriaPrima RETURNS CHARACTER
  (INPUT piArticuloLote AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR EACH origenes_materia_prima WHERE id_articulo_lote = piArticuloLote 
                                  NO-LOCK.
    cRet = cRet + " tambores_industria.id_articulo = " + STRING(id_articulo_mp) + " OR ".
  END.
  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 4).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrix dTables  _DB-REQUIRED
FUNCTION getBrix RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
   FIND CURRENT rowObject NO-ERROR.

   CASE rowObject.id_tipotambor:
     WHEN 1 THEN DO:
       FIND FIRST produccion_jugo WHERE produccion_jugo.id_empresa    = rowObject.id_empresa
                                    AND produccion_jugo.id_sucursal   = rowObject.id_sucursal
                                    AND produccion_jugo.id_tipotambor = rowObject.id_tipotambor
                                    AND produccion_jugo.nromov        = rowObject.nromov
                         NO-LOCK NO-ERROR.
       IF AVAILABLE produccion_jugo THEN
         dRet = produccion_jugo.bx_20_20.
       ELSE 
         dRet = 0.
     END.
   END CASE.

   RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowsNumber dTables  _DB-REQUIRED
FUNCTION getRowsNumber RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buRowObject FOR rowObject.
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  FOR EACH buRowObject NO-LOCK.
    i = i + 1.
  END.

  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedRowIds dTables  _DB-REQUIRED
FUNCTION getSelectedRowIds RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo    AS INTEGER    NO-UNDO.


  DO iRow = 1 TO NUM-ENTRIES(pcRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow', INTEGER(ENTRY(iRow,pcRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,id_tambor').
    ASSIGN iEmp = INTEGER(ENTRY(2, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(3, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(4, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(5, cFields, CHR(1)))
           iTbo = INTEGER(ENTRY(6, cFields, CHR(1))).

    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".
    FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = iEmp
                                    AND tambores_industria.id_sucursal   = iSuc
                                    AND tambores_industria.id_tipotambor = iTip
                                    AND tambores_industria.nromov        = iNro
                                    AND tambores_industria.id_tambor     = iTbo
                                  NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      cRet = cRet + STRING(ROWID(tambores_industria)) + CHR(10).
    END.
  END.
  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1). 
  RETURN cRet.  
   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

