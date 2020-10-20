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
&Scoped-define INTERNAL-TABLES gastos_items_release_delivery gastos_venta

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_gasto id_release_delivery importe item_release_delivery id_grupo_gasto~
 id_orden importe_por_tonelada importe_total importe_unidad_venta~
 tipo_calculo
&Scoped-define ENABLED-FIELDS-IN-gastos_items_release_delivery id_gasto ~
id_release_delivery importe item_release_delivery id_grupo_gasto id_orden ~
importe_por_tonelada importe_total importe_unidad_venta tipo_calculo 
&Scoped-Define DATA-FIELDS  id_gasto id_release_delivery importe item_release_delivery descripcion~
 id_grupo_gasto id_orden importe_por_tonelada importe_total~
 importe_unidad_venta tipo_calculo
&Scoped-define DATA-FIELDS-IN-gastos_items_release_delivery id_gasto ~
id_release_delivery importe item_release_delivery id_grupo_gasto id_orden ~
importe_por_tonelada importe_total importe_unidad_venta tipo_calculo 
&Scoped-define DATA-FIELDS-IN-gastos_venta descripcion 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dgastositemsreleasedelivery.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH gastos_items_release_delivery NO-LOCK, ~
      EACH gastos_venta OF gastos_items_release_delivery NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH gastos_items_release_delivery NO-LOCK, ~
      EACH gastos_venta OF gastos_items_release_delivery NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main gastos_items_release_delivery ~
gastos_venta
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main gastos_items_release_delivery
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main gastos_venta


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calculo-clausulas dTables  _DB-REQUIRED
FUNCTION calculo-clausulas RETURNS DECIMAL
  (INPUT pRelease AS INTEGER,
   INPUT pItemRelease AS INTEGER,
   INPUT pImporte AS DECIMAL,
   INPUT pClausula AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calculo-ddp dTables  _DB-REQUIRED
FUNCTION calculo-ddp RETURNS DECIMAL
  ( INPUT pContrato AS CHAR,
    INPUT pItemContrato AS INTEGER,
    INPUT pRelease AS INTEGER,
    INPUT pItemRelease AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calculo-fob dTables  _DB-REQUIRED
FUNCTION calculo-fob RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calculo-gastos dTables  _DB-REQUIRED
FUNCTION calculo-gastos RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD generacion-gastos dTables  _DB-REQUIRED
FUNCTION generacion-gastos RETURNS CHARACTER
  (INPUT pGasto AS INTEGER,
   INPUT pImporte AS DECIMAL,
   INPUT pKilosTambores AS DECIMAL,
   INPUT pCantidadTambores AS INTEGER,
   INPUT pTamboresOE AS INTEGER,
   INPUT pReleaseDelivery AS INTEGER,
   INPUT pItemReleaseDelivery AS INTEGER,
   INPUT pUnidadVenta AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      gastos_items_release_delivery, 
      gastos_venta SCROLLING.
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
     _TblList          = "general.gastos_items_release_delivery,industria.gastos_venta OF general.gastos_items_release_delivery"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.gastos_items_release_delivery.id_gasto
"id_gasto" "id_gasto" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[2]   > general.gastos_items_release_delivery.id_release_delivery
"id_release_delivery" "id_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 6.4 yes
     _FldNameList[3]   > general.gastos_items_release_delivery.importe
"importe" "importe" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[4]   > general.gastos_items_release_delivery.item_release_delivery
"item_release_delivery" "item_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 yes
     _FldNameList[5]   > general.gastos_venta.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[6]   > general.gastos_items_release_delivery.id_grupo_gasto
"id_grupo_gasto" "id_grupo_gasto" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes
     _FldNameList[7]   > general.gastos_items_release_delivery.id_orden
"id_orden" "id_orden" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes
     _FldNameList[8]   > general.gastos_items_release_delivery.importe_por_tonelada
"importe_por_tonelada" "importe_por_tonelada" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.8 yes
     _FldNameList[9]   > general.gastos_items_release_delivery.importe_total
"importe_total" "importe_total" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[10]   > general.gastos_items_release_delivery.importe_unidad_venta
"importe_unidad_venta" "importe_unidad_venta" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[11]   > general.gastos_items_release_delivery.tipo_calculo
"tipo_calculo" "tipo_calculo" ? ? "logical" ? ? ? ? ? ? yes ? no 11.2 yes
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
DEFINE VAR hSource AS HANDLE.
DEFINE VAR vReleaseDelivery AS DECIMAL.
DEFINE VAR vItemReleaseDelivery AS DECIMAL.

FIND LAST RowObjUpd NO-ERROR.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A" OR RowObjUpd.RowMod = "C".
  
  hSource       = DYNAMIC-FUNCTION('getDataSource').
  vReleaseDelivery        = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_release_delivery').
  vItemReleaseDelivery    = DYNAMIC-FUNCTION('columnValue' IN hSource, 'item_release_delivery').

  ASSIGN RowObjUpd.id_release_delivery    = vReleaseDelivery
         RowObjUpd.item_release_delivery  = vItemReleaseDelivery
      .

    RowObjUpd.changedFields = RowObjUpd.changedFields + ",".

END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-kilos-galones dTables  _DB-REQUIRED
PROCEDURE get-kilos-galones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pRelease AS INTEGER.
DEFINE INPUT PARAMETER pItemRelease AS INTEGER.
DEFINE OUTPUT PARAMETER pKilos AS DECIMAL.
DEFINE OUTPUT PARAMETER pGalones AS DECIMAL.

DEFINE BUFFER bbLotes FOR lotes_jugo.
DEFINE BUFFER bbTambores FOR tambores_industria.
DEFINE VAR vGalonesTemp AS DECIMAL.
DEFINE VAR vKilosTemp AS DECIMAL.


FOR EACH bbTambores WHERE bbTambores.id_release_delivery    = pRelease
                      AND bbTambores.ITEM_release_delivery  = pItemRelease
                        NO-LOCK
                    BREAK BY bbTambores.nromov.

    pKilos = pKilos + bbTambores.kilos_tambor.
    vKilosTemp = vKilosTemp + bbTambores.kilos_tambor.

    IF LAST-OF(bbTambores.nromov) THEN DO:
        FIND FIRST bbLotes OF bbTambores
                            NO-LOCK NO-ERROR.
        IF AVAILABLE bbLotes THEN DO:
            FIND LAST inspecciones_lote OF bbLotes NO-LOCK NO-ERROR.
            IF AVAILABLE inspecciones_lote THEN DO:
                FIND LAST brix WHERE brix.brix <= ROUND(inspecciones_lote.bx_correg,1) NO-LOCK NO-ERROR.
                IF AVAILABLE brix THEN DO:
                    vGalonesTemp    = ROUND((vKilosTemp / brix.pe) / 3.785 , 2).
                    pGalones        = pGalones + vGalonesTemp.
                END.
            END.        
        END.
        vKilosTemp = 0.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calculo-clausulas dTables  _DB-REQUIRED
FUNCTION calculo-clausulas RETURNS DECIMAL
  (INPUT pRelease AS INTEGER,
   INPUT pItemRelease AS INTEGER,
   INPUT pImporte AS DECIMAL,
   INPUT pClausula AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bbGastos FOR gastos_items_release_delivery.
  DEFINE VAR vGastos AS DECIMAL.
  DEFINE VAR vImporteClausula AS DECIMAL.
  
  FOR EACH gastos_items_release_clausulas WHERE id_clausula = pClausula
                                          NO-LOCK.
    FIND FIRST bbGastos WHERE bbGastos.id_release_delivery      = pRelease
                          AND bbGastos.ITEM_release_delivery    = pItemRelease
                          AND bbGastos.id_gasto                 = gastos_items_release_clausulas.id_gasto
                        NO-LOCK NO-ERROR.
    IF AVAILABLE bbGastos THEN DO:
        vGastos = vGastos + bbGastos.importe.        
    END.
  END.
      
  vImporteClausula = pImporte - vGastos.
  
  RETURN vImporteClausula.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calculo-ddp dTables  _DB-REQUIRED
FUNCTION calculo-ddp RETURNS DECIMAL
  ( INPUT pContrato AS CHAR,
    INPUT pItemContrato AS INTEGER,
    INPUT pRelease AS INTEGER,
    INPUT pItemRelease AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VAR vKilos AS DECIMAL.
DEFINE VAR v_precio_total AS DECIMAL.
DEFINE VAR vGalones AS DECIMAL.

DEFINE BUFFER bbItemContrato FOR items_contratos.
DEFINE BUFFER bbTambores FOR tambores_industria.

FIND FIRST bbItemContrato WHERE bbItemContrato.id_contrato = pContrato
                            AND bbItemContrato.ITEM        = pItemContrato
                            NO-LOCK NO-ERROR.
IF AVAILABLE bbItemContrato THEN DO:

    RUN get-kilos-galones(INPUT pRelease,
                          INPUT pItemRelease,
                          OUTPUT vKilos,
                          OUTPUT  vGalones).
    
    CASE bbItemContrato.id_tipo_unidad_venta_origen:
        WHEN 1 then /* TONELADAS */ DO:
            v_precio_total = ((vKilos / 1000) * bbItemContrato.precio_origen).
        end.
        WHEN 2 then /* KILOS */ DO:
            v_precio_total = (vKilos * bbItemContrato.precio_origen).
        end.
        when 3 then /* GALONES */  DO: 
            v_precio_total = vGalones * bbItemContrato.precio_origen.
        end.
        when 4 then /* LIBRAS */ DO:
            v_precio_total = ((vKilos * 2.20462) * bbItemContrato.precio_origen).
        end.
    END case. 

END.

RETURN v_precio_total.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calculo-fob dTables  _DB-REQUIRED
FUNCTION calculo-fob RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VAR vGastos AS DECIMAL.
DEFINE BUFFER bbItemsOE FOR items_orden_entrega.

FOR EACH bbItemsOE WHERE bbItemsOE.id_orden_entrega = 1
                     AND bbItemsOE.ITEM_oe          = 1.
END.
  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calculo-gastos dTables  _DB-REQUIRED
FUNCTION calculo-gastos RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR hSource AS HANDLE.
DEFINE VAR vReleaseDelivery AS INTEGER.
DEFINE VAR vItemReleaseDelivery AS INTEGER.
DEFINE VAR vContrato AS CHAR.
DEFINE VAR vItemContrato AS INTEGER.
DEFINE VAR vGastoItemOE AS DECIMAL.
DEFINE VAR vGastoPorTamborItemOE AS DECIMAL.
DEFINE VAR vGastoTotalItemReleaseDelivery AS DECIMAL.
DEFINE VAR vGastoParcialItemReleaseDelivery AS DECIMAL.
DEFINE VAR vGastoTonItemReleaseDelivery AS DECIMAL.
DEFINE VAR vGastoUniVtaItemReleaseDelivery AS INTEGER.
DEFINE VAR vTamboresOE AS INTEGER.
DEFINE VAR vKilosTambores AS DECIMAL.
DEFINE VAR vToneladastambores AS DECIMAL.
DEFINE VAR vUnidadVenta AS INTEGER.
DEFINE VAR vTamboresFactura AS INTEGER.
DEFINE VAR vImporteFactura AS DECIMAL.
DEFINE VAR vGastosTemporal AS DECIMAL.
DEFINE VAR vDDP AS DECIMAL.
DEFINE VAR vCIF AS DECIMAL.
DEFINE VAR vCFR AS DECIMAL.
DEFINE VAR vFOBCOM AS DECIMAL.
DEFINE VAR vFOB AS DECIMAL.
DEFINE VAR v_porc_comision AS DECIMAL.
DEFINE VAR p_comision AS DECIMAL.
DEFINE BUFFER bbItemsRelease FOR items_release_delivery.


DEFINE BUFFER bbGastosItemRelease FOR gastos_items_release_delivery.

hSource                 = DYNAMIC-FUNCTION('getDataSource').
vReleaseDelivery        = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_release_delivery').
vItemReleaseDelivery    = DYNAMIC-FUNCTION('columnValue' IN hSource, 'item_release_delivery').
vContrato               = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_contrato').
vItemContrato           = DYNAMIC-FUNCTION('columnValue' IN hSource, 'item').
vUnidadVenta            = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_tipo_unidad_venta_origen').


FIND FIRST items_release_delivery WHERE items_release_delivery.id_release_delivery = vReleaseDelivery
                                    AND items_release_delivery.item_release_delivery = vItemReleaseDelivery
                            NO-LOCK NO-ERROR.
IF AVAILABLE items_release_delivery THEN DO:
    FOR EACH bbGastosItemRelease WHERE bbGastosItemRelease.id_release_delivery   = vReleaseDelivery
                                   AND bbGastosItemRelease.ITEM_release_delivery = vItemReleaseDelivery
                                   AND bbGastosItemRelease.tipo_calculo.
        DELETE bbGastosItemRelease.
    END.
    RELEASE bbGastosItemRelease.
    /* CALCULO LOS GASTOS SEGUN LA ORDEN DE ENTREGA */
    FOR EACH tambores_industria WHERE tambores_industria.id_release_delivery    = items_release_delivery.id_release_delivery
                                  AND tambores_industria.ITEM_release_delivery  = items_release_delivery.ITEM_release_delivery
                                BREAK BY tambores_industria.id_orden_entrega.
        
        vTamboresOE = vTamboresOE + 1.
        vKilosTambores = vKilosTambores + tambores_industria.kilos_tambor.
        
        IF LAST-OF(tambores_industria.id_orden_entrega) THEN DO:
            FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = tambores_industria.id_orden_entrega
                                             AND items_orden_entrega.ITEM_oe          = tambores_industria.ITEM_oe
                                            NO-LOCK NO-ERROR.
            IF AVAILABLE items_orden_entrega THEN DO:
                
                /***********************CALCULO DE LA COMISION ****************************************/
                FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
                IF AVAILABLE items_contratos THEN DO:
                    IF items_contratos.comision_broker = 0 THEN DO:
                        v_porc_comision = 100.
                    END.
                    ELSE DO:
                        v_porc_comision = items_contratos.comision_broker.
                    END.
                    CASE items_contratos.id_tipo_unidad_venta:
                        WHEN 1 THEN /* TONELADAS */ DO:
                            p_comision = (items_contratos.precio_comision * (items_orden_entrega.kgs_netos_tambores / 1000)) * (v_porc_comision / 100).
                        END.
                        WHEN 2 THEN /* KILOS */ DO:
                            p_comision = (items_contratos.precio_comision * items_orden_entrega.kgs_netos_tambores) * (v_porc_comision / 100).
                        END.
                        WHEN 3 THEN /* GALONES */ DO:
                            p_comision = (items_contratos.precio_comision * items_orden_entrega.total_galones) * (v_porc_comision / 100).
                        END.
                        WHEN 4 THEN /* LIBRAS PERO ES IGUAL QUE TONELADAS */ DO:
                            p_comision = (items_contratos.precio_comision * (items_orden_entrega.kgs_netos_tambores * 2.20462)) * (v_porc_comision / 100).
                        END.
                    END CASE.
                    
                    generacion-gastos(INPUT 10,
                                      INPUT p_comision,
                                      INPUT vKilosTambores,
                                      INPUT items_orden_entrega.cantidad_tambores,
                                      INPUT vTamboresOE,
                                      INPUT vReleaseDelivery,
                                      INPUT vItemReleaseDelivery,
                                      INPUT vUnidadVenta).

                END. /* if available items_contratos*/
            
                /***************** END COMISIONES  **********************************************************/
                
                FOR EACH r_clausulas_gastos_item_oe WHERE r_clausulas_gastos_item_oe.id_clausula =
                                                          items_orden_entrega.id_condicion_venta
                                                      NO-LOCK.
                    FIND FIRST gastos_items_orden_entrega OF items_orden_entrega
                                                          WHERE gastos_items_orden_entrega.id_gasto =
                                                                r_clausulas_gastos_item_oe.id_gasto
                                                          NO-LOCK NO-ERROR.
                    IF AVAILABLE gastos_items_orden_entrega THEN DO:
                        
                        generacion-gastos(INPUT gastos_items_orden_entrega.id_gasto,
                                          INPUT gastos_items_orden_entrega.importe,
                                          INPUT vKilosTambores,
                                          INPUT items_orden_entrega.cantidad_tambores,
                                          INPUT vTamboresOE,
                                          INPUT vReleaseDelivery,
                                          INPUT vItemReleaseDelivery,
                                          INPUT vUnidadVenta).

                        /*
                        vToneladastambores                  = vKilosTambores / 1000.
                        vGastoItemOE                        = gastos_items_orden_entrega.importe.
                        vGastoPorTamborItemOE               = vGastoItemOE / items_orden_entrega.cantidad_tambores.
                        vGastoTotalItemReleaseDelivery      = vGastoItemOE.
                        vGastoParcialItemReleaseDelivery    = vGastoPorTamborItemOE * vTamboresOE.
                        vGastoTonItemReleaseDelivery        = vGastoParcialItemReleaseDelivery / vToneladastambores.
                        
                        CASE vUnidadVenta.
                            WHEN 1 THEN /* TONELADA */ DO:
                                vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery.
                            END.
                            WHEN 2 THEN /* KILO */ DO:
                                vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery / 1000.
                            END.
                            WHEN 3 THEN /* GALON */ DO:
                                vGastoUniVtaItemReleaseDelivery = 0.
                            END.
                            WHEN 4 THEN /* LIBRA */ DO:
                                vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery * 0.45.
                            END.
                        END CASE.
                        FIND FIRST bbGastosItemRelease WHERE bbGastosItemRelease.id_release_delivery   = vReleaseDelivery
                                                       AND bbGastosItemRelease.ITEM_release_delivery = vItemReleaseDelivery
                                                       AND bbGastosItemRelease.id_gasto              = gastos_items_orden_entrega.id_gasto
                                                       NO-ERROR.
                        
                        IF AVAILABLE bbGastosItemRelease THEN DO:
                            IF bbGastosItemRelease.tipo_calculo THEN DO:
                                bbGastosItemRelease.importe                = bbGastosItemRelease.importe + vGastoTotalItemReleaseDelivery.
                                bbGastosItemRelease.importe_total          = bbGastosItemRelease.importe_total + vGastoTotalItemReleaseDelivery.
                                bbGastosItemRelease.importe_por_tonelada   = bbGastosItemRelease.importe_por_tonelada + vGastoTonItemReleaseDelivery.
                            END.
                        END.
                        ELSE DO:
                                CREATE gastos_items_release_delivery.
                                ASSIGN gastos_items_release_delivery.id_release_delivery    = vReleaseDelivery
                                       gastos_items_release_delivery.item_release_delivery  = vItemReleaseDelivery
                                       gastos_items_release_delivery.id_gasto               = gastos_items_orden_entrega.id_gasto
                                       gastos_items_release_delivery.importe                = vGastoParcialItemReleaseDelivery
                                       gastos_items_release_delivery.importe_total          = vGastoTotalItemReleaseDelivery
                                       gastos_items_release_delivery.importe_por_tonelada   = vGastoTonItemReleaseDelivery
                                       gastos_items_release_delivery.importe_unidad_venta   = vGastoUniVtaItemReleaseDelivery.
                            
                        END.
                        */

                    END.
                END.
            END.
            vTamboresOE = 0.
            vKilosTambores = 0.
        END.
    END.

    vTamboresOE = 0.
    vKilosTambores = 0.
    RELEASE bbGastosItemRelease.
    /* CALCULO LOS GASTOS SEGUN LAS FACTURAS EN MERCADO EXTERNO */
    FOR EACH tambores_industria WHERE tambores_industria.id_release_delivery    = items_release_delivery.id_release_delivery
                                  AND tambores_industria.ITEM_release_delivery  = items_release_delivery.ITEM_release_delivery
                                BREAK BY tambores_industria.nromov.
        
        vTamboresOE = vTamboresOE + 1.
        vKilosTambores = vKilosTambores + tambores_industria.kilos_tambor.
        
        IF LAST-OF(tambores_industria.nromov) THEN DO:
            FOR EACH r_facturas_exterior_lote WHERE r_facturas_exterior_lote.id_empresa     = tambores_industria.id_empresa
                                                AND r_facturas_exterior_lote.id_sucursal    = tambores_industria.id_sucursal
                                                AND r_facturas_exterior_lote.id_tipotambor  = tambores_industria.id_tipotambor
                                                AND r_facturas_exterior_lote.nromov_lote    = tambores_industria.nromov
                                                NO-LOCK.

                vTamboresFactura                    = r_facturas_exterior_lote.hasta_tambor - r_facturas_exterior_lote.desde_tambor + 1.
                vImporteFactura                     = r_facturas_exterior_lote.importe.
                vToneladastambores                  = vKilosTambores / 1000.
                vGastoPorTamborItemOE               = vImporteFactura / vTamboresFactura.
                vGastoTotalItemReleaseDelivery      = vImporteFactura.
                vGastoParcialItemReleaseDelivery    = vGastoPorTamborItemOE * vTamboresOE.
                vGastoTonItemReleaseDelivery        = vGastoParcialItemReleaseDelivery / vToneladastambores.

                CASE vUnidadVenta.
                    WHEN 1 THEN /* TONELADA */ DO:
                        vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery.
                    END.
                    WHEN 2 THEN /* KILO */ DO:
                        vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery / 1000.
                    END.
                    WHEN 3 THEN /* GALON */ DO:
                        vGastoUniVtaItemReleaseDelivery = 66.
                    END.
                    WHEN 4 THEN /* LIBRA */ DO:
                        vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery * 0.45.
                    END.
                END CASE.
                FIND FIRST bbGastosItemRelease WHERE bbGastosItemRelease.id_release_delivery   = vReleaseDelivery
                                               AND bbGastosItemRelease.ITEM_release_delivery   = vItemReleaseDelivery
                                               AND bbGastosItemRelease.id_gasto                = r_facturas_exterior_lote.id_gasto
                                               NO-ERROR.
                IF bbGastosItemRelease.tipo_calculo THEN DO:
                    IF AVAILABLE bbGastosItemRelease THEN DO:
                        /*
                        vGastosTemporal = bbGastosItemRelease.importe.
                        DELETE bbGastosItemRelease.
    
                        CREATE gastos_items_release_delivery.
                        ASSIGN gastos_items_release_delivery.id_release_delivery    = vReleaseDelivery
                               gastos_items_release_delivery.item_release_delivery  = vItemReleaseDelivery
                               gastos_items_release_delivery.id_gasto               = gastos_items_orden_entrega.id_gasto
                               gastos_items_release_delivery.importe                = vGastosTemporal + vGastoParcialItemReleaseDelivery
                               gastos_items_release_delivery.importe_total          = vGastoTotalItemReleaseDelivery
                               gastos_items_release_delivery.importe_por_tonelada   = vGastoTonItemReleaseDelivery
                               gastos_items_release_delivery.importe_unidad_venta   = vGastoUniVtaItemReleaseDelivery.
                               */
                        
                        ASSIGN bbGastosItemRelease.importe                = bbGastosItemRelease.importe + vGastoParcialItemReleaseDelivery
                               bbGastosItemRelease.importe_total          = bbGastosItemRelease.importe_total + vGastoTotalItemReleaseDelivery
                               bbGastosItemRelease.importe_por_tonelada   = bbGastosItemRelease.importe_por_tonelada + vGastoTonItemReleaseDelivery
                               bbGastosItemRelease.importe_unidad_venta   = vGastoUniVtaItemReleaseDelivery.
                          
                    END.
                    ELSE DO:
                        CREATE gastos_items_release_delivery.
                        ASSIGN gastos_items_release_delivery.id_release_delivery    = vReleaseDelivery
                               gastos_items_release_delivery.item_release_delivery  = vItemReleaseDelivery
                               gastos_items_release_delivery.id_gasto               = gastos_items_orden_entrega.id_gasto
                               gastos_items_release_delivery.importe                = vGastoParcialItemReleaseDelivery
                               gastos_items_release_delivery.importe_total          = vGastoTotalItemReleaseDelivery
                               gastos_items_release_delivery.importe_por_tonelada   = vGastoTonItemReleaseDelivery
                               gastos_items_release_delivery.importe_unidad_venta   = vGastoUniVtaItemReleaseDelivery.
                    END.
                END.
            END.

            vTamboresOE = 0.
            vKilosTambores = 0.
        END.
    END.
END.

vDDP = calculo-ddp(INPUT vContrato,
                   INPUT vItemContrato,
                   INPUT vReleaseDelivery,
                   INPUT vItemReleaseDelivery).

vCIF = calculo-clausulas(INPUT vReleaseDelivery,
                         INPUT vItemReleaseDelivery,
                         INPUT vDDP,
                         INPUT 12).

vCFR = calculo-clausulas(INPUT vReleaseDelivery,
                         INPUT vItemReleaseDelivery,
                         INPUT vCIF,
                         INPUT 3).

vFOB = calculo-clausulas(INPUT vReleaseDelivery,
                         INPUT vItemReleaseDelivery,
                         INPUT vCFR,
                         INPUT 2).

FIND FIRST bbItemsRelease WHERE bbItemsRelease.id_release_delivery = vReleaseDelivery
                            AND bbItemsRelease.item_release_delivery = vItemReleaseDelivery
                            NO-ERROR.

IF AVAILABLE bbItemsRelease THEN DO:
    ASSIGN bbItemsRelease.calculo_ddp = vDDP
           bbItemsRelease.calculo_cif = vCIF
           bbItemsRelease.calculo_cfr = vCFR
           bbItemsRelease.calculo_fob = vFOB.
END.
RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION generacion-gastos dTables  _DB-REQUIRED
FUNCTION generacion-gastos RETURNS CHARACTER
  (INPUT pGasto AS INTEGER,
   INPUT pImporte AS DECIMAL,
   INPUT pKilosTambores AS DECIMAL,
   INPUT pCantidadTambores AS INTEGER,
   INPUT pTamboresOE AS INTEGER,
   INPUT pReleaseDelivery AS INTEGER,
   INPUT pItemReleaseDelivery AS INTEGER,
   INPUT pUnidadVenta AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR vToneladastambores AS DECIMAL.
DEFINE VAR vGastoItemOE AS DECIMAL.
DEFINE VAR vGastoPorTamborItemOE AS DECIMAL.
DEFINE VAR vGastoTotalItemReleaseDelivery AS DECIMAL.
DEFINE VAR vGastoParcialItemReleaseDelivery AS DECIMAL.
DEFINE VAR vGastoTonItemReleaseDelivery AS DECIMAL.
DEFINE VAR vGastoUniVtaItemReleaseDelivery AS INTEGER.

DEFINE BUFFER bbGastosItemRelease FOR gastos_items_release_delivery.

vToneladastambores                  = pKilosTambores / 1000.
vGastoItemOE                        = pImporte.
vGastoPorTamborItemOE               = vGastoItemOE / pCantidadTambores.
vGastoTotalItemReleaseDelivery      = vGastoItemOE.
vGastoParcialItemReleaseDelivery    = vGastoPorTamborItemOE * pTamboresOE.
vGastoTonItemReleaseDelivery        = vGastoParcialItemReleaseDelivery / vToneladastambores.

CASE pUnidadVenta.
    WHEN 1 THEN /* TONELADA */ DO:
        vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery.
    END.
    WHEN 2 THEN /* KILO */ DO:
        vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery / 1000.
    END.
    WHEN 3 THEN /* GALON */ DO:
        vGastoUniVtaItemReleaseDelivery = 0.
    END.
    WHEN 4 THEN /* LIBRA */ DO:
        vGastoUniVtaItemReleaseDelivery = vGastoTonItemReleaseDelivery * 0.45.
    END.
END CASE.
FIND FIRST bbGastosItemRelease WHERE bbGastosItemRelease.id_release_delivery   = pReleaseDelivery
                                 AND bbGastosItemRelease.ITEM_release_delivery = pItemReleaseDelivery
                                 AND bbGastosItemRelease.id_gasto              = pGasto
                               NO-ERROR.

IF AVAILABLE bbGastosItemRelease THEN DO:
    IF bbGastosItemRelease.tipo_calculo THEN DO:
        bbGastosItemRelease.importe                = bbGastosItemRelease.importe + vGastoTotalItemReleaseDelivery.
        bbGastosItemRelease.importe_total          = bbGastosItemRelease.importe_total + vGastoTotalItemReleaseDelivery.
        bbGastosItemRelease.importe_por_tonelada   = bbGastosItemRelease.importe_por_tonelada + vGastoTonItemReleaseDelivery.
    END.
END.
ELSE DO:
        CREATE gastos_items_release_delivery.
        ASSIGN gastos_items_release_delivery.id_release_delivery    = pReleaseDelivery
               gastos_items_release_delivery.item_release_delivery  = pItemReleaseDelivery
               gastos_items_release_delivery.id_gasto               = pGasto
               gastos_items_release_delivery.importe                = vGastoParcialItemReleaseDelivery
               gastos_items_release_delivery.importe_total          = vGastoTotalItemReleaseDelivery
               gastos_items_release_delivery.importe_por_tonelada   = vGastoTonItemReleaseDelivery
               gastos_items_release_delivery.importe_unidad_venta   = vGastoUniVtaItemReleaseDelivery.
    
END.
    
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

