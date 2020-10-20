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
&Scoped-define INTERNAL-TABLES r_lote_cascara_remito

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_empresa id_sucursal id_tipotambor nromov id_sucursal_remito~
 id_tipo_movsto nro_remito item_factura
&Scoped-define ENABLED-FIELDS-IN-r_lote_cascara_remito id_empresa ~
id_sucursal id_tipotambor nromov id_sucursal_remito id_tipo_movsto ~
nro_remito item_factura 
&Scoped-Define DATA-FIELDS  id_empresa FechaRemito Sucursal LugDesc SucDestino Cantidad NroComp~
 id_sucursal id_tipotambor nromov id_sucursal_remito id_tipo_movsto~
 nro_remito item_factura
&Scoped-define DATA-FIELDS-IN-r_lote_cascara_remito id_empresa id_sucursal ~
id_tipotambor nromov id_sucursal_remito id_tipo_movsto nro_remito ~
item_factura 
&Scoped-Define MANDATORY-FIELDS  id_sucursal
&Scoped-Define APPLICATION-SERVICE asindustria
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "drlotecascararemito.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH r_lote_cascara_remito NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH r_lote_cascara_remito NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main r_lote_cascara_remito
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main r_lote_cascara_remito


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArrayRemitos dTables  _DB-REQUIRED
FUNCTION getArrayRemitos RETURNS CHARACTER
  (INPUT piNroMov AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidad dTables  _DB-REQUIRED
FUNCTION getCantidad RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFechaRemito dTables  _DB-REQUIRED
FUNCTION getFechaRemito RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLugarDescarga dTables  _DB-REQUIRED
FUNCTION getLugarDescarga RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNroComp dTables  _DB-REQUIRED
FUNCTION getNroComp RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursalDesde dTables  _DB-REQUIRED
FUNCTION getSucursalDesde RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursalHasta dTables  _DB-REQUIRED
FUNCTION getSucursalHasta RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      r_lote_cascara_remito SCROLLING.
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
   Partition: asindustria
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
     _TblList          = "general.r_lote_cascara_remito"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.r_lote_cascara_remito.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[2]   > "_<CALC>"
"getFechaRemito()" "FechaRemito" "FechaRemitos" "99/99/9999" "Date" ? ? ? ? ? ? no ? no 13.6 no
     _FldNameList[3]   > "_<CALC>"
"getSucursalDesde()" "Sucursal" "Sucursal" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _FldNameList[4]   > "_<CALC>"
"getLugarDescarga()" "LugDesc" "LugDesc" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _FldNameList[5]   > "_<CALC>"
"getSucursalHasta()" "SucDestino" "SucDestino" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _FldNameList[6]   > "_<CALC>"
"getCantidad()" "Cantidad" "Cantidad" ">>>>>" "Integer" ? ? ? ? ? ? no ? no 8.4 no
     _FldNameList[7]   > "_<CALC>"
"getNroComp()" "NroComp" "NroComp" "XXXX-XXXXXXXX" "character" ? ? ? ? ? ? no ? no 13 no
     _FldNameList[8]   > general.r_lote_cascara_remito.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[9]   > general.r_lote_cascara_remito.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[10]   > general.r_lote_cascara_remito.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[11]   > general.r_lote_cascara_remito.id_sucursal_remito
"id_sucursal_remito" "id_sucursal_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[12]   > general.r_lote_cascara_remito.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[13]   > general.r_lote_cascara_remito.nro_remito
"nro_remito" "nro_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[14]   > general.r_lote_cascara_remito.item_factura
"item_factura" "item_factura" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
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
         rowObject.Cantidad = (getCantidad())
         rowObject.FechaRemito = (getFechaRemito())
         rowObject.LugDesc = (getLugarDescarga())
         rowObject.NroComp = (getNroComp())
         rowObject.SucDestino = (getSucursalHasta())
         rowObject.Sucursal = (getSucursalDesde())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArrayRemitos dTables  _DB-REQUIRED
FUNCTION getArrayRemitos RETURNS CHARACTER
  (INPUT piNroMov AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcArray AS CHARACTER  NO-UNDO.

  FOR EACH r_lote_cascara_remito WHERE r_lote_cascara_remito.nromov = piNroMov 
                                 NO-LOCK .
    FIND FIRST remitos WHERE r_lote_cascara_remito.nro_remito         = remitos.nro
                         AND r_lote_cascara_remito.id_sucursal_remito = remitos.id_sucursal
                       NO-LOCK NO-ERROR.
    IF AVAILABLE remitos THEN DO:
      vcArray = vcArray + STRING(remitos.nro_comp, "XXXX-XXXXXXXX") +  "," + STRING(remitos.fecha).
      FIND FIRST items_factura WHERE r_lote_cascara_remito.nro_remito         = items_factura.nro
                                 AND r_lote_cascara_remito.ITEM_factura       = items_factura.ITEM
                                 AND r_lote_cascara_remito.id_sucursal_remito = items_factura.id_sucursal
                               NO-LOCK NO-ERROR.
      IF AVAILABLE items_factura THEN
        vcArray = vcArray + "," + STRING(items_factura.cantidad).
      ELSE
        vcArray = vcArray + ",Wrong Qty".
      FIND FIRST sucursales WHERE sucursales.id_sucursal = items_factura.id_sucursal NO-LOCK NO-ERROR.
      vcArray = vcArray + "," + IF AVAILABLE sucursales THEN sucursales.nombre ELSE "#".
      FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = remitos.id_lugdes NO-LOCK NO-ERROR.
      vcArray = vcArray + "," + IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "#".
    END.
    IF remitos.estado THEN
      vcArray = vcArray + ",Vigente".
    ELSE 
      vcArray = vcArray + ",Anulado".
    IF remitos.id_tipo_movsto = 123 THEN
      vcArray = vcArray + ",Automatico".
    ELSE
      vcArray = vcArray + ",Manual".

    vcArray = vcArray + CHR(14).
  END.
  vcArray = SUBSTRING(vcArray, 1, LENGTH(vcArray) - 1).

  RETURN vcArray.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidad dTables  _DB-REQUIRED
FUNCTION getCantidad RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO.

  FIND CURRENT rowObject.
  FIND FIRST items_factura WHERE items_factura.nro         = rowObject.nro_remito
                             AND items_factura.ITEM        = rowObject.ITEM_factura
                             AND items_factura.id_sucursal = rowObject.id_sucursal_remito
                           NO-LOCK NO-ERROR.
  IF AVAILABLE items_factura THEN DO:
    viCant = items_factura.cantidad.
  END.
  
  RETURN viCant.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFechaRemito dTables  _DB-REQUIRED
FUNCTION getFechaRemito RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vdtFecha AS DATE       NO-UNDO.
  
  FIND CURRENT rowObject.
  FIND FIRST remitos WHERE remitos.nro         = rowObject.nro_remito
                       AND remitos.id_sucursal = rowObject.id_sucursal_remito
                     NO-LOCK NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    vdtFecha = remitos.fecha.
  END.

  RETURN vdtFecha.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLugarDescarga dTables  _DB-REQUIRED
FUNCTION getLugarDescarga RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcLugDes AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject.
  FIND FIRST remitos WHERE remitos.nro = rowObject.nro_remito
                       AND remitos.id_sucursal = rowObject.id_sucursal_remito
                     NO-LOCK NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = remitos.id_lugdes
                              NO-LOCK NO-ERROR.
    IF AVAILABLE lugar_descarga THEN DO:
      vcLugDes = lugar_descarga.descripcion.
    END.
  END.

  RETURN vcLugDes.  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNroComp dTables  _DB-REQUIRED
FUNCTION getNroComp RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcNroComp AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject.
  FIND FIRST remitos WHERE remitos.nro         = rowObject.nro_remito
                       AND remitos.id_sucursal = rowObject.id_sucursal_remito
                     NO-LOCK NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    vcNroComp = STRING(remitos.nro_comp, "XXXX-XXXXXXXX").
  END.

  RETURN vcNroComp.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursalDesde dTables  _DB-REQUIRED
FUNCTION getSucursalDesde RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcSucDesde AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject.
  FIND FIRST remitos WHERE remitos.nro = rowObject.nro_remito
                       AND remitos.id_sucursal = rowObject.id_sucursal_remito
                     NO-LOCK NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    FIND FIRST sucursales WHERE sucursales.id_sucursal = remitos.id_sucursal
                          NO-LOCK NO-ERROR.
    IF AVAILABLE sucursales THEN DO:
      vcSucDesde = IF sucursales.abreviatura = "" THEN sucursales.nombre ELSE sucursales.abreviatura.
    END.
  END.

  RETURN vcSucDesde.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursalHasta dTables  _DB-REQUIRED
FUNCTION getSucursalHasta RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcSucHasta AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject.
  FIND FIRST remitos WHERE remitos.nro = rowObject.nro_remito
                       AND remitos.id_sucursal = rowObject.id_sucursal_remito
                     NO-LOCK NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = remitos.id_lugdes
                              NO-LOCK NO-ERROR.
    IF AVAILABLE lugar_descarga THEN DO:
      FIND FIRST sucursales WHERE sucursales.id_sucursal = lugar_descarga.id_sucursal
                            NO-LOCK NO-ERROR.
      IF AVAILABLE sucursales THEN DO:
        vcSucHasta = IF sucursales.abreviatura = "" THEN sucursales.nombre ELSE sucursales.abreviatura.
      END.
    END.
  END.

  RETURN vcSucHasta.  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}
