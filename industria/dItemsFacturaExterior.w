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
&Scoped-define INTERNAL-TABLES items_facturas_exterior gastos_venta

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  cantidad c_fecha c_hora c_usuario fecha_desde fecha_hasta id_gasto~
 id_operacion id_tipocomp id_unidad_medida importe importe_unitario item~
 nromov observaciones
&Scoped-define ENABLED-FIELDS-IN-items_facturas_exterior cantidad c_fecha ~
c_hora c_usuario fecha_desde fecha_hasta id_gasto id_operacion id_tipocomp ~
id_unidad_medida importe importe_unitario item nromov observaciones 
&Scoped-Define DATA-FIELDS  cantidad c_fecha c_hora c_usuario fecha_desde fecha_hasta id_gasto~
 id_operacion id_tipocomp id_unidad_medida importe importe_unitario item~
 nromov observaciones abreviatura descripcion
&Scoped-define DATA-FIELDS-IN-items_facturas_exterior cantidad c_fecha ~
c_hora c_usuario fecha_desde fecha_hasta id_gasto id_operacion id_tipocomp ~
id_unidad_medida importe importe_unitario item nromov observaciones 
&Scoped-define DATA-FIELDS-IN-gastos_venta abreviatura descripcion 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "ditemsfacturaexterior.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_facturas_exterior NO-LOCK, ~
      EACH gastos_venta OF items_facturas_exterior NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_facturas_exterior NO-LOCK, ~
      EACH gastos_venta OF items_facturas_exterior NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_facturas_exterior ~
gastos_venta
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_facturas_exterior
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main gastos_venta


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_facturas_exterior, 
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
     _TblList          = "general.items_facturas_exterior,industria.gastos_venta OF general.items_facturas_exterior"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.items_facturas_exterior.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[2]   > general.items_facturas_exterior.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[3]   > general.items_facturas_exterior.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[4]   > general.items_facturas_exterior.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[5]   > general.items_facturas_exterior.fecha_desde
"fecha_desde" "fecha_desde" ? ? "date" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[6]   > general.items_facturas_exterior.fecha_hasta
"fecha_hasta" "fecha_hasta" ? ? "date" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[7]   > general.items_facturas_exterior.id_gasto
"id_gasto" "id_gasto" ? ? "integer" ? ? ? ? ? ? yes ? no 3.6 yes
     _FldNameList[8]   > general.items_facturas_exterior.id_operacion
"id_operacion" "id_operacion" ? ? "integer" ? ? ? ? ? ? yes ? no 3.6 yes
     _FldNameList[9]   > general.items_facturas_exterior.id_tipocomp
"id_tipocomp" "id_tipocomp" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[10]   > general.items_facturas_exterior.id_unidad_medida
"id_unidad_medida" "id_unidad_medida" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[11]   > general.items_facturas_exterior.importe
"importe" "importe" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[12]   > general.items_facturas_exterior.importe_unitario
"importe_unitario" "importe_unitario" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[13]   > general.items_facturas_exterior.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes
     _FldNameList[14]   > general.items_facturas_exterior.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[15]   > general.items_facturas_exterior.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[16]   > general.gastos_venta.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[17]   > general.gastos_venta.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
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
DEFINE BUFFER bbItemFac FOR items_facturas_exterior.
DEFINE VAR hSource AS HANDLE.
DEFINE VAR vOperacion AS INTEGER.
DEFINE VAR vTipocomp AS INTEGER.
DEFINE VAR vNromov AS INTEGER.
DEFINE VAR vItem AS INTEGER.

FIND LAST RowObjUpd NO-ERROR.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A" OR RowObjUpd.RowMod = "C".
  
  hSource       = DYNAMIC-FUNCTION('getDataSource').
  vTipocomp     = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_tipocomp').
  vNromov       = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nromov').

  FIND LAST bbItemFac WHERE bbItemFac.id_operacion  = vOperacion
                        AND bbItemFac.id_tipocomp   = vTipocomp
                        AND bbItemFac.nromov        = vNromov
                        NO-LOCK NO-ERROR.
  vItem = IF AVAILABLE bbItemFac THEN (bbItemFac.ITEM + 1) ELSE 1.
  FIND FIRST gastos_venta OF rowObjUpd NO-LOCK NO-ERROR.

  ASSIGN RowObjUpd.id_tipocomp           = vTipocomp
         RowObjUpd.nromov                = vNromov
         RowObjUpd.ITEM                  = vItem
         RowObjUpd.id_operacion          = IF AVAILABLE gastos_venta THEN gastos_venta.id_operacion ELSE 0
         RowObjUpd.c_fecha               = TODAY
         RowObjUpd.c_usuario             = USERID("userdb")
         RowObjUpd.c_hora                = STRING(TIME,"HH:MM:SS").

  RowObjUpd.changedFields = RowObjUpd.changedFields + ",id_operacion,id_tipocomp,item,nromov,c_fecha,c_usuario,c_hora".
END.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "U".
  
  FIND FIRST gastos_venta OF rowObjUpd NO-LOCK NO-ERROR.

  ASSIGN RowObjUpd.id_operacion          = IF AVAILABLE gastos_venta THEN gastos_venta.id_operacion ELSE 0.

  RowObjUpd.changedFields = RowObjUpd.changedFields + ",id_operacion".
END.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D".
    FOR EACH r_facturas_exterior_proforma WHERE r_facturas_exterior_proforma.id_tipocomp_factura    = RowObjUpd.id_tipocomp
                                            AND r_facturas_exterior_proforma.id_operacion           = RowObjUpd.id_operacion
                                            AND r_facturas_exterior_proforma.nromov                 = RowObjUpd.nromov
                                            AND r_facturas_exterior_proforma.ITEM                   = RowObjUpd.ITEM.
        DELETE r_facturas_exterior_proforma.
    END.

    FOR EACH r_facturas_exterior_lote WHERE r_facturas_exterior_lote.id_tipocomp    = RowObjUpd.id_tipocomp
                                        AND r_facturas_exterior_lote.id_operacion   = RowObjUpd.id_operacion
                                        AND r_facturas_exterior_lote.nromov         = RowObjUpd.nromov
                                        AND r_facturas_exterior_lote.ITEM           = RowObjUpd.ITEM.
        DELETE r_facturas_exterior_lote.
    END.
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

