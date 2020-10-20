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
&Scoped-define INTERNAL-TABLES items_insumo

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  cantidad id_articulo id_calidad id_confeccion id_envase id_insumo~
 id_tipo_unidad item
&Scoped-define ENABLED-FIELDS-IN-items_insumo cantidad id_articulo ~
id_calidad id_confeccion id_envase id_insumo id_tipo_unidad item 
&Scoped-Define DATA-FIELDS  cantidad Insumo Unidad id_articulo id_calidad id_confeccion id_envase~
 id_insumo id_tipo_unidad item
&Scoped-define DATA-FIELDS-IN-items_insumo cantidad id_articulo id_calidad ~
id_confeccion id_envase id_insumo id_tipo_unidad item 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "ditemsinsumo.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_insumo NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_insumo NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_insumo
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_insumo


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInsumo dTables  _DB-REQUIRED
FUNCTION getInsumo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextItem dTables  _DB-REQUIRED
FUNCTION getNextItem RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidad dTables  _DB-REQUIRED
FUNCTION getUnidad RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_insumo SCROLLING.
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
     _TblList          = "general.items_insumo"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.items_insumo.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[2]   > "_<CALC>"
"getInsumo()" "Insumo" "Insumo" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no
     _FldNameList[3]   > "_<CALC>"
"getUnidad()" "Unidad" "Unidad" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[4]   > general.items_insumo.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[5]   > general.items_insumo.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[6]   > general.items_insumo.id_confeccion
"id_confeccion" "id_confeccion" ? ? "integer" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[7]   > general.items_insumo.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[8]   > general.items_insumo.id_insumo
"id_insumo" "id_insumo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[9]   > general.items_insumo.id_tipo_unidad
"id_tipo_unidad" "id_tipo_unidad" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[10]   > general.items_insumo.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes
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
DEFINE VARIABLE vhData AS HANDLE     NO-UNDO.

FIND LAST RowObjUpd NO-ERROR.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C" OR rowObjUpd.rowMod = "U". 
  vhData = DYNAMIC-FUNCTION('getDataSource').
  ASSIGN rowObjUpd.id_articulo   = DYNAMIC-FUNCTION('columnValue' IN vhData, 'id_articulo_insumo')
         rowObjUpd.id_calidad    = DYNAMIC-FUNCTION('columnValue' IN vhData, 'id_calidad')
         rowObjUpd.id_envase     = DYNAMIC-FUNCTION('columnValue' IN vhData, 'id_envase')
         rowObjUpd.id_confeccion = DYNAMIC-FUNCTION('columnValue' IN vhData, 'id_confeccion').
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_articulo,id_calidad,id_envase,id_confeccion".
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
         rowObject.Insumo = (getInsumo())
         rowObject.Unidad = (getUnidad())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInsumo dTables  _DB-REQUIRED
FUNCTION getInsumo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST insumos WHERE insumos.id_articulo = rowObject.id_insumo NO-LOCK NO-ERROR.
  IF AVAILABLE insumos THEN DO:
    FIND FIRST articulos WHERE insumos.id_articulo = articulos.id_articulo NO-LOCK NO-ERROR.
      IF AVAILABLE articulos THEN
        vcRet = articulos.descripcion.
  END.

  RETURN vcRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextItem dTables  _DB-REQUIRED
FUNCTION getNextItem RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viRet AS INTEGER INITIAL 1   NO-UNDO.

  FOR EACH items_insumo BY ITEM DESC.
    viRet = items_insumo.ITEM + 1.
    LEAVE.
  END.
  
  RETURN viRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidad dTables  _DB-REQUIRED
FUNCTION getUnidad RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcRet AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST tipo_unidad_venta WHERE tipo_unidad_venta.id_tipo_unidad_venta = rowObject.id_tipo_unidad NO-LOCK NO-ERROR.
  IF AVAILABLE tipo_unidad_venta THEN 
    vcRet = tipo_unidad_venta.descripcion.

  RETURN vcRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}
