&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
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
&Scoped-define INTERNAL-TABLES sobrante_lotes_aceite

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  cantidad c_fecha c_hora c_usuario fecha_elaboracion identificacion~
 id_articulo id_empresa id_envase id_lote id_sobrante id_sucursal~
 id_tipotambor id_tipotambor_sobrante kilos kilos_tambor nromov~
 nromov_sobrante tipo_almacenamiento volumen
&Scoped-define ENABLED-FIELDS-IN-sobrante_lotes_aceite cantidad c_fecha ~
c_hora c_usuario fecha_elaboracion identificacion id_articulo id_empresa ~
id_envase id_lote id_sobrante id_sucursal id_tipotambor ~
id_tipotambor_sobrante kilos kilos_tambor nromov nromov_sobrante ~
tipo_almacenamiento volumen 
&Scoped-Define DATA-FIELDS  cantidad c_fecha c_hora c_usuario fecha_elaboracion identificacion~
 id_articulo id_empresa id_envase id_lote id_sobrante id_sucursal~
 id_tipotambor id_tipotambor_sobrante kilos kilos_tambor nromov~
 nromov_sobrante tipo_almacenamiento volumen Articulo Envase
&Scoped-define DATA-FIELDS-IN-sobrante_lotes_aceite cantidad c_fecha c_hora ~
c_usuario fecha_elaboracion identificacion id_articulo id_empresa id_envase ~
id_lote id_sobrante id_sucursal id_tipotambor id_tipotambor_sobrante kilos ~
kilos_tambor nromov nromov_sobrante tipo_almacenamiento volumen 
&Scoped-Define MANDATORY-FIELDS  id_lote id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dsobranteaceite.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH sobrante_lotes_aceite NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH sobrante_lotes_aceite NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main sobrante_lotes_aceite
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main sobrante_lotes_aceite


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLastNroSobrante dTables  _DB-REQUIRED
FUNCTION getLastNroSobrante RETURNS INTEGER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      sobrante_lotes_aceite SCROLLING.
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
     _TblList          = "general.sobrante_lotes_aceite"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.sobrante_lotes_aceite.cantidad
"cantidad" "cantidad" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[2]   > general.sobrante_lotes_aceite.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[3]   > general.sobrante_lotes_aceite.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[4]   > general.sobrante_lotes_aceite.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[5]   > general.sobrante_lotes_aceite.fecha_elaboracion
"fecha_elaboracion" "fecha_elaboracion" ? ? "date" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[6]   > general.sobrante_lotes_aceite.identificacion
"identificacion" "identificacion" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[7]   > general.sobrante_lotes_aceite.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[8]   > general.sobrante_lotes_aceite.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[9]   > general.sobrante_lotes_aceite.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[10]   > general.sobrante_lotes_aceite.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[11]   > general.sobrante_lotes_aceite.id_sobrante
"id_sobrante" "id_sobrante" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[12]   > general.sobrante_lotes_aceite.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[13]   > general.sobrante_lotes_aceite.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[14]   > general.sobrante_lotes_aceite.id_tipotambor_sobrante
"id_tipotambor_sobrante" "id_tipotambor_sobrante" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[15]   > general.sobrante_lotes_aceite.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[16]   > general.sobrante_lotes_aceite.kilos_tambor
"kilos_tambor" "kilos_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[17]   > general.sobrante_lotes_aceite.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[18]   > general.sobrante_lotes_aceite.nromov_sobrante
"nromov_sobrante" "nromov_sobrante" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[19]   > general.sobrante_lotes_aceite.tipo_almacenamiento
"tipo_almacenamiento" "tipo_almacenamiento" ? ? "logical" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[20]   > general.sobrante_lotes_aceite.volumen
"volumen" "volumen" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[21]   > "_<CALC>"
"getArticulo()" "Articulo" "Articulo" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[22]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
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
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSob    AS INTEGER    NO-UNDO.



FIND LAST RowObjUpd NO-ERROR.



FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".

  hSource = DYNAMIC-FUNCTION('getDataSource').

  ASSIGN iEmp = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_empresa')
         iSuc = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal')
         iTip = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_tipotambor')
         iNro = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nromov').


  ASSIGN rowObjUpd.id_empresa             = iEmp
         rowObjUpd.id_sucursal            = iSuc
         rowObjUpd.id_tipotambor          = iTip
         rowObjUpd.nromov                 = iNro
         rowObjUpd.id_lote                = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_lote')
         rowObjUpd.id_sobrante            = getLastNroSobrante(iEmp, iSuc, iTip, iNro)
         rowObjUpd.id_tipotambor_sobrante = 8
         rowObjUpd.nromov_sobrante        = NEXT-VALUE(nromov)
         rowObjUpd.c_usuario              = USERID("userdb")
         rowObjUpd.c_fecha                = TODAY  
         rowObjUpd.c_hora                 = STRING(TIME,"HH:MM:SS").
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora,id_empresa,id_sucursal,id_tipotambor,nromov,id_lote,id_sobrante,id_tipotambor_sobrante,nromov_sobrante".
  /*crear tambores de sobrante*/

  

END.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".
  ASSIGN rowObjUpd.c_usuario  = USERID("userdb")
         rowObjUpd.c_fecha    = TODAY  
         rowObjUpd.c_hora     = STRING(TIME,"HH:MM:SS").
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".
  
END.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D".
  /*borrar tambores de sobrante*/
  RUN deleteDrumsFromBatch IN hLib (rowObjUpd.id_empresa, 
                                    rowObjUpd.id_sucursal, 
                                    rowObjUpd.id_tipotambor_sobrante,
                                    rowObjUpd.nromov_sobrante, 
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

  RUN libTamboresIndustria.p PERSISTENT SET hLib.



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
  /*crear tambores sobrante*/
  RUN createDrumsInBatch IN hLib (rowObjUpd.id_empresa, 
                                  rowObjUpd.id_sucursal, 
                                  rowObjUpd.id_tipotambor_sobrante, 
                                  rowObjUpd.nromov_sobrante, 
                                  rowObjUpd.cantidad, 
                                  1, 
                                  rowObjUpd.cantidad, 
                                  rowObjUpd.kilos,
                                  0, 
                                  6).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLastNroSobrante dTables  _DB-REQUIRED
FUNCTION getLastNroSobrante RETURNS INTEGER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iSobrante AS INTEGER INITIAL 1   NO-UNDO.
  

  FIND LAST sobrante_lotes_aceite WHERE sobrante_lotes_aceite.id_empresa       = piEmp
                                    AND sobrante_lotes_aceite.id_sucursal      = piSuc
                                    AND sobrante_lotes_aceite.id_tipotambor    = piTip
                                    AND sobrante_lotes_aceite.nromov           = piNro
                                  NO-LOCK NO-ERROR.
  IF AVAILABLE sobrante_lotes_aceite THEN DO:
      iSobrante = sobrante_lotes_aceite.id_sobrante + 1.
  END.
  
  RETURN iSobrante.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

