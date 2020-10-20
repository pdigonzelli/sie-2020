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
&Scoped-define INTERNAL-TABLES r_analisis_especificacion

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio Anio_lote c_fecha c_hora c_usuario Fecha id_analisis id_contrato~
 id_empresa id_especificacion id_lote id_sucursal id_tipotambor~
 id_tipo_contrato id_unidad item nromov observaciones valor
&Scoped-define ENABLED-FIELDS-IN-r_analisis_especificacion anio Anio_lote ~
c_fecha c_hora c_usuario Fecha id_analisis id_contrato id_empresa ~
id_especificacion id_lote id_sucursal id_tipotambor id_tipo_contrato ~
id_unidad item nromov observaciones valor 
&Scoped-Define DATA-FIELDS  anio Anio_lote c_fecha c_hora c_usuario Fecha id_analisis id_contrato~
 id_empresa id_especificacion id_lote id_sucursal id_tipotambor~
 id_tipo_contrato id_unidad item nromov observaciones valor Analisis Unidad~
 Especificacion
&Scoped-define DATA-FIELDS-IN-r_analisis_especificacion anio Anio_lote ~
c_fecha c_hora c_usuario Fecha id_analisis id_contrato id_empresa ~
id_especificacion id_lote id_sucursal id_tipotambor id_tipo_contrato ~
id_unidad item nromov observaciones valor 
&Scoped-Define MANDATORY-FIELDS  id_lote id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dranalisisespecificacion.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH r_analisis_especificacion NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH r_analisis_especificacion NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main r_analisis_especificacion
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main r_analisis_especificacion


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAnalisis dTables  _DB-REQUIRED
FUNCTION getAnalisis RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEspecificacion dTables  _DB-REQUIRED
FUNCTION getEspecificacion RETURNS CHARACTER
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
      r_analisis_especificacion SCROLLING.
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
     _TblList          = "general.r_analisis_especificacion"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.r_analisis_especificacion.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > general.r_analisis_especificacion.Anio_lote
"Anio_lote" "Anio_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[3]   > general.r_analisis_especificacion.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[4]   > general.r_analisis_especificacion.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[5]   > general.r_analisis_especificacion.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[6]   > general.r_analisis_especificacion.Fecha
"Fecha" "Fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[7]   > general.r_analisis_especificacion.id_analisis
"id_analisis" "id_analisis" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[8]   > general.r_analisis_especificacion.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[9]   > general.r_analisis_especificacion.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[10]   > general.r_analisis_especificacion.id_especificacion
"id_especificacion" "id_especificacion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.4 yes
     _FldNameList[11]   > general.r_analisis_especificacion.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[12]   > general.r_analisis_especificacion.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.6 yes
     _FldNameList[13]   > general.r_analisis_especificacion.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[14]   > general.r_analisis_especificacion.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[15]   > general.r_analisis_especificacion.id_unidad
"id_unidad" "id_unidad" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[16]   > general.r_analisis_especificacion.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[17]   > general.r_analisis_especificacion.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[18]   > general.r_analisis_especificacion.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
     _FldNameList[19]   > general.r_analisis_especificacion.valor
"valor" "valor" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes
     _FldNameList[20]   > "_<CALC>"
"getAnalisis()" "Analisis" "Analisis" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[21]   > "_<CALC>"
"getUnidad()" "Unidad" "Unidad" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[22]   > "_<CALC>"
"getEspecificacion()" "Especificacion" "Especificacion" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
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

  DEFINE VARIABLE hCont    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hTam     AS HANDLE     NO-UNDO.

  {get ContainerSource hCont}.

  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wResultAnal.w" THEN DO:
    hTam = DYNAMIC-FUNCTION('getSdoTamIndHandle' IN hCont).
  END.

  FIND LAST RowObjUpd NO-ERROR.
  
  /*add*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C". 

    IF VALID-HANDLE(hTam) THEN DO:
      ASSIGN rowObjUpd.id_empresa    = DYNAMIC-FUNCTION('columnValue' IN hTam, 'id_empresa')
             rowObjUpd.id_sucursal   = DYNAMIC-FUNCTION('columnValue' IN hTam, 'id_sucursal')
             rowObjUpd.id_tipotambor = DYNAMIC-FUNCTION('columnValue' IN hTam, 'id_tipotambor')
             rowObjUpd.nromov        = DYNAMIC-FUNCTION('columnValue' IN hTam, 'nromov')
             rowObjUpd.id_lote       = DYNAMIC-FUNCTION('columnValue' IN hTam, 'id_lote')
             rowObjUpd.anio_lote     = DYNAMIC-FUNCTION('columnValue' IN hTam, 'anio')
             .
    END.
  
    ASSIGN rowObjUpd.c_usuario = USERID("userdb")
           rowObjUpd.c_fecha   = TODAY
           rowObjUpd.c_hora    = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora,id_empresa,id_sucursal,id_tipotambor,nromov,id_lote,anio_lote".    
  END.
  
  /*update*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U". 
    ASSIGN rowObjUpd.c_usuario = USERID("userdb")
           rowObjUpd.c_fecha   = TODAY
           rowObjUpd.c_hora    = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".        
  END.
  
  /*delete*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D". 
  
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
         rowObject.Analisis = (getAnalisis())
         rowObject.Especificacion = (getEspecificacion())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAnalisis dTables  _DB-REQUIRED
FUNCTION getAnalisis RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST analisis_especificacion WHERE analisis_especificacion.id_analisis = rowObject.id_analisis NO-LOCK.
    cRet = analisis_especificacion.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEspecificacion dTables  _DB-REQUIRED
FUNCTION getEspecificacion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST especificacion_contrato WHERE especificacion_contrato.id_especificacion = rowObject.id_especificacion NO-LOCK.
    cRet = especificacion_contrato.descripcion.
  END.
  
  RETURN cRet.

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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR FIRST unidades_quimicas WHERE unidades_quimicas.id_unidad = rowObject.id_unidad NO-LOCK.
    cRet = unidades_quimicas.descripcion.
  END.
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

