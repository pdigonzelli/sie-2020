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
&Scoped-define INTERNAL-TABLES sobrante

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  cantidad c_fecha c_hora c_usuario fecha_elaboracion identificacion~
 id_articulo id_empresa id_envase id_lote id_sobrante id_sucursal~
 id_tipotambor id_tipotambor_sobrante kilos_tambor nromov nromov_sobrante~
 tipo_almacenamiento volumen cantidad_envases_recup cantidad_envases_nuevo
&Scoped-define ENABLED-FIELDS-IN-sobrante cantidad c_fecha c_hora c_usuario ~
fecha_elaboracion identificacion id_articulo id_empresa id_envase id_lote ~
id_sobrante id_sucursal id_tipotambor id_tipotambor_sobrante kilos_tambor ~
nromov nromov_sobrante tipo_almacenamiento volumen cantidad_envases_recup ~
cantidad_envases_nuevo 
&Scoped-Define DATA-FIELDS  cantidad c_fecha c_hora c_usuario fecha_elaboracion identificacion~
 id_articulo id_empresa id_envase id_lote id_sobrante id_sucursal~
 id_tipotambor id_tipotambor_sobrante kilos_tambor nromov nromov_sobrante~
 tipo_almacenamiento volumen Envase cantidad_envases_recup~
 cantidad_envases_nuevo
&Scoped-define DATA-FIELDS-IN-sobrante cantidad c_fecha c_hora c_usuario ~
fecha_elaboracion identificacion id_articulo id_empresa id_envase id_lote ~
id_sobrante id_sucursal id_tipotambor id_tipotambor_sobrante kilos_tambor ~
nromov nromov_sobrante tipo_almacenamiento volumen cantidad_envases_recup ~
cantidad_envases_nuevo 
&Scoped-Define MANDATORY-FIELDS  id_lote id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dsobrante.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH sobrante NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH sobrante NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main sobrante
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main sobrante


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      sobrante SCROLLING.
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
     _TblList          = "general.sobrante"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.sobrante.cantidad
"cantidad" "cantidad" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[2]   > general.sobrante.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[3]   > general.sobrante.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[4]   > general.sobrante.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[5]   > general.sobrante.fecha_elaboracion
"fecha_elaboracion" "fecha_elaboracion" ? ? "date" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[6]   > general.sobrante.identificacion
"identificacion" "identificacion" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[7]   > general.sobrante.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[8]   > general.sobrante.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[9]   > general.sobrante.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[10]   > general.sobrante.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[11]   > general.sobrante.id_sobrante
"id_sobrante" "id_sobrante" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[12]   > general.sobrante.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[13]   > general.sobrante.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[14]   > general.sobrante.id_tipotambor_sobrante
"id_tipotambor_sobrante" "id_tipotambor_sobrante" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[15]   > general.sobrante.kilos_tambor
"kilos_tambor" "kilos_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[16]   > general.sobrante.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[17]   > general.sobrante.nromov_sobrante
"nromov_sobrante" "nromov_sobrante" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[18]   > general.sobrante.tipo_almacenamiento
"tipo_almacenamiento" "tipo_almacenamiento" ? ? "logical" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[19]   > general.sobrante.volumen
"volumen" "volumen" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[20]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[21]   > general.sobrante.cantidad_envases_recup
"cantidad_envases_recup" "cantidad_envases_recup" ? ? "integer" ? ? ? ? ? ? yes ? no 24 yes
     _FldNameList[22]   > general.sobrante.cantidad_envases_nuevo
"cantidad_envases_nuevo" "cantidad_envases_nuevo" ? ? "integer" ? ? ? ? ? ? yes ? no 24.6 yes
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

  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iMov AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSob AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDesde AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta AS INTEGER    NO-UNDO.

  hSource = DYNAMIC-FUNCTION('getDataSource').
  ASSIGN iEmp = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_empresa')
         iSuc = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal')
         iTip = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_tipotambor')
         iNro = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nromov')
         iLot = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_lote')
         iArt = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_articulo')
         iAno = DYNAMIC-FUNCTION('columnValue' IN hSource, 'anio').


  /*agregar*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
    /*si ya existe un sobrante para ese lote, continuo a partir del ultimo tambor*/
    iMov = NEXT-VALUE(nromov).
    FIND LAST sobrante WHERE sobrante.id_empresa    = iEmp
                         AND sobrante.id_sucursal   = iSuc
                         AND sobrante.id_tipotambor = iTip
                         AND sobrante.nromov        = iNro
                       NO-LOCK NO-ERROR.
    IF AVAILABLE sobrante THEN DO:
      iSob = sobrante.id_sobrante + 1.
    END.
    ELSE DO:
      iSob = 1.
    END.

    ASSIGN rowObjUpd.id_empresa             = iEmp
           rowObjUpd.id_sucursal            = iSuc
           rowObjUpd.id_lote                = iLot
           rowObjUpd.id_tipotambor          = iTip
           rowObjUpd.nromov                 = iNro
           rowObjUpd.id_tipotambor_sobrante = 4
           rowObjUpd.nromov_sobrante        = iMov
           rowObjUpd.id_sobrante            = iSob
           rowObjUpd.c_usuario              = USERID("userdb")
           rowObjUpd.c_fecha                = TODAY
           rowObjUpd.c_hora                 = STRING(TIME,"HH:MM:SS").
    
    rowObjUpd.changedFields = rowObjUpd.changedFields + "id_empresa,id_sucursal,id_lote,id_tipotambor,nromov,id_tipotambor_sobrante,nromov_sobrante,id_sobrante,c_usuario,c_fecha,c_hora".
    
  END.
  
  /*modificar*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U". 
    ASSIGN rowObjUpd.c_usuario = USERID("userdb")
          rowObjUpd.c_fecha    = TODAY
          rowObjUpd.c_hora     = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + "c_usuario,c_fecha,c_hora".    
  END.
  
  /*eliminar*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D". 
    /*chequeo que los tambores no esten asociados a otro lote*/
    FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = sobrante.id_empresa 
                                    AND tambores_industria.id_sucursal   = sobrante.id_sucursal 
                                    AND tambores_industria.id_tipotambor = sobrante.id_tipotambor_sobrante 
                                    AND tambores_industria.nromov        = sobrante.nromov_sobrante 
                                    AND (tambores_industria.id_empresa_destino    <> 0 OR
                                         tambores_industria.id_sucursal_destino   <> 0 OR
                                         tambores_industria.id_tipotambor_destino <> 0 OR
                                         tambores_industria.nromov_destino        <> 0) 
                                  NO-LOCK NO-ERROR.                                            
    IF AVAILABLE tambores_industria THEN DO:
      MESSAGE "No puede borrar el sobrante porque ya esta siendo utilizado por otros lotes.".
      RETURN "ADM-ERROR".
    END.

   FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = rowObjUpd.id_empresa
                                   AND tambores_industria.id_sucursal   = rowObjUpd.id_sucursal
                                   AND tambores_industria.id_tipotambor = rowObjUpd.id_tipotambor_sobrante
                                   AND tambores_industria.nromov        = rowObjUpd.nromov_sobrante
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      iDesde = tambores_industria.id_tambor.
    END.
  
    FIND LAST tambores_industria WHERE tambores_industria.id_empresa    = rowObjUpd.id_empresa
                                   AND tambores_industria.id_sucursal   = rowObjUpd.id_sucursal
                                   AND tambores_industria.id_tipotambor = rowObjUpd.id_tipotambor_sobrante
                                   AND tambores_industria.nromov        = rowObjUpd.nromov_sobrante
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      iHasta = tambores_industria.id_tambor.
    END.

    RUN deleteDrumsFromLoteRango IN hLib (rowObjUpd.id_empresa, 
                                          rowObjUpd.id_sucursal, 
                                          rowObjUpd.id_tipotambor_sobrante,
                                          rowObjUpd.nromov_sobrante, 
                                          iDesde, 
                                          iHasta,
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
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.

  DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam AS INTEGER    NO-UNDO.

  hSource = DYNAMIC-FUNCTION('getDataSource').
  ASSIGN iEmp = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_empresa')
         iSuc = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal')
         iTip = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_tipotambor')
         iNro = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nromov').

  FIND LAST RowObjUpd NO-ERROR.
  
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
    FIND LAST sobrante WHERE sobrante.id_empresa      = iEmp
                         AND sobrante.id_sucursal     = iSuc
                         AND sobrante.id_tipotambor   = iTip
                         AND sobrante.nromov          = iNro
                         AND sobrante.nromov_sobrante <> rowObjUpd.nromov_sobrante
                       NO-LOCK NO-ERROR.
    IF AVAILABLE sobrante THEN DO:
      FIND LAST tambores_industria WHERE tambores_industria.id_empresa    = sobrante.id_empresa
                                     AND tambores_industria.id_sucursal   = sobrante.id_sucursal
                                     AND tambores_industria.id_tipotambor = sobrante.id_tipotambor_sobrante
                                     AND tambores_industria.nromov        = sobrante.nromov_sobrante
                                   NO-LOCK NO-ERROR.
      IF AVAILABLE tambores_industria THEN 
        iTam = tambores_industria.id_tambor + 1.
      ELSE
        iTam = 1.
    END.
    ELSE DO:
      iTam = 1.
    END.
    
    /*crear tambores sobrante*/
    RUN createDrumsInBatch IN hLib (rowObjUpd.id_empresa, 
                                    rowObjUpd.id_sucursal, 
                                    rowObjUpd.id_tipotambor_sobrante, 
                                    rowObjUpd.nromov_sobrante, 
                                    rowObjUpd.cantidad, 
                                    iTam, 
                                    iTam + rowObjUpd.cantidad - 1,  
                                    rowObjUpd.kilos,
                                    0, 
                                    2).
  
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

