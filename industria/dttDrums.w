&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          tablastemp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttDrums NO-UNDO LIKE drums.


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

DEFINE TEMP-TABLE ttPesoEspecifico
FIELD brix              AS DECIMAL  COLUMN-LABEL "Brix"
FIELD peso_especifico   AS DECIMAL  COLUMN-LABEL "Peso Especifico"
FIELD solido_soluto     AS DECIMAL  COLUMN-LABEL "Solido Soluto".

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
&Scoped-define INTERNAL-TABLES ttDrums

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  articulo
&Scoped-define ENABLED-FIELDS-IN-ttDrums articulo 
&Scoped-Define DATA-FIELDS  id_lote_prod id_tambor kilos id_articulo id_envase id_sucursal_ubicacion~
 id_locacion_ubicacion brix id_empresa id_sucursal id_tipotambor nromov~
 id_calidad fecha acidez sodio litros_tambor articulo
&Scoped-define DATA-FIELDS-IN-ttDrums id_lote_prod id_tambor kilos ~
id_articulo id_envase id_sucursal_ubicacion id_locacion_ubicacion brix ~
id_empresa id_sucursal id_tipotambor nromov id_calidad fecha acidez sodio ~
litros_tambor articulo 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttdrums.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ttDrums NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ttDrums NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ttDrums
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ttDrums


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantTamboresLote dTables  _DB-REQUIRED
FUNCTION getCantTamboresLote RETURNS INTEGER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getClaveTamborFromRowId dTables  _DB-REQUIRED
FUNCTION getClaveTamborFromRowId RETURNS CHARACTER
  (prTambor AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLitrosTambor dTables  _DB-REQUIRED
FUNCTION getLitrosTambor RETURNS DECIMAL
  (pdBrix AS DECIMAL, 
   pdPeso AS DECIMAL)  FORWARD.

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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumSelectedValues dTables  _DB-REQUIRED
FUNCTION getSumSelectedValues RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ttDrums SCROLLING.
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
   Temp-Tables and Buffers:
      TABLE: ttDrums T "?" NO-UNDO tablasTemp drums
   END-TABLES.
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
     _TblList          = "Temp-Tables.ttDrums"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ttDrums.id_lote_prod
"id_lote_prod" "id_lote_prod" ? ? "integer" ? ? ? ? ? ? no ? no 11.6 no
     _FldNameList[2]   > Temp-Tables.ttDrums.id_tambor
"id_tambor" "id_tambor" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 no
     _FldNameList[3]   > Temp-Tables.ttDrums.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[4]   > Temp-Tables.ttDrums.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[5]   > Temp-Tables.ttDrums.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[6]   > Temp-Tables.ttDrums.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[7]   > Temp-Tables.ttDrums.id_locacion_ubicacion
"id_locacion_ubicacion" "id_locacion_ubicacion" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[8]   > Temp-Tables.ttDrums.brix
"brix" "brix" ? ? "decimal" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[9]   > Temp-Tables.ttDrums.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? no ? no 9.8 no
     _FldNameList[10]   > Temp-Tables.ttDrums.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? no ? no 9.8 no
     _FldNameList[11]   > Temp-Tables.ttDrums.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? no ? no 13 no
     _FldNameList[12]   > Temp-Tables.ttDrums.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 no
     _FldNameList[13]   > Temp-Tables.ttDrums.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[14]   > Temp-Tables.ttDrums.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? no ? no 11.6 no
     _FldNameList[15]   > Temp-Tables.ttDrums.acidez
"acidez" "acidez" ? ? "decimal" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[16]   > Temp-Tables.ttDrums.sodio
"sodio" "sodio" ? ? "decimal" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[17]   > Temp-Tables.ttDrums.litros_tambor
"litros_tambor" "litros_tambor" ? ? "decimal" ? ? ? ? ? ? no ? no 9 no
     _FldNameList[18]   > Temp-Tables.ttDrums.articulo
"articulo" "articulo" ? "X(25)" "character" ? ? ? ? ? ? yes ? no 25 no
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF
  

/*
INPUT FROM pesoespecifico.txt.
REPEAT:
  CREATE ttPesoEspecifico.
  IMPORT ttPesoEspecifico.
END.
*/
INPUT CLOSE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTTDrums dTables  _DB-REQUIRED
PROCEDURE fillTTDrums :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha   AS DATE       NO-UNDO.


  DEFINE VARIABLE dBrix   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSodio  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcidez AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPE     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dLitros AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iTbsLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cAnal   AS CHARACTER  NO-UNDO.


  /*vacio la ttDrums*/
  FOR EACH ttDrums.
    DELETE ttDrums.
  END.
  
  IF NOT VALID-HANDLE(hLib) THEN RETURN.

  FOR EACH tambores_industria WHERE tambores_industria.fecha                >= DATE("01/01/2005")                                
                                /*AND tambores_industria.fecha                <= pdFecha + 3*/
                                AND tambores_industria.id_locacion_ubicacion = 4
                                AND tambores_industria.id_sucursal_ubicacion = piSucUbi
                                AND tambores_industria.id_tipotambor         = piTipoTam
                                AND tambores_industria.id_estado            <> 3
                                AND tambores_industria.id_estado            <> 8
                                AND (IF piTipoTam = 3 THEN tambores_industria.id_estado = 2 ELSE TRUE) /*mp para reproceso*/
                              NO-LOCK.
    
    cAnal   = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, tambores_industria.id_empresa,
                                                             tambores_industria.id_sucursal,
                                                             tambores_industria.id_tipotambor,
                                                             tambores_industria.nromov).
    ASSIGN dBrix   = DECIMAL(ENTRY(1, cAnal, CHR(1)))
           dSodio  = DECIMAL(ENTRY(2, cAnal, CHR(1)))
           dAcidez = DECIMAL(ENTRY(3, cAnal, CHR(1))).

    dLitros = DYNAMIC-FUNCTION('getLitrosTambor' IN hLib, tambores_industria.id_empresa,
                                                          tambores_industria.id_sucursal,
                                                          tambores_industria.id_tipotambor,
                                                          tambores_industria.nromov,
                                                          tambores_industria.kilos_tambor).

    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.

    CREATE ttDrums.
    ASSIGN ttDrums.id_empresa            = tambores_industria.id_empresa
           ttDrums.id_sucursal           = tambores_industria.id_sucursal
           ttDrums.id_tipotambor         = tambores_industria.id_tipotambor
           ttDrums.nromov                = tambores_industria.nromov
           ttDrums.id_lote_prod          = tambores_industria.id_lote
           ttDrums.id_tambor             = tambores_industria.id_tambor
           ttDrums.fecha                 = tambores_industria.fecha
           ttDrums.kilos                 = tambores_industria.kilos_tambor
           ttDrums.id_articulo           = tambores_industria.id_articulo
           ttDrums.id_calidad            = tambores_industria.id_calidad
           ttDrums.id_envase             = tambores_industria.id_envase
           ttDrums.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
           ttDrums.id_locacion_ubicacion = tambores_industria.id_locacion_ubicacion
           ttDrums.brix                  = dBrix
           ttDrums.sodio                 = dSodio
           ttDrums.acidez                = dAcidez
           ttDrums.litros_tambor         = dLitros
           ttDrums.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE".
    
  END.

  DYNAMIC-FUNCTION('openQuery').

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

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantTamboresLote dTables  _DB-REQUIRED
FUNCTION getCantTamboresLote RETURNS INTEGER
  (piEmp AS INTEGER, 
   piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE BUFFER ti FOR tambores_industria.

  FOR EACH ti WHERE ti.id_empresa    = piEmp
                AND ti.id_sucursal   = piSuc
                AND ti.id_tipotambor = piTip
                AND ti.nromov        = piNro
              NO-LOCK.
    i = i + 1.    
  END.


  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getClaveTamborFromRowId dTables  _DB-REQUIRED
FUNCTION getClaveTamborFromRowId RETURNS CHARACTER
  (prTambor AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE BUFFER buTam FOR ttDrums.

  FIND FIRST buTam WHERE ROWID(buTam) = prTambor
                   NO-LOCK NO-ERROR.
  IF AVAILABLE buTam THEN 
    cRet = STRING(buTam.id_empresa) + "," +
           STRING(buTam.id_sucursal) + "," +
           STRING(buTam.id_tipotambor) + "," +
           STRING(buTam.nromov) + "," +
           STRING(buTam.id_tambor).
  ELSE 
    cRet = "0,0,0,0,0".

  RELEASE buTam.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLitrosTambor dTables  _DB-REQUIRED
FUNCTION getLitrosTambor RETURNS DECIMAL
  (pdBrix AS DECIMAL, 
   pdPeso AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dPE AS DECIMAL    NO-UNDO.

  FOR LAST ttPesoEspecifico WHERE ttPesoEspecifico.brix <= pdBrix
                              AND ttPesoEspecifico.brix <> 0
                            BY ttPesoEspecifico.brix .
    dPE = ttPesoEspecifico.peso_especifico.
  END.
 
  RETURN pdPeso / dPE.

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
  DEFINE VARIABLE cRid    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo    AS INTEGER    NO-UNDO.


  DO iRow = 1 TO NUM-ENTRIES(pcRows) ON ERROR UNDO, LEAVE:
    cRid = ENTRY(iRow, pcRows).
    cKey = getClaveTamborFromRowId(TO-ROWID(cRid)).
    
    ASSIGN iEmp = INTEGER(ENTRY(1, cKey))
           iSuc = INTEGER(ENTRY(2, cKey))
           iTip = INTEGER(ENTRY(3, cKey))
           iNro = INTEGER(ENTRY(4, cKey))
           iTbo = INTEGER(ENTRY(5, cKey)).

    
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSumSelectedValues dTables  _DB-REQUIRED
FUNCTION getSumSelectedValues RETURNS CHARACTER
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
  
  DEFINE VARIABLE iKilos  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dLitros AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrix   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcid   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSodio  AS DECIMAL    NO-UNDO.

  DEFINE BUFFER ro FOR rowObject.

  cRet = STRING(0) + CHR(1) + STRING(0).
  DO iRow = 1 TO NUM-ENTRIES(pcRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow', INTEGER(ENTRY(iRow,pcRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,id_tambor').
    ASSIGN iEmp = INTEGER(ENTRY(2, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(3, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(4, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(5, cFields, CHR(1)))
           iTbo = INTEGER(ENTRY(6, cFields, CHR(1))).

    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".
    FIND FIRST ro WHERE ro.id_empresa    = iEmp
                    AND ro.id_sucursal   = iSuc
                    AND ro.id_tipotambor = iTip
                    AND ro.nromov        = iNro
                    AND ro.id_tambor     = iTbo
                  NO-LOCK NO-ERROR.
    IF AVAILABLE ro THEN DO:
      ASSIGN iKilos  = iKilos + ro.kilos
             dLitros = dLitros + ro.litros_tambor
             dBrix   = dBrix + (ro.brix * ro.litros_tambor)
             dAcid   = dAcid + (ro.acidez * ro.litros_tambor)
             dSodio  = dSodio + (ro.sodio * ro.litros_tambor).
    END.
  END.
  ASSIGN dLitros = ROUND(dLitros, 2)
         dBrix   = ROUND(dBrix / dLitros, 2)
         dAcid   = ROUND(dAcid / dLitros, 2)
         dSodio  = ROUND(dSodio / dLitros, 2).
  cRet = STRING(iKilos) + CHR(1) + 
         STRING(dLitros) + CHR(1) + 
         STRING(dBrix) + CHR(1) +
         STRING(dAcid) + CHR(1) + 
         STRING(dSodio).
  RETURN cRet.  
   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

