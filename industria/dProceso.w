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
&Scoped-define INTERNAL-TABLES proceso

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  abreviatura Anio c_fecha c_hora c_usuario descripcion fecha fecha_fin~
 fecha_inicio hora_comienzo hora_fin id_empresa id_proceso id_quimico_fin~
 id_quimico_inicio id_sucursal id_tipotambor nromov observaciones perdida-2~
 eficiencia tratamiento_fruta
&Scoped-define ENABLED-FIELDS-IN-proceso abreviatura Anio c_fecha c_hora ~
c_usuario descripcion fecha fecha_fin fecha_inicio hora_comienzo hora_fin ~
id_empresa id_proceso id_quimico_fin id_quimico_inicio id_sucursal ~
id_tipotambor nromov observaciones perdida-2 eficiencia tratamiento_fruta 
&Scoped-Define DATA-FIELDS  abreviatura Anio c_fecha c_hora c_usuario descripcion fecha fecha_fin~
 fecha_inicio hora_comienzo hora_fin id_empresa id_proceso id_quimico_fin~
 id_quimico_inicio id_sucursal id_tipotambor nromov observaciones Sucursal~
 perdida-2 eficiencia tratamiento_fruta
&Scoped-define DATA-FIELDS-IN-proceso abreviatura Anio c_fecha c_hora ~
c_usuario descripcion fecha fecha_fin fecha_inicio hora_comienzo hora_fin ~
id_empresa id_proceso id_quimico_fin id_quimico_inicio id_sucursal ~
id_tipotambor nromov observaciones perdida-2 eficiencia tratamiento_fruta 
&Scoped-Define MANDATORY-FIELDS  id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.perdida-2 = proceso.perdida
&Scoped-Define DATA-FIELD-DEFS "dproceso.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH proceso NO-LOCK ~
    BY proceso.Anio DESCENDING ~
       BY proceso.id_proceso DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH proceso NO-LOCK ~
    BY proceso.Anio DESCENDING ~
       BY proceso.id_proceso DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main proceso
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main proceso


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlAjuste dTables  _DB-REQUIRED
FUNCTION getControlAjuste RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosGrafico dTables  _DB-REQUIRED
FUNCTION getDatosGrafico RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos dTables  _DB-REQUIRED
FUNCTION getKilos RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilos400 dTables  _DB-REQUIRED
FUNCTION getKilos400 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextId dTables  _DB-REQUIRED
FUNCTION getNextId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPerdida dTables  _DB-REQUIRED
FUNCTION getPerdida RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSolidosResultado dTables  _DB-REQUIRED
FUNCTION getSolidosResultado RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSolidosTotales dTables  _DB-REQUIRED
FUNCTION getSolidosTotales RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      proceso SCROLLING.
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
     _TblList          = "general.proceso"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "general.proceso.Anio|no,industria.proceso.id_proceso|no"
     _FldNameList[1]   > general.proceso.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[2]   > general.proceso.Anio
"Anio" "Anio" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[3]   > general.proceso.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[4]   > general.proceso.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[5]   > general.proceso.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[6]   > general.proceso.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[7]   > general.proceso.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[8]   > general.proceso.fecha_fin
"fecha_fin" "fecha_fin" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[9]   > general.proceso.fecha_inicio
"fecha_inicio" "fecha_inicio" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[10]   > general.proceso.hora_comienzo
"hora_comienzo" "hora_comienzo" ? ? "character" ? ? ? ? ? ? yes ? no 14.4 yes
     _FldNameList[11]   > general.proceso.hora_fin
"hora_fin" "hora_fin" ? ? "character" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[12]   > general.proceso.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[13]   > general.proceso.id_proceso
"id_proceso" "id_proceso" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[14]   > general.proceso.id_quimico_fin
"id_quimico_fin" "id_quimico_fin" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[15]   > general.proceso.id_quimico_inicio
"id_quimico_inicio" "id_quimico_inicio" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[16]   > general.proceso.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.6 yes
     _FldNameList[17]   > general.proceso.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[18]   > general.proceso.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[19]   > general.proceso.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[20]   > "_<CALC>"
"getSucursal()" "Sucursal" "Sucursal" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[21]   > general.proceso.perdida
"perdida" "perdida-2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[22]   > general.proceso.eficiencia
"eficiencia" "eficiencia" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[23]   > general.proceso.tratamiento_fruta
"tratamiento_fruta" "tratamiento_fruta" ? ? "logical" ? ? ? ? ? ? yes ? no 15.8 yes
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
  
  /*add*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C". 
  
    ASSIGN rowObjUpd.nromov         = NEXT-VALUE(nromov)
           rowObjUpd.id_tipotambor  = 13
           rowObjUpd.fecha_inicio   = TODAY
           rowObjUpd.c_usuario      = USERID("userdb")
           rowObjUpd.c_fecha        = TODAY
           rowObjUpd.c_hora         = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_tipotambor,nromov,c_usuario,c_fecha,c_hora,fecha_inicio".    
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
    IF rowObjUpd.fecha_fin <> ? THEN DO:
      MESSAGE "Imposible Eliminar un Proceso que ya esta Cerrado" SKIP "Contacte al encargado de Sistemas" VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "ADM-ERROR".
    END.
  
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createControls dTables  _DB-REQUIRED
PROCEDURE createControls :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customAddRecord dTables  _DB-REQUIRED
PROCEDURE customAddRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iProc   AS INTEGER    NO-UNDO.

  hSource = DYNAMIC-FUNCTION('getDataSource').

  iProc = DYNAMIC-FUNCTION('getNextProceso' IN hSource).
  

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
         rowObject.Sucursal = (getSucursal())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_sucursalValidate dTables  _DB-REQUIRED
PROCEDURE id_sucursalValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValue AS CHARACTER  NO-UNDO.

  CASE INTEGER(pcValue):
    WHEN 95 THEN.
    WHEN 96 THEN.
    OTHERWISE 
      RETURN "Valor de Sucursal Invalido".
  END CASE.
  
  
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshRow dTables  _DB-REQUIRED
PROCEDURE refreshRow :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  DEFINE VARIABLE hCont    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE ibgcolor AS INTEGER    NO-UNDO.  

  {get ContainerSource hCont}.

  /*para frame fCargas*/
  IF VALID-HANDLE(hCont) AND (hCont:FILE-NAME MATCHES "*wProcesos.w") THEN DO:
    RUN fillProcesoGraph IN hCont.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFechaCierre dTables  _DB-REQUIRED
PROCEDURE setFechaCierre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plAct AS LOGICAL    NO-UNDO. /*true cierra false abre*/

  DEFINE VARIABLE iEstado AS INTEGER    NO-UNDO.
  
  
  FIND FIRST proceso WHERE proceso.id_empresa     = piEmp
                       AND proceso.id_sucursal    = piSuc
                       AND proceso.id_tipotambor  = piTip
                       AND proceso.nromov         = piNro
                     NO-ERROR.
  IF AVAILABLE proceso THEN DO:
    IF plAct THEN DO:
      ASSIGN proceso.fecha_fin = TODAY
             proceso.hora_fin  = STRING(TIME).
      /*cambiar estado a tambores para que aparezcan en stock*/
      FOR EACH r_proceso_tambor WHERE r_proceso_tambor.id_empresa_proceso     = proceso.id_empresa
                                  AND r_proceso_tambor.id_sucursal_proceso    = proceso.id_sucursal
                                  AND r_proceso_tambor.id_tipotambor_proceso  = proceso.id_tipotambor
                                  AND r_proceso_tambor.nromov_proceso         = proceso.nromov
                                NO-LOCK.
        iEstado = 7.
        IF r_proceso_tambor.id_tipotambor = 1 THEN
          iEstado = 5.
        
        RUN setEstadoTambor IN hLib (r_proceso_tambor.id_empresa,
                                     r_proceso_tambor.id_sucursal,
                                     r_proceso_tambor.id_tipotambor,
                                     r_proceso_tambor.nromov, 
                                     r_proceso_tambor.id_tambor,
                                     iEstado).
      END.

      /*cambio el campo locacion_ubicacion de los tambores volcados en las cargas
        del proceso para que desaparezcan del stock
        esto asi esta feo deberia pensar como implementarlo con funciones de libreerias*/

      FOR EACH cargas 
          WHERE cargas.nromov_proceso = proceso.nromov
          NO-LOCK.

        FOR EACH tambores_industria 
            WHERE tambores_industria.id_tipotambor_destino = cargas.id_tipotambor
              AND tambores_industria.nromov_destino        = cargas.nromov.
          
          ASSIGN tambores_industria.id_locacion_ubicacion = 10
                 tambores_industria.id_estado             = 9
                 .
          
        END. /*each tambores_industria*/
      END. /*each cargas*/

    END.
    ELSE DO:
      ASSIGN proceso.fecha_fin = ?
             proceso.hora_fin  = ?.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlAjuste dTables  _DB-REQUIRED
FUNCTION getControlAjuste RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST registro_post_cierre
      WHERE registro_post_cierre.id_empresa    = rowObject.id_empresa
        AND registro_post_cierre.id_sucursal   = rowObject.id_sucursal
        AND registro_post_cierre.id_tipotambor = rowObject.id_tipotambor
        AND registro_post_cierre.nromov        = rowObject.nromov
      NO-LOCK.
    cRet = registro_post_cierre.serial_control.
  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosGrafico dTables  _DB-REQUIRED
FUNCTION getDatosGrafico RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cGra AS CHARACTER  NO-UNDO.

  cGra = DYNAMIC-FUNCTION('getComposicionProceso' IN hLib, rowObject.id_empresa,
                                                           rowObject.id_sucursal,
                                                           rowObject.id_tipotambor,
                                                           rowObject.nromov).

  RETURN cGra.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos dTables  _DB-REQUIRED
FUNCTION getKilos RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKgs AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
/*
  FOR EACH cargas WHERE cargas.nromov_proceso = rowObject.nromov NO-LOCK. 
    dKgs = DYNAMIC-FUNCTION('getKilosFromAcidez' IN hLib, cargas.acidez_w_w, cargas.bx_20_20, cargas.litros).
    dRet = dRet + dKgs.
  END.
  */
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilos400 dTables  _DB-REQUIRED
FUNCTION getKilos400 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKgs AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  
  FIND CURRENT rowObject NO-ERROR.
  
  FOR EACH cargas WHERE cargas.nromov_proceso = rowObject.nromov NO-LOCK. 
    dKgs = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, cargas.bx_20_20, cargas.acidez_w_v, cargas.litros).
    dRet = dRet + dKgs.
  END.
    
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextId dTables  _DB-REQUIRED
FUNCTION getNextId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  FOR LAST proceso NO-LOCK.
    iRet = proceso.id_proceso.
  END.

  iRet = iRet + 1.
   

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPerdida dTables  _DB-REQUIRED
FUNCTION getPerdida RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  
  /*RETURN getSolidosTotales() - getSolidosResultado().*/

  RETURN 0.00.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSolidosResultado dTables  _DB-REQUIRED
FUNCTION getSolidosResultado RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.


  FIND CURRENT rowObject NO-ERROR.
  
  FOR EACH punto_envase WHERE punto_envase.nromov_proceso = rowObject.nromov NO-LOCK.
    dSol = punto_envase.sol_totales.
    dRet = dRet + dSol.    
  END.
  
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSolidosTotales dTables  _DB-REQUIRED
FUNCTION getSolidosTotales RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.
  
  FOR EACH cargas WHERE cargas.nromov_proceso = rowObject.nromov NO-LOCK. 
    dSol = DYNAMIC-FUNCTION('getSolidosSolubles' IN hLib, cargas.bx_correg, cargas.litros).
    dRet = dRet + dSol.    
  END.
  
  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  FOR FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_sucursal NO-LOCK.
    cRet = sucursales.abreviatura.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

