&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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
&Scoped-define INTERNAL-TABLES orden_entrega agencias tipos_orden_entrega ~
despachantes vapores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_orden_entrega fecha semana_embarque pedido_fondos id_tipo_orden_entrega~
 id_agencia id_vapor id_despachante id_destino fecha_embarque fecha_arribo~
 id_lugdes observaciones cotizacion
&Scoped-define ENABLED-FIELDS-IN-orden_entrega id_orden_entrega fecha ~
semana_embarque pedido_fondos id_tipo_orden_entrega id_agencia id_vapor ~
id_despachante id_destino fecha_embarque fecha_arribo id_lugdes ~
observaciones cotizacion 
&Scoped-Define DATA-FIELDS  id_orden_entrega fecha abreviatura abreviatura-2 abreviatura-3 descripcion~
 semana_embarque pedido_fondos id_tipo_orden_entrega id_agencia id_vapor~
 id_despachante id_destino fecha_embarque fecha_arribo id_lugdes~
 observaciones cotizacion
&Scoped-define DATA-FIELDS-IN-orden_entrega id_orden_entrega fecha ~
semana_embarque pedido_fondos id_tipo_orden_entrega id_agencia id_vapor ~
id_despachante id_destino fecha_embarque fecha_arribo id_lugdes ~
observaciones cotizacion 
&Scoped-define DATA-FIELDS-IN-agencias abreviatura 
&Scoped-define DATA-FIELDS-IN-tipos_orden_entrega abreviatura-3 
&Scoped-define DATA-FIELDS-IN-despachantes descripcion 
&Scoped-define DATA-FIELDS-IN-vapores abreviatura-2 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.abreviatura-2 = vapores.abreviatura~
  rowObject.abreviatura-3 = tipos_orden_entrega.abreviatura
&Scoped-Define DATA-FIELD-DEFS "dNewOrdenEntrega.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH orden_entrega NO-LOCK, ~
      EACH agencias WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK, ~
      EACH tipos_orden_entrega OF orden_entrega NO-LOCK, ~
      EACH despachantes OF orden_entrega NO-LOCK, ~
      EACH vapores OF orden_entrega NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH orden_entrega NO-LOCK, ~
      EACH agencias WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK, ~
      EACH tipos_orden_entrega OF orden_entrega NO-LOCK, ~
      EACH despachantes OF orden_entrega NO-LOCK, ~
      EACH vapores OF orden_entrega NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main orden_entrega agencias ~
tipos_orden_entrega despachantes vapores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main orden_entrega
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main agencias
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main tipos_orden_entrega
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main despachantes
&Scoped-define FIFTH-TABLE-IN-QUERY-Query-Main vapores


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      orden_entrega, 
      agencias, 
      tipos_orden_entrega, 
      despachantes, 
      vapores SCROLLING.
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
     _TblList          = "general.orden_entrega,industria.agencias WHERE general.orden_entrega ...,industria.tipos_orden_entrega OF general.orden_entrega,industria.despachantes OF general.orden_entrega,comercial.vapores OF general.orden_entrega"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "general.agencias.id_agencia = general.orden_entrega.id_agencia"
     _FldNameList[1]   > general.orden_entrega.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[2]   > general.orden_entrega.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[3]   > general.agencias.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[4]   > comercial.vapores.abreviatura
"abreviatura" "abreviatura-2" ? ? "character" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[5]   > general.tipos_orden_entrega.abreviatura
"abreviatura" "abreviatura-3" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[6]   > general.despachantes.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[7]   > general.orden_entrega.semana_embarque
"semana_embarque" "semana_embarque" ? ? "integer" ? ? ? ? ? ? yes ? no 17.8 yes
     _FldNameList[8]   > general.orden_entrega.pedido_fondos
"pedido_fondos" "pedido_fondos" ? ? "logical" ? ? ? ? ? ? yes ? no 17.2 yes
     _FldNameList[9]   > general.orden_entrega.id_tipo_orden_entrega
"id_tipo_orden_entrega" "id_tipo_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[10]   > general.orden_entrega.id_agencia
"id_agencia" "id_agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[11]   > general.orden_entrega.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[12]   > general.orden_entrega.id_despachante
"id_despachante" "id_despachante" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[13]   > general.orden_entrega.id_destino
"id_destino" "id_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[14]   > general.orden_entrega.fecha_embarque
"fecha_embarque" "fecha_embarque" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[15]   > general.orden_entrega.fecha_arribo
"fecha_arribo" "fecha_arribo" ? ? "date" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[16]   > general.orden_entrega.id_lugdes
"id_lugdes" "id_lugdes" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[17]   > general.orden_entrega.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[18]   > general.orden_entrega.cotizacion
"cotizacion" "cotizacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
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

