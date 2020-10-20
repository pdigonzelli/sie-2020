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
&Scoped-Define ENABLED-FIELDS  anio cerrado cheque cotizacion c_fecha c_hora c_usuario fecha fecha_arribo~
 fecha_embarque id_agencia id_despachante id_destino id_estado id_lugdes~
 id_orden_entrega id_tipo_orden_entrega id_vapor observaciones pedido_fondos~
 semana_embarque
&Scoped-define ENABLED-FIELDS-IN-orden_entrega anio cerrado cheque ~
cotizacion c_fecha c_hora c_usuario fecha fecha_arribo fecha_embarque ~
id_agencia id_despachante id_destino id_estado id_lugdes id_orden_entrega ~
id_tipo_orden_entrega id_vapor observaciones pedido_fondos semana_embarque 
&Scoped-Define DATA-FIELDS  anio cerrado cheque cotizacion c_fecha c_hora c_usuario fecha fecha_arribo~
 fecha_embarque id_agencia id_despachante id_destino id_estado id_lugdes~
 id_orden_entrega id_tipo_orden_entrega id_vapor observaciones pedido_fondos~
 semana_embarque abreviatura descripcion abreviatura-2 descripcion-2~
 descripcion-3 descripcion-4 abreviatura-3
&Scoped-define DATA-FIELDS-IN-orden_entrega anio cerrado cheque cotizacion ~
c_fecha c_hora c_usuario fecha fecha_arribo fecha_embarque id_agencia ~
id_despachante id_destino id_estado id_lugdes id_orden_entrega ~
id_tipo_orden_entrega id_vapor observaciones pedido_fondos semana_embarque 
&Scoped-define DATA-FIELDS-IN-agencias abreviatura descripcion 
&Scoped-define DATA-FIELDS-IN-tipos_orden_entrega abreviatura-2 ~
descripcion-2 
&Scoped-define DATA-FIELDS-IN-despachantes descripcion-3 
&Scoped-define DATA-FIELDS-IN-vapores descripcion-4 abreviatura-3 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.abreviatura-2 = tipos_orden_entrega.abreviatura~
  rowObject.descripcion-2 = tipos_orden_entrega.descripcion~
  rowObject.descripcion-3 = despachantes.descripcion~
  rowObject.descripcion-4 = vapores.descripcion~
  rowObject.abreviatura-3 = vapores.abreviatura
&Scoped-Define DATA-FIELD-DEFS "dordenentregaindustria.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH orden_entrega NO-LOCK, ~
      EACH agencias WHERE agencias.id_agencia = orden_entrega.id_agencia OUTER-JOIN NO-LOCK, ~
      EACH tipos_orden_entrega OF orden_entrega OUTER-JOIN NO-LOCK, ~
      EACH despachantes OF orden_entrega OUTER-JOIN NO-LOCK, ~
      EACH vapores OF orden_entrega OUTER-JOIN NO-LOCK ~
    BY orden_entrega.fecha DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH orden_entrega NO-LOCK, ~
      EACH agencias WHERE agencias.id_agencia = orden_entrega.id_agencia OUTER-JOIN NO-LOCK, ~
      EACH tipos_orden_entrega OF orden_entrega OUTER-JOIN NO-LOCK, ~
      EACH despachantes OF orden_entrega OUTER-JOIN NO-LOCK, ~
      EACH vapores OF orden_entrega OUTER-JOIN NO-LOCK ~
    BY orden_entrega.fecha DESCENDING INDEXED-REPOSITION.
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
     _TblOptList       = ", OUTER, OUTER, OUTER, OUTER"
     _OrdList          = "general.orden_entrega.fecha|no"
     _JoinCode[2]      = "general.agencias.id_agencia = general.orden_entrega.id_agencia"
     _FldNameList[1]   > general.orden_entrega.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > general.orden_entrega.cerrado
"cerrado" "cerrado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[3]   > general.orden_entrega.cheque
"cheque" "cheque" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[4]   > general.orden_entrega.cotizacion
"cotizacion" "cotizacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[5]   > general.orden_entrega.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[6]   > general.orden_entrega.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[7]   > general.orden_entrega.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[8]   > general.orden_entrega.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[9]   > general.orden_entrega.fecha_arribo
"fecha_arribo" "fecha_arribo" ? ? "date" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[10]   > general.orden_entrega.fecha_embarque
"fecha_embarque" "fecha_embarque" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[11]   > general.orden_entrega.id_agencia
"id_agencia" "id_agencia" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[12]   > general.orden_entrega.id_despachante
"id_despachante" "id_despachante" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[13]   > general.orden_entrega.id_destino
"id_destino" "id_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[14]   > general.orden_entrega.id_estado
"id_estado" "id_estado" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[15]   > general.orden_entrega.id_lugdes
"id_lugdes" "id_lugdes" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[16]   > general.orden_entrega.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[17]   > general.orden_entrega.id_tipo_orden_entrega
"id_tipo_orden_entrega" "id_tipo_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[18]   > general.orden_entrega.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[19]   > general.orden_entrega.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[20]   > general.orden_entrega.pedido_fondos
"pedido_fondos" "pedido_fondos" ? ? "logical" ? ? ? ? ? ? yes ? no 17.2 yes
     _FldNameList[21]   > general.orden_entrega.semana_embarque
"semana_embarque" "semana_embarque" ? ? "integer" ? ? ? ? ? ? yes ? no 17.8 yes
     _FldNameList[22]   > general.agencias.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[23]   > general.agencias.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[24]   > general.tipos_orden_entrega.abreviatura
"abreviatura" "abreviatura-2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[25]   > general.tipos_orden_entrega.descripcion
"descripcion" "descripcion-2" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[26]   > general.despachantes.descripcion
"descripcion" "descripcion-3" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[27]   > comercial.vapores.descripcion
"descripcion" "descripcion-4" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[28]   > comercial.vapores.abreviatura
"abreviatura" "abreviatura-3" ? ? "character" ? ? ? ? ? ? no ? no 10.8 yes
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

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A" OR RowObjUpd.RowMod = "C".

  ASSIGN RowObjUpd.anio                  = YEAR(TODAY)
         RowObjUpd.c_fecha               = TODAY
         RowObjUpd.c_usuario             = USERID("userdb")
         RowObjUpd.c_hora                = STRING(TIME,"HH:MM:SS").

  RowObjUpd.changedFields = RowObjUpd.changedFields + ",anio,c_fecha,c_usuario,c_hora".
END.


FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "U".

    FIND FIRST packing_list WHERE packing_list.id_orden_entrega = RowObjUpd.id_orden_entrega 
                            NO-LOCK NO-ERROR.
    IF AVAILABLE packing_list THEN DO:
        
        ASSIGN RowObjUpd.fecha_embarque    = ventas.packing_list.fecha_salida_vapor
               RowObjUpd.fecha_arribo      = ventas.packing_list.fecha_salida_vapor + ventas.packing_list.dias_transito.
        
        MESSAGE "Se actualizo la fecha de embarque y fecha estimada de arribo. " 
                ventas.packing_list.fecha_salida_vapor
                ventas.packing_list.fecha_salida_vapor + ventas.packing_list.dias_transito
                VIEW-AS ALERT-BOX.
                
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

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D".
    FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = RowObjUpd.id_orden_entrega.
        MESSAGE "Estoy borrando las partes de la OE " items_orden_entrega.ITEM_oe VIEW-AS ALERT-BOX.
        DELETE items_orden_entrega.
    END.
END.

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

