&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          repoindust       PROGRESS
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
&Scoped-define INTERNAL-TABLES stock_tambores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio anio_contrato anio_lote anio_semana articulo calidad cantidad_pedido~
 cantidad_total_of cliente envase estado fecha id_articulo id_calidad~
 id_cliente id_contrato id_empresa id_envase id_estado id_lote id_sucursal~
 id_sucursal_ubicacion id_tipotambor id_tipo_contrato item kilos kilos_400~
 kilos_total_of orden_fabricacion orden_reporte semana_desde semana_hasta~
 sucursal tambores
&Scoped-define ENABLED-FIELDS-IN-stock_tambores anio anio_contrato ~
anio_lote anio_semana articulo calidad cantidad_pedido cantidad_total_of ~
cliente envase estado fecha id_articulo id_calidad id_cliente id_contrato ~
id_empresa id_envase id_estado id_lote id_sucursal id_sucursal_ubicacion ~
id_tipotambor id_tipo_contrato item kilos kilos_400 kilos_total_of ~
orden_fabricacion orden_reporte semana_desde semana_hasta sucursal tambores 
&Scoped-Define DATA-FIELDS  anio anio_contrato anio_lote anio_semana articulo calidad cantidad_pedido~
 cantidad_total_of cliente envase estado fecha id_articulo id_calidad~
 id_cliente id_contrato id_empresa id_envase id_estado id_lote id_sucursal~
 id_sucursal_ubicacion id_tipotambor id_tipo_contrato item kilos kilos_400~
 kilos_total_of orden_fabricacion orden_reporte semana_desde semana_hasta~
 sucursal tambores
&Scoped-define DATA-FIELDS-IN-stock_tambores anio anio_contrato anio_lote ~
anio_semana articulo calidad cantidad_pedido cantidad_total_of cliente ~
envase estado fecha id_articulo id_calidad id_cliente id_contrato ~
id_empresa id_envase id_estado id_lote id_sucursal id_sucursal_ubicacion ~
id_tipotambor id_tipo_contrato item kilos kilos_400 kilos_total_of ~
orden_fabricacion orden_reporte semana_desde semana_hasta sucursal tambores 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dstocktambores.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH stock_tambores NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH stock_tambores NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main stock_tambores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main stock_tambores


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      stock_tambores SCROLLING.
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
     _TblList          = "repoindust.stock_tambores"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > repoindust.stock_tambores.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 5.4 yes
     _FldNameList[2]   > repoindust.stock_tambores.anio_contrato
"anio_contrato" "anio_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 yes
     _FldNameList[3]   > repoindust.stock_tambores.anio_lote
"anio_lote" "anio_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 3.8 yes
     _FldNameList[4]   > repoindust.stock_tambores.anio_semana
"anio_semana" "anio_semana" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[5]   > repoindust.stock_tambores.articulo
"articulo" "articulo" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[6]   > repoindust.stock_tambores.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[7]   > repoindust.stock_tambores.cantidad_pedido
"cantidad_pedido" "cantidad_pedido" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[8]   > repoindust.stock_tambores.cantidad_total_of
"cantidad_total_of" "cantidad_total_of" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[9]   > repoindust.stock_tambores.cliente
"cliente" "cliente" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[10]   > repoindust.stock_tambores.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[11]   > repoindust.stock_tambores.estado
"estado" "estado" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[12]   > repoindust.stock_tambores.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[13]   > repoindust.stock_tambores.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.2 yes
     _FldNameList[14]   > repoindust.stock_tambores.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 5 yes
     _FldNameList[15]   > repoindust.stock_tambores.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[16]   > repoindust.stock_tambores.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[17]   > repoindust.stock_tambores.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 4.2 yes
     _FldNameList[18]   > repoindust.stock_tambores.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 6.4 yes
     _FldNameList[19]   > repoindust.stock_tambores.id_estado
"id_estado" "id_estado" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[20]   > repoindust.stock_tambores.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 5.4 yes
     _FldNameList[21]   > repoindust.stock_tambores.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes
     _FldNameList[22]   > repoindust.stock_tambores.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.2 yes
     _FldNameList[23]   > repoindust.stock_tambores.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[24]   > repoindust.stock_tambores.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes
     _FldNameList[25]   > repoindust.stock_tambores.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 5 yes
     _FldNameList[26]   > repoindust.stock_tambores.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[27]   > repoindust.stock_tambores.kilos_400
"kilos_400" "kilos_400" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[28]   > repoindust.stock_tambores.kilos_total_of
"kilos_total_of" "kilos_total_of" ? ? "integer" ? ? ? ? ? ? yes ? no 15.4 yes
     _FldNameList[29]   > repoindust.stock_tambores.orden_fabricacion
"orden_fabricacion" "orden_fabricacion" ? ? "character" ? ? ? ? ? ? yes ? no 3 yes
     _FldNameList[30]   > repoindust.stock_tambores.orden_reporte
"orden_reporte" "orden_reporte" ? ? "integer" ? ? ? ? ? ? yes ? no 19.6 yes
     _FldNameList[31]   > repoindust.stock_tambores.semana_desde
"semana_desde" "semana_desde" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[32]   > repoindust.stock_tambores.semana_hasta
"semana_hasta" "semana_hasta" ? ? "integer" ? ? ? ? ? ? yes ? no 5.2 yes
     _FldNameList[33]   > repoindust.stock_tambores.sucursal
"sucursal" "sucursal" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[34]   > repoindust.stock_tambores.tambores
"tambores" "tambores" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
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

