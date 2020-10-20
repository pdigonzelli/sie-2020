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
&Scoped-define INTERNAL-TABLES release_delivery items_release_delivery

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  id_release_delivery id_sucursal_ubicacion calidad envase estado~
 fecha_confirmacion_entrega fecha_creacion fecha_entrega fecha_envio~
 fecha_limite_frio id_articulo id_calidad id_cliente id_envase~
 numero_release observaciones unidad anio_contrato cantidad contenedor~
 id_contrato id_empresa id_envase-2 id_lote id_lote_deposito~
 id_release_delivery-2 id_sucursal id_sucursal_ubicacion-2 id_tipotambor~
 id_tipo_contrato id_unidad_medida id_vapor item_contrato~
 item_release_delivery nromov tambores
&Scoped-define DATA-FIELDS-IN-release_delivery id_release_delivery ~
id_sucursal_ubicacion calidad envase estado fecha_confirmacion_entrega ~
fecha_creacion fecha_entrega fecha_envio fecha_limite_frio id_articulo ~
id_calidad id_cliente id_envase numero_release observaciones unidad 
&Scoped-define DATA-FIELDS-IN-items_release_delivery anio_contrato cantidad ~
contenedor id_contrato id_empresa id_envase-2 id_lote id_lote_deposito ~
id_release_delivery-2 id_sucursal id_sucursal_ubicacion-2 id_tipotambor ~
id_tipo_contrato id_unidad_medida id_vapor item_contrato ~
item_release_delivery nromov tambores 
&Scoped-Define MANDATORY-FIELDS  id_sucursal_ubicacion-2
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.id_envase-2 = items_release_delivery.id_envase~
  rowObject.id_release_delivery-2 = items_release_delivery.id_release_delivery~
  rowObject.id_sucursal_ubicacion-2 = items_release_delivery.id_sucursal_ubicacion
&Scoped-Define DATA-FIELD-DEFS "sdoreleasecontratos.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH release_delivery NO-LOCK, ~
      EACH items_release_delivery OF release_delivery NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH release_delivery NO-LOCK, ~
      EACH items_release_delivery OF release_delivery NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main release_delivery ~
items_release_delivery
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main release_delivery
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main items_release_delivery


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      release_delivery, 
      items_release_delivery SCROLLING.
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
     _TblList          = "general.release_delivery,general.items_release_delivery OF general.release_delivery"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.release_delivery.id_release_delivery
"id_release_delivery" "id_release_delivery" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[2]   > general.release_delivery.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[3]   > general.release_delivery.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? no ? no 40 yes
     _FldNameList[4]   > general.release_delivery.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? no ? no 40 yes
     _FldNameList[5]   > general.release_delivery.estado
"estado" "estado" ? ? "integer" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[6]   > general.release_delivery.fecha_confirmacion_entrega
"fecha_confirmacion_entrega" "fecha_confirmacion_entrega" ? ? "date" ? ? ? ? ? ? no ? no 26.8 yes
     _FldNameList[7]   > general.release_delivery.fecha_creacion
"fecha_creacion" "fecha_creacion" ? ? "date" ? ? ? ? ? ? no ? no 15 yes
     _FldNameList[8]   > general.release_delivery.fecha_entrega
"fecha_entrega" "fecha_entrega" ? ? "date" ? ? ? ? ? ? no ? no 14 yes
     _FldNameList[9]   > general.release_delivery.fecha_envio
"fecha_envio" "fecha_envio" ? ? "date" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[10]   > general.release_delivery.fecha_limite_frio
"fecha_limite_frio" "fecha_limite_frio" ? ? "date" ? ? ? ? ? ? no ? no 16 yes
     _FldNameList[11]   > general.release_delivery.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[12]   > general.release_delivery.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[13]   > general.release_delivery.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[14]   > general.release_delivery.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[15]   > general.release_delivery.numero_release
"numero_release" "numero_release" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[16]   > general.release_delivery.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? no ? no 250 yes
     _FldNameList[17]   > general.release_delivery.unidad
"unidad" "unidad" ? ? "character" ? ? ? ? ? ? no ? no 15 yes
     _FldNameList[18]   > general.items_release_delivery.anio_contrato
"anio_contrato" "anio_contrato" ? ? "integer" ? ? ? ? ? ? no ? no 4.8 yes
     _FldNameList[19]   > general.items_release_delivery.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[20]   > general.items_release_delivery.contenedor
"contenedor" "contenedor" ? ? "character" ? ? ? ? ? ? no ? no 20 yes
     _FldNameList[21]   > general.items_release_delivery.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[22]   > general.items_release_delivery.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[23]   > general.items_release_delivery.id_envase
"id_envase" "id_envase-2" ? ? "integer" ? ? ? ? ? ? no ? no 11 yes
     _FldNameList[24]   > general.items_release_delivery.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[25]   > general.items_release_delivery.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? no ? no 40 yes
     _FldNameList[26]   > general.items_release_delivery.id_release_delivery
"id_release_delivery" "id_release_delivery-2" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[27]   > general.items_release_delivery.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? no ? no 7.6 yes
     _FldNameList[28]   > general.items_release_delivery.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion-2" ? ? "integer" ? ? ? ? ? ? no ? yes 21 yes
     _FldNameList[29]   > general.items_release_delivery.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? no ? no 6.8 yes
     _FldNameList[30]   > general.items_release_delivery.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[31]   > general.items_release_delivery.id_unidad_medida
"id_unidad_medida" "id_unidad_medida" ? ? "integer" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[32]   > general.items_release_delivery.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[33]   > general.items_release_delivery.item_contrato
"item_contrato" "item_contrato" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[34]   > general.items_release_delivery.item_release_delivery
"item_release_delivery" "item_release_delivery" ? ? "integer" ? ? ? ? ? ? no ? no 20.2 yes
     _FldNameList[35]   > general.items_release_delivery.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[36]   > general.items_release_delivery.tambores
"tambores" "tambores" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
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

