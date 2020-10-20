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
&Scoped-define INTERNAL-TABLES inspeccion_contenedores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  c_fecha c_hora c_usuario empresa_contratista evidencia_actividad fecha~
 fecha_recepcion identificacion_contenedor id_articulo id_inspeccion~
 id_sucursal_inspeccion id_sucursal_remito id_tipo_movsto inspector lugar~
 nro_precinto nro_remito observaciones olor_compartimiento~
 olor_compartimiento_desc parte_exterior parte_exterior_desc pat_chasis~
 pat_semi precinto_seguridad precinto_seguridad_desc~
 reacondicionar_container reacondicionar_container_desc~
 responsable_proveedor sanidad_estado sanidad_estado_desc tipo_contenedor
&Scoped-define ENABLED-FIELDS-IN-inspeccion_contenedores c_fecha c_hora ~
c_usuario empresa_contratista evidencia_actividad fecha fecha_recepcion ~
identificacion_contenedor id_articulo id_inspeccion id_sucursal_inspeccion ~
id_sucursal_remito id_tipo_movsto inspector lugar nro_precinto nro_remito ~
observaciones olor_compartimiento olor_compartimiento_desc parte_exterior ~
parte_exterior_desc pat_chasis pat_semi precinto_seguridad ~
precinto_seguridad_desc reacondicionar_container ~
reacondicionar_container_desc responsable_proveedor sanidad_estado ~
sanidad_estado_desc tipo_contenedor 
&Scoped-Define DATA-FIELDS  c_fecha c_hora c_usuario empresa_contratista evidencia_actividad fecha~
 fecha_recepcion identificacion_contenedor id_articulo id_inspeccion~
 id_sucursal_inspeccion id_sucursal_remito id_tipo_movsto inspector lugar~
 nro_precinto nro_remito observaciones olor_compartimiento~
 olor_compartimiento_desc parte_exterior parte_exterior_desc pat_chasis~
 pat_semi precinto_seguridad precinto_seguridad_desc~
 reacondicionar_container reacondicionar_container_desc~
 responsable_proveedor sanidad_estado sanidad_estado_desc tipo_contenedor
&Scoped-define DATA-FIELDS-IN-inspeccion_contenedores c_fecha c_hora ~
c_usuario empresa_contratista evidencia_actividad fecha fecha_recepcion ~
identificacion_contenedor id_articulo id_inspeccion id_sucursal_inspeccion ~
id_sucursal_remito id_tipo_movsto inspector lugar nro_precinto nro_remito ~
observaciones olor_compartimiento olor_compartimiento_desc parte_exterior ~
parte_exterior_desc pat_chasis pat_semi precinto_seguridad ~
precinto_seguridad_desc reacondicionar_container ~
reacondicionar_container_desc responsable_proveedor sanidad_estado ~
sanidad_estado_desc tipo_contenedor 
&Scoped-Define MANDATORY-FIELDS  id_sucursal_inspeccion
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dInspeccionesContenedores}.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH inspeccion_contenedores NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH inspeccion_contenedores NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main inspeccion_contenedores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main inspeccion_contenedores


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      inspeccion_contenedores SCROLLING.
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
     _TblList          = "general.inspeccion_contenedores"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.inspeccion_contenedores.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[2]   > general.inspeccion_contenedores.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[3]   > general.inspeccion_contenedores.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[4]   > general.inspeccion_contenedores.empresa_contratista
"empresa_contratista" "empresa_contratista" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[5]   > general.inspeccion_contenedores.evidencia_actividad
"evidencia_actividad" "evidencia_actividad" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[6]   > general.inspeccion_contenedores.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[7]   > general.inspeccion_contenedores.fecha_recepcion
"fecha_recepcion" "fecha_recepcion" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[8]   > general.inspeccion_contenedores.identificacion_contenedor
"identificacion_contenedor" "identificacion_contenedor" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[9]   > general.inspeccion_contenedores.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[10]   > general.inspeccion_contenedores.id_inspeccion
"id_inspeccion" "id_inspeccion" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[11]   > general.inspeccion_contenedores.id_sucursal_inspeccion
"id_sucursal_inspeccion" "id_sucursal_inspeccion" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[12]   > general.inspeccion_contenedores.id_sucursal_remito
"id_sucursal_remito" "id_sucursal_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[13]   > general.inspeccion_contenedores.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[14]   > general.inspeccion_contenedores.inspector
"inspector" "inspector" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[15]   > general.inspeccion_contenedores.lugar
"lugar" "lugar" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[16]   > general.inspeccion_contenedores.nro_precinto
"nro_precinto" "nro_precinto" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[17]   > general.inspeccion_contenedores.nro_remito
"nro_remito" "nro_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[18]   > general.inspeccion_contenedores.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 150 yes
     _FldNameList[19]   > general.inspeccion_contenedores.olor_compartimiento
"olor_compartimiento" "olor_compartimiento" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[20]   > general.inspeccion_contenedores.olor_compartimiento_desc
"olor_compartimiento_desc" "olor_compartimiento_desc" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[21]   > general.inspeccion_contenedores.parte_exterior
"parte_exterior" "parte_exterior" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[22]   > general.inspeccion_contenedores.parte_exterior_desc
"parte_exterior_desc" "parte_exterior_desc" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[23]   > general.inspeccion_contenedores.pat_chasis
"pat_chasis" "pat_chasis" ? ? "character" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[24]   > general.inspeccion_contenedores.pat_semi
"pat_semi" "pat_semi" ? ? "character" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[25]   > general.inspeccion_contenedores.precinto_seguridad
"precinto_seguridad" "precinto_seguridad" ? ? "logical" ? ? ? ? ? ? yes ? no 18 yes
     _FldNameList[26]   > general.inspeccion_contenedores.precinto_seguridad_desc
"precinto_seguridad_desc" "precinto_seguridad_desc" ? ? "character" ? ? ? ? ? ? yes ? no 23.8 yes
     _FldNameList[27]   > general.inspeccion_contenedores.reacondicionar_container
"reacondicionar_container" "reacondicionar_container" ? ? "logical" ? ? ? ? ? ? yes ? no 24 yes
     _FldNameList[28]   > general.inspeccion_contenedores.reacondicionar_container_desc
"reacondicionar_container_desc" "reacondicionar_container_desc" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[29]   > general.inspeccion_contenedores.responsable_proveedor
"responsable_proveedor" "responsable_proveedor" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[30]   > general.inspeccion_contenedores.sanidad_estado
"sanidad_estado" "sanidad_estado" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[31]   > general.inspeccion_contenedores.sanidad_estado_desc
"sanidad_estado_desc" "sanidad_estado_desc" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[32]   > general.inspeccion_contenedores.tipo_contenedor
"tipo_contenedor" "tipo_contenedor" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
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

