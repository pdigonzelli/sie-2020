&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
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
&Scoped-define INTERNAL-TABLES liq_legajos liq_categorias

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_empresa_liq legajo apellido_nombre codigo_postal cuil domicilio~
 fecha_egreso fecha_ingreso fecha_inicio_temp fecha_nacimiento id_cargo~
 id_categoria id_centro_costo_liq id_contrato id_convenio id_motivo_egreso~
 id_sector_liq liquida localidad sexo tipo_liquidacion id_ccargo_liq~
 id_ccategoria_liq id_ccontrato_liq id_cconvenio_liq id_ccostos_liq~
 id_division legajo_rhpro toma_ccosto_legajo
&Scoped-define ENABLED-FIELDS-IN-liq_legajos id_empresa_liq legajo ~
apellido_nombre codigo_postal cuil domicilio fecha_egreso fecha_ingreso ~
fecha_inicio_temp fecha_nacimiento id_cargo id_categoria ~
id_centro_costo_liq id_contrato id_convenio id_motivo_egreso id_sector_liq ~
liquida localidad sexo tipo_liquidacion id_ccargo_liq id_ccategoria_liq ~
id_ccontrato_liq id_cconvenio_liq id_ccostos_liq id_division legajo_rhpro ~
toma_ccosto_legajo 
&Scoped-Define DATA-FIELDS  id_empresa_liq legajo apellido_nombre codigo_postal cuil domicilio~
 fecha_egreso fecha_ingreso fecha_inicio_temp fecha_nacimiento id_cargo~
 id_categoria id_centro_costo_liq id_contrato id_convenio id_motivo_egreso~
 id_sector_liq liquida localidad sexo tipo_liquidacion descripcion~
 id_ccargo_liq id_ccategoria_liq id_ccontrato_liq id_cconvenio_liq~
 id_ccostos_liq id_division legajo_rhpro toma_ccosto_legajo
&Scoped-define DATA-FIELDS-IN-liq_legajos id_empresa_liq legajo ~
apellido_nombre codigo_postal cuil domicilio fecha_egreso fecha_ingreso ~
fecha_inicio_temp fecha_nacimiento id_cargo id_categoria ~
id_centro_costo_liq id_contrato id_convenio id_motivo_egreso id_sector_liq ~
liquida localidad sexo tipo_liquidacion id_ccargo_liq id_ccategoria_liq ~
id_ccontrato_liq id_cconvenio_liq id_ccostos_liq id_division legajo_rhpro ~
toma_ccosto_legajo 
&Scoped-define DATA-FIELDS-IN-liq_categorias descripcion 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dliqlegajos.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH liq_legajos NO-LOCK, ~
      EACH liq_categorias OF liq_legajos OUTER-JOIN NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH liq_legajos NO-LOCK, ~
      EACH liq_categorias OF liq_legajos OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main liq_legajos liq_categorias
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main liq_legajos
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main liq_categorias


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      liq_legajos, 
      liq_categorias SCROLLING.
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
     _TblList          = "agricola.liq_legajos,agricola.liq_categorias OF agricola.liq_legajos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _FldNameList[1]   > agricola.liq_legajos.id_empresa_liq
"id_empresa_liq" "id_empresa_liq" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[2]   > agricola.liq_legajos.legajo
"legajo" "legajo" ? ">>>>>>>>9" "integer" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[3]   > agricola.liq_legajos.apellido_nombre
"apellido_nombre" "apellido_nombre" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[4]   > agricola.liq_legajos.codigo_postal
"codigo_postal" "codigo_postal" ? ? "character" ? ? ? ? ? ? yes ? no 13 yes ?
     _FldNameList[5]   > agricola.liq_legajos.cuil
"cuil" "cuil" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[6]   > agricola.liq_legajos.domicilio
"domicilio" "domicilio" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[7]   > agricola.liq_legajos.fecha_egreso
"fecha_egreso" "fecha_egreso" ? ? "date" ? ? ? ? ? ? yes ? no 13.2 yes ?
     _FldNameList[8]   > agricola.liq_legajos.fecha_ingreso
"fecha_ingreso" "fecha_ingreso" ? ? "date" ? ? ? ? ? ? yes ? no 13.6 yes ?
     _FldNameList[9]   > agricola.liq_legajos.fecha_inicio_temp
"fecha_inicio_temp" "fecha_inicio_temp" ? ? "date" ? ? ? ? ? ? yes ? no 17.6 yes ?
     _FldNameList[10]   > agricola.liq_legajos.fecha_nacimiento
"fecha_nacimiento" "fecha_nacimiento" ? ? "date" ? ? ? ? ? ? yes ? no 17.2 yes ?
     _FldNameList[11]   > agricola.liq_legajos.id_cargo
"id_cargo" "id_cargo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[12]   > agricola.liq_legajos.id_categoria
"id_categoria" "id_categoria" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[13]   > agricola.liq_legajos.id_centro_costo_liq
"id_centro_costo_liq" "id_centro_costo_liq" ? ">>>>>>>>>9" "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[14]   > agricola.liq_legajos.id_contrato
"id_contrato" "id_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[15]   > agricola.liq_legajos.id_convenio
"id_convenio" "id_convenio" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[16]   > agricola.liq_legajos.id_motivo_egreso
"id_motivo_egreso" "id_motivo_egreso" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[17]   > agricola.liq_legajos.id_sector_liq
"id_sector_liq" "id_sector_liq" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[18]   > agricola.liq_legajos.liquida
"liquida" "liquida" ? ? "logical" ? ? ? ? ? ? yes ? no 6.8 yes ?
     _FldNameList[19]   > agricola.liq_legajos.localidad
"localidad" "localidad" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[20]   > agricola.liq_legajos.sexo
"sexo" "sexo" ? ? "character" ? ? ? ? ? ? yes ? no 4.8 yes ?
     _FldNameList[21]   > agricola.liq_legajos.tipo_liquidacion
"tipo_liquidacion" "tipo_liquidacion" ? ? "character" ? ? ? ? ? ? yes ? no 7.6 yes ?
     _FldNameList[22]   > agricola.liq_categorias.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? no ? no 30 yes ?
     _FldNameList[23]   > agricola.liq_legajos.id_ccargo_liq
"id_ccargo_liq" "id_ccargo_liq" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes ?
     _FldNameList[24]   > agricola.liq_legajos.id_ccategoria_liq
"id_ccategoria_liq" "id_ccategoria_liq" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[25]   > agricola.liq_legajos.id_ccontrato_liq
"id_ccontrato_liq" "id_ccontrato_liq" ? ? "integer" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[26]   > agricola.liq_legajos.id_cconvenio_liq
"id_cconvenio_liq" "id_cconvenio_liq" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[27]   > agricola.liq_legajos.id_ccostos_liq
"id_ccostos_liq" "id_ccostos_liq" ? ">>>>>>>>>>9" "int64" ? ? ? ? ? ? yes ? no 13.2 yes ?
     _FldNameList[28]   > agricola.liq_legajos.id_division
"id_division" "id_division" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[29]   > agricola.liq_legajos.legajo_rhpro
"legajo_rhpro" "legajo_rhpro" ? ">>>>>>>>9" "int64" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[30]   > agricola.liq_legajos.toma_ccosto_legajo
"toma_ccosto_legajo" "toma_ccosto_legajo" ? ? "logical" ? ? ? ? ? ? yes ? no 19 yes ?
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
FOR EACH RowObjUpd WHERE rowMod = "A" OR rowMod = "C" OR rowMod = "U":
    /*IF rowObjUpd.id_empresa < 1000 THEN
        RETURN "No puede modificar datos de esta empresa". */
END.
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

