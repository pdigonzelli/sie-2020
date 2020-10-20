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
&Scoped-define INTERNAL-TABLES tareas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  abreviatura caracteristica c_fecha c_hora c_usuario decreto descripcion~
 factor id_centro_costo id_concepto1 id_concepto2 id_concepto3 id_concepto4~
 id_concepto5 id_concepto6 id_concepto7 id_concepto8 id_concepto9~
 id_concepto10 id_concepto11 id_concepto12 id_concepto13 id_concepto14~
 id_concepto15 id_concepto16 id_concepto17 id_concepto18 id_concepto19~
 id_concepto20 id_grupo_tarea id_sector id_tarea id_tipo_tarea valor1 valor2~
 valor3 valor4 valor5 valor6 valor7 valor8 valor9 valor10 valor11 valor12~
 valor13 valor14 valor15 valor16 valor17 valor18 valor19 valor20~
 valor_normal con_restriccion_cupo vigente id_grupo_consulta id_tipo_dato
&Scoped-define ENABLED-FIELDS-IN-tareas abreviatura caracteristica c_fecha ~
c_hora c_usuario decreto descripcion factor id_centro_costo id_concepto1 ~
id_concepto2 id_concepto3 id_concepto4 id_concepto5 id_concepto6 ~
id_concepto7 id_concepto8 id_concepto9 id_concepto10 id_concepto11 ~
id_concepto12 id_concepto13 id_concepto14 id_concepto15 id_concepto16 ~
id_concepto17 id_concepto18 id_concepto19 id_concepto20 id_grupo_tarea ~
id_sector id_tarea id_tipo_tarea valor1 valor2 valor3 valor4 valor5 valor6 ~
valor7 valor8 valor9 valor10 valor11 valor12 valor13 valor14 valor15 ~
valor16 valor17 valor18 valor19 valor20 valor_normal con_restriccion_cupo ~
vigente id_grupo_consulta id_tipo_dato 
&Scoped-Define DATA-FIELDS  abreviatura caracteristica c_fecha c_hora c_usuario decreto descripcion~
 factor id_centro_costo id_concepto1 id_concepto2 id_concepto3 id_concepto4~
 id_concepto5 id_concepto6 id_concepto7 id_concepto8 id_concepto9~
 id_concepto10 id_concepto11 id_concepto12 id_concepto13 id_concepto14~
 id_concepto15 id_concepto16 id_concepto17 id_concepto18 id_concepto19~
 id_concepto20 id_grupo_tarea id_sector id_tarea id_tipo_tarea valor1 valor2~
 valor3 valor4 valor5 valor6 valor7 valor8 valor9 valor10 valor11 valor12~
 valor13 valor14 valor15 valor16 valor17 valor18 valor19 valor20~
 valor_normal con_restriccion_cupo vigente id_grupo_consulta id_tipo_dato
&Scoped-define DATA-FIELDS-IN-tareas abreviatura caracteristica c_fecha ~
c_hora c_usuario decreto descripcion factor id_centro_costo id_concepto1 ~
id_concepto2 id_concepto3 id_concepto4 id_concepto5 id_concepto6 ~
id_concepto7 id_concepto8 id_concepto9 id_concepto10 id_concepto11 ~
id_concepto12 id_concepto13 id_concepto14 id_concepto15 id_concepto16 ~
id_concepto17 id_concepto18 id_concepto19 id_concepto20 id_grupo_tarea ~
id_sector id_tarea id_tipo_tarea valor1 valor2 valor3 valor4 valor5 valor6 ~
valor7 valor8 valor9 valor10 valor11 valor12 valor13 valor14 valor15 ~
valor16 valor17 valor18 valor19 valor20 valor_normal con_restriccion_cupo ~
vigente id_grupo_consulta id_tipo_dato 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.id_concepto1 = tareas.id_concepto[1]~
  rowObject.id_concepto2 = tareas.id_concepto[2]~
  rowObject.id_concepto3 = tareas.id_concepto[3]~
  rowObject.id_concepto4 = tareas.id_concepto[4]~
  rowObject.id_concepto5 = tareas.id_concepto[5]~
  rowObject.id_concepto6 = tareas.id_concepto[6]~
  rowObject.id_concepto7 = tareas.id_concepto[7]~
  rowObject.id_concepto8 = tareas.id_concepto[8]~
  rowObject.id_concepto9 = tareas.id_concepto[9]~
  rowObject.id_concepto10 = tareas.id_concepto[10]~
  rowObject.id_concepto11 = tareas.id_concepto[11]~
  rowObject.id_concepto12 = tareas.id_concepto[12]~
  rowObject.id_concepto13 = tareas.id_concepto[13]~
  rowObject.id_concepto14 = tareas.id_concepto[14]~
  rowObject.id_concepto15 = tareas.id_concepto[15]~
  rowObject.id_concepto16 = tareas.id_concepto[16]~
  rowObject.id_concepto17 = tareas.id_concepto[17]~
  rowObject.id_concepto18 = tareas.id_concepto[18]~
  rowObject.id_concepto19 = tareas.id_concepto[19]~
  rowObject.id_concepto20 = tareas.id_concepto[20]~
  rowObject.valor1 = tareas.valor[1]  rowObject.valor2 = tareas.valor[2]~
  rowObject.valor3 = tareas.valor[3]  rowObject.valor4 = tareas.valor[4]~
  rowObject.valor5 = tareas.valor[5]  rowObject.valor6 = tareas.valor[6]~
  rowObject.valor7 = tareas.valor[7]  rowObject.valor8 = tareas.valor[8]~
  rowObject.valor9 = tareas.valor[9]  rowObject.valor10 = tareas.valor[10]~
  rowObject.valor11 = tareas.valor[11]  rowObject.valor12 = tareas.valor[12]~
  rowObject.valor13 = tareas.valor[13]  rowObject.valor14 = tareas.valor[14]~
  rowObject.valor15 = tareas.valor[15]  rowObject.valor16 = tareas.valor[16]~
  rowObject.valor17 = tareas.valor[17]  rowObject.valor18 = tareas.valor[18]~
  rowObject.valor19 = tareas.valor[19]  rowObject.valor20 = tareas.valor[20]
&Scoped-Define DATA-FIELD-DEFS "dtareas.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH tareas NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH tareas NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main tareas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main tareas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      tareas SCROLLING.
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
     _TblList          = "agricola.tareas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > agricola.tareas.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[2]   > agricola.tareas.caracteristica
"caracteristica" "caracteristica" ? ? "logical" ? ? ? ? ? ? yes ? no 12.6 yes ?
     _FldNameList[3]   > agricola.tareas.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[4]   > agricola.tareas.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[5]   > agricola.tareas.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[6]   > agricola.tareas.decreto
"decreto" "decreto" ? ? "logical" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[7]   > agricola.tareas.descripcion
"descripcion" "descripcion" ? "X(100)" "character" ? ? ? ? ? ? yes ? no 100 yes ?
     _FldNameList[8]   > agricola.tareas.factor
"factor" "factor" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[9]   > agricola.tareas.id_centro_costo
"id_centro_costo" "id_centro_costo" ? ? "character" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[10]   > agricola.tareas.id_concepto[1]
"id_concepto[1]" "id_concepto1" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[11]   > agricola.tareas.id_concepto[2]
"id_concepto[2]" "id_concepto2" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[12]   > agricola.tareas.id_concepto[3]
"id_concepto[3]" "id_concepto3" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[13]   > agricola.tareas.id_concepto[4]
"id_concepto[4]" "id_concepto4" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[14]   > agricola.tareas.id_concepto[5]
"id_concepto[5]" "id_concepto5" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[15]   > agricola.tareas.id_concepto[6]
"id_concepto[6]" "id_concepto6" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[16]   > agricola.tareas.id_concepto[7]
"id_concepto[7]" "id_concepto7" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[17]   > agricola.tareas.id_concepto[8]
"id_concepto[8]" "id_concepto8" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[18]   > agricola.tareas.id_concepto[9]
"id_concepto[9]" "id_concepto9" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[19]   > agricola.tareas.id_concepto[10]
"id_concepto[10]" "id_concepto10" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[20]   > agricola.tareas.id_concepto[11]
"id_concepto[11]" "id_concepto11" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[21]   > agricola.tareas.id_concepto[12]
"id_concepto[12]" "id_concepto12" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[22]   > agricola.tareas.id_concepto[13]
"id_concepto[13]" "id_concepto13" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[23]   > agricola.tareas.id_concepto[14]
"id_concepto[14]" "id_concepto14" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[24]   > agricola.tareas.id_concepto[15]
"id_concepto[15]" "id_concepto15" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[25]   > agricola.tareas.id_concepto[16]
"id_concepto[16]" "id_concepto16" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[26]   > agricola.tareas.id_concepto[17]
"id_concepto[17]" "id_concepto17" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[27]   > agricola.tareas.id_concepto[18]
"id_concepto[18]" "id_concepto18" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[28]   > agricola.tareas.id_concepto[19]
"id_concepto[19]" "id_concepto19" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[29]   > agricola.tareas.id_concepto[20]
"id_concepto[20]" "id_concepto20" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[30]   > agricola.tareas.id_grupo_tarea
"id_grupo_tarea" "id_grupo_tarea" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ?
     _FldNameList[31]   > agricola.tareas.id_sector
"id_sector" "id_sector" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[32]   > agricola.tareas.id_tarea
"id_tarea" "id_tarea" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ?
     _FldNameList[33]   > agricola.tareas.id_tipo_tarea
"id_tipo_tarea" "id_tipo_tarea" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ?
     _FldNameList[34]   > agricola.tareas.valor[1]
"valor[1]" "valor1" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[35]   > agricola.tareas.valor[2]
"valor[2]" "valor2" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[36]   > agricola.tareas.valor[3]
"valor[3]" "valor3" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[37]   > agricola.tareas.valor[4]
"valor[4]" "valor4" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[38]   > agricola.tareas.valor[5]
"valor[5]" "valor5" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[39]   > agricola.tareas.valor[6]
"valor[6]" "valor6" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[40]   > agricola.tareas.valor[7]
"valor[7]" "valor7" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[41]   > agricola.tareas.valor[8]
"valor[8]" "valor8" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[42]   > agricola.tareas.valor[9]
"valor[9]" "valor9" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[43]   > agricola.tareas.valor[10]
"valor[10]" "valor10" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[44]   > agricola.tareas.valor[11]
"valor[11]" "valor11" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[45]   > agricola.tareas.valor[12]
"valor[12]" "valor12" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[46]   > agricola.tareas.valor[13]
"valor[13]" "valor13" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[47]   > agricola.tareas.valor[14]
"valor[14]" "valor14" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[48]   > agricola.tareas.valor[15]
"valor[15]" "valor15" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[49]   > agricola.tareas.valor[16]
"valor[16]" "valor16" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[50]   > agricola.tareas.valor[17]
"valor[17]" "valor17" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[51]   > agricola.tareas.valor[18]
"valor[18]" "valor18" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[52]   > agricola.tareas.valor[19]
"valor[19]" "valor19" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[53]   > agricola.tareas.valor[20]
"valor[20]" "valor20" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[54]   > agricola.tareas.valor_normal
"valor_normal" "valor_normal" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[55]   > agricola.tareas.con_restriccion_cupo
"con_restriccion_cupo" "con_restriccion_cupo" ? ? "logical" ? ? ? ? ? ? yes ? no 20.4 yes ?
     _FldNameList[56]   > agricola.tareas.vigente
"vigente" "vigente" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes ?
     _FldNameList[57]   > agricola.tareas.id_grupo_consulta
"id_grupo_consulta" "id_grupo_consulta" ? ? "integer" ? ? ? ? ? ? yes ? no 3.8 yes ?
     _FldNameList[58]   > agricola.tareas.id_tipo_dato
"id_tipo_dato" "id_tipo_dato" ? ? "integer" ? ? ? ? ? ? yes ? no 4.2 yes ?
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

