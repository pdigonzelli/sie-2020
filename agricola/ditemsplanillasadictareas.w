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

&Global-define DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES items_planilla_adic_tareas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  cantidad fecha_fin fecha_inicio id_dosis-1 id_dosis-2 id_dosis-3 id_dosis-4~
 id_lote id_planilla_adicional item codigo_trazabilidad~
 id_maquinaria_tratamiento id_dosis-5 id_dosis-6 id_dosis-7 id_dosis-8
&Scoped-define ENABLED-FIELDS-IN-items_planilla_adic_tareas cantidad ~
fecha_fin fecha_inicio id_dosis-1 id_dosis-2 id_dosis-3 id_dosis-4 id_lote ~
id_planilla_adicional item codigo_trazabilidad id_maquinaria_tratamiento ~
id_dosis-5 id_dosis-6 id_dosis-7 id_dosis-8 
&Scoped-Define DATA-FIELDS  cantidad fecha_fin fecha_inicio id_dosis-1 id_dosis-2 id_dosis-3 id_dosis-4~
 id_lote id_planilla_adicional item codigo_trazabilidad~
 id_maquinaria_tratamiento desc-lote id_dosis-5 id_dosis-6 id_dosis-7~
 id_dosis-8
&Scoped-define DATA-FIELDS-IN-items_planilla_adic_tareas cantidad fecha_fin ~
fecha_inicio id_dosis-1 id_dosis-2 id_dosis-3 id_dosis-4 id_lote ~
id_planilla_adicional item codigo_trazabilidad id_maquinaria_tratamiento ~
id_dosis-5 id_dosis-6 id_dosis-7 id_dosis-8 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "ditemsplanillasadictareas.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_planilla_adic_tareas NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_planilla_adic_tareas NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_planilla_adic_tareas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_planilla_adic_tareas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD nombre-lote dTables  _DB-REQUIRED
FUNCTION nombre-lote RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_planilla_adic_tareas SCROLLING.
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
     _TblList          = "agricola.items_planilla_adic_tareas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > agricola.items_planilla_adic_tareas.cantidad
"cantidad" "cantidad" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[2]   > agricola.items_planilla_adic_tareas.fecha_fin
"fecha_fin" "fecha_fin" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ?
     _FldNameList[3]   > agricola.items_planilla_adic_tareas.fecha_inicio
"fecha_inicio" "fecha_inicio" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ?
     _FldNameList[4]   > agricola.items_planilla_adic_tareas.id_dosis-1
"id_dosis-1" "id_dosis-1" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[5]   > agricola.items_planilla_adic_tareas.id_dosis-2
"id_dosis-2" "id_dosis-2" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[6]   > agricola.items_planilla_adic_tareas.id_dosis-3
"id_dosis-3" "id_dosis-3" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[7]   > agricola.items_planilla_adic_tareas.id_dosis-4
"id_dosis-4" "id_dosis-4" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[8]   > agricola.items_planilla_adic_tareas.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ?
     _FldNameList[9]   > agricola.items_planilla_adic_tareas.id_planilla_adicional
"id_planilla_adicional" "id_planilla_adicional" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[10]   > agricola.items_planilla_adic_tareas.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ?
     _FldNameList[11]   > agricola.items_planilla_adic_tareas.codigo_trazabilidad
"codigo_trazabilidad" "codigo_trazabilidad" ? ? "character" ? ? ? ? ? ? yes ? no 15.8 yes ?
     _FldNameList[12]   > agricola.items_planilla_adic_tareas.id_maquinaria_tratamiento
"id_maquinaria_tratamiento" "id_maquinaria_tratamiento" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[13]   > "_<CALC>"
"nombre-lote()" "desc-lote" "Desc Lote" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no "Desc Lote"
     _FldNameList[14]   > agricola.items_planilla_adic_tareas.id_dosis-5
"id_dosis-5" "id_dosis-5" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[15]   > agricola.items_planilla_adic_tareas.id_dosis-6
"id_dosis-6" "id_dosis-6" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[16]   > agricola.items_planilla_adic_tareas.id_dosis-7
"id_dosis-7" "id_dosis-7" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[17]   > agricola.items_planilla_adic_tareas.id_dosis-8
"id_dosis-8" "id_dosis-8" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.desc-lote = (nombre-lote())
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
DEF BUFFER b-items FOR items_planilla_adic_tareas.
DEF VAR v_item AS INTEGER.

FOR EACH RowObjUpd WHERE RowMod = "A" OR RowMod = "C" :
        FIND LAST b-items WHERE b-items.id_planilla = RowObjUpd.id_planilla 
            NO-LOCK NO-ERROR.
        IF AVAILABLE b-items THEN  v_item = b-items.ITEM + 1.
                             ELSE  v_item = 1.
        RowObjUpd.ITEM = v_item.
    
    FIND FIRST planillas_adic_tareas WHERE planillas_adic_tareas.id_planilla = RowObjUpd.id_planilla NO-LOCK NO-ERROR.
    IF AVAILABLE planillas_adic_tareas THEN
    DO:
        FIND FIRST lotes_plantacion WHERE
            lotes_plantacion.id_proveedor = planillas_adic_tareas.id_proveedor AND
            lotes_plantacion.id_origen = planillas_adic_tareas.id_origen AND
            lotes_plantacion.codigo_trazabilidad = RowObjUpd.codigo_trazabilidad AND
            lotes_plantacion.estado = YES NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_plantacion THEN
            RowObjUpd.id_lote = lotes_plantacion.id_lote.
        ELSE  
            RETURN "Lote no valido - Verifique".
    END.
END.

FOR EACH RowObjUpd WHERE RowMod = "U":
    
    FIND FIRST planillas_adic_tareas WHERE planillas_adic_tareas.id_planilla = RowObjUpd.id_planilla NO-LOCK NO-ERROR.
    IF AVAILABLE planillas_adic_tareas THEN
    DO:
        FIND FIRST lotes_plantacion WHERE
            lotes_plantacion.id_proveedor = planillas_adic_tareas.id_proveedor AND
            lotes_plantacion.id_origen = planillas_adic_tareas.id_origen AND
            lotes_plantacion.codigo_trazabilidad = RowObjUpd.codigo_trazabilidad AND
            lotes_plantacion.estado = YES NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_plantacion THEN
            RowObjUpd.id_lote = lotes_plantacion.id_lote.
        ELSE  
            RETURN "Lote no valido - Verifique".


    END.
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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION nombre-lote dTables  _DB-REQUIRED
FUNCTION nombre-lote RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR v_nombre AS CHARACTER.
  DEF BUFFER b-pla FOR planillas_adic_tareas.
  DEF BUFFER b-items FOR items_planilla_adic_tareas.
 
  FIND FIRST b-items WHERE b-items.id_planilla = RowObject.id_planilla and
                           b-items.ITEM = RowObject.ITEM NO-LOCK NO-ERROR.
  IF AVAILABLE b-items THEN
  DO:
      FIND FIRST b-pla OF b-items NO-LOCK NO-ERROR.
      IF AVAILABLE b-pla THEN
      DO:
          FIND FIRST lotes_plantacion WHERE
              lotes_plantacion.id_proveedor = b-pla.id_proveedor AND
              lotes_plantacion.id_origen = b-pla.id_origen AND
              lotes_plantacion.id_lote = b-items.id_lote NO-LOCK NO-ERROR.
          IF AVAILABLE lotes_plantacion THEN
              v_nombre = lotes_plantacion.descripcion.
           ELSE 
              v_nombre = "".
      END.
  END.
  RETURN v_nombre.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

