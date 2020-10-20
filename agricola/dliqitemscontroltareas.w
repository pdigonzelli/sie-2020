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
&Scoped-define INTERNAL-TABLES liq_items_control_tareas tareas liq_legajos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  cantidad cant_horas cant_hs_extras cant_jornal c_fecha c_hora c_usuario~
 dni_cuil fecha hora_fin hora_inicio id_centro_costo id_empresa id_grupo~
 id_lote id_origen id_proveedor id_sector id_sucursal id_tarea~
 id_unidad_liquidacion legajo nombre nro_maquina nro_tractor total_horas~
 cant_hs_compensa cant_hs_norm cant_jornal_norm tipo_turno id_tipo_planilla~
 hora_inicio-1 hora_fin-1 ajuste_categoria compensa_hs id_reserva~
 cantidad_adicional id_unidad_adicional cantidad_adicional-1 id_turno
&Scoped-define ENABLED-FIELDS-IN-liq_items_control_tareas cantidad ~
cant_horas cant_hs_extras cant_jornal c_fecha c_hora c_usuario dni_cuil ~
fecha hora_fin hora_inicio id_centro_costo id_empresa id_grupo id_lote ~
id_origen id_proveedor id_sector id_sucursal id_tarea id_unidad_liquidacion ~
legajo nombre nro_maquina nro_tractor total_horas cant_hs_compensa ~
cant_hs_norm cant_jornal_norm tipo_turno id_tipo_planilla hora_inicio-1 ~
hora_fin-1 ajuste_categoria compensa_hs id_reserva cantidad_adicional ~
id_unidad_adicional cantidad_adicional-1 id_turno 
&Scoped-Define DATA-FIELDS  cantidad cant_horas cant_hs_extras cant_jornal c_fecha c_hora c_usuario~
 dni_cuil fecha hora_fin hora_inicio id_centro_costo id_empresa id_grupo~
 id_lote id_origen id_proveedor id_sector id_sucursal id_tarea abreviatura~
 id_unidad_liquidacion legajo nombre nro_maquina nro_tractor total_horas~
 cant_hs_compensa cant_hs_norm cant_jornal_norm tipo_turno id_tipo_planilla~
 hora_inicio-1 hora_fin-1 ajuste_categoria compensa_hs id_reserva~
 cantidad_adicional id_unidad_adicional desc-lote cantidad_adicional-1~
 apellido_nombre id_turno
&Scoped-define DATA-FIELDS-IN-liq_items_control_tareas cantidad cant_horas ~
cant_hs_extras cant_jornal c_fecha c_hora c_usuario dni_cuil fecha hora_fin ~
hora_inicio id_centro_costo id_empresa id_grupo id_lote id_origen ~
id_proveedor id_sector id_sucursal id_tarea id_unidad_liquidacion legajo ~
nombre nro_maquina nro_tractor total_horas cant_hs_compensa cant_hs_norm ~
cant_jornal_norm tipo_turno id_tipo_planilla hora_inicio-1 hora_fin-1 ~
ajuste_categoria compensa_hs id_reserva cantidad_adicional ~
id_unidad_adicional cantidad_adicional-1 id_turno 
&Scoped-define DATA-FIELDS-IN-tareas abreviatura 
&Scoped-define DATA-FIELDS-IN-liq_legajos apellido_nombre 
&Scoped-Define MANDATORY-FIELDS  id_origen id_proveedor
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dliqitemscontroltareas.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH liq_items_control_tareas NO-LOCK, ~
      FIRST tareas OF liq_items_control_tareas OUTER-JOIN NO-LOCK, ~
      FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_control_tareas.id_empresa ~
  AND liq_legajos.legajo = liq_items_control_tareas.legajo OUTER-JOIN NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH liq_items_control_tareas NO-LOCK, ~
      FIRST tareas OF liq_items_control_tareas OUTER-JOIN NO-LOCK, ~
      FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_control_tareas.id_empresa ~
  AND liq_legajos.legajo = liq_items_control_tareas.legajo OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main liq_items_control_tareas tareas ~
liq_legajos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main liq_items_control_tareas
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main tareas
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main liq_legajos


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
      liq_items_control_tareas, 
      tareas, 
      liq_legajos SCROLLING.
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
     _TblList          = "agricola.liq_items_control_tareas,agricola.tareas OF agricola.liq_items_control_tareas,agricola.liq_legajos WHERE agricola.liq_items_control_tareas ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER"
     _JoinCode[3]      = "agricola.liq_legajos.id_empresa_liq = agricola.liq_items_control_tareas.id_empresa
  AND agricola.liq_legajos.legajo = agricola.liq_items_control_tareas.legajo"
     _FldNameList[1]   > agricola.liq_items_control_tareas.cantidad
"cantidad" "cantidad" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[2]   > agricola.liq_items_control_tareas.cant_horas
"cant_horas" "cant_horas" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes ?
     _FldNameList[3]   > agricola.liq_items_control_tareas.cant_hs_extras
"cant_hs_extras" "cant_hs_extras" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[4]   > agricola.liq_items_control_tareas.cant_jornal
"cant_jornal" "cant_jornal" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes ?
     _FldNameList[5]   > agricola.liq_items_control_tareas.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[6]   > agricola.liq_items_control_tareas.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[7]   > agricola.liq_items_control_tareas.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[8]   > agricola.liq_items_control_tareas.dni_cuil
"dni_cuil" "dni_cuil" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[9]   > agricola.liq_items_control_tareas.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ?
     _FldNameList[10]   > agricola.liq_items_control_tareas.hora_fin
"hora_fin" "hora_fin" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[11]   > agricola.liq_items_control_tareas.hora_inicio
"hora_inicio" "hora_inicio" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[12]   > agricola.liq_items_control_tareas.id_centro_costo
"id_centro_costo" "id_centro_costo" ? ? "character" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[13]   > agricola.liq_items_control_tareas.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ?
     _FldNameList[14]   > agricola.liq_items_control_tareas.id_grupo
"id_grupo" "id_grupo" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes ?
     _FldNameList[15]   > agricola.liq_items_control_tareas.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ?
     _FldNameList[16]   > agricola.liq_items_control_tareas.id_origen
"id_origen" "id_origen" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.2 yes ?
     _FldNameList[17]   > agricola.liq_items_control_tareas.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? yes ? yes 14.8 yes ?
     _FldNameList[18]   > agricola.liq_items_control_tareas.id_sector
"id_sector" "id_sector" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[19]   > agricola.liq_items_control_tareas.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ?
     _FldNameList[20]   > agricola.liq_items_control_tareas.id_tarea
"id_tarea" "id_tarea" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ?
     _FldNameList[21]   > agricola.tareas.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? no ? no 12 yes ?
     _FldNameList[22]   > agricola.liq_items_control_tareas.id_unidad_liquidacion
"id_unidad_liquidacion" "id_unidad_liquidacion" ? ? "integer" ? ? ? ? ? ? yes ? no 16.4 yes ?
     _FldNameList[23]   > agricola.liq_items_control_tareas.legajo
"legajo" "legajo" ? ">>>>>>>>9" "integer" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[24]   > agricola.liq_items_control_tareas.nombre
"nombre" "nombre" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[25]   > agricola.liq_items_control_tareas.nro_maquina
"nro_maquina" "nro_maquina" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ?
     _FldNameList[26]   > agricola.liq_items_control_tareas.nro_tractor
"nro_tractor" "nro_tractor" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[27]   > agricola.liq_items_control_tareas.total_horas
"total_horas" "total_horas" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[28]   > agricola.liq_items_control_tareas.cant_hs_compensa
"cant_hs_compensa" "cant_hs_compensa" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[29]   > agricola.liq_items_control_tareas.cant_hs_norm
"cant_hs_norm" "cant_hs_norm" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[30]   > agricola.liq_items_control_tareas.cant_jornal_norm
"cant_jornal_norm" "cant_jornal_norm" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.2 yes ?
     _FldNameList[31]   > agricola.liq_items_control_tareas.tipo_turno
"tipo_turno" "tipo_turno" ? ? "logical" ? ? ? ? ? ? yes ? no 5.6 yes ?
     _FldNameList[32]   > agricola.liq_items_control_tareas.id_tipo_planilla
"id_tipo_planilla" "id_tipo_planilla" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[33]   > agricola.liq_items_control_tareas.hora_inicio-1
"hora_inicio-1" "hora_inicio-1" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[34]   > agricola.liq_items_control_tareas.hora_fin-1
"hora_fin-1" "hora_fin-1" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[35]   > agricola.liq_items_control_tareas.ajuste_categoria
"ajuste_categoria" "ajuste_categoria" ? ? "logical" ? ? ? ? ? ? yes ? no 5.8 yes ?
     _FldNameList[36]   > agricola.liq_items_control_tareas.compensa_hs
"compensa_hs" "compensa_hs" ? ? "logical" ? ? ? ? ? ? yes ? no 13.2 yes ?
     _FldNameList[37]   > agricola.liq_items_control_tareas.id_reserva
"id_reserva" "id_reserva" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[38]   > agricola.liq_items_control_tareas.cantidad_adicional
"cantidad_adicional" "cantidad_adicional" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[39]   > agricola.liq_items_control_tareas.id_unidad_adicional
"id_unidad_adicional" "id_unidad_adicional" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes ?
     _FldNameList[40]   > "_<CALC>"
"nombre-lote()" "desc-lote" "Lote" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no "Lote"
     _FldNameList[41]   > agricola.liq_items_control_tareas.cantidad_adicional-1
"cantidad_adicional-1" "cantidad_adicional-1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[42]   > agricola.liq_legajos.apellido_nombre
"apellido_nombre" "apellido_nombre" ? ? "character" ? ? ? ? ? ? no ? no 30 yes ?
     _FldNameList[43]   > agricola.liq_items_control_tareas.id_turno
"id_turno" "id_turno" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
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
DEF BUFFER b_control FOR CONTROL_tareas.
DEF BUFFER b_items FOR liq_items_control_tareas.

FIND LAST RowObjUpd NO-ERROR.

/* Actualiza Planilla */
FOR EACH b_control WHERE b_control.id_empresa = RowObjUpd.id_empresa AND
    b_control.id_sucursal = RowObjUpd.id_sucursal AND
    b_control.id_sector = RowObjUpd.id_sector AND
    b_control.fecha = RowObjUpd.fecha AND
    b_control.id_proveedor = RowObjUpd.id_proveedor AND
    b_control.id_origen = RowObjUpd.id_origen AND
    b_control.id_grupo = RowObjUpd.id_grupo AND
    b_control.id_tipo_planilla = RowObjUpd.id_tipo_planilla NO-LOCK:
    FOR EACH b_items OF b_control :
        assign  cant_jornal = cant_jornal_norm
                cant_horas = (cant_hs_norm + cant_hs_comp + cant_hs_extras).
    END.
END.


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
DEF VAR v_hs_compensadas AS INTEGER NO-UNDO.
DEF BUFFER bctrl FOR control_tareas. 
DEF BUFFER bitems FOR liq_items_control_tareas. 
DEF VAR v_cantidad AS INTEGER.
DEF VAR v_de_vacaciones AS LOGICAL.

FOR EACH RowObjUpd WHERE rowMod = "A" OR rowMod = "C" OR rowMod = "U":

    /* Chequeo que el legajo tenga liquida = yes */

     FIND FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = RowObjUpd.id_empresa AND
        liq_legajos.legajo = RowObjUpd.legajo NO-LOCK NO-ERROR.
     IF NOT AVAILABLE liq_legajos THEN  RETURN "Debe ingresar un legajo v lido".

     IF liq_legajos.liquida = NO THEN
        RETURN "El estado del legajo no le permite ingresar en la tarja".

       ASSIGN RowObjUpd.nombre = liq_legajos.apellido_nombre
                 RowObjUpd.dni_cuil = liq_legajos.cuil.

    IF RowObjUpd.compensa_hs = yes THEN
    DO:
        /* Calculo Total de hs compensadas */
        v_hs_compensadas = 0.
        FOR EACH bitems WHERE bitems.id_empresa = RowObjUpd.id_empresa and
            bitems.id_sector = RowObjUpd.id_sector and
            bItems.id_sucursal = RowObjUpd.id_sucursal and
            bitems.legajo = RowObjUpd.legajo AND bitems.id_reserva = RowObjUpd.id_reserva NO-LOCK:
            v_hs_compensadas = v_hs_compensadas + bitems.cant_hs_compensa.
        END.

        /* Actualizo Reserva de Hs */
        IF v_hs_compensadas <> 0 THEN
        DO:
            FIND FIRST reserva_horas WHERE reserva_horas.id_empresa = RowObjUpd.id_empresa AND
                       reserva_horas.id_sector = RowObjUpd.id_sector AND
                       reserva_horas.id_sucursal = RowObjUpd.id_sucursal AND
                       reserva_horas.legajo = RowObjUpd.legajo NO-ERROR.
            IF AVAILABLE reserva_horas THEN
                ASSIGN reserva_horas.cant_hs_consumidas = v_hs_compensadas.
        END.
    END.

    /* chequeo si la tarea requiere cantidad adicional */
    FIND FIRST aux_tareas WHERE aux_tareas.id_tarea = RowObjUpd.id_tarea NO-LOCK NO-ERROR.
    IF AVAILABLE aux_tareas THEN
    DO:
        IF aux_tareas.datos_adicionales = YES THEN
        DO:
            IF RowObjUpd.cantidad_adicional = 0 THEN
            DO:
                RETURN "Debe ingresar una cantidad adicional a la tarea".
            END.
        END.
    END.

    /* Chequeo que el lote sea valido */
    FIND FIRST lotes_plantacion WHERE lotes_plantacion.id_proveedor = RowObjUpd.id_proveedor AND
        lotes_plantacion.id_origen = RowObjUpd.id_origen AND
        lotes_plantacion.id_lote = RowObjUpd.id_lote and
        lotes_plantacion.estado = YES NO-LOCK NO-ERROR.
    IF NOT AVAILABLE lotes_plantacion THEN
           RETURN "Lote no valido - Verifique".

 /*  Chequeo que la tareas  sea valida */
    FIND FIRST tareas WHERE tareas.id_tarea = RowObjUpd.id_tarea  AND
        tareas.vigente = YES NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tareas THEN
           RETURN "Tarea no valida - Verifique".

 /*  Chequeo que la unidad_liquidacion  sea valida */
    
    IF RowObjUpd.id_tarea <> 0 AND RowObjUpd.id_unidad_liquidacion = 0 THEN
    DO:
        RETURN "Debe ingresar una unidad de liquidacion a la tarea".
    END.
    ELSE
    DO:
       IF RowObjUpd.id_tarea <> 0 THEN
       DO:
            FIND FIRST r_tareas_unidades WHERE r_tareas_unidades.id_tarea = RowObjUpd.id_tarea  AND
                r_tareas_unidades.id_unidad_liquidacion = RowObjUpd.id_unidad_liquidacion NO-LOCK NO-ERROR.
            IF NOT AVAILABLE r_tareas_unidades THEN
                   RETURN "Unidad  no valida para esa tarea - Verifique".
       END.
    END.

    IF RowObjUpd.cantidad <> 0 THEN
    DO:
        IF RowObjUpd.cant_jornal_norm <> 0 OR RowObjUpd.cant_hs_norm <> 0  THEN
        RETURN "Para ingresar cantidad, Js y Hs deben estar en 0".
    END.

  /*  IF RowObjUpd.cant_jornal_norm = 0 AND RowObjUpd.cant_hs_norm = 0  THEN
    DO:
        IF RowObjUpd.cantidad = 0 THEN
            RETURN "Debe ingresar cantidad <> 0".
    END.
    */


   /* Chequeo que la persona no figure en esa fecha en planilla tipo 5 (vacaciones) */
    IF RowObjUpd.id_tipo_planilla <> 5 THEN
    DO:
        v_de_vacaciones = NO.
        FOR EACH bitems WHERE bitems.id_empresa = RowObjUpd.id_empresa and
            bitems.legajo = RowObjUpd.legajo NO-LOCK, FIRST bctrl OF bitems where
            bctrl.fecha = RowObjUpd.fecha AND
            bctrl.id_tipo_planilla = 5 NO-LOCK:
            v_de_vacaciones = YES.
            LEAVE.
        END.
        IF v_de_vacaciones THEN RETURN "Esta persona esta registrada de vacaciones".
    END.

    ASSIGN RowObjUpd.c_usuario = userid("userdb")
           RowObjUpd.c_fecha = TODAY
           RowObjUpd.c_hora = STRING(TIME,"HH:MM:SS").


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
          
  FIND FIRST lotes_plantacion WHERE
              lotes_plantacion.id_proveedor = RowObject.id_proveedor AND
              lotes_plantacion.id_origen = RowObject.id_origen AND
              lotes_plantacion.id_lote = RowObject.id_lote NO-LOCK NO-ERROR.
          IF AVAILABLE lotes_plantacion THEN
              v_nombre = lotes_plantacion.descripcion.
           ELSE 
              v_nombre = "".

  RETURN v_nombre.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

