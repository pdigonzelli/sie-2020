&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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
&Scoped-define INTERNAL-TABLES sucursales

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  abreviatura barrio cajas cant_empleados codigo_dgi c_fecha c_hora c_usuario~
 domicilio estado e_mail fax fecha_ficha_stock fecha_inicio_act id_categoria~
 id_centro_costo id_sucursal id_tipo_sistema id_tipo_sucursal localidad~
 metros_playa metros_salon metros_servicios metros_total nombre~
 observaciones pais postal provincia telefono1 telefono2 telefono3
&Scoped-define ENABLED-FIELDS-IN-sucursales abreviatura barrio cajas ~
cant_empleados codigo_dgi c_fecha c_hora c_usuario domicilio estado e_mail ~
fax fecha_ficha_stock fecha_inicio_act id_categoria id_centro_costo ~
id_sucursal id_tipo_sistema id_tipo_sucursal localidad metros_playa ~
metros_salon metros_servicios metros_total nombre observaciones pais postal ~
provincia telefono1 telefono2 telefono3 
&Scoped-Define DATA-FIELDS  abreviatura barrio cajas cant_empleados codigo_dgi c_fecha c_hora c_usuario~
 domicilio estado e_mail fax fecha_ficha_stock fecha_inicio_act id_categoria~
 id_centro_costo id_sucursal id_tipo_sistema id_tipo_sucursal localidad~
 metros_playa metros_salon metros_servicios metros_total nombre~
 observaciones pais postal provincia telefono1 telefono2 telefono3
&Scoped-define DATA-FIELDS-IN-sucursales abreviatura barrio cajas ~
cant_empleados codigo_dgi c_fecha c_hora c_usuario domicilio estado e_mail ~
fax fecha_ficha_stock fecha_inicio_act id_categoria id_centro_costo ~
id_sucursal id_tipo_sistema id_tipo_sucursal localidad metros_playa ~
metros_salon metros_servicios metros_total nombre observaciones pais postal ~
provincia telefono1 telefono2 telefono3 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.telefono1 = sucursales.telefono[1]~
  rowObject.telefono2 = sucursales.telefono[2]~
  rowObject.telefono3 = sucursales.telefono[3]
&Scoped-Define DATA-FIELD-DEFS "dSucursales.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH sucursales NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH sucursales NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main sucursales
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main sucursales


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      sucursales SCROLLING.
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
     _TblList          = "general.sucursales"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.sucursales.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[2]   > general.sucursales.barrio
"barrio" "barrio" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[3]   > general.sucursales.cajas
"cajas" "cajas" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes ?
     _FldNameList[4]   > general.sucursales.cant_empleados
"cant_empleados" "cant_empleados" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ?
     _FldNameList[5]   > general.sucursales.codigo_dgi
"codigo_dgi" "codigo_dgi" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[6]   > general.sucursales.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 8.4 yes ?
     _FldNameList[7]   > general.sucursales.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[8]   > general.sucursales.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[9]   > general.sucursales.domicilio
"domicilio" "domicilio" ? ? "character" ? ? ? ? ? ? yes ? no 35 yes ?
     _FldNameList[10]   > general.sucursales.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[11]   > general.sucursales.e_mail
"e_mail" "e_mail" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[12]   > general.sucursales.fax
"fax" "fax" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[13]   > general.sucursales.fecha_ficha_stock
"fecha_ficha_stock" "fecha_ficha_stock" ? ? "date" ? ? ? ? ? ? yes ? no 8.4 yes ?
     _FldNameList[14]   > general.sucursales.fecha_inicio_act
"fecha_inicio_act" "fecha_inicio_act" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[15]   > general.sucursales.id_categoria
"id_categoria" "id_categoria" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[16]   > general.sucursales.id_centro_costo
"id_centro_costo" "id_centro_costo" ? ? "character" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[17]   > general.sucursales.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes ?
     _FldNameList[18]   > general.sucursales.id_tipo_sistema
"id_tipo_sistema" "id_tipo_sistema" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ?
     _FldNameList[19]   > general.sucursales.id_tipo_sucursal
"id_tipo_sucursal" "id_tipo_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ?
     _FldNameList[20]   > general.sucursales.localidad
"localidad" "localidad" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[21]   > general.sucursales.metros_playa
"metros_playa" "metros_playa" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[22]   > general.sucursales.metros_salon
"metros_salon" "metros_salon" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[23]   > general.sucursales.metros_servicios
"metros_servicios" "metros_servicios" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[24]   > general.sucursales.metros_total
"metros_total" "metros_total" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes ?
     _FldNameList[25]   > general.sucursales.nombre
"nombre" "nombre" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[26]   > general.sucursales.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[27]   > general.sucursales.pais
"pais" "pais" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[28]   > general.sucursales.postal
"postal" "postal" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[29]   > general.sucursales.provincia
"provincia" "provincia" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[30]   > general.sucursales.telefono[1]
"telefono[1]" "telefono1" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[31]   > general.sucursales.telefono[2]
"telefono[2]" "telefono2" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[32]   > general.sucursales.telefono[3]
"telefono[3]" "telefono3" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tipo dTables  _DB-REQUIRED
PROCEDURE tipo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ptipo AS INTEGER NO-UNDO.

DEFINE VAR CWHERE AS CHARACTER NO-UNDO.

CWHERE = 'for each sucursales where id_tipo_sucursal = ' + STRING(PTIPO) .

SETQUERYWHERE(CWHERE).
OPENQUERY().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

