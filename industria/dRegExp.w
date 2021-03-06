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

DEFINE TEMP-TABLE ttCompetencia
  RCODE-INFORMATION
  FIELD semana          AS CHARACTER COLUMN-LABEL "Semana"
  FIELD puerto_origen   AS CHARACTER COLUMN-LABEL "Puerto Origen"
  FIELD compania        AS CHARACTER COLUMN-LABEL "Compa�ia"
  FIELD region          AS CHARACTER COLUMN-LABEL "Region"
  FIELD puerto_destino  AS CHARACTER COLUMN-LABEL "Puerto Destino"
  FIELD producto        AS CHARACTER COLUMN-LABEL "Producto"
  FIELD vapor           AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD fecha           AS DATE      COLUMN-LABEL "Fecha"
  FIELD cantidad        AS CHARACTER COLUMN-LABEL "Cantidad"
  FIELD anio            AS INTEGER   COLUMN-LABEL "Anio".

DEFINE TEMP-TABLE ttSamiVSOtros
  RCODE-INFORMATION
  FIELD region          AS CHARACTER COLUMN-LABEL "Region"
  FIELD puerto_destino  AS CHARACTER COLUMN-LABEL "Puerto Destino"
  FIELD cantidad_sami   AS INTEGER   COLUMN-LABEL "Cantidad Sami"
  FIELD cantidad_otros  AS INTEGER   COLUMN-LABEL "Cantidad Otros"
  FIELD cantidad_total  AS INTEGER   COLUMN-LABEL "Cantidad Total"
  FIELD porcentaje_sami AS INTEGER   COLUMN-LABEL "(%) Porc. Sami"
  FIELD anio            AS INTEGER   COLUMN-LABEL "Anio".

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
&Scoped-define INTERNAL-TABLES registro_exportacion

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  semana anio id_origen id_puerto_origen
&Scoped-define ENABLED-FIELDS-IN-registro_exportacion semana anio id_origen ~
id_puerto_origen 
&Scoped-Define DATA-FIELDS  semana Origen Cantidad anio id_origen id_puerto_origen
&Scoped-define DATA-FIELDS-IN-registro_exportacion semana anio id_origen ~
id_puerto_origen 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dregexp.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH registro_exportacion ~
      WHERE registro_exportacion.id_puerto_origen > 1 NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH registro_exportacion ~
      WHERE registro_exportacion.id_puerto_origen > 1 NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main registro_exportacion
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main registro_exportacion


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidad dTables  _DB-REQUIRED
FUNCTION getCantidad RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGraphEntry dTables  _DB-REQUIRED
FUNCTION getGraphEntry RETURNS CHARACTER
   (INPUT piSemana AS INTEGER, 
   INPUT piPuerto AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPuertoOrigen dTables  _DB-REQUIRED
FUNCTION getPuertoOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQty dTables  _DB-REQUIRED
FUNCTION getQty RETURNS INTEGER
  (INPUT piPuerto  AS INTEGER, 
   INPUT piEmpresa AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValidUser dTables  _DB-REQUIRED
FUNCTION getValidUser RETURNS LOGICAL
  (pcUser AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      registro_exportacion SCROLLING.
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
     _TblList          = "general.registro_exportacion"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "general.registro_exportacion.id_puerto_origen > 1"
     _FldNameList[1]   > general.registro_exportacion.semana
"semana" "semana" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[2]   > "_<CALC>"
"getPuertoOrigen()" "Origen" "Origen" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[3]   > "_<CALC>"
"getCantidad()" "Cantidad" "Cantidad" ">>>>>>>>>9" "Integer" ? ? ? ? ? ? no ? no 12 no
     _FldNameList[4]   > general.registro_exportacion.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[5]   > general.registro_exportacion.id_origen
"id_origen" "id_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[6]   > general.registro_exportacion.id_puerto_origen
"id_puerto_origen" "id_puerto_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 17.4 yes
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


IF NOT getValidUser(USERID("userdb")) THEN DO:
  RETURN "Error. Ud. no esta habilitado para realizar esta accion.".
END.

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
         rowObject.Cantidad = (getCantidad())
         rowObject.Origen = (getPuertoOrigen())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDataForTT dTables  _DB-REQUIRED
PROCEDURE getDataForTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttCompetencia.

  DEFINE VARIABLE vcRow   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcLista AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcEmp   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcLug   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcPro   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcVap   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcReg   AS CHARACTER  NO-UNDO.

  DEFINE BUFFER pd FOR lugar_descarga.

  FOR EACH ttCompetencia.
    DELETE ttCompetencia.
  END.

  FOR EACH rowObject ,
      EACH items_registro_exportacion WHERE rowObject.anio                               = items_registro_exportacion.anio
                                        AND rowObject.id_puerto_origen                   = items_registro_exportacion.id_puerto_origen
                                        AND rowObject.semana                             = items_registro_exportacion.semana
                                        AND items_registro_exportacion.cantidad          > 0 /*esto lo puse porque tengo un error en la semana 26 puerto_origen 1*/
                                        AND items_registro_exportacion.id_puerto_destino > 1
      BY items_registro_exportacion.semana BY items_registro_exportacion.cantidad DESC.

    FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = rowObject.id_puerto_origen NO-LOCK NO-ERROR.
    FIND FIRST empresa_exportadora WHERE empresa_exportadora.id_empresa_exportadora = items_registro_exportacion.id_empresa_exportadora NO-LOCK NO-ERROR.
    FIND FIRST pd WHERE pd.id_lugdes = items_registro_exportacion.id_puerto_destino NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = items_registro_exportacion.id_producto NO-LOCK NO-ERROR.
    FIND FIRST vapores WHERE vapores.id_vapor = items_registro_exportacion.id_vapor NO-LOCK NO-ERROR.
    FIND FIRST r_lugdes_region WHERE r_lugdes_region.id_lugdes = items_registro_exportacion.id_puerto_destino NO-LOCK NO-ERROR.
    FIND FIRST regiones_mercado WHERE regiones_mercado.id_region_mercado = r_lugdes_region.id_region NO-LOCK NO-ERROR.

    vcEmp = IF AVAILABLE empresa_exportadora THEN empresa_exportadora.nombre ELSE STRING(items_registro_exportacion.id_empresa_exportadora).
    vcLug = IF AVAILABLE pd THEN CAPS(pd.descripcion) ELSE "#".
    vcPro = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "#".
    vcVap = IF AVAILABLE vapores THEN vapores.descripcion ELSE "#".
    vcReg = IF AVAILABLE regiones_mercado THEN regiones_mercado.descripcion ELSE "#".

    CREATE ttCompetencia.
    ASSIGN ttCompetencia.semana         = STRING(rowObject.semana)
           ttCompetencia.puerto_origen  = lugar_descarga.descripcion
           ttCompetencia.compania       = vcEmp
           ttCompetencia.region         = vcReg
           ttCompetencia.puerto_destino = vcLug
           ttCompetencia.producto       = vcPro
           ttCompetencia.vapor          = vcVap
           ttCompetencia.fecha          = items_registro_exportacion.fecha
           ttCompetencia.cantidad       = STRING(items_registro_exportacion.cantidad)
           ttCompetencia.anio           = rowObject.anio.
  END.
  
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDataForTTCuadro dTables  _DB-REQUIRED
PROCEDURE getDataForTTCuadro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttSamiVSOtros.

  DEFINE VARIABLE viCantSami  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE viCantOtros AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iQty        AS INTEGER    NO-UNDO.

  FOR EACH ttSamiVsOtros.
    DELETE ttSamiVsOtros.
  END.

  FOR EACH items_registro_exportacion WHERE items_registro_exportacion.id_puerto_destino > 1 
                                        AND items_registro_exportacion.cantidad > 0,
      EACH registro_exportacion OF items_registro_exportacion
      BREAK BY id_puerto_destino.    
    IF items_registro_exportacion.id_empresa_exportadora = 1 THEN
      viCantSami = viCantSami + items_registro_exportacion.cantidad.
    ELSE 
      viCantOtros = viCantOtros + items_registro_exportacion.cantidad.

    IF LAST-OF(items_registro_exportacion.id_puerto_destino) THEN DO:    
      FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = items_registro_exportacion.id_puerto_destino NO-LOCK NO-ERROR.
      FIND FIRST empresa_exportadora WHERE empresa_exportadora.id_empresa_exportadora = items_registro_exportacion.id_empresa_exportadora NO-LOCK NO-ERROR.
      FIND FIRST r_lugdes_region WHERE items_registro_exportacion.id_puerto_destino = r_lugdes_region.id_lugdes NO-LOCK NO-ERROR.
      FIND FIRST regiones_mercado WHERE regiones_mercado.id_region_mercado = r_lugdes_region.id_region NO-LOCK NO-ERROR.
      CREATE ttSamiVSOtros.
      ASSIGN ttSamiVSOtros.region          = IF AVAILABLE regiones_mercado THEN regiones_mercado.descripcion ELSE "puerto(" + STRING(items_registro_exportacion.id_puerto_destino) + ")"
             ttSamiVSOtros.puerto_destino  = IF AVAILABLE lugar_descarga THEN CAPS(lugar_descarga.descripcion) ELSE "#"
             ttSamiVSOtros.cantidad_sami   = viCantSami
             ttSamiVSOtros.cantidad_otros  = viCantOtros
             ttSamiVSOtros.cantidad_total  = viCantSami + viCantOtros
             ttSamiVSOtros.porcentaje_sami = viCantSami * 100 /(viCantSami + viCantOtros)
             ttSamiVSOtros.anio            = items_registro_exportacion.anio.
      viCantSami  = 0.
      viCantOtros = 0.
    END.

    
  END.
  
END PROCEDURE.


      /*
      FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = items_registro_exportacion.id_puerto_destino NO-LOCK NO-ERROR.
      FIND FIRST empresa_exportadora WHERE empresa_exportadora.id_empresa_exportadora = items_registro_exportacion.id_empresa_exportadora NO-LOCK NO-ERROR.
      FIND FIRST r_lugdes_region WHERE items_registro_exportacion.id_puerto_destino = r_lugdes_region.id_lugdes NO-LOCK NO-ERROR.
      FIND FIRST regiones_mercado WHERE regiones_mercado.id_region_mercado = r_lugdes_region.id_region NO-LOCK NO-ERROR.
      CREATE ttSamiVSOtros.
      ASSIGN ttSamiVSOtros.region          = IF AVAILABLE regiones_mercado THEN regiones_mercado.descripcion ELSE "#"
             ttSamiVSOtros.puerto_destino  = IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "#"
             ttSamiVSOtros.cantidad_sami   = viCantSami
             ttSamiVSOtros.cantidad_otros  = viCantOtros
             ttSamiVSOtros.cantidad_total  = viCantSami + viCantOtros
             ttSamiVSOtros.porcentaje_sami = viCantSami * 100 /(viCantSami + viCantOtros).
      viCantSami  = 0.
      viCantOtros = 0.
      */























/*

IF items_registro_exportacion.id_empresa_exportadora = 1 THEN
      viCantSami = viCantSami + items_registro_exportacion.cantidad.
    ELSE 
      viCantOtros = viCantOtros + items_registro_exportacion.cantidad.
    IF LAST-OF(items_registro_exportacion.id_puerto_destino) THEN DO:
      FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = items_registro_exportacion.id_puerto_destino NO-LOCK NO-ERROR.
      FIND FIRST empresa_exportadora WHERE empresa_exportadora.id_empresa_exportadora = items_registro_exportacion.id_empresa_exportadora NO-LOCK NO-ERROR.
      FIND FIRST r_lugdes_region WHERE items_registro_exportacion.id_puerto_destino = r_lugdes_region.id_lugdes NO-LOCK NO-ERROR.
      FIND FIRST regiones_mercado WHERE regiones_mercado.id_region_mercado = r_lugdes_region.id_region NO-LOCK NO-ERROR.
      CREATE ttSamiVSOtros.
      ASSIGN ttSamiVSOtros.region          = IF AVAILABLE regiones_mercado THEN regiones_mercado.descripcion ELSE "#"
             ttSamiVSOtros.puerto_destino  = IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "#"
             ttSamiVSOtros.cantidad_sami   = viCantSami
             ttSamiVSOtros.cantidad_otros  = viCantOtros
             ttSamiVSOtros.cantidad_total  = viCantSami + viCantOtros
             ttSamiVSOtros.porcentaje_sami = viCantSami * 100 /(viCantSami + viCantOtros).
      viCantSami  = 0.
      viCantOtros = 0.
    END.

*/

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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidad dTables  _DB-REQUIRED
FUNCTION getCantidad RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viCant AS INTEGER    NO-UNDO INITIAL 0.

  FIND CURRENT rowObject NO-ERROR.
  FOR EACH items_registro_exportacion WHERE items_registro_exportacion.id_puerto_origen = rowObject.id_puerto_origen
                                        AND items_registro_exportacion.semana           = rowObject.semana  
                                        AND items_registro_exportacion.anio             = rowObject.anio
                                      NO-LOCK.
    viCant = viCant + items_registro_exportacion.cantidad.
  END.
  

  RETURN viCant.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGraphEntry dTables  _DB-REQUIRED
FUNCTION getGraphEntry RETURNS CHARACTER
   (INPUT piSemana AS INTEGER, 
   INPUT piPuerto AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve una cadenda de tipo entry con los datos necesarios para el grafico
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcLista AS CHARACTER  NO-UNDO.

  FOR EACH items_registro_exportacion WHERE items_registro_exportacion.semana           = piSemana
                                        AND items_registro_exportacion.id_puerto_origen = piPuerto
                                      NO-LOCK.
    FIND FIRST empresa_exportadora WHERE empresa_exportadora.id_empresa_exportadora = items_registro_exportacion.id_empresa_exportadora NO-LOCK NO-ERROR.
    vcLista = vcLista + LC(empresa_exportadora.nombre) + "," + STRING(items_registro_exportacion.cantidad) + CHR(14).
  END.

  RETURN vcLista.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPuertoOrigen dTables  _DB-REQUIRED
FUNCTION getPuertoOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = rowObject.id_puerto_origen
                            NO-LOCK NO-ERROR.
  IF AVAILABLE lugar_descarga THEN
    RETURN lugar_descarga.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQty dTables  _DB-REQUIRED
FUNCTION getQty RETURNS INTEGER
  (INPUT piPuerto  AS INTEGER, 
   INPUT piEmpresa AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  devuelve la cantidad registrada para el puerto destino piPuerto y 
            la empresa piEmpresa
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCant AS INTEGER    NO-UNDO.
  DEFINE BUFFER ire FOR items_registro_exportacion.

  FOR EACH ire WHERE ire.id_puerto_destino      = piPuerto
                 AND ire.id_empresa_exportadora = piEmpresa
               NO-LOCK.
    iCant = iCant + ire.cantidad.      
  END.

  RETURN iCant.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValidUser dTables  _DB-REQUIRED
FUNCTION getValidUser RETURNS LOGICAL
  (pcUser AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose: recorre un archivo de usuarios habilitados y comprueba contra el usuario
           logueado.
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcArchivoUsuarios AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcLine            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE viPos             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vlPermitido       AS LOGICAL    NO-UNDO INITIAL FALSE.

  vcArchivoUsuarios = "..\industria\usuariosValidosCompetencia.val".
  INPUT FROM VALUE(vcArchivoUsuarios).
  REPEAT :
    IMPORT UNFORMATTED vcLine.
    IF LENGTH(vcLine) = 0 THEN DO:  
      /*continua si encuentra una linea en blanco*/
      NEXT.
    END.
    viPos = INDEX(vcLine, "#").
    IF viPos > 0 THEN DO:        
      /*continuo con la regla siguiente si esta el caracter de comentario #*/
      NEXT.
    END.
    
    IF pcUser = vcLine THEN DO:
      vlPermitido = TRUE.      
    END.
  END.

  RETURN vlPermitido.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

