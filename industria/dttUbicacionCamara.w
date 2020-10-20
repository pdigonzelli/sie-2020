&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          tablastemp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttUbicacionCamara NO-UNDO LIKE ubicacioncamara.


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
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE hApp AS HANDLE     NO-UNDO.
DEFINE VARIABLE lFlg AS LOGICAL    NO-UNDO.



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
&Scoped-define INTERNAL-TABLES ttUbicacionCamara

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_sucursal_ubicacion id_empresa id_sucursal id_tipotambor nromov id_tambor~
 id_empresa_camara id_sucursal_camara id_camara nro_fila nro_columna~
 ubicacion camara obs id_lote anio id_articulo id_calidad id_envase articulo~
 calidad envase
&Scoped-define ENABLED-FIELDS-IN-ttUbicacionCamara id_sucursal_ubicacion ~
id_empresa id_sucursal id_tipotambor nromov id_tambor id_empresa_camara ~
id_sucursal_camara id_camara nro_fila nro_columna ubicacion camara obs ~
id_lote anio id_articulo id_calidad id_envase articulo calidad envase 
&Scoped-Define DATA-FIELDS  id_sucursal_ubicacion id_empresa id_sucursal id_tipotambor nromov id_tambor~
 id_empresa_camara id_sucursal_camara id_camara nro_fila nro_columna~
 ubicacion camara obs id_lote anio id_articulo id_calidad id_envase articulo~
 calidad envase
&Scoped-define DATA-FIELDS-IN-ttUbicacionCamara id_sucursal_ubicacion ~
id_empresa id_sucursal id_tipotambor nromov id_tambor id_empresa_camara ~
id_sucursal_camara id_camara nro_fila nro_columna ubicacion camara obs ~
id_lote anio id_articulo id_calidad id_envase articulo calidad envase 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttubicacioncamara.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ttUbicacionCamara NO-LOCK ~
    BY ttUbicacionCamara.id_lote ~
       BY ttUbicacionCamara.anio ~
        BY ttUbicacionCamara.id_tambor INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ttUbicacionCamara NO-LOCK ~
    BY ttUbicacionCamara.id_lote ~
       BY ttUbicacionCamara.anio ~
        BY ttUbicacionCamara.id_tambor INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ttUbicacionCamara
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ttUbicacionCamara


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectApp dTables  _DB-REQUIRED
FUNCTION connectApp RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disconnectApp dTables  _DB-REQUIRED
FUNCTION disconnectApp RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedRowids dTables  _DB-REQUIRED
FUNCTION getSelectedRowids RETURNS CHARACTER
  (pcRowIds AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ttUbicacionCamara SCROLLING.
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
   Temp-Tables and Buffers:
      TABLE: ttUbicacionCamara T "?" NO-UNDO tablastemp ubicacioncamara
   END-TABLES.
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
     _TblList          = "Temp-Tables.ttUbicacionCamara"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.ttUbicacionCamara.id_lote|yes,Temp-Tables.ttUbicacionCamara.anio|yes,Temp-Tables.ttUbicacionCamara.id_tambor|yes"
     _FldNameList[1]   > Temp-Tables.ttUbicacionCamara.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 21 no
     _FldNameList[2]   > Temp-Tables.ttUbicacionCamara.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 no
     _FldNameList[3]   > Temp-Tables.ttUbicacionCamara.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 no
     _FldNameList[4]   > Temp-Tables.ttUbicacionCamara.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 no
     _FldNameList[5]   > Temp-Tables.ttUbicacionCamara.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[6]   > Temp-Tables.ttUbicacionCamara.id_tambor
"id_tambor" "id_tambor" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 no
     _FldNameList[7]   > Temp-Tables.ttUbicacionCamara.id_empresa_camara
"id_empresa_camara" "id_empresa_camara" ? ? "integer" ? ? ? ? ? ? yes ? no 19 no
     _FldNameList[8]   > Temp-Tables.ttUbicacionCamara.id_sucursal_camara
"id_sucursal_camara" "id_sucursal_camara" ? ? "integer" ? ? ? ? ? ? yes ? no 18.8 no
     _FldNameList[9]   > Temp-Tables.ttUbicacionCamara.id_camara
"id_camara" "id_camara" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[10]   > Temp-Tables.ttUbicacionCamara.nro_fila
"nro_fila" "nro_fila" ? ? "character" ? ? ? ? ? ? yes ? no 10 no
     _FldNameList[11]   > Temp-Tables.ttUbicacionCamara.nro_columna
"nro_columna" "nro_columna" ? ? "character" ? ? ? ? ? ? yes ? no 12.2 no
     _FldNameList[12]   > Temp-Tables.ttUbicacionCamara.ubicacion
"ubicacion" "ubicacion" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[13]   > Temp-Tables.ttUbicacionCamara.camara
"camara" "camara" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[14]   > Temp-Tables.ttUbicacionCamara.obs
"obs" "obs" ? ? "character" ? ? ? ? ? ? yes ? no 30 no
     _FldNameList[15]   > Temp-Tables.ttUbicacionCamara.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[16]   > Temp-Tables.ttUbicacionCamara.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no
     _FldNameList[17]   > Temp-Tables.ttUbicacionCamara.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[18]   > Temp-Tables.ttUbicacionCamara.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[19]   > Temp-Tables.ttUbicacionCamara.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[20]   > Temp-Tables.ttUbicacionCamara.articulo
"articulo" "articulo" ? ? "character" ? ? ? ? ? ? yes ? no 25 no
     _FldNameList[21]   > Temp-Tables.ttUbicacionCamara.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[22]   > Temp-Tables.ttUbicacionCamara.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTT dTables  _DB-REQUIRED
PROCEDURE addTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piUbi AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEca AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSca AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcFil AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcCol AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.
  
  FIND FIRST buTam WHERE buTam.id_empresa    = piEmp
                     AND buTam.id_sucursal   = piSuc
                     AND buTam.id_tipotambor = piTip
                     AND buTam.nromov        = piNro
                     AND buTam.id_tambor     = piTam
                   NO-LOCK NO-ERROR.

  FIND FIRST productos_terminados OF buTam NO-LOCK NO-ERROR.
  FIND FIRST envases_prod OF buTam NO-LOCK NO-ERROR.
  FIND FIRST calidades OF buTam NO-LOCK NO-ERROR.
  FIND FIRST sucursales WHERE sucursales.id_sucursal = piUbi NO-LOCK NO-ERROR.
  FIND FIRST camara WHERE camara.id_empresa   = piEca
                      AND camara.id_sucursal  = piSca
                      AND camara.id_camara    = piCam
                    NO-LOCK NO-ERROR.
  
  CREATE ttUbicacionCamara.
  ASSIGN ttUbicacionCamara.id_sucursal_ubicacion  = piUbi
         ttUbicacionCamara.id_empresa             = piEmp
         ttUbicacionCamara.id_sucursal            = piSuc
         ttUbicacionCamara.id_tipotambor          = piTip
         ttUbicacionCamara.nromov                 = piNro
         ttUbicacionCamara.id_tambor              = piTam
         ttUbicacionCamara.id_empresa_camara      = piEca
         ttUbicacionCamara.id_sucursal_camara     = piSca
         ttUbicacionCamara.id_camara              = piCam
         ttUbicacionCamara.nro_fila               = pcFil
         ttUbicacionCamara.nro_columna            = pcCol
         ttUbicacionCamara.ubicacion              = sucursales.abreviatura
         ttUbicacionCamara.camara                 = IF AVAILABLE camara THEN camara.descripcion ELSE "SIN-CAMARA"
         ttUbicacionCamara.id_lote                = buTam.id_lote
         ttUbicacionCamara.anio                   = buTam.anio
         ttUbicacionCamara.id_articulo            = buTam.id_Articulo
         ttUbicacionCamara.id_calidad             = buTam.id_calidad
         ttUbicacionCamara.id_envase              = buTam.id_envase
         ttUbicacionCamara.articulo               = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NO-PROD"
         ttUbicacionCamara.calidad                = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NO-CALIDAD"
         ttUbicacionCamara.envase                 = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NO-ENVASE".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTT dTables  _DB-REQUIRED
PROCEDURE fillTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcFil AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcCol AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cFil  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCol  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRows AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow  AS CHARACTER  NO-UNDO.

  FOR EACH ttUbicacionCamara.
    DELETE ttUbicacionCamara.
  END.

  RUN fillTTCamara IN hLib (piEmp, piSuc, piCam, pcFil, pcCol, INPUT-OUTPUT TABLE ttUbicacionCamara).

  openQuery().



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

  connectApp().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectApp dTables  _DB-REQUIRED
FUNCTION connectApp RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  hApp = DYNAMIC-FUNCTION('connectApp' IN hLibCom).

  IF VALID-HANDLE(hApp) THEN 
    RUN libTamboresIndustria.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
  ELSE 
    RUN libTamboresIndustria.p PERSISTENT SET hLib .

  lFlg = VALID-HANDLE(hApp).
  
  
  RETURN lFlg.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disconnectApp dTables  _DB-REQUIRED
FUNCTION disconnectApp RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(hApp) THEN DO:      
    hApp:DISCONNECT().
    DELETE OBJECT hApp.
  END.


  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedRowids dTables  _DB-REQUIRED
FUNCTION getSelectedRowids RETURNS CHARACTER
  (pcRowIds AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRid AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  DEFINE BUFFER buRO FOR ttUbicacionCamara.

  DO i = 1 TO NUM-ENTRIES(pcRowIds):
    cRid = ENTRY(i, pcRowIds).

    FIND FIRST buRO WHERE ROWID(buRO) = TO-ROWID(cRid) NO-LOCK NO-ERROR.
    IF AVAILABLE buRO THEN DO:
      FIND FIRST tambores_industria WHERE tambores_industria.id_empresa     = buRO.id_empresa
                                      AND tambores_industria.id_sucursal    = buRO.id_sucursal
                                      AND tambores_industria.id_tipotambor  = buRO.id_tipotambor
                                      AND tambores_industria.nromov         = buRO.nromov
                                      AND tambores_industria.id_tambor      = buRO.id_tambor
                                    NO-LOCK NO-ERROR.
      IF AVAILABLE tambores_industria THEN 
        cRet = cRet + STRING(ROWID(tambores_industria)) + ",".
    END.


  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

