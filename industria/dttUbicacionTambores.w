&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          tablastemp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttUbicacionTambores NO-UNDO LIKE ubicaciontambores.


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

  DEFINE VARIABLE iFila AS INTEGER    NO-UNDO.

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
&Scoped-define INTERNAL-TABLES ttUbicacionTambores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_empresa id_sucursal id_tipotambor nromov id_sucursal_ubicacion id_lote~
 anio id_lote_deposito articulo calidad envase cantidad id_articulo kilos~
 fecha id_empresa_destino id_sucursal_destino id_tipotambor_destino~
 nromov_destino id_calidad id_envase kilos_400
&Scoped-define ENABLED-FIELDS-IN-ttUbicacionTambores id_empresa id_sucursal ~
id_tipotambor nromov id_sucursal_ubicacion id_lote anio id_lote_deposito ~
articulo calidad envase cantidad id_articulo kilos fecha id_empresa_destino ~
id_sucursal_destino id_tipotambor_destino nromov_destino id_calidad ~
id_envase kilos_400 
&Scoped-Define DATA-FIELDS  id_empresa id_sucursal id_tipotambor nromov id_sucursal_ubicacion id_lote~
 anio id_lote_deposito articulo calidad envase cantidad id_articulo kilos~
 fecha id_empresa_destino id_sucursal_destino id_tipotambor_destino~
 nromov_destino id_calidad id_envase kilos_400
&Scoped-define DATA-FIELDS-IN-ttUbicacionTambores id_empresa id_sucursal ~
id_tipotambor nromov id_sucursal_ubicacion id_lote anio id_lote_deposito ~
articulo calidad envase cantidad id_articulo kilos fecha id_empresa_destino ~
id_sucursal_destino id_tipotambor_destino nromov_destino id_calidad ~
id_envase kilos_400 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE asindustria
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttubicaciontambores.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ttUbicacionTambores NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ttUbicacionTambores NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ttUbicacionTambores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ttUbicacionTambores


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAjustesOE dTables  _DB-REQUIRED
FUNCTION getAjustesOE RETURNS CHARACTER
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArrastreInfo dTables  _DB-REQUIRED
FUNCTION getArrastreInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDestinoReproceso dTables  _DB-REQUIRED
FUNCTION getDestinoReproceso RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFacturasInfo dTables  _DB-REQUIRED
FUNCTION getFacturasInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosOfArticulo dTables  _DB-REQUIRED
FUNCTION getKilosOfArticulo RETURNS CHARACTER
  (piArt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrigenProducciones dTables  _DB-REQUIRED
FUNCTION getOrigenProducciones RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReleasesInfo dTables  _DB-REQUIRED
FUNCTION getReleasesInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRemitosInfo dTables  _DB-REQUIRED
FUNCTION getRemitosInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRemitosInfoOld dTables  _DB-REQUIRED
FUNCTION getRemitosInfoOld RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSobranteInfo dTables  _DB-REQUIRED
FUNCTION getSobranteInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUbicacionTambores dTables  _DB-REQUIRED
FUNCTION getUbicacionTambores RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ttUbicacionTambores SCROLLING.
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
   Partition: asindustria
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
   Temp-Tables and Buffers:
      TABLE: ttUbicacionTambores T "?" NO-UNDO tablasTemp ubicaciontambores
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
     _TblList          = "Temp-Tables.ttUbicacionTambores"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ttUbicacionTambores.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 no
     _FldNameList[2]   > Temp-Tables.ttUbicacionTambores.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 no
     _FldNameList[3]   > Temp-Tables.ttUbicacionTambores.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 no
     _FldNameList[4]   > Temp-Tables.ttUbicacionTambores.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[5]   > Temp-Tables.ttUbicacionTambores.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 21 no
     _FldNameList[6]   > Temp-Tables.ttUbicacionTambores.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[7]   > Temp-Tables.ttUbicacionTambores.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no
     _FldNameList[8]   > Temp-Tables.ttUbicacionTambores.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? yes ? no 15 no
     _FldNameList[9]   > Temp-Tables.ttUbicacionTambores.articulo
"articulo" "articulo" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[10]   > Temp-Tables.ttUbicacionTambores.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[11]   > Temp-Tables.ttUbicacionTambores.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[12]   > Temp-Tables.ttUbicacionTambores.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[13]   > Temp-Tables.ttUbicacionTambores.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[14]   > Temp-Tables.ttUbicacionTambores.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[15]   > Temp-Tables.ttUbicacionTambores.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no
     _FldNameList[16]   > Temp-Tables.ttUbicacionTambores.id_empresa_destino
"id_empresa_destino" "id_empresa_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 17.6 no
     _FldNameList[17]   > Temp-Tables.ttUbicacionTambores.id_sucursal_destino
"id_sucursal_destino" "id_sucursal_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 no
     _FldNameList[18]   > Temp-Tables.ttUbicacionTambores.id_tipotambor_destino
"id_tipotambor_destino" "id_tipotambor_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 no
     _FldNameList[19]   > Temp-Tables.ttUbicacionTambores.nromov_destino
"nromov_destino" "nromov_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[20]   > Temp-Tables.ttUbicacionTambores.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[21]   > Temp-Tables.ttUbicacionTambores.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[22]   > Temp-Tables.ttUbicacionTambores.kilos_400
"kilos_400" "kilos_400" ? ? "decimal" ? ? ? ? ? ? yes ? no 15 no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addLote dTables  _DB-REQUIRED
PROCEDURE addLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEst AS INTEGER    NO-UNDO.

  DEFINE VARIABLE c    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE k4   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cPro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.



  FOR EACH tambores_industria 
      WHERE tambores_industria.id_empresa     = piEmp
        AND tambores_industria.id_sucursal    = piSuc
        AND tambores_industria.id_tipotambor  = piTip
        AND tambores_industria.nromov         = piNro
        AND (IF (piTip = 6 OR piTip = 7) THEN TRUE ELSE tambores_industria.id_estado = piEst)
      BREAK BY tambores_industria.nromov.
    c = c + 1.
    k = k + tambores_industria.kilos_tambor.
    IF LAST-OF(tambores_industria.nromov) THEN DO:

      FOR FIRST productos_terminados OF tambores_industria NO-LOCK.
        cPro = productos_terminados.descripcion.
      END.
      FOR FIRST calidades OF tambores_industria NO-LOCK.
        cCal = calidades.descripcion.
      END.
      FOR FIRST envases_prod OF tambores_industria NO-LOCK.
        cEnv = envases_prod.descripcion.
      END.
      k4 = DYNAMIC-FUNCTION('getKilos400' IN hLib, tambores_industria.id_tipotambor, tambores_industria.id_articulo, tambores_industria.id_calidad, k).
      CREATE ttUbicacionTambores.
      ASSIGN ttUbicacionTambores.id_empresa            = tambores_industria.id_empresa
             ttUbicacionTambores.id_sucursal           = tambores_industria.id_sucursal
             ttUbicacionTambores.id_tipotambor         = tambores_industria.id_tipotambor
             ttUbicacionTambores.nromov                = tambores_industria.nromov
             ttUbicacionTambores.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
             ttUbicacionTambores.id_lote               = tambores_industria.id_lote
             ttUbicacionTambores.anio                  = tambores_industria.anio
             ttUbicacionTambores.id_lote_deposito      = tambores_industria.id_lote_deposito
             ttUbicacionTambores.id_articulo           = tambores_industria.id_articulo
             ttUbicacionTambores.id_calidad            = tambores_industria.id_calidad
             ttUbicacionTambores.articulo              = cPro
             ttUbicacionTambores.calidad               = cCal
             ttUbicacionTambores.envase                = cEnv
             ttUbicacionTambores.cantidad              = c
             ttUbicacionTambores.kilos                 = k
             ttUbicaciontambores.kilos_400             = k4
             ttUbicacionTambores.fecha                 = tambores_industria.fecha
             ttUbicacionTambores.id_empresa_destino    = tambores_industria.id_empresa_destino
             ttUbicaciontambores.id_sucursal_destino   = tambores_industria.id_sucursal_destino
             ttUbicacionTambores.id_tipotambor_destino = tambores_industria.id_tipotambor_destino
             ttUbicacionTambores.nromov_destino        = tambores_industria.nromov_destino.
      c = 0.
      k = 0.
    END.
  END.

  FOR FIRST lotes_cascara 
      WHERE lotes_cascara.id_empresa    = piEmp
        AND lotes_cascara.id_sucursal   = piSuc
        AND lotes_cascara.id_tipotambor = piTip
        AND lotes_cascara.nromov        = piNro
      NO-LOCK.

      FOR FIRST productos_terminados OF lotes_cascara NO-LOCK.
        cPro = productos_terminados.descripcion.
      END.
      FOR FIRST calidades OF lotes_cascara NO-LOCK.
        cCal = calidades.descripcion.
      END.
      FOR FIRST envases_prod OF lotes_cascara NO-LOCK.
        cEnv = envases_prod.descripcion.
      END.

      c = 0.
      FOR EACH r_produccion_cascara_lote WHERE r_produccion_cascara_lote.id_empresa_lote    = lotes_cascara.id_empresa
                                       AND r_produccion_cascara_lote.id_sucursal_lote   = lotes_cascara.id_sucursal
                                       AND r_produccion_cascara_lote.id_tipotambor_lote = lotes_cascara.id_tipotambor
                                       AND r_produccion_cascara_lote.nromov_lote        = lotes_cascara.nromov
                                     NO-LOCK.
        c = c + r_produccion_cascara_lote.cantidad.
      END.

      CREATE ttUbicacionTambores.
      ASSIGN ttUbicacionTambores.id_empresa            = lotes_cascara.id_empresa
             ttUbicacionTambores.id_sucursal           = lotes_cascara.id_sucursal
             ttUbicacionTambores.id_tipotambor         = lotes_cascara.id_tipotambor
             ttUbicacionTambores.nromov                = lotes_cascara.nromov
             ttUbicacionTambores.id_sucursal_ubicacion = 0
             ttUbicacionTambores.id_lote               = lotes_cascara.id_lote
             ttUbicacionTambores.anio                  = lotes_cascara.anio
             ttUbicacionTambores.id_lote_deposito      = ""
             ttUbicacionTambores.id_articulo           = lotes_cascara.id_articulo
             ttUbicacionTambores.id_calidad            = lotes_cascara.id_calidad
             ttUbicacionTambores.articulo              = cPro
             ttUbicacionTambores.calidad               = cCal
             ttUbicacionTambores.envase                = cEnv
             ttUbicacionTambores.cantidad              = c
             ttUbicacionTambores.kilos                 = c * 50
             ttUbicacionTambores.fecha                 = lotes_cascara.fecha
             ttUbicacionTambores.id_empresa_destino    = 0
             ttUbicaciontambores.id_sucursal_destino   = 0
             ttUbicacionTambores.id_tipotambor_destino = 0
             ttUbicacionTambores.nromov_destino        = 0
             .
  END.

  FOR EACH produccion_cascara
      WHERE produccion_cascara.id_empresa    = piEmp
        AND produccion_cascara.id_sucursal   = piSuc
        AND produccion_cascara.id_tipotambor = piTip
        AND produccion_cascara.nromov        = piNro
      NO-LOCK.

    FOR FIRST productos_terminados OF produccion_cascara NO-LOCK.
        cPro = productos_terminados.descripcion.
      END.
      

      c = produccion_cascara.cantidad.
      CREATE ttUbicacionTambores.
      ASSIGN ttUbicacionTambores.id_empresa            = produccion_cascara.id_empresa
             ttUbicacionTambores.id_sucursal           = produccion_cascara.id_sucursal
             ttUbicacionTambores.id_tipotambor         = produccion_cascara.id_tipotambor
             ttUbicacionTambores.nromov                = produccion_cascara.nromov
             ttUbicacionTambores.id_sucursal_ubicacion = 0
             ttUbicacionTambores.id_lote               = produccion_cascara.id_produccion
             ttUbicacionTambores.anio                  = produccion_cascara.anio
             ttUbicacionTambores.id_lote_deposito      = ""
             ttUbicacionTambores.id_articulo           = produccion_cascara.id_articulo
             ttUbicacionTambores.articulo              = cPro
             ttUbicacionTambores.calidad               = ""
             ttUbicacionTambores.envase                = ""
             ttUbicacionTambores.cantidad              = c
             ttUbicacionTambores.kilos                 = c * 50
             ttUbicacionTambores.fecha                 = produccion_cascara.fecha
             ttUbicacionTambores.id_empresa_destino    = 0
             ttUbicaciontambores.id_sucursal_destino   = 0
             ttUbicacionTambores.id_tipotambor_destino = 0
             ttUbicacionTambores.nromov_destino        = 0
             .

  END.


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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearTT dTables  _DB-REQUIRED
PROCEDURE clearTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH ttUbicacionTambores.
    DELETE ttUbicacionTambores.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportParte dTables  _DB-REQUIRED
PROCEDURE exportParte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.

  /********* Variables de Excel **************/

  DEFINE VAR chExcelAplication AS COM-HANDLE.
  DEFINE VAR chWorkbook        AS COM-HANDLE.
  DEFINE VAR chWorkSheet       AS COM-HANDLE.
  DEFINE VAR chchart           AS COM-HANDLE.
  DEFINE VAR chWorkSheetRange  AS COM-HANDLE.

  DEFINE VAR cFila  AS CHARACTER.
  DEFINE VAR cRange AS CHARACTER.
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j AS INTEGER    NO-UNDO.

  CREATE "Excel.Application" chExcelAplication.
  
  chExcelAplication:VISIBLE = TRUE.
  
  chWorkbook  = chExcelAplication:Workbooks:OPEN('..\industria\tplParteDiario.xls'). 
  chWorkSheet = chExcelAplication:Sheets:ITEM(1).

  cRange = "A1".
  chWorkSheet:Range(cRange):Value = "Fecha" .
  cRange = "B1".
  chWorkSheet:Range(cRange):Value = "'" + STRING(pdFec).

  cRange = "A40".
  chWorkSheet:Range(cRange):Value = "Fecha" .
  cRange = "B40".
  chWorkSheet:Range(cRange):Value = "'" + STRING(pdFec - 365).

  
  /* lotes jugo turbio */
  RUN putProducto(0, 52, 0, 7, "B", chWorkSheet).
  /* lotes jugo claro */
  RUN putProducto(0, 53, 0, 7, "F", chWorkSheet).
  /* pulpa */
  RUN putProducto(0, 71, 0, 7, "J", chWorkSheet).
  /* ael */
  RUN putProducto(0, 51, 0, 7, "N", chWorkSheet).
  /* cascara */
  RUN putProducto(11, 54, 798, 7, "V", chWorkSheet).
  RUN putProducto(11, 54, 799, 7, "Z", chWorkSheet).
  
  i = IF iFila > 9 THEN iFila ELSE 9.
  j = i.
  cRange = "A" + STRING(i).
  chWorkSheet:Range(cRange):Value = "Produccion" .
  /* produccion jugo turbio */
  RUN putProducto(0, 521, 0, i, "B", chWorkSheet).
  /* produccion jugo claro */
  RUN putProducto(0, 532, 0, i, "F", chWorkSheet).
  
  i = j.
  /* produccion aceite */
  RUN putProducto(0, 512, 0, i, "N", chWorkSheet).
  RUN putProducto(0, 501, 0, iFila, "N", chWorkSheet).
  RUN putProducto(0, 581, 0, iFila, "N", chWorkSheet).

  RUN putProducto(0, 58, 0, i, "R", chWorkSheet).
  RUN putProducto(0, 59, 0, iFila, "R", chWorkSheet).
  RUN putProducto(0, 762, 0, iFila, "R", chWorkSheet).

  /* produccion cascara */
  i = j.
  RUN putProducto(12, 54, 0, i, "V", chWorkSheet).








  

 
  IF VALID-HANDLE(chExcelAplication) THEN RELEASE OBJECT chExcelAplication.
  IF VALID-HANDLE(chWorkBook)        THEN RELEASE OBJECT chWorkBook.
  IF VALID-HANDLE(chWorkSheet)       THEN RELEASE OBJECT chWorkSheet. 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillData dTables  _DB-REQUIRED
PROCEDURE fillData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piAnio AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipo AS INTEGER    NO-UNDO.

  DEFINE VARIABLE c    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cPro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.

  FOR EACH ttUbicacionTambores.
    DELETE ttUbicacionTambores.    
  END.


  FOR EACH tambores_industria WHERE tambores_industria.fecha >= DATE("01/01/" + STRING(piAnio))
                                AND tambores_industria.fecha <= DATE("31/12/" + STRING(piAnio))
                                AND (IF piTipo <> 0 THEN tambores_industria.id_tipotambor = piTipo ELSE (tambores_industria.id_tipotambor = 3 OR tambores_industria.id_tipotambor = 6))
                              BREAK BY tambores_industria.nromov.
    c = c + 1.
    k = k + tambores_industria.kilos_tambor.
    IF LAST-OF(tambores_industria.nromov) THEN DO:

      FOR FIRST productos_terminados OF tambores_industria NO-LOCK.
        cPro = productos_terminados.descripcion.
      END.
      FOR FIRST calidades OF tambores_industria NO-LOCK.
        cCal = calidades.descripcion.
      END.
      FOR FIRST envases_prod OF tambores_industria NO-LOCK.
        cEnv = envases_prod.descripcion.
      END.

      CREATE ttUbicacionTambores.
      ASSIGN ttUbicacionTambores.id_empresa            = tambores_industria.id_empresa
             ttUbicacionTambores.id_sucursal           = tambores_industria.id_sucursal
             ttUbicacionTambores.id_tipotambor         = tambores_industria.id_tipotambor
             ttUbicacionTambores.nromov                = tambores_industria.nromov
             ttUbicacionTambores.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
             ttUbicacionTambores.id_lote               = IF (piTipo = 2 AND tambores_industria.id_lote = 0) THEN tambores_industria.id_tambor ELSE tambores_industria.id_lote
             ttUbicacionTambores.anio                  = tambores_industria.anio
             ttUbicacionTambores.id_lote_deposito      = tambores_industria.id_lote_deposito
             ttUbicacionTambores.id_articulo           = tambores_industria.id_articulo
             ttUbicacionTambores.articulo              = cPro
             ttUbicacionTambores.calidad               = cCal
             ttUbicacionTambores.envase                = cEnv
             ttUbicacionTambores.cantidad              = c
             ttUbicacionTambores.kilos                 = k
             ttUbicacionTambores.fecha                 = tambores_industria.fecha
             ttUbicacionTambores.id_empresa_destino    = tambores_industria.id_empresa_destino
             ttUbicaciontambores.id_sucursal_destino   = tambores_industria.id_sucursal_destino
             ttUbicacionTambores.id_tipotambor_destino = tambores_industria.id_tipotambor_destino
             ttUbicacionTambores.nromov_destino        = tambores_industria.nromov_destino.
      c = 0.
      k = 0.
    END.
  END.

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

  /*RUN fillData (2025).*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putProducto dTables  _DB-REQUIRED
PROCEDURE putProducto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArt AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCal AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piRow AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcCol AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER phExc AS COM-HANDLE     NO-UNDO.

  DEFINE VARIABLE cRange AS CHARACTER  NO-UNDO.

  iFila = piRow.
  FOR EACH ttUbicacionTambores
      WHERE id_articulo = piArt
        AND (IF piTip = 0 THEN TRUE ELSE id_tipotambor = piTip)
        AND (IF piCal = 0 THEN TRUE ELSE id_calidad = piCal)
      NO-LOCK.
    cRange = pcCol + STRING(iFila).
    phExc:Range(cRange):Value = "'" + STRING(ttUbicacionTambores.id_lote) +  "/" + STRING(ttUbicacionTambores.anio).
    cRange = phExc:Range(cRange):NEXT:Address(0, 0, 1, 0, 0).
    phExc:Range(cRange):Value = STRING(ttUbicacionTambores.cantidad) .
    cRange = phExc:Range(cRange):NEXT:Address(0, 0, 1, 0, 0).
    phExc:Range(cRange):Value = STRING(ttUbicacionTambores.kilos) .
    cRange = phExc:Range(cRange):NEXT:Address(0, 0, 1, 0, 0).
    phExc:Range(cRange):Value = STRING(ttUbicacionTambores.kilos_400) .


    iFila = iFila + 1.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startFilter dTables  _DB-REQUIRED
PROCEDURE startFilter :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAjustesOE dTables  _DB-REQUIRED
FUNCTION getAjustesOE RETURNS CHARACTER
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR EACH ajustes_oe
      WHERE ajustes_oe.id_empresa    = rowObject.id_empresa
        AND ajustes_oe.id_sucursal   = rowObject.id_sucursal
        AND ajustes_oe.id_tipotambor = rowObject.id_tipotambor
        AND ajustes_oe.nromov        = rowObject.nromov
      NO-LOCK.

    cRet = cRet + 
           STRING(ajustes_oe.id_ajuste) + "," +
           STRING(ajustes_oe.id_orden_entrega) + "," +
           STRING(ajustes_oe.ITEM_oe) + "," +
           STRING(ajustes_oe.id_orden_entrega_new) + "," +
           STRING(ajustes_oe.ITEM_oe_new) + "," +
           ajustes_oe.c_usuario + "," +
           STRING(ajustes_oe.c_fecha) + "," +
           CHR(14).
    
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArrastreInfo dTables  _DB-REQUIRED
FUNCTION getArrastreInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  FOR EACH arrastre WHERE arrastre.id_empresa    = rowObject.id_empresa
                      AND arrastre.id_sucursal   = rowObject.id_sucursal
                      AND arrastre.id_tipotambor = rowObject.id_tipotambor
                      AND arrastre.nromov        = rowObject.nromov
                    NO-LOCK.
    FOR EACH tambores_industria WHERE arrastre.nromov_arrastre = tambores_industria.nromov 
                                NO-LOCK.
      i = i + 1.
      k = k + tambores_industria.kilos_tambor.
    END.
    cRet = cRet + 
           STRING(arrastre.id_arrastre) + "," +
           STRING(i) + "," + 
           STRING(k) + "," + 
           STRING(arrastre.id_lote) + 
           CHR(14).
    i = 0.
    k = 0.
    
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDestinoReproceso dTables  _DB-REQUIRED
FUNCTION getDestinoReproceso RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAno AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFec AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dF1  AS DATE       NO-UNDO.
  DEFINE VARIABLE dF2  AS DATE       NO-UNDO.
  DEFINE VARIABLE dF3  AS DATE       NO-UNDO.

  DEFINE BUFFER buTI  FOR tambores_industria.
  DEFINE BUFFER buCga FOR tambores_industria.
  DEFINE BUFFER buJgo FOR lotes_jugo.
  DEFINE BUFFER buAce FOR lotes_aceite.
  DEFINE BUFFER buPro FOR proceso.

  FIND CURRENT rowObject NO-ERROR.

  i = 0.
  k = 0.
  cRet = "".

  FOR EACH tambores_industria NO-LOCK 
      WHERE tambores_industria.id_empresa      = rowObject.id_empresa
        AND tambores_industria.id_sucursal     = rowObject.id_sucursal
        AND tambores_industria.id_tipotambor   = rowObject.id_tipotambor
        AND tambores_industria.nromov          = rowObject.nromov
        AND tambores_industria.nromov_destino <> 0
      BREAK BY tambores_industria.nromov_destino.
    i = i + 1.
    k = k + tambores_industria.kilos_tambor.
    IF LAST-OF(tambores_industria.nromov_destino) THEN DO:      
      IF tambores_industria.id_tipotambor_destino <> 10  THEN DO: /*reprocesado en lotes*/      
        FIND FIRST buTi WHERE tambores_industria.nromov_destino = buTi.nromov NO-LOCK NO-ERROR.      
        IF AVAILABLE buTI THEN DO:
          cFec = IF (tambores_industria.fecha_reproceso <> ?) THEN STRING(tambores_industria.fecha_reproceso) ELSE "Error en fecha Rep.".

          FIND FIRST productos_terminados OF buTI NO-LOCK NO-ERROR.
          FIND FIRST sucursales OF buTi NO-LOCK NO-ERROR.
          FIND FIRST lotes_jugo OF buTi NO-LOCK NO-ERROR.
          FIND FIRST lotes_aceite OF buTi NO-LOCK NO-ERROR.
          IF AVAILABLE lotes_jugo THEN 
            ASSIGN dF1 = lotes_jugo.fecha
                   dF2 = lotes_jugo.fecha_comienzo
                   dF3 = IF lotes_jugo.fecha_finalizacion <> ? THEN lotes_jugo.fecha_finalizacion ELSE lotes_jugo.fecha_comienzo
                   .
          IF AVAILABLE lotes_aceite THEN 
            ASSIGN dF1 = lotes_aceite.fecha
                   dF2 = lotes_aceite.fecha_comienzo
                   dF3 = lotes_aceite.fecha_finalizacion
                   .

          cRet = cRet + 
                 STRING(buTI.id_lote) + "," + 
                 STRING(buTI.anio) + "," + 
                 STRING(i) + "," + 
                 productos_terminados.descripcion + "," + 
                 STRING(buTI.id_empresa) + "," +
                 STRING(buTI.id_sucursal) + "," + 
                 STRING(buTI.id_tipotambor) + "," +
                 STRING(buTI.nromov) + "," +
                 cFec + "," + 
                 sucursales.abreviatura + "," + 
                 "-" + "," + 
                 STRING(k) + "," +
                 STRING(buTI.fecha) + "," + 
                 STRING(dF1) + "," + 
                 STRING(dF2) + "," + 
                 STRING(dF3) +
                 CHR(14).
        END.
        i = 0.
        k = 0.
      END.
      ELSE DO: /*reprocesado en cargas*/
        FIND FIRST cargas WHERE cargas.nromov = tambores_industria.nromov_destino NO-LOCK NO-ERROR.
        FIND FIRST sucursales OF cargas NO-LOCK NO-ERROR.
        FIND FIRST productos_terminados OF cargas NO-LOCK NO-ERROR.
        FIND FIRST proceso WHERE proceso.nromov = cargas.nromov_proceso NO-LOCK NO-ERROR.
        IF AVAILABLE proceso THEN
          ASSIGN dF1 = proceso.fecha
                 dF2 = proceso.fecha_inicio
                 dF3 = IF proceso.fecha_fin <> ? THEN proceso.fecha_fin ELSE proceso.fecha_inicio
                 .

        IF AVAILABLE cargas THEN DO:
          cAno = STRING(YEAR(cargas.fecha)).
          cFec = IF (tambores_industria.fecha_reproceso <> ?) THEN STRING(tambores_industria.fecha_reproceso) ELSE "Error en fecha Rep.".
          cRet = cRet + 
                 STRING(cargas.id_carga) + "," + 
                 cAno + "," + 
                 STRING(i) + "," + 
                 productos_terminados.descripcion + "," + 
                 STRING(cargas.id_empresa) + "," +
                 STRING(cargas.id_sucursal) + "," + 
                 STRING(cargas.id_tipotambor) + "," +
                 STRING(cargas.nromov) + "," +
                 cFec + "," + 
                 sucursales.abreviatura + "," + 
                 STRING(cargas.id_proceso) + "," + 
                 STRING(k) + "," + 
                 STRING(cargas.fecha) + "," +
                 STRING(dF1) + "," + 
                 STRING(dF2) + "," + 
                 STRING(dF3) + 
                 CHR(14).
  
  
          i = 0.
          k = 0.
        END.
      END.
    END.    
  END.

  /*
  i = 0.
  FOR EACH buCga WHERE buCga.id_empresa      = rowObject.id_empresa
                   AND buCga.id_sucursal     = rowObject.id_sucursal
                   AND buCga.id_tipotambor   = rowObject.id_tipotambor
                   AND buCga.nromov          = rowObject.nromov
                   AND buCga.nromov_destino <> 0
                 BREAK BY buCga.nromov_destino.
    i = i + 1.
    IF LAST-OF(buCga.nromov_destino) THEN DO:

      FIND FIRST cargas WHERE cargas.nromov = buCga.nromov_destino NO-LOCK NO-ERROR.
      FIND FIRST sucursales OF cargas NO-LOCK NO-ERROR.
      FIND FIRST productos_terminados OF cargas NO-LOCK NO-ERROR.
      IF AVAILABLE cargas THEN DO:
        cAno = STRING(cargas.anio).
        cRet = cRet + 
               STRING(cargas.id_carga) + "," + 
               cAno + "," + 
               STRING(i) + "," + 
               productos_terminados.descripcion + "," + 
               STRING(cargas.id_empresa) + "," +
               STRING(cargas.id_sucursal) + "," + 
               STRING(cargas.id_tipotambor) + "," +
               STRING(cargas.nromov) + "," +
               STRING(cargas.fecha) + "," + 
               sucursales.abreviatura + 
               CHR(14).


        i = 0.
      END.

    END.
  END.
  */

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFacturasInfo dTables  _DB-REQUIRED
FUNCTION getFacturasInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOE  AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buSuc FOR sucursales.

  FIND CURRENT rowObject NO-ERROR.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = rowObject.id_empresa
                                AND tambores_industria.id_sucursal   = rowObject.id_sucursal
                                AND tambores_industria.id_tipotambor = rowObject.id_tipotambor
                                AND tambores_industria.nromov        = rowObject.nromov
                              BREAK BY tambores_industria.nro_remito.
    i = i + 1.
    IF LAST-OF(tambores_industria.nro_remito) THEN DO:
      FIND FIRST remitos WHERE remitos.id_sucursal    = tambores_industria.id_sucursal_remito
                           AND remitos.id_tipo_movsto = tambores_industria.id_tipo_movsto
                           AND remitos.nro            = tambores_industria.nro_remito
                         NO-LOCK NO-ERROR.
      FIND FIRST items_factura OF remitos WHERE remitos.ITEM = tambores_industria.ITEM_factura 
                                          NO-LOCK NO-ERROR.
      FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
      FIND FIRST buSuc WHERE lugar_descarga.id_sucursal = buSuc.id_sucursal NO-LOCK NO-ERROR.
      FIND FIRST sucursales OF tambores_industria NO-LOCK NO-ERROR.
      cSuc = IF AVAILABLE buSuc THEN buSuc.abreviatura ELSE "ERR SUC DEST.".
      FIND FIRST items_orden_entrega OF remitos NO-LOCK NO-ERROR.
      FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
      FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
      IF AVAILABLE clientes  THEN DO:
        cCli = clientes.razon_social.
        cOe  = string(remitos.id_orden_entrega).
      END.
      IF AVAILABLE remitos THEN DO:      
        cRet = cRet + 
               sucursales.abreviatura + "," + 
               cSuc + "," +
               STRING(remitos.fecha) + "," +             
               STRING(i) + "," + 
               STRING(remitos.id_tipo_movsto) + "," +
               STRING(remitos.estado) + "," + 
               remitos.c_usuario + "," +
               STRING(remitos.nro) + "," + 
               STRING(remitos.nro_comp, "9999-99999999") + "," + 
               cOe + "," + 
               cCli +               
               CHR(14).
      END.
      i = 0.
    END.
    
  END.


  RETURN cRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosOfArticulo dTables  _DB-REQUIRED
FUNCTION getKilosOfArticulo RETURNS CHARACTER
  (piArt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  DEFINE VARIABLE iCan AS INTEGER    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fKi4 AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE BUFFER buRow FOR rowObject.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  FOR EACH buRow
      WHERE buRow.id_articulo = piArt
      NO-LOCK.

    FIND FIRST tambores_industria WHERE tambores_industria.nromov = buRow.nromov NO-LOCK NO-ERROR.

    iCan = iCan + buRow.cantidad.
    fKil = fKil + buRow.kilos.
    fKi4 = fKi4 + DYNAMIC-FUNCTION('getKilos400' IN hLib, buRow.id_tipotambor, buRow.id_articulo, tambores_industria.id_calidad, buRow.kilos).


  END.


  cRet = STRING(iCan) + CHR(1) + STRING(fKil) + CHR(1) + STRING(fKi4).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrigenProducciones dTables  _DB-REQUIRED
FUNCTION getOrigenProducciones RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  FOR EACH tambores_industria NO-LOCK 
      WHERE tambores_industria.id_empresa_destino    = rowObject.id_empresa
        AND tambores_industria.id_sucursal_destino   = rowObject.id_sucursal
        AND tambores_industria.id_tipotambor_destino = rowObject.id_tipotambor
        AND tambores_industria.nromov_destino        = rowObject.nromov
      BREAK BY tambores_industria.nromov.
    i = i + 1.
    k = k + tambores_industria.kilos_tambor.
    IF LAST-OF(tambores_industria.nromov) THEN DO:
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      cRet = cRet + 
             STRING(tambores_industria.id_lote) + "," + 
             STRING(tambores_industria.anio) + "," + 
             STRING(i) + "," + 
             productos_terminados.descripcion + "," + 
             STRING(tambores_industria.id_empresa) + "," +
             STRING(tambores_industria.id_sucursal) + "," + 
             STRING(tambores_industria.id_tipotambor) + "," +
             STRING(tambores_industria.nromov) + "," + 
             STRING(k) + "," + 
             calidades.descripcion + "," + 
             STRING(tambores_industria.fecha) + 
             CHR(14).
      i = 0.
      k = 0.
    END.
    
  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReleasesInfo dTables  _DB-REQUIRED
FUNCTION getReleasesInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOE  AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buSuc FOR sucursales.
  DEFINE BUFFER buRel FOR items_release_delivery.

  FIND CURRENT rowObject NO-ERROR.

  /*cRet = "0,0,0,0,0,0" + CHR(14).*/

  FOR EACH buRel NO-LOCK 
      WHERE buRel.id_empresa    = rowObject.id_empresa
        AND buRel.id_sucursal   = rowObject.id_sucursal
        AND buRel.id_tipotambor = rowObject.id_tipotambor
        AND buRel.nromov        = rowObject.nromov.

    FIND FIRST RELEASE_delivery OF buRel NO-LOCK NO-ERROR.
    FIND FIRST sucursales WHERE RELEASE_delivery.id_sucursal_ubicacion = sucursales.id_sucursal NO-LOCK NO-ERROR.    
    FIND FIRST clientes OF RELEASE_delivery NO-LOCK NO-ERROR.

    IF AVAILABLE clientes  THEN DO:
      cCli = clientes.razon_social.
    END.
    cSuc = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "ERR SUC REL.".

    IF AVAILABLE RELEASE_delivery THEN DO:
      cRet = cRet + 
             STRING(RELEASE_delivery.numero_release) + "," + 
             cSuc + "," +
             cCli + "," + 
             buRel.id_lote_deposito + "," + 
             STRING(buRel.tambores) + "," + 
             STRING(RELEASE_delivery.fecha_entrega) + "," + 
             STRING(RELEASE_delivery.fecha_confirmacion_entrega) +
             CHR(14).

    END.
  END.
                                      
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRemitosInfo dTables  _DB-REQUIRED
FUNCTION getRemitosInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOE  AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buSuc FOR sucursales.

  FIND CURRENT rowObject NO-ERROR.

  FOR EACH r_tambor_remito 
      WHERE r_tambor_remito.id_empresa    = rowObject.id_empresa
        AND r_tambor_remito.id_sucursal   = rowObject.id_sucursal
        AND r_tambor_remito.id_tipotambor = rowObject.id_tipotambor
        AND r_tambor_remito.nromov        = rowObject.nromov
      NO-LOCK, 
      FIRST items_factura NO-LOCK
      WHERE items_factura.id_sucursal    = r_tambor_remito.id_sucursal_remito
        AND items_factura.id_tipo_movsto = r_tambor_remito.id_tipo_movsto
        AND items_factura.nro            = r_tambor_remito.nro_remito
        AND items_factura.ITEM           = r_tambor_remito.ITEM_factura
      BREAK BY r_tambor_remito.nro_remito.                           
    i = i + 1.
    IF LAST-OF(r_tambor_remito.nro_remito) THEN DO:

      FIND FIRST remitos 
           WHERE remitos.id_sucursal    = r_tambor_remito.id_sucursal_remito
             AND remitos.id_tipo_movsto = r_tambor_remito.id_tipo_movsto
             AND remitos.nro            = r_tambor_remito.nro_remito
           NO-LOCK NO-ERROR.
     
      FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
      FIND FIRST buSuc WHERE lugar_descarga.id_sucursal = buSuc.id_sucursal NO-LOCK NO-ERROR.
      FIND FIRST sucursales WHERE sucursales.id_sucursal = r_tambor_remito.id_sucursal_remito NO-LOCK NO-ERROR.
      cSuc = IF AVAILABLE buSuc THEN buSuc.abreviatura ELSE "ERR SUC DEST.".
      FIND FIRST items_orden_entrega OF remitos NO-LOCK NO-ERROR.
      FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
      FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
      IF AVAILABLE clientes  THEN DO:
        cCli = clientes.razon_social.
        cOe  = string(remitos.id_orden_entrega).
      END.
     
      IF AVAILABLE remitos  THEN DO:      
        cRet = cRet + 
               sucursales.abreviatura + "," + 
               cSuc + "," +
               STRING(remitos.fecha) + "," +             
               STRING(i) + "," + 
               STRING(remitos.id_tipo_movsto) + "," +
               STRING(remitos.estado) + "," + 
               remitos.c_usuario + "," +
               STRING(remitos.nro) + "," + 
               STRING(remitos.nro_comp, "9999-99999999") + "," + 
               cOe + "," + 
               cCli +               
               CHR(14).
      END.
      i = 0.
    END.
    
  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRemitosInfoOld dTables  _DB-REQUIRED
FUNCTION getRemitosInfoOld RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSuc AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOE  AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buSuc FOR sucursales.

  FIND CURRENT rowObject NO-ERROR.

  FOR EACH tambores_industria WHERE tambores_industria.id_empresa    = rowObject.id_empresa
                                AND tambores_industria.id_sucursal   = rowObject.id_sucursal
                                AND tambores_industria.id_tipotambor = rowObject.id_tipotambor
                                AND tambores_industria.nromov        = rowObject.nromov
                              BREAK BY tambores_industria.nro_remito.
    i = i + 1.
    IF LAST-OF(tambores_industria.nro_remito) THEN DO:
      FIND FIRST remitos WHERE remitos.id_sucursal    = tambores_industria.id_sucursal_remito
                           AND remitos.id_tipo_movsto = tambores_industria.id_tipo_movsto
                           AND remitos.nro            = tambores_industria.nro_remito
                         NO-LOCK NO-ERROR.
      FIND FIRST items_factura OF remitos WHERE remitos.ITEM = tambores_industria.ITEM_factura 
                                          NO-LOCK NO-ERROR.
      FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
      FIND FIRST buSuc WHERE lugar_descarga.id_sucursal = buSuc.id_sucursal NO-LOCK NO-ERROR.
      FIND FIRST sucursales OF tambores_industria NO-LOCK NO-ERROR.
      cSuc = IF AVAILABLE buSuc THEN buSuc.abreviatura ELSE "ERR SUC DEST.".
      FIND FIRST items_orden_entrega OF remitos NO-LOCK NO-ERROR.
      FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
      FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
      IF AVAILABLE clientes  THEN DO:
        cCli = clientes.razon_social.
        cOe  = string(remitos.id_orden_entrega).
      END.
      IF AVAILABLE remitos THEN DO:      
        cRet = cRet + 
               sucursales.abreviatura + "," + 
               cSuc + "," +
               STRING(remitos.fecha) + "," +             
               STRING(i) + "," + 
               STRING(remitos.id_tipo_movsto) + "," +
               STRING(remitos.estado) + "," + 
               remitos.c_usuario + "," +
               STRING(remitos.nro) + "," + 
               STRING(remitos.nro_comp, "9999-99999999") + "," + 
               cOe + "," + 
               cCli +               
               CHR(14).
      END.
      i = 0.
    END.
    
  END.


  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSobranteInfo dTables  _DB-REQUIRED
FUNCTION getSobranteInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND CURRENT rowObject NO-ERROR.

  IF rowObject.id_tipotambor = 3  THEN DO:
    FOR EACH sobrante WHERE sobrante.id_empresa    = rowObject.id_empresa
                        AND sobrante.id_sucursal   = rowObject.id_sucursal
                        AND sobrante.id_tipotambor = rowObject.id_tipotambor
                        AND sobrante.nromov        = rowObject.nromov
                      NO-LOCK.
      FOR EACH tambores_industria WHERE sobrante.nromov_sobrante = tambores_industria.nromov 
                                  NO-LOCK.
        i = i + 1.
        k = k + tambores_industria.kilos_tambor.
      END.
      cRet = cRet + 
             STRING(sobrante.id_sobrante) + "," +
             STRING(i) + "," + 
             STRING(k) + ", " + 
             STRING(sobrante.id_lote) + 
             CHR(14).
      i = 0.
      k = 0.      
    END.
  END.  

  IF rowObject.id_tipotambor = 6 THEN DO:
    FOR EACH sobrante_lotes_aceite WHERE sobrante_lotes_aceite.id_empresa    = rowObject.id_empresa
                                     AND sobrante_lotes_aceite.id_sucursal   = rowObject.id_sucursal
                                     AND sobrante_lotes_aceite.id_tipotambor = rowObject.id_tipotambor
                                     AND sobrante_lotes_aceite.nromov        = rowObject.nromov
                                   NO-LOCK.
      FOR EACH tambores_industria WHERE sobrante_lotes_aceite.nromov_sobrante = tambores_industria.nromov 
                                  NO-LOCK.
        i = i + 1.
        k = k + tambores_industria.kilos_tambor.
      END.
      cRet = cRet + 
             STRING(sobrante_lotes_aceite.id_sobrante) + "," +
             STRING(i) + "," + 
             STRING(k) + ", " + 
             STRING(sobrante_lotes_aceite.id_lote) + 
             CHR(14).
      i = 0.
      k = 0.      
    END.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUbicacionTambores dTables  _DB-REQUIRED
FUNCTION getUbicacionTambores RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buTI FOR tambores_industria.

  FIND CURRENT rowObject NO-ERROR.

  FOR EACH tambores_industria NO-LOCK 
      WHERE tambores_industria.id_empresa    = rowObject.id_empresa
        AND tambores_industria.id_sucursal   = rowObject.id_sucursal
        AND tambores_industria.id_tipotambor = rowObject.id_tipotambor
        AND tambores_industria.nromov        = rowObject.nromov
        AND tambores_industria.id_locacion_ubicacion <> 10
      BREAK BY (STRING(tambores_industria.id_sucursal_ubicacion) + STRING(tambores_industria.id_estado)).
    i = i + 1.
    k = k + tambores_industria.kilos_tambor.
    IF LAST-OF(STRING(tambores_industria.id_sucursal_ubicacion) + STRING(tambores_industria.id_estado)) THEN DO:
      FIND FIRST estados_tambor WHERE estados_tambor.id_estado = tambores_industria.id_estado NO-LOCK NO-ERROR.
      FIND FIRST sucursales WHERE sucursales.id_sucursal = tambores_industria.id_sucursal_ubicacion NO-LOCK NO-ERROR.
      cRet = cRet + 
             sucursales.abreviatura + " - " + estados_tambor.descripcion + "," + 
             STRING(i) + "," + 
             STRING(k) + "," + 
             STRING(tambores_industria.nromov) + "," +
             STRING(tambores_industria.id_estado) +
             CHR(14).
      i = 0.
    END.
    
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

