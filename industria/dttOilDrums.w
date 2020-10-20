&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          tablastemp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttOilDrums NO-UNDO LIKE oildrums.


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
&Scoped-define INTERNAL-TABLES ttOilDrums

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  fecha id_articulo id_calidad id_empresa id_envase id_locacion_ubicacion~
 id_lote_prod id_sucursal id_sucursal_ubicacion id_tambor id_tipotambor~
 kilos litros_tambor nromov articulo calidad envase anio
&Scoped-define ENABLED-FIELDS-IN-ttOilDrums fecha id_articulo id_calidad ~
id_empresa id_envase id_locacion_ubicacion id_lote_prod id_sucursal ~
id_sucursal_ubicacion id_tambor id_tipotambor kilos litros_tambor nromov ~
articulo calidad envase anio 
&Scoped-Define DATA-FIELDS  fecha id_articulo id_calidad id_empresa id_envase id_locacion_ubicacion~
 id_lote_prod id_sucursal id_sucursal_ubicacion id_tambor id_tipotambor~
 kilos litros_tambor nromov articulo calidad envase anio
&Scoped-define DATA-FIELDS-IN-ttOilDrums fecha id_articulo id_calidad ~
id_empresa id_envase id_locacion_ubicacion id_lote_prod id_sucursal ~
id_sucursal_ubicacion id_tambor id_tipotambor kilos litros_tambor nromov ~
articulo calidad envase anio 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE asindustria
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttoildrums.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ttOilDrums NO-LOCK ~
    BY ttOilDrums.id_lote_prod ~
       BY ttOilDrums.id_tambor INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ttOilDrums NO-LOCK ~
    BY ttOilDrums.id_lote_prod ~
       BY ttOilDrums.id_tambor INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ttOilDrums
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ttOilDrums


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArticulo dTables  _DB-REQUIRED
FUNCTION getArticulo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalidad dTables  _DB-REQUIRED
FUNCTION getCalidad RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getClaveTamborFromRowId dTables  _DB-REQUIRED
FUNCTION getClaveTamborFromRowId RETURNS CHARACTER
  (prTambor AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowsNumber dTables  _DB-REQUIRED
FUNCTION getRowsNumber RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedRowIds dTables  _DB-REQUIRED
FUNCTION getSelectedRowIds RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumSelectedValues dTables  _DB-REQUIRED
FUNCTION getSumSelectedValues RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ttOilDrums SCROLLING.
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
      TABLE: ttOilDrums T "?" NO-UNDO tablastemp oildrums
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
     _TblList          = "Temp-Tables.ttOilDrums"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.ttOilDrums.id_lote_prod|yes,Temp-Tables.ttOilDrums.id_tambor|yes"
     _FldNameList[1]   > Temp-Tables.ttOilDrums.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no
     _FldNameList[2]   > Temp-Tables.ttOilDrums.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[3]   > Temp-Tables.ttOilDrums.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[4]   > Temp-Tables.ttOilDrums.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[5]   > Temp-Tables.ttOilDrums.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[6]   > Temp-Tables.ttOilDrums.id_locacion_ubicacion
"id_locacion_ubicacion" "id_locacion_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[7]   > Temp-Tables.ttOilDrums.id_lote_prod
"id_lote_prod" "id_lote_prod" ? ? "integer" ? ? ? ? ? ? yes ? no 12 no
     _FldNameList[8]   > Temp-Tables.ttOilDrums.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[9]   > Temp-Tables.ttOilDrums.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[10]   > Temp-Tables.ttOilDrums.id_tambor
"id_tambor" "id_tambor" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 no
     _FldNameList[11]   > Temp-Tables.ttOilDrums.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 13 no
     _FldNameList[12]   > Temp-Tables.ttOilDrums.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[13]   > Temp-Tables.ttOilDrums.litros_tambor
"litros_tambor" "litros_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no 9 no
     _FldNameList[14]   > Temp-Tables.ttOilDrums.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 no
     _FldNameList[15]   > Temp-Tables.ttOilDrums.articulo
"articulo" "articulo" ? ? "character" ? ? ? ? ? ? yes ? no 25 no
     _FldNameList[16]   > Temp-Tables.ttOilDrums.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[17]   > Temp-Tables.ttOilDrums.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[18]   > Temp-Tables.ttOilDrums.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no
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


FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A".
  rowObjUpd.nromov = NEXT-VALUE(nromov).
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",nromov".
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTTOilDrums dTables  _DB-REQUIRED
PROCEDURE fillTTOilDrums :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucUbi   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipoTam  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha    AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER piArticulo AS INTEGER    NO-UNDO.

  /*lleno la oiilDrums*/
  FOR EACH ttOilDrums.
    DELETE ttOilDrums.
  END.
  
  CASE piTipoTam:
    WHEN 2 THEN /*tambores de produccion de aceite*/
      RUN getTamboresProduccion(piTipoTam, piSucUbi, piArticulo).
    OTHERWISE 
      RUN getTambores(piTipoTam, piSucUbi, pdFecha).
  END CASE.
  
  DYNAMIC-FUNCTION('openQuery').

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTambores dTables  _DB-REQUIRED
PROCEDURE getTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piTipoTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucUbi  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha   AS DATE       NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.fecha                >= DATE("01/01/2001")
                                AND tambores_industria.id_locacion_ubicacion = 4
                                AND tambores_industria.id_sucursal_ubicacion = piSucUbi
                                AND tambores_industria.id_tipotambor         = piTipoTam
                                /*AND tambores_industria.fecha                <= pdFecha*/
                                AND tambores_industria.id_empresa_destino    = 0
                                AND tambores_industria.id_sucursal_destino   = 0
                                AND tambores_industria.id_tipotambor_destino = 0
                                AND tambores_industria.nromov_destino        = 0
                              NO-LOCK.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
    
    IF tambores_industria.id_articulo <> 610  AND 
       tambores_industria.id_articulo <> 503  AND 
       tambores_industria.id_articulo <> 504  AND
       /*tambores_industria.id_articulo <> 578  AND */
       tambores_industria.id_articulo <> 5128 AND 
       tambores_industria.id_articulo <> 5138 THEN DO:
    
      CREATE ttOilDrums.
      ASSIGN ttOilDrums.id_empresa            = tambores_industria.id_empresa
             ttOilDrums.id_sucursal           = tambores_industria.id_sucursal
             ttOilDrums.id_tipotambor         = tambores_industria.id_tipotambor
             ttOilDrums.nromov                = tambores_industria.nromov
             ttOilDrums.id_lote_prod          = tambores_industria.id_lote
             ttOilDrums.anio                  = tambores_industria.anio
             ttOilDrums.id_tambor             = tambores_industria.id_tambor
             ttOilDrums.fecha                 = tambores_industria.fecha
             ttOilDrums.kilos                 = tambores_industria.kilos_tambor
             ttOilDrums.id_articulo           = tambores_industria.id_articulo
             ttOilDrums.id_calidad            = tambores_industria.id_calidad
             ttOilDrums.id_envase             = tambores_industria.id_envase
             ttOilDrums.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
             ttOilDrums.id_locacion_ubicacion = tambores_industria.id_locacion_ubicacion    
             ttOilDrums.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
             ttOilDrums.calidad               = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
             ttOilDrums.envase                = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE".
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTamboresProduccion dTables  _DB-REQUIRED
PROCEDURE getTamboresProduccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piTipoTam  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucUbi   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piArticulo AS INTEGER    NO-UNDO.

  FOR EACH origenes_materia_prima WHERE origenes_materia_prima.id_articulo_lote = piArticulo NO-LOCK.

    FOR EACH tambores_industria WHERE tambores_industria.id_articulo             = origenes_materia_prima.id_articulo_mp
                                  AND tambores_industria.id_empresa_destino      = 0
                                  AND tambores_industria.id_sucursal_destino     = 0
                                  AND tambores_industria.id_tipotambor_destino   = 0
                                  AND tambores_industria.nromov_destino          = 0
                                  AND tambores_industria.id_locacion_ubicacion   = 4
                                  AND tambores_industria.id_sucursal_ubicacion   = piSucUbi
                                  AND tambores_industria.id_tipotambor           = piTipoTam
                                NO-LOCK.
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
  
      CREATE ttOilDrums.
      ASSIGN ttOilDrums.id_empresa            = tambores_industria.id_empresa
             ttOilDrums.id_sucursal           = tambores_industria.id_sucursal
             ttOilDrums.id_tipotambor         = tambores_industria.id_tipotambor
             ttOilDrums.nromov                = tambores_industria.nromov
             ttOilDrums.id_lote_prod          = IF tambores_industria.id_lote = 0 THEN tambores_industria.id_tambor ELSE tambores_industria.id_lote
             ttOilDrums.anio                  = tambores_industria.anio
             ttOilDrums.id_tambor             = tambores_industria.id_tambor
             ttOilDrums.fecha                 = tambores_industria.fecha
             ttOilDrums.kilos                 = tambores_industria.kilos_tambor
             ttOilDrums.id_articulo           = tambores_industria.id_articulo
             ttOilDrums.id_calidad            = tambores_industria.id_calidad
             ttOilDrums.id_envase             = tambores_industria.id_envase
             ttOilDrums.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
             ttOilDrums.id_locacion_ubicacion = tambores_industria.id_locacion_ubicacion    
             ttOilDrums.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
             ttOilDrums.calidad               = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
             ttOilDrums.envase                = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE".


    END.

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTamboresSobranteBorra dTables  _DB-REQUIRED
PROCEDURE getTamboresSobranteBorra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piTipoTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucUbi  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdFecha   AS DATE       NO-UNDO.

  FOR EACH tambores_industria WHERE tambores_industria.fecha                >= DATE("01/01/2001")
                                AND tambores_industria.id_locacion_ubicacion = 4
                                AND tambores_industria.id_sucursal_ubicacion = piSucUbi
                                AND tambores_industria.id_tipotambor         = 8
                                AND tambores_industria.fecha                <= pdFecha
                                AND tambores_industria.id_empresa_destino    = 0
                                AND tambores_industria.id_sucursal_destino   = 0
                                AND tambores_industria.id_tipotambor_destino = 0
                                AND tambores_industria.nromov_destino        = 0
                              NO-LOCK.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.

    CREATE ttOilDrums.
    ASSIGN ttOilDrums.id_empresa            = tambores_industria.id_empresa
           ttOilDrums.id_sucursal           = tambores_industria.id_sucursal
           ttOilDrums.id_tipotambor         = tambores_industria.id_tipotambor
           ttOilDrums.nromov                = tambores_industria.nromov
           ttOilDrums.id_lote_prod          = tambores_industria.id_lote
           ttOilDrums.anio                  = tambores_industria.anio
           ttOilDrums.id_tambor             = tambores_industria.id_tambor
           ttOilDrums.fecha                 = tambores_industria.fecha
           ttOilDrums.kilos                 = tambores_industria.kilos_tambor
           ttOilDrums.id_articulo           = tambores_industria.id_articulo
           ttOilDrums.id_calidad            = tambores_industria.id_calidad
           ttOilDrums.id_envase             = tambores_industria.id_envase
           ttOilDrums.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
           ttOilDrums.id_locacion_ubicacion = tambores_industria.id_locacion_ubicacion    
           ttOilDrums.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
           ttOilDrums.calidad               = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
           ttOilDrums.envase                = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE "NONE".
  END.


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

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArticulo dTables  _DB-REQUIRED
FUNCTION getArticulo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = rowObject.id_articulo
                    NO-LOCK NO-ERROR.
  IF AVAILABLE productos_terminados THEN
    RETURN productos_terminados.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalidad dTables  _DB-REQUIRED
FUNCTION getCalidad RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST calidades WHERE calidades.id_calidad = rowObject.id_calidad
                    NO-LOCK NO-ERROR.
  IF AVAILABLE calidades THEN
    RETURN calidades.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getClaveTamborFromRowId dTables  _DB-REQUIRED
FUNCTION getClaveTamborFromRowId RETURNS CHARACTER
  (prTambor AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  DEFINE BUFFER buTam FOR ttOilDrums.

  FIND FIRST buTam WHERE ROWID(buTam) = prTambor
                   NO-LOCK NO-ERROR.
  IF AVAILABLE buTam THEN
    cRet = STRING(buTam.id_empresa) + "," + 
           STRING(buTam.id_sucursal) + "," + 
           STRING(buTam.id_tipotambor) + "," + 
           STRING(buTam.nromov) + "," + 
           STRING(buTam.id_tambor).
  ELSE
    cRet = "0,0,0,0,0".

  RELEASE buTam.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST envases_prod WHERE envases_prod.id_envase = rowObject.id_envase
                    NO-LOCK NO-ERROR.
  IF AVAILABLE envases_prod THEN
    RETURN envases_prod.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowsNumber dTables  _DB-REQUIRED
FUNCTION getRowsNumber RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER buRowObject FOR rowObject.
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  FOR EACH buRowObject NO-LOCK.
    i = i + 1.
  END.

  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedRowIds dTables  _DB-REQUIRED
FUNCTION getSelectedRowIds RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRid    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo    AS INTEGER    NO-UNDO.


  DO iRow = 1 TO NUM-ENTRIES(pcRows) ON ERROR UNDO, LEAVE:
    cRid = ENTRY(iRow, pcRows).
    cKey = getClaveTamborFromRowId(TO-ROWID(cRid)).

    ASSIGN iEmp = INTEGER(ENTRY(1, cKey))
           iSuc = INTEGER(ENTRY(2, cKey))
           iTip = INTEGER(ENTRY(3, cKey))
           iNro = INTEGER(ENTRY(4, cKey))
           iTbo = INTEGER(ENTRY(5, cKey)).

    
    FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = iEmp
                                    AND tambores_industria.id_sucursal   = iSuc
                                    AND tambores_industria.id_tipotambor = iTip
                                    AND tambores_industria.nromov        = iNro
                                    AND tambores_industria.id_tambor     = iTbo
                                  NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      cRet = cRet + STRING(ROWID(tambores_industria)) + CHR(10).
    END.
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1). 

  RETURN cRet.  
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSumSelectedValues dTables  _DB-REQUIRED
FUNCTION getSumSelectedValues RETURNS CHARACTER
  (INPUT pcRows AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTbo    AS INTEGER    NO-UNDO.
  
  DEFINE VARIABLE iCant   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iKilos  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dLitros AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBrix   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dAcid   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSodio  AS DECIMAL    NO-UNDO.

  DEFINE BUFFER ro FOR rowObject.

  cRet = STRING(0) + CHR(1) + STRING(0).
  DO iRow = 1 TO NUM-ENTRIES(pcRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow', INTEGER(ENTRY(iRow,pcRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,id_tambor').
    ASSIGN iEmp = INTEGER(ENTRY(2, cFields, CHR(1)))
           iSuc = INTEGER(ENTRY(3, cFields, CHR(1)))
           iTip = INTEGER(ENTRY(4, cFields, CHR(1)))
           iNro = INTEGER(ENTRY(5, cFields, CHR(1)))
           iTbo = INTEGER(ENTRY(6, cFields, CHR(1))).

    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".
    FIND FIRST ro WHERE ro.id_empresa    = iEmp
                    AND ro.id_sucursal   = iSuc
                    AND ro.id_tipotambor = iTip
                    AND ro.nromov        = iNro
                    AND ro.id_tambor     = iTbo
                  NO-LOCK NO-ERROR.
    IF AVAILABLE ro THEN DO:
      ASSIGN iCant   = iCant + 1
             iKilos  = iKilos + ro.kilos.
    END.
  END.
  
  cRet = STRING(iCant) + CHR(1) + 
         STRING(iKilos) + CHR(1).
  RETURN cRet. 


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

