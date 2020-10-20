&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          tablastemp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttLotesDepExt NO-UNDO LIKE lotesdepext.


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
&Scoped-define INTERNAL-TABLES ttLotesDepExt

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  articulo calidad cantidad envase id_empresa id_lote id_lote_deposito~
 id_sucursal id_sucursal_ubicacion id_tipotambor lote nromov show~
 tambor_desde tambor_hasta
&Scoped-define ENABLED-FIELDS-IN-ttLotesDepExt articulo calidad cantidad ~
envase id_empresa id_lote id_lote_deposito id_sucursal ~
id_sucursal_ubicacion id_tipotambor lote nromov show tambor_desde ~
tambor_hasta 
&Scoped-Define DATA-FIELDS  articulo calidad cantidad envase id_empresa id_lote id_lote_deposito~
 id_sucursal id_sucursal_ubicacion id_tipotambor lote nromov show~
 tambor_desde tambor_hasta
&Scoped-define DATA-FIELDS-IN-ttLotesDepExt articulo calidad cantidad ~
envase id_empresa id_lote id_lote_deposito id_sucursal ~
id_sucursal_ubicacion id_tipotambor lote nromov show tambor_desde ~
tambor_hasta 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttLotesDepExt.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ttLotesDepExt NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ttLotesDepExt NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ttLotesDepExt
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ttLotesDepExt


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ttLotesDepExt SCROLLING.
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
      TABLE: ttLotesDepExt T "?" NO-UNDO tablasTemp lotesdepext
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
     _TblList          = "Temp-Tables.ttLotesDepExt"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ttLotesDepExt.articulo
"articulo" "articulo" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[2]   > Temp-Tables.ttLotesDepExt.calidad
"calidad" "calidad" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[3]   > Temp-Tables.ttLotesDepExt.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[4]   > Temp-Tables.ttLotesDepExt.envase
"envase" "envase" ? ? "character" ? ? ? ? ? ? yes ? no 20 no
     _FldNameList[5]   > Temp-Tables.ttLotesDepExt.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 no
     _FldNameList[6]   > Temp-Tables.ttLotesDepExt.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[7]   > Temp-Tables.ttLotesDepExt.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? yes ? no 15 no
     _FldNameList[8]   > Temp-Tables.ttLotesDepExt.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 no
     _FldNameList[9]   > Temp-Tables.ttLotesDepExt.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? no 21 no
     _FldNameList[10]   > Temp-Tables.ttLotesDepExt.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 no
     _FldNameList[11]   > Temp-Tables.ttLotesDepExt.lote
"lote" "lote" ? ? "character" ? ? ? ? ? ? yes ? no 10 no
     _FldNameList[12]   > Temp-Tables.ttLotesDepExt.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[13]   > Temp-Tables.ttLotesDepExt.show
"show" "show" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[14]   > Temp-Tables.ttLotesDepExt.tambor_desde
"tambor_desde" "tambor_desde" ? ? "integer" ? ? ? ? ? ? yes ? no 13.4 no
     _FldNameList[15]   > Temp-Tables.ttLotesDepExt.tambor_hasta
"tambor_hasta" "tambor_hasta" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillttLotesDepExt dTables  _DB-REQUIRED
PROCEDURE fillttLotesDepExt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCant  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iDesde AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta AS INTEGER    NO-UNDO.
    
      
  FOR EACH ttLotesDepExt.
    DELETE ttLotesDepExt.
  END.


  FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_ubicacion = 73
                                 OR tambores_industria.id_sucursal_ubicacion = 74
                                 OR tambores_industria.id_sucursal_ubicacion = 84
                                 OR tambores_industria.id_sucursal_ubicacion = 86
                                 OR tambores_industria.id_sucursal_ubicacion = 426
                                 OR tambores_industria.id_sucursal_ubicacion = 427
                              BREAK BY tambores_industria.id_lote_deposito
                              BY tambores_industria.id_tambor.
    iCant = iCant + 1.
    IF FIRST-OF(tambores_industria.id_lote_deposito) THEN
      iDesde = tambores_industria.id_tambor - 1.
    IF iDesde = 0 THEN iDesde = 1.

    IF LAST-OF(tambores_industria.id_lote_deposito) THEN DO:
      iHasta = tambores_industria.id_tambor.
      FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
      FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
  
      CREATE ttLotesDepExt.
      ASSIGN ttLotesDepExt.lote             = IF AVAILABLE tambores_industria THEN string(tambores_industria.id_lote, ">>>>99") + "/" + STRING(tambores_industria.anio - 2000, "99") ELSE ""
             ttLotesDepExt.id_lote_deposito = tambores_industria.id_lote_deposito
             ttLotesDepExt.cantidad         = iCant
             ttLotesDepExt.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE ""
             ttLotesDepExt.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE ""
             ttLotesDepExt.envase           = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE ""
             ttLotesDepExt.id_lote          = IF AVAILABLE tambores_industria THEN tambores_industria.id_lote ELSE 0
             ttLotesDepExt.nromov           = tambores_industria.nromov
             ttLotesDepExt.id_empresa       = tambores_industria.id_empresa
             ttLotesDepExt.id_sucursal      = tambores_industria.id_sucursal
             ttLotesDepExt.id_tipotambor    = tambores_industria.id_tipotambor
             ttLotesDepExt.id_sucursal_ubi  = tambores_industria.id_sucursal_ubicacion
             ttLotesDepExt.show             = 1
             ttLotesDepExt.tambor_desde     = iDesde
             ttLotesDepExt.tambor_hasta     = iHasta.
      iCant = 0.
    END.
  END.

openQuery().

END PROCEDURE.


/*
FOR EACH ITEM_ingreso_lote_ubicacion WHERE ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = 73
                                        OR ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = 74
                                        OR ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = 84
                                        OR ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = 86
                                        OR ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = 426
                                        OR ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = 427
                                     BREAK BY ITEM_ingreso.id_lote_deposito BY ITEM_ingreso_lote_ubicacion.nromov.
  IF LAST-OF(ITEM_ingreso_lote_ubicacion.nromov) THEN DO:
    iCant = 0.
    FOR EACH items_release_delivery WHERE items_release_delivery.id_empresa             = ITEM_ingreso_lote_ubicacion.id_empresa
                                      AND items_release_delivery.id_sucursal            = ITEM_ingreso_lote_ubicacion.id_sucursal
                                      AND items_release_delivery.id_tipotambor          = ITEM_ingreso_lote_ubicacion.id_tipotambor
                                      AND items_release_delivery.nromov                 = ITEM_ingreso_lote_ubicacion.nromov
                                      AND items_release_delivery.id_lote_deposito       = ITEM_ingreso_lote_ubicacion.id_lote_deposito
                                    NO-LOCK.
      iCant = iCant + items_release_delivery.tambores.        
    END.
    FIND LAST tambores_industria WHERE tambores_industria.nromov = ITEM_ingreso_lote_ubicacion.nromov NO-LOCK NO-ERROR.
    iHasta = tambores_industria.id_tambor.
    FIND FIRST tambores_industria WHERE tambores_industria.nromov = ITEM_ingreso_lote_ubicacion.nromov NO-LOCK NO-ERROR.
    iDesde = tambores_industria.id_tambor.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.

    CREATE ttLotesDepExt.
    ASSIGN ttLotesDepExt.lote             = IF AVAILABLE tambores_industria THEN string(tambores_industria.id_lote, ">>>>99") + "/" + STRING(tambores_industria.anio - 2000, "99") ELSE ""
           ttLotesDepExt.id_lote_deposito = ITEM_ingreso_lote_ubicacion.id_lote_deposito
           ttLotesDepExt.cantidad         = ITEM_ingreso_lote_ubicacion.cantidad - iCant
           ttLotesDepExt.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE ""
           ttLotesDepExt.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE ""
           ttLotesDepExt.envase           = IF AVAILABLE envases_prod THEN envases_prod.descripcion ELSE ""
           ttLotesDepExt.id_lote          = IF AVAILABLE tambores_industria THEN tambores_industria.id_lote ELSE 0
           ttLotesDepExt.nromov           = ITEM_ingreso_lote_ubicacion.nromov
           ttLotesDepExt.id_empresa       = ITEM_ingreso_lote_ubicacion.id_empresa
           ttLotesDepExt.id_sucursal      = ITEM_ingreso_lote_ubicacion.id_sucursal
           ttLotesDepExt.id_tipotambor    = ITEM_ingreso_lote_ubicacion.id_tipotambor
           ttLotesDepExt.id_sucursal_ubi  = ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion
           ttLotesDepExt.show             = 1
           ttLotesDepExt.tambor_desde     = iDesde
           ttLotesDepExt.tambor_hasta     = iHasta.
  END.
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

  RUN fillttLotesDepExt.



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

