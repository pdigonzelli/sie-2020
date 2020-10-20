&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          tablastemp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCabeceraTambor NO-UNDO LIKE cabecera_tambor.


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
&Scoped-define INTERNAL-TABLES ttCabeceraTambor

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio cantidad fecha id_articulo id_calidad id_empresa id_envase id_lote~
 id_sucursal id_tambor_desde id_tambor_hasta id_tipotambor kilos nromov~
 id_empresa_carga id_sucursal_carga id_tipotambor_carga nromov_carga~
 tipo_tambor
&Scoped-define ENABLED-FIELDS-IN-ttCabeceraTambor anio cantidad fecha ~
id_articulo id_calidad id_empresa id_envase id_lote id_sucursal ~
id_tambor_desde id_tambor_hasta id_tipotambor kilos nromov id_empresa_carga ~
id_sucursal_carga id_tipotambor_carga nromov_carga tipo_tambor 
&Scoped-Define DATA-FIELDS  anio cantidad fecha id_articulo id_calidad id_empresa id_envase id_lote~
 id_sucursal id_tambor_desde id_tambor_hasta id_tipotambor kilos nromov~
 id_empresa_carga id_sucursal_carga id_tipotambor_carga nromov_carga~
 tipo_tambor
&Scoped-define DATA-FIELDS-IN-ttCabeceraTambor anio cantidad fecha ~
id_articulo id_calidad id_empresa id_envase id_lote id_sucursal ~
id_tambor_desde id_tambor_hasta id_tipotambor kilos nromov id_empresa_carga ~
id_sucursal_carga id_tipotambor_carga nromov_carga tipo_tambor 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dttcabeceratambor.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ttCabeceraTambor NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ttCabeceraTambor NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ttCabeceraTambor
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ttCabeceraTambor


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ttCabeceraTambor SCROLLING.
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
      TABLE: ttCabeceraTambor T "?" NO-UNDO tablastemp cabecera_tambor
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
     _TblList          = "Temp-Tables.ttCabeceraTambor"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ttCabeceraTambor.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no
     _FldNameList[2]   > Temp-Tables.ttCabeceraTambor.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[3]   > Temp-Tables.ttCabeceraTambor.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no
     _FldNameList[4]   > Temp-Tables.ttCabeceraTambor.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[5]   > Temp-Tables.ttCabeceraTambor.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[6]   > Temp-Tables.ttCabeceraTambor.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[7]   > Temp-Tables.ttCabeceraTambor.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[8]   > Temp-Tables.ttCabeceraTambor.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no
     _FldNameList[9]   > Temp-Tables.ttCabeceraTambor.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[10]   > Temp-Tables.ttCabeceraTambor.id_tambor_desde
"id_tambor_desde" "id_tambor_desde" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 no
     _FldNameList[11]   > Temp-Tables.ttCabeceraTambor.id_tambor_hasta
"id_tambor_hasta" "id_tambor_hasta" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 no
     _FldNameList[12]   > Temp-Tables.ttCabeceraTambor.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 13 no
     _FldNameList[13]   > Temp-Tables.ttCabeceraTambor.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 no
     _FldNameList[14]   > Temp-Tables.ttCabeceraTambor.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 no
     _FldNameList[15]   > Temp-Tables.ttCabeceraTambor.id_empresa_carga
"id_empresa_carga" "id_empresa_carga" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[16]   > Temp-Tables.ttCabeceraTambor.id_sucursal_carga
"id_sucursal_carga" "id_sucursal_carga" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 no
     _FldNameList[17]   > Temp-Tables.ttCabeceraTambor.id_tipotambor_carga
"id_tipotambor_carga" "id_tipotambor_carga" ? ? "integer" ? ? ? ? ? ? yes ? no 13 no
     _FldNameList[18]   > Temp-Tables.ttCabeceraTambor.nromov_carga
"nromov_carga" "nromov_carga" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 no
     _FldNameList[19]   > Temp-Tables.ttCabeceraTambor.tipo_tambor
"tipo_tambor" "tipo_tambor" ? ? "integer" ? ? ? ? ? ? yes ? no 15 no
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

  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
    RUN createTambores.
    
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTambores dTables  _DB-REQUIRED
PROCEDURE createTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iEstado AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  hSource = DYNAMIC-FUNCTION('getDataSource').

  /*cuidado!! - uso los campos de clave de la carga pero con los valores de la clave del proceso*/
  ASSIGN rowObjUpd.id_empresa_carga     = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_empresa_proceso')
         rowObjUpd.id_sucursal_carga    = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal_proceso')
         rowObjUpd.id_tipotambor_carga  = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_tipotambor_proceso')
         rowObjUpd.nromov_carga         = DYNAMIC-FUNCTION('columnValue' IN hSource, 'nromov_proceso')
         rowObjUpd.id_empresa           = 1
         rowObjUpd.id_sucursal          = DYNAMIC-FUNCTION('columnValue' IN hSource, 'id_sucursal')
         rowObjUpd.id_tipotambor        = rowObjUpd.tipo_tambor
         rowObjUpd.nromov               = NEXT-VALUE(nromov).
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_empresa_carga,id_sucursal_carga,id_tipotambor_carga,nromov_carga,id_empresa,id_sucursal,id_tipotambor,nromov".


  /*creo las cabeceras*/
  IF rowObjUpd.tipo_tambor = 1 THEN DO: /*producciones*/
    RUN createCabeceraProduccion IN hLib (rowObjUpd.id_empresa,
                                          rowObjUpd.id_sucursal,
                                          rowObjUpd.id_tipotambor,
                                          rowObjUpd.nromov,
                                          rowObjUpd.id_lote,
                                          rowObjUpd.anio,
                                          rowObjUpd.id_articulo,
                                          rowObjUpd.id_calidad,
                                          rowObjUpd.id_envase,
                                          rowObjUpd.fecha).
    iEstado = 2.
  END.

  IF rowObjUpd.tipo_tambor = 3 THEN DO: /*lotes*/
    RUN createCabeceraLoteJugo IN hLib (rowObjUpd.id_empresa,
                                        rowObjUpd.id_sucursal,
                                        rowObjUpd.id_tipotambor,
                                        rowObjUpd.nromov,
                                        rowObjUpd.id_lote,
                                        rowObjUpd.anio,
                                        rowObjUpd.id_articulo,
                                        rowObjUpd.id_calidad,
                                        rowObjUpd.id_envase,
                                        rowObjUpd.fecha).
    iEstado = 4.
  END.

  /*creo los tambores*/
  RUN createDrumsInBatch IN hLib (rowObjUpd.id_empresa,
                                  rowObjUpd.id_sucursal,
                                  rowObjUpd.id_tipotambor, 
                                  rowObjUpd.nromov,
                                  rowObjUpd.cantidad,
                                  rowObjUpd.id_tambor_desde,
                                  rowObjUpd.id_tambor_hasta,
                                  rowObjUpd.kilos,
                                  0, 
                                  iEstado).

  
  /*creo la relacion con el proceso*/
  DO i = rowObjUpd.id_tambor_desde TO rowObjUpd.id_tambor_hasta:
    RUN setRelProcesoTambor IN hLib (rowObjUpd.id_empresa_carga,
                                     rowObjUpd.id_sucursal_carga,
                                     rowObjUpd.id_tipotambor_carga,
                                     rowObjUpd.nromov_carga,
                                     rowObjUpd.id_empresa,
                                     rowObjUpd.id_sucursal,
                                     rowObjUpd.id_tipotambor,
                                     rowObjUpd.nromov,
                                     i).

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
