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

{adm2/support/customColors.i}

DEFINE VAR hProg AS HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES ingreso_lote_ubicacion

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  fecha id_sucursal_ubicacion nro_ingreso observacion fecha_proceso c_fecha~
 c_hora c_usuario fecha_anulacion usuario_anulacion cantidad_comprometida~
 signo nromov_ingreso
&Scoped-define ENABLED-FIELDS-IN-ingreso_lote_ubicacion fecha ~
id_sucursal_ubicacion nro_ingreso observacion fecha_proceso c_fecha c_hora ~
c_usuario fecha_anulacion usuario_anulacion cantidad_comprometida signo ~
nromov_ingreso 
&Scoped-Define DATA-FIELDS  fecha id_sucursal_ubicacion nro_ingreso observacion fecha_proceso c_fecha~
 c_hora c_usuario fecha_anulacion usuario_anulacion cantidad_comprometida~
 signo nromov_ingreso
&Scoped-define DATA-FIELDS-IN-ingreso_lote_ubicacion fecha ~
id_sucursal_ubicacion nro_ingreso observacion fecha_proceso c_fecha c_hora ~
c_usuario fecha_anulacion usuario_anulacion cantidad_comprometida signo ~
nromov_ingreso 
&Scoped-Define MANDATORY-FIELDS  id_sucursal_ubicacion
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dingresoloteubicacion.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ingreso_lote_ubicacion ~
      WHERE ingreso_lote_ubicacion.fecha_proceso = ? ~
 AND ingreso_lote_ubicacion.signo NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ingreso_lote_ubicacion ~
      WHERE ingreso_lote_ubicacion.fecha_proceso = ? ~
 AND ingreso_lote_ubicacion.signo NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ingreso_lote_ubicacion
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ingreso_lote_ubicacion


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD esDevolucion dTables  _DB-REQUIRED
FUNCTION esDevolucion RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteIndustria dTables  _DB-REQUIRED
FUNCTION getLoteIndustria RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD verifica dTables  _DB-REQUIRED
FUNCTION verifica RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ingreso_lote_ubicacion SCROLLING.
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
     _TblList          = "general.ingreso_lote_ubicacion"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "general.ingreso_lote_ubicacion.fecha_proceso = ?
 AND general.ingreso_lote_ubicacion.signo"
     _FldNameList[1]   > general.ingreso_lote_ubicacion.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[2]   > general.ingreso_lote_ubicacion.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.2 yes
     _FldNameList[3]   > general.ingreso_lote_ubicacion.nro_ingreso
"nro_ingreso" "nro_ingreso" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[4]   > general.ingreso_lote_ubicacion.observacion
"observacion" "observacion" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[5]   > general.ingreso_lote_ubicacion.fecha_proceso
"fecha_proceso" "fecha_proceso" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[6]   > general.ingreso_lote_ubicacion.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[7]   > general.ingreso_lote_ubicacion.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[8]   > general.ingreso_lote_ubicacion.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[9]   > general.ingreso_lote_ubicacion.fecha_anulacion
"fecha_anulacion" "fecha_anulacion" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[10]   > general.ingreso_lote_ubicacion.usuario_anulacion
"usuario_anulacion" "usuario_anulacion" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[11]   > general.ingreso_lote_ubicacion.cantidad_comprometida
"cantidad_comprometida" "cantidad_comprometida" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes
     _FldNameList[12]   > general.ingreso_lote_ubicacion.signo
"signo" "signo" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 yes
     _FldNameList[13]   > general.ingreso_lote_ubicacion.nromov_ingreso
"nromov_ingreso" "nromov_ingreso" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
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

DEFINE VAR r AS ROWID NO-UNDO.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D".
    FIND ingreso_lote_ubicacion WHERE ROWID(ingreso_lote_ubicacion) = TO-ROWID(RowObjUpd.RowIdent).
    FOR EACH ITEM_ingreso_lote_ubicacion
        WHERE ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion = ingreso_lote_ubicacion.id_sucursal_ubicacion AND
              ITEM_ingreso_lote_ubicacion.nromov_ingreso = ingreso_lote_ubicacion.nromov_ingreso :
        
        RUN transferenciaLoteUbicacion IN hProg (ITEM_ingreso_lote_ubicacion.id_empresa , 
                                                 ITEM_ingreso_lote_ubicacion.id_sucursal ,
                                                 ITEM_ingreso_lote_ubicacion.id_tipotambor ,
                                                 ITEM_ingreso_lote_ubicacion.nromov ,
                                                 91,
                                                 ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion,
                                                 ingreso_lote_ubicacion.fecha,
                                                 ITEM_ingreso_lote_ubicacion.cantidad,
                                                 TRUE).

        DELETE ITEM_ingreso_lote_ubicacion.
    END.


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaContraIngreso dTables  _DB-REQUIRED
PROCEDURE creaContraIngreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND CURRENT RowObject NO-LOCK.
DEFINE BUFFER iilu FOR ITEM_ingreso_lote_ubicacion.
DEFINE BUFFER ilu  FOR ingreso_lote_ubicacion.
DEFINE VAR viNum    AS INTEGER NO-UNDO.

DO TRANSACTION ON ERROR UNDO , RETURN "Error de contramovimiento":

    CREATE ingreso_lote_ubicacion.
    BUFFER-COPY RowObject EXCEPT nromov_ingreso signo nro_ingreso TO ingreso_lote_ubicacion.
    ASSIGN ingreso_lote_ubicacion.nromov_ingreso = NEXT-VALUE(seq-ingreso)
           ingreso_lote_ubicacion.signo = NOT RowObject.signo.
    FOR LAST ilu WHERE nro_ingreso <> ? BY ilu.nro_ingreso:
        viNum = ilu.nro_ingreso.
    END.
    viNum = viNum + 1.
    ASSIGN ingreso_lote_ubicacion.nro_ingreso = viNum
           ingreso_lote_ubicacion.nromov_contra_ingreso = RowObject.nromov_ingreso.
    

    FOR EACH ITEM_ingreso_lote_ubicacion WHERE ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion  = RowObject.id_sucursal_ubicacion AND
                                               ITEM_ingreso_lote_ubicacion.nromov_ingreso       = RowObject.nromov_ingreso  NO-LOCK.
        CREATE iilu.
        BUFFER-COPY item_ingreso_lote_ubicacion EXCEPT nromov_ingreso id_lote_deposito TO iilu.
        iilu.nromov_ingreso = ingreso_lote_ubicacion.nromov_ingreso.
        
        RUN transferenciaLoteUbicacion IN hProg (ITEM_ingreso_lote_ubicacion.id_empresa , 
                                                 ITEM_ingreso_lote_ubicacion.id_sucursal ,
                                                 ITEM_ingreso_lote_ubicacion.id_tipotambor ,
                                                 ITEM_ingreso_lote_ubicacion.nromov ,
                                                 91,
                                                 ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion,
                                                 ingreso_lote_ubicacion.fecha,
                                                 ITEM_ingreso_lote_ubicacion.cantidad).
    END.
END.
openQuery().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject dTables  _DB-REQUIRED
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  IF VALID-HANDLE( hProg ) THEN
      DELETE PROCEDURE hProg.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE emiteIngresoLoteUbicacion dTables  _DB-REQUIRED
PROCEDURE emiteIngresoLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_sucursal_ubicacionValidate dTables  _DB-REQUIRED
PROCEDURE id_sucursal_ubicacionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcValor AS CHARACTER NO-UNDO.

IF pcValor = "" THEN 
  RETURN "Deposito Seleccionado Invalido".

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

  RUN libLotesUbicacion.p PERSISTENT SET hProg.
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  /*
  DEFINE VAR xDataSource AS CHARACTER NO-UNDO.
  {get DataSource xDataSource}.
  IF xDataSource <> ? THEN
  DO:
      {set AutoCommit YES}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables 
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A" OR RowObjUpd.RowMod = "C".
    
    RowObjUpd.nromov_ingreso = NEXT-VALUE(seq-ingreso).
    RowObjUpd.c_usuario      = USERID("userdb").
    RowObjUpd.c_fecha        = TODAY.
    RowObjUpd.c_hora         = STRING(TIME,"hh:mm:ss").

    RowObjUpd.changedFields  = RowObjUpd.changedFields + "," + "nromov_ingreso" 
                                                       + "," + "c_usuario"
                                                       + "," + "c_fecha" 
                                                       + "," + "c_hora".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesarIngresoLoteUbicacion dTables  _DB-REQUIRED
PROCEDURE procesarIngresoLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER ilu FOR ingreso_lote_ubicacion.
DEFINE VAR viNum  AS INTEGER NO-UNDO.

FIND CURRENT RowObject NO-LOCK NO-ERROR.
IF AVAILABLE RowObject THEN
DO:
    FIND ingreso_lote_ubicacion WHERE ingreso_lote_ubicacion.id_sucursal_ubicacion = 
         RowObject.id_sucursal_ubicacion AND 
         ingreso_lote_ubicacion.nromov_ingreso = 
         RowObject.nromov NO-ERROR.
    IF AVAILABLE ingreso_lote_ubicacion THEN
    DO:
        IF verifica() = "" THEN
        DO ON ERROR UNDO, RETURN "Error de Procesamiento":
            FOR LAST ilu WHERE fecha_proceso <> ? BY ilu.nro_ingreso:
                viNum = ilu.nro_ingreso.
            END.
            viNum = viNUm + 1.
            ASSIGN ingreso_lote_ubicacion.fecha_proceso = TODAY
                   ingreso_lote_ubicacion.nro_ingreso = viNum.
        END.
        ELSE
            RETURN RETURN-VALUE.
    END.
    ELSE
        RETURN " No esta disponible el ingreso ".
END.
ELSE
    RETURN " No encuentra RowObject ".

    openQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION esDevolucion dTables  _DB-REQUIRED
FUNCTION esDevolucion RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN NOT RowObject.signo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteIndustria dTables  _DB-REQUIRED
FUNCTION getLoteIndustria RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION verifica dTables  _DB-REQUIRED
FUNCTION verifica RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

