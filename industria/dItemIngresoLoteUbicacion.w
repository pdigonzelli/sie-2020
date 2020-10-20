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
&Scoped-define INTERNAL-TABLES item_ingreso_lote_ubicacion

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  cantidad id_empresa id_sucursal id_sucursal_ubicacion id_tipotambor nromov~
 nro_ingreso id_lote_deposito nromov_ingreso
&Scoped-define ENABLED-FIELDS-IN-item_ingreso_lote_ubicacion cantidad ~
id_empresa id_sucursal id_sucursal_ubicacion id_tipotambor nromov ~
nro_ingreso id_lote_deposito nromov_ingreso 
&Scoped-Define DATA-FIELDS  cantidad lote id_empresa id_sucursal id_sucursal_ubicacion id_tipotambor~
 nromov nro_ingreso id_lote_deposito nromov_ingreso
&Scoped-define DATA-FIELDS-IN-item_ingreso_lote_ubicacion cantidad ~
id_empresa id_sucursal id_sucursal_ubicacion id_tipotambor nromov ~
nro_ingreso id_lote_deposito nromov_ingreso 
&Scoped-Define MANDATORY-FIELDS  id_sucursal id_sucursal_ubicacion
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "ditemingresoloteubicacion.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH item_ingreso_lote_ubicacion NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH item_ingreso_lote_ubicacion NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main item_ingreso_lote_ubicacion
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main item_ingreso_lote_ubicacion


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createItemFromLoteUbicacion dTables  _DB-REQUIRED
FUNCTION createItemFromLoteUbicacion RETURNS CHARACTER
  (INPUT hDataSource AS HANDLE , cRows AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLote dTables  _DB-REQUIRED
FUNCTION getLote RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      item_ingreso_lote_ubicacion SCROLLING.
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
         HEIGHT             = 1.52
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
     _TblList          = "general.item_ingreso_lote_ubicacion"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.item_ingreso_lote_ubicacion.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[2]   > "_<CALC>"
"getLote()" "lote" ? "XXXXXX/XXXX" "character" ? ? ? ? ? ? no ? no 11 no
     _FldNameList[3]   > general.item_ingreso_lote_ubicacion.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[4]   > general.item_ingreso_lote_ubicacion.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[5]   > general.item_ingreso_lote_ubicacion.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.2 yes
     _FldNameList[6]   > general.item_ingreso_lote_ubicacion.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[7]   > general.item_ingreso_lote_ubicacion.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[8]   > general.item_ingreso_lote_ubicacion.nro_ingreso
"nro_ingreso" "nro_ingreso" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[9]   > general.item_ingreso_lote_ubicacion.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[10]   > general.item_ingreso_lote_ubicacion.nromov_ingreso
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

DEFINE BUFFER ROU FOR RowObjUpd.
DEFINE VAR hDataSource AS HANDLE NO-UNDO.
DEFINE VAR hRowObject  AS HANDLE NO-UNDO.
DEFINE VAR idif        AS INTEGER NO-UNDO.

{get dataSource hDataSource}.
hRowObject = DYNAMIC-FUNCTION ('getRowObject' IN hDataSource).

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "U".
    FIND FIRST ROU WHERE ROU.RowNum = RowObjUpd.RowNum AND ROU.RowMod = "" NO-ERROR.
    IF AVAILABLE ROU THEN DO:
        idif = RowObjUpd.cantidad - ROU.cantidad .
        IF idif <> 0 THEN DO:
            IF idif > 0  THEN
                RUN transferenciaLoteUbicacion IN hProg (RowObjUpd.id_empresa , 
                                                         RowObjUpd.id_sucursal ,
                                                         RowObjUpd.id_tipotambor ,
                                                         RowObjUpd.nromov ,
                                                         82,
                                                         RowObjUpd.id_sucursal_ubicacion,
                                                         hRowObject:BUFFER-FIELD('fecha'):BUFFER-VALUE,
                                                         idif,
                                                         20,
                                                         TRUE).
            ELSE
                RUN transferenciaLoteUbicacion IN hProg (RowObjUpd.id_empresa , 
                                                         RowObjUpd.id_sucursal ,
                                                         RowObjUpd.id_tipotambor ,
                                                         RowObjUpd.nromov ,
                                                         RowObjUpd.id_sucursal_ubicacion,
                                                         82,
                                                         hRowObject:BUFFER-FIELD('fecha'):BUFFER-VALUE,
                                                         abs(idif),
                                                         20,
                                                         TRUE).
                          
            IF RETURN-VALUE <> "" THEN
                RETURN RETURN-VALUE.

        END.
        /*actualiza lote_deposito en tambores_industria*/
        RUN setLoteDeposito IN hProg (RowObjUpd.id_empresa , 
                                      RowObjUpd.id_sucursal ,
                                      RowObjUpd.id_tipotambor ,
                                      RowObjUpd.nromov ,
                                      RowObjUpd.id_sucursal_ubicacion,
                                      rowObjUpd.cantidad,
                                      rowObjUpd.id_lote_deposito).
    END.
END.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D".
    
    RUN transferenciaLoteUbicacion IN hProg (RowObjUpd.id_empresa , 
                                             RowObjUpd.id_sucursal ,
                                             RowObjUpd.id_tipotambor ,
                                             RowObjUpd.nromov ,
                                             RowObjUpd.id_sucursal_ubicacion,
                                             82,
                                             hRowObject:BUFFER-FIELD('fecha'):BUFFER-VALUE,
                                             RowObjUpd.cantidad,
                                             20,
                                             TRUE). /*by Facundo 18/12/2003 hs 08.45*/
    IF RETURN-VALUE <> "" THEN
        RETURN RETURN-VALUE.


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
         rowObject.lote = (getLote())
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject dTables 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moverTamboresIndustria dTables  _DB-REQUIRED
PROCEDURE moverTamboresIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER piEmpresa               AS INTEGER NO-UNDO.
 DEFINE INPUT PARAMETER piSucursal              AS INTEGER NO-UNDO.
 DEFINE INPUT PARAMETER piTipoTambor            AS INTEGER NO-UNDO.
 DEFINE INPUT PARAMETER piNromov                AS INTEGER NO-UNDO.
 DEFINE INPUT PARAMETER piSucursalOrigen        AS INTEGER NO-UNDO.
 DEFINE INPUT PARAMETER piSucursalDestino       AS INTEGER NO-UNDO.
 DEFINE INPUT PARAMETER piTipoIngreso           AS INTEGER NO-UNDO.
 DEFINE INPUT PARAMETER pdCantidad              AS DECIMAL NO-UNDO.

         
 DEFINE VAR iSuc                                AS INTEGER   NO-UNDO.
 DEFINE VAR idesde                              AS INTEGER   NO-UNDO INITIAL 9999.
 DEFINE VAR ihasta                              AS INTEGER   NO-UNDO.
 DEFINE VAR iTipo                               AS INTEGER   NO-UNDO INITIAL 20.
 DEFINE VAR viTambores                          AS INTEGER   NO-UNDO INITIAL 0.

 viTambores = 0.
 FOR EACH tambores_industria  
     WHERE tambores_industria.id_empresa = piEmpresa AND
           tambores_industria.id_sucursal = piSucursal AND
           tambores_industria.id_tipotambor = piTipoTambor AND
           tambores_industria.nromov = piNromov AND
           tambores_industria.id_sucursal_ubicacion = piSucursalOrigen 
     BY tambores_industria.id_tambor.
     IF viTambores >= abs(pdCantidad) THEN LEAVE.
     ASSIGN tambores_industria.id_sucursal_ubicacion = piSucursalDestino.
     idesde = IF tambores_industria.id_tambor < idesde THEN tambores_industria.id_tambor ELSE idesde.
     ihasta = IF tambores_industria.id_tambor > ihasta THEN tambores_industria.id_tambor ELSE ihasta.
     viTambores = viTambores + 1.
 END.


 RUN Y_gstkmovdep.p (   piEmpresa , 
                        piSucursal ,
                        piTipoTambor ,
                        piNromov ,
                        piSucursalOrigen,
                        piSucursalDestino,
                        idesde,
                        ihasta,
                        piTipoIngreso ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postTransactionValidate dTables 
PROCEDURE postTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

DEFINE VAR hContainer AS HANDLE NO-UNDO.
{get ContainerSource hContainer}.
IF VALID-HANDLE(hContainer) THEN
    RUN openLoteUbicacion IN hContainer.
ELSE
    MESSAGE "No encuentra el contenedor" VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
DEFINE VAR hDataSource  AS HANDLE NO-UNDO.
DEFINE VAR hRow         AS HANDLE NO-UNDO.

{get dataSource hDataSource}.
hRow = DYNAMIC-FUNCTION('getRowObject' IN hDataSource).

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A" OR RowObjUpd.RowMod = "C".
    ASSIGN RowObjUpd.id_sucursal_ubicacion = hRow:BUFFER-FIELD('id_sucursal_ubicacion'):BUFFER-VALUE
           RowObjUpd.nromov_ingreso = hRow:BUFFER-FIELD('nromov_ingreso'):BUFFER-VALUE.
    RowObjUpd.ChangedFields = RowObjUpd.ChangedFields + ",id_sucursal_ubicacion,nromov_ingreso".
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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createItemFromLoteUbicacion dTables  _DB-REQUIRED
FUNCTION createItemFromLoteUbicacion RETURNS CHARACTER
  (INPUT hDataSource AS HANDLE , cRows AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VAR iRow           AS INTEGER      NO-UNDO.
  DEFINE VAR cFields        AS CHARACTER    NO-UNDO.
  DEFINE VAR hDataSource1   AS HANDLE       NO-UNDO.
  DEFINE VAR hRowObject     AS HANDLE       NO-UNDO.
  DEFINE VAR hRowObject1    AS HANDLE       NO-UNDO.


  DEFINE VAR iSuc           AS INTEGER      NO-UNDO.
  DEFINE VAR idesde         AS INTEGER      NO-UNDO INITIAL 9999.
  DEFINE VAR ihasta         AS INTEGER      NO-UNDO.
  DEFINE VAR iTipo          AS INTEGER      NO-UNDO INITIAL 20.


  {get dataSource hDataSource1}.
  hRowObject = DYNAMIC-FUNCTION('getRowObject' IN hDataSource1).

  IF hRowObject = ? THEN
      RETURN "Error en la obtencion de Ingreso".

     _transaccion: 
     DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
          cFields = DYNAMIC-FUNCTION ('fetchRow' IN hDataSource , integer(ENTRY(iRow,cRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,cantidad').
          IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".

          CREATE    ITEM_ingreso_Lote_ubicacion.
          ASSIGN    ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion  = hRowObject:BUFFER-FIELD('id_sucursal_ubicacion'):BUFFER-VALUE
                    ITEM_ingreso_lote_ubicacion.nromov_ingreso         = hRowObject:BUFFER-FIELD('nromov_ingreso'):BUFFER-VALUE
                    ITEM_ingreso_lote_ubicacion.id_empresa             = integer(ENTRY(2,cFields,CHR(1)))
                    ITEM_ingreso_lote_ubicacion.id_sucursal            = integer(ENTRY(3,cFields,CHR(1)))
                    ITEM_ingreso_lote_ubicacion.id_tipotambor          = integer(ENTRY(4,cFields,CHR(1)))
                    ITEM_ingreso_lote_ubicacion.nromov                 = integer(ENTRY(5,cFields,CHR(1)))
                    ITEM_ingreso_lote_ubicacion.cantidad               = DECIMAL(ENTRY(6,cFields,CHR(1))).
          
          RUN transferenciaLoteUbicacion IN hProg (ITEM_ingreso_lote_ubicacion.id_empresa , 
                                                   ITEM_ingreso_lote_ubicacion.id_sucursal ,
                                                   ITEM_ingreso_lote_ubicacion.id_tipotambor ,
                                                   ITEM_ingreso_lote_ubicacion.nromov ,
                                                   82,
                                                   ITEM_ingreso_lote_ubicacion.id_sucursal_ubicacion,
                                                   hRowObject:BUFFER-FIELD('fecha'):BUFFER-VALUE,
                                                   ITEM_ingreso_lote_ubicacion.cantidad ,
                                                   20 ,
                                                   TRUE). /*by facundo para hacer pruebas 18/12/2003 hs 8.40*/
         IF RETURN-VALUE <> "" THEN
         DO:
             MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
             UNDO _transaccion , LEAVE _transaccion.
         END.

      END.

  openQuery().

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLote dTables  _DB-REQUIRED
FUNCTION getLote RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR cLote          AS CHARACTER  FORMAT "XXXXXX/XXXX"  NO-UNDO.

    FIND lotes_jugo OF RowObject NO-LOCK NO-ERROR.
    IF NOT AVAILABLE lotes_jugo  THEN
    DO:
        FIND lotes_aceite OF RowObject NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_aceite THEN 
            cLote = STRING(lotes_aceite.id_lote) + STRING(lotes_aceite.anio).
    END.
    ELSE
        cLote = STRING(lotes_jugo.id_lote,"999999") + STRING(lotes_jugo.anio,"9999").

  RETURN cLote.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

