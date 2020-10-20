&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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

DEFINE VARIABLE hProg AS HANDLE     NO-UNDO.

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
&Scoped-define INTERNAL-TABLES items_release_delivery vapores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio_contrato cantidad contenedor c_fecha c_hora c_usuario id_contrato~
 id_empresa id_envase id_lote id_lote_deposito id_release_delivery~
 id_sucursal id_sucursal_ubicacion id_tipotambor id_tipo_contrato~
 id_unidad_medida id_vapor item_contrato item_release_delivery nromov~
 tambores
&Scoped-define ENABLED-FIELDS-IN-items_release_delivery anio_contrato ~
cantidad contenedor c_fecha c_hora c_usuario id_contrato id_empresa ~
id_envase id_lote id_lote_deposito id_release_delivery id_sucursal ~
id_sucursal_ubicacion id_tipotambor id_tipo_contrato id_unidad_medida ~
id_vapor item_contrato item_release_delivery nromov tambores 
&Scoped-Define DATA-FIELDS  anio_contrato Calidad Envase Peso Galones cantidad contenedor c_fecha~
 c_hora c_usuario id_contrato id_empresa id_envase id_lote id_lote_deposito~
 id_release_delivery id_sucursal id_sucursal_ubicacion id_tipotambor~
 id_tipo_contrato id_unidad_medida id_vapor item_contrato~
 item_release_delivery nromov tambores Vapor
&Scoped-define DATA-FIELDS-IN-items_release_delivery anio_contrato cantidad ~
contenedor c_fecha c_hora c_usuario id_contrato id_empresa id_envase ~
id_lote id_lote_deposito id_release_delivery id_sucursal ~
id_sucursal_ubicacion id_tipotambor id_tipo_contrato id_unidad_medida ~
id_vapor item_contrato item_release_delivery nromov tambores 
&Scoped-define DATA-FIELDS-IN-vapores Vapor 
&Scoped-Define MANDATORY-FIELDS  id_sucursal_ubicacion
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Vapor = vapores.descripcion
&Scoped-Define DATA-FIELD-DEFS "dItemsRelease.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_release_delivery NO-LOCK, ~
      EACH vapores OF items_release_delivery NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_release_delivery NO-LOCK, ~
      EACH vapores OF items_release_delivery NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_release_delivery vapores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_release_delivery
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main vapores


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createItemReleaseFromLUbicacionContrato dTables  _DB-REQUIRED
FUNCTION createItemReleaseFromLUbicacionContrato RETURNS CHARACTER
  (INPUT hDataSource    AS HANDLE , 
   INPUT cRows          AS CHARACTER , 
   INPUT pcContrato     AS CHARACTER , 
   INPUT piItem         AS INTEGER,
   INPUT piTipoContrato AS INTEGER, 
   INPUT piAnioContrato AS INTEGER)  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEnvase dTables  _DB-REQUIRED
FUNCTION getEnvase RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGalones dTables  _DB-REQUIRED
FUNCTION getGalones RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPeso dTables  _DB-REQUIRED
FUNCTION getPeso RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_release_delivery, 
      vapores SCROLLING.
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
     _TblList          = "general.items_release_delivery,comercial.vapores OF general.items_release_delivery"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.items_release_delivery.anio_contrato
"anio_contrato" "anio_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[3]   > "_<CALC>"
"getEnvase()" "Envase" "Envase" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[4]   > "_<CALC>"
"getPeso()" "Peso" "Peso" "<<<<<<<9" "Decimal" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[5]   > "_<CALC>"
"getGalones()" "Galones" "Galones" "<<<<<<<9" "Decimal" ? ? ? ? ? ? no ? no 9.6 no
     _FldNameList[6]   > general.items_release_delivery.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[7]   > general.items_release_delivery.contenedor
"contenedor" "contenedor" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[8]   > general.items_release_delivery.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[9]   > general.items_release_delivery.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[10]   > general.items_release_delivery.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[11]   > general.items_release_delivery.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[12]   > general.items_release_delivery.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[13]   > general.items_release_delivery.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[14]   > general.items_release_delivery.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[15]   > general.items_release_delivery.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[16]   > general.items_release_delivery.id_release_delivery
"id_release_delivery" "id_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[17]   > general.items_release_delivery.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[18]   > general.items_release_delivery.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? yes 21 yes
     _FldNameList[19]   > general.items_release_delivery.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 yes
     _FldNameList[20]   > general.items_release_delivery.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[21]   > general.items_release_delivery.id_unidad_medida
"id_unidad_medida" "id_unidad_medida" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[22]   > general.items_release_delivery.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[23]   > general.items_release_delivery.item_contrato
"item_contrato" "item_contrato" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[24]   > general.items_release_delivery.item_release_delivery
"item_release_delivery" "item_release_delivery" ? ? "integer" ? ? ? ? ? ? yes ? no 20.2 yes
     _FldNameList[25]   > general.items_release_delivery.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[26]   > general.items_release_delivery.tambores
"tambores" "tambores" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[27]   > comercial.vapores.descripcion
"descripcion" "Vapor" "Vapor" ? "character" ? ? ? ? ? ? no ? no 30 yes
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
         rowObject.Calidad = (getCalidad())
         rowObject.Envase = (getEnvase())
         rowObject.Galones = (getGalones())
         rowObject.Peso = (getPeso())
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
FIND FIRST RowObjUpd NO-ERROR. 

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D".
  RUN releaseLoteUbicacion IN hProg (RowObjUpd.id_empresa, 
                                     RowObjUpd.id_sucursal, 
                                     RowObjUpd.id_tipotambor, 
                                     RowObjUpd.nromov, 
                                     RowObjUpd.id_sucursal_ubicacion, 
                                     RowObjUpd.tambores).
END.

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

  RUN libItemsReleaseDelivery.p PERSISTENT SET hProg.
  
  RUN SUPER.

  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createItemReleaseFromLUbicacionContrato dTables  _DB-REQUIRED
FUNCTION createItemReleaseFromLUbicacionContrato RETURNS CHARACTER
  (INPUT hDataSource    AS HANDLE , 
   INPUT cRows          AS CHARACTER , 
   INPUT pcContrato     AS CHARACTER , 
   INPUT piItem         AS INTEGER,
   INPUT piTipoContrato AS INTEGER, 
   INPUT piAnioContrato AS INTEGER) :
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
  DEFINE VAR iSeq           AS INTEGER      NO-UNDO.
  DEFINE VAR iItem          AS INTEGER      NO-UNDO.
  DEFINE VAR iVapor         AS INTEGER      NO-UNDO INITIAL 9999.
  DEFINE VAR cCont          AS CHARACTER    NO-UNDO.
  DEFINE VAR cNroPL         AS CHARACTER    NO-UNDO INITIAL 20.
  DEFINE VAR iLote          AS INTEGER      NO-UNDO.
  DEFINE VAR cPL            AS CHARACTER    NO-UNDO.
  DEFINE VAR cLote          AS CHARACTER    NO-UNDO.
  DEFINE VAR iCant          AS INTEGER      NO-UNDO.
  DEFINE VAR iComp          AS INTEGER      NO-UNDO.

  DEFINE BUFFER bItemRelease FOR ITEMs_release_delivery.


  {get dataSource hDataSource1}.
  hRowObject = DYNAMIC-FUNCTION('getRowObject' IN hDataSource1).

  IF hRowObject = ? THEN DO:
    RETURN "Error en la obtencion de Ingreso".
  END.
      
  /*encuentro proximo valor de item*/
  FIND LAST items_release_delivery WHERE items_release_delivery.id_release_delivery = INTEGER(hRowObject:BUFFER-FIELD('id_release_delivery'):BUFFER-VALUE)
                                     NO-LOCK NO-ERROR.
  IF AVAILABLE items_release_delivery THEN DO:
    iItem = ITEM_release_delivery + 1.
  END.
  ELSE DO:
    iItem = 1.
  END.
  
  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow' IN hDataSource , INTEGER(ENTRY(iRow,cRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,cantidad,id_lote_deposito,lote,id_sucursal_ubicacion,cantidad_comprometida').
    IF cFields EQ ? OR cFields = "" THEN DO: 
      RETURN-VALUE = "Error en cFields".
      RETURN "Error".
    END.

    /*
    /*elimino items anteriores del mismo lote*/
    FOR EACH bItemRelease WHERE bItemRelease.id_sucursal_ubicacion = INTEGER(ENTRY(9, cFields, CHR(1)))
                            AND bItemRelease.id_empresa            = INTEGER(ENTRY(2, cFields, CHR(1)))
                            AND bItemRelease.id_sucursal           = INTEGER(ENTRY(3, cFields, CHR(1)))
                            AND bItemRelease.id_tipotambor         = INTEGER(ENTRY(4, cFields, CHR(1)))
                            AND bItemRelease.nromov                = INTEGER(ENTRY(5, cFields, CHR(1))).
      DELETE bItemRelease.
    END.
    */
    /*obtengo datos de packing_list*/
    RUN getPackingList IN hProg (INPUT INTEGER(ENTRY(2, cFields, CHR(1))), 
                                 INPUT INTEGER(ENTRY(3, cFields, CHR(1))),
                                 INPUT INTEGER(ENTRY(4, cFields, CHR(1))),
                                 INPUT INTEGER(ENTRY(5, cFields, CHR(1))), 
                                 OUTPUT cPL).
     IF cPL <> ",,,," THEN DO:
       iVapor = INTEGER(ENTRY(3, cPL)).
       cCont  = ENTRY(4, cPL).
       cNroPL = ENTRY(5, cPL).
     END.

     RUN getLoteDeposito IN hProg (INPUT INTEGER(ENTRY(2, cFields, CHR(1))), 
                                   INPUT INTEGER(ENTRY(3, cFields, CHR(1))),
                                   INPUT INTEGER(ENTRY(4, cFields, CHR(1))),
                                   INPUT INTEGER(ENTRY(5, cFields, CHR(1))), 
                                   OUTPUT cLote).
    iCant = INTEGER(ENTRY(6, cFields, CHR(1))) - INTEGER(ENTRY(10, cFields, CHR(1))).
    /*creo item release*/
    CREATE items_release_delivery.
    ASSIGN items_release_delivery.id_release_delivery   = hRowObject:BUFFER-FIELD('id_release_delivery'):BUFFER-VALUE
           items_release_delivery.ITEM_release_delivery = iItem
           items_release_delivery.id_sucursal_ubicacion = INTEGER(ENTRY(9, cFields, CHR(1)))
           items_release_delivery.id_empresa            = INTEGER(ENTRY(2, cFields, CHR(1)))
           items_release_delivery.id_sucursal           = INTEGER(ENTRY(3, cFields, CHR(1)))
           items_release_delivery.id_tipotambor         = INTEGER(ENTRY(4, cFields, CHR(1)))
           items_release_delivery.nromov                = INTEGER(ENTRY(5, cFields, CHR(1)))
           items_release_delivery.tambores              = iCant
           items_release_delivery.id_lote_deposito      = cLote
           items_release_delivery.id_lote               = INTEGER(SUBSTRING(ENTRY(8, cFields, CHR(1)), 1, 6))
           items_release_delivery.id_vapor              = iVapor
           items_release_delivery.contenedor            = cCont
           items_release_delivery.id_contrato           = pcContrato
           items_release_delivery.ITEM_contrato         = piItem
           items_release_delivery.id_tipo_contrato      = piTipoContrato
           items_release_delivery.anio_contrato         = piAnioContrato.
    
    iItem = iItem + 1.

    /*actualizo la cantidad_comprometida en lote_ubicacion*/
    
    FOR FIRST lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = INTEGER(ENTRY(2, cFields, CHR(1)))
                                AND lotes_ubicacion.id_sucursal           = INTEGER(ENTRY(3, cFields, CHR(1)))
                                AND lotes_ubicacion.id_tipotambor         = INTEGER(ENTRY(4, cFields, CHR(1)))
                                AND lotes_ubicacion.nromov                = INTEGER(ENTRY(5, cFields, CHR(1)))
                                AND lotes_ubicacion.id_sucursal_ubicacion = INTEGER(ENTRY(9, cFields, CHR(1))).
      ASSIGN lotes_ubicacion.cantidad_comprometida = lotes_ubicacion.cantidad_comprometida + iCant.
    END.
    
    
  END.

  openQuery().
  DYNAMIC-FUNCTION('openQuery' IN hDataSource).
  
  DEFINE VARIABLE hData      AS HANDLE     NO-UNDO.

  hData = DYNAMIC-FUNCTION('getDataSource').
  RUN refreshRow IN hData.

  RETURN "".   /* Function return value. */
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
  
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO INITIAL "".
  FIND CURRENT RowObject NO-ERROR.
  IF AVAILABLE RowObject THEN DO:
    RUN getCalidad IN hProg (RowObject.id_empresa, 
                             RowObject.id_sucursal, 
                             RowObject.id_tipotambor, 
                             RowObject.nromov, 
                             OUTPUT cRet).
    RETURN cRet.
  END.
  
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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  FIND CURRENT RowObject NO-ERROR.
  RUN getEnvase IN hProg (RowObject.id_empresa, 
                          RowObject.id_sucursal, 
                          RowObject.id_tipotambor, 
                          RowObject.nromov, 
                          OUTPUT cRet).
  RETURN cRet.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGalones dTables  _DB-REQUIRED
FUNCTION getGalones RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
  DEFINE VAR dPeso AS DECIMAL NO-UNDO.
  DEFINE VAR dGal  AS DECIMAL NO-UNDO.

  FIND CURRENT RowOBject NO-LOCK NO-ERROR.

  IF AVAILABLE RowOBject THEN DO:
    FIND LAST inspecciones_lote WHERE inspecciones_lote.id_empresa       =   RowObject.id_empresa AND 
                                      inspecciones_lote.id_sucursal      =   RowObject.id_sucursal AND
                                      inspecciones_lote.id_tipotambor    =   RowObject.id_tipotambor AND
                                      inspecciones_lote.nromov           =   RowObject.nromov 
                                NO-LOCK NO-ERROR.
    IF AVAILABLE inspecciones_lote THEN DO:                                     
      FOR EACH tambores_industria  WHERE  tambores_industria.nromov                = RowObject.nromov AND
                                          tambores_industria.id_sucursal_ubicacion = RowObject.id_sucursal_ubicacion 
                                   NO-LOCK.
        dPeso = dPeso + tambores_industria.kilos_tambor.
      END.
      FIND LAST brix WHERE brix.brix <= ROUND(inspecciones_lote.bx_correg,1) 
                     NO-LOCK NO-ERROR.
      IF AVAILABLE brix THEN DO:
        dGal = (dPeso / brix.pe) / 3.785.
      END.
    END.
  END.

  RETURN dGal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPeso dTables  _DB-REQUIRED
FUNCTION getPeso RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  DEFINE VAR dPeso AS DECIMAL NO-UNDO.

  FIND CURRENT RowOBject NO-LOCK NO-ERROR.
  IF AVAILABLE RowOBject THEN DO:
    FOR EACH tambores_industria  WHERE  tambores_industria.nromov                = RowObject.nromov AND
                                        tambores_industria.id_sucursal_ubicacion = RowObject.id_sucursal_ubicacion 
                                 NO-LOCK.
        dPeso = dPeso + tambores_industria.kilos_tambor.
    END.
  END.
  

  RETURN dPeso.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

