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


DEFINE VARIABLE vhLib AS HANDLE     NO-UNDO.

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
&Scoped-define INTERNAL-TABLES recepcion_cascara_externa

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  id_suc_origen id_suc_destino cantidad kilos id_transporte nombre_chofer~
 nro_patente observaciones aux_1 aux_2 c_usuario c_fecha c_hora fecha aux_3
&Scoped-define ENABLED-FIELDS-IN-recepcion_cascara_externa id_suc_origen ~
id_suc_destino cantidad kilos id_transporte nombre_chofer nro_patente ~
observaciones aux_1 aux_2 c_usuario c_fecha c_hora fecha aux_3 
&Scoped-Define DATA-FIELDS  id_suc_origen Origen id_suc_destino Destino cantidad kilos Transporte~
 id_transporte nombre_chofer nro_patente observaciones aux_1 aux_2 c_usuario~
 c_fecha c_hora fecha aux_3
&Scoped-define DATA-FIELDS-IN-recepcion_cascara_externa id_suc_origen ~
id_suc_destino cantidad kilos id_transporte nombre_chofer nro_patente ~
observaciones aux_1 aux_2 c_usuario c_fecha c_hora fecha aux_3 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "drecepcioncascara.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH recepcion_cascara_externa NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH recepcion_cascara_externa NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main recepcion_cascara_externa
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main recepcion_cascara_externa


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucDes dTables  _DB-REQUIRED
FUNCTION getSucDes RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucOri dTables  _DB-REQUIRED
FUNCTION getSucOri RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTransporte dTables  _DB-REQUIRED
FUNCTION getTransporte RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      recepcion_cascara_externa SCROLLING.
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
     _TblList          = "general.recepcion_cascara_externa"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.recepcion_cascara_externa.id_suc_origen
"id_suc_origen" "id_suc_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 16.2 yes
     _FldNameList[2]   > "_<CALC>"
"getSucOri()" "Origen" "Origen" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no
     _FldNameList[3]   > general.recepcion_cascara_externa.id_suc_destino
"id_suc_destino" "id_suc_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[4]   > "_<CALC>"
"getSucDes()" "Destino" "Destino" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no
     _FldNameList[5]   > general.recepcion_cascara_externa.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 11.2 yes
     _FldNameList[6]   > general.recepcion_cascara_externa.kilos
"kilos" "kilos" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[7]   > "_<CALC>"
"getTransporte()" "Transporte" "Transporte" "x(50)" "character" ? ? ? ? ? ? no ? no 50 no
     _FldNameList[8]   > general.recepcion_cascara_externa.id_transporte
"id_transporte" "id_transporte" ? ? "integer" ? ? ? ? ? ? yes ? no 14.6 yes
     _FldNameList[9]   > general.recepcion_cascara_externa.nombre_chofer
"nombre_chofer" "nombre_chofer" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[10]   > general.recepcion_cascara_externa.nro_patente
"nro_patente" "nro_patente" ? ? "character" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[11]   > general.recepcion_cascara_externa.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
     _FldNameList[12]   > general.recepcion_cascara_externa.aux_1
"aux_1" "aux_1" "Fecha" ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[13]   > general.recepcion_cascara_externa.aux_2
"aux_2" "aux_2" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[14]   > general.recepcion_cascara_externa.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[15]   > general.recepcion_cascara_externa.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[16]   > general.recepcion_cascara_externa.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[17]   > general.recepcion_cascara_externa.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[18]   > general.recepcion_cascara_externa.aux_3
"aux_3" "aux_3" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
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
DEFINE VARIABLE vcLotes AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcRow   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i       AS INTEGER    NO-UNDO.


FIND LAST RowObjUpd NO-ERROR.
FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
  vcLotes = rowObjUpd.aux_3.
  DO i = 1 TO NUM-ENTRIES(vcLotes, CHR(10)) - 1:
    vcRow = ENTRY(i, vcLotes, CHR(10)).
    RUN transferenciaLoteCascara IN vhLib (INTEGER(ENTRY(1, vcRow)), 
                                           INTEGER(ENTRY(2, vcRow)), 
                                           INTEGER(ENTRY(3, vcRow)), 
                                           rowObjUpd.id_suc_origen, 
                                           rowObjUpd.id_suc_destino, 
                                           INTEGER(ENTRY(4, vcRow))).    
  END.
  
  ASSIGN rowObjUpd.c_usuario = USERID("userdb")
         rowObjUpd.c_fecha   = TODAY
         rowObjUpd.c_hora    = STRING(TIME,"HH:MM:SS").
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".
END.

FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".
  ASSIGN rowObject.c_usuario = USERID("userdb")
         rowObject.c_fecha   = TODAY
         rowObject.c_hora    = STRING(TIME,"HH:MM:SS").
  rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".
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
         rowObject.Destino = (getSucDes())
         rowObject.Origen = (getSucOri())
         rowObject.Transporte = (getTransporte())
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

  RUN libLotesUbicacion.p PERSISTENT SET vhLib.
  THIS-PROCEDURE:ADD-SUPER-PROCEDURE(vhLib).
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferirLoteCascara dTables  _DB-REQUIRED
PROCEDURE transferirLoteCascara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*1.- si existe en lote_ubicacion ==> modificar cantidad, sino ==> crear lote_ubicacion
  2.- llamar a la rutina de cacha para stock.*/
  DEFINE INPUT PARAMETER piEmpresa  AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucursal AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piNroMov   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucOri   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucDes   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piCantidad AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER pcSigno    AS CHARACTER  NO-UNDO.
  DEFINE INPUT PARAMETER piTipoMov  AS INTEGER    NO-UNDO.

  DEFINE VARIABLE viSucOri AS INTEGER    NO-UNDO.
  DEFINE VARIABLE viSucDes AS INTEGER    NO-UNDO.

  DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
    FIND lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = piEmpresa 
                           AND lotes_ubicacion.id_sucursal           = piSucursal
                           AND lotes_ubicacion.id_tipotambor         = 11
                           AND lotes_ubicacion.nromov                = piNroMov
                           AND lotes_ubicacion.id_sucursal_ubicacion = piSucursal
                         NO-ERROR.
    IF AVAILABLE lotes_ubicacion THEN DO:
      IF pcSigno = "+" THEN
        ASSIGN lotes_ubicacion.cantidad = lotes_ubicacion.cantidad + piCantidad.
      ELSE DO:
        ASSIGN lotes_ubicacion.cantidad = lotes_ubicacion.cantidad - piCantidad.
        IF lotes_ubicacion.cantidad <= 0 THEN
          DELETE lotes_ubicacion.
      END.
    END.
    ELSE DO:
      FIND lotes_cascara WHERE lotes_cascara.nromov = piNroMov NO-LOCK NO-ERROR.
      FIND calidades WHERE calidades.id_calidad = lotes_cascara.id_calidad NO-LOCK NO-ERROR.
      FIND envases_prod WHERE lotes_cascara.id_envase = envases_prod.id_envase NO-LOCK NO-ERROR.
      CREATE  lotes_ubicacion.
      ASSIGN  lotes_ubicacion.id_empresa            = piEmpresa
              lotes_ubicacion.id_sucursal           = piSucursal
              lotes_ubicacion.id_tipotambor         = 11
              lotes_ubicacion.nromov                = piNroMov
              lotes_ubicacion.id_sucursal_ubicacion = piSucursal
              lotes_ubicacion.cantidad              = piCantidad
              lotes_ubicacion.id_lote               = lotes_cascara.id_lote
              lotes_ubicacion.calidad               = calidades.descripcion
              lotes_ubicacion.envase                = envases_prod.descripcion
              lotes_ubicacion.lote                  = STRING(lotes_cascara.id_lote,"999999") + STRING(lotes_cascara.anio,"9999").
    END.
  
    viSucOri = piSucOri. 
    viSucDes = piSucDes.
    /*
    IF pcSigno = "-" AND piTipoMov <> 34 THEN DO: /*34 = reproceso no intercambiar las sucursales*/
      viSucOri = piSucDes.
      viSucDes = piSucOri.
    END.
    */

    RUN y_gstkmovdep_cascara.p (piEmpresa, 
                                piSucursal,
                                11,
                                piNroMov,
                                viSucOri,
                                viSucDes,
                                1,
                                piCantidad,
                                piTipoMov,
                                TODAY).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucDes dTables  _DB-REQUIRED
FUNCTION getSucDes RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_suc_des
                        NO-LOCK NO-ERROR.
  IF AVAILABLE sucursales THEN
    RETURN sucursal.nombre.
  ELSE
    RETURN "no-info".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucOri dTables  _DB-REQUIRED
FUNCTION getSucOri RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST sucursales WHERE sucursales.id_sucursal = rowObject.id_suc_ori
                        NO-LOCK NO-ERROR.
  IF AVAILABLE sucursales THEN
    RETURN sucursal.nombre.
  ELSE
    RETURN "no-info".
 
 END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTransporte dTables  _DB-REQUIRED
FUNCTION getTransporte RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.

  FIND FIRST proveedores WHERE proveedores.id_proveedor = rowObject.id_transporte
                         NO-LOCK NO-ERROR.
  IF AVAILABLE proveedores THEN
    RETURN proveedores.razon_social.
  ELSE 
    RETURN "no-info".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

