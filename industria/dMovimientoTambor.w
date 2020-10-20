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
&Scoped-define INTERNAL-TABLES movimientos_tambores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio c_fecha c_hora c_usuario datos_adicionales fecha id_articulo~
 id_empresa id_lote id_sucursal id_suc_des id_suc_origen id_tipotambor~
 id_tipo_movimiento nromov nromov_mov tambor_desde tambor_hasta cantidad~
 nro_movimiento
&Scoped-define ENABLED-FIELDS-IN-movimientos_tambores anio c_fecha c_hora ~
c_usuario datos_adicionales fecha id_articulo id_empresa id_lote ~
id_sucursal id_suc_des id_suc_origen id_tipotambor id_tipo_movimiento ~
nromov nromov_mov tambor_desde tambor_hasta cantidad nro_movimiento 
&Scoped-Define DATA-FIELDS  anio articulo Lote c_fecha c_hora c_usuario datos_adicionales fecha~
 id_articulo id_empresa id_lote id_sucursal id_suc_des id_suc_origen~
 id_tipotambor id_tipo_movimiento nromov nromov_mov tambor_desde~
 tambor_hasta cantidad nro_movimiento destino movimiento origen
&Scoped-define DATA-FIELDS-IN-movimientos_tambores anio c_fecha c_hora ~
c_usuario datos_adicionales fecha id_articulo id_empresa id_lote ~
id_sucursal id_suc_des id_suc_origen id_tipotambor id_tipo_movimiento ~
nromov nromov_mov tambor_desde tambor_hasta cantidad nro_movimiento 
&Scoped-Define MANDATORY-FIELDS  id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dmovimientotambor.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH movimientos_tambores ~
      WHERE movimientos_tambores.fecha >= today - 30 NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH movimientos_tambores ~
      WHERE movimientos_tambores.fecha >= today - 30 NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main movimientos_tambores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main movimientos_tambores


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDestino dTables  _DB-REQUIRED
FUNCTION getDestino RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrigen dTables  _DB-REQUIRED
FUNCTION getOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTipoMovimiento dTables  _DB-REQUIRED
FUNCTION getTipoMovimiento RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      movimientos_tambores SCROLLING.
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
     _TblList          = "general.movimientos_tambores"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "general.movimientos_tambores.fecha >= today - 30"
     _FldNameList[1]   > general.movimientos_tambores.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > "_<CALC>"
"getArticulo()" "articulo" ? "x(25)" "character" ? ? ? ? ? ? no ? no 25 no
     _FldNameList[3]   > "_<CALC>"
"getLote()" "Lote" ? "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[4]   > general.movimientos_tambores.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[5]   > general.movimientos_tambores.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[6]   > general.movimientos_tambores.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[7]   > general.movimientos_tambores.datos_adicionales
"datos_adicionales" "datos_adicionales" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[8]   > general.movimientos_tambores.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[9]   > general.movimientos_tambores.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[10]   > general.movimientos_tambores.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[11]   > general.movimientos_tambores.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[12]   > general.movimientos_tambores.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.6 yes
     _FldNameList[13]   > general.movimientos_tambores.id_suc_des
"id_suc_des" "id_suc_des" ? ? "integer" ? ? ? ? ? ? yes ? no 5 yes
     _FldNameList[14]   > general.movimientos_tambores.id_suc_origen
"id_suc_origen" "id_suc_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 4.4 yes
     _FldNameList[15]   > general.movimientos_tambores.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[16]   > general.movimientos_tambores.id_tipo_movimiento
"id_tipo_movimiento" "id_tipo_movimiento" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[17]   > general.movimientos_tambores.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[18]   > general.movimientos_tambores.nromov_mov
"nromov_mov" "nromov_mov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[19]   > general.movimientos_tambores.tambor_desde
"tambor_desde" "tambor_desde" ? ? "integer" ? ? ? ? ? ? yes ? no 14.2 yes
     _FldNameList[20]   > general.movimientos_tambores.tambor_hasta
"tambor_hasta" "tambor_hasta" ? ? "integer" ? ? ? ? ? ? yes ? no 13.6 yes
     _FldNameList[21]   > general.movimientos_tambores.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[22]   > general.movimientos_tambores.nro_movimiento
"nro_movimiento" "nro_movimiento" ? ? "integer" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[23]   > "_<CALC>"
"getDestino()" "destino" "Destino" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no
     _FldNameList[24]   > "_<CALC>"
"getTipoMovimiento()" "movimiento" "Movimiento" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no
     _FldNameList[25]   > "_<CALC>"
"getOrigen()" "origen" "Origen" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no
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

DEFINE BUFFER ROU       FOR RowObjUpd.
DEFINE VAR idif         AS INTEGER  NO-UNDO.
DEFINE VAR hContainer   AS HANDLE   NO-UNDO.
DEFINE VAR hProg        AS HANDLE   NO-UNDO.


hContainer = DYNAMIC-FUNCTION('linkHandles' , 'Container-source').

hProg = DYNAMIC-FUNCTION('getLoteUbicacionHandle' IN hContainer).

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "U".
    FIND FIRST ROU WHERE ROU.RowNum = RowObjUpd.RowNum AND ROU.RowMod = "" NO-ERROR.
    IF AVAILABLE ROU THEN
    DO:
        idif = RowObjUpd.cantidad - ROU.cantidad .
        IF idif <> 0 THEN
        DO:
            IF idif > 0  THEN
                RUN transferenciaLoteUbicacion IN hProg (RowObjUpd.id_empresa , 
                                                         RowObjUpd.id_sucursal ,
                                                         RowObjUpd.id_tipotambor ,
                                                         RowObjUpd.nromov ,
                                                         RowObjUpd.id_suc_origen,
                                                         RowObjUpd.id_suc_des,
                                                         RowObjUpd.fecha,
                                                         idif,
                                                         RowObjUpd.id_tipo_movimiento,
                                                         TRUE).
            ELSE
                RUN transferenciaLoteUbicacion IN hProg (RowObjUpd.id_empresa , 
                                                         RowObjUpd.id_sucursal ,
                                                         RowObjUpd.id_tipotambor ,
                                                         RowObjUpd.nromov ,
                                                         RowObjUpd.id_suc_des,
                                                         RowObjUpd.id_suc_origen,
                                                         RowObjUpd.fecha,
                                                         abs(idif),
                                                         RowObjUpd.id_tipo_movimiento,
                                                         TRUE).

            IF RETURN-VALUE <> "" THEN
                RETURN RETURN-VALUE.

            RUN getDesdeHasta IN hProg (RowObjUpd.id_empresa , 
                                        RowObjUpd.id_sucursal ,
                                        RowObjUpd.id_tipotambor ,
                                        RowObjUpd.nromov ,
                                        RowObjUpd.id_suc_des,
                                        OUTPUT RowObjUpd.tambor_desde ,
                                        OUTPUT RowObjUpd.tambor_hasta ).

            IF RETURN-VALUE <> "" THEN
                RETURN RETURN-VALUE.
            RowObjUpd.ChangedFields = RowObjUpd.ChangedFields + ',tambor_desde,tambor_hasta'.
        END.
    END.
END.

FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D".
      
    RUN transferenciaLoteUbicacion IN hProg (RowObjUpd.id_empresa , 
                                             RowObjUpd.id_sucursal ,
                                             RowObjUpd.id_tipotambor ,
                                             RowObjUpd.nromov ,
                                             RowObjUpd.id_suc_des,
                                             RowObjUpd.id_suc_origen,
                                             RowObjUpd.fecha,
                                             RowObjUpd.cantidad,
                                             RowObjUpd.id_tipo_movimiento,
                                             TRUE).
    IF RETURN-VALUE <> "" THEN
        RETURN RETURN-VALUE.
    DYNAMIC-FUNCTION ('openQuery' IN hProg).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createMovimientoTambor dTables  _DB-REQUIRED
PROCEDURE createMovimientoTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phLoteUbicacion      AS HANDLE       NO-UNDO.
DEFINE INPUT PARAMETER phSucursalDestino    AS HANDLE       NO-UNDO.
DEFINE INPUT PARAMETER phTipoMovimiento     AS HANDLE       NO-UNDO.
DEFINE INPUT PARAMETER piCantidad           AS INTEGER      NO-UNDO.
DEFINE INPUT PARAMETER phlibLoteUbicacion   AS HANDLE       NO-UNDO.

DEFINE VAR hField                           AS HANDLE       NO-UNDO.
DEFINE VAR hField1                          AS HANDLE       NO-UNDO.
DEFINE VAR hField2                          AS HANDLE       NO-UNDO.
DEFINE VAR viNro                            AS INTEGER      NO-UNDO.
DEFINE VAR vcQuery                          AS CHARACTER    NO-UNDO.
DEFINE VAR viSerial                         AS INTEGER      NO-UNDO.

DEFINE BUFFER mt                            FOR movimientos_tambores.

DO TRANSACTION ON ERROR UNDO, RETURN 'Error al generar movimiento tambor':
    
    hField = phLoteUbicacion:BUFFER-FIELD('id_empresa').
    
    FOR LAST mt WHERE mt.id_empresa = hField:BUFFER-VALUE BY nro_movimiento.
        viNro = mt.nro_movimiento.
    END.
    viNro = vinro + 1.
    
    CREATE movimientos_tambor.
    ASSIGN  movimientos_tambores.c_fecha            = today
            movimientos_tambores.c_hora             = string(time,"hh:mm:ss")
            movimientos_tambores.c_usuario          = userid("userdb")
            movimientos_tambores.datos_adicionales  = "Sin datos adicionales"
            movimientos_tambores.fecha              = today.

    movimientos_tambores.id_empresa         = hfield:BUFFER-VALUE.
    hField = phLoteUbicacion:BUFFER-FIELD('id_sucursal').
    movimientos_tambores.id_sucursal        = hfield:BUFFER-VALUE.
    hField = phLoteUbicacion:BUFFER-FIELD('id_tipotambor').
    movimientos_tambores.id_tipotambor      = hfield:BUFFER-VALUE.
    hField = phLoteUbicacion:BUFFER-FIELD('nromov').
    movimientos_tambores.nromov             = hfield:BUFFER-VALUE.
    movimientos_tambores.cantidad           = piCantidad.
    hField = phLoteUbicacion:BUFFER-FIELD('id_sucursal_ubicacion').
    movimientos_tambores.id_suc_origen      = hfield:BUFFER-VALUE.

    hField = phLoteUbicacion:BUFFER-FIELD('lote').
    movimientos_tambores.id_lote            = INTEGER (SUBSTRING (hfield:BUFFER-VALUE,1,6)).
    movimientos_tambores.anio               = INTEGER (SUBSTRING (hfield:BUFFER-VALUE,8,4)).
    
    hField = phLoteUbicacion:BUFFER-FIELD('id_articulo').
    movimientos_tambores.id_articulo        = INTEGER (hfield:BUFFER-VALUE).

    hField = phsucursalDestino:BUFFER-FIELD('id_sucursal').
    movimientos_tambores.id_suc_des      = hfield:BUFFER-VALUE.

    
    hField = phTipoMovimiento:BUFFER-FIELD('id_tipo_movimiento').
    movimientos_tambores.id_tipo_movimiento  = hfield:BUFFER-VALUE.
    
    movimientos_tambores.nromov_mov          = NEXT-VALUE(seq-movimiento).
    movimientos_tambores.nro_movimiento = viNro + 1.

    viSerial = 0.

    RUN getDesdeHasta IN phlibLoteUbicacion 
        (  input movimientos_tambores.id_empresa,
           input movimientos_tambores.id_sucursal,
           input movimientos_tambores.id_tipotambor,
           input movimientos_tambores.nromov ,
           INPUT movimientos_tambores.id_suc_origen ,
           INPUT movimientos_tambores.cantidad,
           OUTPUT movimientos_tambores.tambor_desde,
           OUTPUT movimientos_tambores.tambor_hasta ).
    
    IF RETURN-VALUE <> "" THEN
        UNDO , RETURN RETURN-VALUE.

    RUN transferenciaLoteUbicacion IN phlibLoteUbicacion (  input movimientos_tambores.id_empresa,
                                                            input movimientos_tambores.id_sucursal,
                                                            input movimientos_tambores.id_tipotambor,
                                                            input movimientos_tambores.nromov ,
                                                            INPUT movimientos_tambores.id_suc_origen ,
                                                            INPUT movimientos_tambores.id_suc_des ,
                                                            INPUT movimientos_tambores.fecha ,
                                                            INPUT movimientos_tambores.cantidad ,
                                                            INPUT movimientos_tambores.id_tipo_movimiento ,
                                                            INPUT TRUE ).
    IF RETURN-VALUE <> "" THEN
        UNDO , RETURN RETURN-VALUE.
    
    
    vcQuery = "for each movimientos_tambores where movimientos_tambores.id_empresa = " + STRING(movimientos_tambores.id_empresa) + " and movimientos_tambores.nro_movimiento = " + STRING(movimientos_tambores.nro_movimiento) .
    {set queryWhere vcQuery}.
    openQuery().
    
END.
RETURN "".
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
         rowObject.articulo = (getArticulo())
         rowObject.destino = (getDestino())
         rowObject.Lote = (getLote())
         rowObject.movimiento = (getTipoMovimiento())
         rowObject.origen = (getOrigen())
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

  FIND CURRENT RowObject .
  FIND productos_terminados OF RowObject NO-LOCK NO-ERROR.
  
  IF AVAILABLE productos_terminados THEN
      RETURN productos_terminados.descripcion.

  RETURN "Sin Articulo".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDestino dTables  _DB-REQUIRED
FUNCTION getDestino RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND sucursales WHERE sucursales.id_sucursal = RowObject.id_suc_des NO-LOCK NO-ERROR.

  IF AVAILABLE sucursales THEN
      RETURN sucursal.nombre.
  ELSE
      RETURN "Sucursal inexistente".


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
  FIND CURRENT RowObject.
  /*
  FIND FIRST lotes_ubicacion WHERE 
      lotes_ubicacion.id_empresa    = RowObject.id_empresa AND
      lotes_ubicacion.id_sucursal   = RowObject.id_sucursal AND
      lotes_ubicacion.id_tipotambor = RowObject.id_tipotambor AND
      lotes_ubicacion.nromov        = RowObject.nromov NO-LOCK NO-ERROR.
  IF AVAILABLE Lotes_ubicacion THEN
      RETURN lotes_ubicacion.lote.
  */
  RETURN STRING(RowObject.id_lote, "999999") + "/" + STRING(RowObject.anio,"9999").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrigen dTables  _DB-REQUIRED
FUNCTION getOrigen RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND sucursales WHERE sucursales.id_sucursal = RowObject.id_suc_origen NO-LOCK NO-ERROR.

  IF AVAILABLE sucursales THEN
      RETURN sucursal.nombre.
  ELSE
      RETURN "Sucursal inexistente".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTipoMovimiento dTables  _DB-REQUIRED
FUNCTION getTipoMovimiento RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND tipos_movimientos WHERE tipos_movimientos.id_tipo_movimiento = RowObject.id_tipo_movimiento NO-LOCK NO-ERROR.
  IF AVAILABLE tipos_movimientos  THEN
      RETURN tipos_movimientos.descripcion.
  ELSE
      RETURN "Tipo de Movimiento inexistente".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

