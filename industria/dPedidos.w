&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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
&Scoped-define INTERNAL-TABLES pedidos cant_pedidos proveedores

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  recibido estado bultos cantidad cant_recibida id_articulo id_suc_origen~
 peso unidades
&Scoped-define ENABLED-FIELDS-IN-pedidos recibido estado 
&Scoped-define ENABLED-FIELDS-IN-cant_pedidos bultos cantidad cant_recibida ~
id_articulo id_suc_origen peso unidades 
&Scoped-Define DATA-FIELDS  id_pedido id_proveedor nombre recibido estado bultos cantidad cant_recibida~
 id_articulo id_suc_origen peso unidades
&Scoped-define DATA-FIELDS-IN-pedidos id_pedido id_proveedor recibido ~
estado 
&Scoped-define DATA-FIELDS-IN-cant_pedidos bultos cantidad cant_recibida ~
id_articulo id_suc_origen peso unidades 
&Scoped-define DATA-FIELDS-IN-proveedores nombre 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dpedidos.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH pedidos ~
      WHERE pedidos.id_origen_compra = 3 ~
 AND not pedidos.recibido NO-LOCK, ~
      EACH cant_pedidos OF pedidos NO-LOCK, ~
      EACH proveedores OF pedidos NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH pedidos ~
      WHERE pedidos.id_origen_compra = 3 ~
 AND not pedidos.recibido NO-LOCK, ~
      EACH cant_pedidos OF pedidos NO-LOCK, ~
      EACH proveedores OF pedidos NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main pedidos cant_pedidos proveedores
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main pedidos
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main cant_pedidos
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main proveedores


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD proceso-orden-compra dTables  _DB-REQUIRED
FUNCTION proceso-orden-compra RETURNS CHARACTER
  (INPUT pProductoTerceros AS ROWID /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      pedidos, 
      cant_pedidos, 
      proveedores SCROLLING.
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
     _TblList          = "comercial.pedidos,comercial.cant_pedidos OF comercial.pedidos,comercial.proveedores OF comercial.pedidos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "comercial.pedidos.id_origen_compra = 3
 AND not comercial.pedidos.recibido"
     _FldNameList[1]   > comercial.pedidos.id_pedido
"id_pedido" "id_pedido" ? ? "integer" ? ? ? ? ? ? no ? no 11 yes
     _FldNameList[2]   > comercial.pedidos.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? no ? no 9.4 yes
     _FldNameList[3]   > comercial.proveedores.nombre
"nombre" "nombre" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[4]   > comercial.pedidos.recibido
"recibido" "recibido" ? ? "logical" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[5]   > comercial.pedidos.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[6]   > comercial.cant_pedidos.bultos
"bultos" "bultos" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[7]   > comercial.cant_pedidos.cantidad
"cantidad" "cantidad" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[8]   > comercial.cant_pedidos.cant_recibida
"cant_recibida" "cant_recibida" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[9]   > comercial.cant_pedidos.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[10]   > comercial.cant_pedidos.id_suc_origen
"id_suc_origen" "id_suc_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[11]   > comercial.cant_pedidos.peso
"peso" "peso" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[12]   > comercial.cant_pedidos.unidades
"unidades" "unidades" ? ? "decimal" ? ? ? ? ? ? yes ? no 9 yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-datos dTables  _DB-REQUIRED
PROCEDURE carga-datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pProdTerc AS ROWID NO-UNDO.

DEFINE VAR xQuery AS CHAR NO-UNDO.

FIND FIRST productos_terceros WHERE ROWID(productos_terceros) = pProdTerc
                                      NO-LOCK NO-ERROR.
  IF AVAILABLE productos_terceros THEN DO:
    
      xQuery = "pedidos.id_proveedor = " + STRING(productos_terceros.id_proveedor) + " and " +
               "pedidos.recibido = FALSE".
    
    {set queryWhere xQuery}.
    DYNAMIC-FUNCTION ('openQuery':U).
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION proceso-orden-compra dTables  _DB-REQUIRED
FUNCTION proceso-orden-compra RETURNS CHARACTER
  (INPUT pProductoTerceros AS ROWID /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE BUFFER bbProductosTerceros FOR productos_terceros.
DEFINE BUFFER bbPedidos FOR pedidos.
DEFINE BUFFER bbCantPedidos FOR cant_pedidos.
DEFINE BUFFER bbCanPedTerminados FOR cant_pedidos.
DEFINE VAR rPedidos AS ROWID.
DEFINE VAR rCantPedidos AS ROWID.
DEFINE VAR vTotalCantidad AS INTEGER.
DEFINE VAR vCerrada AS LOGICAL INITIAL TRUE.

rPedidos        = DYNAMIC-FUNCTION('getRowidByIndex':U, INPUT 1).
rCantPedidos    = DYNAMIC-FUNCTION('getRowidByIndex':U, INPUT 2).
  
FIND FIRST bbProductosTerceros WHERE ROWID(bbProductosTerceros) = pProductoTerceros
                               NO-ERROR.
IF AVAILABLE bbProductosTerceros THEN DO:

    FIND FIRST bbPedidos WHERE ROWID(bbPedidos) = rPedidos NO-ERROR.
    IF AVAILABLE bbPedidos THEN DO:

        FIND FIRST bbCantPedidos WHERE ROWID(bbCantPedidos) = rCantPedidos NO-ERROR.
        IF AVAILABLE bbCantPedidos THEN DO:
            IF bbProductosTerceros.id_pedido = 0 THEN DO:
                ASSIGN bbProductosTerceros.id_pedido        = bbPedidos.id_pedido
                       bbProductosTerceros.id_suc_origen    = bbPedidos.id_suc_origen
                       bbProductosTerceros.id_art_pedido    = bbCantPedidos.id_articulo
                       bbProductosTerceros.id_suc_pedido    = bbCantPedidos.id_sucursal
                
    
                vTotalCantidad = bbProductosTerceros.cantidad + bbCantPedidos.cant_recibida.
                ASSIGN bbCantPedidos.cant_recibida = vTotalCantidad.
    
                IF vTotalCantidad >= bbCantPedidos.cantidad THEN DO:
                    /* SE CANCELO ESA PARTE DEL PEDIDO */
                    vCerrada = TRUE.
                    FOR EACH bbCanPedTerminados OF bbPedidos NO-LOCK.
                        IF bbCanPedTerminados.cantidad > bbCanPedTerminados.cant_recibida  THEN vCerrada = FALSE.
                    END.
                    IF vCerrada THEN DO:
                        /* SI NO ENTRA ACA ES PORQUE UNO DE LOS ITEMS DE LA OC NO ESTA CERRADA */
                        ASSIGN bbPedidos.recibido = TRUE.
                        MESSAGE "Se completo la Orden de Compra" VIEW-AS ALERT-BOX.
                    END.
                END.
                RETURN "Se relaciono la Orden de Compra satisfactoriamente".
            END.
            ELSE DO: 
                MESSAGE "El Lote de Terceros ya esta vinculado con la Orden de Compra " 
                         bbProductosTerceros.id_pedido ". No se puede vincular a otra Orden." 
                         VIEW-AS ALERT-BOX.
                RETURN "No se relaciono la Orden de Compra".
            END.
        END.
        ELSE RETURN "Error Cant Pedidos".
    END.
    ELSE RETURN "Error Pedido".
END.
ELSE RETURN "Error Producto Tercero".
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

