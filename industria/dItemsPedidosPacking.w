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
&Scoped-define INTERNAL-TABLES items_pedidos_packing r_pallets_envases

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  cant_pallets calibre contramarca en_proceso id_articulo id_calidad~
 id_caract id_categoria id_envase id_marca id_orden id_empresa~
 id_punto_emisor id_tipo_pallet id_tipo_esquinero id_vapor id_variedad item~
 semana anio pallets
&Scoped-define DATA-FIELDS-IN-items_pedidos_packing cant_pallets calibre ~
contramarca en_proceso id_articulo id_calidad id_caract id_categoria ~
id_envase id_marca id_orden id_empresa id_punto_emisor id_tipo_pallet ~
id_tipo_esquinero id_vapor id_variedad item semana anio 
&Scoped-define DATA-FIELDS-IN-r_pallets_envases pallets 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dItemsPedidosPacking.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_pedidos_packing NO-LOCK, ~
      EACH r_pallets_envases OF items_pedidos_packing OUTER-JOIN NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_pedidos_packing NO-LOCK, ~
      EACH r_pallets_envases OF items_pedidos_packing OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_pedidos_packing ~
r_pallets_envases
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_pedidos_packing
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main r_pallets_envases


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCantidadPallets dTables  _DB-REQUIRED
FUNCTION setCantidadPallets RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEstadoIPP dTables  _DB-REQUIRED
FUNCTION setEstadoIPP RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_pedidos_packing, 
      r_pallets_envases SCROLLING.
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
     _TblList          = "general.items_pedidos_packing,general.r_pallets_envases OF general.items_pedidos_packing"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _FldNameList[1]   > general.items_pedidos_packing.cant_pallets
"cant_pallets" "cant_pallets" "Pallets" ">>>9" "integer" ? ? ? ? ? ? no ? no 6.2 yes
     _FldNameList[2]   > general.items_pedidos_packing.calibre
"calibre" "calibre" ? ? "character" ? ? ? ? ? ? no ? no 5.2 yes
     _FldNameList[3]   > general.items_pedidos_packing.contramarca
"contramarca" "contramarca" ? ? "character" ? ? ? ? ? ? no ? no 4.8 yes
     _FldNameList[4]   > general.items_pedidos_packing.en_proceso
"en_proceso" "en_proceso" ? ? "logical" ? ? ? ? ? ? no ? no 11 yes
     _FldNameList[5]   > general.items_pedidos_packing.id_articulo
"id_articulo" "id_articulo" "Articulo" ">>>>9" "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[6]   > general.items_pedidos_packing.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[7]   > general.items_pedidos_packing.id_caract
"id_caract" "id_caract" "Caract." ? "integer" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[8]   > general.items_pedidos_packing.id_categoria
"id_categoria" "id_categoria" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[9]   > general.items_pedidos_packing.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[10]   > general.items_pedidos_packing.id_marca
"id_marca" "id_marca" ? ? "integer" ? ? ? ? ? ? no ? no 6 yes
     _FldNameList[11]   > general.items_pedidos_packing.id_orden
"id_orden" "id_orden" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[12]   > general.items_pedidos_packing.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? no ? no 6.8 yes
     _FldNameList[13]   > general.items_pedidos_packing.id_punto_emisor
"id_punto_emisor" "id_punto_emisor" ? ? "integer" ? ? ? ? ? ? no ? no 4 yes
     _FldNameList[14]   > general.items_pedidos_packing.id_tipo_pallet
"id_tipo_pallet" "id_tipo_pallet" ? ? "integer" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[15]   > general.items_pedidos_packing.id_tipo_esquinero
"id_tipo_esquinero" "id_tipo_esquinero" ? ? "integer" ? ? ? ? ? ? no ? no 14.2 yes
     _FldNameList[16]   > general.items_pedidos_packing.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[17]   > general.items_pedidos_packing.id_variedad
"id_variedad" "id_variedad" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[18]   > general.items_pedidos_packing.item
"item" "item" ? ? "integer" ? ? ? ? ? ? no ? no 4 yes
     _FldNameList[19]   > general.items_pedidos_packing.semana
"semana" "semana" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[20]   > general.items_pedidos_packing.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? no ? no 5.4 yes
     _FldNameList[21]   > general.r_pallets_envases.pallets
"pallets" "pallets" "Cajas" ? "integer" ? ? ? ? ? ? no ? no 6.2 no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCantidadPallets dTables  _DB-REQUIRED
FUNCTION setCantidadPallets RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE BUFFER bbItemsPedidos FOR items_pedidos_packing.
    DEFINE VAR vPallets AS INTEGER INITIAL 0.

    FIND FIRST bbItemsPedidos WHERE bbItemsPedidos.id_empresa       = RowObject.id_empresa
                                AND bbItemsPedidos.id_punto_emisor  = RowObject.id_punto_emisor
                                AND bbItemsPedidos.id_orden         = RowObject.id_orden
                                AND bbItemsPedidos.item             = RowObject.ITEM
                                NO-LOCK NO-ERROR.
    IF AVAILABLE bbItemsPedidos THEN DO:
        FOR EACH pallets OF bbItemsPedidos.
            vPallets = vPallets + 1.
        END.
    END.

RETURN vPallets.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEstadoIPP dTables  _DB-REQUIRED
FUNCTION setEstadoIPP RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VAR vPalletsOE AS INTEGER NO-UNDO.
    DEFINE VAR vPalletsIPP AS INTEGER NO-UNDO.
    DEFINE BUFFER bbItemOE FOR items_orden_entrega.

    /*  FOR EACH rowObject. */
    vPalletsIPP = vPalletsIPP + RowObject.cant_pallets.
    FOR EACH bbItemOE WHERE bbItemOE.id_empresa_ipp       = RowObject.id_empresa
                        AND bbItemOE.id_orden_ipp         = RowObject.id_orden
                        AND bbItemOE.id_punto_emisor_ipp  = RowObject.id_punto_emisor
                        AND bbItemOE.item_ipp             = RowObject.ITEM
                        NO-LOCK.
        vPalletsOE = vPalletsOE + bbItemOE.cantidad_pallets.
    END.

    
    IF vPalletsOE = 0 THEN DO: /* EL PEDIDO NO TIENE NINGUN "DESPACHO" */
        RETURN 15. /* BLANCO */
    END.
    ELSE DO:
        IF vPalletsIPP = vPalletsOE THEN DO: /* EL PEDIDO ESTA COMPLETAMENTE "DESPACHADO" */
            RETURN 10. /* VERDE */
        END.
        ELSE DO:
            IF vPalletsIPP > vPalletsOE THEN DO: /* EL PEDIDO ESTA PARCIALMENTE "DESPACHADO" */
                RETURN 14. /* AMARILLO */
            END.
            ELSE DO: /* EL PEDIDO ESTA OVER "DESPACHADO". SE DESPACHO MAS DE LO QUE EL PEDIDO DECIA */
                RETURN 12. /* ROJO */
            END.
        END.
    END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

