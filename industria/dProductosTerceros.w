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
&Scoped-define INTERNAL-TABLES productos_terceros

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio_lote cantidad chofer codigo_lote c_fecha c_hora c_usuario estado fecha~
 fecha_remito id_articulo id_art_pedido id_calidad id_empresa id_envase~
 id_lote id_lugdes id_pedido id_proveedor id_punto_venta id_sucursal~
 id_suc_ingreso id_suc_origen id_suc_pedido id_tipotambor~
 id_tipo_mov_ingreso id_transportista importe kilos kilos2 kilos_tambor~
 nromov nro_comp nro_ingreso nro_orden_compra observaciones pat_acopla~
 pat_chasis tarifa volumen
&Scoped-define ENABLED-FIELDS-IN-productos_terceros anio_lote cantidad ~
chofer codigo_lote c_fecha c_hora c_usuario estado fecha fecha_remito ~
id_articulo id_art_pedido id_calidad id_empresa id_envase id_lote id_lugdes ~
id_pedido id_proveedor id_punto_venta id_sucursal id_suc_ingreso ~
id_suc_origen id_suc_pedido id_tipotambor id_tipo_mov_ingreso ~
id_transportista importe kilos kilos2 kilos_tambor nromov nro_comp ~
nro_ingreso nro_orden_compra observaciones pat_acopla pat_chasis tarifa ~
volumen 
&Scoped-Define DATA-FIELDS  anio_lote cantidad chofer codigo_lote c_fecha c_hora c_usuario estado fecha~
 fecha_remito id_articulo id_art_pedido id_calidad id_empresa id_envase~
 id_lote id_lugdes id_pedido id_proveedor id_punto_venta id_sucursal~
 id_suc_ingreso id_suc_origen id_suc_pedido id_tipotambor~
 id_tipo_mov_ingreso id_transportista importe kilos kilos2 kilos_tambor~
 nromov Producto Calidad Proveedor Sucursal nro_comp nro_ingreso~
 nro_orden_compra observaciones pat_acopla pat_chasis tarifa volumen~
 Tambores
&Scoped-define DATA-FIELDS-IN-productos_terceros anio_lote cantidad chofer ~
codigo_lote c_fecha c_hora c_usuario estado fecha fecha_remito id_articulo ~
id_art_pedido id_calidad id_empresa id_envase id_lote id_lugdes id_pedido ~
id_proveedor id_punto_venta id_sucursal id_suc_ingreso id_suc_origen ~
id_suc_pedido id_tipotambor id_tipo_mov_ingreso id_transportista importe ~
kilos kilos2 kilos_tambor nromov nro_comp nro_ingreso nro_orden_compra ~
observaciones pat_acopla pat_chasis tarifa volumen 
&Scoped-Define MANDATORY-FIELDS  id_calidad id_lote id_sucursal
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dProductosTerceros.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH productos_terceros NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH productos_terceros NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main productos_terceros
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main productos_terceros


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidad dTables  _DB-REQUIRED
FUNCTION getCantidad RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProveedor dTables  _DB-REQUIRED
FUNCTION getProveedor RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      productos_terceros SCROLLING.
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
     _TblList          = "general.productos_terceros"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.productos_terceros.anio_lote
"anio_lote" "anio_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > general.productos_terceros.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[3]   > general.productos_terceros.chofer
"chofer" "chofer" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[4]   > general.productos_terceros.codigo_lote
"codigo_lote" "codigo_lote" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[5]   > general.productos_terceros.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[6]   > general.productos_terceros.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[7]   > general.productos_terceros.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[8]   > general.productos_terceros.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[9]   > general.productos_terceros.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[10]   > general.productos_terceros.fecha_remito
"fecha_remito" "fecha_remito" ? ? "date" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[11]   > general.productos_terceros.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[12]   > general.productos_terceros.id_art_pedido
"id_art_pedido" "id_art_pedido" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[13]   > general.productos_terceros.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 7 yes
     _FldNameList[14]   > general.productos_terceros.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[15]   > general.productos_terceros.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[16]   > general.productos_terceros.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[17]   > general.productos_terceros.id_lugdes
"id_lugdes" "id_lugdes" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[18]   > general.productos_terceros.id_pedido
"id_pedido" "id_pedido" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[19]   > general.productos_terceros.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[20]   > general.productos_terceros.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[21]   > general.productos_terceros.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.2 yes
     _FldNameList[22]   > general.productos_terceros.id_suc_ingreso
"id_suc_ingreso" "id_suc_ingreso" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 yes
     _FldNameList[23]   > general.productos_terceros.id_suc_origen
"id_suc_origen" "id_suc_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[24]   > general.productos_terceros.id_suc_pedido
"id_suc_pedido" "id_suc_pedido" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[25]   > general.productos_terceros.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[26]   > general.productos_terceros.id_tipo_mov_ingreso
"id_tipo_mov_ingreso" "id_tipo_mov_ingreso" ? ? "integer" ? ? ? ? ? ? yes ? no 19.4 yes
     _FldNameList[27]   > general.productos_terceros.id_transportista
"id_transportista" "id_transportista" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[28]   > general.productos_terceros.importe
"importe" "importe" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[29]   > general.productos_terceros.kilos
"kilos" "kilos" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[30]   > general.productos_terceros.kilos2
"kilos2" "kilos2" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[31]   > general.productos_terceros.kilos_tambor
"kilos_tambor" "kilos_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[32]   > general.productos_terceros.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[33]   > "_<CALC>"
"getArticulo()" "Producto" "Producto" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[34]   > "_<CALC>"
"getCalidad()" "Calidad" "Calidad" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[35]   > "_<CALC>"
"getProveedor()" "Proveedor" "Proveedor" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[36]   > "_<CALC>"
"getSucursal()" "Sucursal" "Sucursal" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no
     _FldNameList[37]   > general.productos_terceros.nro_comp
"nro_comp" "nro_comp" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 yes
     _FldNameList[38]   > general.productos_terceros.nro_ingreso
"nro_ingreso" "nro_ingreso" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[39]   > general.productos_terceros.nro_orden_compra
"nro_orden_compra" "nro_orden_compra" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[40]   > general.productos_terceros.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 150 yes
     _FldNameList[41]   > general.productos_terceros.pat_acopla
"pat_acopla" "pat_acopla" ? ? "character" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[42]   > general.productos_terceros.pat_chasis
"pat_chasis" "pat_chasis" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[43]   > general.productos_terceros.tarifa
"tarifa" "tarifa" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[44]   > general.productos_terceros.volumen
"volumen" "volumen" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[45]   > "_<CALC>"
"getCantidad()" "Tambores" "Tambores" ">>,>>9" "Integer" ? ? ? ? ? ? no ? no 9.4 no
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
  DEFINE VARIABLE lTieneRemito AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lTieneReproc AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lExiste      AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE hCont     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE ibgcolor  AS INTEGER    NO-UNDO.  
  DEFINE VARIABLE hTam      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  {get ContainerSource hCont}.
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.


  
  FIND LAST RowObjUpd NO-ERROR.


  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C".
    IF rowObjUpd.id_articulo = 0 THEN
      RETURN "Error en articulo".
  
    IF rowObjUpd.id_articulo <> 54 THEN
      lExiste = DYNAMIC-FUNCTION('getLoteExistente' IN hLib, rowObjUpd.id_sucursal,
                                                             9, 
                                                             rowObjUpd.id_lote,
                                                             rowObjUpd.anio,
                                                             rowObjUpd.id_articulo).
    IF lExiste THEN 
      RETURN "Lote Existente".
  
  
    
    ASSIGN rowObjUpd.id_tipotambor  = 9
           rowObjUpd.nromov         = NEXT-VALUE(nromov)
           rowObjUpd.c_usuario      = USERID("userdb")
           rowObjUpd.c_fecha        = TODAY
           rowObjUpd.c_hora         = STRING(TIME,"HH:MM:SS").
  
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",id_tipotambor,nromov,c_usuario,c_fecha,c_hora".  
  END.

  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U".
    ASSIGN rowObjUpd.c_usuario      = USERID("userdb")
           rowObjUpd.c_fecha        = TODAY
           rowObjUpd.c_hora         = STRING(TIME,"HH:MM:SS").
  END.

  
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D".
    IF rowObjUpd.id_articulo = 54 THEN DO:
      FIND FIRST r_lote_cascara_remito
           WHERE r_lote_cascara_remito.nromov = rowObjUpd.nromov
           NO-ERROR.
      IF AVAILABLE r_lote_cascara_remito THEN DO:
        MESSAGE "Imposible elminar este lote porque posee remitos"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "ADM-ERROR".

      END.
      ELSE DO:
        /* borro lotes_ubicacion */
        FOR EACH lotes_ubicacion 
            WHERE lotes_ubicacion.nromov = rowObjUpd.nromov.
          DELETE lotes_ubicacion.
        END.
        /* borro asoc producciones */
        FOR EACH r_produccion_cascara_lote
            WHERE r_produccion_cascara_lote.nromov_lote = rowObjUpd.nromov.
          /* borro producciones cascara */
          FOR EACH produccion_cascara
              WHERE produccion_cascara.id_produccion = r_produccion_cascara_lote.id_produccion.
            DELETE produccion_cascara.
          END.
          DELETE r_produccion_cascara_lote.
        END.
        /* borro lotes_cascara */
        FOR EACH lotes_cascara 
            WHERE lotes_cascara.nromov = rowObjUpd.nromov.
          DELETE lotes_cascara.
        END.
      END.
    END.
    ELSE DO:
      RUN deleteDrumsFromBatch IN hLib (rowObjUpd.id_empresa,
                                      rowObjUpd.id_sucursal,
                                      rowObjUpd.id_tipotambor,
                                      rowObjUpd.nromov,
                                      TRUE).
    END.

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
         rowObject.Calidad = (getCalidad())
         rowObject.Producto = (getArticulo())
         rowObject.Proveedor = (getProveedor())
         rowObject.Sucursal = (getSucursal())
         rowObject.Tambores = (getCantidad())
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

  FOR EACH rowObjUpd
      WHERE rowObjUpd.rowMod = "A".
    /* para ingresos de terceros de lotes de cascara */
    IF rowObjUpd.id_articulo = 54 THEN DO:
      /* creo lote cascara */
      CREATE lotes_cascara.
      ASSIGN  lotes_cascara.id_empresa    = rowObjUpd.id_empresa
              lotes_cascara.id_sucursal   = rowObjUpd.id_sucursal
              lotes_cascara.id_tipotambor = 11
              lotes_cascara.nromov        = rowObjUpd.nromov
              lotes_cascara.id_lote       = rowObjUpd.id_lote
              lotes_cascara.anio          = rowObjUpd.anio
              lotes_cascara.codigo_lote   = rowObjUpd.codigo_lote
              lotes_cascara.fecha         = rowObjUpd.fecha
              lotes_cascara.id_articulo   = rowObjUpd.id_articulo
              lotes_cascara.id_calidad    = rowObjUpd.id_calidad
              lotes_cascara.id_envase     = 626
              lotes_cascara.observaciones = "Ingreso de Terceros"
              lotes_cascara.c_usuario     = USERID('userdb')
              lotes_cascara.c_fecha       = TODAY
              lotes_cascara.c_hora        = STRING(TIME, "HH:MM")
              .
      /* creo lots_ubicacion */
      CREATE lotes_ubicacion.
      BUFFER-COPY lotes_cascara TO lotes_ubicacion.
      ASSIGN lotes_ubicacion.id_sucursal_ubicacion = 95
             lotes_ubicacion.lote                  = STRING(rowObjUpd.id_lote, "999999") + "/" + STRING(rowObjUpd.anio,"9999")
             lotes_ubicacion.c_usuario             = USERID('userdb')
             lotes_ubicacion.c_fecha               = TODAY
             lotes_ubicacion.c_hora                = STRING(TIME, "HH:MM")
             .

      
    END.
  END.

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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST productos_terminados
      WHERE productos_terminados.id_articulo = rowObject.id_articulo
      NO-LOCK.
    cRet = productos_terminados.descripcion.
  END.

  RETURN cRet.

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
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST calidades
      WHERE calidades.id_calidad = rowObject.id_calidad
      NO-LOCK.
    cRet = calidades.descripcion.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidad dTables  _DB-REQUIRED
FUNCTION getCantidad RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.nromov = rowObject.nromov
      NO-LOCK.
    i = i + 1.
  END.

  IF rowObject.id_articulo = 54 THEN DO:
    FOR FIRST lotes_ubicacion 
        WHERE lotes_ubicacion.nromov = rowObject.nromov
        NO-LOCK.
      i = lotes_ubicacion.cantidad.
    END.
  END.

  RETURN i.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProveedor dTables  _DB-REQUIRED
FUNCTION getProveedor RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST proveedores
      WHERE proveedores.id_proveedor = rowObject.id_proveedor
      NO-LOCK.
    cRet = proveedores.razon_social.
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursal dTables  _DB-REQUIRED
FUNCTION getSucursal RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST sucursales
      WHERE sucursales.id_sucursal = rowObject.id_sucursal
      NO-LOCK.
    cRet = sucursales.nombre.
  END.
  

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

