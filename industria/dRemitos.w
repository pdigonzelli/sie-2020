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
&Scoped-define INTERNAL-TABLES remitos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio_of anio_produccion bultos chofer c_fecha c_hora c_usuario estado fecha~
 fecha_descarga fecha_llegada fecha_proceso fecha_salida hora_descarga~
 hora_llegada hora_salida id_ciaseg id_cliente id_destino id_galpon~
 id_lugdes id_operacion_compra id_orden_entrega id_proveedor id_punto_venta~
 id_sucursal id_tipocomp_compra id_tipo_movsto id_vapor impresion item_oe~
 mercado nro nromov_compra nro_comprobante nro_contenedor nro_iascav~
 nro_orden_fab nro_ord_carga nro_per_embarque nro_precinto observaciones~
 observ_ordenentrega orden_embarque orden_fabricacion Pat_acopla pat_chasis~
 peso Peso_bruto Peso_neto Tara temp_llegada temp_salida tipo_remito~
 Valor_declarado id_operacion
&Scoped-define ENABLED-FIELDS-IN-remitos anio_of anio_produccion bultos ~
chofer c_fecha c_hora c_usuario estado fecha fecha_descarga fecha_llegada ~
fecha_proceso fecha_salida hora_descarga hora_llegada hora_salida id_ciaseg ~
id_cliente id_destino id_galpon id_lugdes id_operacion_compra ~
id_orden_entrega id_proveedor id_punto_venta id_sucursal id_tipocomp_compra ~
id_tipo_movsto id_vapor impresion item_oe mercado nro nromov_compra ~
nro_comprobante nro_contenedor nro_iascav nro_orden_fab nro_ord_carga ~
nro_per_embarque nro_precinto observaciones observ_ordenentrega ~
orden_embarque orden_fabricacion Pat_acopla pat_chasis peso Peso_bruto ~
Peso_neto Tara temp_llegada temp_salida tipo_remito Valor_declarado ~
id_operacion 
&Scoped-Define DATA-FIELDS  anio_of Destino anio_produccion LugarDescarga bultos chofer c_fecha c_hora~
 c_usuario estado fecha fecha_descarga fecha_llegada fecha_proceso~
 fecha_salida hora_descarga hora_llegada hora_salida id_ciaseg id_cliente~
 id_destino id_galpon id_lugdes id_operacion_compra id_orden_entrega~
 id_proveedor id_punto_venta id_sucursal id_tipocomp_compra id_tipo_movsto~
 id_vapor impresion item_oe mercado nro nromov_compra nro_comprobante~
 nro_contenedor nro_iascav nro_orden_fab nro_ord_carga nro_per_embarque~
 nro_precinto observaciones observ_ordenentrega orden_embarque~
 orden_fabricacion Pat_acopla pat_chasis peso Peso_bruto Peso_neto Tara~
 temp_llegada temp_salida tipo_remito Valor_declarado id_operacion~
 FechaArribo
&Scoped-define DATA-FIELDS-IN-remitos anio_of anio_produccion bultos chofer ~
c_fecha c_hora c_usuario estado fecha fecha_descarga fecha_llegada ~
fecha_proceso fecha_salida hora_descarga hora_llegada hora_salida id_ciaseg ~
id_cliente id_destino id_galpon id_lugdes id_operacion_compra ~
id_orden_entrega id_proveedor id_punto_venta id_sucursal id_tipocomp_compra ~
id_tipo_movsto id_vapor impresion item_oe mercado nro nromov_compra ~
nro_comprobante nro_contenedor nro_iascav nro_orden_fab nro_ord_carga ~
nro_per_embarque nro_precinto observaciones observ_ordenentrega ~
orden_embarque orden_fabricacion Pat_acopla pat_chasis peso Peso_bruto ~
Peso_neto Tara temp_llegada temp_salida tipo_remito Valor_declarado ~
id_operacion 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dRemitos.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH remitos ~
      WHERE remitos.fecha >= 01/01/2005 ~
 AND remitos.id_operacion = 311 NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH remitos ~
      WHERE remitos.fecha >= 01/01/2005 ~
 AND remitos.id_operacion = 311 NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main remitos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main remitos


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArrayRemitosSemana dTables  _DB-REQUIRED
FUNCTION getArrayRemitosSemana RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getConfirmacionArribo dTables  _DB-REQUIRED
FUNCTION getConfirmacionArribo RETURNS DATE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLugDes dTables  _DB-REQUIRED
FUNCTION getLugDes RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      remitos SCROLLING.
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
     _TblList          = "general.remitos"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "general.remitos.fecha >= 01/01/2005
 AND general.remitos.id_operacion = 311"
     _FldNameList[1]   > general.remitos.anio_of
"anio_of" "anio_of" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[2]   > "_<CALC>"
"getDestino()" "Destino" "Destino" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[3]   > general.remitos.anio_produccion
"anio_produccion" "anio_produccion" ? ? "integer" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[4]   > "_<CALC>"
"getLugDes()" "LugarDescarga" "LugarDescarga" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no
     _FldNameList[5]   > general.remitos.bultos
"bultos" "bultos" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[6]   > general.remitos.chofer
"chofer" "chofer" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[7]   > general.remitos.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[8]   > general.remitos.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[9]   > general.remitos.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[10]   > general.remitos.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[11]   > general.remitos.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[12]   > general.remitos.fecha_descarga
"fecha_descarga" "fecha_descarga" ? ? "date" ? ? ? ? ? ? yes ? no 9.8 yes
     _FldNameList[13]   > general.remitos.fecha_llegada
"fecha_llegada" "fecha_llegada" ? ? "date" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[14]   > general.remitos.fecha_proceso
"fecha_proceso" "fecha_proceso" ? ? "date" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[15]   > general.remitos.fecha_salida
"fecha_salida" "fecha_salida" ? ? "date" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[16]   > general.remitos.hora_descarga
"hora_descarga" "hora_descarga" ? ? "character" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[17]   > general.remitos.hora_llegada
"hora_llegada" "hora_llegada" ? ? "character" ? ? ? ? ? ? yes ? no 12.8 yes
     _FldNameList[18]   > general.remitos.hora_salida
"hora_salida" "hora_salida" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[19]   > general.remitos.id_ciaseg
"id_ciaseg" "id_ciaseg" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[20]   > general.remitos.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[21]   > general.remitos.id_destino
"id_destino" "id_destino" ? ? "integer" ? ? ? ? ? ? yes ? no 17 yes
     _FldNameList[22]   > general.remitos.id_galpon
"id_galpon" "id_galpon" ? ? "integer" ? ? ? ? ? ? yes ? no 16.6 yes
     _FldNameList[23]   > general.remitos.id_lugdes
"id_lugdes" "id_lugdes" ? ? "integer" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[24]   > general.remitos.id_operacion_compra
"id_operacion_compra" "id_operacion_compra" ? ? "integer" ? ? ? ? ? ? yes ? no 20.4 yes
     _FldNameList[25]   > general.remitos.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[26]   > general.remitos.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? yes ? no 14.2 yes
     _FldNameList[27]   > general.remitos.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[28]   > general.remitos.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[29]   > general.remitos.id_tipocomp_compra
"id_tipocomp_compra" "id_tipocomp_compra" ? ? "integer" ? ? ? ? ? ? yes ? no 19.6 yes
     _FldNameList[30]   > general.remitos.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[31]   > general.remitos.id_vapor
"id_vapor" "id_vapor" ? ? "integer" ? ? ? ? ? ? yes ? no 15.6 yes
     _FldNameList[32]   > general.remitos.impresion
"impresion" "impresion" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes
     _FldNameList[33]   > general.remitos.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[34]   > general.remitos.mercado
"mercado" "mercado" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[35]   > general.remitos.nro
"nro" "nro" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[36]   > general.remitos.nromov_compra
"nromov_compra" "nromov_compra" ? ? "integer" ? ? ? ? ? ? yes ? no 15.2 yes
     _FldNameList[37]   > general.remitos.nro_comprobante
"nro_comprobante" "nro_comprobante" ? ? "character" ? ? ? ? ? ? yes ? no 13 yes
     _FldNameList[38]   > general.remitos.nro_contenedor
"nro_contenedor" "nro_contenedor" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[39]   > general.remitos.nro_iascav
"nro_iascav" "nro_iascav" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes
     _FldNameList[40]   > general.remitos.nro_orden_fab
"nro_orden_fab" "nro_orden_fab" ? ? "character" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[41]   > general.remitos.nro_ord_carga
"nro_ord_carga" "nro_ord_carga" ? ? "integer" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[42]   > general.remitos.nro_per_embarque
"nro_per_embarque" "nro_per_embarque" ? ? "character" ? ? ? ? ? ? yes ? no 18.6 yes
     _FldNameList[43]   > general.remitos.nro_precinto
"nro_precinto" "nro_precinto" ? ? "character" ? ? ? ? ? ? yes ? no 10.4 yes
     _FldNameList[44]   > general.remitos.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes
     _FldNameList[45]   > general.remitos.observ_ordenentrega
"observ_ordenentrega" "observ_ordenentrega" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes
     _FldNameList[46]   > general.remitos.orden_embarque
"orden_embarque" "orden_embarque" ? ? "integer" ? ? ? ? ? ? yes ? no 11.2 yes
     _FldNameList[47]   > general.remitos.orden_fabricacion
"orden_fabricacion" "orden_fabricacion" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[48]   > general.remitos.Pat_acopla
"Pat_acopla" "Pat_acopla" ? ? "character" ? ? ? ? ? ? yes ? no 16.8 yes
     _FldNameList[49]   > general.remitos.pat_chasis
"pat_chasis" "pat_chasis" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes
     _FldNameList[50]   > general.remitos.peso
"peso" "peso" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[51]   > general.remitos.Peso_bruto
"Peso_bruto" "Peso_bruto" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[52]   > general.remitos.Peso_neto
"Peso_neto" "Peso_neto" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[53]   > general.remitos.Tara
"Tara" "Tara" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[54]   > general.remitos.temp_llegada
"temp_llegada" "temp_llegada" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[55]   > general.remitos.temp_salida
"temp_salida" "temp_salida" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[56]   > general.remitos.tipo_remito
"tipo_remito" "tipo_remito" ? ? "logical" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[57]   > general.remitos.Valor_declarado
"Valor_declarado" "Valor_declarado" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.8 yes
     _FldNameList[58]   > general.remitos.id_operacion
"id_operacion" "id_operacion" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[59]   > "_<CALC>"
"getConfirmacionArribo()" "FechaArribo" "FechaArribo" "99/99/9999" "Date" ? ? ? ? ? ? no ? no 11.6 no
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
  
  /*add*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "A" OR rowObjUpd.rowMod = "C". 
  
    ASSIGN rowObjUpd.tipo_remito  = IF rowObjUpd.id_tipo_movsto = 122 THEN FALSE ELSE TRUE
           rowObjUpd.id_operacion = 311
           rowObjUpd.c_usuario    = USERID("userdb")
           rowObjUpd.c_fecha      = TODAY
           rowObjUpd.c_hora       = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",tipo_remito,id_operacion,c_usuario,c_fecha,c_hora,peso_neto,peso_bruto,bultos".    
  END.
  
  /*update*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "U". 
    ASSIGN rowObjUpd.c_usuario = USERID("userdb")
           rowObjUpd.c_fecha   = TODAY
           rowObjUpd.c_hora    = STRING(TIME,"HH:MM:SS").
    rowObjUpd.changedFields = rowObjUpd.changedFields + ",c_usuario,c_fecha,c_hora".        
  END.
  
  /*delete*/
  FOR EACH rowObjUpd WHERE rowObjUpd.rowMod = "D". 

    /*eliminar releacion de r_tambor_remito*/
    FOR EACH r_tambor_remito
        WHERE r_tambor_remito.id_sucursal_remito = rowObjUpd.id_sucursal
          AND r_tambor_remito.id_tipo_movsto     = rowObjUpd.id_tipo_movsto
          AND r_tambor_remito.nro_remito         = rowObjUpd.nro.

      /*borra clave de remito en tambor*/
      FOR FIRST tambores_industria
          WHERE tambores_industria.id_empresa    = r_tambor_remito.id_empresa
            AND tambores_industria.id_sucursal   = r_tambor_remito.id_sucursal
            AND tambores_industria.id_tipotambor = r_tambor_remito.id_tipotambor
            AND tambores_industria.nromov        = r_tambor_remito.nromov
            AND tambores_industria.id_tambor     = r_tambor_remito.id_tambor.
        
        ASSIGN tambores_industria.id_sucursal_remito = 0
               tambores_industria.id_tipo_movsto     = 0
               tambores_industria.nro_remito         = 0
               tambores_industria.ITEM_factura       = 0
               .
      END.

      DELETE r_tambor_remito.
    END.

    /*eliminar items_factura*/
    FOR EACH items_factura
        WHERE items_factura.id_sucursal    = rowObjUpd.id_sucursal
          AND items_factura.id_tipo_movsto = rowObjUpd.id_tipo_movsto
          AND items_factura.nro            = rowObjUpd.nro.
      DELETE items_factura.
    END.

  
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createItemRemito dTables  _DB-REQUIRED
PROCEDURE createItemRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStream AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRowIds AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibRto AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCont   AS HANDLE     NO-UNDO.

  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBru AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iBul AS INTEGER    NO-UNDO.


  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibRto = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  hCont   = DYNAMIC-FUNCTION('getContainerSource').
  DELETE OBJECT hLibCom.

  /* hago esto porque esta oe tiene asociados 5760 tbs y da error */
  IF rowObjUpd.id_orden_entrega = 16627 OR
     rowObjUpd.id_orden_entrega = 16628 THEN RETURN.

  /*elimino partes antes de agregar, en caso de que modifiquen la oe en la cabecera del remito*/
  FOR EACH items_factura
      WHERE items_factura.id_sucursal     = rowObjUpd.id_sucursal
        AND items_factura.id_tipo_movsto  = rowObjUpd.id_tipo_movsto
        AND items_factura.nro             = rowObjUpd.nro.

    RUN liberarTambores IN hLibRto (rowObjUpd.id_sucursal,
                                    rowObjUpd.id_tipo_movsto,
                                    rowObjUpd.nro,
                                    items_factura.ITEM).
    DELETE items_factura.
  END.


   cRowIds = "".
   cStream = "".
   FOR EACH tambores_industria   
      WHERE tambores_industria.id_orden_entrega      = rowObjUpd.id_orden_entrega
        AND tambores_industria.ITEM_oe               = rowObjUpd.ITEM_oe
        AND tambores_industria.id_locacion_ubicacion = 4
        AND tambores_industria.id_sucursal_ubicacion = rowObjUpd.id_sucursal
      NO-LOCK.

    cRowIds = cRowIds + STRING(ROWID(tambores_industria)) + ",".
    iBul    = iBul + 1.
    dKil    = dKil + tambores_industria.kilos_tambor.
    dBru    = dBru + (tambores_industria.kilos_tambor + tambores_industria.tara).

  END.
  cRowIds = SUBSTRING(cRowIds, 1, LENGTH(cRowIds) - 1).
  cStream = DYNAMIC-FUNCTION('getRangos' IN hLibTam, cRowIds).

  
  RUN addItemRemito IN hLibRto (rowObjUpd.id_sucursal, 
                                rowObjUpd.id_tipo_movsto, 
                                rowObjUpd.nro, 
                                cStream).

  
  
  /*recalculo  pesos de la cabecera del remito*/
  /*
  ASSIGN rowObjUpd.peso_neto  = ROUND(dKil, 2)
         rowObjUpd.peso_bruto = ROUND(dBru, 2)
         rowObjUpd.bultos     = iBul.

  rowObjUpd.changedFields = rowObjUpd.changedFields + ",peso_neto,peso_bruto,bultos".


  */

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
         rowObject.Destino = (getDestino())
         rowObject.FechaArribo = (getConfirmacionArribo())
         rowObject.LugarDescarga = (getLugDes())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fecha_salidaValidate dTables  _DB-REQUIRED
PROCEDURE fecha_salidaValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  IF DATE(pcArgs) = ? THEN DO:
    MESSAGE "Fecha de Salida Invalida"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "ADM-ERROR".
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hora_salidaValidate dTables  _DB-REQUIRED
PROCEDURE hora_salidaValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  IF pcArgs = "" THEN DO:
    MESSAGE "Hora de Salida Invalida"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "ADM-ERROR".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_destinoValidate dTables  _DB-REQUIRED
PROCEDURE id_destinoValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  IF INTEGER(pcArgs) = 0 THEN DO:
    MESSAGE "Informacion de Destino Invalida"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "ADM-ERROR".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_lugdesValidate dTables  _DB-REQUIRED
PROCEDURE id_lugdesValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  IF INTEGER(pcArgs) = 0 THEN DO:
    MESSAGE "Informacion de Lugar de Descarga Invalida"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "ADM-ERROR".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_sucursalValidate dTables  _DB-REQUIRED
PROCEDURE id_sucursalValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.

  
  IF INTEGER(pcValor) <> 95 AND INTEGER(pcValor) <> 96 AND INTEGER(pcValor) <> 92 AND INTEGER(pcValor) <> 159 AND INTEGER(pcValor) <> 462 THEN
    RETURN "Sucursal Invalida".
    
 
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_tipo_movstoValidate dTables  _DB-REQUIRED
PROCEDURE id_tipo_movstoValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValor AS CHARACTER  NO-UNDO.
 
  CASE pcValor:
    WHEN "121" THEN .
    WHEN "122" THEN .
    WHEN "123" THEN .
    OTHERWISE
      RETURN "Error en tipo de remito (manual/automatico).".
  END CASE.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_transporteValidate dTables  _DB-REQUIRED
PROCEDURE id_transporteValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  IF INTEGER(pcArgs) = 0 THEN DO:
    MESSAGE "Informacion de Transporte Invalida"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "ADM-ERROR".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_vaporValidate dTables  _DB-REQUIRED
PROCEDURE id_vaporValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  IF INTEGER(pcArgs) = 0 THEN DO:
    MESSAGE "Informacion de Vapor Invalida"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "ADM-ERROR".
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE item_oeValidate dTables  _DB-REQUIRED
PROCEDURE item_oeValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.
/* 
  IF rowObject.mercado:SCREEN-VALUE  = "1" THEN DO:
    IF pcArgs = "0" THEN DO:
      MESSAGE "Ingrese una parte de OE Valida"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "ADM-ERROR".
    END.
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
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libRemitos.p').
  DELETE OBJECT hLibCom.

  FIND LAST RowObjUpd NO-ERROR.

  FOR EACH rowObjUpd 
      WHERE rowObjUpd.rowMod = "A" 
         OR rowObjUpd.rowMod = "C" 
         /*OR rowObjUpd.RowMod = "U"*/. 

    ASSIGN iSuc = rowObjUpd.id_sucursal
           iTip = rowObjUpd.id_tipo_movsto
           iNro = rowObjUpd.nro
           .
    IF rowObjUpd.id_orden_entrega <> 0 AND rowObjUpd.ITEM_oe <> 0 THEN
      RUN createItemRemito.


  END.

  RUN recalcKilosRemito IN hLib (iSuc, iTip, iNro).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp_salidaValidate dTables  _DB-REQUIRED
PROCEDURE temp_salidaValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  IF INTEGER(pcArgs) = 0 THEN DO:
    MESSAGE "Temperatura de Salida Invalida"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "ADM-ERROR".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArrayRemitosSemana dTables  _DB-REQUIRED
FUNCTION getArrayRemitosSemana RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iSem AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.

  DEFINE BUFFER ro FOR remitos.
   
  FOR EACH ro NO-LOCK WHERE YEAR(ro.fecha) >= 2004
                        AND ro.estado = TRUE
                        AND ro.mercado = 1,
      EACH items_factura OF ro, 
      EACH r_lote_cascara_remito WHERE r_lote_cascara_remito.nro_remito = items_factura.nro
                                   AND r_lote_cascara_remito.id_sucursal_remito = items_factura.id_sucursal
                                   AND r_lote_cascara_remito.ITEM_factura = items_factura.ITEM. 
  
    RUN semana_anio.p (INPUT ro.fecha,
                       OUTPUT iSem, 
                       OUTPUT iAno).
    
    cRet = cRet + ro.nro_comprobante + "," +
                  STRING(iSem) + "," +
                  STRING(iAno) + "," + 
                  STRING(items_factura.cantidad) +  
                  CHR(10).

  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getConfirmacionArribo dTables  _DB-REQUIRED
FUNCTION getConfirmacionArribo RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DATE       NO-UNDO.

  FOR FIRST confirmacion_remito
      WHERE confirmacion_remito.id_sucursal     = rowObject.id_sucursal
        AND confirmacion_remito.id_tipo_movsto  = rowObject.id_tipo_movsto
        AND confirmacion_remito.nro             = rowObject.nro
      NO-LOCK.
    dRet = confirmacion_remito.fecha.
  END.

  RETURN dRet.

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
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST destinos WHERE destinos.id_destino = rowObject.id_destino
                      NO-LOCK NO-ERROR.
  IF AVAILABLE destinos THEN
    RETURN destinos.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLugDes dTables  _DB-REQUIRED
FUNCTION getLugDes RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND CURRENT rowObject NO-ERROR.
  FIND FIRST lugar_descarga WHERE lugar_descarga.id_lugdes = rowObject.id_lugdes
                    NO-LOCK NO-ERROR.
  IF AVAILABLE lugar_descarga THEN
    RETURN lugar_descarga.descripcion.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}
