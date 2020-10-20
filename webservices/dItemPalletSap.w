&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          produccion       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
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

&glob DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES items_pallets

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  bultos calibre camara certificado cert_china codigo_trazabilidad cod_prod~
 cod_trazabilidad contramarca con_testigo c_fecha c_hora c_usuario~
 fecha_ingreso fecha_operativa id_articulo id_calidad id_caract id_categoria~
 id_color id_empresa id_envase id_finca_senasa id_lote id_lote_senasa~
 id_marca id_orden id_origen id_packing id_pallet id_proveedor~
 id_punto_emisor id_sucursal_remito id_suc_trabajo id_tipo_movsto~
 id_turno_packing id_variedad item item_factura item_pallet item_remito nro~
 nro_certificado nro_partida_general renspa tipo_fruta tipo_proceso~
 ubicacion unidad_productora zona_up
&Scoped-define ENABLED-FIELDS-IN-items_pallets bultos calibre camara ~
certificado cert_china codigo_trazabilidad cod_prod cod_trazabilidad ~
contramarca con_testigo c_fecha c_hora c_usuario fecha_ingreso ~
fecha_operativa id_articulo id_calidad id_caract id_categoria id_color ~
id_empresa id_envase id_finca_senasa id_lote id_lote_senasa id_marca ~
id_orden id_origen id_packing id_pallet id_proveedor id_punto_emisor ~
id_sucursal_remito id_suc_trabajo id_tipo_movsto id_turno_packing ~
id_variedad item item_factura item_pallet item_remito nro nro_certificado ~
nro_partida_general renspa tipo_fruta tipo_proceso ubicacion ~
unidad_productora zona_up 
&Scoped-Define DATA-FIELDS  bultos calibre camara certificado cert_china codigo_trazabilidad cod_prod~
 cod_trazabilidad contramarca con_testigo c_fecha c_hora c_usuario~
 fecha_ingreso fecha_operativa id_articulo id_calidad id_caract id_categoria~
 id_color id_empresa id_envase id_finca_senasa id_lote id_lote_senasa~
 id_marca id_orden id_origen id_packing id_pallet id_proveedor~
 id_punto_emisor id_sucursal_remito id_suc_trabajo id_tipo_movsto~
 id_turno_packing id_variedad item item_factura item_pallet item_remito nro~
 nro_certificado nro_partida_general renspa tipo_fruta tipo_proceso~
 ubicacion unidad_productora zona_up
&Scoped-define DATA-FIELDS-IN-items_pallets bultos calibre camara ~
certificado cert_china codigo_trazabilidad cod_prod cod_trazabilidad ~
contramarca con_testigo c_fecha c_hora c_usuario fecha_ingreso ~
fecha_operativa id_articulo id_calidad id_caract id_categoria id_color ~
id_empresa id_envase id_finca_senasa id_lote id_lote_senasa id_marca ~
id_orden id_origen id_packing id_pallet id_proveedor id_punto_emisor ~
id_sucursal_remito id_suc_trabajo id_tipo_movsto id_turno_packing ~
id_variedad item item_factura item_pallet item_remito nro nro_certificado ~
nro_partida_general renspa tipo_fruta tipo_proceso ubicacion ~
unidad_productora zona_up 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS ".\dItemPalletSap.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_pallets NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_pallets NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_pallets
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_pallets


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_pallets SCROLLING.
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
     _TblList          = "produccion.items_pallets"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > produccion.items_pallets.bultos
"bultos" "bultos" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes ?
     _FldNameList[2]   > produccion.items_pallets.calibre
"calibre" "calibre" ? ? "character" ? ? ? ? ? ? yes ? no 6.4 yes ?
     _FldNameList[3]   > produccion.items_pallets.camara
"camara" "camara" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ?
     _FldNameList[4]   > produccion.items_pallets.certificado
"certificado" "certificado" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[5]   > produccion.items_pallets.cert_china
"cert_china" "cert_china" ? ? "character" ? ? ? ? ? ? yes ? no 9.8 yes ?
     _FldNameList[6]   > produccion.items_pallets.codigo_trazabilidad
"codigo_trazabilidad" "codigo_trazabilidad" ? ? "character" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[7]   > produccion.items_pallets.cod_prod
"cod_prod" "cod_prod" ? ? "character" ? ? ? ? ? ? yes ? no 9.4 yes ?
     _FldNameList[8]   > produccion.items_pallets.cod_trazabilidad
"cod_trazabilidad" "cod_trazabilidad" ? ? "character" ? ? ? ? ? ? yes ? no 15.8 yes ?
     _FldNameList[9]   > produccion.items_pallets.contramarca
"contramarca" "contramarca" ? ? "character" ? ? ? ? ? ? yes ? no 4.8 yes ?
     _FldNameList[10]   > produccion.items_pallets.con_testigo
"con_testigo" "con_testigo" ? ? "logical" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[11]   > produccion.items_pallets.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[12]   > produccion.items_pallets.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[13]   > produccion.items_pallets.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[14]   > produccion.items_pallets.fecha_ingreso
"fecha_ingreso" "fecha_ingreso" ? ? "date" ? ? ? ? ? ? yes ? no 15.8 yes ?
     _FldNameList[15]   > produccion.items_pallets.fecha_operativa
"fecha_operativa" "fecha_operativa" ? ? "date" ? ? ? ? ? ? yes ? no 15.8 yes ?
     _FldNameList[16]   > produccion.items_pallets.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[17]   > produccion.items_pallets.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[18]   > produccion.items_pallets.id_caract
"id_caract" "id_caract" ? ? "integer" ? ? ? ? ? ? yes ? no 17.4 yes ?
     _FldNameList[19]   > produccion.items_pallets.id_categoria
"id_categoria" "id_categoria" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[20]   > produccion.items_pallets.id_color
"id_color" "id_color" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[21]   > produccion.items_pallets.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes ?
     _FldNameList[22]   > produccion.items_pallets.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ?
     _FldNameList[23]   > produccion.items_pallets.id_finca_senasa
"id_finca_senasa" "id_finca_senasa" ? ? "integer" ? ? ? ? ? ? yes ? no 13 yes ?
     _FldNameList[24]   > produccion.items_pallets.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 4.2 yes ?
     _FldNameList[25]   > produccion.items_pallets.id_lote_senasa
"id_lote_senasa" "id_lote_senasa" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[26]   > produccion.items_pallets.id_marca
"id_marca" "id_marca" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ?
     _FldNameList[27]   > produccion.items_pallets.id_orden
"id_orden" "id_orden" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ?
     _FldNameList[28]   > produccion.items_pallets.id_origen
"id_origen" "id_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes ?
     _FldNameList[29]   > produccion.items_pallets.id_packing
"id_packing" "id_packing" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 yes ?
     _FldNameList[30]   > produccion.items_pallets.id_pallet
"id_pallet" "id_pallet" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[31]   > produccion.items_pallets.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[32]   > produccion.items_pallets.id_punto_emisor
"id_punto_emisor" "id_punto_emisor" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[33]   > produccion.items_pallets.id_sucursal_remito
"id_sucursal_remito" "id_sucursal_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 15.4 yes ?
     _FldNameList[34]   > produccion.items_pallets.id_suc_trabajo
"id_suc_trabajo" "id_suc_trabajo" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ?
     _FldNameList[35]   > produccion.items_pallets.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes ?
     _FldNameList[36]   > produccion.items_pallets.id_turno_packing
"id_turno_packing" "id_turno_packing" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ?
     _FldNameList[37]   > produccion.items_pallets.id_variedad
"id_variedad" "id_variedad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ?
     _FldNameList[38]   > produccion.items_pallets.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ?
     _FldNameList[39]   > produccion.items_pallets.item_factura
"item_factura" "item_factura" ? ? "integer" ? ? ? ? ? ? yes ? no 11.2 yes ?
     _FldNameList[40]   > produccion.items_pallets.item_pallet
"item_pallet" "item_pallet" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 yes ?
     _FldNameList[41]   > produccion.items_pallets.item_remito
"item_remito" "item_remito" ? ? "integer" ? ? ? ? ? ? yes ? no 11.2 yes ?
     _FldNameList[42]   > produccion.items_pallets.nro
"nro" "nro" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ?
     _FldNameList[43]   > produccion.items_pallets.nro_certificado
"nro_certificado" "nro_certificado" ? ? "integer" ? ? ? ? ? ? yes ? no 17.8 yes ?
     _FldNameList[44]   > produccion.items_pallets.nro_partida_general
"nro_partida_general" "nro_partida_general" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.6 yes ?
     _FldNameList[45]   > produccion.items_pallets.renspa
"renspa" "renspa" ? ? "character" ? ? ? ? ? ? yes ? no 21 yes ?
     _FldNameList[46]   > produccion.items_pallets.tipo_fruta
"tipo_fruta" "tipo_fruta" ? ? "logical" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[47]   > produccion.items_pallets.tipo_proceso
"tipo_proceso" "tipo_proceso" ? ? "character" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[48]   > produccion.items_pallets.ubicacion
"ubicacion" "ubicacion" ? ? "character" ? ? ? ? ? ? yes ? no 9.6 yes ?
     _FldNameList[49]   > produccion.items_pallets.unidad_productora
"unidad_productora" "unidad_productora" ? ? "character" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[50]   > produccion.items_pallets.zona_up
"zona_up" "zona_up" ? ? "character" ? ? ? ? ? ? yes ? no 8.6 yes ?
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

