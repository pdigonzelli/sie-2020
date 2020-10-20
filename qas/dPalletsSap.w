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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Global-define DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES pallets

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  bultos calibre camara certificado cert_china china codigo_trazabilidad~
 cod_prod cod_trazabilidad contramarca c_fecha c_hora c_usuario estado~
 fecha_anul fecha_anulacion fecha_comp fecha_operativa fecha_peso fecha_prod~
 fecha_proforma gln hora_anul hora_peso hora_prod id_articulo id_calidad~
 id_caract id_categoria id_color id_empresa id_envase id_estado_pallet~
 id_euroamerica id_finca_senasa id_lector_pallet id_lote id_lote_senasa~
 id_marca id_orden id_orden_anterior id_origen id_packing id_pallet~
 id_pallet_anterior id_pallet_sap id_pedido_sap id_proveedor~
 id_proveedor_caja id_punto_emisor id_punto_venta id_punto_venta_prof~
 id_sucursal_remito id_suc_destino id_suc_trabajo id_suc_trabajo_anterior~
 id_tipo_esquinero id_tipo_movsto id_tipo_pallet id_turno_packing~
 id_variedad id_viaje item item_pedido_sap item_remito merma nro nromov~
 nro_comp nro_comp_terceros nro_orden nro_pack_list nro_proforma pallet_chep~
 pallet_senasa peso renspa status_sap temperatura temporada testigo~
 tipo_fruta tipo_proceso ubicacion unidad_productora union_europea~
 usuario_anul zona_up
&Scoped-define DATA-FIELDS-IN-pallets bultos calibre camara certificado ~
cert_china china codigo_trazabilidad cod_prod cod_trazabilidad contramarca ~
c_fecha c_hora c_usuario estado fecha_anul fecha_anulacion fecha_comp ~
fecha_operativa fecha_peso fecha_prod fecha_proforma gln hora_anul ~
hora_peso hora_prod id_articulo id_calidad id_caract id_categoria id_color ~
id_empresa id_envase id_estado_pallet id_euroamerica id_finca_senasa ~
id_lector_pallet id_lote id_lote_senasa id_marca id_orden id_orden_anterior ~
id_origen id_packing id_pallet id_pallet_anterior id_pallet_sap ~
id_pedido_sap id_proveedor id_proveedor_caja id_punto_emisor id_punto_venta ~
id_punto_venta_prof id_sucursal_remito id_suc_destino id_suc_trabajo ~
id_suc_trabajo_anterior id_tipo_esquinero id_tipo_movsto id_tipo_pallet ~
id_turno_packing id_variedad id_viaje item item_pedido_sap item_remito ~
merma nro nromov nro_comp nro_comp_terceros nro_orden nro_pack_list ~
nro_proforma pallet_chep pallet_senasa peso renspa status_sap temperatura ~
temporada testigo tipo_fruta tipo_proceso ubicacion unidad_productora ~
union_europea usuario_anul zona_up 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dPalletsSap.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH pallets NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH pallets NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main pallets
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main pallets


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      pallets SCROLLING.
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
         HEIGHT             = 4.52
         WIDTH              = 61.6.
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
     _TblList          = "produccion.pallets"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > produccion.pallets.bultos
"bultos" "bultos" ? ? "integer" ? ? ? ? ? ? no ? no 6.6 yes ?
     _FldNameList[2]   > produccion.pallets.calibre
"calibre" "calibre" ? ? "character" ? ? ? ? ? ? no ? no 6.4 yes ?
     _FldNameList[3]   > produccion.pallets.camara
"camara" "camara" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes ?
     _FldNameList[4]   > produccion.pallets.certificado
"certificado" "certificado" ? ? "character" ? ? ? ? ? ? no ? no 11.6 yes ?
     _FldNameList[5]   > produccion.pallets.cert_china
"cert_china" "cert_china" ? ? "character" ? ? ? ? ? ? no ? no 9.8 yes ?
     _FldNameList[6]   > produccion.pallets.china
"china" "china" ? ? "logical" ? ? ? ? ? ? no ? no 5.4 yes ?
     _FldNameList[7]   > produccion.pallets.codigo_trazabilidad
"codigo_trazabilidad" "codigo_trazabilidad" ? ? "character" ? ? ? ? ? ? no ? no 9.2 yes ?
     _FldNameList[8]   > produccion.pallets.cod_prod
"cod_prod" "cod_prod" ? ? "character" ? ? ? ? ? ? no ? no 9.4 yes ?
     _FldNameList[9]   > produccion.pallets.cod_trazabilidad
"cod_trazabilidad" "cod_trazabilidad" ? ? "character" ? ? ? ? ? ? no ? no 15.8 yes ?
     _FldNameList[10]   > produccion.pallets.contramarca
"contramarca" "contramarca" ? ? "character" ? ? ? ? ? ? no ? no 4.8 yes ?
     _FldNameList[11]   > produccion.pallets.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes ?
     _FldNameList[12]   > produccion.pallets.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? no ? no 8 yes ?
     _FldNameList[13]   > produccion.pallets.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? no ? no 12 yes ?
     _FldNameList[14]   > produccion.pallets.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? no ? no 6.6 yes ?
     _FldNameList[15]   > produccion.pallets.fecha_anul
"fecha_anul" "fecha_anul" ? ? "date" ? ? ? ? ? ? no ? no 16 yes ?
     _FldNameList[16]   > produccion.pallets.fecha_anulacion
"fecha_anulacion" "fecha_anulacion" ? ? "date" ? ? ? ? ? ? no ? no 16 yes ?
     _FldNameList[17]   > produccion.pallets.fecha_comp
"fecha_comp" "fecha_comp" ? ? "date" ? ? ? ? ? ? no ? no 19.2 yes ?
     _FldNameList[18]   > produccion.pallets.fecha_operativa
"fecha_operativa" "fecha_operativa" ? ? "date" ? ? ? ? ? ? no ? no 15.8 yes ?
     _FldNameList[19]   > produccion.pallets.fecha_peso
"fecha_peso" "fecha_peso" ? ? "date" ? ? ? ? ? ? no ? no 11.4 yes ?
     _FldNameList[20]   > produccion.pallets.fecha_prod
"fecha_prod" "fecha_prod" ? ? "date" ? ? ? ? ? ? no ? no 17.4 yes ?
     _FldNameList[21]   > produccion.pallets.fecha_proforma
"fecha_proforma" "fecha_proforma" ? ? "date" ? ? ? ? ? ? no ? no 14.8 yes ?
     _FldNameList[22]   > produccion.pallets.gln
"gln" "gln" ? ? "decimal" ? ? ? ? ? ? no ? no 16.8 yes ?
     _FldNameList[23]   > produccion.pallets.hora_anul
"hora_anul" "hora_anul" ? ? "character" ? ? ? ? ? ? no ? no 10 yes ?
     _FldNameList[24]   > produccion.pallets.hora_peso
"hora_peso" "hora_peso" ? ? "character" ? ? ? ? ? ? no ? no 10 yes ?
     _FldNameList[25]   > produccion.pallets.hora_prod
"hora_prod" "hora_prod" ? ? "character" ? ? ? ? ? ? no ? no 10.2 yes ?
     _FldNameList[26]   > produccion.pallets.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes ?
     _FldNameList[27]   > produccion.pallets.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes ?
     _FldNameList[28]   > produccion.pallets.id_caract
"id_caract" "id_caract" ? ? "integer" ? ? ? ? ? ? no ? no 17.4 yes ?
     _FldNameList[29]   > produccion.pallets.id_categoria
"id_categoria" "id_categoria" ? ? "integer" ? ? ? ? ? ? no ? no 10.6 yes ?
     _FldNameList[30]   > produccion.pallets.id_color
"id_color" "id_color" ? ? "integer" ? ? ? ? ? ? no ? no 12 yes ?
     _FldNameList[31]   > produccion.pallets.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? no ? no 12.6 yes ?
     _FldNameList[32]   > produccion.pallets.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes ?
     _FldNameList[33]   > produccion.pallets.id_estado_pallet
"id_estado_pallet" "id_estado_pallet" ? ? "integer" ? ? ? ? ? ? no ? no 12.4 yes ?
     _FldNameList[34]   > produccion.pallets.id_euroamerica
"id_euroamerica" "id_euroamerica" ? ? "integer" ? ? ? ? ? ? no ? no 9.8 yes ?
     _FldNameList[35]   > produccion.pallets.id_finca_senasa
"id_finca_senasa" "id_finca_senasa" ? ? "integer" ? ? ? ? ? ? no ? no 13 yes ?
     _FldNameList[36]   > produccion.pallets.id_lector_pallet
"id_lector_pallet" "id_lector_pallet" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes ?
     _FldNameList[37]   > produccion.pallets.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? no ? no 4.2 yes ?
     _FldNameList[38]   > produccion.pallets.id_lote_senasa
"id_lote_senasa" "id_lote_senasa" ? ? "integer" ? ? ? ? ? ? no ? no 12 yes ?
     _FldNameList[39]   > produccion.pallets.id_marca
"id_marca" "id_marca" ? ? "integer" ? ? ? ? ? ? no ? no 6 yes ?
     _FldNameList[40]   > produccion.pallets.id_orden
"id_orden" "id_orden" ? ? "integer" ? ? ? ? ? ? no ? no 10.4 yes ?
     _FldNameList[41]   > produccion.pallets.id_orden_anterior
"id_orden_anterior" "id_orden_anterior" ? ? "integer" ? ? ? ? ? ? no ? no 10.4 yes ?
     _FldNameList[42]   > produccion.pallets.id_origen
"id_origen" "id_origen" ? ? "integer" ? ? ? ? ? ? no ? no 6.6 yes ?
     _FldNameList[43]   > produccion.pallets.id_packing
"id_packing" "id_packing" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes ?
     _FldNameList[44]   > produccion.pallets.id_pallet
"id_pallet" "id_pallet" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes ?
     _FldNameList[45]   > produccion.pallets.id_pallet_anterior
"id_pallet_anterior" "id_pallet_anterior" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes ?
     _FldNameList[46]   > produccion.pallets.id_pallet_sap
"id_pallet_sap" "id_pallet_sap" ? ? "character" ? ? ? ? ? ? no ? no 10 yes ?
     _FldNameList[47]   > produccion.pallets.id_pedido_sap
"id_pedido_sap" "id_pedido_sap" ? ? "character" ? ? ? ? ? ? no ? no 17.6 yes ?
     _FldNameList[48]   > produccion.pallets.id_proveedor
"id_proveedor" "id_proveedor" ? ? "integer" ? ? ? ? ? ? no ? no 9.2 yes ?
     _FldNameList[49]   > produccion.pallets.id_proveedor_caja
"id_proveedor_caja" "id_proveedor_caja" ? ? "integer" ? ? ? ? ? ? no ? no 9.2 yes ?
     _FldNameList[50]   > produccion.pallets.id_punto_emisor
"id_punto_emisor" "id_punto_emisor" ? ? "integer" ? ? ? ? ? ? no ? no 12.4 yes ?
     _FldNameList[51]   > produccion.pallets.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes ?
     _FldNameList[52]   > produccion.pallets.id_punto_venta_prof
"id_punto_venta_prof" "id_punto_venta_prof" ? ? "integer" ? ? ? ? ? ? no ? no 19.6 yes ?
     _FldNameList[53]   > produccion.pallets.id_sucursal_remito
"id_sucursal_remito" "id_sucursal_remito" ? ? "integer" ? ? ? ? ? ? no ? no 15.4 yes ?
     _FldNameList[54]   > produccion.pallets.id_suc_destino
"id_suc_destino" "id_suc_destino" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes ?
     _FldNameList[55]   > produccion.pallets.id_suc_trabajo
"id_suc_trabajo" "id_suc_trabajo" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes ?
     _FldNameList[56]   > produccion.pallets.id_suc_trabajo_anterior
"id_suc_trabajo_anterior" "id_suc_trabajo_anterior" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes ?
     _FldNameList[57]   > produccion.pallets.id_tipo_esquinero
"id_tipo_esquinero" "id_tipo_esquinero" ? ? "integer" ? ? ? ? ? ? no ? no 14.2 yes ?
     _FldNameList[58]   > produccion.pallets.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? no ? no 9.4 yes ?
     _FldNameList[59]   > produccion.pallets.id_tipo_pallet
"id_tipo_pallet" "id_tipo_pallet" ? ? "integer" ? ? ? ? ? ? no ? no 10 yes ?
     _FldNameList[60]   > produccion.pallets.id_turno_packing
"id_turno_packing" "id_turno_packing" ? ? "integer" ? ? ? ? ? ? no ? no 12.2 yes ?
     _FldNameList[61]   > produccion.pallets.id_variedad
"id_variedad" "id_variedad" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes ?
     _FldNameList[62]   > produccion.pallets.id_viaje
"id_viaje" "id_viaje" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes ?
     _FldNameList[63]   > produccion.pallets.item
"item" "item" ? ? "integer" ? ? ? ? ? ? no ? no 4 yes ?
     _FldNameList[64]   > produccion.pallets.item_pedido_sap
"item_pedido_sap" "item_pedido_sap" ? ? "character" ? ? ? ? ? ? no ? no 21 yes ?
     _FldNameList[65]   > produccion.pallets.item_remito
"item_remito" "item_remito" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes ?
     _FldNameList[66]   > produccion.pallets.merma
"merma" "merma" ? ? "logical" ? ? ? ? ? ? no ? no 6.4 yes ?
     _FldNameList[67]   > produccion.pallets.nro
"nro" "nro" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes ?
     _FldNameList[68]   > produccion.pallets.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes ?
     _FldNameList[69]   > produccion.pallets.nro_comp
"nro_comp" "nro_comp" ? ? "integer" ? ? ? ? ? ? no ? no 16.6 yes ?
     _FldNameList[70]   > produccion.pallets.nro_comp_terceros
"nro_comp_terceros" "nro_comp_terceros" ? ? "character" ? ? ? ? ? ? no ? no 16.6 yes ?
     _FldNameList[71]   > produccion.pallets.nro_orden
"nro_orden" "nro_orden" ? ? "integer" ? ? ? ? ? ? no ? no 9.8 yes ?
     _FldNameList[72]   > produccion.pallets.nro_pack_list
"nro_pack_list" "nro_pack_list" ? ? "character" ? ? ? ? ? ? no ? no 15.6 yes ?
     _FldNameList[73]   > produccion.pallets.nro_proforma
"nro_proforma" "nro_proforma" ? ? "integer" ? ? ? ? ? ? no ? no 12.4 yes ?
     _FldNameList[74]   > produccion.pallets.pallet_chep
"pallet_chep" "pallet_chep" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes ?
     _FldNameList[75]   > produccion.pallets.pallet_senasa
"pallet_senasa" "pallet_senasa" ? ? "character" ? ? ? ? ? ? no ? no 18 yes ?
     _FldNameList[76]   > produccion.pallets.peso
"peso" "peso" ? ? "decimal" ? ? ? ? ? ? no ? no 9.6 yes ?
     _FldNameList[77]   > produccion.pallets.renspa
"renspa" "renspa" ? ? "character" ? ? ? ? ? ? no ? no 21 yes ?
     _FldNameList[78]   > produccion.pallets.status_sap
"status_sap" "status_sap" ? ? "integer" ? ? ? ? ? ? no ? no 10.4 yes ?
     _FldNameList[79]   > produccion.pallets.temperatura
"temperatura" "temperatura" ? ? "decimal" ? ? ? ? ? ? no ? no 12 yes ?
     _FldNameList[80]   > produccion.pallets.temporada
"temporada" "temporada" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes ?
     _FldNameList[81]   > produccion.pallets.testigo
"testigo" "testigo" ? ? "logical" ? ? ? ? ? ? no ? no 7 yes ?
     _FldNameList[82]   > produccion.pallets.tipo_fruta
"tipo_fruta" "tipo_fruta" ? ? "logical" ? ? ? ? ? ? no ? no 11.8 yes ?
     _FldNameList[83]   > produccion.pallets.tipo_proceso
"tipo_proceso" "tipo_proceso" ? ? "character" ? ? ? ? ? ? no ? no 7 yes ?
     _FldNameList[84]   > produccion.pallets.ubicacion
"ubicacion" "ubicacion" ? ? "character" ? ? ? ? ? ? no ? no 9.6 yes ?
     _FldNameList[85]   > produccion.pallets.unidad_productora
"unidad_productora" "unidad_productora" ? ? "character" ? ? ? ? ? ? no ? no 12.4 yes ?
     _FldNameList[86]   > produccion.pallets.union_europea
"union_europea" "union_europea" ? ? "logical" ? ? ? ? ? ? no ? no 14.2 yes ?
     _FldNameList[87]   > produccion.pallets.usuario_anul
"usuario_anul" "usuario_anul" ? ? "character" ? ? ? ? ? ? no ? no 12.6 yes ?
     _FldNameList[88]   > produccion.pallets.zona_up
"zona_up" "zona_up" ? ? "character" ? ? ? ? ? ? no ? no 8.6 yes ?
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

