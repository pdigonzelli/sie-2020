&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
          cons             PROGRESS
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
&Scoped-define INTERNAL-TABLES r_cobranzas_pe cobranzas_me ~
permisos_embarque

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio cobro_ingresado c_fecha c_hora c_usuario fecha id_aduana id_cobranza~
 id_operacion id_permiso_embarque id_sucursal id_tipocomp importe nromov~
 nro_comp nro_minuta
&Scoped-define ENABLED-FIELDS-IN-r_cobranzas_pe anio cobro_ingresado ~
c_fecha c_hora c_usuario fecha id_aduana id_cobranza id_operacion ~
id_permiso_embarque id_sucursal id_tipocomp importe nromov nro_comp ~
nro_minuta 
&Scoped-Define DATA-FIELDS  anio cobro_ingresado c_fecha c_hora c_usuario fecha id_aduana id_cobranza~
 id_operacion id_permiso_embarque id_sucursal id_tipocomp importe nromov~
 nro_comp nro_minuta conciliada c_fecha-2 c_hora-2 c_usuario-2 estado~
 fecha-2 fecha_comp id_campana id_cliente id_documento id_empresa id_moneda~
 id_operacion-2 id_punto_venta id_sucursal-2 id_tipocomp-2 id_tipo_cobranza~
 id_viaje importe-2 importe_uss nromov-2 nro_comp-2 nro_factura nro_minuta-2~
 observaciones pvta_factura saldo saldo_uss tipo_cambio tipo_minuta~
 vencimiento anio-2 banco_interviniente consignacion c_fecha-3 c_hora-3~
 c_usuario-3 estado_afip estado_bcra fecha-3 fecha_concluido fecha_cumplido~
 fecha_declaracion_venta fecha_envio_tuc fecha_oficializacion fecha_rec_tuc~
 fecha_venc_derecho id_aduana-2 id_articulo id_calidad id_cliente-2~
 id_despachante id_moneda_origen id_orden_entrega id_permiso_embarque-2~
 id_posicion_arancelaria id_tipo_articulo importe-3 importe_derecho~
 importe_reembolso importe_reintegro item_oe nro_remito_envio_tuc~
 observaciones-2 rectificado tipo_pe
&Scoped-define DATA-FIELDS-IN-r_cobranzas_pe anio cobro_ingresado c_fecha ~
c_hora c_usuario fecha id_aduana id_cobranza id_operacion ~
id_permiso_embarque id_sucursal id_tipocomp importe nromov nro_comp ~
nro_minuta 
&Scoped-define DATA-FIELDS-IN-cobranzas_me conciliada c_fecha-2 c_hora-2 ~
c_usuario-2 estado fecha-2 fecha_comp id_campana id_cliente id_documento ~
id_empresa id_moneda id_operacion-2 id_punto_venta id_sucursal-2 ~
id_tipocomp-2 id_tipo_cobranza id_viaje importe-2 importe_uss nromov-2 ~
nro_comp-2 nro_factura nro_minuta-2 observaciones pvta_factura saldo ~
saldo_uss tipo_cambio tipo_minuta vencimiento 
&Scoped-define DATA-FIELDS-IN-permisos_embarque anio-2 banco_interviniente ~
consignacion c_fecha-3 c_hora-3 c_usuario-3 estado_afip estado_bcra fecha-3 ~
fecha_concluido fecha_cumplido fecha_declaracion_venta fecha_envio_tuc ~
fecha_oficializacion fecha_rec_tuc fecha_venc_derecho id_aduana-2 ~
id_articulo id_calidad id_cliente-2 id_despachante id_moneda_origen ~
id_orden_entrega id_permiso_embarque-2 id_posicion_arancelaria ~
id_tipo_articulo importe-3 importe_derecho importe_reembolso ~
importe_reintegro item_oe nro_remito_envio_tuc observaciones-2 rectificado ~
tipo_pe 
&Scoped-Define MANDATORY-FIELDS  id_sucursal id_calidad
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.c_fecha-2 = cobranzas_me.c_fecha~
  rowObject.c_hora-2 = cobranzas_me.c_hora~
  rowObject.c_usuario-2 = cobranzas_me.c_usuario~
  rowObject.fecha-2 = cobranzas_me.fecha~
  rowObject.id_operacion-2 = cobranzas_me.id_operacion~
  rowObject.id_sucursal-2 = cobranzas_me.id_sucursal~
  rowObject.id_tipocomp-2 = cobranzas_me.id_tipocomp~
  rowObject.importe-2 = cobranzas_me.importe~
  rowObject.nromov-2 = cobranzas_me.nromov~
  rowObject.nro_comp-2 = cobranzas_me.nro_comp~
  rowObject.nro_minuta-2 = cobranzas_me.nro_minuta~
  rowObject.anio-2 = permisos_embarque.anio~
  rowObject.c_fecha-3 = permisos_embarque.c_fecha~
  rowObject.c_hora-3 = permisos_embarque.c_hora~
  rowObject.c_usuario-3 = permisos_embarque.c_usuario~
  rowObject.fecha-3 = permisos_embarque.fecha~
  rowObject.id_aduana-2 = permisos_embarque.id_aduana~
  rowObject.id_cliente-2 = permisos_embarque.id_cliente~
  rowObject.id_permiso_embarque-2 = permisos_embarque.id_permiso_embarque~
  rowObject.importe-3 = permisos_embarque.importe~
  rowObject.observaciones-2 = permisos_embarque.observaciones
&Scoped-Define DATA-FIELD-DEFS "d000cobranzaspermisosembarque.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH r_cobranzas_pe NO-LOCK, ~
      EACH cobranzas_me OF r_cobranzas_pe NO-LOCK, ~
      EACH permisos_embarque OF r_cobranzas_pe NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH r_cobranzas_pe NO-LOCK, ~
      EACH cobranzas_me OF r_cobranzas_pe NO-LOCK, ~
      EACH permisos_embarque OF r_cobranzas_pe NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main r_cobranzas_pe cobranzas_me ~
permisos_embarque
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main r_cobranzas_pe
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main cobranzas_me
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main permisos_embarque


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      r_cobranzas_pe, 
      cobranzas_me, 
      permisos_embarque SCROLLING.
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
     _TblList          = "general.r_cobranzas_pe,cons.cobranzas_me OF general.r_cobranzas_pe,industria.permisos_embarque OF general.r_cobranzas_pe"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.r_cobranzas_pe.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > general.r_cobranzas_pe.cobro_ingresado
"cobro_ingresado" "cobro_ingresado" ? ? "logical" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[3]   > general.r_cobranzas_pe.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[4]   > general.r_cobranzas_pe.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[5]   > general.r_cobranzas_pe.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[6]   > general.r_cobranzas_pe.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[7]   > general.r_cobranzas_pe.id_aduana
"id_aduana" "id_aduana" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[8]   > general.r_cobranzas_pe.id_cobranza
"id_cobranza" "id_cobranza" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[9]   > general.r_cobranzas_pe.id_operacion
"id_operacion" "id_operacion" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[10]   > general.r_cobranzas_pe.id_permiso_embarque
"id_permiso_embarque" "id_permiso_embarque" ? ? "character" ? ? ? ? ? ? yes ? no 13 yes
     _FldNameList[11]   > general.r_cobranzas_pe.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[12]   > general.r_cobranzas_pe.id_tipocomp
"id_tipocomp" "id_tipocomp" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[13]   > general.r_cobranzas_pe.importe
"importe" "importe" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[14]   > general.r_cobranzas_pe.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[15]   > general.r_cobranzas_pe.nro_comp
"nro_comp" "nro_comp" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[16]   > general.r_cobranzas_pe.nro_minuta
"nro_minuta" "nro_minuta" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[17]   > cons.cobranzas_me.conciliada
"conciliada" "conciliada" ? ? "logical" ? ? ? ? ? ? no ? no 9.8 yes
     _FldNameList[18]   > cons.cobranzas_me.c_fecha
"c_fecha" "c_fecha-2" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[19]   > cons.cobranzas_me.c_hora
"c_hora" "c_hora-2" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[20]   > cons.cobranzas_me.c_usuario
"c_usuario" "c_usuario-2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[21]   > cons.cobranzas_me.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[22]   > cons.cobranzas_me.fecha
"fecha" "fecha-2" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[23]   > cons.cobranzas_me.fecha_comp
"fecha_comp" "fecha_comp" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[24]   > cons.cobranzas_me.id_campana
"id_campana" "id_campana" ? ? "integer" ? ? ? ? ? ? no ? no 9 yes
     _FldNameList[25]   > cons.cobranzas_me.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[26]   > cons.cobranzas_me.id_documento
"id_documento" "id_documento" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[27]   > cons.cobranzas_me.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[28]   > cons.cobranzas_me.id_moneda
"id_moneda" "id_moneda" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[29]   > cons.cobranzas_me.id_operacion
"id_operacion" "id_operacion-2" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[30]   > cons.cobranzas_me.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? no ? no 7.6 yes
     _FldNameList[31]   > cons.cobranzas_me.id_sucursal
"id_sucursal" "id_sucursal-2" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[32]   > cons.cobranzas_me.id_tipocomp
"id_tipocomp" "id_tipocomp-2" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[33]   > cons.cobranzas_me.id_tipo_cobranza
"id_tipo_cobranza" "id_tipo_cobranza" ? ? "integer" ? ? ? ? ? ? no ? no 13.8 yes
     _FldNameList[34]   > cons.cobranzas_me.id_viaje
"id_viaje" "id_viaje" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[35]   > cons.cobranzas_me.importe
"importe" "importe-2" ? ? "decimal" ? ? ? ? ? ? no ? no 19.8 yes
     _FldNameList[36]   > cons.cobranzas_me.importe_uss
"importe_uss" "importe_uss" ? ? "decimal" ? ? ? ? ? ? no ? no 19.8 yes
     _FldNameList[37]   > cons.cobranzas_me.nromov
"nromov" "nromov-2" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[38]   > cons.cobranzas_me.nro_comp
"nro_comp" "nro_comp-2" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[39]   > cons.cobranzas_me.nro_factura
"nro_factura" "nro_factura" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[40]   > cons.cobranzas_me.nro_minuta
"nro_minuta" "nro_minuta-2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[41]   > cons.cobranzas_me.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? no ? no 60 yes
     _FldNameList[42]   > cons.cobranzas_me.pvta_factura
"pvta_factura" "pvta_factura" ? ? "integer" ? ? ? ? ? ? no ? no 12.8 yes
     _FldNameList[43]   > cons.cobranzas_me.saldo
"saldo" "saldo" ? ? "decimal" ? ? ? ? ? ? no ? no 19.8 yes
     _FldNameList[44]   > cons.cobranzas_me.saldo_uss
"saldo_uss" "saldo_uss" ? ? "decimal" ? ? ? ? ? ? no ? no 19.8 yes
     _FldNameList[45]   > cons.cobranzas_me.tipo_cambio
"tipo_cambio" "tipo_cambio" ? ? "decimal" ? ? ? ? ? ? no ? no 15 yes
     _FldNameList[46]   > cons.cobranzas_me.tipo_minuta
"tipo_minuta" "tipo_minuta" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[47]   > cons.cobranzas_me.vencimiento
"vencimiento" "vencimiento" ? ? "date" ? ? ? ? ? ? no ? no 12.2 yes
     _FldNameList[48]   > general.permisos_embarque.anio
"anio" "anio-2" ? ? "integer" ? ? ? ? ? ? no ? no 4.8 yes
     _FldNameList[49]   > general.permisos_embarque.banco_interviniente
"banco_interviniente" "banco_interviniente" ? ? "character" ? ? ? ? ? ? no ? no 60 yes
     _FldNameList[50]   > general.permisos_embarque.consignacion
"consignacion" "consignacion" ? ? "logical" ? ? ? ? ? ? no ? no 4.8 yes
     _FldNameList[51]   > general.permisos_embarque.c_fecha
"c_fecha" "c_fecha-3" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[52]   > general.permisos_embarque.c_hora
"c_hora" "c_hora-3" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[53]   > general.permisos_embarque.c_usuario
"c_usuario" "c_usuario-3" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[54]   > general.permisos_embarque.estado_afip
"estado_afip" "estado_afip" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[55]   > general.permisos_embarque.estado_bcra
"estado_bcra" "estado_bcra" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[56]   > general.permisos_embarque.fecha
"fecha" "fecha-3" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[57]   > general.permisos_embarque.fecha_concluido
"fecha_concluido" "fecha_concluido" ? ? "date" ? ? ? ? ? ? no ? no 16 yes
     _FldNameList[58]   > general.permisos_embarque.fecha_cumplido
"fecha_cumplido" "fecha_cumplido" ? ? "date" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[59]   > general.permisos_embarque.fecha_declaracion_venta
"fecha_declaracion_venta" "fecha_declaracion_venta" ? ? "date" ? ? ? ? ? ? no ? no 13.2 yes
     _FldNameList[60]   > general.permisos_embarque.fecha_envio_tuc
"fecha_envio_tuc" "fecha_envio_tuc" ? ? "date" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[61]   > general.permisos_embarque.fecha_oficializacion
"fecha_oficializacion" "fecha_oficializacion" ? ? "date" ? ? ? ? ? ? no ? no 11 yes
     _FldNameList[62]   > general.permisos_embarque.fecha_rec_tuc
"fecha_rec_tuc" "fecha_rec_tuc" ? ? "date" ? ? ? ? ? ? no ? no 13.8 yes
     _FldNameList[63]   > general.permisos_embarque.fecha_venc_derecho
"fecha_venc_derecho" "fecha_venc_derecho" ? ? "date" ? ? ? ? ? ? no ? no 14.4 yes
     _FldNameList[64]   > general.permisos_embarque.id_aduana
"id_aduana" "id_aduana-2" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[65]   > general.permisos_embarque.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[66]   > general.permisos_embarque.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? yes 10.2 yes
     _FldNameList[67]   > general.permisos_embarque.id_cliente
"id_cliente" "id_cliente-2" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[68]   > general.permisos_embarque.id_despachante
"id_despachante" "id_despachante" ? ? "integer" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[69]   > general.permisos_embarque.id_moneda_origen
"id_moneda_origen" "id_moneda_origen" ? ? "integer" ? ? ? ? ? ? no ? no 14.6 yes
     _FldNameList[70]   > general.permisos_embarque.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[71]   > general.permisos_embarque.id_permiso_embarque
"id_permiso_embarque" "id_permiso_embarque-2" ? ? "character" ? ? ? ? ? ? no ? no 13 yes
     _FldNameList[72]   > general.permisos_embarque.id_posicion_arancelaria
"id_posicion_arancelaria" "id_posicion_arancelaria" ? ? "character" ? ? ? ? ? ? no ? no 14 yes
     _FldNameList[73]   > general.permisos_embarque.id_tipo_articulo
"id_tipo_articulo" "id_tipo_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[74]   > general.permisos_embarque.importe
"importe" "importe-3" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[75]   > general.permisos_embarque.importe_derecho
"importe_derecho" "importe_derecho" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[76]   > general.permisos_embarque.importe_reembolso
"importe_reembolso" "importe_reembolso" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[77]   > general.permisos_embarque.importe_reintegro
"importe_reintegro" "importe_reintegro" ? ? "decimal" ? ? ? ? ? ? no ? no 13.2 yes
     _FldNameList[78]   > general.permisos_embarque.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? no ? no 8.6 yes
     _FldNameList[79]   > general.permisos_embarque.nro_remito_envio_tuc
"nro_remito_envio_tuc" "nro_remito_envio_tuc" ? ? "character" ? ? ? ? ? ? no ? no 20 yes
     _FldNameList[80]   > general.permisos_embarque.observaciones
"observaciones" "observaciones-2" ? ? "character" ? ? ? ? ? ? no ? no 100 yes
     _FldNameList[81]   > general.permisos_embarque.rectificado
"rectificado" "rectificado" ? ? "logical" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[82]   > general.permisos_embarque.tipo_pe
"tipo_pe" "tipo_pe" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*Algoritmo
  
  tomar nro de minuta
  entrar a la tabla cobranza_me y recuperar los valores de los campos
  id_operacion
  id_tipocomp
  id_sucursal
  nromov
  nro_comp
  
  obtener el ultimo valor de id_cobranza (secuencia)
  
  luego hacer un assign sobre la tabla r_cobranza_pe
  
  
  */
  
  
  
  DEFINE VAR iIdOperacion     AS INTEGER NO-UNDO.
  DEFINE VAR iIdTipoComp      AS INTEGER NO-UNDO.
  DEFINE VAR iIdSucursal      AS INTEGER NO-UNDO.
  DEFINE VAR iNroMov          AS INTEGER NO-UNDO.
  DEFINE VAR iNroComp         AS INTEGER NO-UNDO.
  DEFINE VAR iIdCobranza      AS INTEGER NO-UNDO.
  DEFINE VAR iIdAduana        AS INTEGER NO-UNDO.
  DEFINE VAR iAnio            AS INTEGER NO-UNDO.
  
  DEFINE VAR hDataSource      AS HANDLE    NO-UNDO.
  DEFINE VAR cPermisoEmbarque AS CHARACTER NO-UNDO.

  
  FIND FIRST RowObjUpd.
  
  /*valores del sdo de permisos de embarque*/
  hDataSource      = DYNAMIC-FUNCTION ('getDataSource':U).
  cPermisoEmbarque = DYNAMIC-FUNCTION('columnValue':U IN hDataSource, 'id_permiso_embarque':U).
  iIdAduana        = DYNAMIC-FUNCTION('columnValue':U IN hDataSource, 'id_Aduana':U).
  iAnio            = DYNAMIC-FUNCTION('columnValue':U IN hDataSource, 'anio':U).

  /*insert*/
  
  FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "A":U:

    FIND FIRST cobranzas_me WHERE cobranzas_me.nro_minuta = RowObjUpd.nro_minuta NO-LOCK.
    IF AVAILABLE cobranzas_me THEN DO:
      iIdOperacion = cobranzas_me.id_operacion.
      iIdTipoComp  = cobranzas_me.id_tipocomp.
      iIdSucursal  = cobranzas_me.id_sucursal.
      iNroMov      = cobranzas_me.nromov.
      iNroComp     = cobranzas_me.nro_comp.
    END.
    
    iIdCobranza = 1.
    FOR EACH r_cobranzas_pe WHERE r_cobranzas_pe.id_aduana            = iIdAduana
                              AND r_cobranzas_pe.anio                 = iAnio
                              AND r_cobranzas_pe.id_permiso_embarque  = cPermisoEmbarque
                              AND r_cobranzas_pe.id_operacion         = iIdOperacion
                              AND r_cobranzas_pe.id_tipocomp          = iIdTipoComp
                              AND r_cobranzas_pe.id_sucursal          = iIdSucursal
                              AND r_cobranzas_pe.nromov               = iNroMov 
                            BY r_cobranzas_pe.id_cobranza DESC.
      iIdCobranza = r_cobranzas_pe.id_cobranza + 1.
    END.
    /*MESSAGE iIdAduana iAnio cPermisoEmbarque iIdOperacion iIdTipoComp iIdSucursal iNroMov iIdCobranza VIEW-AS ALERT-BOX.*/
    
    ASSIGN RowObjUpd.id_aduana            = iIdAduana
           RowObjUpd.anio                 = iAnio
           RowObjUpd.id_permiso_embarque  = cPermisoEmbarque
           RowObjUpd.id_operacion         = iIdOperacion
           RowObjUpd.id_tipocomp          = iIdTipoComp
           RowObjUpd.id_sucursal          = iIdSucursal
           RowObjUpd.nromov               = iNroMov 
           RowObjUpd.id_cobranza          = iIdCobranza
           RowObjUpd.c_fecha              = TODAY
           RowObjUpd.c_hora               = STRING(TIME,"hh:mm:ss")
           RowObjUpd.c_usuario            = USERID('userdb').
           
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}
