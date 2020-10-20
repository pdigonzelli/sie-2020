&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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
&Scoped-define INTERNAL-TABLES cobranzas_me tipo_moneda clientes ~
tipo_cobranza

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  conciliada c_fecha c_hora c_usuario estado fecha fecha_comp id_campana~
 id_cliente id_documento id_empresa id_moneda id_operacion id_punto_venta~
 id_sucursal id_tipocomp id_tipo_cobranza id_viaje importe importe_uss~
 nromov nro_comp nro_factura nro_minuta observaciones pvta_factura saldo~
 saldo_uss tipo_cambio tipo_minuta vencimiento abreviatura~
 abreviatura_ingles c_fecha-2 c_hora-2 c_usuario-2 descripcion~
 descripcion_ingles id_moneda-2 barrio bon_global1 bon_global2 cf_global~
 citde coneccion cuit c_fecha-3 c_hora-3 c_usuario-3 datos_contratos~
 desp_global1 desp_global2 desp_global3 des_global1 des_global2 des_global3~
 dias_acreditacion dias_pago dia_resumen domicilio domicilio-1 empacador~
 estado-2 es_consignatario fax ganancias id_articulo id_cliente-2~
 id_despachante id_documento-2 id_interno_contribuyente id_iva_contribuyente~
 id_localidad id_provincia id_prov_usa id_ret_ganancias id_ret_ingbrutos~
 id_ret_iva id_tipo_caratula_cliente id_tipo_caratula_despachante~
 id_tipo_cliente id_zona ingresos_brutos letra_permitida localidad Mercado~
 mercado_externo municipalidad nombre observaciones-2 pago_habitual pais~
 porc_ret_3337 porc_ret_ganancias porc_ret_ingbrutos porc_ret_iva postal~
 provincia razon_social razon_social-1 ret_global1 ret_global2 ret_global3~
 saldo-2 Sdo_Inicial$ sdo_inicialu$ telefono1 telefono2 telefono3~
 vto_ret_ganancias vto_ret_ingbrutos vto_ret_iva abreviatura-2 descripcion-2~
 id_tipo_cobranza-2
&Scoped-define DATA-FIELDS-IN-cobranzas_me conciliada c_fecha c_hora ~
c_usuario estado fecha fecha_comp id_campana id_cliente id_documento ~
id_empresa id_moneda id_operacion id_punto_venta id_sucursal id_tipocomp ~
id_tipo_cobranza id_viaje importe importe_uss nromov nro_comp nro_factura ~
nro_minuta observaciones pvta_factura saldo saldo_uss tipo_cambio ~
tipo_minuta vencimiento 
&Scoped-define DATA-FIELDS-IN-tipo_moneda abreviatura abreviatura_ingles ~
c_fecha-2 c_hora-2 c_usuario-2 descripcion descripcion_ingles id_moneda-2 
&Scoped-define DATA-FIELDS-IN-clientes barrio bon_global1 bon_global2 ~
cf_global citde coneccion cuit c_fecha-3 c_hora-3 c_usuario-3 ~
datos_contratos desp_global1 desp_global2 desp_global3 des_global1 ~
des_global2 des_global3 dias_acreditacion dias_pago dia_resumen domicilio ~
domicilio-1 empacador estado-2 es_consignatario fax ganancias id_articulo ~
id_cliente-2 id_despachante id_documento-2 id_interno_contribuyente ~
id_iva_contribuyente id_localidad id_provincia id_prov_usa id_ret_ganancias ~
id_ret_ingbrutos id_ret_iva id_tipo_caratula_cliente ~
id_tipo_caratula_despachante id_tipo_cliente id_zona ingresos_brutos ~
letra_permitida localidad Mercado mercado_externo municipalidad nombre ~
observaciones-2 pago_habitual pais porc_ret_3337 porc_ret_ganancias ~
porc_ret_ingbrutos porc_ret_iva postal provincia razon_social ~
razon_social-1 ret_global1 ret_global2 ret_global3 saldo-2 Sdo_Inicial$ ~
sdo_inicialu$ telefono1 telefono2 telefono3 vto_ret_ganancias ~
vto_ret_ingbrutos vto_ret_iva 
&Scoped-define DATA-FIELDS-IN-tipo_cobranza abreviatura-2 descripcion-2 ~
id_tipo_cobranza-2 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.c_fecha-2 = tipo_moneda.c_fecha~
  rowObject.c_hora-2 = tipo_moneda.c_hora~
  rowObject.c_usuario-2 = tipo_moneda.c_usuario~
  rowObject.id_moneda-2 = tipo_moneda.id_moneda~
  rowObject.bon_global1 = clientes.bon_global[1]~
  rowObject.bon_global2 = clientes.bon_global[2]~
  rowObject.c_fecha-3 = clientes.c_fecha~
  rowObject.c_hora-3 = clientes.c_hora~
  rowObject.c_usuario-3 = clientes.c_usuario~
  rowObject.desp_global1 = clientes.desp_global[1]~
  rowObject.desp_global2 = clientes.desp_global[2]~
  rowObject.desp_global3 = clientes.desp_global[3]~
  rowObject.des_global1 = clientes.des_global[1]~
  rowObject.des_global2 = clientes.des_global[2]~
  rowObject.des_global3 = clientes.des_global[3]~
  rowObject.estado-2 = clientes.estado~
  rowObject.id_cliente-2 = clientes.id_cliente~
  rowObject.id_documento-2 = clientes.id_documento~
  rowObject.observaciones-2 = clientes.observaciones~
  rowObject.ret_global1 = clientes.ret_global[1]~
  rowObject.ret_global2 = clientes.ret_global[2]~
  rowObject.ret_global3 = clientes.ret_global[3]~
  rowObject.saldo-2 = clientes.saldo~
  rowObject.telefono1 = clientes.telefono[1]~
  rowObject.telefono2 = clientes.telefono[2]~
  rowObject.telefono3 = clientes.telefono[3]~
  rowObject.abreviatura-2 = tipo_cobranza.abreviatura~
  rowObject.descripcion-2 = tipo_cobranza.descripcion~
  rowObject.id_tipo_cobranza-2 = tipo_cobranza.id_tipo_cobranza
&Scoped-Define DATA-FIELD-DEFS "d000minutas.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH cobranzas_me NO-LOCK, ~
      EACH tipo_moneda OF cobranzas_me NO-LOCK, ~
      EACH clientes OF cobranzas_me NO-LOCK, ~
      EACH tipo_cobranza OF cobranzas_me NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH cobranzas_me NO-LOCK, ~
      EACH tipo_moneda OF cobranzas_me NO-LOCK, ~
      EACH clientes OF cobranzas_me NO-LOCK, ~
      EACH tipo_cobranza OF cobranzas_me NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main cobranzas_me tipo_moneda clientes ~
tipo_cobranza
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main cobranzas_me
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main tipo_moneda
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main clientes
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main tipo_cobranza


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      cobranzas_me, 
      tipo_moneda, 
      clientes, 
      tipo_cobranza SCROLLING.
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
     _TblList          = "cons.cobranzas_me,comercial.tipo_moneda OF cons.cobranzas_me,general.clientes OF cons.cobranzas_me,cons.tipo_cobranza OF cons.cobranzas_me"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > cons.cobranzas_me.conciliada
"conciliada" "conciliada" ? ? "logical" ? ? ? ? ? ? no ? no 9.8 yes
     _FldNameList[2]   > cons.cobranzas_me.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[3]   > cons.cobranzas_me.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[4]   > cons.cobranzas_me.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[5]   > cons.cobranzas_me.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[6]   > cons.cobranzas_me.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[7]   > cons.cobranzas_me.fecha_comp
"fecha_comp" "fecha_comp" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[8]   > cons.cobranzas_me.id_campana
"id_campana" "id_campana" ? ? "integer" ? ? ? ? ? ? no ? no 9 yes
     _FldNameList[9]   > cons.cobranzas_me.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[10]   > cons.cobranzas_me.id_documento
"id_documento" "id_documento" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[11]   > cons.cobranzas_me.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[12]   > cons.cobranzas_me.id_moneda
"id_moneda" "id_moneda" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[13]   > cons.cobranzas_me.id_operacion
"id_operacion" "id_operacion" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[14]   > cons.cobranzas_me.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? no ? no 7.6 yes
     _FldNameList[15]   > cons.cobranzas_me.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[16]   > cons.cobranzas_me.id_tipocomp
"id_tipocomp" "id_tipocomp" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[17]   > cons.cobranzas_me.id_tipo_cobranza
"id_tipo_cobranza" "id_tipo_cobranza" ? ? "integer" ? ? ? ? ? ? no ? no 13.8 yes
     _FldNameList[18]   > cons.cobranzas_me.id_viaje
"id_viaje" "id_viaje" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[19]   > cons.cobranzas_me.importe
"importe" "importe" ? ? "decimal" ? ? ? ? ? ? no ? no 19.8 yes
     _FldNameList[20]   > cons.cobranzas_me.importe_uss
"importe_uss" "importe_uss" ? ? "decimal" ? ? ? ? ? ? no ? no 19.8 yes
     _FldNameList[21]   > cons.cobranzas_me.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[22]   > cons.cobranzas_me.nro_comp
"nro_comp" "nro_comp" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[23]   > cons.cobranzas_me.nro_factura
"nro_factura" "nro_factura" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[24]   > cons.cobranzas_me.nro_minuta
"nro_minuta" "nro_minuta" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[25]   > cons.cobranzas_me.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? no ? no 60 yes
     _FldNameList[26]   > cons.cobranzas_me.pvta_factura
"pvta_factura" "pvta_factura" ? ? "integer" ? ? ? ? ? ? no ? no 12.8 yes
     _FldNameList[27]   > cons.cobranzas_me.saldo
"saldo" "saldo" ? ? "decimal" ? ? ? ? ? ? no ? no 19.8 yes
     _FldNameList[28]   > cons.cobranzas_me.saldo_uss
"saldo_uss" "saldo_uss" ? ? "decimal" ? ? ? ? ? ? no ? no 19.8 yes
     _FldNameList[29]   > cons.cobranzas_me.tipo_cambio
"tipo_cambio" "tipo_cambio" ? ? "decimal" ? ? ? ? ? ? no ? no 15 yes
     _FldNameList[30]   > cons.cobranzas_me.tipo_minuta
"tipo_minuta" "tipo_minuta" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[31]   > cons.cobranzas_me.vencimiento
"vencimiento" "vencimiento" ? ? "date" ? ? ? ? ? ? no ? no 12.2 yes
     _FldNameList[32]   > comercial.tipo_moneda.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[33]   > comercial.tipo_moneda.abreviatura_ingles
"abreviatura_ingles" "abreviatura_ingles" ? ? "character" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[34]   > comercial.tipo_moneda.c_fecha
"c_fecha" "c_fecha-2" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[35]   > comercial.tipo_moneda.c_hora
"c_hora" "c_hora-2" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[36]   > comercial.tipo_moneda.c_usuario
"c_usuario" "c_usuario-2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[37]   > comercial.tipo_moneda.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[38]   > comercial.tipo_moneda.descripcion_ingles
"descripcion_ingles" "descripcion_ingles" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[39]   > comercial.tipo_moneda.id_moneda
"id_moneda" "id_moneda-2" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[40]   > general.clientes.barrio
"barrio" "barrio" ? ? "character" ? ? ? ? ? ? no ? no 70 yes
     _FldNameList[41]   > general.clientes.bon_global[1]
"bon_global[1]" "bon_global1" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[42]   > general.clientes.bon_global[2]
"bon_global[2]" "bon_global2" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[43]   > general.clientes.cf_global
"cf_global" "cf_global" ? ? "decimal" ? ? ? ? ? ? no ? no 8.8 yes
     _FldNameList[44]   > general.clientes.citde
"citde" "citde" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[45]   > general.clientes.coneccion
"coneccion" "coneccion" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[46]   > general.clientes.cuit
"cuit" "cuit" ? ? "character" ? ? ? ? ? ? no ? no 13 yes
     _FldNameList[47]   > general.clientes.c_fecha
"c_fecha" "c_fecha-3" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[48]   > general.clientes.c_hora
"c_hora" "c_hora-3" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[49]   > general.clientes.c_usuario
"c_usuario" "c_usuario-3" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[50]   > general.clientes.datos_contratos
"datos_contratos" "datos_contratos" ? ? "logical" ? ? ? ? ? ? no ? no 26.4 yes
     _FldNameList[51]   > general.clientes.desp_global[1]
"desp_global[1]" "desp_global1" ? ? "decimal" ? ? ? ? ? ? no ? no 10.6 yes
     _FldNameList[52]   > general.clientes.desp_global[2]
"desp_global[2]" "desp_global2" ? ? "decimal" ? ? ? ? ? ? no ? no 10.6 yes
     _FldNameList[53]   > general.clientes.desp_global[3]
"desp_global[3]" "desp_global3" ? ? "decimal" ? ? ? ? ? ? no ? no 10.6 yes
     _FldNameList[54]   > general.clientes.des_global[1]
"des_global[1]" "des_global1" ? ? "decimal" ? ? ? ? ? ? no ? no 7.6 yes
     _FldNameList[55]   > general.clientes.des_global[2]
"des_global[2]" "des_global2" ? ? "decimal" ? ? ? ? ? ? no ? no 7.6 yes
     _FldNameList[56]   > general.clientes.des_global[3]
"des_global[3]" "des_global3" ? ? "decimal" ? ? ? ? ? ? no ? no 7.6 yes
     _FldNameList[57]   > general.clientes.dias_acreditacion
"dias_acreditacion" "dias_acreditacion" ? ? "integer" ? ? ? ? ? ? no ? no 6.2 yes
     _FldNameList[58]   > general.clientes.dias_pago
"dias_pago" "dias_pago" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[59]   > general.clientes.dia_resumen
"dia_resumen" "dia_resumen" ? ? "integer" ? ? ? ? ? ? no ? no 13.2 yes
     _FldNameList[60]   > general.clientes.domicilio
"domicilio" "domicilio" ? ? "character" ? ? ? ? ? ? no ? no 70 yes
     _FldNameList[61]   > general.clientes.domicilio-1
"domicilio-1" "domicilio-1" ? ? "character" ? ? ? ? ? ? no ? no 70 yes
     _FldNameList[62]   > general.clientes.empacador
"empacador" "empacador" ? ? "logical" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[63]   > general.clientes.estado
"estado" "estado-2" ? ? "logical" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[64]   > general.clientes.es_consignatario
"es_consignatario" "es_consignatario" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[65]   > general.clientes.fax
"fax" "fax" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[66]   > general.clientes.ganancias
"ganancias" "ganancias" ? ? "character" ? ? ? ? ? ? no ? no 11 yes
     _FldNameList[67]   > general.clientes.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 7.4 yes
     _FldNameList[68]   > general.clientes.id_cliente
"id_cliente" "id_cliente-2" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[69]   > general.clientes.id_despachante
"id_despachante" "id_despachante" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[70]   > general.clientes.id_documento
"id_documento" "id_documento-2" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[71]   > general.clientes.id_interno_contribuyente
"id_interno_contribuyente" "id_interno_contribuyente" ? ? "integer" ? ? ? ? ? ? no ? no 3.6 yes
     _FldNameList[72]   > general.clientes.id_iva_contribuyente
"id_iva_contribuyente" "id_iva_contribuyente" ? ? "integer" ? ? ? ? ? ? no ? no 5.8 yes
     _FldNameList[73]   > general.clientes.id_localidad
"id_localidad" "id_localidad" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[74]   > general.clientes.id_provincia
"id_provincia" "id_provincia" ? ? "integer" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[75]   > general.clientes.id_prov_usa
"id_prov_usa" "id_prov_usa" ? ? "integer" ? ? ? ? ? ? no ? no 9.4 yes
     _FldNameList[76]   > general.clientes.id_ret_ganancias
"id_ret_ganancias" "id_ret_ganancias" ? ? "integer" ? ? ? ? ? ? no ? no 18.6 yes
     _FldNameList[77]   > general.clientes.id_ret_ingbrutos
"id_ret_ingbrutos" "id_ret_ingbrutos" ? ? "integer" ? ? ? ? ? ? no ? no 16.8 yes
     _FldNameList[78]   > general.clientes.id_ret_iva
"id_ret_iva" "id_ret_iva" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[79]   > general.clientes.id_tipo_caratula_cliente
"id_tipo_caratula_cliente" "id_tipo_caratula_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 14 yes
     _FldNameList[80]   > general.clientes.id_tipo_caratula_despachante
"id_tipo_caratula_despachante" "id_tipo_caratula_despachante" ? ? "integer" ? ? ? ? ? ? no ? no 14.4 yes
     _FldNameList[81]   > general.clientes.id_tipo_cliente
"id_tipo_cliente" "id_tipo_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[82]   > general.clientes.id_zona
"id_zona" "id_zona" ? ? "integer" ? ? ? ? ? ? no ? no 13 yes
     _FldNameList[83]   > general.clientes.ingresos_brutos
"ingresos_brutos" "ingresos_brutos" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[84]   > general.clientes.letra_permitida
"letra_permitida" "letra_permitida" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[85]   > general.clientes.localidad
"localidad" "localidad" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[86]   > general.clientes.Mercado
"Mercado" "Mercado" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[87]   > general.clientes.mercado_externo
"mercado_externo" "mercado_externo" ? ? "logical" ? ? ? ? ? ? no ? no 9 yes
     _FldNameList[88]   > general.clientes.municipalidad
"municipalidad" "municipalidad" ? ? "character" ? ? ? ? ? ? no ? no 13 yes
     _FldNameList[89]   > general.clientes.nombre
"nombre" "nombre" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[90]   > general.clientes.observaciones
"observaciones" "observaciones-2" ? ? "character" ? ? ? ? ? ? no ? no 50 yes
     _FldNameList[91]   > general.clientes.pago_habitual
"pago_habitual" "pago_habitual" ? ? "character" ? ? ? ? ? ? no ? no 50 yes
     _FldNameList[92]   > general.clientes.pais
"pais" "pais" ? ? "character" ? ? ? ? ? ? no ? no 20 yes
     _FldNameList[93]   > general.clientes.porc_ret_3337
"porc_ret_3337" "porc_ret_3337" ? ? "decimal" ? ? ? ? ? ? no ? no 13.8 yes
     _FldNameList[94]   > general.clientes.porc_ret_ganancias
"porc_ret_ganancias" "porc_ret_ganancias" ? ? "decimal" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[95]   > general.clientes.porc_ret_ingbrutos
"porc_ret_ingbrutos" "porc_ret_ingbrutos" ? ? "decimal" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[96]   > general.clientes.porc_ret_iva
"porc_ret_iva" "porc_ret_iva" ? ? "decimal" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[97]   > general.clientes.postal
"postal" "postal" ? ? "integer" ? ? ? ? ? ? no ? no 5.8 yes
     _FldNameList[98]   > general.clientes.provincia
"provincia" "provincia" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[99]   > general.clientes.razon_social
"razon_social" "razon_social" ? ? "character" ? ? ? ? ? ? no ? no 70 yes
     _FldNameList[100]   > general.clientes.razon_social-1
"razon_social-1" "razon_social-1" ? ? "character" ? ? ? ? ? ? no ? no 70 yes
     _FldNameList[101]   > general.clientes.ret_global[1]
"ret_global[1]" "ret_global1" ? ? "decimal" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[102]   > general.clientes.ret_global[2]
"ret_global[2]" "ret_global2" ? ? "decimal" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[103]   > general.clientes.ret_global[3]
"ret_global[3]" "ret_global3" ? ? "decimal" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[104]   > general.clientes.saldo
"saldo" "saldo-2" ? ? "decimal" ? ? ? ? ? ? no ? no 11.4 yes
     _FldNameList[105]   > general.clientes.Sdo_Inicial$
"Sdo_Inicial$" "Sdo_Inicial$" ? ? "decimal" ? ? ? ? ? ? no ? no 13.2 yes
     _FldNameList[106]   > general.clientes.sdo_inicialu$
"sdo_inicialu$" "sdo_inicialu$" ? ? "decimal" ? ? ? ? ? ? no ? no 13.2 yes
     _FldNameList[107]   > general.clientes.telefono[1]
"telefono[1]" "telefono1" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[108]   > general.clientes.telefono[2]
"telefono[2]" "telefono2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[109]   > general.clientes.telefono[3]
"telefono[3]" "telefono3" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[110]   > general.clientes.vto_ret_ganancias
"vto_ret_ganancias" "vto_ret_ganancias" ? ? "date" ? ? ? ? ? ? no ? no 11.4 yes
     _FldNameList[111]   > general.clientes.vto_ret_ingbrutos
"vto_ret_ingbrutos" "vto_ret_ingbrutos" ? ? "date" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[112]   > general.clientes.vto_ret_iva
"vto_ret_iva" "vto_ret_iva" ? ? "date" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[113]   > cons.tipo_cobranza.abreviatura
"abreviatura" "abreviatura-2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[114]   > cons.tipo_cobranza.descripcion
"descripcion" "descripcion-2" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[115]   > cons.tipo_cobranza.id_tipo_cobranza
"id_tipo_cobranza" "id_tipo_cobranza-2" ? ? "integer" ? ? ? ? ? ? no ? no 13.8 yes
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
