&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          ventas           PROGRESS
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
&Scoped-define INTERNAL-TABLES subd_vtas items_venta aux_items_venta

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  bon_global1 bon_global2 cant_copias condicion cotizacion c_fecha c_hora~
 c_usuario descuento des_global1 des_global2 des_global3 des_pago estado~
 estado_subd_vtas factor fecha fecha_comp id_cliente id_codigo id_ejercicio~
 id_empresa id_moneda_base id_moneda_local id_moneda_origen id_operacion~
 id_punto_venta id_punto_venta_legal id_tipocomp id_tipo_plazo id_tipo_venta~
 importe_base importe_local Importe_neto_base importe_neto_local~
 importe_neto_origen importe_origen impreso iva iva_rni mercado nromov~
 nro_asiento nro_comp nro_comp_legal nro_proforma observaciones plazo~
 proforma saldo_base saldo_local saldo_origen transito vencimiento bultos~
 cantidad c_fecha-2 c_hora-2 c_usuario-2 descripcion descuento-2 factor-2~
 gastos gastos_sb id_articulo id_calidad id_caract id_categoria id_color~
 id_envase id_envase_impor id_interno_articulos id_iva_articulos id_marca~
 id_punto_venta-2 id_tipo_descuento id_tipo_pallet id_variedad item iva-2~
 kilos_por_envase nromov-2 peso precio_base precio_local precio_origen~
 precio_origen_sb subtotal_base subtotal_local subtotal_origen tamano~
 tipo_unidad unidades unidad_distribucion cantidad_bolsas c_fecha-3 c_hora-3~
 c_usuario-3 descripcion-2 desde_lote hasta_lote id_punto_venta-3 item-2~
 kilos_item kilos_por_bolsa nromov-3 nro_contenedor nro_lote nro_PO_cliente~
 nro_prod_cliente nro_release
&Scoped-define DATA-FIELDS-IN-subd_vtas bon_global1 bon_global2 cant_copias ~
condicion cotizacion c_fecha c_hora c_usuario descuento des_global1 ~
des_global2 des_global3 des_pago estado estado_subd_vtas factor fecha ~
fecha_comp id_cliente id_codigo id_ejercicio id_empresa id_moneda_base ~
id_moneda_local id_moneda_origen id_operacion id_punto_venta ~
id_punto_venta_legal id_tipocomp id_tipo_plazo id_tipo_venta importe_base ~
importe_local Importe_neto_base importe_neto_local importe_neto_origen ~
importe_origen impreso iva iva_rni mercado nromov nro_asiento nro_comp ~
nro_comp_legal nro_proforma observaciones plazo proforma saldo_base ~
saldo_local saldo_origen transito vencimiento 
&Scoped-define DATA-FIELDS-IN-items_venta bultos cantidad c_fecha-2 ~
c_hora-2 c_usuario-2 descripcion descuento-2 factor-2 gastos gastos_sb ~
id_articulo id_calidad id_caract id_categoria id_color id_envase ~
id_envase_impor id_interno_articulos id_iva_articulos id_marca ~
id_punto_venta-2 id_tipo_descuento id_tipo_pallet id_variedad item iva-2 ~
kilos_por_envase nromov-2 peso precio_base precio_local precio_origen ~
precio_origen_sb subtotal_base subtotal_local subtotal_origen tamano ~
tipo_unidad unidades unidad_distribucion 
&Scoped-define DATA-FIELDS-IN-aux_items_venta cantidad_bolsas c_fecha-3 ~
c_hora-3 c_usuario-3 descripcion-2 desde_lote hasta_lote id_punto_venta-3 ~
item-2 kilos_item kilos_por_bolsa nromov-3 nro_contenedor nro_lote ~
nro_PO_cliente nro_prod_cliente nro_release 
&Scoped-Define MANDATORY-FIELDS  id_moneda_base id_punto_venta importe_local importe_neto_origen~
 importe_origen nromov id_punto_venta-2 item nromov-2 precio_base~
 precio_local precio_origen precio_origen_sb subtotal_local id_punto_venta-3~
 item-2 nromov-3 nro_contenedor
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.bon_global1 = subd_vtas.bon_global[1]~
  rowObject.bon_global2 = subd_vtas.bon_global[2]~
  rowObject.des_global1 = subd_vtas.des_global[1]~
  rowObject.des_global2 = subd_vtas.des_global[2]~
  rowObject.des_global3 = subd_vtas.des_global[3]~
  rowObject.c_fecha-2 = items_venta.c_fecha~
  rowObject.c_hora-2 = items_venta.c_hora~
  rowObject.c_usuario-2 = items_venta.c_usuario~
  rowObject.descuento-2 = items_venta.descuento~
  rowObject.factor-2 = items_venta.factor~
  rowObject.id_punto_venta-2 = items_venta.id_punto_venta~
  rowObject.iva-2 = items_venta.iva  rowObject.nromov-2 = items_venta.nromov~
  rowObject.c_fecha-3 = aux_items_venta.c_fecha~
  rowObject.c_hora-3 = aux_items_venta.c_hora~
  rowObject.c_usuario-3 = aux_items_venta.c_usuario~
  rowObject.descripcion-2 = aux_items_venta.descripcion~
  rowObject.id_punto_venta-3 = aux_items_venta.id_punto_venta~
  rowObject.item-2 = aux_items_venta.item~
  rowObject.nromov-3 = aux_items_venta.nromov
&Scoped-Define DATA-FIELD-DEFS "d000itemsventapermisosembarque.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH subd_vtas NO-LOCK, ~
      EACH items_venta OF subd_vtas NO-LOCK, ~
      FIRST aux_items_venta OF subd_vtas NO-LOCK ~
    BY aux_items_venta.desde_lote ~
       BY aux_items_venta.hasta_lote INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH subd_vtas NO-LOCK, ~
      EACH items_venta OF subd_vtas NO-LOCK, ~
      FIRST aux_items_venta OF subd_vtas NO-LOCK ~
    BY aux_items_venta.desde_lote ~
       BY aux_items_venta.hasta_lote INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main subd_vtas items_venta ~
aux_items_venta
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main subd_vtas
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main items_venta
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main aux_items_venta


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      subd_vtas, 
      items_venta, 
      aux_items_venta SCROLLING.
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
     _TblList          = "ventas.subd_vtas,ventas.items_venta OF ventas.subd_vtas,ventas.aux_items_venta OF ventas.subd_vtas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ",, FIRST"
     _OrdList          = "ventas.aux_items_venta.desde_lote|yes,ventas.aux_items_venta.hasta_lote|yes"
     _FldNameList[1]   > ventas.subd_vtas.bon_global[1]
"bon_global[1]" "bon_global1" ? ? "integer" ? ? ? ? ? ? no ? no 11.4 yes
     _FldNameList[2]   > ventas.subd_vtas.bon_global[2]
"bon_global[2]" "bon_global2" ? ? "integer" ? ? ? ? ? ? no ? no 11.4 yes
     _FldNameList[3]   > ventas.subd_vtas.cant_copias
"cant_copias" "cant_copias" ? ? "integer" ? ? ? ? ? ? no ? no 11.4 yes
     _FldNameList[4]   > ventas.subd_vtas.condicion
"condicion" "condicion" ? ? "logical" ? ? ? ? ? ? no ? no 9.4 yes
     _FldNameList[5]   > ventas.subd_vtas.cotizacion
"cotizacion" "cotizacion" ? ? "decimal" ? ? ? ? ? ? no ? no 9 yes
     _FldNameList[6]   > ventas.subd_vtas.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[7]   > ventas.subd_vtas.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[8]   > ventas.subd_vtas.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[9]   > ventas.subd_vtas.descuento
"descuento" "descuento" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[10]   > ventas.subd_vtas.des_global[1]
"des_global[1]" "des_global1" ? ? "decimal" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[11]   > ventas.subd_vtas.des_global[2]
"des_global[2]" "des_global2" ? ? "decimal" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[12]   > ventas.subd_vtas.des_global[3]
"des_global[3]" "des_global3" ? ? "decimal" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[13]   > ventas.subd_vtas.des_pago
"des_pago" "des_pago" ? ? "decimal" ? ? ? ? ? ? no ? no 10.6 yes
     _FldNameList[14]   > ventas.subd_vtas.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[15]   > ventas.subd_vtas.estado_subd_vtas
"estado_subd_vtas" "estado_subd_vtas" ? ? "integer" ? ? ? ? ? ? no ? no 17.4 yes
     _FldNameList[16]   > ventas.subd_vtas.factor
"factor" "factor" ? ? "decimal" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[17]   > ventas.subd_vtas.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[18]   > ventas.subd_vtas.fecha_comp
"fecha_comp" "fecha_comp" ? ? "date" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[19]   > ventas.subd_vtas.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[20]   > ventas.subd_vtas.id_codigo
"id_codigo" "id_codigo" ? ? "integer" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[21]   > ventas.subd_vtas.id_ejercicio
"id_ejercicio" "id_ejercicio" ? ? "integer" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[22]   > ventas.subd_vtas.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[23]   > ventas.subd_vtas.id_moneda_base
"id_moneda_base" "id_moneda_base" ? ? "integer" ? ? ? ? ? ? no ? yes 9.6 yes
     _FldNameList[24]   > ventas.subd_vtas.id_moneda_local
"id_moneda_local" "id_moneda_local" ? ? "integer" ? ? ? ? ? ? no ? no 16 yes
     _FldNameList[25]   > ventas.subd_vtas.id_moneda_origen
"id_moneda_origen" "id_moneda_origen" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[26]   > ventas.subd_vtas.id_operacion
"id_operacion" "id_operacion" ? ? "integer" ? ? ? ? ? ? no ? no 3.6 yes
     _FldNameList[27]   > ventas.subd_vtas.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? no ? yes 7.6 yes
     _FldNameList[28]   > ventas.subd_vtas.id_punto_venta_legal
"id_punto_venta_legal" "id_punto_venta_legal" ? ? "integer" ? ? ? ? ? ? no ? no 20.4 yes
     _FldNameList[29]   > ventas.subd_vtas.id_tipocomp
"id_tipocomp" "id_tipocomp" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[30]   > ventas.subd_vtas.id_tipo_plazo
"id_tipo_plazo" "id_tipo_plazo" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[31]   > ventas.subd_vtas.id_tipo_venta
"id_tipo_venta" "id_tipo_venta" ? ? "integer" ? ? ? ? ? ? no ? no 10.6 yes
     _FldNameList[32]   > ventas.subd_vtas.importe_base
"importe_base" "importe_base" ? ? "decimal" ? ? ? ? ? ? no ? no 17.4 yes
     _FldNameList[33]   > ventas.subd_vtas.importe_local
"importe_local" "importe_local" ? ? "decimal" ? ? ? ? ? ? no ? yes 17.4 yes
     _FldNameList[34]   > ventas.subd_vtas.Importe_neto_base
"Importe_neto_base" "Importe_neto_base" ? ? "decimal" ? ? ? ? ? ? no ? no 17.4 yes
     _FldNameList[35]   > ventas.subd_vtas.importe_neto_local
"importe_neto_local" "importe_neto_local" ? ? "decimal" ? ? ? ? ? ? no ? no 17.4 yes
     _FldNameList[36]   > ventas.subd_vtas.importe_neto_origen
"importe_neto_origen" "importe_neto_origen" ? ? "decimal" ? ? ? ? ? ? no ? yes 17.4 yes
     _FldNameList[37]   > ventas.subd_vtas.importe_origen
"importe_origen" "importe_origen" ? ? "decimal" ? ? ? ? ? ? no ? yes 17.4 yes
     _FldNameList[38]   > ventas.subd_vtas.impreso
"impreso" "impreso" ? ? "logical" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[39]   > ventas.subd_vtas.iva
"iva" "iva" ? ? "decimal" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[40]   > ventas.subd_vtas.iva_rni
"iva_rni" "iva_rni" ? ? "decimal" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[41]   > ventas.subd_vtas.mercado
"mercado" "mercado" ? ? "logical" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[42]   > ventas.subd_vtas.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? no ? yes 10.2 yes
     _FldNameList[43]   > ventas.subd_vtas.nro_asiento
"nro_asiento" "nro_asiento" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[44]   > ventas.subd_vtas.nro_comp
"nro_comp" "nro_comp" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[45]   > ventas.subd_vtas.nro_comp_legal
"nro_comp_legal" "nro_comp_legal" ? ? "integer" ? ? ? ? ? ? no ? no 15 yes
     _FldNameList[46]   > ventas.subd_vtas.nro_proforma
"nro_proforma" "nro_proforma" ? ? "integer" ? ? ? ? ? ? no ? no 12.4 yes
     _FldNameList[47]   > ventas.subd_vtas.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? no ? no 57 yes
     _FldNameList[48]   > ventas.subd_vtas.plazo
"plazo" "plazo" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[49]   > ventas.subd_vtas.proforma
"proforma" "proforma" ? ? "logical" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[50]   > ventas.subd_vtas.saldo_base
"saldo_base" "saldo_base" ? ? "decimal" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[51]   > ventas.subd_vtas.saldo_local
"saldo_local" "saldo_local" ? ? "decimal" ? ? ? ? ? ? no ? no 10.6 yes
     _FldNameList[52]   > ventas.subd_vtas.saldo_origen
"saldo_origen" "saldo_origen" ? ? "decimal" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[53]   > ventas.subd_vtas.transito
"transito" "transito" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[54]   > ventas.subd_vtas.vencimiento
"vencimiento" "vencimiento" ? ? "date" ? ? ? ? ? ? no ? no 12.2 yes
     _FldNameList[55]   > ventas.items_venta.bultos
"bultos" "bultos" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[56]   > ventas.items_venta.cantidad
"cantidad" "cantidad" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[57]   > ventas.items_venta.c_fecha
"c_fecha" "c_fecha-2" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[58]   > ventas.items_venta.c_hora
"c_hora" "c_hora-2" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[59]   > ventas.items_venta.c_usuario
"c_usuario" "c_usuario-2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[60]   > ventas.items_venta.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? no ? no 50 yes
     _FldNameList[61]   > ventas.items_venta.descuento
"descuento" "descuento-2" ? ? "decimal" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[62]   > ventas.items_venta.factor
"factor" "factor-2" ? ? "decimal" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[63]   > ventas.items_venta.gastos
"gastos" "gastos" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[64]   > ventas.items_venta.gastos_sb
"gastos_sb" "gastos_sb" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[65]   > ventas.items_venta.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[66]   > ventas.items_venta.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[67]   > ventas.items_venta.id_caract
"id_caract" "id_caract" ? ? "integer" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[68]   > ventas.items_venta.id_categoria
"id_categoria" "id_categoria" ? ? "integer" ? ? ? ? ? ? no ? no 3.8 yes
     _FldNameList[69]   > ventas.items_venta.id_color
"id_color" "id_color" ? ? "integer" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[70]   > ventas.items_venta.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[71]   > ventas.items_venta.id_envase_impor
"id_envase_impor" "id_envase_impor" ? ? "integer" ? ? ? ? ? ? no ? no 13.6 yes
     _FldNameList[72]   > ventas.items_venta.id_interno_articulos
"id_interno_articulos" "id_interno_articulos" ? ? "integer" ? ? ? ? ? ? no ? no 3.6 yes
     _FldNameList[73]   > ventas.items_venta.id_iva_articulos
"id_iva_articulos" "id_iva_articulos" ? ? "integer" ? ? ? ? ? ? no ? no 5.8 yes
     _FldNameList[74]   > ventas.items_venta.id_marca
"id_marca" "id_marca" ? ? "integer" ? ? ? ? ? ? no ? no 6 yes
     _FldNameList[75]   > ventas.items_venta.id_punto_venta
"id_punto_venta" "id_punto_venta-2" ? ? "integer" ? ? ? ? ? ? no ? yes 7.6 yes
     _FldNameList[76]   > ventas.items_venta.id_tipo_descuento
"id_tipo_descuento" "id_tipo_descuento" ? ? "integer" ? ? ? ? ? ? no ? no 3.6 yes
     _FldNameList[77]   > ventas.items_venta.id_tipo_pallet
"id_tipo_pallet" "id_tipo_pallet" ? ? "integer" ? ? ? ? ? ? no ? no 12.4 yes
     _FldNameList[78]   > ventas.items_venta.id_variedad
"id_variedad" "id_variedad" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[79]   > ventas.items_venta.item
"item" "item" ? ? "integer" ? ? ? ? ? ? no ? yes 4.8 yes
     _FldNameList[80]   > ventas.items_venta.iva
"iva" "iva-2" ? ? "decimal" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[81]   > ventas.items_venta.kilos_por_envase
"kilos_por_envase" "kilos_por_envase" ? ? "decimal" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[82]   > ventas.items_venta.nromov
"nromov" "nromov-2" ? ? "integer" ? ? ? ? ? ? no ? yes 10.2 yes
     _FldNameList[83]   > ventas.items_venta.peso
"peso" "peso" ? ? "decimal" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[84]   > ventas.items_venta.precio_base
"precio_base" "precio_base" ? ? "decimal" ? ? ? ? ? ? no ? yes 17.4 yes
     _FldNameList[85]   > ventas.items_venta.precio_local
"precio_local" "precio_local" ? ? "decimal" ? ? ? ? ? ? no ? yes 17.4 yes
     _FldNameList[86]   > ventas.items_venta.precio_origen
"precio_origen" "precio_origen" ? ? "decimal" ? ? ? ? ? ? no ? yes 24.6 yes
     _FldNameList[87]   > ventas.items_venta.precio_origen_sb
"precio_origen_sb" "precio_origen_sb" ? ? "decimal" ? ? ? ? ? ? no ? yes 24.6 yes
     _FldNameList[88]   > ventas.items_venta.subtotal_base
"subtotal_base" "subtotal_base" ? ? "decimal" ? ? ? ? ? ? no ? no 15.6 yes
     _FldNameList[89]   > ventas.items_venta.subtotal_local
"subtotal_local" "subtotal_local" ? ? "decimal" ? ? ? ? ? ? no ? yes 17.4 yes
     _FldNameList[90]   > ventas.items_venta.subtotal_origen
"subtotal_origen" "subtotal_origen" ? ? "decimal" ? ? ? ? ? ? no ? no 15.6 yes
     _FldNameList[91]   > ventas.items_venta.tamano
"tamano" "tamano" ? ? "integer" ? ? ? ? ? ? no ? no 4.8 yes
     _FldNameList[92]   > ventas.items_venta.tipo_unidad
"tipo_unidad" "tipo_unidad" ? ? "character" ? ? ? ? ? ? no ? no 14.2 yes
     _FldNameList[93]   > ventas.items_venta.unidades
"unidades" "unidades" ? ? "decimal" ? ? ? ? ? ? no ? no 9 yes
     _FldNameList[94]   > ventas.items_venta.unidad_distribucion
"unidad_distribucion" "unidad_distribucion" ? ? "integer" ? ? ? ? ? ? no ? no 9.8 yes
     _FldNameList[95]   > ventas.aux_items_venta.cantidad_bolsas
"cantidad_bolsas" "cantidad_bolsas" ? ? "integer" ? ? ? ? ? ? no ? no 11.2 yes
     _FldNameList[96]   > ventas.aux_items_venta.c_fecha
"c_fecha" "c_fecha-3" ? ? "date" ? ? ? ? ? ? no ? no 11.6 yes
     _FldNameList[97]   > ventas.aux_items_venta.c_hora
"c_hora" "c_hora-3" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[98]   > ventas.aux_items_venta.c_usuario
"c_usuario" "c_usuario-3" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[99]   > ventas.aux_items_venta.descripcion
"descripcion" "descripcion-2" ? ? "character" ? ? ? ? ? ? no ? no 120 yes
     _FldNameList[100]   > ventas.aux_items_venta.desde_lote
"desde_lote" "desde_lote" ? ? "integer" ? ? ? ? ? ? no ? no 11 yes
     _FldNameList[101]   > ventas.aux_items_venta.hasta_lote
"hasta_lote" "hasta_lote" ? ? "integer" ? ? ? ? ? ? no ? no 10.4 yes
     _FldNameList[102]   > ventas.aux_items_venta.id_punto_venta
"id_punto_venta" "id_punto_venta-3" ? ? "integer" ? ? ? ? ? ? no ? yes 7.6 yes
     _FldNameList[103]   > ventas.aux_items_venta.item
"item" "item-2" ? ? "integer" ? ? ? ? ? ? no ? yes 4.8 yes
     _FldNameList[104]   > ventas.aux_items_venta.kilos_item
"kilos_item" "kilos_item" ? ? "decimal" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[105]   > ventas.aux_items_venta.kilos_por_bolsa
"kilos_por_bolsa" "kilos_por_bolsa" ? ? "decimal" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[106]   > ventas.aux_items_venta.nromov
"nromov" "nromov-3" ? ? "integer" ? ? ? ? ? ? no ? yes 10.2 yes
     _FldNameList[107]   > ventas.aux_items_venta.nro_contenedor
"nro_contenedor" "nro_contenedor" ? ? "character" ? ? ? ? ? ? no ? yes 18 yes
     _FldNameList[108]   > ventas.aux_items_venta.nro_lote
"nro_lote" "nro_lote" ? ? "character" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[109]   > ventas.aux_items_venta.nro_PO_cliente
"nro_PO_cliente" "nro_PO_cliente" ? ? "character" ? ? ? ? ? ? no ? no 16.4 yes
     _FldNameList[110]   > ventas.aux_items_venta.nro_prod_cliente
"nro_prod_cliente" "nro_prod_cliente" ? ? "character" ? ? ? ? ? ? no ? no 26.4 yes
     _FldNameList[111]   > ventas.aux_items_venta.nro_release
"nro_release" "nro_release" ? ? "character" ? ? ? ? ? ? no ? no 15 yes
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

