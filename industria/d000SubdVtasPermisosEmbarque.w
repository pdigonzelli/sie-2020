&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          ventas           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asbroker1                AS HANDLE          NO-UNDO.
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
&Scoped-define INTERNAL-TABLES r_subd_ventas_embarque subd_vtas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio_permiso id_aduana id_punto_venta nromov nro_embarque bon_global1~
 bon_global2 cant_copias condicion cotizacion c_fecha c_hora c_usuario~
 descuento des_global1 des_global2 des_global3 des_pago estado~
 estado_subd_vtas factor fecha fecha_comp id_cliente id_codigo id_ejercicio~
 id_empresa id_moneda_base id_moneda_local id_moneda_origen id_operacion~
 id_punto_venta-2 id_punto_venta_legal id_tipocomp id_tipo_plazo~
 id_tipo_venta importe_base importe_local Importe_neto_base~
 importe_neto_local importe_neto_origen importe_origen impreso iva iva_rni~
 mercado nromov-2 nro_asiento nro_comp nro_comp_legal nro_proforma~
 observaciones plazo proforma saldo_base saldo_local saldo_origen transito~
 vencimiento
&Scoped-define ENABLED-FIELDS-IN-r_subd_ventas_embarque anio_permiso ~
id_aduana id_punto_venta nromov nro_embarque 
&Scoped-define ENABLED-FIELDS-IN-subd_vtas bon_global1 bon_global2 ~
cant_copias condicion cotizacion c_fecha c_hora c_usuario descuento ~
des_global1 des_global2 des_global3 des_pago estado estado_subd_vtas factor ~
fecha fecha_comp id_cliente id_codigo id_ejercicio id_empresa ~
id_moneda_base id_moneda_local id_moneda_origen id_operacion ~
id_punto_venta-2 id_punto_venta_legal id_tipocomp id_tipo_plazo ~
id_tipo_venta importe_base importe_local Importe_neto_base ~
importe_neto_local importe_neto_origen importe_origen impreso iva iva_rni ~
mercado nromov-2 nro_asiento nro_comp nro_comp_legal nro_proforma ~
observaciones plazo proforma saldo_base saldo_local saldo_origen transito ~
vencimiento 
&Scoped-Define DATA-FIELDS  anio_permiso id_aduana id_punto_venta nromov nro_embarque bon_global1~
 bon_global2 cant_copias condicion cotizacion c_fecha c_hora c_usuario~
 descuento des_global1 des_global2 des_global3 des_pago estado~
 estado_subd_vtas factor fecha fecha_comp id_cliente id_codigo id_ejercicio~
 id_empresa id_moneda_base id_moneda_local id_moneda_origen id_operacion~
 id_punto_venta-2 id_punto_venta_legal id_tipocomp id_tipo_plazo~
 id_tipo_venta importe_base importe_local Importe_neto_base~
 importe_neto_local importe_neto_origen importe_origen impreso iva iva_rni~
 mercado nromov-2 nro_asiento nro_comp nro_comp_legal nro_proforma~
 observaciones plazo proforma saldo_base saldo_local saldo_origen transito~
 vencimiento
&Scoped-define DATA-FIELDS-IN-r_subd_ventas_embarque anio_permiso id_aduana ~
id_punto_venta nromov nro_embarque 
&Scoped-define DATA-FIELDS-IN-subd_vtas bon_global1 bon_global2 cant_copias ~
condicion cotizacion c_fecha c_hora c_usuario descuento des_global1 ~
des_global2 des_global3 des_pago estado estado_subd_vtas factor fecha ~
fecha_comp id_cliente id_codigo id_ejercicio id_empresa id_moneda_base ~
id_moneda_local id_moneda_origen id_operacion id_punto_venta-2 ~
id_punto_venta_legal id_tipocomp id_tipo_plazo id_tipo_venta importe_base ~
importe_local Importe_neto_base importe_neto_local importe_neto_origen ~
importe_origen impreso iva iva_rni mercado nromov-2 nro_asiento nro_comp ~
nro_comp_legal nro_proforma observaciones plazo proforma saldo_base ~
saldo_local saldo_origen transito vencimiento 
&Scoped-Define MANDATORY-FIELDS  id_punto_venta nromov nro_embarque id_moneda_base id_punto_venta-2~
 importe_local importe_neto_origen importe_origen nromov-2
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.bon_global1 = subd_vtas.bon_global[1]~
  rowObject.bon_global2 = subd_vtas.bon_global[2]~
  rowObject.des_global1 = subd_vtas.des_global[1]~
  rowObject.des_global2 = subd_vtas.des_global[2]~
  rowObject.des_global3 = subd_vtas.des_global[3]~
  rowObject.id_punto_venta-2 = subd_vtas.id_punto_venta~
  rowObject.nromov-2 = subd_vtas.nromov
&Scoped-Define DATA-FIELD-DEFS "d000subdvtaspermisosembarque.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH r_subd_ventas_embarque ~
      WHERE length(ventas.r_subd_ventas_embarque.nro_embarque) <> 11 NO-LOCK, ~
      EACH subd_vtas OF r_subd_ventas_embarque NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH r_subd_ventas_embarque ~
      WHERE length(ventas.r_subd_ventas_embarque.nro_embarque) <> 11 NO-LOCK, ~
      EACH subd_vtas OF r_subd_ventas_embarque NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main r_subd_ventas_embarque subd_vtas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main r_subd_ventas_embarque
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main subd_vtas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      r_subd_ventas_embarque, 
      subd_vtas SCROLLING.
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
     _TblList          = "ventas.r_subd_ventas_embarque,ventas.subd_vtas OF ventas.r_subd_ventas_embarque"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "length(ventas.r_subd_ventas_embarque.nro_embarque) <> 11"
     _FldNameList[1]   > ventas.r_subd_ventas_embarque.anio_permiso
"anio_permiso" "anio_permiso" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes
     _FldNameList[2]   > ventas.r_subd_ventas_embarque.id_aduana
"id_aduana" "id_aduana" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[3]   > ventas.r_subd_ventas_embarque.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.6 yes
     _FldNameList[4]   > ventas.r_subd_ventas_embarque.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[5]   > ventas.r_subd_ventas_embarque.nro_embarque
"nro_embarque" "nro_embarque" ? ? "character" ? ? ? ? ? ? yes ? yes 18 yes
     _FldNameList[6]   > ventas.subd_vtas.bon_global[1]
"bon_global[1]" "bon_global1" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[7]   > ventas.subd_vtas.bon_global[2]
"bon_global[2]" "bon_global2" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[8]   > ventas.subd_vtas.cant_copias
"cant_copias" "cant_copias" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[9]   > ventas.subd_vtas.condicion
"condicion" "condicion" ? ? "logical" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[10]   > ventas.subd_vtas.cotizacion
"cotizacion" "cotizacion" ? ? "decimal" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[11]   > ventas.subd_vtas.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[12]   > ventas.subd_vtas.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[13]   > ventas.subd_vtas.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[14]   > ventas.subd_vtas.descuento
"descuento" "descuento" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[15]   > ventas.subd_vtas.des_global[1]
"des_global[1]" "des_global1" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[16]   > ventas.subd_vtas.des_global[2]
"des_global[2]" "des_global2" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[17]   > ventas.subd_vtas.des_global[3]
"des_global[3]" "des_global3" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[18]   > ventas.subd_vtas.des_pago
"des_pago" "des_pago" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[19]   > ventas.subd_vtas.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[20]   > ventas.subd_vtas.estado_subd_vtas
"estado_subd_vtas" "estado_subd_vtas" ? ? "integer" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[21]   > ventas.subd_vtas.factor
"factor" "factor" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[22]   > ventas.subd_vtas.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[23]   > ventas.subd_vtas.fecha_comp
"fecha_comp" "fecha_comp" ? ? "date" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[24]   > ventas.subd_vtas.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[25]   > ventas.subd_vtas.id_codigo
"id_codigo" "id_codigo" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[26]   > ventas.subd_vtas.id_ejercicio
"id_ejercicio" "id_ejercicio" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[27]   > ventas.subd_vtas.id_empresa
"id_empresa" "id_empresa" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[28]   > ventas.subd_vtas.id_moneda_base
"id_moneda_base" "id_moneda_base" ? ? "integer" ? ? ? ? ? ? yes ? yes 9.6 yes
     _FldNameList[29]   > ventas.subd_vtas.id_moneda_local
"id_moneda_local" "id_moneda_local" ? ? "integer" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[30]   > ventas.subd_vtas.id_moneda_origen
"id_moneda_origen" "id_moneda_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[31]   > ventas.subd_vtas.id_operacion
"id_operacion" "id_operacion" ? ? "integer" ? ? ? ? ? ? yes ? no 3.6 yes
     _FldNameList[32]   > ventas.subd_vtas.id_punto_venta
"id_punto_venta" "id_punto_venta-2" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.6 yes
     _FldNameList[33]   > ventas.subd_vtas.id_punto_venta_legal
"id_punto_venta_legal" "id_punto_venta_legal" ? ? "integer" ? ? ? ? ? ? yes ? no 20.4 yes
     _FldNameList[34]   > ventas.subd_vtas.id_tipocomp
"id_tipocomp" "id_tipocomp" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[35]   > ventas.subd_vtas.id_tipo_plazo
"id_tipo_plazo" "id_tipo_plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[36]   > ventas.subd_vtas.id_tipo_venta
"id_tipo_venta" "id_tipo_venta" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[37]   > ventas.subd_vtas.importe_base
"importe_base" "importe_base" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[38]   > ventas.subd_vtas.importe_local
"importe_local" "importe_local" ? ? "decimal" ? ? ? ? ? ? yes ? yes 17.4 yes
     _FldNameList[39]   > ventas.subd_vtas.Importe_neto_base
"Importe_neto_base" "Importe_neto_base" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[40]   > ventas.subd_vtas.importe_neto_local
"importe_neto_local" "importe_neto_local" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes
     _FldNameList[41]   > ventas.subd_vtas.importe_neto_origen
"importe_neto_origen" "importe_neto_origen" ? ? "decimal" ? ? ? ? ? ? yes ? yes 17.4 yes
     _FldNameList[42]   > ventas.subd_vtas.importe_origen
"importe_origen" "importe_origen" ? ? "decimal" ? ? ? ? ? ? yes ? yes 17.4 yes
     _FldNameList[43]   > ventas.subd_vtas.impreso
"impreso" "impreso" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[44]   > ventas.subd_vtas.iva
"iva" "iva" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[45]   > ventas.subd_vtas.iva_rni
"iva_rni" "iva_rni" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[46]   > ventas.subd_vtas.mercado
"mercado" "mercado" ? ? "logical" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[47]   > ventas.subd_vtas.nromov
"nromov" "nromov-2" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[48]   > ventas.subd_vtas.nro_asiento
"nro_asiento" "nro_asiento" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[49]   > ventas.subd_vtas.nro_comp
"nro_comp" "nro_comp" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[50]   > ventas.subd_vtas.nro_comp_legal
"nro_comp_legal" "nro_comp_legal" ? ? "integer" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[51]   > ventas.subd_vtas.nro_proforma
"nro_proforma" "nro_proforma" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes
     _FldNameList[52]   > ventas.subd_vtas.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 57 yes
     _FldNameList[53]   > ventas.subd_vtas.plazo
"plazo" "plazo" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[54]   > ventas.subd_vtas.proforma
"proforma" "proforma" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[55]   > ventas.subd_vtas.saldo_base
"saldo_base" "saldo_base" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[56]   > ventas.subd_vtas.saldo_local
"saldo_local" "saldo_local" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[57]   > ventas.subd_vtas.saldo_origen
"saldo_origen" "saldo_origen" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[58]   > ventas.subd_vtas.transito
"transito" "transito" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[59]   > ventas.subd_vtas.vencimiento
"vencimiento" "vencimiento" ? ? "date" ? ? ? ? ? ? yes ? no 12.2 yes
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

