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
&Scoped-define INTERNAL-TABLES items_factura

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  bonificacion1 bonificacion2 bon_global1 bon_global2 bultos cantidad~
 descripcion descuentos1 descuentos2 descuentos3 desde_lote desp_global1~
 desp_global2 desp_global3 des_global1 des_global2 des_global3 dto_pago1~
 dto_pago2 dto_pago3 fecha fec_cosecha finca flete gastos_clausulas~
 gastos_otros hasta_lote id_articulo id_calidad id_caract id_centro_costo~
 id_color id_cuenta_corto id_envase id_envase_impor id_finca_senasa~
 id_interno_articulos id_iva_articulos id_lote_senasa id_marca id_packing~
 id_serial id_sucursal id_tipotambor id_tipo_movsto id_variedad importe item~
 kilos letra merma nro nro_lote nro_remito pallets peso precio_venta renspa~
 retorno1 retorno2 retorno3 ret_global1 ret_global2 ret_global3 tamano~
 tipo_unidad unidades unidad_compra unidad_distribucion
&Scoped-define ENABLED-FIELDS-IN-items_factura bonificacion1 bonificacion2 ~
bon_global1 bon_global2 bultos cantidad descripcion descuentos1 descuentos2 ~
descuentos3 desde_lote desp_global1 desp_global2 desp_global3 des_global1 ~
des_global2 des_global3 dto_pago1 dto_pago2 dto_pago3 fecha fec_cosecha ~
finca flete gastos_clausulas gastos_otros hasta_lote id_articulo id_calidad ~
id_caract id_centro_costo id_color id_cuenta_corto id_envase ~
id_envase_impor id_finca_senasa id_interno_articulos id_iva_articulos ~
id_lote_senasa id_marca id_packing id_serial id_sucursal id_tipotambor ~
id_tipo_movsto id_variedad importe item kilos letra merma nro nro_lote ~
nro_remito pallets peso precio_venta renspa retorno1 retorno2 retorno3 ~
ret_global1 ret_global2 ret_global3 tamano tipo_unidad unidades ~
unidad_compra unidad_distribucion 
&Scoped-Define DATA-FIELDS  bonificacion1 bonificacion2 bon_global1 bon_global2 bultos cantidad~
 descripcion descuentos1 descuentos2 descuentos3 desde_lote desp_global1~
 desp_global2 desp_global3 des_global1 des_global2 des_global3 dto_pago1~
 dto_pago2 dto_pago3 fecha fec_cosecha finca flete gastos_clausulas~
 gastos_otros hasta_lote id_articulo id_calidad id_caract id_centro_costo~
 id_color id_cuenta_corto id_envase id_envase_impor id_finca_senasa~
 id_interno_articulos id_iva_articulos id_lote_senasa id_marca id_packing~
 id_serial id_sucursal id_tipotambor id_tipo_movsto id_variedad importe item~
 kilos letra merma nro nro_lote nro_remito pallets peso precio_venta renspa~
 retorno1 retorno2 retorno3 ret_global1 ret_global2 ret_global3 tamano~
 tipo_unidad unidades unidad_compra unidad_distribucion
&Scoped-define DATA-FIELDS-IN-items_factura bonificacion1 bonificacion2 ~
bon_global1 bon_global2 bultos cantidad descripcion descuentos1 descuentos2 ~
descuentos3 desde_lote desp_global1 desp_global2 desp_global3 des_global1 ~
des_global2 des_global3 dto_pago1 dto_pago2 dto_pago3 fecha fec_cosecha ~
finca flete gastos_clausulas gastos_otros hasta_lote id_articulo id_calidad ~
id_caract id_centro_costo id_color id_cuenta_corto id_envase ~
id_envase_impor id_finca_senasa id_interno_articulos id_iva_articulos ~
id_lote_senasa id_marca id_packing id_serial id_sucursal id_tipotambor ~
id_tipo_movsto id_variedad importe item kilos letra merma nro nro_lote ~
nro_remito pallets peso precio_venta renspa retorno1 retorno2 retorno3 ~
ret_global1 ret_global2 ret_global3 tamano tipo_unidad unidades ~
unidad_compra unidad_distribucion 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.bonificacion1 = items_factura.bonificacion[1]~
  rowObject.bonificacion2 = items_factura.bonificacion[2]~
  rowObject.bon_global1 = items_factura.bon_global[1]~
  rowObject.bon_global2 = items_factura.bon_global[2]~
  rowObject.descuentos1 = items_factura.descuentos[1]~
  rowObject.descuentos2 = items_factura.descuentos[2]~
  rowObject.descuentos3 = items_factura.descuentos[3]~
  rowObject.desp_global1 = items_factura.desp_global[1]~
  rowObject.desp_global2 = items_factura.desp_global[2]~
  rowObject.desp_global3 = items_factura.desp_global[3]~
  rowObject.des_global1 = items_factura.des_global[1]~
  rowObject.des_global2 = items_factura.des_global[2]~
  rowObject.des_global3 = items_factura.des_global[3]~
  rowObject.dto_pago1 = items_factura.dto_pago[1]~
  rowObject.dto_pago2 = items_factura.dto_pago[2]~
  rowObject.dto_pago3 = items_factura.dto_pago[3]~
  rowObject.retorno1 = items_factura.retorno[1]~
  rowObject.retorno2 = items_factura.retorno[2]~
  rowObject.retorno3 = items_factura.retorno[3]~
  rowObject.ret_global1 = items_factura.ret_global[1]~
  rowObject.ret_global2 = items_factura.ret_global[2]~
  rowObject.ret_global3 = items_factura.ret_global[3]
&Scoped-Define DATA-FIELD-DEFS "dItemsFactura.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_factura NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_factura NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_factura
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_factura


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_factura SCROLLING.
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
     _TblList          = "general.items_factura"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > general.items_factura.bonificacion[1]
"bonificacion[1]" "bonificacion1" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[2]   > general.items_factura.bonificacion[2]
"bonificacion[2]" "bonificacion2" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[3]   > general.items_factura.bon_global[1]
"bon_global[1]" "bon_global1" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[4]   > general.items_factura.bon_global[2]
"bon_global[2]" "bon_global2" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[5]   > general.items_factura.bultos
"bultos" "bultos" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[6]   > general.items_factura.cantidad
"cantidad" "cantidad" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[7]   > general.items_factura.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes
     _FldNameList[8]   > general.items_factura.descuentos[1]
"descuentos[1]" "descuentos1" ? ? "decimal" ? ? ? ? ? ? yes ? no 5.4 yes
     _FldNameList[9]   > general.items_factura.descuentos[2]
"descuentos[2]" "descuentos2" ? ? "decimal" ? ? ? ? ? ? yes ? no 5.4 yes
     _FldNameList[10]   > general.items_factura.descuentos[3]
"descuentos[3]" "descuentos3" ? ? "decimal" ? ? ? ? ? ? yes ? no 5.4 yes
     _FldNameList[11]   > general.items_factura.desde_lote
"desde_lote" "desde_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[12]   > general.items_factura.desp_global[1]
"desp_global[1]" "desp_global1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[13]   > general.items_factura.desp_global[2]
"desp_global[2]" "desp_global2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[14]   > general.items_factura.desp_global[3]
"desp_global[3]" "desp_global3" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[15]   > general.items_factura.des_global[1]
"des_global[1]" "des_global1" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[16]   > general.items_factura.des_global[2]
"des_global[2]" "des_global2" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[17]   > general.items_factura.des_global[3]
"des_global[3]" "des_global3" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[18]   > general.items_factura.dto_pago[1]
"dto_pago[1]" "dto_pago1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[19]   > general.items_factura.dto_pago[2]
"dto_pago[2]" "dto_pago2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[20]   > general.items_factura.dto_pago[3]
"dto_pago[3]" "dto_pago3" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[21]   > general.items_factura.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[22]   > general.items_factura.fec_cosecha
"fec_cosecha" "fec_cosecha" ? ? "character" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[23]   > general.items_factura.finca
"finca" "finca" ? ? "character" ? ? ? ? ? ? yes ? no 5.2 yes
     _FldNameList[24]   > general.items_factura.flete
"flete" "flete" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.2 yes
     _FldNameList[25]   > general.items_factura.gastos_clausulas
"gastos_clausulas" "gastos_clausulas" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[26]   > general.items_factura.gastos_otros
"gastos_otros" "gastos_otros" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[27]   > general.items_factura.hasta_lote
"hasta_lote" "hasta_lote" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[28]   > general.items_factura.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[29]   > general.items_factura.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[30]   > general.items_factura.id_caract
"id_caract" "id_caract" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[31]   > general.items_factura.id_centro_costo
"id_centro_costo" "id_centro_costo" ? ? "character" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[32]   > general.items_factura.id_color
"id_color" "id_color" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[33]   > general.items_factura.id_cuenta_corto
"id_cuenta_corto" "id_cuenta_corto" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 yes
     _FldNameList[34]   > general.items_factura.id_envase
"id_envase" "id_envase" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[35]   > general.items_factura.id_envase_impor
"id_envase_impor" "id_envase_impor" ? ? "integer" ? ? ? ? ? ? yes ? no 13.6 yes
     _FldNameList[36]   > general.items_factura.id_finca_senasa
"id_finca_senasa" "id_finca_senasa" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[37]   > general.items_factura.id_interno_articulos
"id_interno_articulos" "id_interno_articulos" ? ? "integer" ? ? ? ? ? ? yes ? no 3.6 yes
     _FldNameList[38]   > general.items_factura.id_iva_articulos
"id_iva_articulos" "id_iva_articulos" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes
     _FldNameList[39]   > general.items_factura.id_lote_senasa
"id_lote_senasa" "id_lote_senasa" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[40]   > general.items_factura.id_marca
"id_marca" "id_marca" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes
     _FldNameList[41]   > general.items_factura.id_packing
"id_packing" "id_packing" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[42]   > general.items_factura.id_serial
"id_serial" "id_serial" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.2 yes
     _FldNameList[43]   > general.items_factura.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[44]   > general.items_factura.id_tipotambor
"id_tipotambor" "id_tipotambor" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[45]   > general.items_factura.id_tipo_movsto
"id_tipo_movsto" "id_tipo_movsto" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[46]   > general.items_factura.id_variedad
"id_variedad" "id_variedad" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes
     _FldNameList[47]   > general.items_factura.importe
"importe" "importe" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[48]   > general.items_factura.item
"item" "item" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes
     _FldNameList[49]   > general.items_factura.kilos
"kilos" "kilos" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[50]   > general.items_factura.letra
"letra" "letra" ? ? "character" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[51]   > general.items_factura.merma
"merma" "merma" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 yes
     _FldNameList[52]   > general.items_factura.nro
"nro" "nro" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes
     _FldNameList[53]   > general.items_factura.nro_lote
"nro_lote" "nro_lote" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[54]   > general.items_factura.nro_remito
"nro_remito" "nro_remito" ? ? "character" ? ? ? ? ? ? yes ? no 13 yes
     _FldNameList[55]   > general.items_factura.pallets
"pallets" "pallets" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes
     _FldNameList[56]   > general.items_factura.peso
"peso" "peso" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[57]   > general.items_factura.precio_venta
"precio_venta" "precio_venta" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes
     _FldNameList[58]   > general.items_factura.renspa
"renspa" "renspa" ? ? "character" ? ? ? ? ? ? yes ? no 21 yes
     _FldNameList[59]   > general.items_factura.retorno[1]
"retorno[1]" "retorno1" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[60]   > general.items_factura.retorno[2]
"retorno[2]" "retorno2" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[61]   > general.items_factura.retorno[3]
"retorno[3]" "retorno3" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.6 yes
     _FldNameList[62]   > general.items_factura.ret_global[1]
"ret_global[1]" "ret_global1" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[63]   > general.items_factura.ret_global[2]
"ret_global[2]" "ret_global2" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[64]   > general.items_factura.ret_global[3]
"ret_global[3]" "ret_global3" ? ? "decimal" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[65]   > general.items_factura.tamano
"tamano" "tamano" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[66]   > general.items_factura.tipo_unidad
"tipo_unidad" "tipo_unidad" ? ? "character" ? ? ? ? ? ? yes ? no 14.2 yes
     _FldNameList[67]   > general.items_factura.unidades
"unidades" "unidades" ? ? "decimal" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[68]   > general.items_factura.unidad_compra
"unidad_compra" "unidad_compra" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes
     _FldNameList[69]   > general.items_factura.unidad_distribucion
"unidad_distribucion" "unidad_distribucion" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 yes
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

