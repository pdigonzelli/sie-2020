&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
          general         PROGRESS
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
&Scoped-define INTERNAL-TABLES items_release_delivery items_contratos ~
contratos release_delivery clientes clausulas contactos_industria ~
productos_terminados destinos tipos_plazo tipo_unidad_venta calidades ~
items_orden_entrega tipo_moneda

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  calculo_cfr calculo_cif calculo_ddp calculo_fob calculo_fob_comision~
 observaciones numero_release id_lote_deposito id_sucursal_ubicacion
&Scoped-define ENABLED-FIELDS-IN-items_release_delivery calculo_cfr ~
calculo_cif calculo_ddp calculo_fob calculo_fob_comision observaciones ~
id_lote_deposito id_sucursal_ubicacion 
&Scoped-define ENABLED-FIELDS-IN-release_delivery numero_release 
&Scoped-Define DATA-FIELDS  id_contrato fecha_entrega id_cliente id_broker id_articulo id_calidad~
 id_destino id_puerto_ent id_clausula id_cond_pago_cliente comision_broker~
 cantidad tambores nombre descripcion nombre-2 descripcion-2 descripcion-3~
 descripcion-4 descripcion-5 descripcion-6 id_release_delivery~
 item_release_delivery anio item id_tipo_unidad_venta_origen id_lote~
 calculo_cfr calculo_cif calculo_ddp calculo_fob calculo_fob_comision~
 item_contrato numero_release1 plazo id_moneda_origen kilos_netos~
 kilos_brutos observaciones descripcion-7 id_tipo_contrato numero_release~
 id_lote_deposito id_sucursal_ubicacion
&Scoped-define DATA-FIELDS-IN-items_release_delivery id_contrato cantidad ~
tambores id_release_delivery item_release_delivery id_lote calculo_cfr ~
calculo_cif calculo_ddp calculo_fob calculo_fob_comision item_contrato ~
kilos_netos kilos_brutos observaciones id_lote_deposito ~
id_sucursal_ubicacion 
&Scoped-define DATA-FIELDS-IN-items_contratos id_destino id_puerto_ent ~
id_clausula id_cond_pago_cliente comision_broker anio item ~
id_tipo_unidad_venta_origen numero_release1 id_moneda_origen ~
id_tipo_contrato 
&Scoped-define DATA-FIELDS-IN-contratos id_broker plazo 
&Scoped-define DATA-FIELDS-IN-release_delivery fecha_entrega id_cliente ~
id_articulo id_calidad numero_release 
&Scoped-define DATA-FIELDS-IN-clientes nombre 
&Scoped-define DATA-FIELDS-IN-clausulas descripcion 
&Scoped-define DATA-FIELDS-IN-contactos_industria nombre-2 
&Scoped-define DATA-FIELDS-IN-productos_terminados descripcion-2 
&Scoped-define DATA-FIELDS-IN-destinos descripcion-3 
&Scoped-define DATA-FIELDS-IN-tipos_plazo descripcion-4 
&Scoped-define DATA-FIELDS-IN-tipo_unidad_venta descripcion-5 
&Scoped-define DATA-FIELDS-IN-calidades descripcion-6 
&Scoped-define DATA-FIELDS-IN-tipo_moneda descripcion-7 
&Scoped-Define MANDATORY-FIELDS  id_sucursal_ubicacion
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.nombre-2 = contactos_industria.nombre~
  rowObject.descripcion-2 = productos_terminados.descripcion~
  rowObject.descripcion-3 = destinos.descripcion~
  rowObject.descripcion-4 = tipos_plazo.descripcion~
  rowObject.descripcion-5 = tipo_unidad_venta.descripcion~
  rowObject.descripcion-6 = calidades.descripcion~
  rowObject.numero_release1 = items_contratos.numero_release[1]~
  rowObject.descripcion-7 = tipo_moneda.descripcion
&Scoped-Define DATA-FIELD-DEFS "ditemsreleasedeliverygastos.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH items_release_delivery NO-LOCK, ~
      EACH items_contratos WHERE items_contratos.id_contrato = items_release_delivery.id_contrato ~
  AND items_contratos.id_tipo_contrato = items_release_delivery.id_tipo_contrato ~
  AND items_contratos.anio = items_release_delivery.anio_contrato ~
  AND items_contratos.item = items_release_delivery.item_contrato OUTER-JOIN NO-LOCK, ~
      EACH contratos OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH release_delivery OF items_release_delivery OUTER-JOIN NO-LOCK, ~
      EACH clientes OF release_delivery OUTER-JOIN NO-LOCK, ~
      EACH clausulas OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH contactos_industria WHERE contactos_industria.id_contacto = contratos.id_broker OUTER-JOIN NO-LOCK, ~
      EACH productos_terminados OF release_delivery OUTER-JOIN NO-LOCK, ~
      EACH destinos OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH tipos_plazo OF contratos OUTER-JOIN NO-LOCK, ~
      EACH tipo_unidad_venta OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH calidades OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH items_orden_entrega OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH tipo_moneda OF items_orden_entrega OUTER-JOIN NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH items_release_delivery NO-LOCK, ~
      EACH items_contratos WHERE items_contratos.id_contrato = items_release_delivery.id_contrato ~
  AND items_contratos.id_tipo_contrato = items_release_delivery.id_tipo_contrato ~
  AND items_contratos.anio = items_release_delivery.anio_contrato ~
  AND items_contratos.item = items_release_delivery.item_contrato OUTER-JOIN NO-LOCK, ~
      EACH contratos OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH release_delivery OF items_release_delivery OUTER-JOIN NO-LOCK, ~
      EACH clientes OF release_delivery OUTER-JOIN NO-LOCK, ~
      EACH clausulas OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH contactos_industria WHERE contactos_industria.id_contacto = contratos.id_broker OUTER-JOIN NO-LOCK, ~
      EACH productos_terminados OF release_delivery OUTER-JOIN NO-LOCK, ~
      EACH destinos OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH tipos_plazo OF contratos OUTER-JOIN NO-LOCK, ~
      EACH tipo_unidad_venta OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH calidades OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH items_orden_entrega OF items_contratos OUTER-JOIN NO-LOCK, ~
      EACH tipo_moneda OF items_orden_entrega OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main items_release_delivery ~
items_contratos contratos release_delivery clientes clausulas ~
contactos_industria productos_terminados destinos tipos_plazo ~
tipo_unidad_venta calidades items_orden_entrega tipo_moneda
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main items_release_delivery
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main items_contratos
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main contratos
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main release_delivery
&Scoped-define FIFTH-TABLE-IN-QUERY-Query-Main clientes
&Scoped-define SIXTH-TABLE-IN-QUERY-Query-Main clausulas
&Scoped-define SEVENTH-TABLE-IN-QUERY-Query-Main contactos_industria
&Scoped-define EIGHTH-TABLE-IN-QUERY-Query-Main productos_terminados
&Scoped-define NINTH-TABLE-IN-QUERY-Query-Main destinos
&Scoped-define TENTH-TABLE-IN-QUERY-Query-Main tipos_plazo
&Scoped-define ELEVENTH-TABLE-IN-QUERY-Query-Main tipo_unidad_venta
&Scoped-define TWELFTH-TABLE-IN-QUERY-Query-Main calidades
&Scoped-define THIRTEENTH-TABLE-IN-QUERY-Query-Main items_orden_entrega
&Scoped-define FOURTEENTH-TABLE-IN-QUERY-Query-Main tipo_moneda


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cargo-lote dTables  _DB-REQUIRED
FUNCTION cargo-lote RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cargo-proforma dTables  _DB-REQUIRED
FUNCTION cargo-proforma RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      items_release_delivery, 
      items_contratos, 
      contratos, 
      release_delivery, 
      clientes, 
      clausulas, 
      contactos_industria, 
      productos_terminados, 
      destinos, 
      tipos_plazo, 
      tipo_unidad_venta, 
      calidades, 
      items_orden_entrega, 
      tipo_moneda SCROLLING.
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
     _TblList          = "general.items_release_delivery,general.items_contratos WHERE general.items_release_delivery ...,industria.contratos OF general.items_contratos,industria.release_delivery OF general.items_release_delivery,general.clientes OF general.release_delivery,general.clausulas OF general.items_contratos,industria.contactos_industria WHERE general.contratos ...,general.productos_terminados OF general.release_delivery,comercial.destinos OF general.items_contratos,industria.tipos_plazo OF general.contratos,industria.tipo_unidad_venta OF general.items_contratos,general.calidades OF general.items_contratos,general.items_orden_entrega OF general.items_contratos,comercial.tipo_moneda OF general.items_orden_entrega"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, OUTER, FIRST OUTER"
     _JoinCode[2]      = "general.items_contratos.id_contrato = general.items_release_delivery.id_contrato
  AND general.items_contratos.id_tipo_contrato = general.items_release_delivery.id_tipo_contrato
  AND general.items_contratos.anio = general.items_release_delivery.anio_contrato
  AND general.items_contratos.item = general.items_release_delivery.item_contrato"
     _JoinCode[7]      = "general.contactos_industria.id_contacto = general.contratos.id_broker"
     _FldNameList[1]   > general.items_release_delivery.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[2]   > general.release_delivery.fecha_entrega
"fecha_entrega" "fecha_entrega" ? ? "date" ? ? ? ? ? ? no ? no 14 yes
     _FldNameList[3]   > general.release_delivery.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[4]   > general.contratos.id_broker
"id_broker" "id_broker" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[5]   > general.release_delivery.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[6]   > general.release_delivery.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[7]   > general.items_contratos.id_destino
"id_destino" "id_destino" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[8]   > general.items_contratos.id_puerto_ent
"id_puerto_ent" "id_puerto_ent" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[9]   > general.items_contratos.id_clausula
"id_clausula" "id_clausula" ? ? "integer" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[10]   > general.items_contratos.id_cond_pago_cliente
"id_cond_pago_cliente" "id_cond_pago_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[11]   > general.items_contratos.comision_broker
"comision_broker" "comision_broker" ? ? "decimal" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[12]   > general.items_release_delivery.cantidad
"cantidad" "cantidad" ? ? "integer" ? ? ? ? ? ? no ? no 8.4 yes
     _FldNameList[13]   > general.items_release_delivery.tambores
"tambores" "tambores" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[14]   > general.clientes.nombre
"nombre" "nombre" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[15]   > general.clausulas.descripcion
"descripcion" "descripcion" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[16]   > general.contactos_industria.nombre
"nombre" "nombre-2" ? ? "character" ? ? ? ? ? ? no ? no 40 yes
     _FldNameList[17]   > general.productos_terminados.descripcion
"descripcion" "descripcion-2" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[18]   > comercial.destinos.descripcion
"descripcion" "descripcion-3" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[19]   > general.tipos_plazo.descripcion
"descripcion" "descripcion-4" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[20]   > general.tipo_unidad_venta.descripcion
"descripcion" "descripcion-5" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[21]   > general.calidades.descripcion
"descripcion" "descripcion-6" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[22]   > general.items_release_delivery.id_release_delivery
"id_release_delivery" "id_release_delivery" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[23]   > general.items_release_delivery.item_release_delivery
"item_release_delivery" "item_release_delivery" ? ? "integer" ? ? ? ? ? ? no ? no 20.2 yes
     _FldNameList[24]   > general.items_contratos.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? no ? no 4.8 yes
     _FldNameList[25]   > general.items_contratos.item
"item" "item" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[26]   > general.items_contratos.id_tipo_unidad_venta_origen
"id_tipo_unidad_venta_origen" "id_tipo_unidad_venta_origen" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[27]   > general.items_release_delivery.id_lote
"id_lote" "id_lote" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[28]   > general.items_release_delivery.calculo_cfr
"calculo_cfr" "calculo_cfr" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[29]   > general.items_release_delivery.calculo_cif
"calculo_cif" "calculo_cif" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[30]   > general.items_release_delivery.calculo_ddp
"calculo_ddp" "calculo_ddp" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[31]   > general.items_release_delivery.calculo_fob
"calculo_fob" "calculo_fob" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[32]   > general.items_release_delivery.calculo_fob_comision
"calculo_fob_comision" "calculo_fob_comision" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[33]   > general.items_release_delivery.item_contrato
"item_contrato" "item_contrato" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[34]   > general.items_contratos.numero_release[1]
"numero_release[1]" "numero_release1" ? ? "character" ? ? ? ? ? ? no ? no 40 yes
     _FldNameList[35]   > general.contratos.plazo
"plazo" "plazo" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[36]   > general.items_contratos.id_moneda_origen
"id_moneda_origen" "id_moneda_origen" ? ? "integer" ? ? ? ? ? ? no ? no 10.6 yes
     _FldNameList[37]   > general.items_release_delivery.kilos_netos
"kilos_netos" "kilos_netos" ? ? "decimal" ? ? ? ? ? ? no ? no 13.8 yes
     _FldNameList[38]   > general.items_release_delivery.kilos_brutos
"kilos_brutos" "kilos_brutos" ? ? "decimal" ? ? ? ? ? ? no ? no 13.8 yes
     _FldNameList[39]   > general.items_release_delivery.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 200 yes
     _FldNameList[40]   > comercial.tipo_moneda.descripcion
"descripcion" "descripcion-7" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[41]   > general.items_contratos.id_tipo_contrato
"id_tipo_contrato" "id_tipo_contrato" ? ? "integer" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[42]   > general.release_delivery.numero_release
"numero_release" "numero_release" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[43]   > general.items_release_delivery.id_lote_deposito
"id_lote_deposito" "id_lote_deposito" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes
     _FldNameList[44]   > general.items_release_delivery.id_sucursal_ubicacion
"id_sucursal_ubicacion" "id_sucursal_ubicacion" ? ? "integer" ? ? ? ? ? ? yes ? yes 21 yes
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeCursor dTables  _DB-REQUIRED
PROCEDURE changeCursor :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcCursor AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcCursor).

  /*
  MESSAGE "Cambio a " items_release_delivery.ITEM_release_delivery 
          " " items_release_delivery.id_RELEASE_delivery
      VIEW-AS ALERT-BOX.
  */

  /* Code placed here will execute AFTER standard behavior.    */

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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendRows dTables  _DB-REQUIRED
PROCEDURE sendRows :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT  PARAMETER piStartRow     AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pcRowIdent     AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER plNext         AS LOGICAL NO-UNDO.
  DEFINE INPUT  PARAMETER piRowsToReturn AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER piRowsReturned AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT piStartRow, INPUT pcRowIdent, INPUT plNext, INPUT piRowsToReturn, OUTPUT piRowsReturned).
    
  /*
  MESSAGE "Cambio a " items_release_delivery.ITEM_release_delivery 
          " " items_release_delivery.id_RELEASE_delivery
      VIEW-AS ALERT-BOX.
      */

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viejo-no-sirve-cargo-proforma dTables  _DB-REQUIRED
PROCEDURE viejo-no-sirve-cargo-proforma :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cargo-lote dTables  _DB-REQUIRED
FUNCTION cargo-lote RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE BUFFER bbTambores FOR tambores_industria.
DEFINE VAR vLote AS CHAR.

vLote = "".
FOR EACH bbTambores WHERE bbTambores.id_release_delivery   = rowObject.id_release_delivery 
                      AND bbTambores.ITEM_release_delivery = rowObject.ITEM_release_delivery                
                    NO-LOCK
                    BREAK BY bbTambores.id_lote_deposito.
    IF LAST-OF(bbTambores.id_lote_deposito) THEN DO:
        vLote = vLote + bbTambores.id_lote_deposito + " ".
    END.
END.

  RETURN vLote.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cargo-proforma dTables  _DB-REQUIRED
FUNCTION cargo-proforma RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE BUFFER bbPL FOR packing_list.
DEFINE BUFFER bbItemsPL FOR items_packing_list.
DEFINE BUFFER bbrItemsVtaPackList FOR r_items_venta_pack_list.
DEFINE BUFFER bbItemsVta FOR items_venta.
DEFINE BUFFER bbVentas FOR subd_vtas.
DEFINE VAR vProforma AS CHAR.

vProforma = "".
FOR EACH bbPL WHERE bbPL.id_contrato        = rowObject.id_contrato
                AND bbPL.id_tipo_contrato   = rowObject.id_tipo_contrato
                AND bbPL.anio               = rowObject.anio
                AND bbPL.ITEM_contrato      = rowObject.ITEM.

    /* MESSAGE "PL " bbPL.id_packing_list VIEW-AS ALERT-BOX. */
    
    FOR EACH bbItemsPL OF bbPL NO-LOCK.

    /*    MESSAGE "Items PL " bbItemsPL.ITEM VIEW-AS ALERT-BOX. */

        FOR EACH bbrItemsVtaPackList WHERE bbrItemsVtaPackList.id_packing_list  = bbItemsPL.id_packing_list
                                       AND bbrItemsVtaPackList.id_sucursal      = bbItemsPL.id_sucursal
                                       AND bbrItemsVtaPackList.item_pack        = bbItemsPL.ITEM
                                      NO-LOCK.
            
            FIND FIRST bbItemsVta OF bbrItemsVtaPackList NO-LOCK NO-ERROR.
            IF AVAILABLE bbItemsVta THEN DO:

               /*  MESSAGE "Items Vta " bbItemsVta.ITEM VIEW-AS ALERT-BOX. */

               FIND FIRST bbVentas WHERE bbVentas.id_punto_venta = bbItemsVta.id_punto_venta
                                     AND bbVentas.nromov         = bbItemsVta.nromov
                                    NO-LOCK NO-ERROR.
               IF AVAILABLE bbVentas THEN DO:

                   /* MESSAGE "Proforma " bbVentas.nro_proforma VIEW-AS ALERT-BOX. */
                   IF bbVentas.nro_proforma > 0 THEN
                    vProforma = STRING(bbVentas.nro_proforma).
               END.
            END.
        END.
    END.
END.

    
  RETURN vProforma.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

