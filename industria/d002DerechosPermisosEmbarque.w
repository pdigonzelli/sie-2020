&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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
&Scoped-define INTERNAL-TABLES permisos_embarque despachantes estados_pe ~
aduanas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  anio banco_interviniente consignacion c_fecha c_hora c_usuario estado_afip~
 estado_bcra fecha fecha_concluido fecha_confirmacion_lman fecha_cumplido~
 fecha_declaracion_venta fecha_envio_tuc fecha_oficializacion fecha_rec_tuc~
 fecha_solicitud_fondos fecha_transferencia_fondos fecha_venc_derecho~
 id_aduana id_articulo id_calidad id_cliente id_despachante id_estado_pe~
 id_moneda_origen id_orden_entrega id_permiso_embarque~
 id_posicion_arancelaria id_tipo_articulo importe importe_derecho~
 importe_reembolso importe_reintegro importe_trasnferido item_oe~
 nro_remito_envio_tuc numero_lman observaciones rectificado tipo_pe~
 Despachante Estado descripcion
&Scoped-define ENABLED-FIELDS-IN-permisos_embarque anio banco_interviniente ~
consignacion c_fecha c_hora c_usuario estado_afip estado_bcra fecha ~
fecha_concluido fecha_confirmacion_lman fecha_cumplido ~
fecha_declaracion_venta fecha_envio_tuc fecha_oficializacion fecha_rec_tuc ~
fecha_solicitud_fondos fecha_transferencia_fondos fecha_venc_derecho ~
id_aduana id_articulo id_calidad id_cliente id_despachante id_estado_pe ~
id_moneda_origen id_orden_entrega id_permiso_embarque ~
id_posicion_arancelaria id_tipo_articulo importe importe_derecho ~
importe_reembolso importe_reintegro importe_trasnferido item_oe ~
nro_remito_envio_tuc numero_lman observaciones rectificado tipo_pe 
&Scoped-define ENABLED-FIELDS-IN-despachantes Despachante 
&Scoped-define ENABLED-FIELDS-IN-estados_pe Estado 
&Scoped-define ENABLED-FIELDS-IN-aduanas descripcion 
&Scoped-Define DATA-FIELDS  anio banco_interviniente consignacion c_fecha c_hora c_usuario estado_afip~
 estado_bcra fecha fecha_concluido fecha_confirmacion_lman fecha_cumplido~
 fecha_declaracion_venta fecha_envio_tuc fecha_oficializacion fecha_rec_tuc~
 fecha_solicitud_fondos fecha_transferencia_fondos fecha_venc_derecho~
 id_aduana id_articulo id_calidad id_cliente id_despachante id_estado_pe~
 id_moneda_origen id_orden_entrega id_permiso_embarque~
 id_posicion_arancelaria id_tipo_articulo importe importe_derecho~
 importe_reembolso importe_reintegro importe_trasnferido item_oe~
 nro_remito_envio_tuc numero_lman observaciones rectificado tipo_pe~
 Despachante Estado descripcion
&Scoped-define DATA-FIELDS-IN-permisos_embarque anio banco_interviniente ~
consignacion c_fecha c_hora c_usuario estado_afip estado_bcra fecha ~
fecha_concluido fecha_confirmacion_lman fecha_cumplido ~
fecha_declaracion_venta fecha_envio_tuc fecha_oficializacion fecha_rec_tuc ~
fecha_solicitud_fondos fecha_transferencia_fondos fecha_venc_derecho ~
id_aduana id_articulo id_calidad id_cliente id_despachante id_estado_pe ~
id_moneda_origen id_orden_entrega id_permiso_embarque ~
id_posicion_arancelaria id_tipo_articulo importe importe_derecho ~
importe_reembolso importe_reintegro importe_trasnferido item_oe ~
nro_remito_envio_tuc numero_lman observaciones rectificado tipo_pe 
&Scoped-define DATA-FIELDS-IN-despachantes Despachante 
&Scoped-define DATA-FIELDS-IN-estados_pe Estado 
&Scoped-define DATA-FIELDS-IN-aduanas descripcion 
&Scoped-Define MANDATORY-FIELDS  id_calidad
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Despachante = despachantes.descripcion~
  rowObject.Estado = estados_pe.descripcion
&Scoped-Define DATA-FIELD-DEFS "d002derechospermisosembarque.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH permisos_embarque NO-LOCK, ~
      EACH despachantes OF permisos_embarque NO-LOCK, ~
      EACH estados_pe OF permisos_embarque OUTER-JOIN NO-LOCK, ~
      EACH aduanas OF permisos_embarque NO-LOCK ~
    BY permisos_embarque.id_aduana ~
       BY permisos_embarque.id_despachante ~
        BY permisos_embarque.fecha_venc_derecho INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH permisos_embarque NO-LOCK, ~
      EACH despachantes OF permisos_embarque NO-LOCK, ~
      EACH estados_pe OF permisos_embarque OUTER-JOIN NO-LOCK, ~
      EACH aduanas OF permisos_embarque NO-LOCK ~
    BY permisos_embarque.id_aduana ~
       BY permisos_embarque.id_despachante ~
        BY permisos_embarque.fecha_venc_derecho INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main permisos_embarque despachantes ~
estados_pe aduanas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main permisos_embarque
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main despachantes
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main estados_pe
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main aduanas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      permisos_embarque, 
      despachantes, 
      estados_pe, 
      aduanas SCROLLING.
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
     _TblList          = "general.permisos_embarque,industria.despachantes OF general.permisos_embarque,industria.estados_pe OF general.permisos_embarque,general.aduanas OF general.permisos_embarque"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ",, OUTER,"
     _OrdList          = "general.permisos_embarque.id_aduana|yes,industria.permisos_embarque.id_despachante|yes,industria.permisos_embarque.fecha_venc_derecho|yes"
     _FldNameList[1]   > general.permisos_embarque.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[2]   > general.permisos_embarque.banco_interviniente
"banco_interviniente" "banco_interviniente" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes
     _FldNameList[3]   > general.permisos_embarque.consignacion
"consignacion" "consignacion" ? ? "logical" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[4]   > general.permisos_embarque.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[5]   > general.permisos_embarque.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[6]   > general.permisos_embarque.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[7]   > general.permisos_embarque.estado_afip
"estado_afip" "estado_afip" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes
     _FldNameList[8]   > general.permisos_embarque.estado_bcra
"estado_bcra" "estado_bcra" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes
     _FldNameList[9]   > general.permisos_embarque.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[10]   > general.permisos_embarque.fecha_concluido
"fecha_concluido" "fecha_concluido" ? ? "date" ? ? ? ? ? ? yes ? no 16 yes
     _FldNameList[11]   > general.permisos_embarque.fecha_confirmacion_lman
"fecha_confirmacion_lman" "fecha_confirmacion_lman" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[12]   > general.permisos_embarque.fecha_cumplido
"fecha_cumplido" "fecha_cumplido" ? ? "date" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[13]   > general.permisos_embarque.fecha_declaracion_venta
"fecha_declaracion_venta" "fecha_declaracion_venta" ? ? "date" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[14]   > general.permisos_embarque.fecha_envio_tuc
"fecha_envio_tuc" "fecha_envio_tuc" ? ? "date" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[15]   > general.permisos_embarque.fecha_oficializacion
"fecha_oficializacion" "fecha_oficializacion" ? ? "date" ? ? ? ? ? ? yes ? no 11 yes
     _FldNameList[16]   > general.permisos_embarque.fecha_rec_tuc
"fecha_rec_tuc" "fecha_rec_tuc" ? ? "date" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[17]   > general.permisos_embarque.fecha_solicitud_fondos
"fecha_solicitud_fondos" "fecha_solicitud_fondos" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[18]   > general.permisos_embarque.fecha_transferencia_fondos
"fecha_transferencia_fondos" "fecha_transferencia_fondos" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[19]   > general.permisos_embarque.fecha_venc_derecho
"fecha_venc_derecho" "fecha_venc_derecho" ? ? "date" ? ? ? ? ? ? yes ? no 14.4 yes
     _FldNameList[20]   > general.permisos_embarque.id_aduana
"id_aduana" "id_aduana" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes
     _FldNameList[21]   > general.permisos_embarque.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[22]   > general.permisos_embarque.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.2 yes
     _FldNameList[23]   > general.permisos_embarque.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[24]   > general.permisos_embarque.id_despachante
"id_despachante" "id_despachante" ? ? "integer" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[25]   > general.permisos_embarque.id_estado_pe
"id_estado_pe" "id_estado_pe" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[26]   > general.permisos_embarque.id_moneda_origen
"id_moneda_origen" "id_moneda_origen" ? ? "integer" ? ? ? ? ? ? yes ? no 14.6 yes
     _FldNameList[27]   > general.permisos_embarque.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[28]   > general.permisos_embarque.id_permiso_embarque
"id_permiso_embarque" "id_permiso_embarque" ? ? "character" ? ? ? ? ? ? yes ? no 13 yes
     _FldNameList[29]   > general.permisos_embarque.id_posicion_arancelaria
"id_posicion_arancelaria" "id_posicion_arancelaria" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes
     _FldNameList[30]   > general.permisos_embarque.id_tipo_articulo
"id_tipo_articulo" "id_tipo_articulo" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes
     _FldNameList[31]   > general.permisos_embarque.importe
"importe" "importe" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[32]   > general.permisos_embarque.importe_derecho
"importe_derecho" "importe_derecho" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[33]   > general.permisos_embarque.importe_reembolso
"importe_reembolso" "importe_reembolso" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes
     _FldNameList[34]   > general.permisos_embarque.importe_reintegro
"importe_reintegro" "importe_reintegro" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes
     _FldNameList[35]   > general.permisos_embarque.importe_trasnferido
"importe_trasnferido" "importe_trasnferido" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.6 yes
     _FldNameList[36]   > general.permisos_embarque.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes
     _FldNameList[37]   > general.permisos_embarque.nro_remito_envio_tuc
"nro_remito_envio_tuc" "nro_remito_envio_tuc" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[38]   > general.permisos_embarque.numero_lman
"numero_lman" "numero_lman" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[39]   > general.permisos_embarque.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[40]   > general.permisos_embarque.rectificado
"rectificado" "rectificado" ? ? "logical" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[41]   > general.permisos_embarque.tipo_pe
"tipo_pe" "tipo_pe" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes
     _FldNameList[42]   > general.despachantes.descripcion
"descripcion" "Despachante" "Despachante" ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[43]   > general.estados_pe.descripcion
"descripcion" "Estado" "Estado" ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[44]   > general.aduanas.descripcion
"descripcion" "descripcion" "Aduana" ? "character" ? ? ? ? ? ? yes ? no 30 yes
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

