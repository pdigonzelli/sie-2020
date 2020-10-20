&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
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
&Scoped-define INTERNAL-TABLES permisos_embarque r_subd_ventas_embarque ~
subd_vtas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  id_orden_entrega id_permiso_embarque id_tipo_articulo~
 id_posicion_arancelaria id_moneda_origen id_estado_pe id_despachante~
 id_cliente id_calidad id_articulo id_aduana id_punto_venta nro_embarque~
 nro_comp nro_proforma vencimiento proforma anio nromov id_aduana-2~
 anio_permiso
&Scoped-define DATA-FIELDS-IN-permisos_embarque id_orden_entrega ~
id_permiso_embarque id_tipo_articulo id_posicion_arancelaria ~
id_moneda_origen id_estado_pe id_despachante id_cliente id_calidad ~
id_articulo id_aduana anio 
&Scoped-define DATA-FIELDS-IN-r_subd_ventas_embarque id_punto_venta ~
nro_embarque nromov id_aduana-2 anio_permiso 
&Scoped-define DATA-FIELDS-IN-subd_vtas nro_comp nro_proforma vencimiento ~
proforma 
&Scoped-Define MANDATORY-FIELDS  id_calidad id_punto_venta nro_embarque nromov
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.id_aduana-2 = r_subd_ventas_embarque.id_aduana
&Scoped-Define DATA-FIELD-DEFS "d000facturaspermisosembarque.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH permisos_embarque NO-LOCK, ~
      EACH r_subd_ventas_embarque WHERE r_subd_ventas_embarque.anio_permiso = permisos_embarque.anio ~
  AND r_subd_ventas_embarque.id_aduana = permisos_embarque.id_aduana ~
  AND r_subd_ventas_embarque.nro_embarque = permisos_embarque.id_permiso_embarque NO-LOCK, ~
      EACH subd_vtas OF r_subd_ventas_embarque NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH permisos_embarque NO-LOCK, ~
      EACH r_subd_ventas_embarque WHERE r_subd_ventas_embarque.anio_permiso = permisos_embarque.anio ~
  AND r_subd_ventas_embarque.id_aduana = permisos_embarque.id_aduana ~
  AND r_subd_ventas_embarque.nro_embarque = permisos_embarque.id_permiso_embarque NO-LOCK, ~
      EACH subd_vtas OF r_subd_ventas_embarque NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main permisos_embarque ~
r_subd_ventas_embarque subd_vtas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main permisos_embarque
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main r_subd_ventas_embarque
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main subd_vtas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      permisos_embarque
    FIELDS(permisos_embarque.id_orden_entrega
      permisos_embarque.id_permiso_embarque
      permisos_embarque.id_tipo_articulo
      permisos_embarque.id_posicion_arancelaria
      permisos_embarque.id_moneda_origen
      permisos_embarque.id_estado_pe
      permisos_embarque.id_despachante
      permisos_embarque.id_cliente
      permisos_embarque.id_calidad
      permisos_embarque.id_articulo
      permisos_embarque.id_aduana
      permisos_embarque.anio), 
      r_subd_ventas_embarque
    FIELDS(r_subd_ventas_embarque.id_punto_venta
      r_subd_ventas_embarque.nro_embarque
      r_subd_ventas_embarque.nromov
      r_subd_ventas_embarque.id_aduana
      r_subd_ventas_embarque.anio_permiso), 
      subd_vtas
    FIELDS(subd_vtas.nro_comp
      subd_vtas.nro_proforma
      subd_vtas.vencimiento
      subd_vtas.proforma) SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Other Settings: COMPILE APPSERVER DB-AWARE
 */
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


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "general.permisos_embarque,ventas.r_subd_ventas_embarque WHERE general.permisos_embarque ...,ventas.subd_vtas OF ventas.r_subd_ventas_embarque"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = "USED, USED, USED"
     _JoinCode[2]      = "ventas.r_subd_ventas_embarque.anio_permiso = general.permisos_embarque.anio
  AND ventas.r_subd_ventas_embarque.id_aduana = general.permisos_embarque.id_aduana
  AND ventas.r_subd_ventas_embarque.nro_embarque = general.permisos_embarque.id_permiso_embarque"
     _FldNameList[1]   > general.permisos_embarque.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? no ? no 7.8 yes
     _FldNameList[2]   > general.permisos_embarque.id_permiso_embarque
"id_permiso_embarque" "id_permiso_embarque" ? ? "character" ? ? ? ? ? ? no ? no 13 yes
     _FldNameList[3]   > general.permisos_embarque.id_tipo_articulo
"id_tipo_articulo" "id_tipo_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 6.6 yes
     _FldNameList[4]   > general.permisos_embarque.id_posicion_arancelaria
"id_posicion_arancelaria" "id_posicion_arancelaria" ? ? "character" ? ? ? ? ? ? no ? no 14 yes
     _FldNameList[5]   > general.permisos_embarque.id_moneda_origen
"id_moneda_origen" "id_moneda_origen" ? ? "integer" ? ? ? ? ? ? no ? no 14.6 yes
     _FldNameList[6]   > general.permisos_embarque.id_estado_pe
"id_estado_pe" "id_estado_pe" ? ? "integer" ? ? ? ? ? ? no ? no 9.4 yes
     _FldNameList[7]   > general.permisos_embarque.id_despachante
"id_despachante" "id_despachante" ? ? "integer" ? ? ? ? ? ? no ? no 10 yes
     _FldNameList[8]   > general.permisos_embarque.id_cliente
"id_cliente" "id_cliente" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[9]   > general.permisos_embarque.id_calidad
"id_calidad" "id_calidad" ? ? "integer" ? ? ? ? ? ? no ? yes 10.2 yes
     _FldNameList[10]   > general.permisos_embarque.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 10.2 yes
     _FldNameList[11]   > general.permisos_embarque.id_aduana
"id_aduana" "id_aduana" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[12]   > ventas.r_subd_ventas_embarque.id_punto_venta
"id_punto_venta" "id_punto_venta" ? ? "integer" ? ? ? ? ? ? no ? yes 7.6 yes
     _FldNameList[13]   > ventas.r_subd_ventas_embarque.nro_embarque
"nro_embarque" "nro_embarque" ? ? "character" ? ? ? ? ? ? no ? yes 18 yes
     _FldNameList[14]   > ventas.subd_vtas.nro_comp
"nro_comp" "nro_comp" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[15]   > ventas.subd_vtas.nro_proforma
"nro_proforma" "nro_proforma" ? ? "integer" ? ? ? ? ? ? no ? no 12.4 yes
     _FldNameList[16]   > ventas.subd_vtas.vencimiento
"vencimiento" "vencimiento" ? ? "date" ? ? ? ? ? ? no ? no 12.2 yes
     _FldNameList[17]   > ventas.subd_vtas.proforma
"proforma" "proforma" ? ? "logical" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[18]   > general.permisos_embarque.anio
"anio" "anio" ? ? "integer" ? ? ? ? ? ? no ? no 4.8 yes
     _FldNameList[19]   > ventas.r_subd_ventas_embarque.nromov
"nromov" "nromov" ? ? "integer" ? ? ? ? ? ? no ? yes 10.2 yes
     _FldNameList[20]   > ventas.r_subd_ventas_embarque.id_aduana
"id_aduana" "id_aduana-2" ? ? "integer" ? ? ? ? ? ? no ? no 11.8 yes
     _FldNameList[21]   > ventas.r_subd_ventas_embarque.anio_permiso
"anio_permiso" "anio_permiso" ? ? "integer" ? ? ? ? ? ? no ? no 12.2 yes
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

