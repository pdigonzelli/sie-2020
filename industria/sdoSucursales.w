&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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
&Scoped-define INTERNAL-TABLES sucursales

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  abreviatura barrio cajas cant_empleados codigo_dgi c_fecha c_hora c_usuario~
 domicilio estado fax id_categoria id_centro_costo id_sucursal~
 id_tipo_sucursal localidad metros_playa metros_salon metros_servicios~
 metros_total nombre observaciones pais postal provincia telefono1 telefono2~
 telefono3
&Scoped-define DATA-FIELDS-IN-sucursales abreviatura barrio cajas ~
cant_empleados codigo_dgi c_fecha c_hora c_usuario domicilio estado fax ~
id_categoria id_centro_costo id_sucursal id_tipo_sucursal localidad ~
metros_playa metros_salon metros_servicios metros_total nombre ~
observaciones pais postal provincia telefono1 telefono2 telefono3 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.telefono1 = sucursales.telefono[1]~
  rowObject.telefono2 = sucursales.telefono[2]~
  rowObject.telefono3 = sucursales.telefono[3]
&Scoped-Define DATA-FIELD-DEFS "sdoSucursales.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH sucursales NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH sucursales NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main sucursales
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main sucursales


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      sucursales SCROLLING.
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
     _TblList          = "comercial.sucursales"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > comercial.sucursales.abreviatura
"abreviatura" "abreviatura" ? ? "character" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[2]   > comercial.sucursales.barrio
"barrio" "barrio" ? ? "character" ? ? ? ? ? ? no ? no 20 yes
     _FldNameList[3]   > comercial.sucursales.cajas
"cajas" "cajas" ? ? "integer" ? ? ? ? ? ? no ? no 9.6 yes
     _FldNameList[4]   > comercial.sucursales.cant_empleados
"cant_empleados" "cant_empleados" ? ? "integer" ? ? ? ? ? ? no ? no 10.4 yes
     _FldNameList[5]   > comercial.sucursales.codigo_dgi
"codigo_dgi" "codigo_dgi" ? ? "integer" ? ? ? ? ? ? no ? no 11 yes
     _FldNameList[6]   > comercial.sucursales.c_fecha
"c_fecha" "c_fecha" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[7]   > comercial.sucursales.c_hora
"c_hora" "c_hora" ? ? "character" ? ? ? ? ? ? no ? no 8 yes
     _FldNameList[8]   > comercial.sucursales.c_usuario
"c_usuario" "c_usuario" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[9]   > comercial.sucursales.domicilio
"domicilio" "domicilio" ? ? "character" ? ? ? ? ? ? no ? no 35 yes
     _FldNameList[10]   > comercial.sucursales.estado
"estado" "estado" ? ? "logical" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[11]   > comercial.sucursales.fax
"fax" "fax" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[12]   > comercial.sucursales.id_categoria
"id_categoria" "id_categoria" ? ? "integer" ? ? ? ? ? ? no ? no 6.2 yes
     _FldNameList[13]   > comercial.sucursales.id_centro_costo
"id_centro_costo" "id_centro_costo" ? ? "character" ? ? ? ? ? ? no ? no 16 yes
     _FldNameList[14]   > comercial.sucursales.id_sucursal
"id_sucursal" "id_sucursal" ? ? "integer" ? ? ? ? ? ? no ? no 8.2 yes
     _FldNameList[15]   > comercial.sucursales.id_tipo_sucursal
"id_tipo_sucursal" "id_tipo_sucursal" ? ? "integer" ? ? ? ? ? ? no ? no 6.8 yes
     _FldNameList[16]   > comercial.sucursales.localidad
"localidad" "localidad" ? ? "character" ? ? ? ? ? ? no ? no 15 yes
     _FldNameList[17]   > comercial.sucursales.metros_playa
"metros_playa" "metros_playa" ? ? "decimal" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[18]   > comercial.sucursales.metros_salon
"metros_salon" "metros_salon" ? ? "decimal" ? ? ? ? ? ? no ? no 10.8 yes
     _FldNameList[19]   > comercial.sucursales.metros_servicios
"metros_servicios" "metros_servicios" ? ? "decimal" ? ? ? ? ? ? no ? no 12.2 yes
     _FldNameList[20]   > comercial.sucursales.metros_total
"metros_total" "metros_total" ? ? "decimal" ? ? ? ? ? ? no ? no 12.6 yes
     _FldNameList[21]   > comercial.sucursales.nombre
"nombre" "nombre" ? ? "character" ? ? ? ? ? ? no ? no 30 yes
     _FldNameList[22]   > comercial.sucursales.observaciones
"observaciones" "observaciones" ? ? "character" ? ? ? ? ? ? no ? no 50 yes
     _FldNameList[23]   > comercial.sucursales.pais
"pais" "pais" ? ? "character" ? ? ? ? ? ? no ? no 20 yes
     _FldNameList[24]   > comercial.sucursales.postal
"postal" "postal" ? ? "integer" ? ? ? ? ? ? no ? no 5.8 yes
     _FldNameList[25]   > comercial.sucursales.provincia
"provincia" "provincia" ? ? "character" ? ? ? ? ? ? no ? no 25 yes
     _FldNameList[26]   > comercial.sucursales.telefono[1]
"telefono[1]" "telefono1" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[27]   > comercial.sucursales.telefono[2]
"telefono[2]" "telefono2" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[28]   > comercial.sucursales.telefono[3]
"telefono[3]" "telefono3" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
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
