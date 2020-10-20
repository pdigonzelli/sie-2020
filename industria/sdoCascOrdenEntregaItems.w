&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
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
&Scoped-define INTERNAL-TABLES orden_entrega items_orden_entrega contratos

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS 
&Scoped-Define DATA-FIELDS  id_orden_entrega fecha semana_embarque item_oe tambores_pedidos id_articulo~
 id_contrato orden_fabricacion
&Scoped-define DATA-FIELDS-IN-orden_entrega id_orden_entrega fecha ~
semana_embarque 
&Scoped-define DATA-FIELDS-IN-items_orden_entrega item_oe tambores_pedidos ~
id_articulo id_contrato 
&Scoped-define DATA-FIELDS-IN-contratos orden_fabricacion 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "sdocascordenentregaitems.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH orden_entrega NO-LOCK, ~
      EACH items_orden_entrega OF orden_entrega ~
      WHERE items_orden_entrega.id_articulo = 54 NO-LOCK, ~
      EACH contratos OF items_orden_entrega NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH orden_entrega NO-LOCK, ~
      EACH items_orden_entrega OF orden_entrega ~
      WHERE items_orden_entrega.id_articulo = 54 NO-LOCK, ~
      EACH contratos OF items_orden_entrega NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main orden_entrega items_orden_entrega ~
contratos
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main orden_entrega
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main items_orden_entrega
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main contratos


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      orden_entrega, 
      items_orden_entrega, 
      contratos SCROLLING.
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
     _TblList          = "general.orden_entrega,general.items_orden_entrega OF general.orden_entrega,industria.contratos OF general.items_orden_entrega"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[2]         = "general.items_orden_entrega.id_articulo = 54"
     _FldNameList[1]   > general.orden_entrega.id_orden_entrega
"id_orden_entrega" "id_orden_entrega" ? ? "integer" ? ? ? ? ? ? no ? no 7.2 yes
     _FldNameList[2]   > general.orden_entrega.fecha
"fecha" "fecha" ? ? "date" ? ? ? ? ? ? no ? no 9.2 yes
     _FldNameList[3]   > general.orden_entrega.semana_embarque
"semana_embarque" "semana_embarque" ? ? "integer" ? ? ? ? ? ? no ? no 17.8 yes
     _FldNameList[4]   > general.items_orden_entrega.item_oe
"item_oe" "item_oe" ? ? "integer" ? ? ? ? ? ? no ? no 8.6 yes
     _FldNameList[5]   > general.items_orden_entrega.tambores_pedidos
"tambores_pedidos" "tambores_pedidos" ? ">>>>>>>>>>>9" "integer" ? ? ? ? ? ? no ? no 14.4 yes
     _FldNameList[6]   > general.items_orden_entrega.id_articulo
"id_articulo" "id_articulo" ? ? "integer" ? ? ? ? ? ? no ? no 7 yes
     _FldNameList[7]   > general.items_orden_entrega.id_contrato
"id_contrato" "id_contrato" ? ? "character" ? ? ? ? ? ? no ? no 12 yes
     _FldNameList[8]   > general.contratos.orden_fabricacion
"orden_fabricacion" "orden_fabricacion" ? ? "integer" ? ? ? ? ? ? no ? no 10.8 yes
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

