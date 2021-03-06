&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
          general         PROGRESS
          general         PROGRESS
          ventas           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"denvio.i"}.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "denvio.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.fecha_salida_origen ~
RowObject.fecha_llegada_destino RowObject.fecha_salida_destino ~
RowObject.fecha_custom_entry RowObject.fecha_fda_notice ~
RowObject.nro_fda_notice RowObject.fecha_release_customer ~
RowObject.fecha_release_fda RowObject.fechas_intermedias1 ~
RowObject.descripcion_fechas_intermedias1 RowObject.fechas_intermedias2 ~
RowObject.descripcion_fechas_intermedias2 RowObject.observaciones 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-6 
&Scoped-Define DISPLAYED-FIELDS RowObject.fecha_salida_origen ~
RowObject.id_envio RowObject.id_orden_entrega ~
RowObject.fecha_llegada_destino RowObject.Vapor ~
RowObject.fecha_salida_destino RowObject.destino ~
RowObject.fecha_custom_entry RowObject.DestinoFinal ~
RowObject.fecha_fda_notice RowObject.nro_fda_notice RowObject.Producto ~
RowObject.fecha_release_customer RowObject.Carrier ~
RowObject.fecha_release_fda RowObject.Cliente RowObject.Entrega ~
RowObject.fechas_intermedias1 RowObject.descripcion_fechas_intermedias1 ~
RowObject.fechas_intermedias2 RowObject.descripcion_fechas_intermedias2 ~
RowObject.observaciones 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 3.1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 11.43.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 8.1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 128 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.fecha_salida_origen AT ROW 1.57 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.id_envio AT ROW 1.95 COL 12.2 COLON-ALIGNED
          LABEL "Envio"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.id_orden_entrega AT ROW 1.95 COL 39.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.fecha_llegada_destino AT ROW 2.67 COL 82 COLON-ALIGNED
          LABEL "ETA"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.Vapor AT ROW 3.05 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     RowObject.fecha_salida_destino AT ROW 3.76 COL 82 COLON-ALIGNED
          LABEL "ETA Real"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.destino AT ROW 4.19 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     RowObject.fecha_custom_entry AT ROW 4.86 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.DestinoFinal AT ROW 5.29 COL 12 COLON-ALIGNED
          LABEL "Dest.Final"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     RowObject.fecha_fda_notice AT ROW 5.95 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.nro_fda_notice AT ROW 5.95 COL 113 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.6 BY 1
     RowObject.Producto AT ROW 6.38 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     RowObject.fecha_release_customer AT ROW 7.05 COL 82 COLON-ALIGNED
          LABEL "Fecha Released by Customs"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.Carrier AT ROW 7.48 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     RowObject.fecha_release_fda AT ROW 8.14 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.Cliente AT ROW 8.62 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.Entrega AT ROW 9.76 COL 12 COLON-ALIGNED
          LABEL "Dir.Entrega"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     RowObject.fechas_intermedias1 AT ROW 10.29 COL 77.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.descripcion_fechas_intermedias1 AT ROW 10.29 COL 91.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.fechas_intermedias2 AT ROW 11.43 COL 77.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.descripcion_fechas_intermedias2 AT ROW 11.43 COL 91.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.observaciones AT ROW 13.62 COL 5 NO-LABEL
          VIEW-AS EDITOR
          SIZE 122 BY 2
     RECT-1 AT ROW 9.57 COL 55
     RECT-2 AT ROW 1.24 COL 1
     RECT-3 AT ROW 1.24 COL 55
     RECT-6 AT ROW 12.91 COL 1
     "Identificacion Envio" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 1 COL 3
          FONT 6
     "Fechas Envio" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1 COL 56
          FONT 6
     "Fechas Intermedias" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 9.33 COL 57
          FONT 6
     "Observaciones" VIEW-AS TEXT
          SIZE 18 BY .95 AT ROW 12.67 COL 3
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 128.6 BY 15.05.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "denvio.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {denvio.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 15.05
         WIDTH              = 128.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Carrier IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Carrier:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Cliente IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Cliente:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.destino IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.destino:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.DestinoFinal IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.DestinoFinal:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Entrega IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.Entrega:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fecha_llegada_destino IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.fecha_release_customer IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.fecha_salida_destino IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_envio IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.id_envio:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_orden_entrega IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_orden_entrega:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Producto IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Producto:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Vapor IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Vapor:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */
  {adm2/support/viewTrg.i}.  
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

