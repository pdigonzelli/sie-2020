&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE dqstocklotes
       {"dqstocklotes.i"}.
DEFINE TEMP-TABLE dsucursales
       {"dsucursales.i"}.
DEFINE TEMP-TABLE dsucursalstock
       {"dsucursalstock.i"}.


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
&Scoped-define DATA-FIELD-DEFS "sboqmovimentosstocklotes.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-FIELDS dqstocklotes.id_lote dqstocklotes.anio ~
dqstocklotes.nromov dsucursales.id_sucursal dsucursales.nombre ~
dqstocklotes.tambor_desde dsucursalstock.id_sucursal dsucursalstock.nombre ~
dqstocklotes.tambor_hasta 
&Scoped-define DISPLAYED-TABLES dqstocklotes dsucursales dsucursalstock
&Scoped-define FIRST-DISPLAYED-TABLE dqstocklotes
&Scoped-define SECOND-DISPLAYED-TABLE dsucursales
&Scoped-define THIRD-DISPLAYED-TABLE dsucursalstock


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     dqstocklotes.id_lote AT ROW 1 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     dqstocklotes.anio AT ROW 1 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     dqstocklotes.nromov AT ROW 1 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     dsucursales.id_sucursal AT ROW 2.91 COL 8.4 COLON-ALIGNED
          LABEL "Desde"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     dsucursales.nombre AT ROW 2.91 COL 15 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     dqstocklotes.tambor_desde AT ROW 2.91 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     dsucursalstock.id_sucursal AT ROW 4.1 COL 8.4 COLON-ALIGNED
          LABEL "Hasta"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     dsucursalstock.nombre AT ROW 4.1 COL 15 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     dqstocklotes.tambor_hasta AT ROW 4.1 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 80 BY 4.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sboqmovimentosstocklotes.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: dqstocklotes D "?" ?  
      ADDITIONAL-FIELDS:
          {"dqstocklotes.i"}
      END-FIELDS.
      TABLE: dsucursales D "?" ?  
      ADDITIONAL-FIELDS:
          {"dsucursales.i"}
      END-FIELDS.
      TABLE: dsucursalstock D "?" ?  
      ADDITIONAL-FIELDS:
          {"dsucursalstock.i"}
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
         HEIGHT             = 4.24
         WIDTH              = 80.
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

/* SETTINGS FOR FILL-IN dqstocklotes.anio IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       dqstocklotes.anio:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dqstocklotes.id_lote IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       dqstocklotes.id_lote:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dsucursalstock.id_sucursal IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       dsucursalstock.id_sucursal:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dsucursales.id_sucursal IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       dsucursales.id_sucursal:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dsucursales.nombre IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       dsucursales.nombre:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dsucursalstock.nombre IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       dsucursalstock.nombre:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dqstocklotes.nromov IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       dqstocklotes.nromov:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dqstocklotes.tambor_desde IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       dqstocklotes.tambor_desde:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN dqstocklotes.tambor_hasta IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       dqstocklotes.tambor_hasta:READ-ONLY IN FRAME F-Main        = TRUE.

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

