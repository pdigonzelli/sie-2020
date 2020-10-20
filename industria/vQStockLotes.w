&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dQStockLotes.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dQStockLotes.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS RowObject.id_lote RowObject.tambor_desde ~
RowObject.c_usuario RowObject.anio RowObject.tambor_hasta RowObject.nromov ~
RowObject.cantidad_tambores RowObject.fecha RowObject.c_fecha ~
RowObject.id_articulo RowObject.id_serial RowObject.descripcion ~
RowObject.id_tipo_movimiento 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 5.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_lote AT ROW 1.29 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     RowObject.tambor_desde AT ROW 1.29 COL 39.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.c_usuario AT ROW 1.29 COL 65.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.anio AT ROW 2.43 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.tambor_hasta AT ROW 2.43 COL 39.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.nromov AT ROW 2.43 COL 65.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.cantidad_tambores AT ROW 3.52 COL 52 RIGHT-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.fecha AT ROW 3.57 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.c_fecha AT ROW 3.57 COL 65.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.id_articulo AT ROW 4.67 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_serial AT ROW 4.71 COL 65.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.descripcion AT ROW 5.76 COL 8 COLON-ALIGNED
          LABEL "Movto."
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.id_tipo_movimiento AT ROW 5.81 COL 65.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 88.2 BY 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dQStockLotes.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dQStockLotes.i}
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
         HEIGHT             = 6
         WIDTH              = 88.2.
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

/* SETTINGS FOR FILL-IN RowObject.anio IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.anio:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.cantidad_tambores IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       RowObject.cantidad_tambores:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.c_fecha IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.c_fecha:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.c_usuario IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.c_usuario:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.descripcion:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fecha IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fecha:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_articulo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_articulo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_lote IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_lote:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_serial IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_serial:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_tipo_movimiento IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_tipo_movimiento:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.nromov IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.nromov:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.tambor_desde IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.tambor_desde:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.tambor_hasta IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.tambor_hasta:READ-ONLY IN FRAME F-Main        = TRUE.

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

