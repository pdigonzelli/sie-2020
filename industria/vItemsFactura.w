&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dItemsContratos.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dItemsContratos.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.item RowObject.destino_final ~
RowObject.id_articulo_cliente1 RowObject.id_articulo ~
RowObject.id_tipo_venta RowObject.id_clausula RowObject.numero_release1 ~
RowObject.estado RowObject.marca_tambores RowObject.id_calidad ~
RowObject.pendiente RowObject.id_envase RowObject.cert_fito ~
RowObject.observaciones RowObject.id_tipo_unidad_venta ~
RowObject.semana_entrega RowObject.embarque_estimado ~
RowObject.semana_entrega_hasta RowObject.id_deposito_final ~
RowObject.arribo_estimado RowObject.anio_semana_entrega ~
RowObject.id_puerto_ent RowObject.id_from RowObject.cantidad ~
RowObject.id_vapor RowObject.id_destino RowObject.id_puerto_sal 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.item RowObject.destino_final ~
RowObject.id_articulo_cliente1 RowObject.id_articulo ~
RowObject.id_tipo_venta RowObject.id_clausula RowObject.numero_release1 ~
RowObject.estado RowObject.marca_tambores RowObject.id_calidad ~
RowObject.pendiente RowObject.id_envase RowObject.cert_fito ~
RowObject.observaciones RowObject.id_tipo_unidad_venta ~
RowObject.semana_entrega RowObject.embarque_estimado ~
RowObject.semana_entrega_hasta RowObject.id_deposito_final ~
RowObject.arribo_estimado RowObject.anio_semana_entrega ~
RowObject.id_puerto_ent RowObject.id_from RowObject.cantidad ~
RowObject.id_vapor RowObject.id_destino RowObject.id_puerto_sal 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.item AT ROW 1 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.destino_final AT ROW 1.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_articulo_cliente1 AT ROW 1.71 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.id_articulo AT ROW 1.95 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.id_tipo_venta AT ROW 2.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.id_clausula AT ROW 2.71 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.numero_release1 AT ROW 2.95 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     RowObject.estado AT ROW 3.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     RowObject.marca_tambores AT ROW 3.71 COL 128 NO-LABEL
          VIEW-AS EDITOR
          SIZE 50 BY 2
     RowObject.id_calidad AT ROW 3.95 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.pendiente AT ROW 4.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.id_envase AT ROW 4.95 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.cert_fito AT ROW 5.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.observaciones AT ROW 5.52 COL 128 NO-LABEL
          VIEW-AS EDITOR
          SIZE 50 BY 2
     RowObject.id_tipo_unidad_venta AT ROW 6 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.semana_entrega AT ROW 6.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.embarque_estimado AT ROW 7 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.semana_entrega_hasta AT ROW 7.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.id_deposito_final AT ROW 7.67 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.arribo_estimado AT ROW 8 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.anio_semana_entrega AT ROW 8.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.id_puerto_ent AT ROW 8.62 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_from AT ROW 9.1 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.cantidad AT ROW 9.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     RowObject.id_vapor AT ROW 9.62 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_destino AT ROW 10.1 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_puerto_sal AT ROW 10.62 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 177.2 BY 14.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dItemsContratos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dItemsContratos.i}
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
         HEIGHT             = 14.81
         WIDTH              = 177.2.
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

