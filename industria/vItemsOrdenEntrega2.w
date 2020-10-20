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
       {"ditemsordenentregaindustria.i"}.


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
&Scoped-define DATA-FIELD-DEFS "ditemsordenentregaindustria.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.total_factura ~
RowObject.total_galones RowObject.importe_comisiones RowObject.fob_ton ~
RowObject.fob_unitario RowObject.tipo_cambio RowObject.id_moneda_cambio ~
RowObject.importe_factura_dolar RowObject.importe_fob_dolar ~
RowObject.coeficiente RowObject.importe_derechos_exportacion ~
RowObject.valor_aduana_derechos RowObject.importe_reintegro_fijo ~
RowObject.valor_aduana_reintegro 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-FIELDS RowObject.total_factura ~
RowObject.total_galones RowObject.importe_comisiones RowObject.fob_ton ~
RowObject.fob_unitario RowObject.tipo_cambio RowObject.id_moneda_cambio ~
RowObject.importe_factura_dolar RowObject.importe_fob_dolar ~
RowObject.coeficiente RowObject.importe_derechos_exportacion ~
RowObject.valor_aduana_derechos RowObject.importe_reintegro_fijo ~
RowObject.valor_aduana_reintegro 
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
     SIZE 118 BY 3.57.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 118 BY 3.81.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 118 BY 3.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.total_factura AT ROW 2.19 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.total_galones AT ROW 2.19 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     RowObject.importe_comisiones AT ROW 3.14 COL 14 COLON-ALIGNED
          LABEL "Comisiones" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.fob_ton AT ROW 3.14 COL 63 COLON-ALIGNED
          LABEL "Fob/Ton" FORMAT "->>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.fob_unitario AT ROW 3.14 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.tipo_cambio AT ROW 6 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.id_moneda_cambio AT ROW 6 COL 63 COLON-ALIGNED
          LABEL "Moneda" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     RowObject.importe_factura_dolar AT ROW 6.95 COL 14 COLON-ALIGNED
          LABEL "Total Fac U$S" FORMAT "->>>>>9.999"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.importe_fob_dolar AT ROW 6.95 COL 63 COLON-ALIGNED
          LABEL "Fob U$S" FORMAT "->>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.coeficiente AT ROW 10.29 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     RowObject.importe_derechos_exportacion AT ROW 10.29 COL 63 COLON-ALIGNED
          LABEL "DerechoAduana" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.valor_aduana_derechos AT ROW 10.29 COL 98 COLON-ALIGNED
          LABEL "Derechos" FORMAT "->>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.importe_reintegro_fijo AT ROW 11.24 COL 63 COLON-ALIGNED
          LABEL "ReintegroAduana" FORMAT "->>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.valor_aduana_reintegro AT ROW 11.24 COL 98 COLON-ALIGNED
          LABEL "Reintegros" FORMAT "->>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RECT-1 AT ROW 9.1 COL 2
     RECT-2 AT ROW 4.81 COL 2
     RECT-3 AT ROW 1 COL 2
     "Informacion de Derechos y Reintegros" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 9.33 COL 3
          FGCOLOR 1 
     "Calculos en U$S" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 5.05 COL 3
          FGCOLOR 1 
     "Datos de Factura" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 1.24 COL 3
          FGCOLOR 1 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 120.2 BY 11.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "ditemsordenentregaindustria.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {ditemsordenentregaindustria.i}
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
         HEIGHT             = 11.76
         WIDTH              = 120.2.
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

/* SETTINGS FOR FILL-IN RowObject.fob_ton IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.id_moneda_cambio IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.importe_comisiones IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.importe_derechos_exportacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.importe_factura_dolar IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.importe_fob_dolar IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.importe_reintegro_fijo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.valor_aduana_derechos IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.valor_aduana_reintegro IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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

