&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
          general         PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dItemsOrdenEntrega.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dItemsOrdenEntrega.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.valor_aduana_derechos ~
RowObject.valor_aduana_reintegro RowObject.importe_derechos_exportacion ~
RowObject.importe_reintegro_fijo RowObject.total_factura ~
RowObject.coeficiente RowObject.importe_comisiones RowObject.fob_ton ~
RowObject.tipo_cambio 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-33 RECT-34 RECT-35 RECT-37 RECT-38 
&Scoped-Define DISPLAYED-FIELDS RowObject.UniTon ~
RowObject.valor_aduana_derechos RowObject.Unidad RowObject.UniGal ~
RowObject.valor_aduana_reintegro RowObject.PrecioVenta RowObject.UniLib ~
RowObject.importe_derechos_exportacion RowObject.kgs_netos_tambores ~
RowObject.importe_reintegro_fijo RowObject.total_factura ~
RowObject.kgs_brutos_tambores RowObject.coeficiente RowObject.GastosItem ~
RowObject.total_galones RowObject.importe_comisiones RowObject.fob_ton ~
RowObject.Libras RowObject.tipo_cambio 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 7.62.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 3.57.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 3.81.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 2.86.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.UniTon AT ROW 1.48 COL 65 COLON-ALIGNED
          LABEL "x Tonelada" FORMAT ">>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.valor_aduana_derechos AT ROW 1.48 COL 112 COLON-ALIGNED
          LABEL "Base Derechos"
          VIEW-AS FILL-IN 
          SIZE 21.4 BY 1
     RowObject.Unidad AT ROW 1.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.UniGal AT ROW 2.48 COL 65 COLON-ALIGNED
          LABEL "x Galon" FORMAT ">>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.valor_aduana_reintegro AT ROW 2.52 COL 112 COLON-ALIGNED
          LABEL "Base Reintegro"
          VIEW-AS FILL-IN 
          SIZE 21.4 BY 1
     RowObject.PrecioVenta AT ROW 2.91 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.UniLib AT ROW 3.48 COL 65 COLON-ALIGNED
          LABEL "x Libra" FORMAT ">>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.importe_derechos_exportacion AT ROW 3.57 COL 112 COLON-ALIGNED
          LABEL "Derechos 5%"
          VIEW-AS FILL-IN 
          SIZE 21.4 BY 1
     RowObject.kgs_netos_tambores AT ROW 4.57 COL 16 COLON-ALIGNED FORMAT ">>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.importe_reintegro_fijo AT ROW 4.62 COL 112 COLON-ALIGNED
          LABEL "Reintegro 5%"
          VIEW-AS FILL-IN 
          SIZE 21.4 BY 1
     RowObject.total_factura AT ROW 5.43 COL 63 COLON-ALIGNED FORMAT ">>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.kgs_brutos_tambores AT ROW 5.62 COL 16 COLON-ALIGNED FORMAT ">>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.coeficiente AT ROW 5.67 COL 112 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.4 BY 1
     RowObject.GastosItem AT ROW 6.52 COL 63 COLON-ALIGNED
          LABEL "Gastos Parte"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.total_galones AT ROW 6.67 COL 16 COLON-ALIGNED FORMAT ">>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.importe_comisiones AT ROW 6.71 COL 112 COLON-ALIGNED FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     RowObject.fob_ton AT ROW 7.62 COL 63 COLON-ALIGNED
          LABEL "Fob" FORMAT "->>,>>>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.Libras AT ROW 7.71 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.tipo_cambio AT ROW 7.76 COL 112 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     RECT-33 AT ROW 1.24 COL 87
     RECT-34 AT ROW 1.24 COL 40
     RECT-35 AT ROW 5.05 COL 40
     RECT-37 AT ROW 1.24 COL 1
     RECT-38 AT ROW 4.33 COL 1
     "Aduana" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1 COL 88
          FONT 6
     "Montos Totales" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 4.81 COL 41
          FONT 6
     "Pesos" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 4.1 COL 2
          FONT 6
     "Precios Unitarios Fob" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 1 COL 41
          FONT 6
     "Precios Unitarios Finales" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 1 COL 2
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 136.2 BY 7.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dItemsOrdenEntrega.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dItemsOrdenEntrega.i}
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
         HEIGHT             = 7.95
         WIDTH              = 136.2.
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
/* SETTINGS FOR FILL-IN RowObject.GastosItem IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.GastosItem:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.importe_comisiones IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.importe_derechos_exportacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.importe_reintegro_fijo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.kgs_brutos_tambores IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.kgs_netos_tambores IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.Libras IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Libras:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.PrecioVenta IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.PrecioVenta:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.total_factura IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.total_galones IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.Unidad IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Unidad:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.UniGal IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       RowObject.UniGal:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.UniLib IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       RowObject.UniLib:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.UniTon IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       RowObject.UniTon:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.valor_aduana_derechos IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.valor_aduana_reintegro IN FRAME F-Main
   EXP-LABEL                                                            */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

