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
&Scoped-Define ENABLED-FIELDS RowObject.item_oe RowObject.fecha ~
RowObject.modo_actualizacion RowObject.cert_fito RowObject.cerrado ~
RowObject.id_contrato RowObject.item RowObject.anio ~
RowObject.id_tipo_contrato RowObject.semana_entrega RowObject.id_cliente ~
RowObject.id_articulo RowObject.id_calidad RowObject.tambores_pedidos ~
RowObject.cantidad_tambores RowObject.kgs_netos_tambores ~
RowObject.kgs_brutos_tambores RowObject.grados_brix RowObject.plazo ~
RowObject.id_tipo_plazo RowObject.id_instrumento_pago ~
RowObject.id_condicion_venta RowObject.id_estado ~
RowObject.id_tipo_contenedor RowObject.contenedores RowObject.pendiente ~
RowObject.cheque RowObject.observaciones RowObject.id_tipo_venta ~
RowObject.id_moneda RowObject.id_tipo_unidad_venta RowObject.importe_origen 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.item_oe RowObject.fecha ~
RowObject.modo_actualizacion RowObject.cert_fito RowObject.cerrado ~
RowObject.id_contrato RowObject.item RowObject.anio ~
RowObject.id_tipo_contrato RowObject.semana_entrega RowObject.id_cliente ~
RowObject.id_articulo RowObject.id_calidad RowObject.tambores_pedidos ~
RowObject.cantidad_tambores RowObject.kgs_netos_tambores ~
RowObject.kgs_brutos_tambores RowObject.grados_brix RowObject.plazo ~
RowObject.id_tipo_plazo RowObject.id_instrumento_pago ~
RowObject.id_condicion_venta RowObject.id_estado ~
RowObject.id_tipo_contenedor RowObject.contenedores RowObject.pendiente ~
RowObject.cheque RowObject.observaciones RowObject.id_tipo_venta ~
RowObject.id_moneda RowObject.id_tipo_unidad_venta RowObject.importe_origen 
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
     RowObject.item_oe AT ROW 1 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     RowObject.fecha AT ROW 1 COL 32 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.modo_actualizacion AT ROW 1 COL 67 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     RowObject.cert_fito AT ROW 1 COL 100 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.cerrado AT ROW 1 COL 121 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.id_contrato AT ROW 1.95 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     RowObject.item AT ROW 1.95 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     RowObject.anio AT ROW 1.95 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.id_tipo_contrato AT ROW 1.95 COL 66 COLON-ALIGNED
          LABEL "TCont"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.semana_entrega AT ROW 1.95 COL 82 COLON-ALIGNED
          LABEL "Semana"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.id_cliente AT ROW 2.91 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     RowObject.id_articulo AT ROW 3.86 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.id_calidad AT ROW 3.86 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.tambores_pedidos AT ROW 4.81 COL 13 COLON-ALIGNED
          LABEL "Tam.Pedidos"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     RowObject.cantidad_tambores AT ROW 4.81 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.kgs_netos_tambores AT ROW 4.81 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.kgs_brutos_tambores AT ROW 4.81 COL 85 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.grados_brix AT ROW 4.81 COL 117 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     RowObject.plazo AT ROW 5.76 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     RowObject.id_tipo_plazo AT ROW 5.76 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     RowObject.id_instrumento_pago AT ROW 5.76 COL 85 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     RowObject.id_condicion_venta AT ROW 6.71 COL 13 COLON-ALIGNED
          LABEL "Cond.Venta"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     RowObject.id_estado AT ROW 6.71 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.id_tipo_contenedor AT ROW 7.67 COL 13 COLON-ALIGNED
          LABEL "Cont."
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.contenedores AT ROW 7.67 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.pendiente AT ROW 8.62 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.cheque AT ROW 8.62 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.observaciones AT ROW 8.62 COL 75 NO-LABEL
          VIEW-AS EDITOR
          SIZE 50 BY 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 129 BY 11.52.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.id_tipo_venta AT ROW 9.57 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.id_moneda AT ROW 10.52 COL 13 COLON-ALIGNED
          LABEL "CodMoneda" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RowObject.id_tipo_unidad_venta AT ROW 10.52 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.importe_origen AT ROW 11.48 COL 73 COLON-ALIGNED
          LABEL "Importe Contrato" FORMAT ">>,>>9.999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     "Observaciones:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 8.71 COL 59.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 129 BY 11.52.


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
         HEIGHT             = 11.52
         WIDTH              = 129.
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

ASSIGN 
       RowObject.anio:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fecha IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.id_condicion_venta IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_moneda IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.id_tipo_contenedor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_tipo_contrato IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.id_tipo_contrato:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.importe_origen IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       RowObject.item:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.semana_entrega IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.semana_entrega:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.tambores_pedidos IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RowObject.id_contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_contrato vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_contrato IN FRAME F-Main /* Contract */
DO:
    DEFINE VAR hCon AS HANDLE.

    hCon = DYNAMIC-FUNCTION('getDataSource':U).

    RUN parte-contrato IN hCon (INPUT INTEGER(RowObject.ITEM_oe:SCREEN-VALUE IN FRAME F-Main)).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

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

