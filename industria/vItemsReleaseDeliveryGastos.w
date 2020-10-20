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
       {"ditemsreleasedeliverygastos.i"}.


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

DEFINE VAR hContainer AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "ditemsreleasedeliverygastos.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.numero_release ~
RowObject.observaciones 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS nro_proforma 
&Scoped-Define DISPLAYED-FIELDS RowObject.id_contrato ~
RowObject.item_contrato RowObject.numero_release RowObject.fecha_entrega ~
RowObject.nombre RowObject.nombre-2 RowObject.descripcion-6 ~
RowObject.descripcion-3 RowObject.descripcion RowObject.descripcion-4 ~
RowObject.plazo RowObject.comision_broker RowObject.descripcion-7 ~
RowObject.id_lote RowObject.tambores RowObject.kilos_netos ~
RowObject.kilos_brutos RowObject.descripcion-2 RowObject.descripcion-5 ~
RowObject.observaciones 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS nro_proforma lote_deposito 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE lote_deposito AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE nro_proforma AS CHARACTER FORMAT "X(20)":U 
     LABEL "Proforma" 
     VIEW-AS FILL-IN 
     SIZE 16.2 BY 1
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_contrato AT ROW 1 COL 18 COLON-ALIGNED
          LABEL "Contract Nº" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     RowObject.item_contrato AT ROW 1 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7.8 BY 1
     RowObject.numero_release AT ROW 1.95 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     nro_proforma AT ROW 2.91 COL 18 COLON-ALIGNED
     RowObject.fecha_entrega AT ROW 3.86 COL 18 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.nombre AT ROW 4.86 COL 18 COLON-ALIGNED
          LABEL "Client"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.nombre-2 AT ROW 5.86 COL 18 COLON-ALIGNED
          LABEL "Broker"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.descripcion-6 AT ROW 6.91 COL 18 COLON-ALIGNED
          LABEL "Prod. Description"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.descripcion-3 AT ROW 7.86 COL 18 COLON-ALIGNED
          LABEL "Destino"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.descripcion AT ROW 8.81 COL 18 COLON-ALIGNED
          LABEL "Terms of Sale"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.descripcion-4 AT ROW 9.81 COL 18 COLON-ALIGNED
          LABEL "Terms of Payment"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.plazo AT ROW 10.76 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.comision_broker AT ROW 11.71 COL 18 COLON-ALIGNED
          LABEL "Commission"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.descripcion-7 AT ROW 12.67 COL 18 COLON-ALIGNED
          LABEL "Moneda" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.id_lote AT ROW 13.62 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lote_deposito AT ROW 13.62 COL 35 COLON-ALIGNED NO-LABEL
     RowObject.tambores AT ROW 14.57 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.kilos_netos AT ROW 15.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.kilos_brutos AT ROW 16.48 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.descripcion-2 AT ROW 17.43 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.descripcion-5 AT ROW 18.38 COL 18 COLON-ALIGNED
          LABEL "Unidad"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.observaciones AT ROW 19.81 COL 2 NO-LABEL
          VIEW-AS EDITOR
          SIZE 50 BY 2.24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 51 BY 21.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "ditemsreleasedeliverygastos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {ditemsreleasedeliverygastos.i}
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
         HEIGHT             = 21.19
         WIDTH              = 51.
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

/* SETTINGS FOR FILL-IN RowObject.comision_broker IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.comision_broker:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.descripcion:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.descripcion-2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion-3 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.descripcion-3:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion-4 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.descripcion-4:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion-5 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.descripcion-5:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion-6 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.descripcion-6:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion-7 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       RowObject.descripcion-7:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fecha_entrega IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.fecha_entrega:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_contrato IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       RowObject.id_contrato:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_lote IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_lote:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.item_contrato IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.item_contrato:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.kilos_brutos IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.kilos_brutos:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.kilos_netos IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.kilos_netos:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN lote_deposito IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.nombre IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.nombre:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.nombre-2 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.nombre-2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.plazo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.plazo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.tambores IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.tambores:READ-ONLY IN FRAME F-Main        = TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-datos vTableWin 
PROCEDURE carga-datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pProforma AS CHAR.
DEFINE INPUT PARAMETER pLoteDeposito AS CHAR.

nro_proforma:SCREEN-VALUE IN FRAME F-Main = pProforma.
lote_deposito:SCREEN-VALUE IN FRAME F-Main = pLoteDeposito.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changeCursor vTableWin 
PROCEDURE changeCursor :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcCursor AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcCursor).

  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable vTableWin 
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.
  DEFINE VAR vProf AS CHAR.
  DEFINE VAR vLote AS CHAR.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).


  {get DataSource hContainer}.
   IF VALID-HANDLE(hContainer) THEN DO:
        
       vProf = DYNAMIC-FUNCTION('cargo-proforma':U IN hContainer) NO-ERROR.
       vLote = DYNAMIC-FUNCTION('cargo-lote':U IN hContainer) NO-ERROR.

       nro_proforma:SCREEN-VALUE IN FRAME F-Main = vProf.
       lote_deposito:SCREEN-VALUE IN FRAME F-Main = vLote.

   END.

  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE queryPosition vTableWin 
PROCEDURE queryPosition :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcState AS CHARACTER NO-UNDO.
DEFINE VAR vProf AS CHAR.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcState).

  /*
   {get DataSource hContainer}.
   IF VALID-HANDLE(hContainer) THEN DO:
        vProf = DYNAMIC-FUNCTION('cargo-proforma':U IN hContainer) NO-ERROR.
        MESSAGE "QueryPsition " vProf VIEW-AS ALERT-BOX.
        nro_proforma:SCREEN-VALUE IN FRAME F-Main = vProf.
        MESSAGE nro_proforma:SCREEN-VALUE IN FRAME F-Main VIEW-AS ALERT-BOX.
        /*RUN carga-datos (vProf,"NONE").
        MESSAGE "2 " nro_proforma:SCREEN-VALUE IN FRAME F-Main VIEW-AS ALERT-BOX.
        */
   END.
    */

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetRecord vTableWin 
PROCEDURE resetRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valueChanged vTableWin 
PROCEDURE valueChanged :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

   
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

