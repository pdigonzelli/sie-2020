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

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

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
&Scoped-Define ENABLED-FIELDS RowObject.fecha RowObject.contenedores ~
RowObject.cantidad_tambores RowObject.cert_fito RowObject.grados_brix ~
RowObject.pendiente 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS cmbPartesCont RECT-39 RECT-40 
&Scoped-Define DISPLAYED-FIELDS RowObject.id_orden_entrega ~
RowObject.item_oe RowObject.fecha RowObject.contenedores ~
RowObject.tambores_pedidos RowObject.cantidad_tambores RowObject.cert_fito ~
RowObject.modo_actualizacion RowObject.grados_brix RowObject.item ~
RowObject.anio RowObject.id_tipo_contrato RowObject.semana_entrega ~
RowObject.pendiente 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS cmbPartesCont 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dclausulas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_destadosoe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dproductosterminados AS HANDLE NO-UNDO.
DEFINE VARIABLE h_drproductocalidad AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtipocontenedor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtipomoneda AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-8 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbPartesCont AS CHARACTER FORMAT "X(256)":U 
     LABEL "Parte Cont." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 65 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 10.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55.2 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_orden_entrega AT ROW 1.24 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.item_oe AT ROW 1.24 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.fecha AT ROW 1.24 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     cmbPartesCont AT ROW 4.05 COL 12 COLON-ALIGNED
     RowObject.contenedores AT ROW 2.24 COL 122 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.tambores_pedidos AT ROW 3.33 COL 122 COLON-ALIGNED
          LABEL "Tbs pedidos"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.cantidad_tambores AT ROW 4.38 COL 122 COLON-ALIGNED
          LABEL "Tbs Asociados"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.cert_fito AT ROW 5.48 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.modo_actualizacion AT ROW 6.48 COL 92 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.grados_brix AT ROW 9.81 COL 118 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     RowObject.item AT ROW 4.1 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.anio AT ROW 10.05 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.id_tipo_contrato AT ROW 10.05 COL 34 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.semana_entrega AT ROW 5.29 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.pendiente AT ROW 6.48 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RECT-39 AT ROW 1 COL 1
     RECT-40 AT ROW 1 COL 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 136.2 BY 10.1.


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
         HEIGHT             = 10.1
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
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.anio IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.anio:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.cantidad_tambores IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_orden_entrega IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_tipo_contrato IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_tipo_contrato:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.item IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.item:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.item_oe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.modo_actualizacion IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.modo_actualizacion:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.semana_entrega IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.semana_entrega:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.tambores_pedidos IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
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

&Scoped-define SELF-NAME cmbPartesCont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbPartesCont vTableWin
ON VALUE-CHANGED OF cmbPartesCont IN FRAME F-Main /* Parte Cont. */
DO:
  DEFINE VARIABLE iCla AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iArt AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTam AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iMon AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSem AS INTEGER    NO-UNDO.

  iSem = DYNAMIC-FUNCTION('getSemanaEntrega' IN hLib, DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato'),  
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato'), 
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio'), 
                                                                                                        SELF:SCREEN-VALUE). 

  iTam = DYNAMIC-FUNCTION('getTamboresPedidos' IN hLib, DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato'),  
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato'), 
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio'), 
                                                                                                        SELF:SCREEN-VALUE). 
  
  iCla = DYNAMIC-FUNCTION('getCondicionVenta' IN hLib, DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato'),  
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato'), 
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio'), 
                                                                                                        SELF:SCREEN-VALUE). 

  iArt = DYNAMIC-FUNCTION('getArticulo' IN hLib, DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato'),  
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato'), 
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio'), 
                                                                                                        SELF:SCREEN-VALUE). 

  iCal = DYNAMIC-FUNCTION('getCalidad' IN hLib, DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato'),  
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato'), 
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio'), 
                                                                                                        SELF:SCREEN-VALUE). 

  iMon = DYNAMIC-FUNCTION('getMonedaVenta' IN hLib, DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato'),  
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato'), 
                                                                                                        DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio'), 
                                                                                                        SELF:SCREEN-VALUE). 


  rowObject.ITEM:SCREEN-VALUE              = SELF:SCREEN-VALUE.
  rowObject.semana_entrega:SCREEN-VALUE    = STRING(iSem).
  rowObject.tambores_pedidos:SCREEN-VALUE  = STRING(iTam).
    
  RUN refreshSdo ("productos_terminados", 
                  "productos_terminados.id_articulo = " + STRING(iArt), 
                  h_dProductosTerminados).

  RUN refreshSds (DYNAMIC-FUNCTION('columnValue' IN h_dProductosTerminados, 'id_articulo'), 
                  h_dynselect-2).
  RUN disableField IN h_dynselect-2.

  RUN refreshSdo ("clausulas", 
                  "clausulas.id_clausula = " + STRING(iCla), 
                  h_dClausulas).

  RUN refreshSds (DYNAMIC-FUNCTION('columnValue' IN h_dClausulas, 'id_clausula'), 
                  h_dynselect-4).
  RUN disableField IN h_dynselect-4.

  RUN refreshSdo ("r_productos_calidad", 
                  "r_productos_calidad.id_calidad = " + STRING(iCal), 
                  h_drProductoCalidad).

  RUN refreshSds (DYNAMIC-FUNCTION('columnValue' IN h_drProductoCalidad, 'id_calidad'), 
                  h_dynselect-3).
  RUN disableField IN h_dynselect-3.

  RUN refreshSdo ("tipo_moneda", 
                  "tipo_moneda.id_moneda = " + STRING(iMon), 
                  h_dTipoMoneda).

  RUN refreshSds (DYNAMIC-FUNCTION('columnValue' IN h_dTipoMoneda, 'id_moneda'), 
                  h_dynselect-8).
  RUN disableField IN h_dynselect-8.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libOrdenEntrega.p').
  DELETE OBJECT hLibCom.

  rowObject.ITEM_oe:SCREEN-VALUE IN FRAME f-Main = DYNAMIC-FUNCTION('getNextItemOE' IN hLib, rowObject.id_orden_entrega:SCREEN-VALUE).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'destadosoe.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedestadosoeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_destadosoe ).
       RUN repositionObject IN h_destadosoe ( 2.43 , 85.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_estadoDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelEstadoSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleEstados OEBrowseFieldsdescripcion,id_estadoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_estadoDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-6 ).
       RUN repositionObject IN h_dynselect-6 ( 7.67 , 100.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-6 ( 1.00 , 35.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dclausulas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedclausulasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dclausulas ).
       RUN repositionObject IN h_dclausulas ( 9.10 , 53.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldClausulaKeyFieldid_clausulaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelCond. Vta.SortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleClausulasBrowseFieldsClausula,id_clausulaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_condicion_ventaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 6.00 , 14.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 65.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcontratos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcontratosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcontratos ).
       RUN repositionObject IN h_dcontratos ( 9.10 , 89.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventfillItemsContratoDisplayedFieldcontratoClienteKeyFieldid_contratoDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelContratoSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleContratosBrowseFieldsid_contrato,contratoClienteExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_contratoDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 2.91 , 14.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 65.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dproductosterminados.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedproductosterminadosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dproductosterminados ).
       RUN repositionObject IN h_dproductosterminados ( 9.10 , 76.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldArticuloKeyFieldid_articuloDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelArticuloSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleArticulosBrowseFieldsArticulo,id_articuloExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_articuloDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 7.91 , 14.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 65.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'drproductocalidad.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsr_productos_calidad.id_articulo,id_articuloRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedrproductocalidadUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_drproductocalidad ).
       RUN repositionObject IN h_drproductocalidad ( 9.10 , 63.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldCalidadKeyFieldid_calidadDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelCalidadSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleCalidadesBrowseFieldsCalidad,id_calidadExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_calidadDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 9.10 , 14.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 65.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtipocontenedor.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtipocontenedorUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtipocontenedor ).
       RUN repositionObject IN h_dtipocontenedor ( 9.10 , 42.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_tipo_contenedorDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelTipo Cont.SortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleTipos ContenedorBrowseFieldsdescripcion,id_tipo_contenedorExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_tipo_contenedorDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-5 ).
       RUN repositionObject IN h_dynselect-5 ( 1.24 , 99.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-5 ( 1.00 , 36.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtipomoneda.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtipomonedaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtipomoneda ).
       RUN repositionObject IN h_dtipomoneda ( 9.10 , 99.00 ) NO-ERROR.
       /* Size in AB:  ( 1.71 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_monedaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelMoneda CbioSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleTipos MonedaBrowseFieldsdescripcion,id_monedaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_moneda_cambioDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-8 ).
       RUN repositionObject IN h_dynselect-8 ( 8.71 , 100.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-8 ( 1.00 , 35.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-6. */
       RUN addLink ( h_destadosoe , 'Data':U , h_dynselect-6 ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_dclausulas , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dcontratos , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dproductosterminados , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataObject h_drproductocalidad. */
       RUN addLink ( h_dproductosterminados , 'Data':U , h_drproductocalidad ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_drproductocalidad , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-5. */
       RUN addLink ( h_dtipocontenedor , 'Data':U , h_dynselect-5 ).

       /* Links to SmartDataField h_dynselect-8. */
       RUN addLink ( h_dtipomoneda , 'Data':U , h_dynselect-8 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-6 ,
             RowObject.id_orden_entrega:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.fecha:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             cmbPartesCont:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             h_dynselect-4 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-8 ,
             h_dynselect-3 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-5 ,
             RowObject.modo_actualizacion:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customEnable vTableWin 
PROCEDURE customEnable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER plState AS LOGICAL    NO-UNDO.

  cmbPartesCont:SENSITIVE IN FRAME F-Main = plState.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableFields vTableWin 
PROCEDURE disableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcFieldType AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcFieldType).

  RUN customEnable(FALSE).

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

  DEFINE VARIABLE iLoop          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iNumEntries    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cFieldHandles  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCont          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hTableioSource AS HANDLE    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libContratos.p').
  DELETE OBJECT hLibCom.


  ASSIGN cFieldHandles  = DYNAMIC-FUNCTION('getAllFieldHandles')
         iNumEntries    = NUM-ENTRIES(cFieldHandles)
         hTableioSource = DYNAMIC-FUNCTION('getTableIOSource').

  DO iLoop = 1 TO iNumEntries:
    ASSIGN hField = WIDGET-HANDLE(ENTRY(iLoop,cFieldHandles)).

    IF NOT VALID-HANDLE(hField) THEN RETURN.

    IF hField:NAME = "item"  THEN DO:
      cCont = DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_contrato') NO-ERROR.
      RUN fillItemsContrato (cCont) .
      LEAVE.
    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN customEnable(TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillItemsContrato vTableWin 
PROCEDURE fillItemsContrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcContrato AS CHARACTER  NO-UNDO.


  DEFINE VARIABLE cItm AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLbl AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  
  cItm = DYNAMIC-FUNCTION('getItemsContrato' IN hLib, pcContrato, 
                                                      DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio'), 
                                                      DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato')).
  IF cItm = "" THEN RETURN.

  DO WHILE cmbPartesCont:DELETE ( 1 ) IN FRAME f-Main: /* empty statement */ END.
  DO i = 1 TO NUM-ENTRIES(cItm):
    cRow = ENTRY(i, cItm).
    cLbl = ENTRY(2, cRow, "-").
    cVal = ENTRY(1, cRow, "-").
    cmbPartesCont:ADD-LAST(cLbl, cVal) IN FRAME f-Main.
    cLbl = "".
    cVal = "".
  END.

  cmbPartesCont:SCREEN-VALUE IN FRAME f-Main               = rowObject.ITEM:SCREEN-VALUE NO-ERROR.
  rowObject.id_tipo_contrato:SCREEN-VALUE IN FRAME f-Main  = DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'id_tipo_contrato').
  rowObject.anio:SCREEN-VALUE IN FRAME f-Main              = DYNAMIC-FUNCTION('columnValue' IN h_dContratos, 'anio').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN customEnable(FALSE).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshSdo vTableWin 
PROCEDURE refreshSdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcTable AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcWhere AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER phSdo   AS HANDLE     NO-UNDO.

  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.

  cQry = "FOR EACH " + pcTable + " WHERE " + pcWhere.

  DYNAMIC-FUNCTION('setQueryWhere' IN phSdo, cQry).
  DYNAMIC-FUNCTION('openQuery' IN phSdo).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshSds vTableWin 
PROCEDURE refreshSds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE INPUT  PARAMETER pcValue AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER phSds   AS HANDLE     NO-UNDO.

  DYNAMIC-FUNCTION('setDataValue' IN phSds, pcValue).
  RUN valueChanged IN phSds.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

