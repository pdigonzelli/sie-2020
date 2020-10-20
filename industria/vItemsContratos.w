&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

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
&Scoped-Define ENABLED-FIELDS RowObject.item RowObject.fecha ~
RowObject.semana_entrega RowObject.semana_entrega_hasta ~
RowObject.anio_semana_entrega RowObject.cantidad ~
RowObject.id_articulo_cliente1 RowObject.id_po_cliente1 ~
RowObject.embarque_estimado RowObject.arribo_estimado ~
RowObject.precio_base_calculo RowObject.precio_comision ~
RowObject.comision_broker RowObject.comision_otros ~
RowObject.numero_release1 RowObject.estado RowObject.pendiente ~
RowObject.cert_fito RowObject.marca_tambores RowObject.observaciones ~
RowObject.precio_venta 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 
&Scoped-Define DISPLAYED-FIELDS RowObject.item RowObject.fecha ~
RowObject.semana_entrega RowObject.semana_entrega_hasta ~
RowObject.anio_semana_entrega RowObject.cantidad ~
RowObject.id_articulo_cliente1 RowObject.id_po_cliente1 ~
RowObject.embarque_estimado RowObject.arribo_estimado ~
RowObject.precio_base_calculo RowObject.precio_comision ~
RowObject.comision_broker RowObject.comision_otros ~
RowObject.numero_release1 RowObject.estado RowObject.pendiente ~
RowObject.cert_fito RowObject.marca_tambores RowObject.observaciones ~
RowObject.precio_venta 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcalidades AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dclausulas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddestinos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddestinos-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddestinos-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_denvasesprod AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlugardescargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dproductosterminados AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dpuertos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dpuertos-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtipomoneda AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtipounidadventa AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvapores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-10 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-11 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-12 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-13 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-7 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-8 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-9 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 8.81.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 6.91.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 4.05.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 7.62.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 4.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.item AT ROW 1.14 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.fecha AT ROW 1.14 COL 45 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.semana_entrega AT ROW 5.62 COL 16 COLON-ALIGNED
          LABEL "Sem. Ent."
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.semana_entrega_hasta AT ROW 6.62 COL 16 COLON-ALIGNED
          LABEL "Sem. Ent. Hasta"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.anio_semana_entrega AT ROW 7.62 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.cantidad AT ROW 8.62 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     RowObject.id_articulo_cliente1 AT ROW 5.67 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     RowObject.id_po_cliente1 AT ROW 6.71 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     RowObject.embarque_estimado AT ROW 11.05 COL 19 COLON-ALIGNED
          LABEL "Embarque Est"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.arribo_estimado AT ROW 12.1 COL 19 COLON-ALIGNED
          LABEL "Arribo Est"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.precio_base_calculo AT ROW 1.24 COL 84 COLON-ALIGNED
          LABEL "Precio Base (FOB)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.precio_comision AT ROW 2.38 COL 84 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.comision_broker AT ROW 1.24 COL 120 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.comision_otros AT ROW 2.33 COL 120 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.numero_release1 AT ROW 11.24 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     RowObject.estado AT ROW 12.91 COL 83.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     RowObject.pendiente AT ROW 12.91 COL 110 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.cert_fito AT ROW 12.91 COL 133 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.marca_tambores AT ROW 14 COL 85 NO-LABEL
          VIEW-AS EDITOR
          SIZE 56 BY 1.19
     RowObject.observaciones AT ROW 15.33 COL 85 NO-LABEL
          VIEW-AS EDITOR
          SIZE 56 BY 1.19
     RowObject.precio_venta AT ROW 3.48 COL 84 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RECT-3 AT ROW 1 COL 1
     RECT-4 AT ROW 9.81 COL 1
     RECT-5 AT ROW 1 COL 64
     RECT-6 AT ROW 5.05 COL 64
     RECT-7 AT ROW 12.67 COL 64
     "Marca Tambores" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 14.19 COL 68
     "Observaciones" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 15.38 COL 68.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 142 BY 15.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dItemsContratos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 15.71
         WIDTH              = 142.
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

/* SETTINGS FOR FILL-IN RowObject.arribo_estimado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.embarque_estimado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.precio_base_calculo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.semana_entrega IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.semana_entrega_hasta IN FRAME F-Main
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
             INPUT  'dproductosterminados.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedproductosterminadosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dproductosterminados ).
       RUN repositionObject IN h_dproductosterminados ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 11.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldArticuloKeyFieldid_articuloDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelArticuloSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleArticulosBrowseFieldsid_articulo,ArticuloExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_articuloDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 2.19 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 45.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtipomoneda.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtipomonedaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtipomoneda ).
       RUN repositionObject IN h_dtipomoneda ( 1.48 , 105.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldabreviaturaKeyFieldid_monedaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelMonedaSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleMonedasBrowseFieldsabreviatura,id_monedaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_moneda_origenDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-13 ).
       RUN repositionObject IN h_dynselect-13 ( 3.38 , 115.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-13 ( 1.00 , 25.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcalidades.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcalidadesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcalidades ).
       RUN repositionObject IN h_dcalidades ( 2.67 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldQualityKeyFieldid_calidadDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelCalidadSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleCalidadesBrowseFieldsQuality,id_calidadExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_calidadDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 3.38 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 45.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dclausulas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedclausulasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dclausulas ).
       RUN repositionObject IN h_dclausulas ( 3.14 , 64.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldClausulaKeyFieldid_clausulaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelClausulaSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleClausulasBrowseFieldsClausula,id_clausulaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_clausulaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-8 ).
       RUN repositionObject IN h_dynselect-8 ( 5.29 , 88.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-8 ( 1.00 , 53.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'denvasesprod.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedenvasesprodUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_denvasesprod ).
       RUN repositionObject IN h_denvasesprod ( 4.57 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldEnvaseKeyFieldid_envaseDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelEnvaseSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleEnvasesBrowseFieldsEnvase,id_envaseExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_envaseDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 4.57 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 45.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dpuertos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedpuertosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dpuertos ).
       RUN repositionObject IN h_dpuertos ( 6.48 , 64.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldPuertoKeyFieldid_puertoDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelPuerto EntradaSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitlePuertosBrowseFieldsPuerto,id_puertoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_puerto_entDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-9 ).
       RUN repositionObject IN h_dynselect-9 ( 6.48 , 88.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-9 ( 1.00 , 53.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvapores.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedvaporesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dvapores ).
       RUN repositionObject IN h_dvapores ( 8.62 , 64.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldVaporKeyFieldid_vaporDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelVaporSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleVaporesBrowseFieldsVapor,id_vaporExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_vaporDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-10 ).
       RUN repositionObject IN h_dynselect-10 ( 7.67 , 88.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-10 ( 1.00 , 53.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtipounidadventa.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtipounidadventaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtipounidadventa ).
       RUN repositionObject IN h_dtipounidadventa ( 9.81 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_tipo_unidad_ventaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelTipo UnidadSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleUnidadesBrowseFieldsid_tipo_unidad_venta,descripcionExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_tipo_unidad_ventaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-7 ).
       RUN repositionObject IN h_dynselect-7 ( 10.05 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-7 ( 1.00 , 42.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddestinos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameddestinosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ddestinos-3 ).
       RUN repositionObject IN h_ddestinos-3 ( 10.76 , 123.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDestinoKeyFieldid_destinoDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelDestinoSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleDestinosBrowseFieldsDestino,id_destinoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_deposito_finalDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-12 ).
       RUN repositionObject IN h_dynselect-12 ( 10.05 , 88.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-12 ( 1.00 , 53.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dpuertos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedpuertosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dpuertos-2 ).
       RUN repositionObject IN h_dpuertos-2 ( 10.76 , 64.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldPuertoKeyFieldid_puertoDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelPuerto SalidaSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitlePuertosBrowseFieldsPuerto,id_puertoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_puerto_salDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-11 ).
       RUN repositionObject IN h_dynselect-11 ( 8.86 , 88.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-11 ( 1.00 , 53.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dlugardescargas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlugardescargasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dlugardescargas ).
       RUN repositionObject IN h_dlugardescargas ( 11.24 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldLugarDescargaKeyFieldid_lugdesDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelOrigenSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleOrigenesBrowseFieldsLugarDescarga,id_lugdes,id_sucursalExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_fromDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 13.14 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 42.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddestinos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameddestinosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ddestinos ).
       RUN repositionObject IN h_ddestinos ( 12.91 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDestinoKeyFieldid_destinoDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelDestinoSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleDestinosBrowseFieldsDestino,id_destinoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_destinoDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-5 ).
       RUN repositionObject IN h_dynselect-5 ( 14.33 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-5 ( 1.00 , 42.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddestinos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameddestinosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ddestinos-2 ).
       RUN repositionObject IN h_ddestinos-2 ( 14.57 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.67 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDestinoKeyFieldid_destinoDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelDestino FinalSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleDestinosBrowseFieldsDestino,id_destinoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNamedestino_finalDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-6 ).
       RUN repositionObject IN h_dynselect-6 ( 15.52 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-6 ( 1.00 , 42.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dproductosterminados , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-13. */
       RUN addLink ( h_dtipomoneda , 'Data':U , h_dynselect-13 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dcalidades , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect-8. */
       RUN addLink ( h_dclausulas , 'Data':U , h_dynselect-8 ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_denvasesprod , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-9. */
       RUN addLink ( h_dpuertos , 'Data':U , h_dynselect-9 ).

       /* Links to SmartDataField h_dynselect-10. */
       RUN addLink ( h_dvapores , 'Data':U , h_dynselect-10 ).

       /* Links to SmartDataField h_dynselect-7. */
       RUN addLink ( h_dtipounidadventa , 'Data':U , h_dynselect-7 ).

       /* Links to SmartDataField h_dynselect-12. */
       RUN addLink ( h_ddestinos-3 , 'Data':U , h_dynselect-12 ).

       /* Links to SmartDataField h_dynselect-11. */
       RUN addLink ( h_dpuertos-2 , 'Data':U , h_dynselect-11 ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_dlugardescargas , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect-5. */
       RUN addLink ( h_ddestinos , 'Data':U , h_dynselect-5 ).

       /* Links to SmartDataField h_dynselect-6. */
       RUN addLink ( h_ddestinos-2 , 'Data':U , h_dynselect-6 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-13 ,
             RowObject.fecha:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.fecha:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             h_dynselect , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-7 ,
             RowObject.id_po_cliente1:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             RowObject.arribo_estimado:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-5 ,
             h_dynselect-4 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-6 ,
             h_dynselect-5 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-8 ,
             RowObject.comision_otros:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-9 ,
             h_dynselect-8 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-10 ,
             h_dynselect-9 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-11 ,
             h_dynselect-10 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-12 ,
             h_dynselect-11 , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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

