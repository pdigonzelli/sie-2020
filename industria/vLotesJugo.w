&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dLotesJugo.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dLotesJugo.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.id_sucursal RowObject.Fecha ~
RowObject.Pulpa RowObject.concentracion_mesh RowObject.Jugo_pomelo ~
RowObject.id_condicion_origen RowObject.id_tanque RowObject.Balanza_usada ~
RowObject.Peso_neto RowObject.Calibracion RowObject.Control_pesas ~
RowObject.kilos_jugo_linea RowObject.cantidad_tambores_recup ~
RowObject.cantidad_envases_nuevo RowObject.hora_aprobacion ~
RowObject.id_tanque_sobrante RowObject.id_camara ~
RowObject.etiqueta_adicional RowObject.etiq_adicional_descripcion ~
RowObject.capataz RowObject.envasadores 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-20 RECT-22 RECT-28 RECT-29 RECT-30 
&Scoped-Define DISPLAYED-FIELDS RowObject.id_empresa RowObject.id_sucursal ~
RowObject.Fecha RowObject.codigo_lote RowObject.Pulpa ~
RowObject.concentracion_mesh RowObject.Jugo_pomelo ~
RowObject.id_condicion_origen RowObject.id_tanque RowObject.Balanza_usada ~
RowObject.Peso_neto RowObject.Calibracion RowObject.Control_pesas ~
RowObject.kilos_jugo_linea RowObject.cantidad_tambores_recup ~
RowObject.cantidad_envases_nuevo RowObject.hora_aprobacion ~
RowObject.id_tanque_sobrante RowObject.id_camara ~
RowObject.etiqueta_adicional RowObject.etiq_adicional_descripcion ~
RowObject.capataz RowObject.envasadores RowObject.Hora_comienzo ~
RowObject.Fecha_comienzo RowObject.Hora_comienzo_envase ~
RowObject.Fecha_comienzo_envase RowObject.Hora_finalizacion ~
RowObject.Fecha_finalizacion RowObject.Hora_fin_envase ~
RowObject.Fecha_finalizacion_envase RowObject.id_lote RowObject.anio 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkCierreLote vTableWin 
FUNCTION checkCierreLote RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroLote vTableWin 
FUNCTION getNextNroLote RETURNS CHARACTER
  (piSuc AS INTEGER,
   piArt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcalidades AS HANDLE NO-UNDO.
DEFINE VARIABLE h_denvasesprod AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dproductosterminados AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-5 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 5.95.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 42 BY 8.1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37.4 BY 5.95.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52.4 BY 8.1.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 14.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_empresa AT ROW 1.24 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_sucursal AT ROW 2.33 COL 14 COLON-ALIGNED
          LABEL "Sucursal"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Fecha AT ROW 3.43 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.codigo_lote AT ROW 6.71 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     RowObject.Pulpa AT ROW 10 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.concentracion_mesh AT ROW 11.05 COL 14 COLON-ALIGNED
          LABEL "Conc. Mesh"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Jugo_pomelo AT ROW 12.1 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_condicion_origen AT ROW 13.24 COL 16 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Solo Jugo Linea", 1,
"Jugo Linea + Reproc", 2
          SIZE 24.8 BY 1.62 TOOLTIP "Condicion de Origenes"
     RowObject.id_tanque AT ROW 7.19 COL 68 COLON-ALIGNED
          LABEL "Tanque #"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Balanza_usada AT ROW 8.33 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     RowObject.Peso_neto AT ROW 9.43 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.Calibracion AT ROW 10.57 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.Control_pesas AT ROW 11.71 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.kilos_jugo_linea AT ROW 12.86 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cantidad_tambores_recup AT ROW 1.19 COL 134 COLON-ALIGNED
          LABEL "Envases Recup"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.cantidad_envases_nuevo AT ROW 2.29 COL 134 COLON-ALIGNED
          LABEL "Envases Nuevos"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.hora_aprobacion AT ROW 3.38 COL 134 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.id_tanque_sobrante AT ROW 4.52 COL 127 COLON-ALIGNED
          LABEL "Sbte a Tanque"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_camara AT ROW 5.62 COL 127 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.etiqueta_adicional AT ROW 7.19 COL 106 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.etiq_adicional_descripcion AT ROW 7.19 COL 115 NO-LABEL
          VIEW-AS EDITOR
          SIZE 30 BY 2.86
     RowObject.capataz AT ROW 10.33 COL 107 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     RowObject.envasadores AT ROW 11.48 COL 109 NO-LABEL
          VIEW-AS EDITOR
          SIZE 36 BY 3.33
     RowObject.Hora_comienzo AT ROW 1.24 COL 65 COLON-ALIGNED
          LABEL "Hr Comienzo"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.Fecha_comienzo AT ROW 2.33 COL 65 COLON-ALIGNED
          LABEL "Fec Com"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.Hora_comienzo_envase AT ROW 3.43 COL 65 COLON-ALIGNED
          LABEL "Hr Com Env"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 145.4 BY 14.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Fecha_comienzo_envase AT ROW 4.48 COL 65 COLON-ALIGNED
          LABEL "Fec Com Env"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.Hora_finalizacion AT ROW 1.24 COL 92.4 COLON-ALIGNED
          LABEL "Hr Fin"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.Fecha_finalizacion AT ROW 2.33 COL 92.4 COLON-ALIGNED
          LABEL "Fec Fin"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.Hora_fin_envase AT ROW 3.43 COL 92.4 COLON-ALIGNED
          LABEL "Hr Fin Env"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.Fecha_finalizacion_envase AT ROW 4.48 COL 92.4 COLON-ALIGNED
          LABEL "Fec Fin Env"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.id_lote AT ROW 5.62 COL 14 COLON-ALIGNED
          LABEL "Lote"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.anio AT ROW 5.62 COL 35.6 COLON-ALIGNED
          LABEL "Año"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RECT-20 AT ROW 1 COL 52
     RECT-22 AT ROW 6.95 COL 52
     RECT-28 AT ROW 1 COL 109
     RECT-29 AT ROW 6.95 COL 94
     RECT-30 AT ROW 1 COL 1
     "Origenes" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 13.62 COL 4
     "Envasadores" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 11.48 COL 94.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 145.4 BY 14.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dLotesJugo.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dLotesJugo.i}
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
         HEIGHT             = 14.1
         WIDTH              = 145.4.
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
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.cantidad_envases_nuevo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cantidad_tambores_recup IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.codigo_lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.concentracion_mesh IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Fecha_comienzo IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Fecha_comienzo_envase IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Fecha_finalizacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Fecha_finalizacion_envase IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Hora_comienzo IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Hora_comienzo_envase IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Hora_finalizacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Hora_fin_envase IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.id_empresa IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_lote IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.id_sucursal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_tanque IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_tanque_sobrante IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.etiqueta_adicional
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.etiqueta_adicional vTableWin
ON LEAVE OF RowObject.etiqueta_adicional IN FRAME F-Main /* EtiqAdicional */
DO:
  RUN customEnable.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Fecha vTableWin
ON LEAVE OF RowObject.Fecha IN FRAME F-Main /* Fecha */
DO:
  rowObject.anio:SCREEN-VALUE = STRING(YEAR(DATE(rowObject.fecha:SCREEN-VALUE))).
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

  RUN customEnable.
  RUN filterArticulos.
  RUN filterCalidades.

  rowObject.id_empresa:SCREEN-VALUE IN FRAME F-Main = "1".
  rowObject.fecha:SCREEN-VALUE IN FRAM F-Main       = STRING(TODAY).


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
             INPUT  'dcalidades.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcalidadesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcalidades ).
       RUN repositionObject IN h_dcalidades ( 1.24 , 40.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldCodCalidadKeyFieldid_calidadDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelCalidadSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleCalidadesBrowseFieldsCodCalidad,id_calidadExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_calidadDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 7.81 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 34.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'denvasesprod.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedenvasesprodUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_denvasesprod ).
       RUN repositionObject IN h_denvasesprod ( 2.91 , 40.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldEnvaseKeyFieldid_envaseDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelEnvaseSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleEnvasesBrowseFieldsEnvase,id_envaseExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_envaseDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 8.91 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 34.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dproductosterminados.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedproductosterminadosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dproductosterminados ).
       RUN repositionObject IN h_dproductosterminados ( 11.71 , 39.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventfilterEnvasesDisplayedFieldArticuloKeyFieldid_articuloDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelArticuloSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleArticulosBrowseFieldsArticulo,id_articuloExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_articuloDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-5 ).
       RUN repositionObject IN h_dynselect-5 ( 4.57 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-5 ( 1.00 , 34.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dcalidades , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_denvasesprod , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect-5. */
       RUN addLink ( h_dproductosterminados , 'Data':U , h_dynselect-5 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-5 ,
             RowObject.Fecha:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.codigo_lote:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             h_dynselect , 'AFTER':U ).
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
    

  IF STRING(rowObject.etiqueta_adicional:SCREEN-VALUE IN FRAME F-Main) = "SI" THEN
    rowObject.etiq_adicional_descripcion:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
  ELSE
    rowObject.etiq_adicional_descripcion:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord vTableWin 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  DEFINE VARIABLE h AS HANDLE NO-UNDO.
  h = DYNAMIC-FUNCTION('getContainerSource').
  RUN mailDeposito IN h ("delete").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN customEnable.
  /*
  RUN filterArticulos.
  RUN filterCalidades.
  RUN filterEnvases ("").*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterArticulos vTableWin 
PROCEDURE filterArticulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cList AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry  AS CHARACTER  NO-UNDO.

  cList = "36,70,71,971,30,33,34,52,53,88,244,245,246,247,523,524,544,535,550,~
           600,801,802,42,43,46,49,62,63,66,624,942,946,962,~
           966".

  cQry = DYNAMIC-FUNCTION('getFilterString' IN hLib, cList, 'OR', 'id_articulo').

  DYNAMIC-FUNCTION('setQueryWhere' IN h_dProductosTerminados, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dProductosTerminados).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterCalidades vTableWin 
PROCEDURE filterCalidades :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cList AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry  AS CHARACTER  NO-UNDO.

  cList = "51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,~
           70,71,77,80,81,82,83,84,85,86,87,108,109,250,311,312,~
           313,314,315,316,317,323,324,325,326,327,328,329,330,331,~
           332,334,335,336,337,338,339,340,341,342,350,400,401,403,~
           410,411,412,413,414,415,416,417,418,419,420,421,422,423,~
           424,425,426,427,428,429,550,612,629,669,644,795,796".
  cQry = DYNAMIC-FUNCTION('getFilterString' IN hLib, cList, 'OR', 'id_calidad').

  DYNAMIC-FUNCTION('setQueryWhere' IN h_dCalidades, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dCalidades).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterEnvases vTableWin 
PROCEDURE filterEnvases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcPickedValue AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cLst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.


  cLst = "9999".    
  
  IF pcPickedValue = '52' THEN DO:
    cLst = "21,503,506,518,520,528,532,533,534,535,555,553,558,560,561,562,563".    
  END.

  IF pcPickedValue = '53' THEN DO:
    cLst = "21,503,506,507,518,519,520,521,522,523,533,534,534,535,555,556,558,560,561,562,563".    
  END.

  IF pcPickedValue = '521' THEN DO:
    cLst = "21,503,534,538,555,556,560,561,562".
  END.

  IF pcPickedValue = '88'  OR
     pcPickedValue = '523' OR 
     pcPickedValue = '524' OR
     pcPickedValue = '534' OR
     pcPickedValue = '801' OR
     pcPickedValue = '802' OR
     pcPickedValue = '882' THEN DO:
    cLst = "21,531,556".
  END.

  IF pcPickedValue = '532' OR 
     pcPickedValue = '600' THEN DO:
    cLst = "531,538,556".
  END.

  IF pcPickedValue = '952' OR 
     pcPickedValue = '953' THEN DO:
    cLst = "503,531,555,556".
  END.


  IF pcPickedValue = '533' THEN DO:
    cLst = "556".
  END.

  IF pcPickedValue = '36' THEN DO:
    cLst = "503".
  END.

  IF pcPickedValue = '71' THEN DO:
    cLst = "503,528,556,559".
  END.

   IF pcPickedValue = '30' OR
      pcPickedValue = '33' OR 
      pcPickedValue = '34' THEN DO:
    cLst = "540".
  END.
  
  cQry = DYNAMIC-FUNCTION('getFilterString' IN hLib, cLst, 'OR', 'envases_prod.id_envase').
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dEnvasesProd, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dEnvasesProd).


  getNextNroLote(INTEGER(rowObject.id_sucursal:SCREEN-VALUE IN FRAME F-Main), 
                 INTEGER(pcPickedValue)).

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

  RUN libCommonFunctions.p PERSISTENT SET hLib.
  
  /*
  RUN filterArticulos.
  RUN filterCalidades.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN filterCalidades.
  RUN filterEnvases.

  RUN disableField IN h_dynselect-5.



  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateState vTableWin 
PROCEDURE updateState :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcState AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcState).

 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkCierreLote vTableWin 
FUNCTION checkCierreLote RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.

  hSource = DYNAMIC-FUNCTION('getDataSource').

  IF DYNAMIC-FUNCTION('columnValue' IN hSource, 'fecha_finalizacion') <> ? THEN
    RETURN TRUE.
  ELSE
    RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroLote vTableWin 
FUNCTION getNextNroLote RETURNS CHARACTER
  (piSuc AS INTEGER,
   piArt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cLot    AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  cLot = DYNAMIC-FUNCTION('getNextNroLote' IN hLibTam, piSuc, 3, piArt, YEAR(TODAY)).
  rowObject.codigo_lote:SCREEN-VALUE IN FRAME F-Main = cLot.
  cLot = ENTRY(4, cLot, ".").

  rowObject.id_lote:SCREEN-VALUE IN FRAME F-Main = ENTRY(1, cLot, "/").
  rowObject.anio:SCREEN-VALUE IN FRAME F-Main    = STRING(YEAR(TODAY)).

  
  RETURN cLot.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

