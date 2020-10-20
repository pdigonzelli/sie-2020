&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dliqlegajos.i"}.



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

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dliqlegajos.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.legajo RowObject.tipo_liquidacion ~
RowObject.apellido_nombre RowObject.liquida RowObject.cuil ~
RowObject.domicilio RowObject.localidad RowObject.codigo_postal ~
RowObject.sexo RowObject.fecha_nacimiento RowObject.fecha_ingreso ~
RowObject.fecha_inicio_temp RowObject.fecha_egreso RowObject.id_categoria 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.legajo RowObject.tipo_liquidacion ~
RowObject.apellido_nombre RowObject.liquida RowObject.cuil ~
RowObject.domicilio RowObject.localidad RowObject.codigo_postal ~
RowObject.sexo RowObject.fecha_nacimiento RowObject.fecha_ingreso ~
RowObject.fecha_inicio_temp RowObject.fecha_egreso RowObject.id_categoria ~
RowObject.descripcion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dliqcargos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqcentroscostos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqcontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqconvenios AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqempresas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqmotivosbajas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dliqsectores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-8 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.legajo AT ROW 2 COL 20 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.tipo_liquidacion AT ROW 2.67 COL 110 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     RowObject.apellido_nombre AT ROW 3 COL 20 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     RowObject.liquida AT ROW 3.86 COL 110 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.cuil AT ROW 4 COL 20 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.domicilio AT ROW 5 COL 20 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     RowObject.localidad AT ROW 6 COL 20 COLON-ALIGNED WIDGET-ID 38 FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
     RowObject.codigo_postal AT ROW 6.05 COL 94.2 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.sexo AT ROW 6.95 COL 54 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     RowObject.fecha_nacimiento AT ROW 7.05 COL 20 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.fecha_ingreso AT ROW 8.1 COL 20 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.fecha_inicio_temp AT ROW 8.14 COL 61 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.fecha_egreso AT ROW 9.33 COL 20 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_categoria AT ROW 11.95 COL 83 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     RowObject.descripcion AT ROW 11.95 COL 97 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     SPACE(2.40) SKIP(2.62)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dliqlegajos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dliqlegajos.i}
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
         HEIGHT             = 15
         WIDTH              = 140.4.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.localidad IN FRAME F-Main
   EXP-FORMAT                                                           */
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
             INPUT  'dliqempresas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedliqempresasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqempresas ).
       RUN repositionObject IN h_dliqempresas ( 1.24 , 100.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelEmpresaSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsid_empresa_liq,descripcionExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_empresa_liqDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_empresa_liq':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 1.00 , 22.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 69.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dliqcentroscostos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsliq_centros_costos.id_empresa_liq,id_empresa_liqObjectNamedliqcentroscostosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqcentroscostos ).
       RUN repositionObject IN h_dliqcentroscostos ( 1.48 , 121.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelCentro CostoSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdescripcion,id_centro_costo_liqExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_centro_costo_liqDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_centro_costo_liq':U ,
             OUTPUT h_dynselect-8 ).
       RUN repositionObject IN h_dynselect-8 ( 13.38 , 21.80 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-8 ( 1.00 , 50.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dliqsectores.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsliq_sectores.id_empresa_liq,id_empresa_liqObjectNamedliqsectoresOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqsectores ).
       RUN repositionObject IN h_dliqsectores ( 2.91 , 126.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdescripcion,id_sector_liqExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_sector_liqDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_sector_liq':U ,
             OUTPUT h_dynselect-5 ).
       RUN repositionObject IN h_dynselect-5 ( 11.00 , 22.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-5 ( 1.00 , 50.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dliqcargos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsliq_cargos.id_empresa_liq,id_empresa_liqObjectNamedliqcargosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqcargos ).
       RUN repositionObject IN h_dliqcargos ( 6.95 , 131.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.40 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelCargoSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdescripcion,id_cargoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_cargoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_cargo':U ,
             OUTPUT h_dynselect-6 ).
       RUN repositionObject IN h_dynselect-6 ( 12.14 , 21.80 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-6 ( 1.00 , 50.60 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dliqconvenios.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedliqconveniosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqconvenios ).
       RUN repositionObject IN h_dliqconvenios ( 6.24 , 117.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 7.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelConvenioSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdescripcion,id_convenioExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_convenioDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_convenio':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 14.57 , 82.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 58.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dliqcontratos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedliqcontratosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqcontratos ).
       RUN repositionObject IN h_dliqcontratos ( 7.43 , 122.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 7.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelContratoSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdescripcion,id_contratoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_contratoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_contrato':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 14.57 , 22.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 38.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dliqmotivosbajas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedliqmotivosbajasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dliqmotivosbajas ).
       RUN repositionObject IN h_dliqmotivosbajas ( 7.91 , 109.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelMotivo BajaSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdescripcion,id_motivo_bajaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_motivo_egresoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_motivo_baja':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 9.33 , 63.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 50.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dliqempresas , 'Data':U , h_dynselect ).

       /* Links to SmartDataObject h_dliqcentroscostos. */
       RUN addLink ( h_dliqempresas , 'Data':U , h_dliqcentroscostos ).

       /* Links to SmartDataField h_dynselect-8. */
       RUN addLink ( h_dliqcentroscostos , 'Data':U , h_dynselect-8 ).

       /* Links to SmartDataObject h_dliqsectores. */
       RUN addLink ( h_dliqempresas , 'Data':U , h_dliqsectores ).

       /* Links to SmartDataField h_dynselect-5. */
       RUN addLink ( h_dliqsectores , 'Data':U , h_dynselect-5 ).

       /* Links to SmartDataObject h_dliqcargos. */
       RUN addLink ( h_dliqempresas , 'Data':U , h_dliqcargos ).

       /* Links to SmartDataField h_dynselect-6. */
       RUN addLink ( h_dliqcargos , 'Data':U , h_dynselect-6 ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_dliqconvenios , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_dliqcontratos , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dliqmotivosbajas , 'Data':U , h_dynselect-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.legajo:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             RowObject.fecha_egreso:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-5 ,
             h_dynselect-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-6 ,
             RowObject.descripcion:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-8 ,
             h_dynselect-6 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-8 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             h_dynselect-3 , 'AFTER':U ).
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

