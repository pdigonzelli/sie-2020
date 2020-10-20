&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"ditemsplanillasadictareas.i"}.



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
&Scoped-define DATA-FIELD-DEFS "ditemsplanillasadictareas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.fecha_inicio RowObject.fecha_fin ~
RowObject.cantidad 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.id_planilla_adicional ~
RowObject.item RowObject.fecha_inicio RowObject.fecha_fin ~
RowObject.cantidad 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_ddosistratamiento AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddosistratamiento-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddosistratamiento-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddosistratamiento-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddosistratamiento-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddosistratamiento-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddosistratamiento-7 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddosistratamiento-8 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlotesplantacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dmaqtratamiento AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-10 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-7 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-8 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-9 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_planilla_adicional AT ROW 1 COL 16 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.item AT ROW 1.95 COL 16 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.fecha_inicio AT ROW 4.1 COL 16 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.fecha_fin AT ROW 4.1 COL 48 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cantidad AT ROW 6.52 COL 16 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     SPACE(119.00) SKIP(5.00)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "ditemsplanillasadictareas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {ditemsplanillasadictareas.i}
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
         HEIGHT             = 11.67
         WIDTH              = 157.4.
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

/* SETTINGS FOR FILL-IN RowObject.id_planilla_adicional IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.item IN FRAME F-Main
   NO-ENABLE                                                            */
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
             INPUT  'ddosistratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameddosistratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddosistratamiento-4 ).
       RUN repositionObject IN h_ddosistratamiento-4 ( 1.00 , 38.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-tratamientoDataSourceFilterdosis_tratamiento.id_tipo_prod_tratamiento = 4NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDosis-4ASortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,desc-prod,desc-ing,valor_dosis,desc-dosisExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_dosis-4DisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_dosis_tratamiento':U ,
             OUTPUT h_dynselect-7 ).
       RUN repositionObject IN h_dynselect-7 ( 11.48 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-7 ( 1.00 , 62.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddosistratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameddosistratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddosistratamiento-3 ).
       RUN repositionObject IN h_ddosistratamiento-3 ( 1.24 , 52.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-tratamientoDataSourceFilterdosis_tratamiento.id_tipo_prod_tratamiento = 3NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDosis-3ASortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,desc-prod,desc-ing,valor_dosis,desc-dosisExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_dosis-3DisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_dosis_tratamiento':U ,
             OUTPUT h_dynselect-6 ).
       RUN repositionObject IN h_dynselect-6 ( 10.05 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-6 ( 1.00 , 62.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dmaqtratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedmaqtratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dmaqtratamiento ).
       RUN repositionObject IN h_dmaqtratamiento ( 1.24 , 64.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-maquinaDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,nro_maquinariaExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_maquinaria_tratamientoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_maquinaria_tratamiento':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 5.52 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 38.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dlotesplantacion.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedlotesplantacionOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dlotesplantacion ).
       RUN repositionObject IN h_dlotesplantacion ( 1.48 , 70.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldcodigo_trazabilidadDataSourceFilterlotes_plantacion.estado = yesNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelTrazabilidadSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldscodigo_trazabilidad,descripcion-2,descripcion,id_proveedor,id_origen,id_loteExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNamecodigo_trazabilidadDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldcodigo_trazabilidad':U ,
             OUTPUT h_dynselect-8 ).
       RUN repositionObject IN h_dynselect-8 ( 2.91 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-8 ( 1.00 , 36.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddosistratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameddosistratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddosistratamiento-5 ).
       RUN repositionObject IN h_ddosistratamiento-5 ( 1.95 , 90.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-tratamientoDataSourceFilterdosis_tratamiento.id_tipo_prod_tratamiento = 1NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDosis-1BSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,desc-prod,desc-ing,valor_dosis,desc-dosis,id_dosis_tratamientoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_dosis-5DisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_dosis_tratamiento':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 8.00 , 90.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 63.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddosistratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameddosistratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddosistratamiento-7 ).
       RUN repositionObject IN h_ddosistratamiento-7 ( 2.19 , 133.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-tratamientoDataSourceFilterdosis_tratamiento.id_tipo_prod_tratamiento = 3NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDosis-3BSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,desc-prod,desc-ing,valor_dosis,desc-dosis,id_dosis_tratamientoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_dosis-7DisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_dosis_tratamiento':U ,
             OUTPUT h_dynselect-9 ).
       RUN repositionObject IN h_dynselect-9 ( 10.33 , 90.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-9 ( 1.00 , 63.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddosistratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameddosistratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddosistratamiento ).
       RUN repositionObject IN h_ddosistratamiento ( 3.38 , 72.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 9.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-tratamientoDataSourceFilterdosis_tratamiento.id_tipo_prod_tratamiento = 1NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDosis-1ASortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,desc-prod,desc-ing,valor_dosis,desc-dosis,id_dosis_tratamientoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_dosis-1DisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_dosis_tratamiento':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 7.67 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 62.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddosistratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameddosistratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddosistratamiento-6 ).
       RUN repositionObject IN h_ddosistratamiento-6 ( 3.86 , 93.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-tratamientoDataSourceFilterdosis_tratamiento.id_tipo_prod_tratamiento = 2NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDosis-2BSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,desc-prod,desc-ing,valor_dosis,desc-dosis,id_dosis_tratamientoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_dosis-6DisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_dosis_tratamiento':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 9.19 , 90.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 63.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddosistratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameddosistratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddosistratamiento-8 ).
       RUN repositionObject IN h_ddosistratamiento-8 ( 4.57 , 112.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-tratamientoDataSourceFilterdosis_tratamiento.id_tipo_prod_tratamiento = 4NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDosis-4BSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,desc-prod,desc-ing,valor_dosis,desc-dosis,id_dosis_tratamientoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_dosis-8DisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_dosis_tratamiento':U ,
             OUTPUT h_dynselect-10 ).
       RUN repositionObject IN h_dynselect-10 ( 11.52 , 90.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-10 ( 1.00 , 63.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddosistratamiento.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNameddosistratamientoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_ddosistratamiento-2 ).
       RUN repositionObject IN h_ddosistratamiento-2 ( 5.76 , 66.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddesc-tratamientoDataSourceFilterdosis_tratamiento.id_tipo_prod_tratamiento = 2NumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDosis-2ASortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsdesc-tipo,desc-prod,desc-ing,valor_dosis,desc-dosisExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_dosis-2DisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_dosis_tratamiento':U ,
             OUTPUT h_dynselect-5 ).
       RUN repositionObject IN h_dynselect-5 ( 8.86 , 18.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-5 ( 1.00 , 62.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-7. */
       RUN addLink ( h_ddosistratamiento-4 , 'Data':U , h_dynselect-7 ).

       /* Links to SmartDataField h_dynselect-6. */
       RUN addLink ( h_ddosistratamiento-3 , 'Data':U , h_dynselect-6 ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_dmaqtratamiento , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-8. */
       RUN addLink ( h_dlotesplantacion , 'Data':U , h_dynselect-8 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_ddosistratamiento-5 , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect-9. */
       RUN addLink ( h_ddosistratamiento-7 , 'Data':U , h_dynselect-9 ).

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_ddosistratamiento , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_ddosistratamiento-6 , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect-10. */
       RUN addLink ( h_ddosistratamiento-8 , 'Data':U , h_dynselect-10 ).

       /* Links to SmartDataField h_dynselect-5. */
       RUN addLink ( h_ddosistratamiento-2 , 'Data':U , h_dynselect-5 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-8 ,
             RowObject.item:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             RowObject.fecha_fin:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.cantidad:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             h_dynselect , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-5 ,
             h_dynselect-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             h_dynselect-5 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-6 ,
             h_dynselect-4 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-9 ,
             h_dynselect-6 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-7 ,
             h_dynselect-9 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-10 ,
             h_dynselect-7 , 'AFTER':U ).
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

