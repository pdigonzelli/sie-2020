&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
          general         PROGRESS
          ventas           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dOrdenEntrega.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dOrdenEntrega.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.id_orden_entrega RowObject.fecha ~
RowObject.fecha_embarque RowObject.semana_embarque RowObject.fecha_arribo ~
RowObject.cotizacion RowObject.observaciones RowObject.booking ~
RowObject.fecha_cut_off 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS btnMail optTipoOE 
&Scoped-Define DISPLAYED-FIELDS RowObject.id_orden_entrega RowObject.fecha ~
RowObject.fecha_embarque RowObject.semana_embarque RowObject.fecha_arribo ~
RowObject.cotizacion RowObject.observaciones RowObject.booking ~
RowObject.fecha_cut_off 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS optTipoOE 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dagencias AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddespachantes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddestinos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dlugardescargas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtiposordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvapores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-6 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnMail 
     IMAGE-UP FILE "src/adm2/image/mail.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 5 BY 1.14 TOOLTIP "Mail con Observaciones (Roque, Mu�oz)".

DEFINE VARIABLE optTipoOE AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Exportacion", 1,
"Mercado Interno", 2
     SIZE 35 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnMail AT ROW 9.14 COL 96.8
     RowObject.id_orden_entrega AT ROW 1 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     optTipoOE AT ROW 1 COL 30 NO-LABEL
     RowObject.fecha AT ROW 3 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.fecha_embarque AT ROW 4 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.semana_embarque AT ROW 5 COL 16.2 COLON-ALIGNED
          LABEL "Sem Embarque"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.fecha_arribo AT ROW 6 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.cotizacion AT ROW 6.1 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.observaciones AT ROW 9.1 COL 18.2 NO-LABEL
          VIEW-AS EDITOR
          SIZE 77.8 BY 2.86
     RowObject.booking AT ROW 7 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37.8 BY 1
     RowObject.fecha_cut_off AT ROW 8 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 118.4 BY 11.05.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dOrdenEntrega.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dOrdenEntrega.i}
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
         HEIGHT             = 11.05
         WIDTH              = 118.4.
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

/* SETTINGS FOR FILL-IN RowObject.semana_embarque IN FRAME F-Main
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

&Scoped-define SELF-NAME btnMail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMail vTableWin
ON CHOOSE OF btnMail IN FRAME F-Main /* Button 1 */
DO:
  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTo  AS CHARACTER  NO-UNDO.
    
  cTo  = "rvelez@sa-sanmiguel.com,lmunoz@sa-sanmiguel.com,fpivingut@sa-sanmiguel.com".
  cSub = "Observaciones OE " + string(rowObject.id_orden_entrega:SCREEN-VALUE).
  cMes = rowObject.observaciones:SCREEN-VALUE.

  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME optTipoOE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL optTipoOE vTableWin
ON VALUE-CHANGED OF optTipoOE IN FRAME F-Main
DO:
  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  hSource = DYNAMIC-FUNCTION('getDataSource').

  rowObject.id_orden_entrega:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('getNextNroOE' IN hSource, INTEGER(SELF:SCREEN-VALUE))).
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

  DEFINE VARIABLE hSource AS HANDLE     NO-UNDO.
  hSource = DYNAMIC-FUNCTION('getDataSource').

  rowObject.id_orden_entrega:SCREEN-VALUE IN FRAME F-Main = STRING(DYNAMIC-FUNCTION('getNextNroOE' IN hSource, INTEGER(optTipoOE:SCREEN-VALUE))).


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
             INPUT  'ddestinos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameddestinosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ddestinos ).
       RUN repositionObject IN h_ddestinos ( 2.91 , 51.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldabreviaturaKeyFieldid_destinoDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelDestinoSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleDestinosBrowseFieldsdescripcion,id_destinoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_destinoDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 4.00 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 39.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dvapores.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedvaporesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dvapores ).
       RUN repositionObject IN h_dvapores ( 3.14 , 40.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_vaporDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelVaporSortnoViewAsBrowserToolTipFormatHelpId0BrowseTitleVaporesBrowseFieldsdescripcion,id_vaporExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_vaporDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 2.00 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 39.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'ddespachantes.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameddespachantesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ddespachantes ).
       RUN repositionObject IN h_ddespachantes ( 4.81 , 51.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_despachanteDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelDespachanteSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleDespachantesBrowseFieldsdescripcion,id_despachanteExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_despachanteDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 3.00 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 39.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dlugardescargas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedlugardescargasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dlugardescargas ).
       RUN repositionObject IN h_dlugardescargas ( 4.81 , 41.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_lugdesDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelLugar DescargaSortnoViewAsBrowserToolTipFormatHelpId0BrowseTitleLugares DescargaBrowseFieldsdescripcion,id_lugdesExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_lugdesDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-6 ).
       RUN repositionObject IN h_dynselect-6 ( 5.05 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-6 ( 1.00 , 39.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dagencias.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedagenciasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dagencias ).
       RUN repositionObject IN h_dagencias ( 6.95 , 108.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_agenciaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelAgenciaSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleAgenciasBrowseFieldsid_agencia,descripcionExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_agenciaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 1.00 , 80.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 39.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtiposordenentrega.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtiposordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtiposordenentrega ).
       RUN repositionObject IN h_dtiposordenentrega ( 8.86 , 108.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_tipo_orden_entregaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelTipo OESortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleTipos Orden EntregaBrowseFieldsdescripcion,id_tipo_orden_entregaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_tipo_orden_entregaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-5 ).
       RUN repositionObject IN h_dynselect-5 ( 2.00 , 18.20 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-5 ( 1.00 , 26.80 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_ddestinos , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dvapores , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_ddespachantes , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-6. */
       RUN addLink ( h_dlugardescargas , 'Data':U , h_dynselect-6 ).

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dagencias , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-5. */
       RUN addLink ( h_dtiposordenentrega , 'Data':U , h_dynselect-5 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-5 ,
             optTipoOE:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.fecha_arribo:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             h_dynselect , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-2 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             h_dynselect-3 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-6 ,
             h_dynselect-4 , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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

  optTipoOE:SENSITIVE IN FRAME F-Main = FALSE.
  btnMail:SENSITIVE IN FRAME F-Main   = FALSE.

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

  optTipoOE:SENSITIVE IN FRAME F-Main = TRUE.
  btnMail:SENSITIVE IN FRAME F-Main   = TRUE.

  DYNAMIC-FUNCTION('setQuerySort' IN h_dLugarDescargas, "BY lugar_descarga.descripcion").
  DYNAMIC-FUNCTION('openQuery' IN h_dLugarDescargas).

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
  DEFINE VARIABLE cSrt AS CHARACTER  NO-UNDO.

  optTipoOE:SENSITIVE IN FRAME F-Main = FALSE.
  btnMail:SENSITIVE IN FRAME F-Main   = FALSE.

  cSrt = "descripcion,id_vapor".
  DYNAMIC-FUNCTION('setQuerySort' IN h_dVapores, cSrt).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

