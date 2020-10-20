&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dtareas.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dtareas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.id_tarea RowObject.vigente ~
RowObject.abreviatura RowObject.descripcion RowObject.id_centro_costo ~
RowObject.factor RowObject.decreto RowObject.valor_normal ~
RowObject.con_restriccion_cupo RowObject.caracteristica ~
RowObject.id_concepto1 RowObject.id_concepto16 RowObject.id_concepto6 ~
RowObject.id_concepto11 RowObject.id_concepto2 RowObject.id_concepto17 ~
RowObject.id_concepto7 RowObject.id_concepto12 RowObject.id_concepto3 ~
RowObject.id_concepto18 RowObject.id_concepto8 RowObject.id_concepto13 ~
RowObject.id_concepto4 RowObject.id_concepto19 RowObject.id_concepto9 ~
RowObject.id_concepto14 RowObject.id_concepto5 RowObject.id_concepto10 ~
RowObject.id_concepto15 RowObject.id_concepto20 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.id_tarea RowObject.vigente ~
RowObject.abreviatura RowObject.descripcion RowObject.id_centro_costo ~
RowObject.factor RowObject.decreto RowObject.valor_normal ~
RowObject.con_restriccion_cupo RowObject.caracteristica ~
RowObject.id_concepto1 RowObject.id_concepto16 RowObject.id_concepto6 ~
RowObject.id_concepto11 RowObject.id_concepto2 RowObject.id_concepto17 ~
RowObject.id_concepto7 RowObject.id_concepto12 RowObject.id_concepto3 ~
RowObject.id_concepto18 RowObject.id_concepto8 RowObject.id_concepto13 ~
RowObject.id_concepto4 RowObject.id_concepto19 RowObject.id_concepto9 ~
RowObject.id_concepto14 RowObject.id_concepto5 RowObject.id_concepto10 ~
RowObject.id_concepto15 RowObject.id_concepto20 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dgrupostareas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsectoresagricola AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtipotareas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-5 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_tarea AT ROW 1 COL 18 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.vigente AT ROW 1.48 COL 86 WIDGET-ID 70
          LABEL "Vigente"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.abreviatura AT ROW 2 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     RowObject.descripcion AT ROW 3.14 COL 18 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 97 BY 1
     RowObject.id_centro_costo AT ROW 5.52 COL 18 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 24.4 BY 1
     RowObject.factor AT ROW 6.52 COL 18 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.decreto AT ROW 6.52 COL 58 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.valor_normal AT ROW 6.52 COL 83.2 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.con_restriccion_cupo AT ROW 9.1 COL 103 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.caracteristica AT ROW 9.43 COL 18 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     RowObject.id_concepto1 AT ROW 10.76 COL 12 COLON-ALIGNED WIDGET-ID 30
          LABEL "Concepto1"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto16 AT ROW 10.76 COL 99.4 COLON-ALIGNED WIDGET-ID 60
          LABEL "Concepto16"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto6 AT ROW 10.86 COL 40 COLON-ALIGNED WIDGET-ID 42
          LABEL "Concepto6"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto11 AT ROW 10.86 COL 69 COLON-ALIGNED WIDGET-ID 50
          LABEL "Concepto11"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto2 AT ROW 11.76 COL 12 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto17 AT ROW 11.76 COL 99.4 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto7 AT ROW 11.86 COL 40 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto12 AT ROW 11.86 COL 69 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto3 AT ROW 12.76 COL 12 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto18 AT ROW 12.76 COL 99.4 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto8 AT ROW 12.86 COL 40 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto13 AT ROW 12.86 COL 69 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto4 AT ROW 13.76 COL 12 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto19 AT ROW 13.76 COL 99.4 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto9 AT ROW 13.86 COL 40 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 117.2 BY 15.76 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.id_concepto14 AT ROW 13.86 COL 69 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto5 AT ROW 14.91 COL 12 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto10 AT ROW 14.91 COL 40.2 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto15 AT ROW 14.95 COL 69 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_concepto20 AT ROW 14.95 COL 99.8 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 117.2 BY 15.76 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dtareas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dtareas.i}
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
         HEIGHT             = 15.76
         WIDTH              = 117.2.
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_concepto1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_concepto11 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_concepto16 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_concepto6 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.vigente IN FRAME F-Main
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
             INPUT  'dsectoresagricola.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedsectoresagricolaOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dsectoresagricola ).
       RUN repositionObject IN h_dsectoresagricola ( 1.00 , 48.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_sectorDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_sector':U ,
             OUTPUT h_dynselect-5 ).
       RUN repositionObject IN h_dynselect-5 ( 9.29 , 32.40 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-5 ( 1.00 , 43.60 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtipotareas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedtipotareasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dtipotareas ).
       RUN repositionObject IN h_dtipotareas ( 1.24 , 63.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_tipo_tareaDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_tipo_tarea':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 4.33 , 20.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 50.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dgrupostareas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedgrupostareasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dgrupostareas ).
       RUN repositionObject IN h_dgrupostareas ( 9.57 , 60.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsCombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_grupo_tareaDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_grupo_tarea':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 7.76 , 20.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 50.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-5. */
       RUN addLink ( h_dsectoresagricola , 'Data':U , h_dynselect-5 ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_dtipotareas , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dgrupostareas , 'Data':U , h_dynselect-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-4 ,
             RowObject.descripcion:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             RowObject.valor_normal:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-5 ,
             RowObject.con_restriccion_cupo:HANDLE IN FRAME F-Main , 'AFTER':U ).
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

