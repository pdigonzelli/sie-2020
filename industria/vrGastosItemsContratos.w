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
       {"drGastosItemsContratos.i"}.


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
&Scoped-define DATA-FIELD-DEFS "drGastosItemsContratos.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.importe RowObject.porcentaje 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.importe RowObject.porcentaje 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dgastosventa AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtipomoneda AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dtipounidadventa AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.importe AT ROW 4.43 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25.8 BY 1
     RowObject.porcentaje AT ROW 5.52 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 82.2 BY 5.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "drGastosItemsContratos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {drGastosItemsContratos.i}
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
         HEIGHT             = 5.81
         WIDTH              = 82.2.
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
             INPUT  'dgastosventa.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedgastosventaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dgastosventa ).
       RUN repositionObject IN h_dgastosventa ( 4.81 , 50.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_gastoDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelGastoSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleGastosBrowseFieldsdescripcion,id_gastoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_gastoDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 1.00 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 44.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtipomoneda.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtipomonedaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtipomoneda ).
       RUN repositionObject IN h_dtipomoneda ( 4.81 , 29.00 ) NO-ERROR.
       /* Size in AB:  ( 1.76 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_monedaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelMonedaSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleMonedasBrowseFieldsdescripcion,id_monedaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_monedaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 2.14 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 44.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtipounidadventa.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedtipounidadventaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dtipounidadventa ).
       RUN repositionObject IN h_dtipounidadventa ( 4.81 , 39.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_tipo_unidad_ventaDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelUnidadSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleUnidadesBrowseFieldsdescripcion,id_tipo_unidad_ventaExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_tipo_unidad_ventaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 3.29 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 44.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dgastosventa , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_dtipomoneda , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dtipounidadventa , 'Data':U , h_dynselect-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.importe:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
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

