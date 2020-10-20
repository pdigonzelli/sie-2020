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
       {"dLotesCascara.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dLotesCascara.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.id_lote RowObject.anio ~
RowObject.Fecha RowObject.id_lote_cliente RowObject.observaciones ~
RowObject.anio_cliente 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.id_lote RowObject.anio ~
RowObject.Fecha RowObject.id_lote_cliente RowObject.observaciones ~
RowObject.anio_cliente 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFields vTableWin 
FUNCTION setFields RETURNS LOGICAL
  (pcAction AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dccalidad AS HANDLE NO-UNDO.
DEFINE VARIABLE h_denvasesprod AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dproductosterminados AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dsucursales AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_lote AT ROW 2.19 COL 10 COLON-ALIGNED
          LABEL "Lote" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     RowObject.anio AT ROW 2.19 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.Fecha AT ROW 4.43 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_lote_cliente AT ROW 3.38 COL 10 COLON-ALIGNED
          LABEL "Lote Cte." FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     RowObject.observaciones AT ROW 2.19 COL 56 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 26.6 BY 6.67
     RowObject.anio_cliente AT ROW 3.38 COL 41 COLON-ALIGNED
          LABEL "Año Lote Cliente" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     "Observaciones" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.19 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 82.8 BY 8.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dLotesCascara.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dLotesCascara.i}
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
         HEIGHT             = 8.29
         WIDTH              = 82.8.
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

/* SETTINGS FOR FILL-IN RowObject.anio_cliente IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.id_lote IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.id_lote_cliente IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  
  setFields("ADD").

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
             INPUT  'dccalidad.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedccalidadUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dccalidad ).
       RUN repositionObject IN h_dccalidad ( 3.62 , 30.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_calidadDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelCalidadSortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_calidadDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 6.71 , 12.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 43.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'denvasesprod.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedenvasesprodUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_denvasesprod ).
       RUN repositionObject IN h_denvasesprod ( 3.86 , 46.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_envaseDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelEnvaseSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleBrowseFieldsdescripcion,id_envaseExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_envaseDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 7.86 , 12.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 43.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dproductosterminados.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedproductosterminadosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dproductosterminados ).
       RUN repositionObject IN h_dproductosterminados ( 7.43 , 34.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_articuloDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelArticuloSortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_articuloDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 5.57 , 12.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 43.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dsucursales.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedsucursalesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dsucursales ).
       RUN repositionObject IN h_dsucursales ( 7.43 , 23.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldnombreKeyFieldid_sucursalDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelSucursalSortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_sucursalDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 1.00 , 12.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 71.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_dccalidad , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_denvasesprod , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dproductosterminados , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dsucursales , 'Data':U , h_dynselect ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.id_lote:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             RowObject.Fecha:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-2 , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  DEFINE VARIABLE vcQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cList AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLib.

  cList = "626,641,642,643,797,798,799,800".

  cQry = DYNAMIC-FUNCTION('getFilterString' IN hLib, cList, 'OR', 'id_calidad').

  DYNAMIC-FUNCTION('setQueryWhere' IN h_dcCalidad, cQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dcCalidad).

  setFields("UPDATE").

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

  setFields("UPDATE").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFields vTableWin 
FUNCTION setFields RETURNS LOGICAL
  (pcAction AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcQry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vhDat AS HANDLE     NO-UNDO.

  vcQry = "FOR EACH sucursales WHERE sucursales.id_sucursal = 95 OR sucursales.id_sucursal = 96 OR sucursales.id_sucursal = 461".
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dSucursales, vcQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dSucursales).
  
  vcQry = "FOR EACH productos_terminados WHERE id_articulo = 54 OR id_articulo = 69 OR id_articulo = 55 OR id_articulo = 38 OR id_articulo = 554".
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dProductosTerminados, vcQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dProductosTerminados).

  vcQry = "FOR EACH envases_prod WHERE envases_prod.id_envase = 14".
  DYNAMIC-FUNCTION('setQueryWhere' IN h_dEnvasesProd, vcQry).
  DYNAMIC-FUNCTION('openQuery' IN h_dEnvasesProd).

  IF pcAction = "ADD" THEN DO:  
    vhDat = DYNAMIC-FUNCTION('getDataSource').
    rowObject.id_lote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION('getNextNroLote' IN vhDat).
    rowObject.anio:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(YEAR(TODAY)).
    rowObject.fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(TODAY).
  END.



  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

