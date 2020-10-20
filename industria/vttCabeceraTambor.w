&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dttCabeceraTambor.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dttCabeceraTambor.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.tipo_tambor RowObject.id_lote ~
RowObject.fecha RowObject.kilos RowObject.cantidad ~
RowObject.id_tambor_desde 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.tipo_tambor RowObject.id_lote ~
RowObject.anio RowObject.fecha RowObject.kilos RowObject.cantidad ~
RowObject.id_tambor_desde RowObject.id_tambor_hasta 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dcalidades AS HANDLE NO-UNDO.
DEFINE VARIABLE h_denvasesprod AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dproductosterminados AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.tipo_tambor AT ROW 1 COL 10 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Produccion", 1,
"Lote", 3
          SIZE 16 BY 1.91
     RowObject.id_lote AT ROW 3.1 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.anio AT ROW 4.1 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.fecha AT ROW 5.1 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.kilos AT ROW 6.1 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     RowObject.cantidad AT ROW 4.1 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.id_tambor_desde AT ROW 5.1 COL 54 COLON-ALIGNED
          LABEL "Desde"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.id_tambor_hasta AT ROW 6.1 COL 54 COLON-ALIGNED
          LABEL "Hasta"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 63.2 BY 6.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dttCabeceraTambor.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dttCabeceraTambor.i}
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
         HEIGHT             = 6.19
         WIDTH              = 63.2.
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
/* SETTINGS FOR FILL-IN RowObject.id_tambor_desde IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_tambor_hasta IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.id_tambor_desde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_tambor_desde vTableWin
ON LEAVE OF RowObject.id_tambor_desde IN FRAME F-Main /* Desde */
DO:
  DEFINE VARIABLE iHasta AS INTEGER    NO-UNDO.

  iHasta = INTEGER(rowObject.id_tambor_desde:SCREEN-VALUE) + INTEGER(rowObject.cantidad:SCREEN-VALUE) - 1.
  rowObject.id_tambor_hasta:SCREEN-VALUE IN FRAME F-Main = STRING(iHasta).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.tipo_tambor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.tipo_tambor vTableWin
ON VALUE-CHANGED OF RowObject.tipo_tambor IN FRAME F-Main
DO:
  RUN filterSmartSelect(INTEGER(SELF:SCREEN-VALUE)).
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

  rowObject.fecha:SCREEN-VALUE IN FRAME F-Main = STRING(TODAY).
  rowObject.anio:SCREEN-VALUE IN FRAME F-Main  = STRING(YEAR(TODAY)).
  

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
       RUN repositionObject IN h_dcalidades ( 3.86 , 28.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldQualityKeyFieldid_calidadDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelCalidadSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleCalidadesBrowseFieldsCodCalidad,id_calidadExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_calidadDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 2.00 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 26.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dproductosterminados.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedproductosterminadosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dproductosterminados ).
       RUN repositionObject IN h_dproductosterminados ( 4.57 , 38.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventfilterEnvaseDisplayedFieldArticuloKeyFieldid_articuloDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelArticuloSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleArticulosBrowseFieldsArticulo,id_articuloExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_articuloDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 1.00 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 26.20 ) NO-ERROR.

       RUN constructObject (
             INPUT  'denvasesprod.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedenvasesprodUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_denvasesprod ).
       RUN repositionObject IN h_denvasesprod ( 5.52 , 28.00 ) NO-ERROR.
       /* Size in AB:  ( 1.67 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldEnvaseKeyFieldid_envaseDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelEnvaseSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleEnvasesBrowseFieldsEnvase,id_envaseExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_envaseDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 3.00 , 38.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 26.20 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dcalidades , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dproductosterminados , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_denvasesprod , 'Data':U , h_dynselect-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.kilos:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             h_dynselect , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-2 , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN filterSmartSelect(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterEnvase vTableWin 
PROCEDURE filterEnvase :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcPickedValue AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cLst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.

  IF rowObject.tipo_tambor:SCREEN-VALUE IN FRAME F-Main = "3" THEN DO:
  
    cLst = "9999".    
    
    IF pcPickedValue = '52' THEN DO:
      cLst = "21,503,506,518,520,528,532,533,534,535,555,553,558".    
    END.
  
    IF pcPickedValue = '53' THEN DO:
      cLst = "21,503,506,507,518,519,520,521,522,523,533,534,534,535,555,556,558".    
    END.
  
    IF pcPickedValue = '521' THEN DO:
      cLst = "21,503,534,538,555,556".
    END.
  
    IF pcPickedValue = '88'  OR
       pcPickedValue = '523' OR 
       pcPickedValue = '524' OR
       pcPickedValue = '534' OR
       pcPickedValue = '801' OR
       pcPickedValue = '802' OR
       pcPickedValue = '882' THEN DO:
      cLst = "531,556".
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
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterSmartSelect vTableWin 
PROCEDURE filterSmartSelect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piTipo AS INTEGER    NO-UNDO.
  
  DEFINE VARIABLE cList AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry  AS CHARACTER  NO-UNDO.



  IF piTipo = 1 THEN DO: /*para producciones*/
    cList = "43,521,532,533".
    RUN setSmartSelectQuery(cList, 'id_articulo', h_dProductosTerminados).
    
    cList = "51,52,60,64,313,317,327,331,414,424,429".
    RUN setSmartSelectQuery(cList, 'id_calidad', h_dCalidades).

    cList = "501,531,555,556".
    RUN setSmartSelectQuery(cList, 'id_envase', h_dEnvasesProd).
    

  END.

  IF piTipo = 3 THEN DO: /*para lotes*/
    cList = "70,71,971,30,33,34,52,53,88,523,524,544,535,~
             600,801,802,42,43,46,49,62,63,66,624,942,946,962,~
             966".
    RUN setSmartSelectQuery(cList, 'id_articulo', h_dProductosTerminados).

    cList = "51,52,53,54,55,56,57,58,60,61,62,63,64,65,66,67,68,69,~
             70,71,77,80,81,82,83,84,85,86,87,108,109,250,311,312,~
             313,314,315,316,317,323,324,325,326,327,328,329,330,331,~
             332,334,335,336,337,338,339,340,341,342,350,400,401,403,~
             410,411,412,413,414,415,416,417,418,419,420,421,422,423,~
             424,425,426,427,428,429,612,629".
    RUN setSmartSelectQuery(cList, 'id_calidad', h_dCalidades).

    


  END.

  







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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSmartSelectQuery vTableWin 
PROCEDURE setSmartSelectQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcLst AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcFld AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER phSdo AS HANDLE     NO-UNDO.

  DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.

  cQry = DYNAMIC-FUNCTION('getFilterString' IN hLib, pcLst, 'OR', pcFld).
  DYNAMIC-FUNCTION('setQueryWhere' IN phSdo, cQry).
  DYNAMIC-FUNCTION('openQuery' IN phSdo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

