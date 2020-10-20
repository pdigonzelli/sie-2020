&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dPuntoEnvase.i"}.


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
DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dPuntoEnvase.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Fecha RowObject.Bx_20_20 ~
RowObject.Acidez_w_w RowObject.litros RowObject.sol_totales ~
RowObject.cantidad_tambores RowObject.cant_2 RowObject.tambor_desde ~
RowObject.kilos_tambor RowObject.id_envase_2 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS radTipoTambor 
&Scoped-Define DISPLAYED-FIELDS RowObject.Fecha RowObject.Bx_20_20 ~
RowObject.Acidez_w_w RowObject.bx_correg RowObject.Acidez_w_v ~
RowObject.litros RowObject.sol_totales RowObject.id_lote RowObject.Anio ~
RowObject.cantidad_tambores RowObject.cant_2 RowObject.tambor_desde ~
RowObject.tambor_hasta RowObject.kilos_tambor RowObject.id_envase_2 ~
RowObject.id_tipotambor 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS radTipoTambor 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroLote vTableWin 
FUNCTION getNextNroLote RETURNS CHARACTER
  (piArt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
DEFINE VARIABLE radTipoTambor AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Lote", 3,
"Produccion", 1, 
"Terceros", 9
     SIZE 17 BY 2.14 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     radTipoTambor AT ROW 1 COL 12 NO-LABEL
     RowObject.Fecha AT ROW 3.19 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Bx_20_20 AT ROW 4.24 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     RowObject.Acidez_w_w AT ROW 5.29 COL 10 COLON-ALIGNED
          LABEL "Acidez%"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     RowObject.bx_correg AT ROW 6.33 COL 10 COLON-ALIGNED
          LABEL "Bx Correg"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     RowObject.Acidez_w_v AT ROW 7.38 COL 10 COLON-ALIGNED
          LABEL "AcGPL"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     RowObject.litros AT ROW 8.43 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.sol_totales AT ROW 9.48 COL 10 COLON-ALIGNED
          LABEL "Solidos"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.id_lote AT ROW 4.24 COL 45.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Anio AT ROW 5.29 COL 49.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.cantidad_tambores AT ROW 6.24 COL 46.2 COLON-ALIGNED
          LABEL "Cantidad"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.cant_2 AT ROW 6.24 COL 55.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     RowObject.tambor_desde AT ROW 7.38 COL 52.4 COLON-ALIGNED
          LABEL "Desde"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.tambor_hasta AT ROW 8.43 COL 52.4 COLON-ALIGNED
          LABEL "Hasta"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.kilos_tambor AT ROW 9.48 COL 41.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     RowObject.id_envase_2 AT ROW 9.48 COL 54.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     RowObject.id_tipotambor AT ROW 1 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 63 BY 9.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dPuntoEnvase.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dPuntoEnvase.i}
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
         HEIGHT             = 9.52
         WIDTH              = 63.
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

/* SETTINGS FOR FILL-IN RowObject.Acidez_w_v IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Acidez_w_w IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Anio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.bx_correg IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.cantidad_tambores IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_tipotambor IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.sol_totales IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.tambor_desde IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.tambor_hasta IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.Acidez_w_w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Acidez_w_w vTableWin
ON LEAVE OF RowObject.Acidez_w_w IN FRAME F-Main /* Acidez% */
DO:
  DEFINE VARIABLE fCorr AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fBx AS DECIMAL    NO-UNDO.

  fCorr = DYNAMIC-FUNCTION('getCoefCorreccionAcidez' IN hLibTam, DECIMAL(SELF:SCREEN-VALUE)).
  fBx   = DECIMAL(rowObject.bx_20_20:SCREEN-VALUE) + fCorr.

  rowObject.bx_correg:SCREEN-VALUE IN FRAME F-Main  = STRING(fBx).
  rowObject.acidez_w_v:SCREEN-VALUE IN FRAME F-Main = DYNAMIC-FUNCTION('getAcidezGPL' IN hLibTam, DECIMAL(rowObject.bx_20_20:SCREEN-VALUE), DECIMAL(SELF:SCREEN-VALUE)).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.litros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.litros vTableWin
ON LEAVE OF RowObject.litros IN FRAME F-Main /* Litros */
DO:
  DEFINE VARIABLE dSol AS DECIMAL    NO-UNDO.

  dSol = DYNAMIC-FUNCTION('getSolidosSolubles' IN hLibTam, DECIMAL(rowObject.bx_correg:SCREEN-VALUE), 
                                                           DECIMAL(rowObject.litros:SCREEN-VALUE)).

  rowObject.sol_totales:SCREEN-VALUE = STRING(dSol).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radTipoTambor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radTipoTambor vTableWin
ON VALUE-CHANGED OF radTipoTambor IN FRAME F-Main
DO:
  rowObject.id_tipotambor:SCREEN-VALUE = SELF:SCREEN-VALUE.
  RUN filterSmartSelect(INTEGER(SELF:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.tambor_desde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.tambor_desde vTableWin
ON LEAVE OF RowObject.tambor_desde IN FRAME F-Main /* Desde */
DO:
  rowObject.tambor_hasta:SCREEN-VALUE IN FRAME F-Main = STRING(INTEGER(rowObject.tambor_desde:SCREEN-VALUE) + INTEGER(rowObject.cantidad:SCREEN-VALUE) - 1).
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

  rowObject.id_tipotambor:SCREEN-VALUE IN FRAME F-Main  = "1".
  radTipoTambor:SCREEN-VALUE IN FRAME F-Main            = "1".
  rowObject.anio:SCREEN-VALUE IN FRAME F-Main           = STRING(YEAR(TODAY)).
  rowObject.id_envase_2:SCREEN-VALUE IN FRAME F-Main    = "0".

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
             INPUT  'dproductosterminados.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedproductosterminadosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dproductosterminados ).
       RUN repositionObject IN h_dproductosterminados ( 1.95 , 28.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventfilterEnvaseDisplayedFieldArticuloKeyFieldid_articuloDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelArticuloSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleArticulosBrowseFieldsArticulo,id_articuloExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_articuloDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 1.00 , 40.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 24.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dcalidades.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcalidadesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcalidades ).
       RUN repositionObject IN h_dcalidades ( 3.62 , 28.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldCodCalidadKeyFieldid_calidadDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelCalidadSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleCalidadesBrowseFieldsCodCalidad,id_calidadExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_calidadDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 2.10 , 40.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 24.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'denvasesprod.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedenvasesprodUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_denvasesprod ).
       RUN repositionObject IN h_denvasesprod ( 5.05 , 29.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.00 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventgetDrumsQtyDisplayedFieldEnvaseKeyFieldid_envaseDataSourceFilterNumRows20OptionalnoOptionalString':U + '<none>' + 'LabelEnvaseSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleEnvasesBrowseFieldsEnvase,id_envaseExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_envaseDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 3.14 , 40.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 24.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dproductosterminados , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_dcalidades , 'Data':U , h_dynselect-2 ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_denvasesprod , 'Data':U , h_dynselect-3 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.sol_totales:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             h_dynselect , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-2 , 'AFTER':U ).
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
  DEFINE INPUT  PARAMETER plEnable AS LOGICAL    NO-UNDO.

  radTipoTambor:SENSITIVE IN FRAME F-Main = plEnable.


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

  RUN customEnable (FALSE).

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
  DEFINE VARIABLE hTableioSource AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cName          AS CHARACTER  NO-UNDO.

  
  ASSIGN cFieldHandles  = DYNAMIC-FUNCTION('getAllFieldHandles')
         iNumEntries    = NUM-ENTRIES(cFieldHandles)
         hTableioSource = DYNAMIC-FUNCTION('getTableIOSource').
  
  DO iLoop = 1 TO iNumEntries:
    ASSIGN hField = WIDGET-HANDLE(ENTRY(iLoop,cFieldHandles)).

    ASSIGN cName = hField:NAME NO-ERROR.
    
    IF cName = "id_tipotambor" AND hField:SCREEN-VALUE <> "0" THEN 
      radTipoTambor:SCREEN-VALUE IN FRAME F-Main = hField:SCREEN-VALUE.
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

  RUN customEnable (TRUE).
  RUN filterSmartSelect (INTEGER(radTipoTambor:SCREEN-VALUE IN FRAME F-Main)).

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

  IF radTipoTambor:SCREEN-VALUE IN FRAME F-Main = "3" THEN DO:
  
    cLst = "9999".    
    
    IF pcPickedValue = '52' THEN DO:
      cLst = "21,503,506,518,520,528,532,533,534,535,555,553,558,560,561".    
    END.
  
    IF pcPickedValue = '53' THEN DO:
      cLst = "21,503,506,507,518,519,520,521,522,523,533,534,534,535,555,556,558,560,561".    
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

  getNextNroLote(INTEGER(pcPickedValue)).


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
    
    cList = "51,52,60,64,313,317,327,331,339,414,424,429,795".
    RUN setSmartSelectQuery(cList, 'id_calidad', h_dCalidades).

    cList = "501,531,555,556".
    RUN setSmartSelectQuery(cList, 'id_envase', h_dEnvasesProd).
    

  END.

  IF piTipo = 3 OR piTipo = 9 THEN DO: /*para lotes*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDrumsQty vTableWin 
PROCEDURE getDrumsQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcValue AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cEnv AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRes AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDif AS CHARACTER  NO-UNDO.


  dKil = DYNAMIC-FUNCTION('getKilosFromBxCorreg' IN hLibTam, DECIMAL(rowObject.bx_correg:SCREEN-VALUE IN FRAME F-Main), 
                                                             DECIMAL(rowObject.litros:SCREEN-VALUE IN FRAME F-Main)).
                                                            
  cEnv = DYNAMIC-FUNCTION('getCantidadTambores' IN hLibTam, dKil, 
                                                            INTEGER(pcValue)).

  IF cEnv <> "" THEN DO:
    cRes = ENTRY(1, cEnv, CHR(10)).
    cDif = ENTRY(2, cEnv, CHR(10)).

    rowObject.cantidad_tambores:SCREEN-VALUE = ENTRY(1, cRes, CHR(1)).
    rowObject.kilos_tambor:SCREEN-VALUE      = ENTRY(2, cRes, CHR(1)).
    rowObject.cant_2:SCREEN-VALUE            = ENTRY(1, cDif, CHR(1)).
    rowObject.id_envase_2:SCREEN-VALUE       = ENTRY(2, cDif, CHR(1)).
    rowObject.tambor_desde:SCREEN-VALUE      = "1".
    rowObject.tambor_hasta:SCREEN-VALUE      = STRING(INTEGER(rowObject.tambor_desde:SCREEN-VALUE) + INTEGER(rowObject.cantidad:SCREEN-VALUE) - 1).
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
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLib, 'libTamboresIndustria').
  

  RUN customEnable (FALSE).
  RUN filterSmartSelect(1).

  rowObject.id_tipotambor:SCREEN-VALUE IN FRAME F-Main = radTipoTambor:SCREEN-VALUE.
  rowObject.id_tipotambor:HIDDEN IN FRAME F-Main = TRUE.
  /*radTipoTambor:SCREEN-VALUE IN FRAME F-Main = "1".*/

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroLote vTableWin 
FUNCTION getNextNroLote RETURNS CHARACTER
  (piArt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLot    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hCont   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.

  {get ContainerSource hCont}.

  
  IF VALID-HANDLE(hCont) AND (hCont:FILE-NAME MATCHES "*wProcesos.w") THEN DO:

    iSuc = DYNAMIC-FUNCTION('getSucursalProceso' IN hCont).
    cLot = DYNAMIC-FUNCTION('getNextNroLote' IN hLibTam, iSuc, 
                                                         INTEGER(radTipoTambor:SCREEN-VALUE IN FRAME {&FRAME-NAME}), 
                                                         piArt, 
                                                         YEAR(TODAY)).
    cLot = ENTRY(4, cLot, ".").
  
    rowObject.id_lote:SCREEN-VALUE IN FRAME F-Main = ENTRY(1, cLot, "/").
    rowObject.anio:SCREEN-VALUE IN FRAME F-Main    = STRING(YEAR(TODAY)).

  END. /*if valid-handle()... */
  
  RETURN cLot.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

