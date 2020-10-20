&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general         PROGRESS
          ventas           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"d000permisosembarque.i"}.


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
&Scoped-define DATA-FIELD-DEFS "d000permisosembarque.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.fobfactura RowObject.anio ~
RowObject.id_permiso_embarque RowObject.fecha RowObject.consignacion ~
RowObject.rectificado RowObject.id_posicion_arancelaria ~
RowObject.fecha_oficializacion RowObject.fecha_envio_tuc ~
RowObject.fecha_rec_tuc RowObject.fecha_cumplido ~
RowObject.fecha_venc_derecho RowObject.fecha_declaracion_venta ~
RowObject.importe_reintegro RowObject.importe_derecho ~
RowObject.importe_reembolso RowObject.importe RowObject.observaciones 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 
&Scoped-Define DISPLAYED-FIELDS RowObject.fobfactura RowObject.anio ~
RowObject.id_permiso_embarque RowObject.fecha RowObject.consignacion ~
RowObject.rectificado RowObject.id_posicion_arancelaria ~
RowObject.fecha_oficializacion RowObject.fecha_envio_tuc ~
RowObject.fecha_rec_tuc RowObject.fecha_cumplido ~
RowObject.fecha_venc_derecho RowObject.fecha_declaracion_venta ~
RowObject.importe_reintegro RowObject.importe_derecho ~
RowObject.importe_reembolso RowObject.importe RowObject.observaciones 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_d000aduanas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000cliente AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000despachantes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000itemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000ordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000tipomoneda AS HANDLE NO-UNDO.
DEFINE VARIABLE h_d000tipospe AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-6 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 12.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82.8 BY 4.24.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82.8 BY 8.1.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 129 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.fobfactura AT ROW 14.57 COL 85 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 44 BY 2.38
     RowObject.anio AT ROW 1.95 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.id_permiso_embarque AT ROW 3.1 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     RowObject.fecha AT ROW 5.48 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.consignacion AT ROW 6.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.rectificado AT ROW 9.1 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.id_posicion_arancelaria AT ROW 4.1 COL 104.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     RowObject.fecha_oficializacion AT ROW 6.71 COL 69.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.fecha_envio_tuc AT ROW 7.91 COL 69.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.fecha_rec_tuc AT ROW 9.1 COL 69.2 COLON-ALIGNED
          LABEL "Fecha Rec. Tuc"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.fecha_cumplido AT ROW 10.29 COL 69.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.fecha_venc_derecho AT ROW 11.48 COL 69.2 COLON-ALIGNED
          LABEL "Fecha Vto Derecho"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.fecha_declaracion_venta AT ROW 12.67 COL 69.2 COLON-ALIGNED
          LABEL "Fecha Decl. Vta"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.importe_reintegro AT ROW 7.91 COL 107.2 COLON-ALIGNED
          LABEL "Importe Reintegro."
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     RowObject.importe_derecho AT ROW 9.1 COL 107.2 COLON-ALIGNED
          LABEL "Importe Derecho."
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.importe_reembolso AT ROW 10.29 COL 107.2 COLON-ALIGNED
          LABEL "Importe Reembolso"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.importe AT ROW 11.48 COL 107.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.observaciones AT ROW 14.57 COL 2.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 52.6 BY 2.38
     RECT-1 AT ROW 1.29 COL 1
     RECT-2 AT ROW 1.29 COL 47.2
     RECT-3 AT ROW 5.81 COL 47.2
     RECT-4 AT ROW 14.14 COL 1
     "Permiso" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1 COL 3
          FONT 6
     "Datos" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1 COL 48
          FONT 6
     "Fechas e Importes" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 5.52 COL 48.2
          FONT 6
     "Observaciones" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 13.95 COL 2
          FONT 6
     SPACE(106.80) SKIP(1.86)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "d000permisosembarque.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {d000permisosembarque.i}
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
         HEIGHT             = 16.29
         WIDTH              = 129.4.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fecha_declaracion_venta IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.fecha_rec_tuc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.fecha_venc_derecho IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.importe_derecho IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.importe_reembolso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.importe_reintegro IN FRAME F-Main
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
             INPUT  'd000aduanas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000aduanasUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000aduanas ).
       RUN repositionObject IN h_d000aduanas ( 14.57 , 58.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_aduanaDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelAduanaSortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_aduanaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 4.29 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 30.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'd000cliente.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000clienteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000cliente ).
       RUN repositionObject IN h_d000cliente ( 14.57 , 49.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 8.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldrazon_socialKeyFieldid_clienteDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelClienteSortyesViewAsBrowserToolTipFormatHelpId0BrowseTitleBrowseFieldsrazon_social,id_clienteExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_clienteDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 1.71 , 71.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 57.80 ) NO-ERROR.

       RUN constructObject (
             INPUT  'd000despachantes.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000despachantesUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000despachantes ).
       RUN repositionObject IN h_d000despachantes ( 14.57 , 69.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_despachanteDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelDespachanteSortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_despachanteDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-4 ).
       RUN repositionObject IN h_dynselect-4 ( 2.91 , 71.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-4 ( 1.00 , 57.80 ) NO-ERROR.

       RUN constructObject (
             INPUT  'd000ordenentrega.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000ordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000ordenentrega ).
       RUN repositionObject IN h_d000ordenentrega ( 14.57 , 80.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldid_orden_entregaKeyFieldid_orden_entregaDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelOESortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_orden_entregaDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-6 ).
       RUN repositionObject IN h_dynselect-6 ( 10.29 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-6 ( 1.00 , 30.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'd000itemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000itemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000itemsordenentrega ).
       RUN repositionObject IN h_d000itemsordenentrega ( 14.57 , 91.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'd000tipomoneda.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000tipomonedaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000tipomoneda ).
       RUN repositionObject IN h_d000tipomoneda ( 14.57 , 27.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_monedaDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelMonedaSortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameid_moneda_origenDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-5 ).
       RUN repositionObject IN h_dynselect-5 ( 6.71 , 109.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-5 ( 1.00 , 19.40 ) NO-ERROR.

       RUN constructObject (
             INPUT  'd000tipospe.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamed000tipospeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_d000tipospe ).
       RUN repositionObject IN h_d000tipospe ( 14.57 , 38.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionKeyFieldid_tipo_peDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelTipos P.E.SortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNametipo_peDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-2 ).
       RUN repositionObject IN h_dynselect-2 ( 7.86 , 16.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-2 ( 1.00 , 30.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_d000aduanas , 'Data':U , h_dynselect ).

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_d000cliente , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect-4. */
       RUN addLink ( h_d000despachantes , 'Data':U , h_dynselect-4 ).

       /* Links to SmartDataField h_dynselect-6. */
       RUN addLink ( h_d000ordenentrega , 'Data':U , h_dynselect-6 ).

       /* Links to SmartDataObject h_d000itemsordenentrega. */
       RUN addLink ( h_d000ordenentrega , 'Data':U , h_d000itemsordenentrega ).

       /* Links to SmartDataField h_dynselect-5. */
       RUN addLink ( h_d000tipomoneda , 'Data':U , h_dynselect-5 ).

       /* Links to SmartDataField h_dynselect-2. */
       RUN addLink ( h_d000tipospe , 'Data':U , h_dynselect-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.id_permiso_embarque:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-2 ,
             RowObject.consignacion:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-6 ,
             RowObject.rectificado:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             h_dynselect-6 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-4 ,
             h_dynselect-3 , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-5 ,
             RowObject.fecha_declaracion_venta:HANDLE IN FRAME F-Main , 'AFTER':U ).
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

  /* Code placed here will execute AFTER standard behavior.    */

  /*sentencia para publicar el evento declarado en el smartselect*/
  /*SUBSCRIBE "oechanged" IN h_dynselect-6.*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oechanged vTableWin 
PROCEDURE oechanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cNewValue AS CHARACTER NO-UNDO.
/*
DEFINE VAR cq AS CHARACTER NO-UNDO.

cq = 'for each items_orden_entrega where items_orden_entrega.id_orden_entrega = ' + cNewValue .

{set queryWhere cq h_d000itemsordenentrega}.
DYNAMIC-FUNCTION ('openQuery' IN h_d000itemsordenentrega ).
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

