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
&Scoped-Define ENABLED-FIELDS RowObject.legajo_rhpro RowObject.legajo ~
RowObject.tipo_liquidacion RowObject.apellido_nombre RowObject.liquida ~
RowObject.cuil RowObject.domicilio RowObject.localidad ~
RowObject.codigo_postal RowObject.sexo RowObject.fecha_nacimiento ~
RowObject.id_centro_costo_liq RowObject.fecha_ingreso ~
RowObject.fecha_inicio_temp RowObject.fecha_egreso ~
RowObject.id_motivo_egreso RowObject.id_division RowObject.id_ccostos_liq ~
RowObject.id_ccargo_liq RowObject.id_ccategoria_liq ~
RowObject.id_ccontrato_liq RowObject.id_cconvenio_liq 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.legajo_rhpro RowObject.legajo ~
RowObject.tipo_liquidacion RowObject.apellido_nombre RowObject.liquida ~
RowObject.cuil RowObject.domicilio RowObject.localidad ~
RowObject.codigo_postal RowObject.sexo RowObject.fecha_nacimiento ~
RowObject.id_centro_costo_liq RowObject.fecha_ingreso ~
RowObject.fecha_inicio_temp RowObject.fecha_egreso ~
RowObject.id_motivo_egreso RowObject.id_division RowObject.id_ccostos_liq ~
RowObject.id_ccargo_liq RowObject.id_ccategoria_liq ~
RowObject.id_ccontrato_liq RowObject.id_cconvenio_liq 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS desc-centrocosto desc-motivobaja ~
desc-division desc-cc desc-cargo desc-categoria desc-contrato desc-convenio 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dliqempresas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE desc-cargo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 66 BY .95 NO-UNDO.

DEFINE VARIABLE desc-categoria AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 66 BY .95 NO-UNDO.

DEFINE VARIABLE desc-cc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 65 BY .95 NO-UNDO.

DEFINE VARIABLE desc-centrocosto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .95 NO-UNDO.

DEFINE VARIABLE desc-contrato AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 67 BY .95 NO-UNDO.

DEFINE VARIABLE desc-convenio AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 67 BY .95 NO-UNDO.

DEFINE VARIABLE desc-division AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY .95 NO-UNDO.

DEFINE VARIABLE desc-motivobaja AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.legajo_rhpro AT ROW 2.67 COL 20 COLON-ALIGNED WIDGET-ID 44
          LABEL "Legajo Rhpro"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     RowObject.legajo AT ROW 2.67 COL 92 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.tipo_liquidacion AT ROW 3.86 COL 110 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     RowObject.apellido_nombre AT ROW 4.19 COL 20 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     RowObject.liquida AT ROW 5.05 COL 110 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.cuil AT ROW 5.19 COL 20 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.domicilio AT ROW 6.19 COL 20 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     RowObject.localidad AT ROW 7.19 COL 20 COLON-ALIGNED WIDGET-ID 38 FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
     RowObject.codigo_postal AT ROW 7.24 COL 94.2 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.sexo AT ROW 8.14 COL 54 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     RowObject.fecha_nacimiento AT ROW 8.24 COL 20 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_centro_costo_liq AT ROW 8.62 COL 94 COLON-ALIGNED WIDGET-ID 76
          LABEL "Cod Cab"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     desc-centrocosto AT ROW 8.62 COL 108 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     RowObject.fecha_ingreso AT ROW 9.29 COL 20 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.fecha_inicio_temp AT ROW 9.33 COL 61 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.fecha_egreso AT ROW 10.52 COL 20 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_motivo_egreso AT ROW 10.52 COL 61 COLON-ALIGNED WIDGET-ID 72
          LABEL "Mot.Baja"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     desc-motivobaja AT ROW 10.52 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     RowObject.id_division AT ROW 12.19 COL 20 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     desc-division AT ROW 12.19 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     RowObject.id_ccostos_liq AT ROW 13.14 COL 20 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     desc-cc AT ROW 13.38 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     RowObject.id_ccargo_liq AT ROW 14.19 COL 20 COLON-ALIGNED WIDGET-ID 46
          LABEL "Cargo Liq"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     desc-cargo AT ROW 14.48 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     RowObject.id_ccategoria_liq AT ROW 15.19 COL 20 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     desc-categoria AT ROW 15.52 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     RowObject.id_ccontrato_liq AT ROW 16.38 COL 20 COLON-ALIGNED WIDGET-ID 50
          LABEL "Contrato Liq"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     desc-contrato AT ROW 16.48 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     RowObject.id_cconvenio_liq AT ROW 17.57 COL 20 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     desc-convenio AT ROW 17.67 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     SPACE(23.80) SKIP(0.00)
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
         HEIGHT             = 18.14
         WIDTH              = 156.
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

/* SETTINGS FOR FILL-IN desc-cargo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-categoria IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-cc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-centrocosto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-contrato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-convenio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-division IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-motivobaja IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_ccargo_liq IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_ccontrato_liq IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_centro_costo_liq IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_motivo_egreso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.legajo_rhpro IN FRAME F-Main
   EXP-LABEL                                                            */
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
       RUN repositionObject IN h_dliqempresas ( 1.48 , 125.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFielddescripcionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelEmpresaSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsid_empresa_liq,descripcionExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_empresa_liqDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_empresa_liq':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 1.00 , 22.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 69.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dliqempresas , 'Data':U , h_dynselect ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.legajo_rhpro:HANDLE IN FRAME F-Main , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable vTableWin 
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).

  /* Code placed here will execute AFTER standard behavior.    */
  RUN descriptivos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos vTableWin 
PROCEDURE descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST liq_divisionliq WHERE liq_divisionliq.id_division = INTEGER(rowObject.id_division:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_divisionliq THEN
    desc-division:SCREEN-VALUE = liq_divisionliq.descripcion.
   ELSE
    desc-division:SCREEN-VALUE = "".

 FIND FIRST liq_ccostosliq WHERE liq_ccostosliq.id_centro_costo_liq = INTEGER(rowObject.id_ccostos_liq:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF AVAILABLE liq_ccostosliq THEN
        desc-cc:SCREEN-VALUE = liq_ccostosliq.descripcion.
       ELSE
        desc-cc:SCREEN-VALUE = "".

 FIND FIRST liq_ccargosliq WHERE liq_ccargosliq.id_cargo = INTEGER(rowObject.id_ccargo_liq:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
           IF AVAILABLE liq_ccargosliq THEN
               desc-cargo:SCREEN-VALUE = liq_ccargosliq.descripcion.
              ELSE
               desc-cargo:SCREEN-VALUE = "".

FIND FIRST liq_ccategoriasliq WHERE liq_ccategoriasliq.id_categoria = INTEGER(rowObject.id_ccategoria_liq:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
     IF AVAILABLE liq_ccategoriasliq THEN
        desc-categoria:SCREEN-VALUE = liq_ccategoriasliq.descripcion.
     ELSE
       desc-categoria:SCREEN-VALUE = "".

FIND FIRST liq_ccontratosliq WHERE liq_ccontratosliq.id_contrato = INTEGER(rowObject.id_ccontrato_liq:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_ccontratosliq THEN
    desc-contrato:SCREEN-VALUE = liq_ccontratosliq.descripcion.
  ELSE
    desc-contrato:SCREEN-VALUE = "".


FIND FIRST liq_cconveniosliq WHERE liq_cconveniosliq.id_convenio = INTEGER(rowObject.id_cconvenio_liq:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAILABLE liq_cconveniosliq THEN
     desc-convenio:SCREEN-VALUE = liq_cconveniosliq.descripcion.
  ELSE
    desc-convenio:SCREEN-VALUE = "".

FIND FIRST liq_motivo_baja WHERE liq_motivo_baja.id_motivo_baja = INTEGER(rowObject.id_motivo_egreso:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
      IF AVAILABLE liq_motivo_baja THEN
         desc-motivobaja:SCREEN-VALUE = liq_motivo_baja.descripcion.
      ELSE
        desc-motivobaja:SCREEN-VALUE = "".


FIND FIRST liq_centros_costos WHERE 
    liq_centros_costos.id_centro_costo_liq = INTEGER(rowObject.id_centro_costo_liq:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
           IF AVAILABLE liq_centros_costos THEN
               desc-centrocosto:SCREEN-VALUE = liq_centros_costos.descripcion.
              ELSE
               desc-centrocosto:SCREEN-VALUE = "".



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

