&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"ditemscontroltareas.i"}.



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
&Scoped-define DATA-FIELD-DEFS "ditemscontroltareas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.ajuste_categoria ~
RowObject.compensa_hs RowObject.tipo_turno RowObject.cant_jornal_norm ~
RowObject.cant_hs_norm RowObject.cant_hs_compensa RowObject.cant_hs_extras ~
RowObject.cantidad RowObject.nro_maquina RowObject.nro_tractor ~
RowObject.hora_inicio RowObject.hora_fin RowObject.hora_inicio-1 ~
RowObject.hora_fin-1 RowObject.id_lote RowObject.id_reserva ~
RowObject.cantidad_adicional 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.ajuste_categoria ~
RowObject.compensa_hs RowObject.legajo RowObject.nombre ~
RowObject.tipo_turno RowObject.cant_jornal_norm RowObject.cant_hs_norm ~
RowObject.cant_hs_compensa RowObject.cant_hs_extras RowObject.cantidad ~
RowObject.nro_maquina RowObject.nro_tractor RowObject.hora_inicio ~
RowObject.hora_fin RowObject.hora_inicio-1 RowObject.hora_fin-1 ~
RowObject.id_lote RowObject.id_proveedor RowObject.id_origen ~
RowObject.id_reserva RowObject.id_empresa RowObject.id_sector ~
RowObject.id_sucursal RowObject.cantidad_adicional 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dtareas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dunidadesliquidacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-3 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.ajuste_categoria AT ROW 9.76 COL 68 WIDGET-ID 52
          LABEL "Ajuste Categoria"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     RowObject.compensa_hs AT ROW 9.81 COL 15.6 WIDGET-ID 48
          LABEL "Compensa Hs"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81
     RowObject.legajo AT ROW 1 COL 13.4 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.nombre AT ROW 2.19 COL 18.2 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     RowObject.tipo_turno AT ROW 4.57 COL 13.4 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     RowObject.cant_jornal_norm AT ROW 5.76 COL 13.4 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cant_hs_norm AT ROW 5.76 COL 42.2 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cant_hs_compensa AT ROW 6.95 COL 13.6 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cant_hs_extras AT ROW 6.95 COL 42.2 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.cantidad AT ROW 8.57 COL 13.4 COLON-ALIGNED WIDGET-ID 28
          LABEL "Cantidad"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.nro_maquina AT ROW 10.76 COL 36.4 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     RowObject.nro_tractor AT ROW 10.76 COL 69.2 COLON-ALIGNED WIDGET-ID 26 FORMAT ">>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.hora_inicio AT ROW 11.95 COL 36 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     RowObject.hora_fin AT ROW 11.95 COL 69 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     RowObject.hora_inicio-1 AT ROW 13.05 COL 36 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     RowObject.hora_fin-1 AT ROW 13.05 COL 69 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     RowObject.id_lote AT ROW 3.52 COL 51.4 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     RowObject.id_proveedor AT ROW 5.81 COL 76 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.id_origen AT ROW 6.81 COL 76 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RowObject.id_reserva AT ROW 9.57 COL 45.8 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_empresa AT ROW 1 COL 53.4 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.id_sector AT ROW 3.57 COL 69 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_sucursal AT ROW 4.57 COL 69 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.cantidad_adicional AT ROW 14.33 COL 37 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     "1er Intervalo" VIEW-AS TEXT
          SIZE 20 BY .71 AT ROW 12.1 COL 3 WIDGET-ID 36
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "2do Intervalo" VIEW-AS TEXT
          SIZE 20 BY .71 AT ROW 13.14 COL 2.8 WIDGET-ID 38
          BGCOLOR 1 FGCOLOR 15 
     " Datos Adicionales" VIEW-AS TEXT
          SIZE 19 BY 1.19 AT ROW 14.1 COL 3 WIDGET-ID 60
          BGCOLOR 1 FGCOLOR 15 
     SPACE(70.80) SKIP(0.00)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "ditemscontroltareas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {ditemscontroltareas.i}
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
         HEIGHT             = 14.43
         WIDTH              = 92.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.ajuste_categoria IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.cantidad IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.compensa_hs IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.id_empresa IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_empresa:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_origen IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_origen:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_proveedor IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_proveedor:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_sector IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_sector:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.id_sucursal IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.id_sucursal:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.legajo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.nro_tractor IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RowObject.id_lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_lote vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_lote IN FRAME F-Main /* Lote */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "blotesplantacion.w",
                                 INPUT "dlotesplantacion.w",
                                 INPUT "id_lote",
                                 INPUT "lotes_plantacion.id_proveedor = " + rowObject.id_proveedor:SCREEN-VALUE + 
                                       " and lotes_plantacion.id_origen = " + rowObject.id_origen:SCREEN-VALUE, 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       RowObject.id_lote:SCREEN-VALUE = xfieldResult. 
       RUN fieldModified (SELF:HANDLE).
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_reserva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_reserva vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_reserva IN FRAME F-Main /* Reserva */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
RUN adm2/support/gConsultas.w (INPUT "breservahoras.w",
                               INPUT "dreservashoras.w",
                               INPUT "id_reserva",
                               INPUT "reserva_horas.id_empresa = " + RowObject.id_empresa:SCREEN-VALUE +
                                     " and reserva_horas.id_sector = " + RowObject.id_sector:SCREEN-VALUE +
                                     " and reserva_horas.id_sucursal = " + RowObject.id_sucursal:SCREEN-VALUE +
                                     " and reserva_horas.legajo = " + RowObject.legajo:SCREEN-VALUE +
                                     " and reserva_horas.cant_horas <> reserva_horas.cant_hs_consumidas",
                               OUTPUT xfieldResult).

IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:

       RowObject.id_reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = xfieldResult.     
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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
             INPUT  'dunidadesliquidacion.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedunidadesliquidacionOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dunidadesliquidacion ).
       RUN repositionObject IN h_dunidadesliquidacion ( 1.24 , 82.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldid_unidad_liquidacionDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsid_unidad_liquidacion,abreviatura,descripcionExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_unidad_liquidacionDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_unidad_liquidacion':U ,
             OUTPUT h_dynselect-3 ).
       RUN repositionObject IN h_dynselect-3 ( 8.57 , 47.60 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-3 ( 1.00 , 20.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dtareas.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedtareasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dtareas ).
       RUN repositionObject IN h_dtareas ( 1.71 , 75.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldid_tareaDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitleBrowseFieldsid_tarea,abreviatura,descripcionExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameid_tareaDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyFieldid_tarea':U ,
             OUTPUT h_dynselect ).
       RUN repositionObject IN h_dynselect ( 3.43 , 20.40 ) NO-ERROR.
       RUN resizeObject IN h_dynselect ( 1.00 , 23.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-3. */
       RUN addLink ( h_dunidadesliquidacion , 'Data':U , h_dynselect-3 ).

       /* Links to SmartDataField h_dynselect. */
       RUN addLink ( h_dtareas , 'Data':U , h_dynselect ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect ,
             RowObject.nombre:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynselect-3 ,
             RowObject.id_origen:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  RowObject.id_lote:LOAD-MOUSE-POINTER("glove") IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

