&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"drcatperstareas.i"}.



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
&Scoped-define DATA-FIELD-DEFS "drcatperstareas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.id_convenio RowObject.id_categoria ~
RowObject.id_categoria_tarea RowObject.id_codigo_abacus ~
RowObject.id_diferencial RowObject.id_codigo_abacus_diferencial 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.id_convenio ~
RowObject.id_categoria RowObject.id_categoria_tarea ~
RowObject.id_codigo_abacus RowObject.id_diferencial ~
RowObject.id_codigo_abacus_diferencial 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS nombre-categoria-legajo ~
nombre-categoria-tarea nombre-abacus nombre-diferencial ~
nombre-abacus-diferencial 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE nombre-abacus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-abacus-diferencial AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-categoria-legajo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-categoria-tarea AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-diferencial AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_convenio AT ROW 1 COL 18 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     nombre-categoria-legajo AT ROW 1.95 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     RowObject.id_categoria AT ROW 2 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     RowObject.id_categoria_tarea AT ROW 3 COL 18 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     nombre-categoria-tarea AT ROW 3.14 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     RowObject.id_codigo_abacus AT ROW 4 COL 18 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     nombre-abacus AT ROW 4.1 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     RowObject.id_diferencial AT ROW 5.29 COL 18 COLON-ALIGNED WIDGET-ID 20
          LABEL "Tipo Dif"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     nombre-diferencial AT ROW 5.29 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     RowObject.id_codigo_abacus_diferencial AT ROW 6.48 COL 18 COLON-ALIGNED WIDGET-ID 8 FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     nombre-abacus-diferencial AT ROW 6.52 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "drcatperstareas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {drcatperstareas.i}
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
         HEIGHT             = 9.14
         WIDTH              = 80.6.
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

/* SETTINGS FOR FILL-IN RowObject.id_codigo_abacus_diferencial IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.id_diferencial IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nombre-abacus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-abacus-diferencial IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-categoria-legajo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-categoria-tarea IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-diferencial IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RowObject.id_categoria_tarea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_categoria_tarea vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_categoria_tarea IN FRAME F-Main /* Categoria Tarea */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bcategoriastareas.w",
                                INPUT "dcategoriastareas.w",
                                INPUT "id_categoria_tarea",
                                INPUT "", 
                                OUTPUT xfieldResult).

  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
      RowObject.id_categoria_tarea:SCREEN-VALUE = xfieldResult. 
      RUN fieldModified (SELF:HANDLE).
      RUN descriptivos.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_codigo_abacus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_codigo_abacus vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_codigo_abacus IN FRAME F-Main /* Cod.Abacus */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "bconceptosabacus.w",
                              INPUT "dconceptosabacus.w",
                              INPUT "id_concepto",
                              INPUT "", 
                              OUTPUT xfieldResult).

IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
    RowObject.id_codigo_abacus:SCREEN-VALUE = xfieldResult. 
    RUN fieldModified (SELF:HANDLE).
    RUN descriptivos.
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_codigo_abacus_diferencial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_codigo_abacus_diferencial vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_codigo_abacus_diferencial IN FRAME F-Main /* Cod.Abacus Dif */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "bconceptosabacus.w",
                            INPUT "dconceptosabacus.w",
                            INPUT "id_concepto",
                            INPUT "", 
                            OUTPUT xfieldResult).

IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
  RowObject.id_codigo_abacus_diferencial:SCREEN-VALUE = xfieldResult. 
  RUN fieldModified (SELF:HANDLE).
  RUN descriptivos.
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_diferencial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_diferencial vTableWin
ON LEAVE OF RowObject.id_diferencial IN FRAME F-Main /* Tipo Dif */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_diferencial vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_diferencial IN FRAME F-Main /* Tipo Dif */
DO:
DEFINE VAR hfield AS HANDLE NO-UNDO.
DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "bdiferenciales.w",
                         INPUT "ddiferenciales.w",
                         INPUT "id_diferencial",
                         INPUT "", 
                         OUTPUT xfieldResult).

IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
RowObject.id_diferencial:SCREEN-VALUE = xfieldResult. 
RUN fieldModified (SELF:HANDLE).
RUN descriptivos.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN descriptivos.
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
DEF BUFFER bconceptos FOR conceptos_abacus.
DEF BUFFER bconcep01 FOR conceptos_abacus.

FIND FIRST categorias_tareas WHERE categorias_tareas.id_categoria_tarea = INTEGER(RowObject.id_categoria_tarea:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAILABLE categorias_tareas THEN nombre-categoria-tarea:SCREEN-VALUE = categorias_tareas.descripcion.
  ELSE nombre-categoria-tarea:SCREEN-VALUE = "".

FIND FIRST liq_categorias WHERE liq_categorias.id_convenio = INTEGER(RowObject.id_convenio:SCREEN-VALUE IN FRAME {&FRAME-NAME}) and
        liq_categorias.id_categoria = integer(RowObject.id_categoria:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
         IF AVAILABLE liq_categorias THEN nombre-categoria-legajo:SCREEN-VALUE = liq_categorias.descripcion.
         ELSE nombre-categoria-legajo:SCREEN-VALUE = "".

FIND FIRST conceptos_abacus WHERE conceptos_abacus.id_concepto = INTEGER(RowObject.id_codigo_abacus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAILABLE conceptos_abacus THEN nombre-abacus:SCREEN-VALUE = conceptos_abacus.descripcion.
    ELSE nombre-abacus:SCREEN-VALUE = "".

FIND FIRST diferenciales WHERE diferenciales.id_diferencial = INTEGER(RowObject.id_diferencial:SCREEN-VALUE) NO-LOCK NO-ERROR.
         IF AVAILABLE diferenciales THEN 
              nombre-diferencial:SCREEN-VALUE = diferenciales.descripcion.
              ELSE 
              nombre-diferencial:SCREEN-VALUE = "".

FIND FIRST bconceptos WHERE bconceptos.id_concepto = INTEGER(RowObject.id_codigo_abacus_diferencial:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
                IF AVAILABLE bconceptos THEN nombre-abacus-diferencial:SCREEN-VALUE = bconceptos.descripcion.
                  ELSE nombre-abacus-diferencial:SCREEN-VALUE = "".


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

