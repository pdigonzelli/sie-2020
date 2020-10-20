&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
          produccion       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dliqcontroltareas.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dliqcontroltareas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.id_empresa RowObject.id_sector ~
RowObject.id_sucursal RowObject.fecha RowObject.id_tipo_planilla ~
RowObject.id_origen RowObject.id_proveedor RowObject.id_grupo ~
RowObject.observaciones 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.id_empresa RowObject.id_sector ~
RowObject.id_sucursal RowObject.fecha RowObject.id_tipo_planilla ~
RowObject.nro_planilla RowObject.id_origen RowObject.id_proveedor ~
RowObject.id_grupo RowObject.observaciones 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS nombre-empresa nombre-sector nombre-finca ~
nombre-productor 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 59 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-finca AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-productor AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.id_empresa AT ROW 1 COL 18 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     nombre-empresa AT ROW 1 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     RowObject.id_sector AT ROW 2 COL 18 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     nombre-sector AT ROW 1.95 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     RowObject.id_sucursal AT ROW 3 COL 18 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.fecha AT ROW 4.1 COL 18 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_tipo_planilla AT ROW 4.1 COL 63 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.nro_planilla AT ROW 4.1 COL 84 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.id_origen AT ROW 5.76 COL 18 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     nombre-finca AT ROW 5.52 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     RowObject.id_proveedor AT ROW 6.86 COL 18 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     nombre-productor AT ROW 6.95 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     RowObject.id_grupo AT ROW 7.91 COL 18 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.observaciones AT ROW 9.33 COL 20 NO-LABEL WIDGET-ID 38
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 75 BY 2.14
     "Observaciones" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 9.33 COL 3 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dliqcontroltareas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dliqcontroltareas.i}
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
         HEIGHT             = 10.52
         WIDTH              = 106.6.
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

/* SETTINGS FOR FILL-IN nombre-empresa IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-finca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-productor IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sector IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.nro_planilla IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR RowObject.observaciones IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.id_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_empresa vTableWin
ON LEAVE OF RowObject.id_empresa IN FRAME F-Main /* Empresa */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_empresa vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_empresa IN FRAME F-Main /* Empresa */
DO:
  DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bliqempresas.w",
                                 INPUT "dliqempresas.w",
                                 INPUT "id_empresa_liq",
                                 INPUT "", 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       RowObject.id_empresa:SCREEN-VALUE = xfieldResult. 
       RUN fieldModified (SELF:HANDLE).
       RUN descriptivos.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_origen vTableWin
ON LEAVE OF RowObject.id_origen IN FRAME F-Main /* Origen */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_origen vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_origen IN FRAME F-Main /* Origen */
DO:
    define var r as rowid no-undo.

    run wselfinca.w (output r).
    find FIRST origenes where rowid(origenes) = r no-lock no-error.
    if available origenes then 
      do:
          RowObject.id_proveedor:screen-value = string(origenes.id_proveedor).
          RowObject.id_origen:screen-value = string(origenes.id_origen).
          nombre-finca:screen-value = origenes.descripcion.
          RUN fieldModified (SELF:HANDLE).
      end.
      RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.id_sector
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_sector vTableWin
ON LEAVE OF RowObject.id_sector IN FRAME F-Main /* Sector */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.id_sector vTableWin
ON MOUSE-SELECT-DBLCLICK OF RowObject.id_sector IN FRAME F-Main /* Sector */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bliqsectores.w",
                                 INPUT "dliqsectores.w",
                                 INPUT "id_sector_liq",
                                 INPUT "liq_sectores.id_empresa_liq = " + RowObject.id_empresa:SCREEN-VALUE, 
                                 OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
       RowObject.id_sector:SCREEN-VALUE = xfieldResult. 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

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
FIND FIRST liq_empresas WHERE liq_empresas.id_empresa_liq = integer(RowObject.id_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_empresas THEN
    nombre-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = liq_empresas.descripcion.
  ELSE
   nombre-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

FIND FIRST liq_sectores WHERE liq_sectores.id_empresa_liq = integer(RowObject.id_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND 
    liq_sectores.id_sector_liq = integer(RowObject.id_sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
   IF AVAILABLE liq_sectores THEN
       nombre-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME} = liq_sectores.descripcion.
     ELSE
      nombre-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


FIND FIRST origenes WHERE origenes.id_proveedor = integer(RowObject.id_proveedor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
    origenes.id_origen = integer(rowobject.id_origen:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE origenes THEN
DO:
    nombre-finca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = origenes.descripcion.
    FIND FIRST proveedores WHERE proveedores.id_proveedor = integer(RowObject.id_proveedor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF AVAILABLE proveedores THEN
        nombre-productor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = proveedores.nombre.
      ELSE
       nombre-productor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.
ELSE
    nombre-finca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

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

