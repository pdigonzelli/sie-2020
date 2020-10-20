&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general         PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dInspeccionesCamiones.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dInspeccionesCamiones.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.tipo_vehiculo ~
RowObject.parte_exterior_desc RowObject.sanidad_estado_desc ~
RowObject.olor_compartimiento_desc RowObject.observaciones ~
RowObject.calificacion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS radExterior radEvidencia radSanidad radOlor ~
radCalificacion RECT-1 RECT-12 RECT-13 RECT-2 RECT-3 RECT-4 RECT-5 
&Scoped-Define DISPLAYED-FIELDS RowObject.tipo_vehiculo ~
RowObject.id_inspeccion RowObject.parte_exterior ~
RowObject.parte_exterior_desc RowObject.evidencia_actividad ~
RowObject.sanidad_estado RowObject.sanidad_estado_desc ~
RowObject.olor_compartimiento RowObject.olor_compartimiento_desc ~
RowObject.observaciones RowObject.calificacion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS radExterior radEvidencia radSanidad ~
radOlor radCalificacion 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE radCalificacion AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Aceptado", "Aceptado",
"Aceptado c/Obs", "Aceptado c/Obs",
"Rechazado", "Rechazado"
     SIZE 50 BY .95 NO-UNDO.

DEFINE VARIABLE radEvidencia AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Insectos", "Insectos",
"Roedores", "Roedores",
"Pajaros", "Pajaros",
"Ninguna", "Ninguna"
     SIZE 25 BY 3.81 NO-UNDO.

DEFINE VARIABLE radExterior AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Limpia", "Limpia",
"Fangosa", "Fangosa",
"Sucia", "Sucia",
"Aceitosa", "Aceitosa",
"Otros (Describa)", "Otros"
     SIZE 22 BY 4.52 NO-UNDO.

DEFINE VARIABLE radOlor AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Limpio", "Limpio",
"Olor Desagradable", "Desagradable",
"Destilado Petroleo", "Petroleo",
"Podrido", "Podrido",
"Otros gases (describa)", "Otros"
     SIZE 29 BY 4.52 NO-UNDO.

DEFINE VARIABLE radSanidad AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Excelente", "Excelente",
"Aceptable", "Aceptable",
"Restos Emb. Ant.", "Restos",
"Danado", "Danado",
"Presenta Humedad", "Presenta Humedad",
"Sucio (Describa)", "Sucio"
     SIZE 23 BY 5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 1.43.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 2.86.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 2.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 6.19.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 5.71.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 6.67.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.tipo_vehiculo AT ROW 1.19 COL 14 COLON-ALIGNED
          LABEL "Vehiculo"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     RowObject.id_inspeccion AT ROW 1.19 COL 38 COLON-ALIGNED
          LABEL "Nro"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.parte_exterior AT ROW 2.62 COL 55 RIGHT-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     radExterior AT ROW 3.81 COL 2 NO-LABEL
     RowObject.parte_exterior_desc AT ROW 3.81 COL 24 NO-LABEL
          VIEW-AS EDITOR
          SIZE 32 BY 4.52
     RowObject.evidencia_actividad AT ROW 8.86 COL 55 RIGHT-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     radEvidencia AT ROW 10.05 COL 2 NO-LABEL
     RowObject.sanidad_estado AT ROW 1.24 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     radSanidad AT ROW 2.43 COL 62 NO-LABEL
     RowObject.sanidad_estado_desc AT ROW 2.43 COL 86 NO-LABEL
          VIEW-AS EDITOR
          SIZE 34 BY 5
     RowObject.olor_compartimiento AT ROW 8.14 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 39 BY 1
     radOlor AT ROW 9.33 COL 62 NO-LABEL
     RowObject.olor_compartimiento_desc AT ROW 9.33 COL 91 NO-LABEL
          VIEW-AS EDITOR
          SIZE 29 BY 4.52
     RowObject.observaciones AT ROW 14.57 COL 2 NO-LABEL
          VIEW-AS EDITOR
          SIZE 55 BY 2.14
     RowObject.calificacion AT ROW 14.57 COL 82 COLON-ALIGNED
          LABEL "Calificacion"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     radCalificacion AT ROW 15.76 COL 67 NO-LABEL
     RECT-1 AT ROW 1 COL 1
     RECT-12 AT ROW 14.33 COL 58
     RECT-13 AT ROW 14.33 COL 1
     RECT-2 AT ROW 2.43 COL 1
     RECT-3 AT ROW 8.62 COL 1
     RECT-4 AT ROW 1 COL 58
     RECT-5 AT ROW 7.67 COL 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 121.6 BY 16.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dInspeccionesCamiones.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dInspeccionesCamiones.i}
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
         WIDTH              = 121.6.
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

/* SETTINGS FOR FILL-IN RowObject.calificacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.evidencia_actividad IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN RowObject.id_inspeccion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.olor_compartimiento IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.parte_exterior IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN RowObject.sanidad_estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.tipo_vehiculo IN FRAME F-Main
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

&Scoped-define SELF-NAME radCalificacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radCalificacion vTableWin
ON VALUE-CHANGED OF radCalificacion IN FRAME F-Main
DO:
  rowObject.calificacion:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radEvidencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radEvidencia vTableWin
ON VALUE-CHANGED OF radEvidencia IN FRAME F-Main
DO:
  rowObject.evidencia_actividad:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radExterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radExterior vTableWin
ON VALUE-CHANGED OF radExterior IN FRAME F-Main
DO:
  rowObject.parte_exterior:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radOlor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radOlor vTableWin
ON VALUE-CHANGED OF radOlor IN FRAME F-Main
DO:
  rowObject.olor_compartimiento:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radSanidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radSanidad vTableWin
ON VALUE-CHANGED OF radSanidad IN FRAME F-Main
DO:
  rowObject.sanidad_estado:SCREEN-VALUE = SELF:SCREEN-VALUE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customEnable vTableWin 
PROCEDURE customEnable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER plEnable AS LOGICAL    NO-UNDO.

  radExterior:SENSITIVE IN FRAME F-Main     = plEnable.
  radEvidencia:SENSITIVE IN FRAME F-Main    = plEnable.
  radSanidad:SENSITIVE IN FRAME F-Main      = plEnable.
  radOlor:SENSITIVE IN FRAME F-Main         = plEnable.
  radCalificacion:SENSITIVE IN FRAME F-Main = plEnable.
  
  

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

  RUN customEnable(FALSE).

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
  DEFINE VARIABLE cFieldValue    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFieldName     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hTableioSource AS HANDLE    NO-UNDO.

  ASSIGN cFieldHandles  = DYNAMIC-FUNCTION('getAllFieldHandles') 
         iNumEntries    = NUM-ENTRIES(cFieldHandles)
         hTableioSource = DYNAMIC-FUNCTION('getTableIOSource') NO-ERROR.
  
  DO iLoop = 1 TO iNumEntries:
    ASSIGN hField      = WIDGET-HANDLE(ENTRY(iLoop,cFieldHandles))
           cFieldName  = hField:NAME 
           cFieldValue = hField:SCREEN-VALUE NO-ERROR.
    
    IF cFieldName = "parte_exterior" THEN 
      radExterior:SCREEN-VALUE IN FRAM F-Main = cFieldValue.
    
    IF cFieldName = "evidencia_actividad" THEN 
      radEvidencia:SCREEN-VALUE IN FRAM F-Main = cFieldValue.

    IF cFieldName = "sanidad_estado" THEN 
      radSanidad:SCREEN-VALUE IN FRAM F-Main = cFieldValue.

    IF cFieldName = "olor_compartimiento" THEN 
      radOlor:SCREEN-VALUE IN FRAM F-Main = cFieldValue.

    IF cFieldName = "calificacion" THEN 
      radCalificacion:SCREEN-VALUE IN FRAM F-Main = cFieldValue.

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

  RUN customEnable(TRUE).

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

  RUN customEnable(FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

