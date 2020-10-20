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
       {"dProduccionJugo.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dProduccionJugo.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Bx_20_20 RowObject.Acidez_w_w ~
RowObject.Pulpa RowObject.Sodio RowObject.vitaminac RowObject.nitrogeno ~
RowObject.abs520 RowObject.abs_8_430 RowObject.benzoato RowObject.bisulfito ~
RowObject.pulpa_85 RowObject.t_600 RowObject.ratio 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.Bx_20_20 RowObject.Acidez_w_w ~
RowObject.Bx_correg RowObject.Acidez_w_v RowObject.Pulpa RowObject.Sodio ~
RowObject.vitaminac RowObject.nitrogeno RowObject.abs520 ~
RowObject.abs_8_430 RowObject.benzoato RowObject.bisulfito ~
RowObject.pulpa_85 RowObject.t_600 RowObject.ratio 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Bx_20_20 AT ROW 1 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Acidez_w_w AT ROW 1.95 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Bx_correg AT ROW 2.95 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Acidez_w_v AT ROW 4 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Pulpa AT ROW 5 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Sodio AT ROW 6 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.vitaminac AT ROW 7 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.nitrogeno AT ROW 8 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.abs520 AT ROW 1 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.abs_8_430 AT ROW 2 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.benzoato AT ROW 3 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.bisulfito AT ROW 4 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.pulpa_85 AT ROW 5 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.t_600 AT ROW 6 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.ratio AT ROW 7 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 71.8 BY 8.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dProduccionJugo.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dProduccionJugo.i}
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
         HEIGHT             = 8
         WIDTH              = 71.8.
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
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Bx_correg IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.Acidez_w_w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Acidez_w_w vTableWin
ON LEAVE OF RowObject.Acidez_w_w IN FRAME F-Main /* Acidez_p/p */
DO:
  rowObject.bx_correg:SCREEN-VALUE  = DYNAMIC-FUNCTION('getBrixCorregido' IN hLib, DECIMAL(rowObject.bx_20_20:SCREEN-VALUE), 
                                                                                  DECIMAL(SELF:SCREEN-VALUE)).

  rowObject.acidez_w_v:SCREEN-VALUE = DYNAMIC-FUNCTION('getAcidezGPL' IN hLib, DECIMAL(rowObject.bx_20_20:SCREEN-VALUE), 
                                                                               DECIMAL(SELF:SCREEN-VALUE)).

  
  rowObject.ratio:SCREEN-VALUE      = DYNAMIC-FUNCTION('getRatio' IN hLib, DECIMAL(rowObject.bx_20_20:SCREEN-VALUE),
                                                                           DECIMAL(rowObject.acidez_w_w:SCREEN-VALUE)).

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

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
