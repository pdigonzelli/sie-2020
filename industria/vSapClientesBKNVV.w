&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          prog2sap         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dSapClientes.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dSapClientes.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.BZIRK RowObject.ZTERM ~
RowObject.VKBUR RowObject.KTGRD RowObject.WAERS RowObject.PLTYP ~
RowObject.VKGRP RowObject.AUFSD RowObject.KDGRP RowObject.STCD2 ~
RowObject.KONDA RowObject.COUNC RowObject.KALKS RowObject.KZAZU ~
RowObject.VSBED RowObject.LPRIO RowObject.VWERK RowObject.INCO1 ~
RowObject.INCO2 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.BZIRK RowObject.ZTERM ~
RowObject.VKBUR RowObject.KTGRD RowObject.WAERS RowObject.PLTYP ~
RowObject.VKGRP RowObject.AUFSD RowObject.KDGRP RowObject.STCD2 ~
RowObject.KONDA RowObject.COUNC RowObject.KALKS RowObject.KZAZU ~
RowObject.VSBED RowObject.LPRIO RowObject.VWERK RowObject.INCO1 ~
RowObject.INCO2 
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
     RowObject.BZIRK AT ROW 1.24 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.ZTERM AT ROW 1.24 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.VKBUR AT ROW 2.43 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.KTGRD AT ROW 2.43 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.WAERS AT ROW 3.62 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.PLTYP AT ROW 3.62 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.VKGRP AT ROW 4.81 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.AUFSD AT ROW 4.81 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.KDGRP AT ROW 6 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.STCD2 AT ROW 6 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.KONDA AT ROW 7.19 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.COUNC AT ROW 7.19 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.KALKS AT ROW 8.38 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.KZAZU AT ROW 8.38 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.VSBED AT ROW 9.57 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.LPRIO AT ROW 9.57 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.VWERK AT ROW 10.76 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.INCO1 AT ROW 11.95 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     RowObject.INCO2 AT ROW 13.14 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1
         SIZE 113.8 BY 13.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dSapClientes.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dSapClientes.i}
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
         HEIGHT             = 13.38
         WIDTH              = 113.8.
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
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

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

