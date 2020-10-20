&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

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

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-32 
&Scoped-Define DISPLAYED-OBJECTS fiClaro fiClaro400 fiTurbio fiTurbio400 ~
fiPulpa fiPulpa400 fiOtros fiOtros400 fiTot fiTot400 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValues fFrameWin  _DB-REQUIRED
FUNCTION getValues RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiClaro AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Jugo Claro" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiClaro400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiOtros AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Otros" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiOtros400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiPulpa AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Pulpa" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiPulpa400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiTot AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiTot400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiTurbio AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     LABEL "Jugo Turbio" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiTurbio400 AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 56 BY .24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiClaro AT ROW 1.71 COL 34 RIGHT-ALIGNED
     fiClaro400 AT ROW 1.71 COL 57 RIGHT-ALIGNED NO-LABEL
     fiTurbio AT ROW 2.91 COL 34 RIGHT-ALIGNED
     fiTurbio400 AT ROW 2.91 COL 57 RIGHT-ALIGNED NO-LABEL
     fiPulpa AT ROW 4.1 COL 34 RIGHT-ALIGNED
     fiPulpa400 AT ROW 4.1 COL 57 RIGHT-ALIGNED NO-LABEL
     fiOtros AT ROW 5.29 COL 34 RIGHT-ALIGNED
     fiOtros400 AT ROW 5.29 COL 57 RIGHT-ALIGNED NO-LABEL
     fiTot AT ROW 6.95 COL 34 RIGHT-ALIGNED
     fiTot400 AT ROW 6.95 COL 57 RIGHT-ALIGNED NO-LABEL
     RECT-32 AT ROW 6.52 COL 2
     "Kilos" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1 COL 18
          FONT 6
     "Kilos 400" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1 COL 41
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 57.8 BY 7.05.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 7.05
         WIDTH              = 57.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiClaro IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiClaro400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiOtros IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiOtros400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiPulpa IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiPulpa400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTot IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTot400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTurbio IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTurbio400 IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiClaro fiClaro400 fiTurbio fiTurbio400 fiPulpa fiPulpa400 fiOtros 
          fiOtros400 fiTot fiTot400 
      WITH FRAME fMain.
  ENABLE RECT-32 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTotales fFrameWin 
PROCEDURE setTotales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phSdo AS HANDLE     NO-UNDO.

  RUN setValues (DYNAMIC-FUNCTION('getTotClaro' IN phSdo),
                 DYNAMIC-FUNCTION('getTotTurbio' IN phSdo),
                 DYNAMIC-FUNCTION('getTotPulpa' IN phSdo),
                 DYNAMIC-FUNCTION('getTotOtros' IN phSdo),
                 DYNAMIC-FUNCTION('getTot400Claro' IN phSdo),
                 DYNAMIC-FUNCTION('getTot400Turbio' IN phSdo),
                 DYNAMIC-FUNCTION('getTot400Pulpa' IN phSdo),
                 DYNAMIC-FUNCTION('getTot400Otros' IN phSdo)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setValues fFrameWin 
PROCEDURE setValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pdKilClaro    AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilTurbio   AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilPulpa    AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKilOtros    AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKil4Claro   AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKil4Turbio  AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKil4Pulpa   AS DECIMAL    NO-UNDO. 
  DEFINE INPUT  PARAMETER pdKil4Otros   AS DECIMAL    NO-UNDO.

  ASSIGN fiClaro:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(pdKilClaro)
         fiTurbio:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(pdKilTurbio)
         fiPulpa:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(pdKilPulpa)
         fiOtros:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(pdKilOtros)
         fiClaro400:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(pdKil4Claro)
         fiTurbio400:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pdKil4Turbio)
         fiPulpa400:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(pdKil4Pulpa)
         fiOtros400:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(pdKil4Otros)
         fiTot:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = STRING(pdKilClaro + pdKilTurbio + pdKilPulpa)
         fiTot400:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(pdKil4Claro + pdKil4Turbio + pdKil4Pulpa)
         .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValues fFrameWin 
FUNCTION getValues RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cValues AS CHARACTER  NO-UNDO.

  cValues = fiClaro:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(1) + fiClaro400:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(10) + 
            fiTurbio:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(1) + fiTurbio400:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(10) + 
            fiPulpa:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(1) + fiPulpa400:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(10) + 
            fiOtros:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(1) + fiOtros400:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

  RETURN cValues.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

