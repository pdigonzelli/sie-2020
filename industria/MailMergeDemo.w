&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

DEF STREAM DataStream.

DEF VAR pv_chWord AS COM-HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cNotes btnPrint Btn_Cancel Btn_Help RECT-5 
&Scoped-Define DISPLAYED-OBJECTS cNotes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MailMerge Dialog-Frame 
FUNCTION MailMerge RETURNS LOGICAL
  ( INPUT ip_cDocument          AS CHAR,
    INPUT ip_cDataFile     AS CHAR,
    INPUT ip_cMailMergeDoc AS CHAR,
    INPUT iplPrint        AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE ProgressBar AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chProgressBar AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnPrint AUTO-GO 
     LABEL "&Merge" 
     SIZE 15.4 BY 1.24
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Close" 
     SIZE 15.4 BY 1.24
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15.4 BY 1.24
     BGCOLOR 8 .

DEFINE VARIABLE cNotes AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 67.6 BY 6.1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70.6 BY 6.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cNotes AT ROW 1.48 COL 3.2 NO-LABEL
     btnPrint AT ROW 8.24 COL 26.2
     Btn_Cancel AT ROW 8.24 COL 41.6
     Btn_Help AT ROW 8.24 COL 57
     RECT-5 AT ROW 1.19 COL 1.8
     SPACE(2.19) SKIP(3.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Generate Word 2000 Mail Merge Document".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

ASSIGN 
       cNotes:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME ProgressBar ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 9.67
       COLUMN          = 1.6
       HEIGHT          = .86
       WIDTH           = 70.6
       HIDDEN          = no
       SENSITIVE       = yes.
      ProgressBar:NAME = "ProgressBar":U .
/* ProgressBar OCXINFO:CREATE-CONTROL from: {0713E8D2-850A-101B-AFC0-4210102A8DA7} type: ProgressBar */
      ProgressBar:MOVE-AFTER(Btn_Help:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Generate Word 2000 Mail Merge Document */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint Dialog-Frame
ON CHOOSE OF btnPrint IN FRAME Dialog-Frame /* Merge */
DO:
  ASSIGN SELF:SENSITIVE = NO.
  RUN PrintLetters IN THIS-PROCEDURE.
  ASSIGN SELF:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

CREATE "Word.Application" pv_chWord NO-ERROR.                              /* Try & create word 97 Object */

IF ERROR-STATUS:ERROR OR NOT VALID-HANDLE(pv_chWord) THEN
DO:
 MESSAGE "Word Is Not Installed On This Machine." SKIP
         VIEW-AS ALERT-BOX INFORMATION.
 RETURN.
END.
           
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ASSIGN cNotes = "This is the mail merge demo for Word 2000~n~n"       +
                "This demo will run through all customers,~n"        +
                "and create a mail merge document based on~n"        +
                "the customer.doc file that was alse supplied~n"     + 
                "with this demo.~n~nPress the Merge Button to start".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.
  
  ProgressBar:MOVE-TO-BOTTOM().
  
  APPLY "ENTRY":U TO btnPrint.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

pv_chWord:Documents:Close(NO) NO-ERROR.                                    /* make sure all documents are closed */

pv_chWord:Quit().                                                          /* Destroy Word Object */

RELEASE OBJECT pv_chWord NO-ERROR.                                         /* Make sure we release Word Object !! */

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "MailMergeDemo.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chProgressBar = ProgressBar:COM-HANDLE
    UIB_S = chProgressBar:LoadControls( OCXFile, "ProgressBar":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "MailMergeDemo.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY cNotes 
      WITH FRAME Dialog-Frame.
  ENABLE cNotes btnPrint Btn_Cancel Btn_Help RECT-5 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printLetters Dialog-Frame 
PROCEDURE printLetters :
DEF VAR lv_iSeq     AS INT NO-UNDO.
 DEF VAR lv_iPercent AS INT NO-UNDO.
 DEF VAR lv_iTotal   AS INT NO-UNDO.

 DEF VAR lv_cDocument      AS CHAR INIT "customer.doc":U      NO-UNDO. 
 DEF VAR lv_cDataFile      AS CHAR INIT "MailMergeDemo.dat":U NO-UNDO. 
 DEF VAR lv_cMailMergeFile AS CHAR INIT "MailMergeDemo.doc":U NO-UNDO.

 FOR EACH Customer FIELDS NO-LOCK lv_iTotal = 1 TO lv_iTotal + 1:                 /* count number of "letters" to send */
 END.

 OUTPUT STREAM DataStream TO VALUE(lv_cDataFile).                              /* create data file */
 
 FOR EACH Customer NO-LOCK lv_iSeq = 1 TO lv_iSeq + 1:                            /* run through all customers */
 
  PROCESS EVENTS.                                                           /* keep the system ticking away .. */
  
  ASSIGN lv_iPercent = MIN(100,INT((lv_iSeq / lv_iTotal) * 100))                     /* get % done */
         chProgressBar:CONTROLS:ITEM(1):Value = lv_iPercent                    /* set progress bar */
         FRAME {&FRAME-NAME}:TITLE = "Generating Letter " + STRING(lv_iSeq) +  /* display more info to user */
                                     " Of " + STRING(lv_iTotal) .
   
  EXPORT STREAM DataStream DELIMITER "~t":U                                 /* export the data */
      sports.Customer.Cust-Num 
      sports.Customer.Name 
      sports.Customer.Contact 
      sports.Customer.Address 
      sports.Customer.Address2 
      sports.Customer.City 
      sports.Customer.Postal-Code 
      sports.Customer.State 
      sports.Customer.Country 
      sports.Customer.Credit-Limit 
      sports.Customer.Discount 
      sports.Customer.Balance 
      sports.Customer.Sales-Rep.
      
 END.
  
 OUTPUT STREAM DataStream CLOSE.                                            /* close data file */
 
 ASSIGN FRAME {&FRAME-NAME}:TITLE = "Mail Merging Document In Word".

 ASSIGN pv_chWord:Visible = YES.                                               /* Word is not "hidden" from user */
  
 MailMerge(lv_cDocument,                                                       /* Main Document */
           lv_cDataFile,                                                       /* File that holds all the data */
           lv_cMailMergeFile,                                                  /* File to hold new mail merge document */
           NO).                                                            /* Automatically Print New Mail Merge Document */
  
 MESSAGE  "Mail Merge Demo Completed" SKIP
          "Merge Document Is MailMergeDemo.doc" SKIP(1)
          VIEW-AS ALERT-BOX INFORMATION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MailMerge Dialog-Frame 
FUNCTION MailMerge RETURNS LOGICAL
  ( INPUT ip_cDocument          AS CHAR,
    INPUT ip_cDataFile     AS CHAR,
    INPUT ip_cMailMergeDoc AS CHAR,
    INPUT iplPrint        AS LOGICAL) :
 
 DEF VAR lv_chDocument  AS COM-HANDLE NO-UNDO.                                         /* Pointer to Document Object */
  
 pv_chWord:Documents:Close(NO) NO-ERROR.                                               /* Close all open documents */

 ASSIGN FILE-INFO:FILE-NAME = SEARCH(ip_cDocument)
        ip_cDocument              = FILE-INFO:FULL-PATHNAME
        FILE-INFO:FILE-NAME = SEARCH(ip_cDataFile)
        ip_cDataFile         = FILE-INFO:FULL-PATHNAME.

 ASSIGN lv_chDocument = pv_chWord:Documents:Open(ip_cDocument,NO,YES,YES,,,no,,) NO-ERROR.      /* Try & Open main Document */
 
 IF NOT VALID-HANDLE(lv_chDocument) THEN RETURN FALSE.                                 /* Bummer */
 
 lv_chDocument:MailMerge:OpenDataSource(ip_cDataFile) NO-ERROR.                         /* Try & Open Data Source */
 
 IF ERROR-STATUS:ERROR THEN                                                         /* Bummer */
 DO: 
  RELEASE OBJECT lv_chDocument.
  RETURN NO.
 END.

 ASSIGN lv_chDocument:MailMerge:Destination = 0.                                       /* Merge to New Document */
  
 lv_chDocument:MailMerge:Execute() NO-ERROR.                                           /* Perform the Merge */

 lv_chDocument:Close(NO).                                                              /* Close main document */
 
 RELEASE OBJECT lv_chDocument.                                                         /* Release document object */
 
 ASSIGN lv_chDocument = pv_chWord:Documents:Item(1) NO-ERROR.                             /* Get new mail merge document */
  
 IF NOT VALID-HANDLE(lv_chDocument) OR ERROR-STATUS:ERROR THEN RETURN NO.              /* Bummer */
  
 lv_chDocument:SaveAs(ip_cMailMergeDoc).                                                /* Save new mail merge document */
 
 IF iplPrint THEN lv_chDocument:PrintOut(YES).                                         /* print new document if required */

 RELEASE OBJECT lv_chDocument NO-ERROR.                                                /* Release document object */
 
 pv_chWord:Documents:Close(NO) NO-ERROR.                                               /* close all documents */

 RETURN NOT ERROR-STATUS:ERROR.                                                     /* success ! */
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

