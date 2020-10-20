&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEFINE INPUT  PARAMETER piTurno AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER pdFecha AS DATE       NO-UNDO.
DEFINE OUTPUT PARAMETER pcCod   AS CHARACTER  NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iSup AS INTEGER    NO-UNDO.
DEFINE VARIABLE cNom AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCod AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCodigo btnNum-7 btnNum-8 btnNum-9 btnNum-4 ~
btnNum-5 btnNum-6 btnNum-1 btnNum-2 btnNum-3 btnCorregir btnNum-0 btnEnter ~
Btn_Cancel-2 Btn_OK-2 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS fiNombre fiCodigo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCorregir 
     LABEL "Supr" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnEnter 
     LABEL "Enter" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-0 
     LABEL "0" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-1 
     LABEL "1" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-2 
     LABEL "2" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-3 
     LABEL "3" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-4 
     LABEL "4" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-5 
     LABEL "5" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-6 
     LABEL "6" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-7 
     LABEL "7" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-8 
     LABEL "8" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON btnNum-9 
     LABEL "9" 
     SIZE 16 BY 3.57
     FONT 25.

DEFINE BUTTON Btn_Cancel-2 AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 32 BY 3.57
     FONT 25.

DEFINE BUTTON Btn_OK-2 AUTO-GO 
     LABEL "Aceptar" 
     SIZE 32 BY 3.57
     FONT 25.

DEFINE VARIABLE fiCodigo AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY 2.05
     FGCOLOR 12 FONT 25 NO-UNDO.

DEFINE VARIABLE fiNombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1.91
     FGCOLOR 9 FONT 25 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 16.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiNombre AT ROW 1.24 COL 2 NO-LABEL
     fiCodigo AT ROW 3.95 COL 2 NO-LABEL
     btnNum-7 AT ROW 6.95 COL 7
     btnNum-8 AT ROW 6.95 COL 28
     btnNum-9 AT ROW 6.95 COL 49
     btnNum-4 AT ROW 11.24 COL 7
     btnNum-5 AT ROW 11.24 COL 28
     btnNum-6 AT ROW 11.24 COL 49
     btnNum-1 AT ROW 15.52 COL 7
     btnNum-2 AT ROW 15.52 COL 28
     btnNum-3 AT ROW 15.52 COL 49
     btnCorregir AT ROW 19.81 COL 7
     btnNum-0 AT ROW 19.81 COL 28
     btnEnter AT ROW 19.81 COL 49
     Btn_Cancel-2 AT ROW 23.86 COL 2
     Btn_OK-2 AT ROW 23.86 COL 39
     RECT-4 AT ROW 6.71 COL 2
     SPACE(0.99) SKIP(4.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Codigo de Autorizacion".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
                                                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiCodigo IN FRAME gDialog
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiNombre IN FRAME gDialog
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Codigo de Autorizacion */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCorregir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCorregir gDialog
ON CHOOSE OF btnCorregir IN FRAME gDialog /* Supr */
DO:
  cCod = SUBSTRING(cCod, 1, LENGTH(cCod) - 1).
  fiCodigo:SCREEN-VALUE = SUBSTRING(fiCodigo:SCREEN-VALUE, 1, LENGTH(fiCodigo:SCREEN-VALUE) - 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEnter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEnter gDialog
ON CHOOSE OF btnEnter IN FRAME gDialog /* Enter */
DO:
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cDat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lAux AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.

  cDat = DYNAMIC-FUNCTION('getDatosSupervisor' IN hLib, cCod).
  IF cDat = "Codigo Incorrecto" THEN RETURN.
  iSup = INTEGER(ENTRY(1, cDat, CHR(10))).
  cNom = ENTRY(2, cDat, CHR(10)).
  
  fiNombre:SCREEN-VALUE = cNom.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-0 gDialog
ON CHOOSE OF btnNum-0 IN FRAME gDialog /* 0 */
DO:
  RUN putNumber(0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-1 gDialog
ON CHOOSE OF btnNum-1 IN FRAME gDialog /* 1 */
DO:
  RUN putNumber(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-2 gDialog
ON CHOOSE OF btnNum-2 IN FRAME gDialog /* 2 */
DO:
  RUN putNumber(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-3 gDialog
ON CHOOSE OF btnNum-3 IN FRAME gDialog /* 3 */
DO:
  RUN putNumber(3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-4 gDialog
ON CHOOSE OF btnNum-4 IN FRAME gDialog /* 4 */
DO:
  RUN putNumber(4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-5 gDialog
ON CHOOSE OF btnNum-5 IN FRAME gDialog /* 5 */
DO:
  RUN putNumber(5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-6 gDialog
ON CHOOSE OF btnNum-6 IN FRAME gDialog /* 6 */
DO:
  RUN putNumber(6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-7 gDialog
ON CHOOSE OF btnNum-7 IN FRAME gDialog /* 7 */
DO:
  RUN putNumber(7).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-8 gDialog
ON CHOOSE OF btnNum-8 IN FRAME gDialog /* 8 */
DO:
  RUN putNumber(8).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNum-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNum-9 gDialog
ON CHOOSE OF btnNum-9 IN FRAME gDialog /* 9 */
DO:
  RUN putNumber(9).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel-2 gDialog
ON CHOOSE OF Btn_Cancel-2 IN FRAME gDialog /* Cancelar */
DO:
  pcCod = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK-2 gDialog
ON CHOOSE OF Btn_OK-2 IN FRAME gDialog /* Aceptar */
DO:
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lOk  AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.


  IF fiNombre:SCREEN-VALUE = "Codigo Incorrecto" OR fiNombre:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Presione Enter Para validar su Codigo antes de 'Aceptar'." 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  pcCod = cCod.
  lOk = DYNAMIC-FUNCTION('validarSupervisorTurno' IN hLib, piTurno, iSup, pdFecha).
  IF NOT lOk THEN DO:
    MESSAGE cNom + " No esta autorizado para modificar datos en este turno"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    pcCod = "".
  END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY fiNombre fiCodigo 
      WITH FRAME gDialog.
  ENABLE fiCodigo btnNum-7 btnNum-8 btnNum-9 btnNum-4 btnNum-5 btnNum-6 
         btnNum-1 btnNum-2 btnNum-3 btnCorregir btnNum-0 btnEnter Btn_Cancel-2 
         Btn_OK-2 RECT-4 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putNumber gDialog 
PROCEDURE putNumber :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piNum AS INTEGER    NO-UNDO.

  cCod = cCod + STRING(piNum).
  fiCodigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fiCodigo:SCREEN-VALUE + "*".
  BELL.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

