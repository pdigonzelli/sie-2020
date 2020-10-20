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
DEFINE INPUT  PARAMETER pdFec AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER piLin AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER piTur AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER piSup AS INTEGER    NO-UNDO.
/* Local Variable Definitions ---                                       */

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
&Scoped-Define ENABLED-OBJECTS fiBza fiSAy fiSil fiDes fiMol fiSHy ~
Btn_Cancel Btn_OK RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS fiFec fiHra fiLin fiTur fiSup fiBza fiSAy ~
fiSil fiDes fiMol fiSHy fiTno fiLna 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 32 BY 3.57
     FONT 25.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Aceptar" 
     SIZE 32 BY 3.57
     FONT 25.

DEFINE VARIABLE fiBza AS INTEGER FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiDes AS INTEGER FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiFec AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiHra AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiLin AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiLna AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 37 BY 1.76
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiMol AS INTEGER FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiSAy AS INTEGER FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiSHy AS INTEGER FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.91
     BGCOLOR 12 FGCOLOR 15 FONT 25 NO-UNDO.

DEFINE VARIABLE fiSil AS INTEGER FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.91
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiSup AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1.91
     FGCOLOR 9 FONT 25.

DEFINE VARIABLE fiTno AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 6 BY 1.76
     FONT 25 NO-UNDO.

DEFINE VARIABLE fiTur AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1.91
     FONT 25 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 117 BY 11.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 117 BY 13.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiFec AT ROW 1.48 COL 34 COLON-ALIGNED NO-LABEL
     fiHra AT ROW 3.62 COL 34 COLON-ALIGNED NO-LABEL
     fiLin AT ROW 5.76 COL 34 COLON-ALIGNED NO-LABEL
     fiTur AT ROW 7.91 COL 34 COLON-ALIGNED NO-LABEL
     fiSup AT ROW 10.05 COL 34 COLON-ALIGNED NO-LABEL
     fiBza AT ROW 12.91 COL 67 COLON-ALIGNED NO-LABEL
     fiSAy AT ROW 15.05 COL 67 COLON-ALIGNED NO-LABEL
     fiSil AT ROW 17.19 COL 67 COLON-ALIGNED NO-LABEL
     fiDes AT ROW 19.33 COL 67 COLON-ALIGNED NO-LABEL
     fiMol AT ROW 21.48 COL 67 COLON-ALIGNED NO-LABEL
     fiSHy AT ROW 23.62 COL 67 COLON-ALIGNED NO-LABEL
     Btn_Cancel AT ROW 26.48 COL 51
     Btn_OK AT ROW 26.48 COL 86
     fiTno AT ROW 17.19 COL 47 COLON-ALIGNED NO-LABEL
     fiLna AT ROW 21.48 COL 29 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 12.67 COL 1
     "Ingreso por Balanza" VIEW-AS TEXT
          SIZE 64 BY 1.91 AT ROW 12.91 COL 3
          FONT 25
     "Descarte Packing" VIEW-AS TEXT
          SIZE 64 BY 1.91 AT ROW 19.33 COL 3
          FONT 25
     "Stock Silos" VIEW-AS TEXT
          SIZE 64 BY 1.91 AT ROW 23.62 COL 3
          FONT 25
     "Fecha:" VIEW-AS TEXT
          SIZE 31 BY 1.91 AT ROW 1.48 COL 3
          FONT 25
     "Hora:" VIEW-AS TEXT
          SIZE 32 BY 1.91 AT ROW 3.62 COL 3
          FONT 25
     "Turno:" VIEW-AS TEXT
          SIZE 32 BY 1.91 AT ROW 7.91 COL 3
          FONT 25
     "Supervisor:" VIEW-AS TEXT
          SIZE 33 BY 1.91 AT ROW 10.05 COL 3
          FONT 25
     "Molienda" VIEW-AS TEXT
          SIZE 27 BY 1.91 AT ROW 21.48 COL 3
          FONT 25
     "Stock en Silos Ayer" VIEW-AS TEXT
          SIZE 64 BY 1.91 AT ROW 15.05 COL 3
          FONT 25
     "Linea:" VIEW-AS TEXT
          SIZE 32 BY 1.91 AT ROW 5.76 COL 3
          FONT 25
     "Molienda Turno" VIEW-AS TEXT
          SIZE 46 BY 1.91 AT ROW 17.19 COL 3
          FONT 25
     SPACE(69.59) SKIP(11.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Resumen Info Turno"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

/* SETTINGS FOR FILL-IN fiFec IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHra IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLin IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLna IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSup IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTno IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTur IN FRAME gDialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME gDialog /* Resumen Info Turno */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
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
  DISPLAY fiFec fiHra fiLin fiTur fiSup fiBza fiSAy fiSil fiDes fiMol fiSHy 
          fiTno fiLna 
      WITH FRAME gDialog.
  ENABLE fiBza fiSAy fiSil fiDes fiMol fiSHy Btn_Cancel Btn_OK RECT-1 RECT-2 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cSil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE fSil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fDes AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fBza AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fSHy AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fMol AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fSAy AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libMolienda.p'). 
  DELETE OBJECT hLibCom.

  fSAy = DYNAMIC-FUNCTION('getStockSilos' IN hLib, pdFec - 1, piLin).
  cSil = DYNAMIC-FUNCTION('getMoliendaSilos' IN hLib, pdFec, piLin, piTur).
  fSil = 0.
  fDes = 0.
  DO i = 1 TO NUM-ENTRIES(cSil, CHR(10)):
    cRow = ENTRY(i, cSil, CHR(10)).
    fSil = fSil + DECIMAL(ENTRY(5, cRow, CHR(1))).
    fDes = fDes + DECIMAL(ENTRY(6, cRow, CHR(1))).
  END.

  i = IF piLin = 5 THEN 1 ELSE 6.

  fBza = DYNAMIC-FUNCTION('getIngresoBalanzaDia' IN hLib, pdFec, piLin, piTur).
  fMol = DYNAMIC-FUNCTION('getMoliendaDiaLinea' IN hLib, pdFec, piLin).
  fSHy = fBza + fSAy - fMol. 

  ASSIGN fiBza:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fBza)
         fiSil:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fSil)
         fiDes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fDes)
         fiMol:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fMol)
         fiSAy:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fSAy)
         fiSHy:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fSHy)
         fiSup:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2, DYNAMIC-FUNCTION('getDatosSupervisor' IN hLib, piSup), CHR(10))
         fiFec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pdFec)
         fiHra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, "HH:MM:SS")
         fiTur:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(piTur)
         fiTno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(piTur)
         fiLin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION('getDescripcionLinea' IN hLib, piLin)
         fiLna:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION('getDescripcionLinea' IN hLib, piLin)
         .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

