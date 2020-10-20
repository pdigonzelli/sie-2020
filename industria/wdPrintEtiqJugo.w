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
DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.


/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS optEtiquetas optRango Btn_OK Btn_Cancel ~
RECT-27 RECT-28 RECT-29 
&Scoped-Define DISPLAYED-OBJECTS optEtiquetas optRango fiDesde fiHasta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiDesde AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiHasta AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE optEtiquetas AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Etq. Standard", 1,
"Etq. c/Bisulfito y c/Benzoato", 2,
"Etq. Solo Bisulfito", 4,
"Etq. Sin Identificacion (Mariani)", 5,
"Etq. Pulpa w/w 20 Mesh", 6,
"Etq. Pulpa Aceptica", 7,
"Etq. Pepsi", 8,
"Etq. Solo Benzoato", 9,
"Etq. Pulpa Gr/quart", 10
     SIZE 39 BY 10.48 NO-UNDO.

DEFINE VARIABLE optRango AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos los Tambores", 1,
"Rango de Tambores", 2
     SIZE 31 BY 3.57 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 10.95.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34 BY 4.76.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     optEtiquetas AT ROW 1.24 COL 2 NO-LABEL
     optRango AT ROW 1.48 COL 44 NO-LABEL
     fiDesde AT ROW 6.48 COL 58 COLON-ALIGNED
     fiHasta AT ROW 8.33 COL 58 COLON-ALIGNED
     Btn_OK AT ROW 12.19 COL 44.8
     Btn_Cancel AT ROW 12.19 COL 60.8
     RECT-27 AT ROW 1 COL 1
     RECT-28 AT ROW 1 COL 42
     RECT-29 AT ROW 5.76 COL 42
     SPACE(0.19) SKIP(1.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Impresion Etiquetas"
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

/* SETTINGS FOR FILL-IN fiDesde IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHasta IN FRAME gDialog
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
ON WINDOW-CLOSE OF FRAME gDialog /* Impresion Etiquetas */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
  DEFINE VARIABLE cProgram AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iDesde   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHasta   AS INTEGER    NO-UNDO.

  iDesde = INTEGER(fiDesde:SCREEN-VALUE IN FRAME gDialog).
  iHasta = INTEGER(fiHasta:SCREEN-VALUE IN FRAME gDialog).


  CASE optEtiquetas:SCREEN-VALUE IN FRAME gDialog:
    WHEN "1" THEN DO: /*standard*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqInspeccionesLote IN hLib (piEmp, piSuc, piTip, piNro, "Not Applicable", TRUE).
        WHEN "2" THEN RUN etqInspeccionesLoteRango IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "Not Applicable", TRUE).
      END CASE.
    END.

    WHEN "2" THEN DO: /*con aditivos*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqInspeccionesLote IN hLib (piEmp, piSuc, piTip, piNro, "Sodium Benzoate and Sodium Metabisulphite", TRUE).
        WHEN "2" THEN RUN etqInspeccionesLoteRango IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "Sodium Benzoate and Sodium Metabisulphite", TRUE).
      END CASE.
    END.

    WHEN "3" THEN DO: /*sin aditivos*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqInspeccionesLote IN hLib (piEmp, piSuc, piTip, piNro, "Not Applicable", TRUE).
        WHEN "2" THEN RUN etqInspeccionesLoteRango IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "Not Applicable", TRUE).
      END CASE.
    END.

    WHEN "4" THEN DO: /*solo bisulfito*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqInspeccionesLote IN hLib (piEmp, piSuc, piTip, piNro, "Sodium Metabisulphite", TRUE).
        WHEN "2" THEN RUN etqInspeccionesLoteRango IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "Sodium Metabisulphite", TRUE).
      END CASE.
    END.

    WHEN "5" THEN DO: /*sin nombre*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqInspeccionesLote IN hLib (piEmp, piSuc, piTip, piNro, "Not Applicable", FALSE).
        WHEN "2" THEN RUN etqInspeccionesLoteRango IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "Not Applicable", FALSE).
      END CASE.
    END.

    WHEN "6" THEN DO: /*pulpa*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqPulpa IN hLib (piEmp, piSuc, piTip, piNro, "-05/-10").
        WHEN "2" THEN RUN etqRangoPulpa IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "-05/-10").
      END CASE.
    END.

    WHEN "7" THEN DO: /*pulpa asceptica*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqPulpa IN hLib (piEmp, piSuc, piTip, piNro, "-18/-20").
        WHEN "2" THEN RUN etqRangoPulpa IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "-18/-20").
      END CASE.
    END.

    WHEN "8" THEN DO: /*pepsi preimpresa*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqPepsi IN hLib (piEmp, piSuc, piTip, piNro).
        /*WHEN "2" THEN RUN etqRangoPulpa IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "-18/-20").*/
      END CASE.
    END.

     WHEN "9" THEN DO: /*solo bisulfito*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqInspeccionesLote IN hLib (piEmp, piSuc, piTip, piNro, "Sodium Benzoate", TRUE).
        WHEN "2" THEN RUN etqInspeccionesLoteRango IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "Sodium Benzoate", TRUE).
      END CASE.
    END.

    WHEN "10" THEN DO: /*sin razon social*/
      CASE optRango:SCREEN-VALUE IN FRAME gDialog:
        WHEN "1" THEN RUN etqSinLogo IN hLib (piEmp, piSuc, piTip, piNro, "", FALSE).
        WHEN "2" THEN RUN etqRangoSinLogo IN hLib (piEmp, piSuc, piTip, piNro, iDesde, iHasta, "", FALSE).
      END CASE.
    END.


  END CASE.
  
 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME optRango
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL optRango gDialog
ON VALUE-CHANGED OF optRango IN FRAME gDialog
DO:
  RUN customEnableFields(optRango:SCREEN-VALUE).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customEnableFields gDialog 
PROCEDURE customEnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcOption AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE lVal AS LOGICAL    NO-UNDO.

  CASE pcOption:
    WHEN "1" THEN lVal = FALSE.
    WHEN "2" THEN lVal = TRUE.
  END CASE.

  fiDesde:SENSITIVE IN FRAME gDialog = lVal.
  fiHasta:SENSITIVE IN FRAME gDialog = lVal.


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
  DISPLAY optEtiquetas optRango fiDesde fiHasta 
      WITH FRAME gDialog.
  ENABLE optEtiquetas optRango Btn_OK Btn_Cancel RECT-27 RECT-28 RECT-29 
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

  RUN libImpresionEtiquetas.p PERSISTENT SET hLib.

  fiDesde:SENSITIVE IN FRAME gDialog = FALSE.
  fiHasta:SENSITIVE IN FRAME gDialog = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

