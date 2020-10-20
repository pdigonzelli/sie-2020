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
DEFINE INPUT  PARAMETER pcRows AS CHARACTER  NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iTbs AS INTEGER    NO-UNDO.
DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dGal AS DECIMAL    NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel RECT-33 
&Scoped-Define DISPLAYED-OBJECTS fiTbs fiLit fiGal fiKgs fiKgs400 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bttresumentambores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dttresumentambores AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiGal AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Gal" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiKgs AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Kilos" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiKgs400 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Kilos 400 GPL" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiLit AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Litros" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiTbs AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Tambores" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 4.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     Btn_OK AT ROW 14.57 COL 52
     Btn_Cancel AT ROW 14.57 COL 68
     fiTbs AT ROW 10.52 COL 81 RIGHT-ALIGNED
     fiLit AT ROW 11.29 COL 81 RIGHT-ALIGNED
     fiGal AT ROW 12 COL 81 RIGHT-ALIGNED
     fiKgs AT ROW 12.71 COL 81 RIGHT-ALIGNED
     fiKgs400 AT ROW 13.48 COL 81 RIGHT-ALIGNED
     RECT-33 AT ROW 10.29 COL 1
     SPACE(0.19) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Resumen Lotes"
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

/* SETTINGS FOR FILL-IN fiGal IN FRAME gDialog
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiKgs IN FRAME gDialog
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiKgs400 IN FRAME gDialog
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiLit IN FRAME gDialog
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fiTbs IN FRAME gDialog
   NO-ENABLE ALIGN-R                                                    */
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
ON WINDOW-CLOSE OF FRAME gDialog /* Resumen Lotes */
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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dttresumentambores.wDB-AWARE':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedttresumentamboresUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dttresumentambores ).
       RUN repositionObject IN h_dttresumentambores ( 10.52 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bttresumentambores.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bttresumentambores ).
       RUN repositionObject IN h_bttresumentambores ( 2.43 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bttresumentambores ( 7.62 , 83.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigation,Banda2SubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdate,PrintactionHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 83.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dttresumentambores. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dttresumentambores ).

       /* Links to SmartDataBrowser h_bttresumentambores. */
       RUN addLink ( h_dttresumentambores , 'Data':U , h_bttresumentambores ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             Btn_OK:HANDLE , 'BEFORE':U ).
       RUN adjustTabOrder ( h_bttresumentambores ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY fiTbs fiLit fiGal fiKgs fiKgs400 
      WITH FRAME gDialog.
  ENABLE Btn_OK Btn_Cancel RECT-33 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillResumen gDialog 
PROCEDURE fillResumen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO. 
  
  IF LENGTH(pcRows) > 0 THEN DO:
    DO i = 1 TO NUM-ENTRIES(pcRows, CHR(10)):
      cRow = ENTRY(i, pcRows, CHR(10)).

      RUN addttResumen IN h_dttResumenTambores (INTEGER(ENTRY(14, cRow, CHR(1))),
                                                INTEGER(ENTRY(15, cRow, CHR(1))),
                                                INTEGER(ENTRY(16, cRow, CHR(1))),
                                                INTEGER(ENTRY(17, cRow, CHR(1))),
                                                INTEGER(ENTRY(1, cRow, CHR(1))),
                                                INTEGER(ENTRY(2, cRow, CHR(1))),
                                                INTEGER(ENTRY(6, cRow, CHR(1))),
                                                INTEGER(ENTRY(7, cRow, CHR(1))),
                                                DECIMAL(ENTRY(8, cRow, CHR(1))),
                                                ENTRY(3, cRow, CHR(1)),
                                                ENTRY(4, cRow, CHR(1)),
                                                ENTRY(5, cRow, CHR(1)), 
                                                ENTRY(9, cRow, CHR(1)), 
                                                DECIMAL(ENTRY(10, cRow, CHR(1))),
                                                DECIMAL(ENTRY(11, cRow, CHR(1))),
                                                DECIMAL(ENTRY(12, cRow, CHR(1))),
                                                DECIMAL(ENTRY(13, cRow, CHR(1)))).
      iTbs = iTbs + INTEGER(ENTRY(6, cRow, CHR(1))).
      dKil = dKil + DECIMAL(ENTRY(7, cRow, CHR(1))).
      dKi4 = dKi4 + DECIMAL(ENTRY(10, cRow, CHR(1))).
      dLit = dLit + DECIMAL(ENTRY(8, cRow, CHR(1))).
      dGal = dGal + (dLit / 3.785).
    END.
    fiTbs:SCREEN-VALUE IN FRAME gDialog     = STRING(iTbs).
    fiLit:SCREEN-VALUE IN FRAME gDialog     = STRING(dLit).
    fiGal:SCREEN-VALUE IN FRAME gDialog     = STRING(dGal).
    fiKgs:SCREEN-VALUE IN FRAME gDialog     = STRING(dKil).
    fiKgs400:SCREEN-VALUE IN FRAME gDialog  = STRING(dKi4).
    DYNAMIC-FUNCTION('openQuery' IN h_dttResumenTambores).
  END.

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


  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "excelAction").  
  SUBSCRIBE TO "tlbExcel" IN h_dyntoolbar.

  RUN fillResumen.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbExcel gDialog 
PROCEDURE tlbExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE VARIABLE cFld AS CHARACTER  NO-UNDO.

  cFld = "id_lote,anio,cantidad,kilos,articulo,calidad,envase,ubicacion,kilos,litros,kilos400".
  cFld = "".
  RUN transferToExcel IN h_dttResumenTambores(cFld, TRUE, TRUE, 100). 


END PROCEDURE.



/*
  DEFINE VARIABLE cTable             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFields            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hQuery             AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBuffer            AS HANDLE     NO-UNDO.
DEFINE VARIABLE hField             AS HANDLE     NO-UNDO.
DEFINE VARIABLE iFields            AS INTEGER    NO-UNDO.
DEFINE VARIABLE iRow               AS INTEGER    NO-UNDO.
DEFINE VARIABLE cRow               AS CHARACTER  NO-UNDO INITIAL "A".
DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet        AS COM-HANDLE NO-UNDO.

CREATE "Excel.Application" chExcelApplication.

ASSIGN chExcelApplication:Visible = TRUE
       chWorkbook                 = chExcelApplication:Workbooks:Add()
       chWorkSheet                = chExcelApplication:Sheets:Item(1)
       cTable                     = "Employee"
       cFields                    = "EmpNum,FirstName,LastName,WorkPhone".

CREATE QUERY hQuery.

CREATE BUFFER hBuffer FOR TABLE cTable.

hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE(DYNAMIC-FUNCTION('getQueryWhere' IN h_demployee)).
hQuery:QUERY-OPEN().

IF hQuery:GET-FIRST() = FALSE THEN 
    DO:
        hQuery:QUERY-CLOSE().
        chExcelApplication:QUIT().
        RELEASE OBJECT chWorkSheet        NO-ERROR.
        RELEASE OBJECT chWorkBook         NO-ERROR.
        RELEASE OBJECT chExcelApplication NO-ERROR.
        DELETE  OBJECT hBuffer            NO-ERROR.
        DELETE  OBJECT hQuery             NO-ERROR.
        RETURN.
    END.

REPEAT:
    ASSIGN iRow = iRow + 1.
    DO iFields = 1 TO NUM-ENTRIES(cFields):
        hField = hBuffer:BUFFER-FIELD(ENTRY(iFields,cFields)).
        chWorkSheet:range((CHR(ASC(cRow) + iFields - 1)) + STRING(iRow)):Value = hField:BUFFER-VALUE.
    END.
    IF hQuery:GET-NEXT() =FALSE THEN 
        LEAVE.
END.

hQuery:QUERY-CLOSE().

DELETE OBJECT hField  NO-ERROR.
DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hQuery  NO-ERROR.

RELEASE OBJECT chExcelApplication NO-ERROR.
RELEASE OBJECT chWorkBook         NO-ERROR.
RELEASE OBJECT chWorkSheet        NO-ERROR.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

