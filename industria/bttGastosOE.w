&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS bTableWin 
/*------------------------------------------------------------------------

  File: adm2\src\browser.w

  Description: SmartDataBrowser Object

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

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dttGastosOE.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.id_gasto rowObject.gasto ~
rowObject.importe rowObject.id_orden_entrega rowObject.nro_regla ~
rowObject.antecedente rowObject.consecuente 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table rowObject
&Scoped-define FIRST-TABLE-IN-QUERY-br_table rowObject


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE TEMP-TABLE RowObject
    {{&DATA-FIELD-DEFS}}
    {src/adm2/robjflds.i}.

DEFINE QUERY br_table FOR 
      rowObject SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table bTableWin _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      rowObject.id_gasto FORMAT ">,>>>,>>9":U
      rowObject.gasto FORMAT "X(30)":U
      rowObject.importe FORMAT ">>>,>>>,>>9.99":U
      rowObject.id_orden_entrega FORMAT ">,>>>,>>9":U
      rowObject.nro_regla FORMAT "X(8)":U
      rowObject.antecedente FORMAT "X(100)":U
      rowObject.consecuente FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS SIZE 66 BY 6.67 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataBrowser
   Data Source: "dttGastosOE.w"
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
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
  CREATE WINDOW bTableWin ASSIGN
         HEIGHT             = 6.86
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB bTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW bTableWin
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = _<SDO>.rowObject.id_gasto
     _FldNameList[2]   = _<SDO>.rowObject.gasto
     _FldNameList[3]   = _<SDO>.rowObject.importe
     _FldNameList[4]   = _<SDO>.rowObject.id_orden_entrega
     _FldNameList[5]   = _<SDO>.rowObject.nro_regla
     _FldNameList[6]   = _<SDO>.rowObject.antecedente
     _FldNameList[7]   = _<SDO>.rowObject.consecuente
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON CTRL-END OF br_table IN FRAME F-Main
DO:
  APPLY "END":U TO BROWSE br_table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON CTRL-HOME OF br_table IN FRAME F-Main
DO:
  APPLY "HOME":U TO BROWSE br_table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON END OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON HOME OF br_table IN FRAME F-Main
DO:
  {src/adm2/brshome.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON OFF-END OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsoffnd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON OFF-HOME OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsoffhm.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
  DEFINE VARIABLE hCont    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hdOE     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cRows    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iBgc     AS INTEGER    NO-UNDO.

  {get ContainerSource hCont}.

  /*para pantalla de control gastos oe*/
  IF VALID-HANDLE(hCont) AND (hCont:FILE-NAME MATCHES "*wGastosOE.w") THEN DO:
    hdOE = DYNAMIC-FUNCTION('getSdoGastosOE' IN hCont).


    IF rowObject.importe = DYNAMIC-FUNCTION('getImporteGasto' IN hdOE, rowObject.id_gasto) THEN 
      iBgc = 23.
    ELSE 
      iBgc = 20.

    IF rowObject.importe = 0 THEN
      iBgc = 15.

    ASSIGN rowObject.id_gasto:BGCOLOR IN BROWSE br_table         = iBgc
           rowObject.gasto:BGCOLOR IN BROWSE br_table            = iBgc
           rowObject.importe:BGCOLOR IN BROWSE br_table          = iBgc
           rowObject.id_orden_entrega:BGCOLOR IN BROWSE br_table = iBgc
           rowObject.nro_regla:BGCOLOR IN BROWSE br_table        = iBgc
           rowObject.antecedente:BGCOLOR IN BROWSE br_table      = iBgc
           rowObject.consecuente:BGCOLOR IN BROWSE br_table      = iBgc.
    

  END.

END.


/*


  iBgColor = IF rowObject.rowNum MODULO 2 = 0 THEN 19 /*11 light blue*/ ELSE ? /*default*/.

  iBgColor = 19.

  IF rowObject.importe > 0 THEN DO:
    ASSIGN rowObject.id_gasto:BGCOLOR IN BROWSE br_table         = ibgcolor
           rowObject.gasto:BGCOLOR IN BROWSE br_table            = ibgcolor
           rowObject.importe:BGCOLOR IN BROWSE br_table          = ibgcolor
           rowObject.id_orden_entrega:BGCOLOR IN BROWSE br_table = ibgcolor
           rowObject.nro_regla:BGCOLOR IN BROWSE br_table        = ibgcolor
           rowObject.antecedente:BGCOLOR IN BROWSE br_table      = ibgcolor
           rowObject.consecuente:BGCOLOR IN BROWSE br_table      = ibgcolor.

  END.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON SCROLL-NOTIFY OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsscrol.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
  DEFINE VAR hColumn AS HANDLE NO-UNDO.
  hColumn = SELF:CURRENT-COLUMN.
  RUN createSearchField (hColumn:NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  {src/adm2/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK bTableWin 


/* ***************************  Main Block  *************************** */
{adm2/support/brwTrg.i}.
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN initializeObject.        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI bTableWin  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repaintRows bTableWin 
PROCEDURE repaintRows :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcRows AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iRow  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iGas  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iBgc  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNum  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRow  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dIm1  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dIm2  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE hDat  AS HANDLE     NO-UNDO.
  
  
  hDat = DYNAMIC-FUNCTION('getDataSource').
  

  DO i = 1 TO NUM-ENTRIES(pcRows, CHR(10)):
    cRow = ENTRY(i, pcRows, CHR(10)).
    iRow = INTEGER(ENTRY(1, cRow, CHR(1))).
    iGas = INTEGER(ENTRY(2, cRow, CHR(1))).
    dIm1 = DECIMAL(ENTRY(3, cRow, CHR(1))).
    dIm2 = DYNAMIC-FUNCTION('getImporteGasto' IN hDat, iGas).

    iBgc = 15.
    IF dIm1 = dIm2 THEN
      iBgc = 23.
    ELSE 
      iBgc = 20.

    iNum = INTEGER(DYNAMIC-FUNCTION('getRowNumGasto' IN hDat, iGas)).
    IF iNum <> 0 THEN DO:   
  
      ASSIGN rowObject.id_gasto:BGCOLOR IN BROWSE br_table         = iBgc
             rowObject.gasto:BGCOLOR IN BROWSE br_table            = iBgc
             rowObject.importe:BGCOLOR IN BROWSE br_table          = iBgc
             rowObject.id_orden_entrega:BGCOLOR IN BROWSE br_table = iBgc
             rowObject.nro_regla:BGCOLOR IN BROWSE br_table        = iBgc
             rowObject.antecedente:BGCOLOR IN BROWSE br_table      = iBgc
             rowObject.consecuente:BGCOLOR IN BROWSE br_table      = iBgc.
    END.
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

