&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
          general         PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
&Scoped-define DATA-FIELD-DEFS "dItemsOrdenEntrega.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.id_orden_entrega ~
rowObject.item_oe rowObject.semana_entrega rowObject.cantidad_tambores ~
rowObject.id_contrato rowObject.item rowObject.Producto rowObject.condVta ~
rowObject.PrecioVenta rowObject.Moneda rowObject.Unidad rowObject.cliente ~
rowObject.destino rowObject.Estado rowObject.id_estado 
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
      rowObject.id_orden_entrega FORMAT ">>>>>9":U
      rowObject.item_oe FORMAT ">>>9":U
      rowObject.semana_entrega FORMAT ">>9":U
      rowObject.cantidad_tambores FORMAT ">>>>>9":U
      rowObject.id_contrato FORMAT "X(12)":U
      rowObject.item FORMAT ">>>>>9":U
      rowObject.Producto FORMAT "x(50)":U
      rowObject.condVta FORMAT "x(15)":U
      rowObject.PrecioVenta FORMAT ">>>,>>>,>>9.99":U
      rowObject.Moneda FORMAT "x(20)":U
      rowObject.Unidad FORMAT "x(8)":U
      rowObject.cliente FORMAT "x(50)":U
      rowObject.destino FORMAT "x(50)":U
      rowObject.Estado FORMAT "x(20)":U
      rowObject.id_estado FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 66 BY 6.67 EXPANDABLE.


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
   Data Source: "dItemsOrdenEntrega.w"
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: Neither
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
     _FldNameList[1]   > _<SDO>.rowObject.id_orden_entrega
"rowObject.id_orden_entrega" ? ? "integer" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[2]   = _<SDO>.rowObject.item_oe
     _FldNameList[3]   = _<SDO>.rowObject.semana_entrega
     _FldNameList[4]   = _<SDO>.rowObject.cantidad_tambores
     _FldNameList[5]   = _<SDO>.rowObject.id_contrato
     _FldNameList[6]   = _<SDO>.rowObject.item
     _FldNameList[7]   = _<SDO>.rowObject.Producto
     _FldNameList[8]   = _<SDO>.rowObject.condVta
     _FldNameList[9]   = _<SDO>.rowObject.PrecioVenta
     _FldNameList[10]   = _<SDO>.rowObject.Moneda
     _FldNameList[11]   = _<SDO>.rowObject.Unidad
     _FldNameList[12]   = _<SDO>.rowObject.cliente
     _FldNameList[13]   = _<SDO>.rowObject.destino
     _FldNameList[14]   = _<SDO>.rowObject.Estado
     _FldNameList[15]   = _<SDO>.rowObject.id_estado
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
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.
  DEFINE VARIABLE ibgcolor AS INTEGER    NO-UNDO.

  hCont = DYNAMIC-FUNCTION('getContainerSource').

  IF VALID-HANDLE(hCont) THEN DO:
    CASE rowObject.id_estado:
      WHEN 1 THEN
        ibgcolor = 22.
      WHEN 2 THEN
        ibgcolor = 25.
      WHEN 3 THEN
        ibgcolor = 20.
      WHEN 4 THEN
        ibgcolor = 23.
      WHEN 5 THEN
        ibgcolor = 24.
      WHEN 6 THEN
        ibgcolor = 12.
      
    END CASE.
    
    ASSIGN rowObject.id_orden_entrega:BGCOLOR IN BROWSE br_table  = ibgcolor
           rowObject.ITEM_oe:BGCOLOR IN BROWSE br_table           = ibgcolor
           rowObject.semana_entrega:BGCOLOR IN BROWSE br_table    = ibgcolor
           rowObject.cantidad_tambores:BGCOLOR IN BROWSE br_table = ibgcolor
           rowObject.id_contrato:BGCOLOR IN BROWSE br_table       = ibgcolor
           rowObject.ITEM:BGCOLOR IN BROWSE br_table              = ibgcolor
           rowObject.producto:BGCOLOR IN BROWSE br_table          = ibgcolor
           rowObject.condVta:BGCOLOR IN BROWSE br_table           = ibgcolor
           rowObject.precioVenta:BGCOLOR IN BROWSE br_table       = ibgcolor
           rowObject.moneda:BGCOLOR IN BROWSE br_table            = ibgcolor
           rowObject.unidad:BGCOLOR IN BROWSE br_table            = ibgcolor
           rowObject.cliente:BGCOLOR IN BROWSE br_table           = ibgcolor
           rowObject.destino:BGCOLOR IN BROWSE br_table           = ibgcolor
           rowObject.estado:BGCOLOR IN BROWSE br_table            = ibgcolor
           rowObject.id_estado:BGCOLOR IN BROWSE br_table         = ibgcolor
           .
  END. 
END.

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
  PUBLISH 'oeChanged'.
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

