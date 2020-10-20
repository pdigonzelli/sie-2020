&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
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
&Scoped-define DATA-FIELD-DEFS "dLotesJugo.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.Sucursal ~
rowObject.id_lote rowObject.anio rowObject.Tambores rowObject.Articulo ~
rowObject.Calidad rowObject.Pulpa rowObject.ControlCalidad ~
rowObject.ControlMicro rowObject.pesticida rowObject.sensorial ~
rowObject.Quimico rowObject.QuimicoMicro rowObject.Fecha ~
rowObject.id_articulo rowObject.nromov rowObject.codigo_lote ~
rowObject.Kilos rowObject.Galones rowObject.Kilos400 rowObject.c_usuario 
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
      rowObject.Sucursal FORMAT "x(15)":U WIDTH 10
      rowObject.id_lote FORMAT "->,>>>,>>9":U
      rowObject.anio FORMAT ">>>9":U
      rowObject.Tambores FORMAT ">>>>9":U
      rowObject.Articulo FORMAT "x(15)":U
      rowObject.Calidad FORMAT "x(15)":U
      rowObject.Pulpa FORMAT "->>,>>9.99":U
      rowObject.ControlCalidad FORMAT "x(20)":U
      rowObject.ControlMicro FORMAT "x(20)":U
      rowObject.pesticida FORMAT "Aprobado/Desaprobado":U
      rowObject.sensorial FORMAT "Aprobado/Desaprobado":U
      rowObject.Quimico FORMAT "x(20)":U
      rowObject.QuimicoMicro FORMAT "x(20)":U
      rowObject.Fecha FORMAT "99/99/9999":U
      rowObject.id_articulo FORMAT "->,>>>,>>9":U
      rowObject.nromov FORMAT ">>>,>>>,>>9":U
      rowObject.codigo_lote FORMAT "X(50)":U
      rowObject.Kilos FORMAT ">>>,>>9.99":U
      rowObject.Galones FORMAT ">>>,>>>,>>9.999999":U
      rowObject.Kilos400 FORMAT ">>>,>>>,>>9.99":U
      rowObject.c_usuario FORMAT "X(20)":U
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
   Data Source: "dLotesJugo.w"
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
     _FldNameList[1]   > _<SDO>.rowObject.Sucursal
"rowObject.Sucursal" ? ? "character" ? ? ? ? ? ? no "?" no no "10" yes no no "U" "" ""
     _FldNameList[2]   = _<SDO>.rowObject.id_lote
     _FldNameList[3]   = _<SDO>.rowObject.anio
     _FldNameList[4]   = _<SDO>.rowObject.Tambores
     _FldNameList[5]   > _<SDO>.rowObject.Articulo
"rowObject.Articulo" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[6]   > _<SDO>.rowObject.Calidad
"rowObject.Calidad" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[7]   > _<SDO>.rowObject.Pulpa
"rowObject.Pulpa" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[8]   > _<SDO>.rowObject.ControlCalidad
"rowObject.ControlCalidad" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[9]   > _<SDO>.rowObject.ControlMicro
"rowObject.ControlMicro" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[10]   > _<SDO>.rowObject.pesticida
"rowObject.pesticida" ? ? "logical" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[11]   > _<SDO>.rowObject.sensorial
"rowObject.sensorial" ? ? "logical" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[12]   > _<SDO>.rowObject.Quimico
"rowObject.Quimico" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[13]   > _<SDO>.rowObject.QuimicoMicro
"rowObject.QuimicoMicro" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[14]   = _<SDO>.rowObject.Fecha
     _FldNameList[15]   > _<SDO>.rowObject.id_articulo
"rowObject.id_articulo" ? ? "integer" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[16]   = _<SDO>.rowObject.nromov
     _FldNameList[17]   = _<SDO>.rowObject.codigo_lote
     _FldNameList[18]   = _<SDO>.rowObject.Kilos
     _FldNameList[19]   = _<SDO>.rowObject.Galones
     _FldNameList[20]   > _<SDO>.rowObject.Kilos400
"rowObject.Kilos400" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[21]   = _<SDO>.rowObject.c_usuario
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
ON MOUSE-SELECT-CLICK OF br_table IN FRAME F-Main
DO:
  RUN onSelectRow.

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
  DEFINE VARIABLE ibgcolor AS INTEGER    NO-UNDO.  

  {get ContainerSource hCont}.
  
  ibgcolor = ?.
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wControlCalidadJugo.w" THEN DO:
    IF (rowObject.CONTROL_calidad AND rowObject.microbiologia AND rowObject.pesticida) THEN
      ibgcolor = 23.
    ELSE
      ibgcolor = ?.
  END.

 
  
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wMatPrimaJugo.w" THEN DO:
    iBgColor = IF rowObject.rowNum MODULO 2 = 0 THEN 19 /*11 light yellow*/ ELSE ? /*default*/.
  END.

  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wAjustesCierre.w" THEN DO:
    iBgColor = IF rowObject.fecha_finalizacion <> DATE('') THEN 23  ELSE ? /*default*/.
  END.

  ASSIGN rowObject.sucursal:BGCOLOR IN BROWSE br_table       = ibgcolor
         rowObject.id_lote:BGCOLOR IN BROWSE br_table        = ibgcolor
         rowObject.anio:BGCOLOR IN BROWSE br_table           = ibgcolor
         rowObject.tambores:BGCOLOR IN BROWSE br_table       = 23
         rowObject.articulo:BGCOLOR IN BROWSE br_table       = ibgcolor
         rowObject.calidad:BGCOLOR IN BROWSE br_table        = ibgcolor 
         rowObject.pulpa:BGCOLOR IN BROWSE br_table          = ibgcolor
         rowObject.controlcalidad:BGCOLOR IN BROWSE br_table = ibgcolor
         rowObject.quimico:BGCOLOR IN BROWSE br_table        = ibgcolor
         rowObject.fecha:BGCOLOR IN BROWSE br_table          = ibgcolor
         rowObject.id_articulo:BGCOLOR IN BROWSE br_table    = ibgcolor
         rowObject.nromov:BGCOLOR IN BROWSE br_table         = ibgcolor
         rowObject.controlmicro:BGCOLOR IN BROWSE br_table   = ibgcolor
         rowObject.quimicomicro:BGCOLOR IN BROWSE br_table   = ibgcolor
         rowObject.c_usuario:BGCOLOR IN BROWSE br_table      = ibgcolor
         rowObject.galones:BGCOLOR IN BROWSE br_table        = 19.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onSelectRow bTableWin 
PROCEDURE onSelectRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hCont AS HANDLE     NO-UNDO.

  {get ContainerSource hCont}.
  
  /*jugo*/
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wCtrlJugo.w" THEN DO:
    IF rowObject.controlcalidad:SCREEN-VALUE IN BROWSE br_table <> "Aprobado" THEN
      RUN enableButtons IN hCont (TRUE).
    ELSE 
      RUN enableButtons IN hCont (FALSE).
  END.

  /*micro*/
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wCtrlMicro.w" THEN DO:
    IF rowObject.controlmicro:SCREEN-VALUE IN BROWSE br_table MATCHES "Aprobado*" THEN
      RUN enableButtons IN hCont (FALSE).
    ELSE 
      RUN enableButtons IN hCont (TRUE).
  END.

  /*lotes jugo*/
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wLotesJugo.w" THEN DO:
    RUN loadOrigenes IN hCont.
  END.

  /*datos para mail a deposito*/
  IF VALID-HANDLE(hCont) AND hCont:FILE-NAME MATCHES "*wLotesJugo.w" THEN DO:
    RUN datosMailDeposito IN hCont.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

