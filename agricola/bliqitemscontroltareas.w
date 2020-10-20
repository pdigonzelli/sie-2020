&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          agricola         PROGRESS
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

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dliqitemscontroltareas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table legajo nombre total_horas id_tarea ~
abreviatura id_lote desc-lote tipo_turno cant_jornal_norm cant_hs_norm ~
cant_hs_compensa cant_hs_extras cantidad id_unidad_liquidacion hora_inicio ~
hora_fin hora_inicio-1 hora_fin-1 
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
DEFINE TEMP-TABLE RowObject NO-UNDO
    {{&DATA-FIELD-DEFS}}
    {src/adm2/robjflds.i}.

DEFINE QUERY br_table FOR 
      rowObject SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table bTableWin _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      legajo FORMAT ">>>>>>>>9":U WIDTH 15.2
      nombre FORMAT "x(30)":U
      total_horas FORMAT "->>,>>9.99":U WIDTH 7.6
      id_tarea FORMAT ">>>,>>9":U
      abreviatura FORMAT "X(12)":U
      id_lote COLUMN-LABEL "Cod" FORMAT ">>>9":U
      desc-lote FORMAT "x(15)":U
      tipo_turno FORMAT "D/N":U
      cant_jornal_norm FORMAT "->>,>>9.99":U
      cant_hs_norm FORMAT "->>,>>9.99":U
      cant_hs_compensa FORMAT "->>,>>9.99":U
      cant_hs_extras FORMAT "->>,>>9.99":U
      cantidad FORMAT "->>,>>9.99":U
      id_unidad_liquidacion FORMAT ">>>,>>9":U
      hora_inicio FORMAT "x(12)":U
      hora_fin FORMAT "x(12)":U
      hora_inicio-1 FORMAT "x(12)":U
      hora_fin-1 FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS SIZE 66 BY 6.67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataBrowser
   Data Source: "dliqitemscontroltareas.w"
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
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
     _FldNameList[1]   > _<SDO>.rowObject.legajo
"legajo" ? ? "integer" ? ? ? ? ? ? no "?" no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = _<SDO>.rowObject.nombre
     _FldNameList[3]   > _<SDO>.rowObject.total_horas
"total_horas" ? ? "decimal" ? ? ? ? ? ? no ? no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = _<SDO>.rowObject.id_tarea
     _FldNameList[5]   = _<SDO>.rowObject.abreviatura
     _FldNameList[6]   > _<SDO>.rowObject.id_lote
"id_lote" "Cod" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = _<SDO>.rowObject.desc-lote
     _FldNameList[8]   > _<SDO>.rowObject.tipo_turno
"tipo_turno" ? ? "logical" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > _<SDO>.rowObject.cant_jornal_norm
"cant_jornal_norm" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > _<SDO>.rowObject.cant_hs_norm
"cant_hs_norm" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > _<SDO>.rowObject.cant_hs_compensa
"cant_hs_compensa" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > _<SDO>.rowObject.cant_hs_extras
"cant_hs_extras" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > _<SDO>.rowObject.cantidad
"cantidad" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > _<SDO>.rowObject.id_unidad_liquidacion
"id_unidad_liquidacion" ? ? "integer" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > _<SDO>.rowObject.hora_inicio
"hora_inicio" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > _<SDO>.rowObject.hora_fin
"hora_fin" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   = _<SDO>.rowObject.hora_inicio-1
     _FldNameList[18]   = _<SDO>.rowObject.hora_fin-1
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
  APPLY "END":U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON CTRL-HOME OF br_table IN FRAME F-Main
DO:
  APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsdefault.i}
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
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  {src/adm2/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK bTableWin 


/* ***************************  Main Block  *************************** */

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
