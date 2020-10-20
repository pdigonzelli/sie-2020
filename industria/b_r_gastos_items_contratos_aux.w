&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

define var i as integer no-undo.
define var nombre-item as character no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE csmartbrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Target,TableIO-Target,Consulta-Target,Navigation-Target,record-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES items_contratos
&Scoped-define FIRST-EXTERNAL-TABLE items_contratos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR items_contratos.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES r_gastos_items_contrato gastos_venta ~
tipo_unidad_venta tipo_moneda

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table r_gastos_items_contrato.id_gasto ~
gastos_venta.descripcion r_gastos_items_contrato.id_moneda ~
tipo_moneda.descripcion r_gastos_items_contrato.importe ~
r_gastos_items_contrato.id_tipo_unidad_venta tipo_unidad_venta.abreviatura 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ~
r_gastos_items_contrato.id_gasto r_gastos_items_contrato.id_moneda ~
r_gastos_items_contrato.importe ~
r_gastos_items_contrato.id_tipo_unidad_venta 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table r_gastos_items_contrato
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table r_gastos_items_contrato
&Scoped-define QUERY-STRING-br_table FOR EACH r_gastos_items_contrato OF items_contratos WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH gastos_venta OF r_gastos_items_contrato NO-LOCK, ~
      EACH tipo_unidad_venta OF r_gastos_items_contrato NO-LOCK, ~
      EACH tipo_moneda OF r_gastos_items_contrato NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH r_gastos_items_contrato OF items_contratos WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH gastos_venta OF r_gastos_items_contrato NO-LOCK, ~
      EACH tipo_unidad_venta OF r_gastos_items_contrato NO-LOCK, ~
      EACH tipo_moneda OF r_gastos_items_contrato NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table r_gastos_items_contrato ~
gastos_venta tipo_unidad_venta tipo_moneda
&Scoped-define FIRST-TABLE-IN-QUERY-br_table r_gastos_items_contrato
&Scoped-define SECOND-TABLE-IN-QUERY-br_table gastos_venta
&Scoped-define THIRD-TABLE-IN-QUERY-br_table tipo_unidad_venta
&Scoped-define FOURTH-TABLE-IN-QUERY-br_table tipo_moneda


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
id_gasto||y|general.r_gastos_items_contrato.id_gasto
id_tipo_contrato||y|general.r_gastos_items_contrato.id_tipo_contrato
id_tipo_unidad_venta||y|general.r_gastos_items_contrato.id_tipo_unidad_venta
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_gasto,id_tipo_contrato,id_tipo_unidad_venta"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Menues" B-table-Win _INLINE
/* Actions: ? custom/support/cusbmen.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" B-table-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Cabecera" B-table-Win _INLINE
/* Actions: ? custom/support/set-cabecera.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Detalle" B-table-Win _INLINE
/* Actions: ? custom/support/set-detalle.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Items" B-table-Win _INLINE
/* Actions: ? custom/support/set-items.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Modalidad de Insercion Browser" B-table-Win _INLINE
/* Actions: ? custom/support/bmodins.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Relaciones" B-table-Win _INLINE
/* Actions: ? custom/support/keyeditb.w ? ? ? */
/* campos relacionados con tablas externas 
general.r_gastos_items_contrato.id_gasto;wc_gastos_venta.w;gastos_venta.descripcion
general.r_gastos_items_contrato.id_moneda;wc_tipo_moneda.w;tipo_moneda.descripcion
general.r_gastos_items_contrato.id_tipo_unidad_venta;wc_tipo_unidad_venta.w;tipo_unidad_venta.abreviatura
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valida B-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      r_gastos_items_contrato, 
      gastos_venta, 
      tipo_unidad_venta, 
      tipo_moneda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      r_gastos_items_contrato.id_gasto COLUMN-LABEL "Cod" FORMAT ">>9":U
            WIDTH 5.2
      gastos_venta.descripcion COLUMN-LABEL "Gastos" FORMAT "X(20)":U
            WIDTH 24.2
      r_gastos_items_contrato.id_moneda COLUMN-LABEL "Mon" FORMAT ">>9":U
            WIDTH 5.4
      tipo_moneda.descripcion COLUMN-LABEL "Moneda" FORMAT "x(30)":U
            WIDTH 27.2
      r_gastos_items_contrato.importe FORMAT ">,>>9.9999":U WIDTH 16.2
      r_gastos_items_contrato.id_tipo_unidad_venta FORMAT ">>9":U
      tipo_unidad_venta.abreviatura COLUMN-LABEL "UV" FORMAT "X(2)":U
            WIDTH 4.4
  ENABLE
      r_gastos_items_contrato.id_gasto
      r_gastos_items_contrato.id_moneda
      r_gastos_items_contrato.importe
      r_gastos_items_contrato.id_tipo_unidad_venta
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 138 BY 5
         BGCOLOR 15 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: csmartbrowser
   External Tables: general.items_contratos
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 5.19
         WIDTH              = 138.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/cbrowser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
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
     _TblList          = "general.r_gastos_items_contrato OF general.items_contratos,industria.gastos_venta OF general.r_gastos_items_contrato,industria.tipo_unidad_venta OF general.r_gastos_items_contrato,comercial.tipo_moneda OF general.r_gastos_items_contrato"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > general.r_gastos_items_contrato.id_gasto
"general.r_gastos_items_contrato.id_gasto" "Cod" ">>9" "integer" ? ? ? ? ? ? yes ? no no "5.2" yes no no "U" "" ""
     _FldNameList[2]   > general.gastos_venta.descripcion
"general.gastos_venta.descripcion" "Gastos" "X(20)" "character" ? ? ? ? ? ? no ? no no "24.2" yes no no "U" "" ""
     _FldNameList[3]   > general.r_gastos_items_contrato.id_moneda
"general.r_gastos_items_contrato.id_moneda" "Mon" ">>9" "integer" ? ? ? ? ? ? yes ? no no "5.4" yes no no "U" "" ""
     _FldNameList[4]   > comercial.tipo_moneda.descripcion
"comercial.tipo_moneda.descripcion" "Moneda" ? "character" ? ? ? ? ? ? no ? no no "27.2" yes no no "U" "" ""
     _FldNameList[5]   > general.r_gastos_items_contrato.importe
"general.r_gastos_items_contrato.importe" ? ">,>>9.9999" "decimal" ? ? ? ? ? ? yes ? no no "16.2" yes no no "U" "" ""
     _FldNameList[6]   > general.r_gastos_items_contrato.id_tipo_unidad_venta
"general.r_gastos_items_contrato.id_tipo_unidad_venta" ? ">>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > general.tipo_unidad_venta.abreviatura
"general.tipo_unidad_venta.abreviatura" "UV" ? "character" ? ? ? ? ? ? no ? no no "4.4" yes no no "U" "" ""
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON F3 OF br_table IN FRAME F-Main
DO:
  run buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
  define var r as rowid no-undo.
  define var c as character no-undo.
  define var h as handle no-undo.
  run get-link-handle in adm-broker-hdl ( input this-procedure , input 'Consulta-SOURCE' , output c).
  h=widget-handle(c).
  if valid-handle(h) then
    run dispatch in h ('busca-rowid').  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
     /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
   /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME r_gastos_items_contrato.id_gasto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_gastos_items_contrato.id_gasto br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF r_gastos_items_contrato.id_gasto IN BROWSE br_table /* Cod */
DO:
  /*
    find gastos_venta where gastos_venta.id_gasto = integer(r_gastos_items_contrato.id_gasto:screen-value in browse {&BROWSE-NAME}) no-lock no-error.
  if available gastos_venta then 
        do:
           gastos_venta.descripcion:screen-value in browse {&BROWSE-NAME} = gastos_venta.descripcion.
        end.
        
        */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_gastos_items_contrato.id_gasto br_table _BROWSE-COLUMN B-table-Win
ON MOUSE-SELECT-DBLCLICK OF r_gastos_items_contrato.id_gasto IN BROWSE br_table /* Cod */
DO:
  /*
  define var r as rowid.
    run wc_gastos_venta.w(output r).
    find gastos_venta where rowid(gastos_venta) = r no-lock no-error.
    if available gastos_venta then 
     do:  
       r_gastos_items_contrato.id_gasto:screen-value in browse {&BROWSE-NAME} = string(gastos_venta.id_gasto).
    
        find gastos_venta where gastos_venta.id_gasto = integer(r_gastos_items_contrato.id_gasto:screen-value in browse {&BROWSE-NAME}) no-lock no-error.
        if available gastos_venta then 
            do:
                   gastos_venta.descripcion:screen-value in browse {&BROWSE-NAME} = gastos_venta.descripcion.
            end.
    end.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME r_gastos_items_contrato.id_moneda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_gastos_items_contrato.id_moneda br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF r_gastos_items_contrato.id_moneda IN BROWSE br_table /* Mon */
DO:
  /*
  find tipo_moneda where tipo_moneda.id_moneda = integer(r_gastos_items_contrato.id_moneda:screen-value in browse {&BROWSE-NAME}) no-lock no-error.
     if available tipo_moneda then 
       tipo_moneda.descripcion:screen-value in browse {&BROWSE-NAME} = string(tipo_moneda.descripcion).
       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_gastos_items_contrato.id_moneda br_table _BROWSE-COLUMN B-table-Win
ON MOUSE-SELECT-DBLCLICK OF r_gastos_items_contrato.id_moneda IN BROWSE br_table /* Mon */
DO:
  /*
    define var r as rowid.
   run wc_tipo_moneda.w(output r).
     find tipo_moneda where rowid(tipo_moneda) = r no-lock no-error.
     if available tipo_moneda then 
       r_gastos_items_contrato.id_moneda:screen-value in browse {&BROWSE-NAME} = string(tipo_moneda.id_moneda).
       
       find tipo_moneda where tipo_moneda.id_moneda = integer(r_gastos_items_contrato.id_moneda:screen-value in browse {&BROWSE-NAME}) no-lock no-error.
     if available tipo_moneda then 
       tipo_moneda.descripcion:screen-value in browse {&BROWSE-NAME} = string(tipo_moneda.descripcion).

*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME r_gastos_items_contrato.id_tipo_unidad_venta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_gastos_items_contrato.id_tipo_unidad_venta br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF r_gastos_items_contrato.id_tipo_unidad_venta IN BROWSE br_table /* U.Venta */
DO:
  /*
  find tipo_unidad_venta where tipo_unidad_venta.id_tipo_unidad_venta = integer(r_gastos_items_contrato.id_tipo_unidad_venta:screen-value in browse {&BROWSE-NAME}) no-lock no-error.
     if available tipo_unidad_venta then 
       tipo_unidad_venta.abreviatura:screen-value in browse {&BROWSE-NAME} = string(tipo_unidad_venta.abreviatura).
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r_gastos_items_contrato.id_tipo_unidad_venta br_table _BROWSE-COLUMN B-table-Win
ON MOUSE-SELECT-DBLCLICK OF r_gastos_items_contrato.id_tipo_unidad_venta IN BROWSE br_table /* U.Venta */
DO:
  /*
   define var r as rowid.
   run wc_tipo_unidad_venta.w(output r).
     find tipo_unidad_venta where rowid(tipo_unidad_venta) = r no-lock no-error.
     if available tipo_unidad_venta then 
       r_gastos_items_contrato.id_tipo_unidad_venta:screen-value in browse {&BROWSE-NAME} = string(tipo_unidad_venta.id_tipo_unidad_venta).
       
       
       find tipo_unidad_venta where tipo_unidad_venta.id_tipo_unidad_venta = integer(r_gastos_items_contrato.id_tipo_unidad_venta:screen-value in browse {&BROWSE-NAME}) no-lock no-error.
     if available tipo_unidad_venta then 
       tipo_unidad_venta.abreviatura:screen-value in browse {&BROWSE-NAME} = string(tipo_unidad_venta.abreviatura).
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF



/******** EMPIEZA MENU ************/
define sub-menu Ordena 
  menu-item r_gastos_items_contrato-id_gasto
label 'id_gasto( r_gastos_items_contrato ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=r_gastos_items_contrato.id_gasto').
                  run set-attribute-list('titulo=Codigo Gastos').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by r_gastos_items_contrato.id_gasto
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item r_gastos_items_contrato-id_moneda
label 'id_moneda( r_gastos_items_contrato ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=r_gastos_items_contrato.id_moneda').
                  run set-attribute-list('titulo=Moneda').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by r_gastos_items_contrato.id_moneda
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item r_gastos_items_contrato-importe
label 'importe( r_gastos_items_contrato ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=r_gastos_items_contrato.importe').
                  run set-attribute-list('titulo=Importe').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by r_gastos_items_contrato.importe
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item r_gastos_items_contrato-id_tipo_unidad_venta
label 'id_tipo_unidad_venta( r_gastos_items_contrato ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=r_gastos_items_contrato.id_tipo_unidad_venta').
                  run set-attribute-list('titulo=Unidad Venta').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by r_gastos_items_contrato.id_tipo_unidad_venta
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by r_gastos_items_contrato.id_gasto
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=r_gastos_items_contrato.id_gasto').
run set-attribute-list('titulo=Codigo Gastos').
/********** TERMINA MENU ***********/


/********** COMIENZAN TRIGGERS *************/


r_gastos_items_contrato.id_gasto:label-bgcolor in browse {&BROWSE-NAME} = 7.
r_gastos_items_contrato.id_gasto:label-fgcolor in browse {&BROWSE-NAME} = 15.
r_gastos_items_contrato.id_gasto:label-bgcolor in browse {&BROWSE-NAME} = 7.
r_gastos_items_contrato.id_gasto:label-fgcolor in browse {&BROWSE-NAME} = 15.
r_gastos_items_contrato.id_gasto:label-bgcolor in browse {&BROWSE-NAME} = 7.
r_gastos_items_contrato.id_gasto:label-fgcolor in browse {&BROWSE-NAME} = 15.
on MOUSE-SELECT-DBLCLICK,F10 of r_gastos_items_contrato.id_gasto in browse {&BROWSE-NAME} do:
   define var r as rowid no-undo.
   define var hcolumn as widget-handle no-undo.
   hcolumn = browse {&BROWSE-NAME}:current-column. 
   if hcolumn:name = 'id_gasto' then do: 
      run wc_gastos_venta.w(output r).
     find gastos_venta where rowid(gastos_venta) = r no-lock no-error.
     if available gastos_venta then 
       r_gastos_items_contrato.id_gasto:screen-value in browse {&BROWSE-NAME} = string(gastos_venta.id_gasto).
   end.
end.

on MOUSE-SELECT-DBLCLICK,F10 of r_gastos_items_contrato.id_moneda in browse {&BROWSE-NAME} do:
   define var r as rowid no-undo.
   define var hcolumn as widget-handle no-undo.
   hcolumn = browse {&BROWSE-NAME}:current-column. 
   if hcolumn:name = 'id_moneda' then do: 
      run wc_tipo_moneda.w(output r).
     find tipo_moneda where rowid(tipo_moneda) = r no-lock no-error.
     if available tipo_moneda then 
       r_gastos_items_contrato.id_moneda:screen-value in browse {&BROWSE-NAME} = string(tipo_moneda.id_moneda).
   end.
end.

on MOUSE-SELECT-DBLCLICK,F10 of r_gastos_items_contrato.id_tipo_unidad_venta in browse {&BROWSE-NAME} do:
   define var r as rowid no-undo.
   define var hcolumn as widget-handle no-undo.
   hcolumn = browse {&BROWSE-NAME}:current-column. 
   if hcolumn:name = 'id_tipo_unidad_venta' then do: 
      run wc_tipo_unidad_venta.w(output r).
     find tipo_unidad_venta where rowid(tipo_unidad_venta) = r no-lock no-error.
     if available tipo_unidad_venta then 
       r_gastos_items_contrato.id_tipo_unidad_venta:screen-value in browse {&BROWSE-NAME} = string(tipo_unidad_venta.id_tipo_unidad_venta).
   end.
end.

/********** TERMINAN TRIGGERS **************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-create B-table-Win 
PROCEDURE adm-post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign r_gastos_items_contrato.id_contrato = items_contratos.id_contrato
               r_gastos_items_contrato.id_tipo_contrato = items_contratos.id_tipo_contrato
               r_gastos_items_contrato.anio = items_contratos.anio
               r_gastos_items_contrato.item = items_contratos.item.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-delete B-table-Win 
PROCEDURE adm-post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-update B-table-Win 
PROCEDURE adm-post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-create B-table-Win 
PROCEDURE adm-pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-delete B-table-Win 
PROCEDURE adm-pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-update B-table-Win 
PROCEDURE adm-pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "items_contratos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "items_contratos"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros B-table-Win 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista-parametros as character no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE busca-registro B-table-Win 
PROCEDURE busca-registro :
define input parameter p as character no-undo.
define input parameter valor as character no-undo.
case p:
  WHEN 'r_gastos_items_contrato.id_gasto' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_gasto = integer(valor)  no-lock no-error .
  WHEN 'r_gastos_items_contrato.id_moneda' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_moneda = integer(valor)  no-lock no-error .
  WHEN 'r_gastos_items_contrato.importe' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  importe = decimal(valor)  no-lock no-error .
  WHEN 'r_gastos_items_contrato.id_tipo_unidad_venta' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_tipo_unidad_venta = integer(valor)  no-lock no-error .
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscar B-table-Win 
PROCEDURE buscar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF "{&FIELDS-IN-QUERY-{&BROWSE-NAME}}" <> "" &THEN
define var valor as character.
define var r as rowid no-undo.
define var p as character no-undo.
define var q as character no-undo.

run get-attribute('busca').
p= return-value.

if p <> "yes" Then
return.

run get-attribute('orden').
p= return-value.

run get-attribute('titulo').
q= return-value.

{&BROWSE-NAME}:set-repositioned-row(3,"always") in frame {&FRAME-NAME}.

run custom/support/fbusca.w  ( input p , input q, output valor) .

if valor <> "" then
do:
    run busca-registro (input p , input valor).
    if available {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} then
    do:
        r=rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
        reposition {&BROWSE-NAME} to rowid r.
        apply 'value-changed' to self.
    end.
end.    
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid B-table-Win 
PROCEDURE devuelve-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.
r=?.
&IF "{&FIELDS-IN-QUERY-{&BROWSE-NAME}}" <> "" &THEN
get current {&BROWSE-NAME}.
if available ({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) then
    r=rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
else
    r=?.    
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
{custom/support/crow-available.i}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "id_gasto" "r_gastos_items_contrato" "id_gasto"}
  {src/adm/template/sndkycas.i "id_tipo_contrato" "r_gastos_items_contrato" "id_tipo_contrato"}
  {src/adm/template/sndkycas.i "id_tipo_unidad_venta" "r_gastos_items_contrato" "id_tipo_unidad_venta"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "items_contratos"}
  {src/adm/template/snd-list.i "r_gastos_items_contrato"}
  {src/adm/template/snd-list.i "gastos_venta"}
  {src/adm/template/snd-list.i "tipo_unidad_venta"}
  {src/adm/template/snd-list.i "tipo_moneda"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION valida B-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  case nombre:
    when "id_sucursal" then
        if integer(valor) = 0 then 
        do:
            mensaje = "error".
            return false.
         end.
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
