&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES remitos

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table remitos.id_sucursal ~
remitos.id_tipo_movsto remitos.nro remitos.fecha remitos.nro_comprobante ~
remitos.fecha_proceso remitos.mercado remitos.estado remitos.impresion ~
remitos.Peso_neto remitos.Peso_bruto 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH remitos WHERE ~{&KEY-PHRASE} ~
      AND remitos.id_sucursal = 86 ~
 OR remitos.id_sucursal = 87 ~
 OR remitos.id_sucursal = 88 ~
 OR remitos.id_sucursal = 92 ~
 OR remitos.id_sucursal = 95 ~
 OR remitos.id_sucursal = 96 ~
 OR remitos.id_sucursal = 89 ~
 OR remitos.id_sucursal = 50 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH remitos WHERE ~{&KEY-PHRASE} ~
      AND remitos.id_sucursal = 86 ~
 OR remitos.id_sucursal = 87 ~
 OR remitos.id_sucursal = 88 ~
 OR remitos.id_sucursal = 92 ~
 OR remitos.id_sucursal = 95 ~
 OR remitos.id_sucursal = 96 ~
 OR remitos.id_sucursal = 89 ~
 OR remitos.id_sucursal = 50 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table remitos
&Scoped-define FIRST-TABLE-IN-QUERY-br_table remitos


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
id_cliente||y|general.remitos.id_cliente
nro||y|general.remitos.nro
id_sucursal||y|general.remitos.id_sucursal
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_cliente,nro,id_sucursal"':U).

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
      remitos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      remitos.id_sucursal COLUMN-LABEL "Suc" FORMAT ">>9":U WIDTH 6.2
      remitos.id_tipo_movsto COLUMN-LABEL "TMvsto" FORMAT ">>9":U
            WIDTH 9.2
      remitos.nro FORMAT ">>>>9":U
      remitos.fecha FORMAT "99/99/99":U WIDTH 11.2
      remitos.nro_comprobante FORMAT "xxxx-xxxxxxxx":U WIDTH 20.2
      remitos.fecha_proceso FORMAT "99/99/99":U
      remitos.mercado COLUMN-LABEL "Mer" FORMAT "9":U WIDTH 5.4
      remitos.estado FORMAT "Vigente/Anulado":U WIDTH 10.2
      remitos.impresion FORMAT ">>9":U WIDTH 8.2
      remitos.Peso_neto COLUMN-LABEL "Peso Neto" FORMAT "->>>>9.9999":U
            WIDTH 13.8
      remitos.Peso_bruto COLUMN-LABEL "Peso Bruto" FORMAT "->>>>9.9999":U
            WIDTH 14.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 147 BY 10
         FONT 0.


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
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 10.19
         WIDTH              = 147.2.
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
     _TblList          = "general.remitos"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "general.remitos.id_sucursal = 86
 OR general.remitos.id_sucursal = 87
 OR general.remitos.id_sucursal = 88
 OR general.remitos.id_sucursal = 92
 OR general.remitos.id_sucursal = 95
 OR general.remitos.id_sucursal = 96
 OR general.remitos.id_sucursal = 89
 OR general.remitos.id_sucursal = 50"
     _FldNameList[1]   > general.remitos.id_sucursal
"remitos.id_sucursal" "Suc" ? "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" ""
     _FldNameList[2]   > general.remitos.id_tipo_movsto
"remitos.id_tipo_movsto" "TMvsto" ? "integer" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" ""
     _FldNameList[3]   > general.remitos.nro
"remitos.nro" ? ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > general.remitos.fecha
"remitos.fecha" ? ? "date" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" ""
     _FldNameList[5]   > general.remitos.nro_comprobante
"remitos.nro_comprobante" ? ? "character" ? ? ? ? ? ? no ? no no "20.2" yes no no "U" "" ""
     _FldNameList[6]   = general.remitos.fecha_proceso
     _FldNameList[7]   > general.remitos.mercado
"remitos.mercado" "Mer" ? "integer" ? ? ? ? ? ? no ? no no "5.4" yes no no "U" "" ""
     _FldNameList[8]   > general.remitos.estado
"remitos.estado" ? ? "logical" ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" ""
     _FldNameList[9]   > general.remitos.impresion
"remitos.impresion" ? ? "integer" ? ? ? ? ? ? no ? no no "8.2" yes no no "U" "" ""
     _FldNameList[10]   > general.remitos.Peso_neto
"remitos.Peso_neto" "Peso Neto" "->>>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "13.8" yes no no "U" "" ""
     _FldNameList[11]   > general.remitos.Peso_bruto
"remitos.Peso_bruto" "Peso Bruto" "->>>>9.9999" "decimal" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" ""
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF









/******** EMPIEZA MENU ************/
define sub-menu Ordena 
  menu-item remitos-id_sucursal
label 'id_sucursal( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.id_sucursal').
                  run set-attribute-list('titulo=Sucursal').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.id_sucursal
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-id_tipo_movsto
label 'id_tipo_movsto( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.id_tipo_movsto').
                  run set-attribute-list('titulo=T/Movsto').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.id_tipo_movsto
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-nro
label 'nro( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.nro').
                  run set-attribute-list('titulo=N�mero').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.nro
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-fecha
label 'fecha( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.fecha').
                  run set-attribute-list('titulo=Fecha').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.fecha
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-nro_comprobante
label 'nro_comprobante( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.nro_comprobante').
                  run set-attribute-list('titulo=Comp.Legal').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.nro_comprobante
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-fecha_proceso
label 'fecha_proceso( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.fecha_proceso').
                  run set-attribute-list('titulo=Fecha Proc.').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.fecha_proceso
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-mercado
label 'mercado( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.mercado').
                  run set-attribute-list('titulo=Mercado').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.mercado
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-estado
label 'estado( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.estado').
                  run set-attribute-list('titulo=Estado').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.estado
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-impresion
label 'impresion( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.impresion').
                  run set-attribute-list('titulo=Impresiones').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.impresion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-Peso_neto
label 'Peso_neto( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.Peso_neto').
                  run set-attribute-list('titulo=Peso_neto').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.Peso_neto
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item remitos-Peso_bruto
label 'Peso_bruto( remitos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=remitos.Peso_bruto').
                  run set-attribute-list('titulo=Peso bruto').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by remitos.Peso_bruto
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by remitos.id_sucursal
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=remitos.id_sucursal').
run set-attribute-list('titulo=Sucursal').
/********** TERMINA MENU ***********/

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
  WHEN 'remitos.id_sucursal' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_sucursal = integer(valor)  no-lock no-error .
  WHEN 'remitos.id_tipo_movsto' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_tipo_movsto = integer(valor)  no-lock no-error .
  WHEN 'remitos.nro' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  nro = integer(valor)  no-lock no-error .
  WHEN 'remitos.fecha' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  fecha = date(valor)  no-lock no-error .
  WHEN 'remitos.nro_comprobante' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  nro_comprobante begins valor  no-lock no-error .
  WHEN 'remitos.fecha_proceso' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  fecha_proceso = date(valor)  no-lock no-error .
  WHEN 'remitos.mercado' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  mercado = integer(valor)  no-lock no-error .
  WHEN 'remitos.estado' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  estado no-lock no-error .
  WHEN 'remitos.impresion' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  impresion = integer(valor)  no-lock no-error .
  WHEN 'remitos.Peso_neto' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Peso_neto = decimal(valor)  no-lock no-error .
  WHEN 'remitos.Peso_bruto' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Peso_bruto = decimal(valor)  no-lock no-error .
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
  {src/adm/template/sndkycas.i "id_cliente" "remitos" "id_cliente"}
  {src/adm/template/sndkycas.i "nro" "remitos" "nro"}
  {src/adm/template/sndkycas.i "id_sucursal" "remitos" "id_sucursal"}

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
  {src/adm/template/snd-list.i "remitos"}

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

