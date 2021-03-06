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

&Scoped-define ADM-SUPPORTED-LINKS                                          Record-Target,TableIO-Target,Consulta-Target,Navigation-Target,record-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES lotes_jugo
&Scoped-define FIRST-EXTERNAL-TABLE lotes_jugo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lotes_jugo.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Inspecciones_lote

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table Inspecciones_lote.Hora ~
Inspecciones_lote.Litros Inspecciones_lote.Bx_20_20 ~
Inspecciones_lote.Bx_correg Inspecciones_lote.Acidez_w_w ~
Inspecciones_lote.Acidez_w_v Inspecciones_lote.Porcentaje_pulpa ~
Inspecciones_lote.Ratio Inspecciones_lote.id_legajo_quimico 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table Inspecciones_lote.Hora ~
Inspecciones_lote.Litros Inspecciones_lote.Bx_20_20 ~
Inspecciones_lote.Bx_correg Inspecciones_lote.Acidez_w_w ~
Inspecciones_lote.Acidez_w_v Inspecciones_lote.Porcentaje_pulpa ~
Inspecciones_lote.Ratio 
&Scoped-define FIELD-PAIRS-IN-QUERY-br_table~
 ~{&FP1}Hora ~{&FP2}Hora ~{&FP3}~
 ~{&FP1}Litros ~{&FP2}Litros ~{&FP3}~
 ~{&FP1}Bx_20_20 ~{&FP2}Bx_20_20 ~{&FP3}~
 ~{&FP1}Bx_correg ~{&FP2}Bx_correg ~{&FP3}~
 ~{&FP1}Acidez_w_w ~{&FP2}Acidez_w_w ~{&FP3}~
 ~{&FP1}Acidez_w_v ~{&FP2}Acidez_w_v ~{&FP3}~
 ~{&FP1}Porcentaje_pulpa ~{&FP2}Porcentaje_pulpa ~{&FP3}~
 ~{&FP1}Ratio ~{&FP2}Ratio ~{&FP3}
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table Inspecciones_lote
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table Inspecciones_lote
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH Inspecciones_lote OF lotes_jugo WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table Inspecciones_lote
&Scoped-define FIRST-TABLE-IN-QUERY-br_table Inspecciones_lote


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
id_sucursal||y|general.Inspecciones_lote.id_sucursal
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_sucursal"':U).

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
general.Inspecciones_lote.id_legajo_quimico;wc_quimico.w;legajo.
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
      Inspecciones_lote SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      Inspecciones_lote.Hora
      Inspecciones_lote.Litros FORMAT ">>>,>>9"
      Inspecciones_lote.Bx_20_20 COLUMN-LABEL "Bx 20/20" FORMAT ">,>>9.99"
      Inspecciones_lote.Bx_correg COLUMN-LABEL "Bx Correg." FORMAT ">,>>9.99"
      Inspecciones_lote.Acidez_w_w COLUMN-LABEL "Acidez w/w" FORMAT ">,>>9.99"
      Inspecciones_lote.Acidez_w_v COLUMN-LABEL "Acidez w/v" FORMAT ">,>>9.99"
      Inspecciones_lote.Porcentaje_pulpa COLUMN-LABEL "% pulpa" FORMAT ">>9.99"
      Inspecciones_lote.Ratio FORMAT ">,>>9.99"
      Inspecciones_lote.id_legajo_quimico FORMAT "x(12)"
  ENABLE
      Inspecciones_lote.Hora
      Inspecciones_lote.Litros
      Inspecciones_lote.Bx_20_20
      Inspecciones_lote.Bx_correg
      Inspecciones_lote.Acidez_w_w
      Inspecciones_lote.Acidez_w_v
      Inspecciones_lote.Porcentaje_pulpa
      Inspecciones_lote.Ratio
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 128 BY 4.76
         BGCOLOR 17 FGCOLOR 0 FONT 0.


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
   External Tables: general.lotes_jugo
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 4.76
         WIDTH              = 128.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

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
     _TblList          = "general.Inspecciones_lote OF general.lotes_jugo"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > general.Inspecciones_lote.Hora
"Inspecciones_lote.Hora" ? ? "date" ? ? ? ? ? ? yes ?
     _FldNameList[2]   > general.Inspecciones_lote.Litros
"Inspecciones_lote.Litros" ? ">>>,>>9" "integer" ? ? ? ? ? ? yes ?
     _FldNameList[3]   > general.Inspecciones_lote.Bx_20_20
"Inspecciones_lote.Bx_20_20" "Bx 20/20" ">,>>9.99" "decimal" ? ? ? ? ? ? yes ?
     _FldNameList[4]   > general.Inspecciones_lote.Bx_correg
"Inspecciones_lote.Bx_correg" "Bx Correg." ">,>>9.99" "decimal" ? ? ? ? ? ? yes ?
     _FldNameList[5]   > general.Inspecciones_lote.Acidez_w_w
"Inspecciones_lote.Acidez_w_w" "Acidez w/w" ">,>>9.99" "decimal" ? ? ? ? ? ? yes ?
     _FldNameList[6]   > general.Inspecciones_lote.Acidez_w_v
"Inspecciones_lote.Acidez_w_v" "Acidez w/v" ">,>>9.99" "decimal" ? ? ? ? ? ? yes ?
     _FldNameList[7]   > general.Inspecciones_lote.Porcentaje_pulpa
"Inspecciones_lote.Porcentaje_pulpa" "% pulpa" ">>9.99" "decimal" ? ? ? ? ? ? yes ?
     _FldNameList[8]   > general.Inspecciones_lote.Ratio
"Inspecciones_lote.Ratio" ? ">,>>9.99" "decimal" ? ? ? ? ? ? yes ?
     _FldNameList[9]   > general.Inspecciones_lote.id_legajo_quimico
"Inspecciones_lote.id_legajo_quimico" ? "x(12)" "integer" ? ? ? ? ? ? no ?
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/cbrowser.i}

/* _UIB-CODE-BLOCK-END */
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


&Scoped-define SELF-NAME Inspecciones_lote.Hora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inspecciones_lote.Hora br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF Inspecciones_lote.Hora IN BROWSE br_table /* Hora */
DO:
  if integer(substring(inspecciones_lote.hora:screen-value in browse {&BROWSE-NAME},1,2)) > 24  or integer(substring(inspecciones_lote.hora:screen-value in browse {&BROWSE-NAME},4,2)) > 60 then 
        do:
            message "Debe ingresar una hora v�lida" view-as alert-box.
            return no-apply.
         end.
  /*message "entro" inspecciones_lote.hora:screen-value in browse {&BROWSE-NAME} view-as alert-box.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Inspecciones_lote.id_legajo_quimico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Inspecciones_lote.id_legajo_quimico br_table _BROWSE-COLUMN B-table-Win
ON MOUSE-SELECT-DBLCLICK OF Inspecciones_lote.id_legajo_quimico IN BROWSE br_table /* Quimico */
DO:
  define var r as rowid.
  define var num as integer.
  num = 130.
     run wc_quimico.w (input num, output r).
     find legajo where rowid(legajo) = r no-lock no-error.
     if available legajo then 
        inspecciones_lote.id_legajo_quimico:screen-value in browse {&BROWSE-NAME} = string(legajo.lega-12).
       

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
  menu-item Inspecciones_lote-Hora
label 'Hora( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.Hora').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.Hora
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Inspecciones_lote-Litros
label 'Litros( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.Litros').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.Litros
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Inspecciones_lote-Bx_20_20
label 'Bx_20_20( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.Bx_20_20').
                  run set-attribute-list('titulo=Bx_20/20').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.Bx_20_20
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Inspecciones_lote-Bx_correg
label 'Bx_correg( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.Bx_correg').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.Bx_correg
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Inspecciones_lote-Acidez_w_w
label 'Acidez_w_w( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.Acidez_w_w').
                  run set-attribute-list('titulo=Acidez_w/w').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.Acidez_w_w
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Inspecciones_lote-Acidez_w_v
label 'Acidez_w_v( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.Acidez_w_v').
                  run set-attribute-list('titulo=Acidez_w/v').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.Acidez_w_v
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Inspecciones_lote-Porcentaje_pulpa
label 'Porcentaje_pulpa( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.Porcentaje_pulpa').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.Porcentaje_pulpa
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Inspecciones_lote-Ratio
label 'Ratio( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.Ratio').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.Ratio
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item Inspecciones_lote-id_legajo_quimico
label 'id_legajo_quimico( Inspecciones_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=Inspecciones_lote.id_legajo_quimico').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by Inspecciones_lote.id_legajo_quimico
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by Inspecciones_lote.Hora
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=Inspecciones_lote.Hora').
run set-attribute-list('titulo=Dato').
/********** TERMINA MENU ***********/



/**********EMPIEZA-TIPO-DETALLE*********/
  run set-attribute-list ('tipo-detalle=detalle'). 
/**********TERMINA-TIPO-DETALLE*********/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win adm/support/_adm-opn.p
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
define var hcontainer as handle.
define var r as rowid.
define var ultimo as integer.
define buffer ins for inspecciones_lote.

RUN get-container (output hcontainer).

RUN get-rowid-cabecera in hcontainer (output r).
for each lotes_jugo where rowid(lotes_jugo) = r.

    find last ins where ins.id_empresa = lotes_jugo.id_empresa and
                        ins.id_sucursal = lotes_jugo.id_sucursal and
                        ins.id_tipotambor = lotes_jugo.id_tipotambor and
                        ins.nromov = lotes_jugo.nromov no-error.
    if available ins then ultimo = ins.id_inspeccion.
    else ultimo = 0.
        
    assign inspecciones_lote.id_empresa          = lotes_jugo.id_empresa
           inspecciones_lote.id_sucursal         = lotes_jugo.id_sucursal
           inspecciones_lote.id_lote             = lotes_jugo.id_lote
           inspecciones_lote.id_tipotambor       = lotes_jugo.id_tipotambor
           inspecciones_lote.nromov              = lotes_jugo.nromov
           inspecciones_lote.id_inspeccion       = ultimo + 1
           inspecciones_lote.id_legajo_quimico   = userid("userdb") 
           inspecciones_lote.c_usuario           = userid("userdb")
           inspecciones_lote.c_fecha             = today
           inspecciones_lote.c_hora              = string(time,"HH:MM:SS").

    if lotes_jugo.estado_lote = 1 then assign lotes_jugo.estado_lote = 2.
    
end.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "lotes_jugo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "lotes_jugo"}

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
  WHEN 'Inspecciones_lote.Hora' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Hora = valor  no-lock no-error .
  WHEN 'Inspecciones_lote.Litros' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Litros = integer(valor)  no-lock no-error .
  WHEN 'Inspecciones_lote.Bx_20_20' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Bx_20_20 = decimal(valor)  no-lock no-error .
  WHEN 'Inspecciones_lote.Bx_correg' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Bx_correg = decimal(valor)  no-lock no-error .
  WHEN 'Inspecciones_lote.Acidez_w_w' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Acidez_w_w = decimal(valor)  no-lock no-error .
  WHEN 'Inspecciones_lote.Acidez_w_v' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Acidez_w_v = decimal(valor)  no-lock no-error .
  WHEN 'Inspecciones_lote.Porcentaje_pulpa' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Porcentaje_pulpa = decimal(valor)  no-lock no-error .
  WHEN 'Inspecciones_lote.Ratio' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Ratio = decimal(valor)  no-lock no-error .
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilitar_relacion B-table-Win 
PROCEDURE habilitar_relacion :
define var field-group as handle.
define var cur-control as handle.
define var lista_relacion as character no-undo initial "id_sucursal,id_calidad,id_empresa".
field-group = frame {&FRAME-NAME}:first-child.
cur-control = field-group:first-tab-item.
do while valid-handle(cur-control): 

    if cur-control:visible and cur-control:type = "fill-in"
    and lookup(cur-control:name,lista_relacion) <> 0 then 
        cur-control:load-mouse-pointer("glove").
    cur-control = cur-control:next-tab-item.
end.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "id_sucursal" "Inspecciones_lote" "id_sucursal"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "lotes_jugo"}
  {src/adm/template/snd-list.i "Inspecciones_lote"}

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
    when "Hora" then
        if integer(substring(valor,1,2)) > 24  or integer(substring(valor,4,2)) > 60 then 
        do:
            mensaje = "Debe ingresar una hora v�lida".
            return false.
         end.
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


