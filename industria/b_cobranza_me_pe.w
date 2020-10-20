&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general         PROGRESS
          cons             PROGRESS
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
DEFINE VAR HA AS COM-HANDLE.

def var b-tipo-cliente as integer initial 2.

define var vfecha as date.
define var vcliente as integer.
define var vtipo as integer.
define var vconcil as logical.

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
&Scoped-define INTERNAL-TABLES cobranzas_me tipo_moneda clientes ~
tipo_cobranza

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table cobranzas_me.estado ~
cobranzas_me.nro_minuta cobranzas_me.fecha_comp cobranzas_me.id_sucursal ~
cobranzas_me.nro_comp cobranzas_me.id_cliente clientes.nombre ~
cobranzas_me.importe cobranzas_me.tipo_cambio cobranzas_me.importe_uss ~
cobranzas_me.saldo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH cobranzas_me WHERE ~{&KEY-PHRASE} ~
      AND (if vcliente <> 0 then cobranzas_me.id_cliente = vcliente else true)  and ~
(if vtipo <> 0 then cobranzas_me.id_tipo_cobranza = vtipo else true ) and ~
(if vfecha <> ? then cobranzas_me.fecha_comp = vfecha else true ) and ~
(if vconcil <> ? then cobranzas_me.conciliada = vconcil else true) ~
 AND cobranzas_me.estado = TRUE NO-LOCK, ~
      EACH tipo_moneda WHERE tipo_moneda.id_moneda = cobranzas_me.id_moneda OUTER-JOIN NO-LOCK, ~
      EACH clientes OF cobranzas_me OUTER-JOIN NO-LOCK, ~
      EACH tipo_cobranza OF cobranzas_me OUTER-JOIN NO-LOCK ~
    BY cobranzas_me.nro_minuta.
&Scoped-define TABLES-IN-QUERY-br_table cobranzas_me tipo_moneda clientes ~
tipo_cobranza
&Scoped-define FIRST-TABLE-IN-QUERY-br_table cobranzas_me
&Scoped-define SECOND-TABLE-IN-QUERY-br_table tipo_moneda
&Scoped-define THIRD-TABLE-IN-QUERY-br_table clientes
&Scoped-define FOURTH-TABLE-IN-QUERY-br_table tipo_cobranza


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-fecha fi-cliente fi-tipo COMBO-BOX-1 ~
BUTTON-15 BUTTON-16 br_table RECT-28 
&Scoped-Define DISPLAYED-OBJECTS fi-fecha fi-cliente fi-tipo COMBO-BOX-1 

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
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
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
DEFINE BUTTON BUTTON-15 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "&Filtrar" 
     SIZE 15 BY 1.05.

DEFINE BUTTON BUTTON-16 
     IMAGE-UP FILE "adeicon\cross":U
     LABEL "&Resetear" 
     SIZE 15 BY 1.05.

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos","Conciliadas","Pendientes" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-cliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-fecha AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fi-tipo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 133 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      cobranzas_me, 
      tipo_moneda, 
      clientes, 
      tipo_cobranza SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      cobranzas_me.estado COLUMN-LABEL "  Estado" FORMAT "Vigente/Anulado":U
      cobranzas_me.nro_minuta COLUMN-LABEL "  Numero Minuta" FORMAT "999-99-99999":U
            COLUMN-FONT 6
      cobranzas_me.fecha_comp COLUMN-LABEL "FechaCmp." FORMAT "99/99/99":U
      cobranzas_me.id_sucursal FORMAT ">>9":U
      cobranzas_me.nro_comp FORMAT "99999999":U
      cobranzas_me.id_cliente FORMAT "->,>>>,>>9":U
      clientes.nombre FORMAT "x(30)":U
      cobranzas_me.importe FORMAT "->>>,>>>,>>>,>>9.99":U
      cobranzas_me.tipo_cambio FORMAT "->>,>>9.999999":U
      cobranzas_me.importe_uss FORMAT "->>>,>>>,>>>,>>9.99":U
      cobranzas_me.saldo COLUMN-LABEL "Saldo MO" FORMAT "->>>,>>>,>>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 133 BY 10.48
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-fecha AT ROW 1.24 COL 30 COLON-ALIGNED
     fi-cliente AT ROW 1.24 COL 50 COLON-ALIGNED
     fi-tipo AT ROW 1.24 COL 69 COLON-ALIGNED
     COMBO-BOX-1 AT ROW 1.24 COL 84 COLON-ALIGNED NO-LABEL
     BUTTON-15 AT ROW 1.24 COL 103
     BUTTON-16 AT ROW 1.24 COL 118
     br_table AT ROW 2.67 COL 1
     RECT-28 AT ROW 1 COL 1
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
         HEIGHT             = 12.14
         WIDTH              = 133.4.
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
/* BROWSE-TAB br_table BUTTON-16 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "cons.cobranzas_me,comercial.tipo_moneda WHERE cons.cobranzas_me ...,general.clientes OF cons.cobranzas_me,cons.tipo_cobranza OF cons.cobranzas_me"
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", OUTER, OUTER, OUTER"
     _OrdList          = "cons.cobranzas_me.nro_minuta|yes"
     _Where[1]         = "(if vcliente <> 0 then cobranzas_me.id_cliente = vcliente else true)  and
(if vtipo <> 0 then cobranzas_me.id_tipo_cobranza = vtipo else true ) and
(if vfecha <> ? then cobranzas_me.fecha_comp = vfecha else true ) and
(if vconcil <> ? then cobranzas_me.conciliada = vconcil else true)
 AND cons.cobranzas_me.estado = TRUE"
     _JoinCode[2]      = "comercial.tipo_moneda.id_moneda = cons.cobranzas_me.id_moneda"
     _FldNameList[1]   > cons.cobranzas_me.estado
"cobranzas_me.estado" "  Estado" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > cons.cobranzas_me.nro_minuta
"cobranzas_me.nro_minuta" "  Numero Minuta" ? "character" ? ? 6 ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > cons.cobranzas_me.fecha_comp
"cobranzas_me.fecha_comp" "FechaCmp." "99/99/99" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = cons.cobranzas_me.id_sucursal
     _FldNameList[5]   > cons.cobranzas_me.nro_comp
"cobranzas_me.nro_comp" ? "99999999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   = cons.cobranzas_me.id_cliente
     _FldNameList[7]   = general.clientes.nombre
     _FldNameList[8]   = cons.cobranzas_me.importe
     _FldNameList[9]   = cons.cobranzas_me.tipo_cambio
     _FldNameList[10]   = cons.cobranzas_me.importe_uss
     _FldNameList[11]   > cons.cobranzas_me.saldo
"cobranzas_me.saldo" "Saldo MO" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:

  run color.
/*
  define var r as rowid.
  
  run get-rowid1 (output r).
  find cobranzas_me where rowid(cobranzas_me) = r.
  if r <> ? then
   do:  
/*      find first medios_cobranza where medios_cobranza.id_tipocomp  = cobranzas_me.id_tipocomp and
                                   medios_cobranza.id_operacion     = cobranzas_me.id_operacion and
                                   medios_cobranza.id_sucursal      = cobranzas_me.id_sucursal and
                                   medios_cobranza.nromov           = cobranzas_me.nromov  no-lock no-error.
                                       
      if  available  medios_cobranza then
          do:*/
          
         if cobranzas_me.saldo = 0 then
            do:    
               cobranzas_me.id_sucursal:bgcolor in browse {&BROWSE-NAME} = 14.
               cobranzas_me.nro_comp:bgcolor in browse {&BROWSE-NAME}    = 14.
               cobranzas_me.id_cliente:bgcolor in browse {&BROWSE-NAME}  = 14.
               cobranzas_me.fecha_comp:bgcolor in browse {&BROWSE-NAME}  = 14.
               tipo_moneda.descripcion:bgcolor in browse {&BROWSE-NAME}  = 14.
               cobranzas_me.importe:bgcolor in browse {&BROWSE-NAME}     = 14.
               cobranzas_me.importe_uss:bgcolor in browse {&BROWSE-NAME} = 14.
               cobranzas_me.tipo_cambio:bgcolor in browse {&BROWSE-NAME} = 14.
           end.
  end.         
         
*/
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


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 B-table-Win
ON CHOOSE OF BUTTON-15 IN FRAME F-Main /* Filtrar */
DO:
  vfecha   = date(fi-fecha:screen-value).
  vcliente = integer(fi-cliente:screen-value).
  vtipo    = integer(fi-tipo:screen-value).
  
  case COMBO-BOX-1:screen-value:
    when "Todos" then do:
     vconcil = ?.
    end.
    when "Conciliadas" then do:
     vconcil = true.
    end. 
    when "Pendientes" then do:
     vconcil = false.
    end. 
  end.
  {&OPEN-QUERY-{&BROWSE-NAME}}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 B-table-Win
ON CHOOSE OF BUTTON-16 IN FRAME F-Main /* Resetear */
DO:
  vtipo = 0.
  vcliente = 0.
  vfecha = ?.
  vconcil = ?.
  {&OPEN-QUERY-{&BROWSE-NAME}}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cliente IN FRAME F-Main /* Cliente */
DO:
define var r as rowid no-undo.
run ..\industria\wc_clientes.w(output r).
find clientes where rowid(clientes) = r no-lock no-error.
if available clientes then 
  fi-cliente:screen-value = string(clientes.id_cliente).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-tipo IN FRAME F-Main /* Tipo */
DO:
 define var r as rowid no-undo.
run ..\cons\c_j_tipo_cobranza.w(output r).
find tipo_cobranza where rowid(tipo_cobranza) = r no-lock no-error.
if available tipo_cobranza then 
   fi-tipo:screen-value = string(tipo_cobranza.id_tipo_cobranza).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

  

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF





 cobranzas_me.nro_comp:BGCOLOR IN BROWSE {&BROWSE-NAME} = 5.









/******** EMPIEZA MENU ************/
define sub-menu Ordena 
  menu-item cobranzas_me-estado
label 'estado( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.estado').
                  run set-attribute-list('titulo=Estado').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.estado
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-nro_minuta
label 'nro_minuta( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.nro_minuta').
                  run set-attribute-list('titulo=Nro. Minuta').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.nro_minuta
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-fecha_comp
label 'fecha_comp( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.fecha_comp').
                  run set-attribute-list('titulo=Fecha Comp.').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.fecha_comp
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-id_sucursal
label 'id_sucursal( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.id_sucursal').
                  run set-attribute-list('titulo=Sucursal').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.id_sucursal
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-nro_comp
label 'nro_comp( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.nro_comp').
                  run set-attribute-list('titulo=Nro. de Comp.').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.nro_comp
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-id_cliente
label 'id_cliente( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.id_cliente').
                  run set-attribute-list('titulo=C¢d. Cliente').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.id_cliente
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item clientes-nombre
label 'nombre( clientes ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=clientes.nombre').
                  run set-attribute-list('titulo=Nombre').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by clientes.nombre
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-importe
label 'importe( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.importe').
                  run set-attribute-list('titulo=Importe').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.importe
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-tipo_cambio
label 'tipo_cambio( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.tipo_cambio').
                  run set-attribute-list('titulo=Tipo de Cambio').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.tipo_cambio
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-importe_uss
label 'importe_uss( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.importe_uss').
                  run set-attribute-list('titulo=importe en U$').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.importe_uss
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item cobranzas_me-saldo
label 'saldo( cobranzas_me ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=cobranzas_me.saldo').
                  run set-attribute-list('titulo=Saldo').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by cobranzas_me.saldo
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by cobranzas_me.estado
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=cobranzas_me.estado').
run set-attribute-list('titulo=Estado').
/********** TERMINA MENU ***********/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  WHEN 'cobranzas_me.estado' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  estado no-lock no-error .
  WHEN 'cobranzas_me.nro_minuta' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  nro_minuta begins valor  no-lock no-error .
  WHEN 'cobranzas_me.fecha_comp' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  fecha_comp = date(valor)  no-lock no-error .
  WHEN 'cobranzas_me.id_sucursal' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_sucursal = integer(valor)  no-lock no-error .
  WHEN 'cobranzas_me.nro_comp' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  nro_comp = integer(valor)  no-lock no-error .
  WHEN 'cobranzas_me.id_cliente' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_cliente = integer(valor)  no-lock no-error .
  WHEN 'cobranzas_me.importe' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  importe = decimal(valor)  no-lock no-error .
  WHEN 'cobranzas_me.tipo_cambio' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  tipo_cambio = decimal(valor)  no-lock no-error .
  WHEN 'cobranzas_me.importe_uss' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  importe_uss = decimal(valor)  no-lock no-error .
  WHEN 'cobranzas_me.saldo' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  saldo = decimal(valor)  no-lock no-error .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE color B-table-Win 
PROCEDURE color :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if cobranzas_me.conciliada then
   do:
        cobranzas_me.estado:bgcolor in browse {&BROWSE-NAME}      = 14.
        /*cobranzas_me.nro_minuta:bgcolor in browse {&BROWSE-NAME}  = 14.
        cobranzas_me.nro_comp:bgcolor in browse {&BROWSE-NAME}    = 14.
        cobranzas_me.id_cliente:bgcolor in browse {&BROWSE-NAME}  = 14.
        cobranzas_me.fecha_comp:bgcolor in browse {&BROWSE-NAME}  = 14.
        cobranzas_me.importe:bgcolor in browse {&BROWSE-NAME}     = 14.
        cobranzas_me.importe_uss:bgcolor in browse {&BROWSE-NAME} = 14.
        cobranzas_me.tipo_cambio:bgcolor in browse {&BROWSE-NAME} = 14.
        cobranzas_me.id_sucursal:bgcolor in browse {&BROWSE-NAME} = 14.
        clientes.nombre:bgcolor in browse {&BROWSE-NAME} = 14.*/
   end.
 else
   do:
        cobranzas_me.estado:bgcolor in browse {&BROWSE-NAME}      = 11.
/*        cobranzas_me.nro_minuta:bgcolor in browse {&BROWSE-NAME}  = 11.
        cobranzas_me.nro_comp:bgcolor in browse {&BROWSE-NAME}    = 11.
        cobranzas_me.id_cliente:bgcolor in browse {&BROWSE-NAME}  = 11.
        cobranzas_me.fecha_comp:bgcolor in browse {&BROWSE-NAME}  = 11.
        cobranzas_me.importe:bgcolor in browse {&BROWSE-NAME}     = 11.
        cobranzas_me.importe_uss:bgcolor in browse {&BROWSE-NAME} = 11.
        cobranzas_me.tipo_cambio:bgcolor in browse {&BROWSE-NAME} = 11.
        cobranzas_me.id_sucursal:bgcolor in browse {&BROWSE-NAME} = 11.
        clientes.nombre:bgcolor in browse {&BROWSE-NAME} = 11.*/

   end.     
   
    if not cobranzas_me.estado then do:
        cobranzas_me.estado:bgcolor in browse {&BROWSE-NAME} = 12.
/*        cobranzas_me.nro_minuta:bgcolor in browse {&BROWSE-NAME} = 12.
         cobranzas_me.nro_comp:bgcolor in browse {&BROWSE-NAME}    = 12.
        cobranzas_me.id_cliente:bgcolor in browse {&BROWSE-NAME}  = 12.
        cobranzas_me.fecha_comp:bgcolor in browse {&BROWSE-NAME}  = 12.
        cobranzas_me.importe:bgcolor in browse {&BROWSE-NAME}     = 12.
        cobranzas_me.importe_uss:bgcolor in browse {&BROWSE-NAME} = 12.
        cobranzas_me.tipo_cambio:bgcolor in browse {&BROWSE-NAME} = 12.
        cobranzas_me.id_sucursal:bgcolor in browse {&BROWSE-NAME} = 12.
        clientes.nombre:bgcolor in browse {&BROWSE-NAME} = 12.*/


   end.



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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  vtipo = 0.
  vcliente = 0.
  vfecha = ?.
  vconcil = ?.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  COMBO-BOX-1:screen-value in frame {&FRAME-NAME}= "Todos".
  fi-cliente:load-mouse-pointer ('glove') in frame {&FRAME-NAME}.
  fi-tipo:load-mouse-pointer ('glove') in frame {&FRAME-NAME}.
  


 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refrescar B-table-Win 
PROCEDURE refrescar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&OPEN-QUERY-{&BROWSE-NAME}}
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
  {src/adm/template/snd-list.i "cobranzas_me"}
  {src/adm/template/snd-list.i "tipo_moneda"}
  {src/adm/template/snd-list.i "clientes"}
  {src/adm/template/snd-list.i "tipo_cobranza"}

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

