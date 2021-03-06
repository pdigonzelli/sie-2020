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
define var articulo as integer.
define var fecha as date.
DEFINE VAR sucursal AS INTEGER.
define var v_tambor_desde as integer.
define var v_tambor_hasta as integer.
DEFINE VAR v_art AS INTEGER.
DEFINE VAR v_lote AS INTEGER.

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
&Scoped-define INTERNAL-TABLES tambores_industria productos_terminados ~
origenes_materia_prima

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tambores_industria.id_lote ~
tambores_industria.Anio tambores_industria.id_articulo ~
productos_terminados.descripcion tambores_industria.id_tambor ~
tambores_industria.kilos_tambor tambores_industria.Fecha_cierre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH tambores_industria WHERE ~{&KEY-PHRASE} ~
      AND tambores_industria.id_empresa_destino = 0 ~
 AND tambores_industria.id_sucursal_destino = 0 ~
 AND tambores_industria.id_tipotambor_destino = 0 ~
 AND tambores_industria.nromov_destino = 0 ~
 AND tambores_industria.Fecha_cierre <= fecha ~
 AND tambores_industria.id_locacion_ubicacion = 4 ~
 AND tambores_industria.id_sucursal_ubicacion = sucursal ~
AND (if v_tambor_desde = 0 or v_tambor_hasta = 0 then true else tambores_industria.id_tambor >= v_tambor_desde) ~
AND (if v_tambor_desde = 0 or v_tambor_hasta = 0 then true else tambores_industria.id_tambor <= v_tambor_hasta) ~
AND (if v_art <> 0 then tambores_industria.id_articulo = v_art else true) ~
AND (if v_lote <> 0 then tambores_industria.id_lote = v_lote else true) NO-LOCK, ~
      EACH productos_terminados OF tambores_industria NO-LOCK, ~
      EACH origenes_materia_prima WHERE origenes_materia_prima.id_articulo_mp = tambores_industria.id_articulo ~
      AND origenes_materia_prima.id_articulo_lote = articulo NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH tambores_industria WHERE ~{&KEY-PHRASE} ~
      AND tambores_industria.id_empresa_destino = 0 ~
 AND tambores_industria.id_sucursal_destino = 0 ~
 AND tambores_industria.id_tipotambor_destino = 0 ~
 AND tambores_industria.nromov_destino = 0 ~
 AND tambores_industria.Fecha_cierre <= fecha ~
 AND tambores_industria.id_locacion_ubicacion = 4 ~
 AND tambores_industria.id_sucursal_ubicacion = sucursal ~
AND (if v_tambor_desde = 0 or v_tambor_hasta = 0 then true else tambores_industria.id_tambor >= v_tambor_desde) ~
AND (if v_tambor_desde = 0 or v_tambor_hasta = 0 then true else tambores_industria.id_tambor <= v_tambor_hasta) ~
AND (if v_art <> 0 then tambores_industria.id_articulo = v_art else true) ~
AND (if v_lote <> 0 then tambores_industria.id_lote = v_lote else true) NO-LOCK, ~
      EACH productos_terminados OF tambores_industria NO-LOCK, ~
      EACH origenes_materia_prima WHERE origenes_materia_prima.id_articulo_mp = tambores_industria.id_articulo ~
      AND origenes_materia_prima.id_articulo_lote = articulo NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tambores_industria ~
productos_terminados origenes_materia_prima
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tambores_industria
&Scoped-define SECOND-TABLE-IN-QUERY-br_table productos_terminados
&Scoped-define THIRD-TABLE-IN-QUERY-br_table origenes_materia_prima


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS art lote tambor_desde tambor_hasta BUTTON-13 ~
br_table 
&Scoped-Define DISPLAYED-OBJECTS art lote tambor_desde tambor_hasta 

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
id_envase||y|general.tambores_produccion_jugo.id_envase
id_sucursal||y|general.tambores_produccion_jugo.id_sucursal
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_envase,id_sucursal"':U).

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
DEFINE BUTTON BUTTON-13 
     LABEL "Aceptar" 
     SIZE 15 BY .95.

DEFINE VARIABLE art AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lote AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE tambor_desde AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Desde Tambor" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE tambor_hasta AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "hasta" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tambores_industria, 
      productos_terminados, 
      origenes_materia_prima SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      tambores_industria.id_lote FORMAT ">>>9":U WIDTH 9.2
      tambores_industria.Anio FORMAT ">>>9":U WIDTH 9.2
      tambores_industria.id_articulo COLUMN-LABEL "Art." FORMAT ">>9":U
            WIDTH 6.8
      productos_terminados.descripcion COLUMN-LABEL "Producto" FORMAT "x(25)":U
      tambores_industria.id_tambor FORMAT ">>>9":U WIDTH 12.8
      tambores_industria.kilos_tambor FORMAT ">>>>9.99":U WIDTH 12.2
      tambores_industria.Fecha_cierre COLUMN-LABEL "Fecha" FORMAT "99/99/99":U
            WIDTH 11.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 155 BY 15.14
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     art AT ROW 1 COL 11 COLON-ALIGNED
     lote AT ROW 1 COL 27 COLON-ALIGNED
     tambor_desde AT ROW 1 COL 52 COLON-ALIGNED
     tambor_hasta AT ROW 1 COL 67 COLON-ALIGNED
     BUTTON-13 AT ROW 1 COL 82
     br_table AT ROW 2.19 COL 1
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
         HEIGHT             = 16.52
         WIDTH              = 155.2.
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
/* BROWSE-TAB br_table BUTTON-13 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "general.tambores_industria,general.productos_terminados OF general.tambores_industria,industria.origenes_materia_prima WHERE general.tambores_industria ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ",,"
     _Where[1]         = "general.tambores_industria.id_empresa_destino = 0
 AND general.tambores_industria.id_sucursal_destino = 0
 AND general.tambores_industria.id_tipotambor_destino = 0
 AND general.tambores_industria.nromov_destino = 0
 AND general.tambores_industria.Fecha_cierre <= fecha
 AND general.tambores_industria.id_locacion_ubicacion = 4
 AND general.tambores_industria.id_sucursal_ubicacion = sucursal
AND (if v_tambor_desde = 0 or v_tambor_hasta = 0 then true else general.tambores_industria.id_tambor >= v_tambor_desde)
AND (if v_tambor_desde = 0 or v_tambor_hasta = 0 then true else general.tambores_industria.id_tambor <= v_tambor_hasta)
AND (if v_art <> 0 then tambores_industria.id_articulo = v_art else true)
AND (if v_lote <> 0 then tambores_industria.id_lote = v_lote else true)"
     _JoinCode[3]      = "general.origenes_materia_prima.id_articulo_mp = general.tambores_industria.id_articulo"
     _Where[3]         = "general.origenes_materia_prima.id_articulo_lote = articulo"
     _FldNameList[1]   > general.tambores_industria.id_lote
"general.tambores_industria.id_lote" ? ">>>9" "integer" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" ""
     _FldNameList[2]   > general.tambores_industria.Anio
"general.tambores_industria.Anio" ? ">>>9" "integer" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" ""
     _FldNameList[3]   > general.tambores_industria.id_articulo
"general.tambores_industria.id_articulo" "Art." ">>9" "integer" ? ? ? ? ? ? no ? no no "6.8" yes no no "U" "" ""
     _FldNameList[4]   > general.productos_terminados.descripcion
"general.productos_terminados.descripcion" "Producto" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > general.tambores_industria.id_tambor
"general.tambores_industria.id_tambor" ? ">>>9" "integer" ? ? ? ? ? ? no ? no no "12.8" yes no no "U" "" ""
     _FldNameList[6]   > general.tambores_industria.kilos_tambor
"general.tambores_industria.kilos_tambor" ? ">>>>9.99" "decimal" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" ""
     _FldNameList[7]   > general.tambores_industria.Fecha_cierre
"general.tambores_industria.Fecha_cierre" "Fecha" "99/99/99" "date" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" ""
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


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 B-table-Win
ON CHOOSE OF BUTTON-13 IN FRAME F-Main /* Aceptar */
DO:
  v_tambor_desde = integer(tambor_desde:screen-value in frame F-Main).
  v_tambor_hasta = integer(tambor_hasta:screen-value in frame F-Main).
  v_art = integer(art:screen-value in frame F-Main).
  v_lote = integer(lote:screen-value in frame F-Main).
  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
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
  menu-item tambores_industria-id_lote
label 'id_lote( tambores_industria ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tambores_industria.id_lote').
                  run set-attribute-list('titulo=Lote').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by tambores_industria.id_lote
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item tambores_industria-Anio
label 'Anio( tambores_industria ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tambores_industria.Anio').
                  run set-attribute-list('titulo=A�o').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by tambores_industria.Anio
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item tambores_industria-id_articulo
label 'id_articulo( tambores_industria ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tambores_industria.id_articulo').
                  run set-attribute-list('titulo=Art�culo').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by tambores_industria.id_articulo
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item productos_terminados-descripcion
label 'descripcion( productos_terminados ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=productos_terminados.descripcion').
                  run set-attribute-list('titulo=Descripci�n').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by productos_terminados.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item tambores_industria-id_tambor
label 'id_tambor( tambores_industria ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tambores_industria.id_tambor').
                  run set-attribute-list('titulo=Tambor').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by tambores_industria.id_tambor
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item tambores_industria-kilos_tambor
label 'kilos_tambor( tambores_industria ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tambores_industria.kilos_tambor').
                  run set-attribute-list('titulo=Kilos tambor').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by tambores_industria.kilos_tambor
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item tambores_industria-Fecha_cierre
label 'Fecha_cierre( tambores_industria ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tambores_industria.Fecha_cierre').
                  run set-attribute-list('titulo=Fecha Cierre').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by tambores_industria.Fecha_cierre
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by tambores_industria.id_lote
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=tambores_industria.id_lote').
run set-attribute-list('titulo=Lote').
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
  WHEN 'tambores_industria.id_lote' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_lote = integer(valor)  no-lock no-error .
  WHEN 'tambores_industria.Anio' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Anio = integer(valor)  no-lock no-error .
  WHEN 'tambores_industria.id_articulo' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_articulo = integer(valor)  no-lock no-error .
  WHEN 'tambores_industria.id_tambor' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_tambor = integer(valor)  no-lock no-error .
  WHEN 'tambores_industria.kilos_tambor' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  kilos_tambor = decimal(valor)  no-lock no-error .
  WHEN 'tambores_industria.Fecha_cierre' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Fecha_cierre = date(valor)  no-lock no-error .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-seleccion B-table-Win 
PROCEDURE get-seleccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter elegidos as char.
define output parameter num as integer.
define var i as integer no-undo.
num = BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS.

  do i = 1 to BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS:
    BROWSE {&BROWSE-NAME}:FETCH-SELECTED-ROW(I).
    elegidos = elegidos + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}})) + ",".
  END.
  
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

define var hcontainer as handle.
run get-container (output hcontainer).
run get-articulos in hcontainer (output articulo, 
                                 output fecha,
                                 OUTPUT sucursal).
/* message articulo fecha. */


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .



run get-container (output hcontainer).
run get-articulos in hcontainer (output articulo, 
                                 output fecha,
                                 OUTPUT sucursal).
/* message articulo fecha. */

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/sndkycas.i "id_envase" "tambores_produccion_jugo" "id_envase"}
  {src/adm/template/sndkycas.i "id_sucursal" "tambores_produccion_jugo" "id_sucursal"}

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
  {src/adm/template/snd-list.i "tambores_industria"}
  {src/adm/template/snd-list.i "productos_terminados"}
  {src/adm/template/snd-list.i "origenes_materia_prima"}

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

