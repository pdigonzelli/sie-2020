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
DEFINE VAR v_desde AS DATE.
DEFINE VAR v_hasta AS DATE.

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
&Scoped-define INTERNAL-TABLES orden_entrega agencias tipos_orden_entrega ~
vapores despachantes

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table orden_entrega.id_orden_entrega ~
orden_entrega.fecha agencias.abreviatura vapores.abreviatura ~
tipos_orden_entrega.abreviatura despachantes.descripcion ~
orden_entrega.semana_embarque 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH orden_entrega WHERE ~{&KEY-PHRASE} ~
      AND if (v_desde = ? or v_hasta = ?) then orden_entrega.fecha > today - 60 else (orden_entrega.fecha >= v_desde and orden_entrega.fecha <= v_hasta) NO-LOCK, ~
      EACH agencias WHERE agencias.id_agencia = orden_entrega.id_agencia OUTER-JOIN NO-LOCK, ~
      EACH tipos_orden_entrega OF orden_entrega OUTER-JOIN NO-LOCK, ~
      EACH vapores OF orden_entrega OUTER-JOIN NO-LOCK, ~
      EACH despachantes OF orden_entrega OUTER-JOIN NO-LOCK ~
    BY orden_entrega.fecha DESCENDING ~
       BY orden_entrega.semana_embarque DESCENDING ~
        BY orden_entrega.id_orden_entrega DESCENDING.
&Scoped-define TABLES-IN-QUERY-br_table orden_entrega agencias ~
tipos_orden_entrega vapores despachantes
&Scoped-define FIRST-TABLE-IN-QUERY-br_table orden_entrega
&Scoped-define SECOND-TABLE-IN-QUERY-br_table agencias
&Scoped-define THIRD-TABLE-IN-QUERY-br_table tipos_orden_entrega
&Scoped-define FOURTH-TABLE-IN-QUERY-br_table vapores
&Scoped-define FIFTH-TABLE-IN-QUERY-br_table despachantes


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS desde hasta BUTTON-19 br_table 
&Scoped-Define DISPLAYED-OBJECTS desde hasta 

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
id_tipo_orden_entrega|y|y|general.orden_entrega.id_tipo_orden_entrega
id_despachante||y|general.orden_entrega.id_despachante
id_estado||y|general.orden_entrega.id_estado
id_orden_entrega||y|general.orden_entrega.id_orden_entrega
id_tipo_contrato||y|general.orden_entrega.id_tipo_contrato
id_tipo_plazo||y|general.orden_entrega.id_tipo_plazo
id_tipo_contenedor||y|general.orden_entrega.id_tipo_contenedor
id_tipo_unidad_venta||y|general.orden_entrega.id_tipo_unidad_venta
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "id_tipo_orden_entrega",
     Keys-Supplied = "id_tipo_orden_entrega,id_despachante,id_estado,id_orden_entrega,id_tipo_contrato,id_tipo_plazo,id_tipo_contenedor,id_tipo_unidad_venta"':U).

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
DEFINE BUTTON BUTTON-19 
     LABEL "Aceptar" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE desde AS DATE FORMAT "99/99/99":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE hasta AS DATE FORMAT "99/99/99":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      orden_entrega, 
      agencias, 
      tipos_orden_entrega, 
      vapores, 
      despachantes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      orden_entrega.id_orden_entrega FORMAT ">>>>9":U WIDTH 9.2
      orden_entrega.fecha FORMAT "99/99/99":U WIDTH 12.2
      agencias.abreviatura FORMAT "X(10)":U WIDTH 16.4
      vapores.abreviatura COLUMN-LABEL "Vapor" FORMAT "x(10)":U
            WIDTH 19.2
      tipos_orden_entrega.abreviatura COLUMN-LABEL "TipoOE" FORMAT "X(12)":U
            WIDTH 14
      despachantes.descripcion COLUMN-LABEL "Despachante" FORMAT "X(20)":U
            WIDTH 18.2
      orden_entrega.semana_embarque COLUMN-LABEL "Semana" FORMAT ">9":U
            WIDTH 9.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 118 BY 5.71
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     desde AT ROW 1 COL 7 COLON-ALIGNED
     hasta AT ROW 1 COL 31 COLON-ALIGNED
     BUTTON-19 AT ROW 1 COL 61
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
         HEIGHT             = 6.91
         WIDTH              = 118.
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
/* BROWSE-TAB br_table BUTTON-19 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "general.orden_entrega,industria.agencias WHERE general.orden_entrega ...,industria.tipos_orden_entrega OF general.orden_entrega,comercial.vapores OF general.orden_entrega,industria.despachantes OF general.orden_entrega"
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", OUTER, OUTER, OUTER, OUTER"
     _OrdList          = "general.orden_entrega.fecha|no,general.orden_entrega.semana_embarque|no,general.orden_entrega.id_orden_entrega|no"
     _Where[1]         = "if (v_desde = ? or v_hasta = ?) then orden_entrega.fecha > today - 60 else (orden_entrega.fecha >= v_desde and orden_entrega.fecha <= v_hasta)"
     _JoinCode[2]      = "general.agencias.id_agencia = general.orden_entrega.id_agencia"
     _FldNameList[1]   > general.orden_entrega.id_orden_entrega
"orden_entrega.id_orden_entrega" ? ">>>>9" "integer" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" ""
     _FldNameList[2]   > general.orden_entrega.fecha
"orden_entrega.fecha" ? ? "date" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" ""
     _FldNameList[3]   > general.agencias.abreviatura
"agencias.abreviatura" ? ? "character" ? ? ? ? ? ? no ? no no "16.4" yes no no "U" "" ""
     _FldNameList[4]   > comercial.vapores.abreviatura
"vapores.abreviatura" "Vapor" ? "character" ? ? ? ? ? ? no ? no no "19.2" yes no no "U" "" ""
     _FldNameList[5]   > general.tipos_orden_entrega.abreviatura
"tipos_orden_entrega.abreviatura" "TipoOE" ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[6]   > general.despachantes.descripcion
"despachantes.descripcion" "Despachante" "X(20)" "character" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" ""
     _FldNameList[7]   > general.orden_entrega.semana_embarque
"orden_entrega.semana_embarque" "Semana" ? "integer" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" ""
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


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 B-table-Win
ON CHOOSE OF BUTTON-19 IN FRAME F-Main /* Aceptar */
DO:
  v_desde = DATE(desde:SCREEN-VALUE IN FRAME F-Main).
  v_hasta = DATE(hasta:SCREEN-VALUE IN FRAME F-Main).

  {&OPEN-QUERY-{&BROWSE-NAME}}

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
  menu-item orden_entrega-id_orden_entrega
label 'id_orden_entrega( orden_entrega ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=orden_entrega.id_orden_entrega').
                  run set-attribute-list('titulo=OE').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by orden_entrega.id_orden_entrega
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item orden_entrega-fecha
label 'fecha( orden_entrega ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=orden_entrega.fecha').
                  run set-attribute-list('titulo=Fecha').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by orden_entrega.fecha
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item agencias-abreviatura
label 'abreviatura( agencias ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=agencias.abreviatura').
                  run set-attribute-list('titulo=Agencia').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by agencias.abreviatura
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item vapores-abreviatura
label 'abreviatura( vapores ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=vapores.abreviatura').
                  run set-attribute-list('titulo=Abreviatura').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by vapores.abreviatura
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item tipos_orden_entrega-abreviatura
label 'abreviatura( tipos_orden_entrega ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=tipos_orden_entrega.abreviatura').
                  run set-attribute-list('titulo=Abreviatura').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by tipos_orden_entrega.abreviatura
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item despachantes-descripcion
label 'descripcion( despachantes ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=despachantes.descripcion').
                  run set-attribute-list('titulo=Descripcion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by despachantes.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item orden_entrega-semana_embarque
label 'semana_embarque( orden_entrega ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=orden_entrega.semana_embarque').
                  run set-attribute-list('titulo=Semana embarque').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by orden_entrega.semana_embarque
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by orden_entrega.id_orden_entrega
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=orden_entrega.id_orden_entrega').
run set-attribute-list('titulo=OE').
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
  DEF VAR key-value AS CHAR NO-UNDO.

  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'id_tipo_orden_entrega':U THEN DO:
       &Scope KEY-PHRASE orden_entrega.id_tipo_orden_entrega eq INTEGER(key-value)
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* id_tipo_orden_entrega */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* OTHERWISE...*/
  END CASE.

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
  WHEN 'orden_entrega.id_orden_entrega' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_orden_entrega = integer(valor)  no-lock no-error .
  WHEN 'orden_entrega.fecha' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  fecha = date(valor)  no-lock no-error .
  WHEN 'orden_entrega.semana_embarque' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  semana_embarque = integer(valor)  no-lock no-error .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
/*
  DEFINE VAR r AS ROWID.
RUN get-rowid1 IN h_b_new_orden_entrega (OUTPUT r).
FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = r NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST items_orden_entrega OF orden_entrega
                                   WHERE ITEM_oe = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE items_orden_entrega THEN DO:
        FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
        IF AVAILABLE items_contratos THEN DO:
            IF items_contratos.id_destino <> orden_entrega.id_destino THEN DO:
                MESSAGE "No coinciden el destino de esta OE con la del contrato"
                    VIEW-AS ALERT-BOX.
                MESSAGE "El destino del contrato es " items_contrato.id_destino 
                    VIEW-AS ALERT-BOX.
            END.
                
        END.
    END.
END.

*/
  /* Code placed here will execute PRIOR to standard behavior. */
/* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

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
  {src/adm/template/sndkycas.i "id_tipo_orden_entrega" "orden_entrega" "id_tipo_orden_entrega"}
  {src/adm/template/sndkycas.i "id_despachante" "orden_entrega" "id_despachante"}
  {src/adm/template/sndkycas.i "id_estado" "orden_entrega" "id_estado"}
  {src/adm/template/sndkycas.i "id_orden_entrega" "orden_entrega" "id_orden_entrega"}
  {src/adm/template/sndkycas.i "id_tipo_contrato" "orden_entrega" "id_tipo_contrato"}
  {src/adm/template/sndkycas.i "id_tipo_plazo" "orden_entrega" "id_tipo_plazo"}
  {src/adm/template/sndkycas.i "id_tipo_contenedor" "orden_entrega" "id_tipo_contenedor"}
  {src/adm/template/sndkycas.i "id_tipo_unidad_venta" "orden_entrega" "id_tipo_unidad_venta"}

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
  {src/adm/template/snd-list.i "orden_entrega"}
  {src/adm/template/snd-list.i "agencias"}
  {src/adm/template/snd-list.i "tipos_orden_entrega"}
  {src/adm/template/snd-list.i "vapores"}
  {src/adm/template/snd-list.i "despachantes"}

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

