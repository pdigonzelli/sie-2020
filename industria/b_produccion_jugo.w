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
define var h_con as handle.

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
&Scoped-define INTERNAL-TABLES produccion_jugo productos_terminados

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table produccion_jugo.id_produccion ~
productos_terminados.descripcion produccion_jugo.Fecha ~
produccion_jugo.Bx_20_20 produccion_jugo.Bx_correg ~
produccion_jugo.Acidez_w_w produccion_jugo.Acidez_w_v produccion_jugo.Sodio ~
produccion_jugo.Pulpa produccion_jugo.cantidad_1 produccion_jugo.c_usuario ~
produccion_jugo.c_fecha produccion_jugo.c_hora produccion_jugo.nitrogeno ~
produccion_jugo.nromov produccion_jugo.pulpa_85 produccion_jugo.ratio ~
produccion_jugo.t_600 produccion_jugo.unidad_medida ~
produccion_jugo.vitaminac 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH produccion_jugo WHERE ~{&KEY-PHRASE} ~
      AND produccion_jugo.Fecha >= date("01/01/2002") ~
 AND produccion_jugo.id_tipotambor = 1 NO-LOCK, ~
      EACH productos_terminados WHERE productos_terminados.id_articulo = produccion_jugo.id_articulo NO-LOCK ~
    BY produccion_jugo.Fecha DESCENDING ~
       BY produccion_jugo.id_produccion DESCENDING
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH produccion_jugo WHERE ~{&KEY-PHRASE} ~
      AND produccion_jugo.Fecha >= date("01/01/2002") ~
 AND produccion_jugo.id_tipotambor = 1 NO-LOCK, ~
      EACH productos_terminados WHERE productos_terminados.id_articulo = produccion_jugo.id_articulo NO-LOCK ~
    BY produccion_jugo.Fecha DESCENDING ~
       BY produccion_jugo.id_produccion DESCENDING.
&Scoped-define TABLES-IN-QUERY-br_table produccion_jugo ~
productos_terminados
&Scoped-define FIRST-TABLE-IN-QUERY-br_table produccion_jugo
&Scoped-define SECOND-TABLE-IN-QUERY-br_table productos_terminados


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
id_sucursal||y|general.produccion_jugo.id_sucursal
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
      produccion_jugo, 
      productos_terminados SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      produccion_jugo.id_produccion COLUMN-LABEL "Prod." FORMAT ">>>,>>9":U
      productos_terminados.descripcion FORMAT "x(20)":U
      produccion_jugo.Fecha FORMAT "99/99/9999":U
      produccion_jugo.Bx_20_20 FORMAT ">,>>9.99":U
      produccion_jugo.Bx_correg FORMAT ">,>>9.99":U
      produccion_jugo.Acidez_w_w FORMAT "->>,>>9.99":U
      produccion_jugo.Acidez_w_v FORMAT "->>,>>9.99":U
      produccion_jugo.Sodio FORMAT ">,>>9.99":U
      produccion_jugo.Pulpa FORMAT ">>9.99":U
      produccion_jugo.cantidad_1 FORMAT ">,>>9":U
      produccion_jugo.c_usuario FORMAT "x(12)":U
      produccion_jugo.c_fecha FORMAT "99/99/99":U
      produccion_jugo.c_hora FORMAT "x(8)":U
      produccion_jugo.nitrogeno FORMAT "->>,>>9.99":U
      produccion_jugo.nromov FORMAT ">>>,>>>,>>9":U
      produccion_jugo.pulpa_85 FORMAT "->>,>>9.99":U
      produccion_jugo.ratio FORMAT "->>,>>9.99":U
      produccion_jugo.t_600 FORMAT "->>,>>9.99":U
      produccion_jugo.unidad_medida FORMAT ">>9":U
      produccion_jugo.vitaminac FORMAT "->>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 151 BY 5.71
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
         HEIGHT             = 5.71
         WIDTH              = 151.4.
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
     _TblList          = "general.produccion_jugo,general.productos_terminados WHERE general.produccion_jugo ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "general.produccion_jugo.Fecha|no,industria.produccion_jugo.id_produccion|no"
     _Where[1]         = "general.produccion_jugo.Fecha >= date(""01/01/2002"")
 AND general.produccion_jugo.id_tipotambor = 1"
     _JoinCode[2]      = "general.productos_terminados.id_articulo = general.produccion_jugo.id_articulo"
     _FldNameList[1]   > general.produccion_jugo.id_produccion
"produccion_jugo.id_produccion" "Prod." ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > general.productos_terminados.descripcion
"productos_terminados.descripcion" ? "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = general.produccion_jugo.Fecha
     _FldNameList[4]   > general.produccion_jugo.Bx_20_20
"produccion_jugo.Bx_20_20" ? ">,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > general.produccion_jugo.Bx_correg
"produccion_jugo.Bx_correg" ? ">,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   = general.produccion_jugo.Acidez_w_w
     _FldNameList[7]   = general.produccion_jugo.Acidez_w_v
     _FldNameList[8]   > general.produccion_jugo.Sodio
"produccion_jugo.Sodio" ? ">,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > general.produccion_jugo.Pulpa
"produccion_jugo.Pulpa" ? ">>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   = general.produccion_jugo.cantidad_1
     _FldNameList[11]   = general.produccion_jugo.c_usuario
     _FldNameList[12]   = general.produccion_jugo.c_fecha
     _FldNameList[13]   = general.produccion_jugo.c_hora
     _FldNameList[14]   = general.produccion_jugo.nitrogeno
     _FldNameList[15]   = general.produccion_jugo.nromov
     _FldNameList[16]   = general.produccion_jugo.pulpa_85
     _FldNameList[17]   = general.produccion_jugo.ratio
     _FldNameList[18]   = general.produccion_jugo.t_600
     _FldNameList[19]   = general.produccion_jugo.unidad_medida
     _FldNameList[20]   = general.produccion_jugo.vitaminac
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
     
/*
run get-container(output h_con).
run actualiza-origen-mp in h_con.
*/
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
  menu-item produccion_jugo-id_produccion
label 'id_produccion( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.id_produccion').
                  run set-attribute-list('titulo=Produccion').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.id_produccion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item productos_terminados-descripcion
label 'descripcion( productos_terminados ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=productos_terminados.descripcion').
                  run set-attribute-list('titulo=Descripción').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by productos_terminados.descripcion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item produccion_jugo-Fecha
label 'Fecha( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.Fecha').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.Fecha
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item produccion_jugo-Bx_20_20
label 'Bx_20_20( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.Bx_20_20').
                  run set-attribute-list('titulo=Bx_20/20').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.Bx_20_20
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item produccion_jugo-Bx_correg
label 'Bx_correg( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.Bx_correg').
                  run set-attribute-list('titulo=Bx correg').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.Bx_correg
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item produccion_jugo-Acidez_w_w
label 'Acidez_w_w( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.Acidez_w_w').
                  run set-attribute-list('titulo=Acidez_p/p').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.Acidez_w_w
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item produccion_jugo-Acidez_w_v
label 'Acidez_w_v( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.Acidez_w_v').
                  run set-attribute-list('titulo=Acidez_p/v').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.Acidez_w_v
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item produccion_jugo-Sodio
label 'Sodio( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.Sodio').
                  run set-attribute-list('titulo=Na').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.Sodio
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item produccion_jugo-Pulpa
label 'Pulpa( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.Pulpa').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.Pulpa
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item produccion_jugo-cantidad_1
label 'cantidad_1( produccion_jugo ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=produccion_jugo.cantidad_1').
                  run set-attribute-list('titulo=Cantidad').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by produccion_jugo.cantidad_1
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by produccion_jugo.id_produccion
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=produccion_jugo.id_produccion').
run set-attribute-list('titulo=Produccion').
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
  WHEN 'produccion_jugo.id_produccion' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_produccion = integer(valor)  no-lock no-error .
  WHEN 'produccion_jugo.Fecha' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Fecha = date(valor)  no-lock no-error .
  WHEN 'produccion_jugo.Bx_20_20' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Bx_20_20 = decimal(valor)  no-lock no-error .
  WHEN 'produccion_jugo.Bx_correg' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Bx_correg = decimal(valor)  no-lock no-error .
  WHEN 'produccion_jugo.Acidez_w_w' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Acidez_w_w = decimal(valor)  no-lock no-error .
  WHEN 'produccion_jugo.Acidez_w_v' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Acidez_w_v = decimal(valor)  no-lock no-error .
  WHEN 'produccion_jugo.Sodio' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Sodio = decimal(valor)  no-lock no-error .
  WHEN 'produccion_jugo.Pulpa' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Pulpa = decimal(valor)  no-lock no-error .
  WHEN 'produccion_jugo.cantidad_1' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  cantidad_1 = integer(valor)  no-lock no-error .
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
  {src/adm/template/sndkycas.i "id_sucursal" "produccion_jugo" "id_sucursal"}

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
  {src/adm/template/snd-list.i "produccion_jugo"}
  {src/adm/template/snd-list.i "productos_terminados"}

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

