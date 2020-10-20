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

define var alta as logical initial false.
define var i as integer no-undo.
define var nombre-item as character no-undo.
define var del-emp as integer.
define var del-suc as integer.
define var del-tip as integer.
define var del-nro as integer.
define var del-com as integer.
define var del-fec as date.
define var del-desde as integer.
define var del-hasta as integer.

DEFINE VAR v_borrar AS LOGICAL INITIAL TRUE.

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
&Scoped-define EXTERNAL-TABLES lotes_aceite
&Scoped-define FIRST-EXTERNAL-TABLE lotes_aceite


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lotes_aceite.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES composicion_lote_aceite

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table composicion_lote_aceite.fecha ~
composicion_lote_aceite.cantidad_tambores ~
composicion_lote_aceite.Numeracion_desde ~
composicion_lote_aceite.Numeracion_hasta ~
composicion_lote_aceite.kilos_tambor composicion_lote_aceite.tara 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ~
composicion_lote_aceite.fecha composicion_lote_aceite.cantidad_tambores ~
composicion_lote_aceite.Numeracion_desde ~
composicion_lote_aceite.Numeracion_hasta ~
composicion_lote_aceite.kilos_tambor composicion_lote_aceite.tara 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table composicion_lote_aceite
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table composicion_lote_aceite
&Scoped-define QUERY-STRING-br_table FOR EACH composicion_lote_aceite WHERE composicion_lote_aceite.id_empresa = lotes_aceite.id_empresa ~
  AND composicion_lote_aceite.id_sucursal = lotes_aceite.id_sucursal ~
  AND composicion_lote_aceite.id_tipotambor = lotes_aceite.id_tipotambor ~
  AND composicion_lote_aceite.nromov = lotes_aceite.nromov NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH composicion_lote_aceite WHERE composicion_lote_aceite.id_empresa = lotes_aceite.id_empresa ~
  AND composicion_lote_aceite.id_sucursal = lotes_aceite.id_sucursal ~
  AND composicion_lote_aceite.id_tipotambor = lotes_aceite.id_tipotambor ~
  AND composicion_lote_aceite.nromov = lotes_aceite.nromov NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table composicion_lote_aceite
&Scoped-define FIRST-TABLE-IN-QUERY-br_table composicion_lote_aceite


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
<FOREIGN-KEYS></FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = ':U).

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
      composicion_lote_aceite SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      composicion_lote_aceite.fecha FORMAT "99/99/9999":U
      composicion_lote_aceite.cantidad_tambores COLUMN-LABEL "Tambores" FORMAT ">>>9":U
            WIDTH 11.8
      composicion_lote_aceite.Numeracion_desde COLUMN-LABEL "NumDesde" FORMAT ">>>9":U
            WIDTH 11.6
      composicion_lote_aceite.Numeracion_hasta COLUMN-LABEL "NumHasta" FORMAT ">>>9":U
            WIDTH 12.2
      composicion_lote_aceite.kilos_tambor FORMAT "->>,>>9.99":U
      composicion_lote_aceite.tara FORMAT ">>9.99":U
  ENABLE
      composicion_lote_aceite.fecha
      composicion_lote_aceite.cantidad_tambores
      composicion_lote_aceite.Numeracion_desde
      composicion_lote_aceite.Numeracion_hasta
      composicion_lote_aceite.kilos_tambor
      composicion_lote_aceite.tara
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 105 BY 6.71
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
   External Tables: general.lotes_aceite
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
         HEIGHT             = 6.86
         WIDTH              = 105.
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
     _TblList          = "general.composicion_lote_aceite WHERE general.lotes_aceite ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "general.composicion_lote_aceite.id_empresa = general.lotes_aceite.id_empresa
  AND general.composicion_lote_aceite.id_sucursal = general.lotes_aceite.id_sucursal
  AND general.composicion_lote_aceite.id_tipotambor = general.lotes_aceite.id_tipotambor
  AND general.composicion_lote_aceite.nromov = general.lotes_aceite.nromov"
     _FldNameList[1]   > general.composicion_lote_aceite.fecha
"composicion_lote_aceite.fecha" ? "99/99/9999" "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > general.composicion_lote_aceite.cantidad_tambores
"composicion_lote_aceite.cantidad_tambores" "Tambores" ">>>9" "integer" ? ? ? ? ? ? yes ? no no "11.8" yes no no "U" "" ""
     _FldNameList[3]   > general.composicion_lote_aceite.Numeracion_desde
"composicion_lote_aceite.Numeracion_desde" "NumDesde" ">>>9" "integer" ? ? ? ? ? ? yes ? no no "11.6" yes no no "U" "" ""
     _FldNameList[4]   > general.composicion_lote_aceite.Numeracion_hasta
"composicion_lote_aceite.Numeracion_hasta" "NumHasta" ">>>9" "integer" ? ? ? ? ? ? yes ? no no "12.2" yes no no "U" "" ""
     _FldNameList[5]   > general.composicion_lote_aceite.kilos_tambor
"composicion_lote_aceite.kilos_tambor" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > general.composicion_lote_aceite.tara
"composicion_lote_aceite.tara" ? ">>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
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
  menu-item composicion_lote_aceite-fecha
label 'fecha( composicion_lote_aceite ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=composicion_lote_aceite.fecha').
                  run set-attribute-list('titulo=Fecha').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by composicion_lote_aceite.fecha
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item composicion_lote_aceite-cantidad_tambores
label 'cantidad_tambores( composicion_lote_aceite ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=composicion_lote_aceite.cantidad_tambores').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by composicion_lote_aceite.cantidad_tambores
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item composicion_lote_aceite-Numeracion_desde
label 'Numeracion_desde( composicion_lote_aceite ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=composicion_lote_aceite.Numeracion_desde').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by composicion_lote_aceite.Numeracion_desde
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item composicion_lote_aceite-Numeracion_hasta
label 'Numeracion_hasta( composicion_lote_aceite ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=composicion_lote_aceite.Numeracion_hasta').
                  run set-attribute-list('titulo=Dato').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by composicion_lote_aceite.Numeracion_hasta
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item composicion_lote_aceite-tara
label 'tara( composicion_lote_aceite ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=composicion_lote_aceite.tara').
                  run set-attribute-list('titulo=Tara').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by composicion_lote_aceite.tara
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by composicion_lote_aceite.fecha
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=composicion_lote_aceite.fecha').
run set-attribute-list('titulo=Fecha').
/********** TERMINA MENU ***********/



/**********EMPIEZA-TIPO-DETALLE*********/
  run set-attribute-list ('tipo-detalle=detalle'). 
/**********TERMINA-TIPO-DETALLE*********/

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
define var hcontainer as handle.
define var r as rowid.
DEFINE VAR r_lotes_ind AS ROWID.
define buffer comp for composicion_lote_aceite.
define var ultimo1 as integer.
define var ultimo as integer.
define var cant_tambores as integer.
define var i as integer.
define var desde as integer.
define var hasta as integer.
define var fecha_ela as date.
define var tara as decimal.
define var v_kilos_tambor as decimal.
DEFINE BUFFER bb_tam FOR tambores_industria.



tara             = DECIMAL(composicion_lote_aceite.tara:screen-value in browse {&BROWSE-NAME}).
cant_tambores    = INTEGER(composicion_lote_aceite.cantidad_tambores:screen-value in browse {&BROWSE-NAME}).
desde            = INTEGER(composicion_lote_aceite.numeracion_desde:screen-value in browse {&BROWSE-NAME}).
hasta            = INTEGER(composicion_lote_aceite.numeracion_hasta:screen-value in browse {&BROWSE-NAME}).
fecha_ela        = DATE(composicion_lote_aceite.fecha:screen-value in browse {&BROWSE-NAME}).
v_kilos_tambor   = DECIMAL(composicion_lote_aceite.kilos_tambor:screen-value in browse {&BROWSE-NAME}).

RUN get-container (output hcontainer).
RUN get-rowid-lotes_aceite in hcontainer (output r).
RUN get-rowid-lotes_ind in hcontainer (output r_lotes_ind).
FIND FIRST lotes_ind WHERE ROWID(lotes_ind) = r_lotes_ind NO-LOCK NO-ERROR.

FIND FIRST lotes_aceite WHERE ROWID(lotes_aceite) = r NO-ERROR.
IF AVAILABLE lotes_aceite THEN DO:
    FIND FIRST bb_tam WHERE bb_tam.id_locacion_ubicacion    = 4
                        AND bb_tam.id_sucursal              = lotes_aceite.id_sucursal
                        AND bb_tam.id_articulo              = lotes_aceite.id_articulo
                        AND bb_tam.id_lote                  = lotes_aceite.id_lote
                        AND bb_tam.id_tipotambor            = lotes_aceite.id_tipotambor
                        AND bb_tam.anio                     = lotes_aceite.anio
  /*by facundo*/        AND bb_tam.nromov                   = lotes_aceite.nromov
                        AND bb_tam.id_tambor                >= desde
                        AND bb_tam.id_tambor                <= hasta
                        NO-LOCK NO-ERROR.
    IF AVAILABLE bb_tam THEN DO:
        MESSAGE "Ya existe el tambor " bb_tam.id_tambor
                " de la sucursal " lotes_aceite.id_sucursal
                " del lote " lotes_aceite.id_lote
                " con articulo " lotes_aceite.id_articulo
                " del año " lotes_aceite.anio VIEW-AS ALERT-BOX.
        MESSAGE "Si tiene alguna duda, comunicarse con Sistemas" VIEW-AS ALERT-BOX.
        RETURN "ADM-ERROR".
    END.
    ELSE DO:
        FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = lotes_aceite.id_articulo
                                       NO-LOCK NO-ERROR.
        IF lotes_aceite.activo THEN DO:
                
            ASSIGN  composicion_lote_aceite.id_empresa       = lotes_aceite.id_empresa
                    composicion_lote_aceite.id_sucursal      = lotes_aceite.id_sucursal
                    composicion_lote_aceite.id_lote          = lotes_aceite.id_lote
                    composicion_lote_aceite.id_tipotambor    = lotes_aceite.id_tipotambor
                    composicion_lote_aceite.nromov           = lotes_aceite.nromov
                    composicion_lote_aceite.id_composicion   = NEXT-VALUE(composicion_lote)
                    composicion_lote_aceite.c_usuario        = USERID("userdb")
                    composicion_lote_aceite.c_fecha          = TODAY
                    composicion_lote_aceite.c_hora           = STRING(TIME,"HH:MM:SS").
                
            ASSIGN lotes_aceite.estado_lote = 2.
                     
            /******************CREO LOS REGISTROS PARA LA TABLA TAMBORES_LOTE_ACEITE***************/
                
            DO i = desde TO hasta:
                 FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = lotes_aceite.id_empresa 
                                                 AND tambores_industria.id_sucursal   = lotes_aceite.id_sucursal 
                                                 AND tambores_industria.id_lote       = lotes_aceite.id_lote 
                                                 AND tambores_industria.id_articulo   = lotes_aceite.id_articulo 
                                                 AND tambores_industria.id_tipotambor = lotes_aceite.id_tipotambor 
                                                 AND tambores_industria.nromov        = lotes_aceite.nromov 
                                                 AND tambores_industria.id_tambor     = i   /*ultimo*/ 
                                                 NO-ERROR.
                    
                 IF AVAILABLE tambores_industria THEN DELETE tambores_industria.
                 CREATE tambores_industria.
                 ASSIGN tambores_industria.id_empresa    = lotes_aceite.id_empresa
                        tambores_industria.id_sucursal   = lotes_aceite.id_sucursal
                        tambores_industria.id_lote       = lotes_aceite.id_lote
                        tambores_industria.id_articulo   = lotes_aceite.id_articulo
                        tambores_industria.id_tipotambor = lotes_aceite.id_tipotambor
                        tambores_industria.nromov        = lotes_aceite.nromov
                        tambores_industria.id_tambor     = i   /*ultimo*/
                        tambores_industria.fecha         = fecha_ela
                        tambores_industria.anio          = IF AVAILABLE lotes_ind THEN lotes_ind.anio ELSE YEAR(fecha_ela)
                        tambores_industria.id_envase     = lotes_aceite.id_envase
                        tambores_industria.id_etiqueta   = IF lotes_aceite.id_sucursal = 96 THEN NEXT-VALUE(tambores)
                                                                                               ELSE NEXT-VALUE(tambores_famailla)
                        tambores_industria.tara          = tara
                        tambores_industria.c_usuario     = USERID("userdb")
                        tambores_industria.c_fecha       = TODAY
                        tambores_industria.c_hora        = STRING(TIME,"HH:MM:SS")
                        tambores_industria.kilos_tambor  = v_kilos_tambor
                        tambores_industria.id_empresa_ubicacion  = 1
                        tambores_industria.id_sucursal_ubicacion = lotes_aceite.id_sucursal
                        tambores_industria.id_locacion_ubicacion = 4
                        tambores_industria.id_posicion_ubicacion = 1
                        tambores_industria.id_calidad            = IF AVAILABLE r_productos_calidad THEN r_productos_calidad.id_calidad
                                                                                                       ELSE 602.
                                              
            END.
            RUN y_gstkcre.p (input lotes_aceite.id_empresa,
                             input lotes_aceite.id_sucursal,
                             input lotes_aceite.id_tipotambor,
                             input lotes_aceite.nromov,
                             INPUT desde,
                             INPUT hasta,
                             input 1) "lotes_aceite".
            
            IF RETURN-VALUE <> "" THEN DO:
                MESSAGE "Error en el procesamiento de movimientos de stock" VIEW-AS ALERT-BOX.
                RETURN "error".
            END.
        END.
        ELSE DO:
            MESSAGE "No se puede modificar un lote viejo" VIEW-AS ALERT-BOX.
            RETURN "ADM-ERROR".
            RUN notify ('cancel-record':U). 
        END.
        
        FIND FIRST tambores_industria WHERE tambores_industria.id_empresa = lotes_aceite.id_empresa
                                        AND tambores_industria.id_sucursal = lotes_aceite.id_sucursal
                                        AND tambores_industria.id_tipotambor = lotes_aceite.id_tipotambor
                                        AND tambores_industria.nromov = lotes_aceite.nromov
                                        AND tambores_industria.id_tambor = 0
                                        NO-ERROR.
            
        IF AVAILABLE tambores_industria THEN DO:
            DELETE tambores_industria.
        END.
    END.
END.
ELSE DO: 
    MESSAGE "Hubo un error en la grabación de los tambores" VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.

alta = TRUE.
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
IF v_borrar THEN DO:
    
    for each tambores_industria where tambores_industria.id_empresa       = del-emp
                                  and tambores_industria.id_sucursal      = del-suc
                                  and tambores_industria.id_tipotambor    = del-tip
                                  and tambores_industria.nromov           = del-nro
                                  and tambores_industria.id_tambor        >= del-desde
                                  and tambores_industria.id_tambor        <= del-hasta.
        delete tambores_industria.
    end.                               
    
END. 
ELSE DO:
    MESSAGE "No se borraron los tambores" VIEW-AS ALERT-BOX.
    RETURN "error".
END.
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
define var r as rowid.
define var hcontainer as handle.
define var existe as logical.


if not alta then do:

    run get-container (output hcontainer).
    RUN get-rowid-lotes_aceite in hcontainer (output r).
    find lotes_aceite where rowid(lotes_aceite) = r no-lock no-error.
    
    if available lotes_aceite then
        do:
            if not lotes_aceite.activo then do:
                message "No se puede modificar un lote viejo" view-as alert-box.
                return "ADM-ERROR".
                RUN notify ('cancel-record':U).
                RUN notify ('reset-record':U). 
            end.
        end.

end.

alta = false.
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
define var r as rowid.
define var r1 as rowid.
define var hcontainer as handle.
define var existe as logical.

run get-container (output hcontainer).
RUN get-rowid-lotes_aceite in hcontainer (output r1).
FIND FIRST lotes_aceite where rowid(lotes_aceite) = r1 no-lock no-error.

v_borrar = TRUE.
IF AVAILABLE lotes_aceite THEN DO:
    IF lotes_aceite.activo THEN DO:
        RUN get-rowid1 (output r).
        FIND FIRST composicion_lote_aceite where rowid(composicion_lote_aceite) = r.
        IF AVAILABLE composicion_lote_aceite THEN DO:
            del-emp = composicion_lote_aceite.id_empresa.
            del-suc = composicion_lote_aceite.id_sucursal.
            del-tip = composicion_lote_aceite.id_tipotambor.
            del-nro = composicion_lote_aceite.nromov.    
            del-com = composicion_lote_aceite.id_composicion.
            del-fec = composicion_lote_aceite.fecha.
            del-desde = composicion_lote_aceite.Numeracion_desde.
            del-hasta = composicion_lote_aceite.Numeracion_hasta.

            FOR EACH tambores_industria OF lotes_aceite 
                                       WHERE tambores_industria.id_tambor >= del-desde
                                         AND tambores_industria.id_tambor <= del-hasta
                                         AND tambores_industria.id_sucursal_ubicacion <> 95
                                         AND tambores_industria.id_sucursal_ubicacion <> 96
                                        NO-LOCK.
                v_borrar = FALSE.
                CASE tambores_industria.id_sucursal_ubicacion:
                    WHEN 85 THEN DO:
                        MESSAGE "El tambor " tambores_industria.id_tambor " ya fue despachado."
                                VIEW-AS ALERT-BOX.
                    END.
                    WHEN 91 THEN DO:
                        MESSAGE "El tambor " tambores_industria.id_tambor " ya fue facturado."
                                VIEW-AS ALERT-BOX.
                    END.
                    OTHERWISE DO:
                        MESSAGE "El tambor " tambores_industria.id_tambor " esta en un deposito externo."
                                VIEW-AS ALERT-BOX.
                    END.
                END CASE.
            END.
            IF v_borrar THEN DO:
                RUN y_gstkcre.p (input composicion_lote_aceite.id_empresa,
                                 input composicion_lote_aceite.id_sucursal,
                                 input composicion_lote_aceite.id_tipotambor,
                                 input composicion_lote_aceite.nromov,
                                 INPUT composicion_lote_aceite.Numeracion_desde,
                                 INPUT composicion_lote_aceite.Numeracion_hasta,
                                 input 2) "lotes_aceite".
                
                IF return-value <> "" then do:
                    MESSAGE "Error en el procesamiento de movimientos de stock" view-as alert-box.
                    RETURN "error".
                    UNDO, LEAVE.
                END.
            END.
            ELSE DO:
                MESSAGE "Se cancela el proceso de borrado" VIEW-AS ALERT-BOX.
                RETURN "error".
                UNDO, LEAVE.
            END.
        END.
        ELSE DO: 
            MESSAGE "No se encontro el registro de Composicion de Lote de Aceite"
                    VIEW-AS ALERT-BOX.
            RETURN "error".
        END.                 
    END.
    ELSE DO:
        MESSAGE "No puede borrar el lote porque el sobrante del mismo esta siendo utilizado"
                     view-as alert-box.
        RETURN "error". 
    END.
END.
ELSE DO:
    MESSAGE "No se encontro el lote de los tambores" VIEW-AS ALERT-BOX.
    RETURN "error".
END.

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
define var r as rowid.
define var hcontainer as handle.
define var existe as logical.
/*
run get-container (output hcontainer).
RUN get-rowid-lotes_aceite in hcontainer (output r).
find lotes_aceite where rowid(lotes_aceite) = r no-lock no-error.

if available lotes_aceite then
    do:
        if not lotes_aceite.activo then do:
            message "No se puede modificar un lote viejo" view-as alert-box.
            /* return "ADM-ERROR". */
            RUN notify ('cancel-record':U).
            RUN notify ('reset-record':U). 
        end.
    end.
*/    
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
  {src/adm/template/row-list.i "lotes_aceite"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "lotes_aceite"}

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
  WHEN 'composicion_lote_aceite.fecha' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  fecha = date(valor)  no-lock no-error .
  WHEN 'composicion_lote_aceite.cantidad_tambores' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  cantidad_tambores = integer(valor)  no-lock no-error .
  WHEN 'composicion_lote_aceite.Numeracion_desde' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Numeracion_desde = integer(valor)  no-lock no-error .
  WHEN 'composicion_lote_aceite.Numeracion_hasta' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  Numeracion_hasta = integer(valor)  no-lock no-error .
  WHEN 'composicion_lote_aceite.tara' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  tara = decimal(valor)  no-lock no-error .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilitar_relacion B-table-Win 
PROCEDURE habilitar_relacion :
define var field-group as handle.
define var cur-control as handle.
define var lista_relacion as character no-undo initial "id_envase".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* There are no foreign keys supplied by this SmartObject. */

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
  {src/adm/template/snd-list.i "lotes_aceite"}
  {src/adm/template/snd-list.i "composicion_lote_aceite"}

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

