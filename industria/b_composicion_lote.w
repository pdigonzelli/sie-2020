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
define var empresa as integer.
define var sucursal as integer.
define var tipotambor as integer.
define var nromov as integer.
define var fecha as date.
DEFINE VAR v_desde AS INTEGER.
DEFINE VAR v_hasta AS INTEGER.
define var mi-alta as logical initial false.

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES lotes_jugo
&Scoped-define FIRST-EXTERNAL-TABLE lotes_jugo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lotes_jugo.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES composicion_lote

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table composicion_lote.Fecha ~
composicion_lote.cantidad_tambores composicion_lote.Numeracion_desde ~
composicion_lote.Numeracion_hasta 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table composicion_lote.Fecha ~
composicion_lote.cantidad_tambores composicion_lote.Numeracion_desde ~
composicion_lote.Numeracion_hasta 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table composicion_lote
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table composicion_lote
&Scoped-define QUERY-STRING-br_table FOR EACH composicion_lote WHERE composicion_lote.id_empresa = lotes_jugo.id_empresa ~
  AND composicion_lote.id_sucursal = lotes_jugo.id_sucursal ~
  AND composicion_lote.id_tipotambor = lotes_jugo.id_tipotambor ~
  AND composicion_lote.nromov = lotes_jugo.nromov NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH composicion_lote WHERE composicion_lote.id_empresa = lotes_jugo.id_empresa ~
  AND composicion_lote.id_sucursal = lotes_jugo.id_sucursal ~
  AND composicion_lote.id_tipotambor = lotes_jugo.id_tipotambor ~
  AND composicion_lote.nromov = lotes_jugo.nromov NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table composicion_lote
&Scoped-define FIRST-TABLE-IN-QUERY-br_table composicion_lote


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
id_sucursal||y|general.composicion_lote.id_sucursal
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
      composicion_lote SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      composicion_lote.Fecha FORMAT "99/99/9999":U WIDTH 14.2
      composicion_lote.cantidad_tambores COLUMN-LABEL "Cantidad" FORMAT ">>>9":U
            WIDTH 12.2
      composicion_lote.Numeracion_desde COLUMN-LABEL "Desde" FORMAT ">>9":U
            WIDTH 8.2
      composicion_lote.Numeracion_hasta COLUMN-LABEL "Hasta" FORMAT ">>>9":U
            WIDTH 10.2
  ENABLE
      composicion_lote.Fecha
      composicion_lote.cantidad_tambores
      composicion_lote.Numeracion_desde
      composicion_lote.Numeracion_hasta
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 59 BY 4.76
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
   External Tables: general.lotes_jugo
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
         HEIGHT             = 4.76
         WIDTH              = 59.2.
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
     _TblList          = "general.composicion_lote WHERE general.lotes_jugo ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "general.composicion_lote.id_empresa = general.lotes_jugo.id_empresa
  AND general.composicion_lote.id_sucursal = general.lotes_jugo.id_sucursal
  AND general.composicion_lote.id_tipotambor = general.lotes_jugo.id_tipotambor
  AND general.composicion_lote.nromov = general.lotes_jugo.nromov"
     _FldNameList[1]   > general.composicion_lote.Fecha
"composicion_lote.Fecha" ? ? "date" ? ? ? ? ? ? yes ? no no "14.2" yes no no "U" "" ""
     _FldNameList[2]   > general.composicion_lote.cantidad_tambores
"composicion_lote.cantidad_tambores" "Cantidad" ">>>9" "integer" ? ? ? ? ? ? yes ? no no "12.2" yes no no "U" "" ""
     _FldNameList[3]   > general.composicion_lote.Numeracion_desde
"composicion_lote.Numeracion_desde" "Desde" ">>9" "integer" ? ? ? ? ? ? yes ? no no "8.2" yes no no "U" "" ""
     _FldNameList[4]   > general.composicion_lote.Numeracion_hasta
"composicion_lote.Numeracion_hasta" "Hasta" ">>>9" "integer" ? ? ? ? ? ? yes ? no no "10.2" yes no no "U" "" ""
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


&Scoped-define SELF-NAME composicion_lote.Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL composicion_lote.Fecha br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF composicion_lote.Fecha IN BROWSE br_table /* Fecha */
DO:
  /* if not mi-alta then
   do: 
    define var hcontainer as handle.
    define var r as rowid.
    define var esta-fecha as date.
    
    esta-fecha = date(composicion_lote.fecha:screen-value in browse {&BROWSE-NAME}).
        
    RUN get-container (output hcontainer).
    RUN get-rowid-cabecera in hcontainer (output r).
    
    find lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
    if available lotes_jugo then
        do:
            if (lotes_jugo.fecha_comienzo > esta-fecha) or (lotes_jugo.fecha_finalizacion < esta-fecha) then
                do:
                    message "La fecha de la creación de los tambores debe ser entre " lotes_jugo.fecha_comienzo 
                            " y " lotes_jugo.fecha_finalizacion.
                    
                    return no-apply.
                
                end.
    
        end.    
    
    end.*/
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

run set-attribute-list('alta-automatica = no').

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
run set-attribute-list("alta-automatica=no").
run set-attribute-list('alta-automatica=no').

RUN get-container (output hcontainer).

RUN get-rowid-cabecera in hcontainer (output r).
find lotes_jugo where rowid(lotes_jugo) = r  no-lock no-error.
if available lotes_jugo then
do:    
   assign composicion_lote.id_empresa        = lotes_jugo.id_empresa
          composicion_lote.id_sucursal       = lotes_jugo.id_sucursal
          composicion_lote.id_lote           = lotes_jugo.id_lote
          composicion_lote.id_tipotambor     = lotes_jugo.id_tipotambor
          composicion_lote.nromov            = lotes_jugo.nromov
          composicion_lote.c_usuario         = userid("userdb")
          composicion_lote.c_fecha           = today
          composicion_lote.c_hora            = string(time,"HH:MM:SS").
end.

/*******************************************************************************/
/******************CREO LOS REGISTROS PARA LA TABLA TAMBORES_LOTE***************/
define var ultimo as integer.
define var cant_tambores as integer.
define var i as integer.
define var desde as integer.
define var hasta as integer.
define var fecha_ela as date.

cant_tambores    = integer(composicion_lote.cantidad_tambores:screen-value in browse {&BROWSE-NAME}).
desde            = integer(composicion_lote.numeracion_desde:screen-value in browse {&BROWSE-NAME}).
hasta            = integer(composicion_lote.numeracion_hasta:screen-value in browse {&BROWSE-NAME}).
fecha_ela        = date(composicion_lote.fecha:screen-value in browse {&BROWSE-NAME}).

if available lotes_jugo THEN DO:
    FIND envases_prod WHERE envases_prod.id_envase = lotes_jugo.id_envase  NO-LOCK NO-ERROR.
    do i = desde to hasta:
            create tambores_industria.
            assign tambores_industria.id_empresa         = lotes_jugo.id_empresa
                   tambores_industria.id_sucursal        = lotes_jugo.id_sucursal
                   tambores_industria.id_lote            = lotes_jugo.id_lote
                   tambores_industria.id_calidad         = lotes_jugo.id_calidad
                   tambores_industria.id_tipotambor      = lotes_jugo.id_tipotambor
                   tambores_industria.nromov             = lotes_jugo.nromov
                   tambores_industria.id_tambor          = i   /*ultimo*/
                   tambores_industria.fecha              = fecha_ela
                   tambores_industria.anio               = lotes_jugo.anio
                   tambores_industria.id_etiqueta        = IF lotes_jugo.id_sucursal = 96 THEN NEXT-VALUE(tambores)
                                                                                          ELSE NEXT-VALUE(tambores_famailla)
                   tambores_industria.tara               = IF AVAILABLE envases_prod THEN envases_prod.tara
                                                                                     ELSE 17
                   tambores_industria.id_envase          = lotes_jugo.id_envase
                   tambores_industria.id_articulo        = lotes_jugo.id_articulo
                   tambores_industria.c_usuario          = userid("userdb")
                   tambores_industria.c_fecha            = today
                   tambores_industria.c_hora             = string(time,"HH:MM:SS")
                   tambores_industria.id_empresa_ubicacion = 1 
                   tambores_industria.id_sucursal_ubicacion = lotes_jugo.id_sucursal
                   tambores_industria.id_locacion_ubicacion = 4
                   tambores_industria.id_posicion_ubicacion = 1
                   tambores_industria.kilos_tambor       = lotes_jugo.Peso_neto 
                   tambores_industria.id_estado          = 9.
    end.
     
    RUN y_gstkcre.p (input lotes_jugo.id_empresa,
                     input lotes_jugo.id_sucursal,
                     input lotes_jugo.id_tipotambor,
                     input lotes_jugo.nromov,
                     INPUT desde,
                     INPUT hasta,
                     input 1) "lotes_jugo".

    IF return-value <> "" then do:
        message "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "error".
    END.


    find first tambores_industria where tambores_industria.id_empresa = lotes_jugo.id_empresa
                                    and tambores_industria.id_sucursal = lotes_jugo.id_sucursal
                                    and tambores_industria.id_tipotambor = lotes_jugo.id_tipotambor
                                    and tambores_industria.nromov = lotes_jugo.nromov
                                    and tambores_industria.id_tambor = 0
                                    no-error.
    
    if available tambores_industria then do:
        delete tambores_industria.
    end.    
end.
RUN mailing.
run set-attribute-list('alta-automatica=no').
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
define var r as rowid.

if empresa <> ? and sucursal <> ? and tipotambor <> ? and nromov <> ? and fecha <>  ? then
 do:
    /*
    message "Los tambores de este lote no se borraran. Si desea borrar avise a computos." view-as alert-box.
    message "Atencion, le aviso nuevamente que no se borraran los tambores, si no avisa a computos ocurriran problemas posteriores....." view-as alert-box.
    */
    
    for each tambores_industria where tambores_industria.id_empresa      = empresa and
                                      tambores_industria.id_sucursal     = sucursal and
                                      tambores_industria.id_tipotambor   = tipotambor and
                                      tambores_industria.nromov          = nromov and
                                      tambores_industria.fecha           = fecha AND
                                      tambores_industria.id_tambor       >= v_desde AND
                                      tambores_industria.id_tambor       <= v_hasta.
             delete tambores_industria.
    end.

END.
 else message "Se produjo un error al borrar los tambores, por favor avisar al Centro de Computos" view-as alert-box.

 
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
run set-attribute-list('alta-automatica=no').

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
define var hcontainer as handle.
define var r as rowid.
define var esta-fecha as date.

run set-attribute-list('alta-automatica=no').

RUN get-container (output hcontainer).

RUN get-rowid-cabecera in hcontainer (output r).
FIND lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
IF available lotes_jugo THEN DO:
    for each composicion_lote where composicion_lote.id_empresa      = lotes_jugo.id_empresa and
                                    composicion_lote.id_sucursal     = lotes_jugo.id_sucursal and
                                    composicion_lote.id_tipotambor   = lotes_jugo.id_tipotambor and
                                    composicion_lote.nromov          = lotes_jugo.nromov no-lock.
        
        IF composicion_lote.numeracion_hasta > 
           integer(composicion_lote.numeracion_desde:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
            MESSAGE "Debe cargar tambores que no hayan sido producidos!!!. 
                     Los nuevos tambores deben ser posteriores al " 
                     composicion_lote.numeracion_hasta VIEW-AS ALERT-BOX.
            RETURN "ADM-ERROR".
        END. 
    END.
END.

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
DEFINE BUFFER b_tam FOR tambores_industria.
DEFINE VAR desde AS INTEGER.
DEFINE VAR hasta AS INTEGER.

RUN devuelve-rowid (output r).
FIND composicion_lote where rowid(composicion_lote) = r no-lock no-error.
IF AVAILABLE composicion_lote THEN DO:
    empresa      = composicion_lote.id_empresa.
    sucursal     = composicion_lote.id_sucursal.
    tipotambor   = composicion_lote.id_tipotambor.
    nromov       = composicion_lote.nromov.
    fecha        = composicion_lote.fecha.
    v_desde      = composicion_lote.numeracion_desde.
    v_hasta      = composicion_lote.numeracion_hasta.

    FIND FIRST b_tam WHERE b_tam.id_empresa      = empresa and
                           b_tam.id_sucursal     = sucursal and
                           b_tam.id_tipotambor   = tipotambor and
                           b_tam.nromov          = nromov and
                           b_tam.fecha           = fecha AND
                           b_tam.id_tambor       = v_desde NO-LOCK NO-ERROR.
    
    IF AVAILABLE b_tam THEN DO:
        desde = b_tam.id_tambor.
    END.
    
    FIND LAST b_tam WHERE b_tam.id_empresa       = empresa and
                          b_tam.id_sucursal      = sucursal and
                          b_tam.id_tipotambor    = tipotambor and
                          b_tam.nromov           = nromov and
                          b_tam.fecha            = fecha AND
                          b_tam.id_tambor        = v_hasta NO-LOCK NO-ERROR.
    
    IF AVAILABLE b_tam THEN DO:
        hasta = b_tam.id_tambor.
    END.
    
    RUN y_gstkcre.p (INPUT composicion_lote.id_empresa,
                     INPUT composicion_lote.id_sucursal,
                     INPUT composicion_lote.id_tipotambor,
                     INPUT composicion_lote.nromov,
                     INPUT desde,
                     INPUT hasta,
                     INPUT 2) "lotes_jugo".
    
    IF RETURN-VALUE <> "" THEN DO:
        MESSAGE "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "ADM-ERROR".
    END.
END.
ELSE DO: 
    MESSAGE "Se produjo un error . Por favor avise al Centro de Computos" view-as alert-box.
    RETURN "ADM-ERROR".
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailing B-table-Win 
PROCEDURE mailing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cUsuarios  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBody      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCant      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE r          AS ROWID      NO-UNDO.
  DEFINE VARIABLE hContainer AS HANDLE     NO-UNDO.

  
  RUN get-container (OUTPUT hContainer).

  RUN get-rowid-cabecera in hContainer (OUTPUT r).
  FIND lotes_jugo WHERE ROWID(lotes_jugo) = r  NO-LOCK NO-ERROR.
  
  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
    RUN mailingInsumos IN hLib(lotes_jugo.id_empresa, 
                               lotes_jugo.id_sucursal, 
                               lotes_jugo.id_tipotambor,
                               lotes_jugo.nromov).
  END.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-add B-table-Win 
PROCEDURE post-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
mi-alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-add B-table-Win 
PROCEDURE pre-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
mi-alta = true.
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
  {src/adm/template/sndkycas.i "id_sucursal" "composicion_lote" "id_sucursal"}

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
  {src/adm/template/snd-list.i "lotes_jugo"}
  {src/adm/template/snd-list.i "composicion_lote"}

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

