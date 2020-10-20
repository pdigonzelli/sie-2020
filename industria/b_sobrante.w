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
define var entro as logical.
define var del-emp as integer.
define var del-suc as integer.
define var del-tip as integer.
define var del-nro as integer.
DEFINE VAR del-ids AS INTEGER.
entro = false.
del-emp = 0.
del-suc = 0.
del-tip = 0.
del-nro = 0.

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
&Scoped-define INTERNAL-TABLES sobrante envases_prod

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table sobrante.fecha_elaboracion ~
sobrante.volumen sobrante.id_envase envases_prod.abreviatura ~
sobrante.cantidad sobrante.kilos_tambor sobrante.identificacion 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table sobrante.fecha_elaboracion ~
sobrante.volumen sobrante.id_envase sobrante.cantidad sobrante.kilos_tambor ~
sobrante.identificacion 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table sobrante
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table sobrante
&Scoped-define QUERY-STRING-br_table FOR EACH sobrante OF lotes_jugo WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH envases_prod OF sobrante NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH sobrante OF lotes_jugo WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH envases_prod OF sobrante NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table sobrante envases_prod
&Scoped-define FIRST-TABLE-IN-QUERY-br_table sobrante
&Scoped-define SECOND-TABLE-IN-QUERY-br_table envases_prod


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
id_envase||y|general.sobrante.id_envase
id_articulo||y|general.sobrante.id_articulo
id_tipotambor||y|general.sobrante.id_tipotambor
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_envase,id_articulo,id_tipotambor"':U).

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
      sobrante, 
      envases_prod SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      sobrante.fecha_elaboracion FORMAT "99/99/99":U WIDTH 15.2
      sobrante.volumen COLUMN-LABEL "Volumen" FORMAT "->>,>>9.99":U
            WIDTH 11.2
      sobrante.id_envase COLUMN-LABEL "Env" FORMAT ">>9":U WIDTH 5.2
      envases_prod.abreviatura COLUMN-LABEL "Envase" FORMAT "x(12)":U
            WIDTH 14.2
      sobrante.cantidad COLUMN-LABEL "Cantidad" FORMAT ">>9":U
            WIDTH 12.2
      sobrante.kilos_tambor FORMAT ">>>9.99":U WIDTH 11.2
      sobrante.identificacion COLUMN-LABEL "Identificacion" FORMAT "X(20)":U
  ENABLE
      sobrante.fecha_elaboracion
      sobrante.volumen
      sobrante.id_envase
      sobrante.cantidad
      sobrante.kilos_tambor
      sobrante.identificacion
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 115 BY 4.29
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
   External Tables: general.lotes_jugo
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
         HEIGHT             = 4.29
         WIDTH              = 115.8.
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
     _TblList          = "general.sobrante OF general.lotes_jugo,general.envases_prod OF general.sobrante"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > general.sobrante.fecha_elaboracion
"sobrante.fecha_elaboracion" ? ? "date" ? ? ? ? ? ? yes ? no no "15.2" yes no no "U" "" ""
     _FldNameList[2]   > general.sobrante.volumen
"sobrante.volumen" "Volumen" ? "decimal" ? ? ? ? ? ? yes ? no no "11.2" yes no no "U" "" ""
     _FldNameList[3]   > general.sobrante.id_envase
"sobrante.id_envase" "Env" ">>9" "integer" ? ? ? ? ? ? yes ? no no "5.2" yes no no "U" "" ""
     _FldNameList[4]   > general.envases_prod.abreviatura
"envases_prod.abreviatura" "Envase" ? "character" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" ""
     _FldNameList[5]   > general.sobrante.cantidad
"sobrante.cantidad" "Cantidad" ">>9" "decimal" ? ? ? ? ? ? yes ? no no "12.2" yes no no "U" "" ""
     _FldNameList[6]   > general.sobrante.kilos_tambor
"sobrante.kilos_tambor" ? ">>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "11.2" yes no no "U" "" ""
     _FldNameList[7]   > general.sobrante.identificacion
"sobrante.identificacion" "Identificacion" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
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
define var i as integer initial 1.
define var ultimo as integer.
DEFINE BUFFER bb_sobrante FOR sobrante.
DEFINE BUFFER bb_tam FOR tambores_industria.
DEFINE VAR v_sobrante AS INTEGER.
define var v_nromov_sobrante as integer.
DEFINE VAR nro_tam AS INTEGER.

RUN get-container (output hcontainer).
RUN get-rowid-cabecera in hcontainer (output r).
FOR each lotes_jugo where rowid(lotes_jugo) = r.
    
    FIND LAST bb_sobrante WHERE bb_sobrante.id_empresa       = lotes_jugo.id_empresa
                            AND bb_sobrante.id_sucursal      = lotes_jugo.id_sucursal
                            AND bb_sobrante.id_tipotambor    = lotes_jugo.id_tipotambor
                            AND bb_sobrante.nromov           = lotes_jugo.nromov 
                            NO-LOCK NO-ERROR.
    IF AVAILABLE bb_sobrante THEN DO:
        v_sobrante = bb_sobrante.id_sobrante + 1.
        FIND LAST bb_tam WHERE bb_tam.id_empresa     = bb_sobrante.id_empresa
                           AND bb_tam.id_sucursal    = bb_sobrante.id_sucursal
                           AND bb_tam.id_tipotambor  = bb_sobrante.id_tipotambor_sobrante
                           AND bb_tam.nromov         = bb_sobrante.nromov_sobrante.
        IF AVAILABLE bb_tam THEN DO:
            nro_tam = bb_tam.id_tambor.
        END.
        ELSE DO:
            nro_tam = 0.
        END.
    END.
    ELSE DO:
        v_sobrante = 1.
        nro_tam = 0.
    END.
        
    RELEASE bb_sobrante.
    RELEASE bb_tam.

    v_nromov_sobrante = next-value(nromov).
    assign sobrante.id_empresa               = lotes_jugo.id_empresa
           sobrante.id_sucursal              = lotes_jugo.id_sucursal
           sobrante.id_lote                  = lotes_jugo.id_lote
           sobrante.id_tipotambor            = lotes_jugo.id_tipotambor
           sobrante.nromov                   = lotes_jugo.nromov
           sobrante.id_tipotambor_sobrante   = 4
           sobrante.nromov_sobrante          = v_nromov_sobrante
           sobrante.c_usuario                = userid("userdb")
           sobrante.c_fecha                  = today
           sobrante.c_hora                   = string(time,"HH:MM:SS")
           sobrante.id_articulo              = integer(string(lotes_jugo.id_articulo) + "4")
           general.sobrante.id_sobrante    = v_sobrante.
   
   DO i = 1 to integer(sobrante.cantidad:screen-value in BROWSE br_table):
        
       CREATE tambores_industria.
       assign tambores_industria.id_empresa          = lotes_jugo.id_empresa
              tambores_industria.id_sucursal         = lotes_jugo.id_sucursal
              tambores_industria.id_lote             = lotes_jugo.id_lote
              tambores_industria.id_tambor           = nro_tam + i
              tambores_industria.id_tipotambor       = 4
              tambores_industria.nromov              = v_nromov_sobrante
              tambores_industria.id_envase           = INTEGER(sobrante.id_envase:screen-value in BROWSE br_table)
              tambores_industria.id_etiqueta         = IF lotes_jugo.id_sucursal = 96 THEN next-value(tambores) ELSE next-value(tambores_famailla)
              tambores_industria.fecha               = date(sobrante.fecha_elaboracion:screen-value in BROWSE br_table)
              tambores_industria.anio                = INTEGER(lotes_jugo.anio)
              tambores_industria.id_articulo         = INTEGER(STRING(lotes_jugo.id_articulo) + "4")
              tambores_industria.id_calidad          = lotes_jugo.id_calidad
              tambores_industria.c_usuario           = userid("userdb")
              tambores_industria.c_fecha             = today
              tambores_industria.c_hora              = string(time,"HH:MM:SS")
              tambores_industria.kilos_tambor        = DECIMAL(sobrante.kilos_tambor:SCREEN-VALUE IN BROWSE br_table)
              tambores_industria.id_empresa_ubicacion  = 1
              tambores_industria.id_sucursal_ubicacion = lotes_jugo.id_sucursal
              tambores_industria.id_locacion_ubicacion = 4
              tambores_industria.id_posicion_ubicacion = 1
              tambores_industria.id_estado             = 9.
    END.
    RUN y_gstkcre.p (input lotes_jugo.id_empresa,
                     input lotes_jugo.id_sucursal,
                     input 4,
                     input v_nromov_sobrante,
                     INPUT nro_tam + 1, /* desde */
                     INPUT nro_tam + integer(sobrante.cantidad:screen-value in BROWSE br_table), /* hasta */
                     input 1) "sobrante_jugo".

    IF return-value <> "" then do:
        message "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "error".
    END.
END.

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
IF entro THEN DO:
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa      = del-emp AND
                                      tambores_industria.id_sucursal     = del-suc AND
                                      tambores_industria.id_tipotambor   = del-tip AND
                                      tambores_industria.nromov          = del-nro.
        DELETE tambores_industria.
    END.
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
define var hcontainer as handle.
define var r as rowid.
define var i as integer initial 1.
DEFINE VAR desde AS INTEGER.
DEFINE VAR hasta AS INTEGER.
DEFINE BUFFER b_tam FOR tambores_industria.
    

RUN get-container (output hcontainer).
RUN get-rowid-cabecera in hcontainer (output r).
find lotes_jugo where rowid(lotes_jugo) = r no-error.
if available lotes_jugo then
do:
   if available sobrante then
   do:                 
       find first tambores_industria where tambores_industria.id_empresa    = sobrante.id_empresa and
                                           tambores_industria.id_sucursal   = sobrante.id_sucursal and
                                           tambores_industria.id_tipotambor = sobrante.id_tipotambor_sobrante and
                                           tambores_industria.nromov        = sobrante.nromov_sobrante and
                                           (tambores_industria.id_empresa_destino <> 0 or
                                            tambores_industria.id_sucursal_destino <> 0 or
                                            tambores_industria.id_tipotambor_destino <> 0 or
                                            tambores_industria.nromov_destino <> 0) no-error.
                                            
       IF AVAILABLE tambores_industria THEN DO:
                MESSAGE "No puede borrar el sobrante porque ya esta siendo utilizado por otros lotes.".
                RETURN "ADM-ERROR".
       END.
       ELSE DO:
            entro = true.     
            del-emp = sobrante.id_empresa.
            del-suc = sobrante.id_sucursal.
            del-tip = sobrante.id_tipotambor_sobrante.
            del-nro = sobrante.nromov_sobrante.
            
            FIND FIRST b_tam WHERE b_tam.id_empresa      = sobrante.id_empresa and
                                   b_tam.id_sucursal     = sobrante.id_sucursal and
                                   b_tam.id_tipotambor   = sobrante.id_tipotambor_sobrante and
                                   b_tam.nromov          = sobrante.nromov_sobrante NO-LOCK NO-ERROR.
        
            IF AVAILABLE b_tam THEN DO:
                desde = b_tam.id_tambor.
            END.
            FIND LAST b_tam WHERE b_tam.id_empresa      = sobrante.id_empresa and
                                  b_tam.id_sucursal     = sobrante.id_sucursal and
                                  b_tam.id_tipotambor   = sobrante.id_tipotambor_sobrante and
                                  b_tam.nromov          = sobrante.nromov_sobrante NO-LOCK NO-ERROR.
        
            IF AVAILABLE b_tam THEN DO:
                hasta = b_tam.id_tambor.
            END.
        
            RUN y_gstkcre.p (input lotes_jugo.id_empresa,
                              input lotes_jugo.id_sucursal,
                              input sobrante.id_tipotambor_sobrante,
                              input sobrante.nromov_sobrante,
                              INPUT desde,
                              INPUT hasta,
                              input 2) "sobrante".
        
             IF return-value <> "" then do:
                 message "Error en el procesamiento de movimientos de stock" view-as alert-box.
                 RETURN "ADM-ERROR".
             end.
       END.
        
   end.   
end.
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
  {src/adm/template/sndkycas.i "id_envase" "sobrante" "id_envase"}
  {src/adm/template/sndkycas.i "id_articulo" "sobrante" "id_articulo"}
  {src/adm/template/sndkycas.i "id_tipotambor" "sobrante" "id_tipotambor"}

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
  {src/adm/template/snd-list.i "sobrante"}
  {src/adm/template/snd-list.i "envases_prod"}

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

