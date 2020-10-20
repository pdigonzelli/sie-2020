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
DEF STREAM DataStream.
DEF VAR pv_chWord AS COM-HANDLE NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES muestras
&Scoped-define FIRST-EXTERNAL-TABLE muestras


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR muestras.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES items_muestras r_muestras_protocolos ~
protocolos quimicos tambores_industria

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table protocolos.id_protocolo ~
protocolos.anio protocolos.fecha_aprobacion quimicos.nombre ~
tambores_industria.id_lote tambores_industria.Anio ~
items_muestras.fecha_enviado_bue items_muestras.fecha_enviado_tuc 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH items_muestras OF muestras WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH r_muestras_protocolos OF items_muestras NO-LOCK, ~
      EACH protocolos OF r_muestras_protocolos NO-LOCK, ~
      EACH quimicos OF protocolos NO-LOCK, ~
      FIRST tambores_industria WHERE tambores_industria.id_empresa = protocolos.id_empresa ~
  AND tambores_industria.id_sucursal = protocolos.id_sucursal ~
  AND tambores_industria.id_tipotambor = protocolos.id_tipotambor ~
  AND tambores_industria.nromov = protocolos.nromov OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH items_muestras OF muestras WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH r_muestras_protocolos OF items_muestras NO-LOCK, ~
      EACH protocolos OF r_muestras_protocolos NO-LOCK, ~
      EACH quimicos OF protocolos NO-LOCK, ~
      FIRST tambores_industria WHERE tambores_industria.id_empresa = protocolos.id_empresa ~
  AND tambores_industria.id_sucursal = protocolos.id_sucursal ~
  AND tambores_industria.id_tipotambor = protocolos.id_tipotambor ~
  AND tambores_industria.nromov = protocolos.nromov OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table items_muestras ~
r_muestras_protocolos protocolos quimicos tambores_industria
&Scoped-define FIRST-TABLE-IN-QUERY-br_table items_muestras
&Scoped-define SECOND-TABLE-IN-QUERY-br_table r_muestras_protocolos
&Scoped-define THIRD-TABLE-IN-QUERY-br_table protocolos
&Scoped-define FOURTH-TABLE-IN-QUERY-br_table quimicos
&Scoped-define FIFTH-TABLE-IN-QUERY-br_table tambores_industria


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table BUTTON-32 BUTTON-34 BUTTON-33 ~
BUTTON-36 BUTTON-35 BUTTON-37 

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
id_color||y|general.protocolos.id_color
id_contramarca||y|general.protocolos.id_contramarca
id_articulo||y|general.protocolos.id_articulo
id_protocolo||y|general.protocolos.id_protocolo
id_quimico||y|general.protocolos.id_quimico
id_tipotambor||y|general.protocolos.id_tipotambor
id_tipo_protocolo||y|general.protocolos.id_tipo_protocolo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_color,id_contramarca,id_articulo,id_protocolo,id_quimico,id_tipotambor,id_tipo_protocolo"':U).

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
DEFINE BUTTON BUTTON-32 
     LABEL "Imprimir Protocolo" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-33 
     LABEL "Enviar Mail a Destinatario" 
     SIZE 28 BY 1.14.

DEFINE BUTTON BUTTON-34 
     LABEL "Mail con Protocolo" 
     SIZE 19 BY 1.14.

DEFINE BUTTON BUTTON-35 
     LABEL "Prot. Jugo" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-36 
     LABEL "Prot. Aceite" 
     SIZE 14 BY 1.14.

DEFINE BUTTON BUTTON-37 
     LABEL "Prot. Cascara" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      items_muestras, 
      r_muestras_protocolos, 
      protocolos, 
      quimicos, 
      tambores_industria SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      protocolos.id_protocolo FORMAT "->>>>>9":U
      protocolos.anio FORMAT ">>>9":U WIDTH 8.6
      protocolos.fecha_aprobacion FORMAT "99/99/99":U WIDTH 23.2
      quimicos.nombre COLUMN-LABEL "Quimico" FORMAT "X(30)":U
      tambores_industria.id_lote FORMAT ">>>>9":U
      tambores_industria.Anio FORMAT ">>>9":U
      items_muestras.fecha_enviado_bue COLUMN-LABEL "FechaEnvBue" FORMAT "99/99/99":U
      items_muestras.fecha_enviado_tuc COLUMN-LABEL "FechaEnvTuc" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 146 BY 6.71
         FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     BUTTON-32 AT ROW 7.91 COL 1
     BUTTON-34 AT ROW 7.91 COL 24.2
     BUTTON-33 AT ROW 7.91 COL 46
     BUTTON-36 AT ROW 7.91 COL 80
     BUTTON-35 AT ROW 7.91 COL 94
     BUTTON-37 AT ROW 7.91 COL 109
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: csmartbrowser
   External Tables: general.muestras
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
         HEIGHT             = 8.1
         WIDTH              = 146.
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
     _TblList          = "general.items_muestras OF general.muestras,industria.r_muestras_protocolos OF general.items_muestras,industria.protocolos OF general.r_muestras_protocolos,industria.quimicos OF general.protocolos,industria.tambores_industria WHERE general.protocolos ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ",,,, FIRST OUTER"
     _JoinCode[5]      = "general.tambores_industria.id_empresa = general.protocolos.id_empresa
  AND general.tambores_industria.id_sucursal = general.protocolos.id_sucursal
  AND general.tambores_industria.id_tipotambor = general.protocolos.id_tipotambor
  AND general.tambores_industria.nromov = general.protocolos.nromov"
     _FldNameList[1]   = general.protocolos.id_protocolo
     _FldNameList[2]   > general.protocolos.anio
"protocolos.anio" ? ? "integer" ? ? ? ? ? ? no ? no no "8.6" yes no no "U" "" ""
     _FldNameList[3]   > general.protocolos.fecha_aprobacion
"protocolos.fecha_aprobacion" ? ? "date" ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" ""
     _FldNameList[4]   > general.quimicos.nombre
"quimicos.nombre" "Quimico" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > general.tambores_industria.id_lote
"tambores_industria.id_lote" ? ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > general.tambores_industria.Anio
"tambores_industria.Anio" ? ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > general.items_muestras.fecha_enviado_bue
"items_muestras.fecha_enviado_bue" "FechaEnvBue" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > general.items_muestras.fecha_enviado_tuc
"items_muestras.fecha_enviado_tuc" "FechaEnvTuc" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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


&Scoped-define SELF-NAME BUTTON-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-32 B-table-Win
ON CHOOSE OF BUTTON-32 IN FRAME F-Main /* Imprimir Protocolo */
DO:
  RUN i_impresion_protocolos.i (INPUT ROWID(protocolos)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-33 B-table-Win
ON CHOOSE OF BUTTON-33 IN FRAME F-Main /* Enviar Mail a Destinatario */
DO:  

  /*{i_mail_destinatario.i}.*/

  {..\desarrollo\SERVERDES\industria\iMailAttachProtocol.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-34
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-34 B-table-Win
ON CHOOSE OF BUTTON-34 IN FRAME F-Main /* Mail con Protocolo */
DO:
DEFINE VAR r_prot AS ROWID.
DEFINE VAR r_items_muestras AS ROWID.
DEFINE VAR h_con AS HANDLE.

/* RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot). 

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR. */
    IF AVAILABLE protocolos THEN DO:
        FIND FIRST tambores_industria WHERE tambores_industria.id_empresa = protocolos.id_empresa
                                        AND tambores_industria.id_sucursal = protocolos.id_sucursal
                                        AND tambores_industria.id_tipotambor = protocolos.id_tipotambor
                                        AND tambores_industria.nromov = protocolos.nromov
                                        NO-LOCK NO-ERROR.
        IF AVAILABLE tambores_industria THEN
            {..\desarrollo\SERVERDES\industria\i_mail_destinatario_protocolos.i}
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-35 B-table-Win
ON CHOOSE OF BUTTON-35 IN FRAME F-Main /* Prot. Jugo */
DO:
DEF VAR lv_iSeq     AS INT NO-UNDO.
DEF VAR lv_iPercent AS INT NO-UNDO.
DEF VAR lv_iTotal   AS INT NO-UNDO.
DEF VAR lv_cDocument      AS CHAR INIT "protocolo_jugo.doc":U      NO-UNDO. 
DEF VAR lv_cDataFile      AS CHAR INIT "protocolo_jugo.dat":U NO-UNDO. 
DEF VAR lv_cMailMergeFile AS CHAR INIT "protocolo_jugoHeader.doc":U NO-UNDO.
                                       /* Pointer to Document Object */
DEFINE VAR r_prot AS ROWID.
DEFINE VAR v_body AS CHAR.
DEFINE VAR v_smell AS CHAR.
DEFINE VAR v_color AS CHAR.
DEFINE VAR v_caract AS CHAR.
DEFINE VAR v_valores AS CHAR.
DEFINE VAR v_code AS CHAR.
DEFINE VAR v_valor_code AS CHAR.
DEFINE VAR v_galones AS CHAR.
DEFINE VAR v_valor_galones AS CHAR.

/*
RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR.*/
IF AVAILABLE protocolos THEN DO:
    FIND FIRST contramarcas OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST quimicos OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST lotes_jugo OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST tambores_industria OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST contratos WHERE contratos.id_contrato        = tambores_industria.id_contrato_of
                           AND contratos.id_tipo_contrato   = tambores_industria.id_tipocontrato_of
                           AND contratos.anio               = tambores_industria.anio_of
                         NO-LOCK NO-ERROR.
    FIND FIRST colores_prot OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST cuerpo_prot WHERE cuerpo_prot.id_cuerpo = protocolos.id_body NO-LOCK NO-ERROR.
    FIND FIRST aromas_sabores_prot WHERE aromas_sabores_prot.id_aroma = protocolos.id_smell NO-LOCK NO-ERROR.
    v_caract = "".
    FOR EACH items_protocolos OF protocolos NO-LOCK.
        FIND FIRST caracteristicas_quimicas OF items_protocolos NO-LOCK NO-ERROR.
        v_caract = v_caract + caracteristicas_quimicas.descripcion + CHR(10).
        v_valores = v_valores + general.items_protocolos.valor_caracter + CHR(10).
    END.
    IF protocolos.con_code THEN DO:
        v_code = "CODE:".
        v_valor_code = protocolos.CODE.
    END.
    IF protocolos.con_pyg THEN DO:
        v_code = "P&G RMS Nro:".
        v_valor_code = protocolos.CODE.
    END.
    IF protocolos.con_galones THEN DO:
        v_galones = "Gallon per drum".
        v_valor_galones = STRING(protocolos.galones_tambores).
    END.
    OUTPUT STREAM DataStream TO VALUE(lv_cDataFile).                              /* create data file */
     
    EXPORT STREAM DataStream DELIMITER "~t":U                                 /* export the data */
        STRING(MONTH(protocolos.fecha),"99") + "/" + 
        STRING(DAY(protocolos.fecha),"99")   + "/" + 
        STRING(YEAR(protocolos.fecha),">>99") 
        IF AVAILABLE lotes_jugo THEN STRING(lotes_jugo.id_lote,">999") + "/" + SUBSTRING(STRING(lotes_jugo.anio,">>99"),3,2) ELSE "NONE"
        IF AVAILABLE lotes_jugo THEN STRING(lotes_jugo.fecha) ELSE "NONE"
        IF AVAILABLE productos_terminados THEN UPPER(productos_terminados.descripcion_ingles) ELSE "NONE"
        IF AVAILABLE calidades THEN UPPER(calidades.descripcion) ELSE "NONE"
        IF AVAILABLE contramarcas THEN UPPER(contramarcas.descripcion) ELSE "NONE"
        v_code
        v_valor_code
        STRING(protocolos.id_protocolo,">999") + "/" + STRING(protocolos.anio,">>99")
        IF AVAILABLE contratos THEN STRING(contratos.orden_fabricacion,">999") + "/" + STRING(SUBSTRING(STRING(contratos.anio),3,2),"99") ELSE "NONE"
        IF AVAILABLE colores_prot THEN colores_prot.descripcion ELSE "NONE"
        IF AVAILABLE cuerpo_prot THEN cuerpo_prot.descripcion ELSE "NONE"
        IF AVAILABLE aromas_sabores_prot THEN aromas_sabores_prot.descripcion ELSE "NONE"
        v_caract
        v_valores
        protocolos.cantidad_tambores
        STRING(industria.protocolos.desde_tambor,"999") + " to " + STRING(industria.protocolos.hasta_tambor,"999")
        general.protocolos.peso_neto
        general.protocolos.peso_bruto_tambor
        v_galones
        v_valor_galones
        IF AVAILABLE quimicos THEN UPPER(quimicos.titulo) ELSE "NONE"
        IF AVAILABLE quimicos THEN UPPER(industria.quimicos.nombre_ingles) ELSE "NONE"
        .
    END.
      
    OUTPUT STREAM DataStream CLOSE.                                                  /* close data file */
     
    /* CREO EL DOCUMENTO DE WORD */
    CREATE "Word.Application" pv_chWord NO-ERROR.     
    ASSIGN pv_chWord:Visible = YES.                                                  /* Word is not "hidden" from user */
      
    RUN p_MailMerge.p(lv_cDocument,                                                  /* Main Document */
                      lv_cDataFile,                                                  /* File that holds all the data */
                      lv_cMailMergeFile,                                             /* File to hold new mail merge document */
                      NO,
                      pv_chWord).                                                    /* Automatically Print New Mail Merge Document */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-36
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-36 B-table-Win
ON CHOOSE OF BUTTON-36 IN FRAME F-Main /* Prot. Aceite */
DO:
DEF VAR lv_iSeq     AS INT NO-UNDO.
DEF VAR lv_iPercent AS INT NO-UNDO.
DEF VAR lv_iTotal   AS INT NO-UNDO.
DEF VAR lv_cDocument      AS CHAR INIT "protocolo_aceite.doc":U      NO-UNDO. 
DEF VAR lv_cDataFile      AS CHAR INIT "protocolo_aceite.dat":U NO-UNDO. 
DEF VAR lv_cMailMergeFile AS CHAR INIT "protocolo_aceiteHeader.doc":U NO-UNDO.
                                       /* Pointer to Document Object */
DEFINE VAR r_prot AS ROWID.
DEFINE VAR v_body AS CHAR.
DEFINE VAR v_smell AS CHAR.
DEFINE VAR v_color AS CHAR.
DEFINE VAR v_caract AS CHAR.
DEFINE VAR v_valores AS CHAR.
DEFINE VAR v_code AS CHAR.
DEFINE VAR v_valor_code AS CHAR.
DEFINE VAR v_galones AS CHAR.
DEFINE VAR v_valor_galones AS CHAR.

/*
RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR. */
IF AVAILABLE protocolos THEN DO:
    FIND FIRST contramarcas OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST lotes_aceite OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST tambores_industria OF lotes_aceite NO-LOCK NO-ERROR.
    FIND FIRST quimicos OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST r_muestras_protocolos WHERE r_muestras_protocolos.id_protocolo = protocolos.id_protocolo
                                       AND r_muestras_protocolos.id_articulo  = protocolos.id_articulo
                                       AND r_muestras_protocolos.anio         = protocolos.anio 
                                     NO-LOCK NO-ERROR.

    IF AVAILABLE r_muestras_protocolos THEN DO:
        FIND FIRST items_muestras OF r_muestras_protocolos NO-LOCK NO-ERROR.
        FIND FIRST envases_muestras OF items_muestras NO-LOCK NO-ERROR.
    END.

    FOR EACH items_protocolos OF protocolos NO-LOCK.
        FIND FIRST caracteristicas_quimicas OF items_protocolos NO-LOCK NO-ERROR.
        v_caract = v_caract + caracteristicas_quimicas.descripcion + CHR(10).
        v_valores = v_valores + general.items_protocolos.valor_caracter + CHR(10).
    END.
    
    OUTPUT STREAM DataStream TO VALUE(lv_cDataFile).                              /* create data file */
     
    EXPORT STREAM DataStream DELIMITER "~t":U                                 /* export the data */
        STRING(MONTH(protocolos.fecha),"99") + "/" + 
        STRING(DAY(protocolos.fecha),"99")   + "/" + 
        STRING(YEAR(protocolos.fecha),">>99") 
        IF AVAILABLE contramarcas THEN UPPER(contramarcas.descripcion) ELSE "NONE"
        IF AVAILABLE lotes_aceite THEN STRING(lotes_aceite.id_lote,">999") + "/" + SUBSTRING(STRING(lotes_aceite.anio,">>99"),3,2) ELSE "NONE"
        IF AVAILABLE productos_terminados THEN UPPER(productos_terminados.descripcion_ingles) ELSE "NONE"
        IF AVAILABLE envases_muestras THEN UPPER(envases_muestras.descripcion_ingles) ELSE ""
        IF AVAILABLE envases_muestras THEN STRING(items_muestras.cantidad) + " X " + UPPER(envases_muestras.volume) ELSE ""
        STRING(industria.protocolos.cantidad_tambores,">99") + "/" + STRING(industria.protocolos.cantidad_tambores,">99")
        STRING(industria.protocolos.peso_neto)
        STRING(industria.protocolos.desde_tambor,">99") + "/" + STRING(industria.protocolos.cantidad_tambores,">99") +
        " TO " + STRING(industria.protocolos.hasta_tambor,">99") + "/" + STRING(industria.protocolos.cantidad_tambores,">99")
        STRING(protocolos.id_protocolo,">999") + "/" + STRING(protocolos.anio,"99")
        v_caract
        v_valores
        IF AVAILABLE quimicos THEN UPPER(quimicos.titulo_firma) ELSE "NONE"
        IF AVAILABLE quimicos THEN UPPER(industria.quimicos.nombre_ingles) ELSE "NONE"
        .
    END.
      
    OUTPUT STREAM DataStream CLOSE.                                                  /* close data file */
     
    /* CREO EL DOCUMENTO DE WORD */
    CREATE "Word.Application" pv_chWord NO-ERROR.     
    ASSIGN pv_chWord:Visible = YES.                                                  /* Word is not "hidden" from user */
      
    RUN p_MailMerge.p(lv_cDocument,                                                  /* Main Document */
                      lv_cDataFile,                                                  /* File that holds all the data */
                      lv_cMailMergeFile,                                             /* File to hold new mail merge document */
                      NO,
                      pv_chWord).                                                    /* Automatically Print New Mail Merge Document */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-37
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-37 B-table-Win
ON CHOOSE OF BUTTON-37 IN FRAME F-Main /* Prot. Cascara */
DO:
DEF VAR lv_iSeq     AS INT NO-UNDO.
DEF VAR lv_iPercent AS INT NO-UNDO.
DEF VAR lv_iTotal   AS INT NO-UNDO.
DEF VAR lv_cDocument      AS CHAR INIT "protocolo_cascara.doc":U      NO-UNDO. 
DEF VAR lv_cDataFile      AS CHAR INIT "protocolo_cascara.dat":U NO-UNDO. 
DEF VAR lv_cMailMergeFile AS CHAR INIT "protocolo_cascaraHeader.doc":U NO-UNDO.
                                       /* Pointer to Document Object */
DEFINE VAR r_prot AS ROWID.
DEFINE VAR v_humedad AS CHAR.
DEFINE VAR v_azucar AS CHAR.
DEFINE VAR v_color AS CHAR.

/*
RUN get-rowid1 IN h_b_protocolos (OUTPUT r_prot).

FIND FIRST protocolos WHERE ROWID(protocolos) = r_prot NO-LOCK NO-ERROR. */
IF AVAILABLE protocolos THEN DO:
    FIND FIRST contramarcas OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST quimicos OF protocolos NO-LOCK NO-ERROR.
    FIND FIRST envases_muestras OF protocolos NO-LOCK NO-ERROR.

    FOR EACH items_protocolos OF protocolos NO-LOCK.
        CASE items_protocolos.id_caracteristica:
            WHEN 31 THEN DO:
                /* HUMEDAD */
                v_humedad = general.items_protocolos.valor_caracter.
            END.
            WHEN 32 THEN DO:
                /* AZUCAR */
                v_azucar = general.items_protocolos.valor_caracter.
            END.
            WHEN 33 THEN DO:
                /* COLOR */
                v_color = general.items_protocolos.valor_caracter.
            END.
        END CASE.
    END.
    OUTPUT STREAM DataStream TO VALUE(lv_cDataFile).                              /* create data file */
     
    EXPORT STREAM DataStream DELIMITER "~t":U                                 /* export the data */
        STRING(MONTH(protocolos.fecha),"99") + "/" + 
        STRING(DAY(protocolos.fecha),"99")   + "/" + 
        STRING(YEAR(protocolos.fecha),">>99") 
        STRING(protocolos.id_protocolo,">999")
        STRING(protocolos.anio,">>99")
        IF AVAILABLE contramarcas THEN UPPER(contramarcas.descripcion) ELSE "NONE"
        STRING(protocolos.lote,">>99")
        STRING(protocolos.anio_lote,">>99")
        IF protocolos.tipo_cascara THEN "STANDARD" ELSE "FINE"
        IF AVAILABLE envases_muestras THEN UPPER(envases_muestras.descripcion_ingles) ELSE "NONE"
        IF AVAILABLE productos_terminados THEN UPPER(productos_terminados.descripcion_ingles) ELSE "NONE"
        v_humedad
        v_azucar
        IF AVAILABLE quimicos THEN UPPER(quimicos.titulo) ELSE "NONE"
        IF AVAILABLE quimicos THEN UPPER(industria.quimicos.nombre_ingles) ELSE "NONE"
        UPPER(v_color).
    END.
      
    OUTPUT STREAM DataStream CLOSE.                                                   /* close data file */
     
    /* CREO EL DOCUMENTO DE WORD */
    CREATE "Word.Application" pv_chWord NO-ERROR.     
    ASSIGN pv_chWord:Visible = YES.                                                   /* Word is not "hidden" from user */
      
    RUN p_MailMerge.p(lv_cDocument,                                                       /* Main Document */
                      lv_cDataFile,                                                       /* File that holds all the data */
                      lv_cMailMergeFile,                                                  /* File to hold new mail merge document */
                      NO,
                      pv_chWord).                                                                /* Automatically Print New Mail Merge Document */

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
  menu-item protocolos-id_protocolo
label 'id_protocolo( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.id_protocolo').
                  run set-attribute-list('titulo=CodProtocolo').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.id_protocolo
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-anio
label 'anio( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.anio').
                  run set-attribute-list('titulo=Año').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.anio
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item protocolos-fecha_aprobacion
label 'fecha_aprobacion( protocolos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=protocolos.fecha_aprobacion').
                  run set-attribute-list('titulo=Fecha Aprobacion').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by protocolos.fecha_aprobacion
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item quimicos-nombre_ingles
label 'nombre_ingles( quimicos ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=quimicos.nombre_ingles').
                  run set-attribute-list('titulo=Nombre Completo Ingles').
                  run set-attribute-list('busca= no').
                  &SCOPE SORTBY-PHRASE  by quimicos.nombre_ingles
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by protocolos.id_protocolo
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=protocolos.id_protocolo').
run set-attribute-list('titulo=CodProtocolo').
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "muestras"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "muestras"}

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
  {src/adm/template/sndkycas.i "id_color" "protocolos" "id_color"}
  {src/adm/template/sndkycas.i "id_contramarca" "protocolos" "id_contramarca"}
  {src/adm/template/sndkycas.i "id_articulo" "protocolos" "id_articulo"}
  {src/adm/template/sndkycas.i "id_protocolo" "protocolos" "id_protocolo"}
  {src/adm/template/sndkycas.i "id_quimico" "protocolos" "id_quimico"}
  {src/adm/template/sndkycas.i "id_tipotambor" "protocolos" "id_tipotambor"}
  {src/adm/template/sndkycas.i "id_tipo_protocolo" "protocolos" "id_tipo_protocolo"}

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
  {src/adm/template/snd-list.i "muestras"}
  {src/adm/template/snd-list.i "items_muestras"}
  {src/adm/template/snd-list.i "r_muestras_protocolos"}
  {src/adm/template/snd-list.i "protocolos"}
  {src/adm/template/snd-list.i "quimicos"}
  {src/adm/template/snd-list.i "tambores_industria"}

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

