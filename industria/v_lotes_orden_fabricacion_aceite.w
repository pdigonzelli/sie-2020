&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES lotes_jugo
&Scoped-define FIRST-EXTERNAL-TABLE lotes_jugo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lotes_jugo.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS lotes_jugo.id_contrato_of ~
lotes_jugo.id_tipocontrato_of lotes_jugo.anio_of lotes_jugo.item_of 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}id_contrato_of ~{&FP2}id_contrato_of ~{&FP3}~
 ~{&FP1}id_tipocontrato_of ~{&FP2}id_tipocontrato_of ~{&FP3}~
 ~{&FP1}anio_of ~{&FP2}anio_of ~{&FP3}~
 ~{&FP1}item_of ~{&FP2}item_of ~{&FP3}
&Scoped-define ENABLED-TABLES lotes_jugo
&Scoped-define FIRST-ENABLED-TABLE lotes_jugo
&Scoped-Define DISPLAYED-FIELDS lotes_jugo.id_contrato_of ~
lotes_jugo.id_tipocontrato_of lotes_jugo.anio_of lotes_jugo.item_of 
&Scoped-Define DISPLAYED-OBJECTS semana_entrega kilos nombre_cliente 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_tipotambor||y|general.lotes_jugo.id_tipotambor
id_tipolimon||y|general.lotes_jugo.id_tipolimon
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_tipotambor,id_tipolimon"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valida V-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE kilos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kilos" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE nombre_cliente AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE semana_entrega AS CHARACTER FORMAT "X(2)":U 
     LABEL "Semana de entrega" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lotes_jugo.id_contrato_of AT ROW 1 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     lotes_jugo.id_tipocontrato_of AT ROW 1.95 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     lotes_jugo.anio_of AT ROW 2.91 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     lotes_jugo.item_of AT ROW 3.91 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     semana_entrega AT ROW 6.19 COL 27 COLON-ALIGNED
     kilos AT ROW 6.19 COL 49 COLON-ALIGNED
     nombre_cliente AT ROW 7.14 COL 27 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.lotes_jugo
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.81
         WIDTH              = 79.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN kilos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre_cliente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN semana_entrega IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "campos" V-table-Win _INLINE
/* Actions: custom/support/cuscampv.p custom/support/cuscampv.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "relaciones" V-table-Win _INLINE
/* Actions: custom/support/keyedit.w custom/support/keyedit.w ? ? ? */
/* campos relacionados con tablas externas 
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "borrado" V-table-Win _INLINE
/* Actions: ? custom/support/cusborfv.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Cabecera" V-table-Win _INLINE
/* Actions: ? custom/support/set-cabecera.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Detalle" V-table-Win _INLINE
/* Actions: ? custom/support/set-detalle.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Items" V-table-Win _INLINE
/* Actions: ? custom/support/set-items.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/cviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME lotes_jugo.anio_of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.anio_of V-table-Win
ON LEAVE OF lotes_jugo.anio_of IN FRAME F-Main /* A�o */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_contrato_of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_contrato_of V-table-Win
ON LEAVE OF lotes_jugo.id_contrato_of IN FRAME F-Main /* Contract */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_contrato_of V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_jugo.id_contrato_of IN FRAME F-Main /* Contract */
DO:
    define var r1 as ROWID.
    run wc_items_contratos.w (output r1).
    FIND items_contratos WHERE ROWID(items_contratos) = r1 NO-LOCK NO-ERROR.
    IF AVAILABLE items_contratos THEN
    DO:
        lotes_jugo.id_contrato_of:screen-value = string(items_contratos.id_contrato).
        lotes_jugo.id_tipocontrato_of:screen-value = string(items_contratos.id_tipo_contrato).
        lotes_jugo.anio_of:screen-value = string(items_contratos.anio).
        lotes_jugo.item_of:screen-value = string(items_contratos.ITEM).
/*        lotes_jugo.id_envio_of:screen-value = string(envios_items.id_envio). */
        semana_entrega:screen-value = string(items_contratos.semana_entrega). 
        find contratos of items_contratos no-lock no-error.
        FIND clientes OF contratos NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN nombre_cliente:SCREEN-VALUE = clientes.nombre.
    END.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_tipocontrato_of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_tipocontrato_of V-table-Win
ON LEAVE OF lotes_jugo.id_tipocontrato_of IN FRAME F-Main /* Tipo Contrato */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.item_of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.item_of V-table-Win
ON LEAVE OF lotes_jugo.item_of IN FRAME F-Main /* Item */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kilos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kilos V-table-Win
ON LEAVE OF kilos IN FRAME F-Main /* Kilos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nombre_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nombre_cliente V-table-Win
ON LEAVE OF nombre_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME semana_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL semana_entrega V-table-Win
ON LEAVE OF semana_entrega IN FRAME F-Main /* Semana de entrega */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
 
 
  /************************ INTERNAL PROCEDURES ********************/
{custom/support/vinternal.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-create V-table-Win 
PROCEDURE adm-post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-delete V-table-Win 
PROCEDURE adm-post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-update V-table-Win 
PROCEDURE adm-post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-create V-table-Win 
PROCEDURE adm-pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-delete V-table-Win 
PROCEDURE adm-pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE agregar V-table-Win 
PROCEDURE agregar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define var ch as character no-undo.
    define var i as integer no-undo.
    define var h as handle no-undo.
    run get-link-handle in adm-broker-hdl 
        ( input this-procedure , input 'TABLEIO-SOURCE' , output ch). 
    do i = 1 to num-entries(ch) :
        h = widget-handle(entry(i,ch)).
        if valid-handle(h) then
            run agregar in h.     
    end. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos V-table-Win 
PROCEDURE descriptivos :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_campos V-table-Win 
PROCEDURE deshabilita_campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista_campos as character.
define var i as integer no-undo.
define var f as handle no-undo.
define var h as handle no-undo.


do i = 1 to num-entries(lista_campos):
    f = frame f-main:first-child.
    h = f:first-tab-item.
    do while valid-handle(h):
        if h:name = entry(i,lista_campos) then
        do:
            h:sensitive = false.
            leave.
        end.    
        h = h:next-tab-item.
    end.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grabar V-table-Win 
PROCEDURE grabar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define var ch as character no-undo.
    define var i as integer no-undo.
    define var h as handle no-undo.
    run get-link-handle in adm-broker-hdl 
        ( input this-procedure , input 'TABLEIO-SOURCE' , output ch). 
    do i = 1 to num-entries(ch) :
        h = widget-handle(entry(i,ch)).
        if valid-handle(h) then
        do:
            run activa in h.
            run grabar in h.
        end.        
    end. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilitar_relacion V-table-Win 
PROCEDURE habilitar_relacion :
define var field-group as handle.
define var cur-control as handle.
define var lista_relacion as character no-undo initial "".
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  DEFINE VAR CVINCULADO AS CHARACTER NO-UNDO.
  RUN GET-LINK-HANDLE IN ADM-BROKER-HDL ( THIS-PROCEDURE , 'VINCULADO-SOURCE' , OUTPUT CVINCULADO ).
  HVINCULADO = WIDGET-HANDLE(CVINCULADO).
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run habilitar_relacion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run descriptivos.
  RUN mi_descriptivos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mi_descriptivos V-table-Win 
PROCEDURE mi_descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND items_contratos    WHERE items_contratos.id_contrato = lotes_jugo.id_contrato_of
                          AND items_contratos.anio = lotes_jugo.anio_of
                          AND items_contratos.ITEM = lotes_jugo.item_of
                          AND items_contratos.id_tipo_contrato = lotes_jugo.id_tipocontrato_of 
                            NO-LOCK NO-ERROR.
IF AVAILABLE items_contratos THEN
    DO:
        semana_entrega:SCREEN-VALUE IN FRAME F-Main = string(items_contratos.semana_entrega). 
        kilos:SCREEN-VALUE IN FRAME F-Main          = STRING(cantidad).
    END.
     
find contratos of items_contratos no-error.
FIND clientes OF contratos NO-LOCK NO-ERROR.
IF AVAILABLE clientes THEN
    nombre_cliente:SCREEN-VALUE IN FRAME F-Main = clientes.nombre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "id_tipotambor" "lotes_jugo" "id_tipotambor"}
  {src/adm/template/sndkycas.i "id_tipolimon" "lotes_jugo" "id_tipolimon"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
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

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION valida V-table-Win 
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


