&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES lotes_aceite
&Scoped-define FIRST-EXTERNAL-TABLE lotes_aceite


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lotes_aceite.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS lotes_aceite.id_orden_entrega ~
lotes_aceite.item_oe 
&Scoped-define ENABLED-TABLES lotes_aceite
&Scoped-define FIRST-ENABLED-TABLE lotes_aceite
&Scoped-Define ENABLED-OBJECTS tambores tambores_asignados tambores_free 
&Scoped-Define DISPLAYED-FIELDS lotes_aceite.id_orden_entrega ~
lotes_aceite.item_oe 
&Scoped-define DISPLAYED-TABLES lotes_aceite
&Scoped-define FIRST-DISPLAYED-TABLE lotes_aceite
&Scoped-Define DISPLAYED-OBJECTS tambores tambores_asignados tambores_free 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
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
DEFINE VARIABLE tambores AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Tambores" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE tambores_asignados AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Tambores Asignados" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tambores_free AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Tambores Disponibles" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 10 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lotes_aceite.id_orden_entrega AT ROW 1 COL 8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     lotes_aceite.item_oe AT ROW 1 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     tambores AT ROW 1 COL 57 COLON-ALIGNED
     tambores_asignados AT ROW 2.43 COL 21 COLON-ALIGNED
     tambores_free AT ROW 2.43 COL 57 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.lotes_aceite
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 2.91
         WIDTH              = 69.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/cviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

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
general.lotes_aceite.id_orden_entrega;wc_orden_entrega.w;orden_entrega.;;
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



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME lotes_aceite.id_orden_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_orden_entrega V-table-Win
ON LEAVE OF lotes_aceite.id_orden_entrega IN FRAME F-Main /* OE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_orden_entrega V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_aceite.id_orden_entrega IN FRAME F-Main /* OE */
do: 
define var r as rowid no-undo.
run wc_items_orden_entrega.w(output r).
find items_orden_entrega where rowid(items_orden_entrega) = r no-lock no-error.
if available items_orden_entrega then 
    do:
        general.lotes_aceite.id_orden_entrega:screen-value = string(orden_entrega.id_orden_entrega).
        general.lotes_aceite.item_oe:screen-value = string(items_orden_entrega.item_oe).
        /* run descriptivos1. */
    end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.item_oe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.item_oe V-table-Win
ON LEAVE OF lotes_aceite.item_oe IN FRAME F-Main /* Parte OE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores V-table-Win
ON LEAVE OF tambores IN FRAME F-Main /* Tambores */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_asignados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_asignados V-table-Win
ON LEAVE OF tambores_asignados IN FRAME F-Main /* Tambores Asignados */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_free
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_free V-table-Win
ON LEAVE OF tambores_free IN FRAME F-Main /* Tambores Disponibles */
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
run descriptivos1.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos1 V-table-Win 
PROCEDURE descriptivos1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var v_t as integer.
define var v_tambores as integer.
define var i as integer.

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE iEmp AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSuc AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTip AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.

RUN libTamboresIndustria.p PERSISTENT SET hLib.

FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = integer(lotes_aceite.id_orden_entrega:screen-value in frame F-Main)
                                 AND items_orden_entrega.item_oe          = integer(lotes_aceite.item_oe:screen-value in frame F-Main)
                                NO-LOCK NO-ERROR.
IF AVAILABLE items_orden_entrega OR integer(lotes_aceite.id_orden_entrega:screen-value in frame F-Main) = 0 THEN DO:
    IF lotes_aceite.control_calidad THEN DO:
       /* PARA PODER ASIGNAR TIENE QUE ESTAR EL LOTE APROBADO POR CONTROL DE CALIDAD*/
            
        v_tambores = integer(tambores:screen-value in frame F-Main).
        if v_tambores > 0 then    
            do:
                do i = 1 to v_tambores:
                    find first tambores_industria where tambores_industria.id_orden_entrega = 0
                                                    and tambores_industria.item_oe = 0
                                                    and tambores_industria.id_empresa = lotes_aceite.id_empresa
                                                    and tambores_industria.id_sucursal = lotes_aceite.id_sucursal
                                                    and tambores_industria.id_tipotambor = lotes_aceite.id_tipotambor
                                                    and tambores_industria.nromov = lotes_aceite.nromov.
                    ASSIGN iEmp = tambores_industria.id_empresa 
                           iSuc = tambores_industria.id_sucursal
                           iTip = tambores_industria.id_tipotambor
                           iNro = tambores_industria.nromov.

                    assign tambores_industria.id_orden_entrega = 
                           integer(lotes_aceite.id_orden_entrega:screen-value in frame F-Main)
                           tambores_industria.ITEM_oe = 
                           integer(lotes_aceite.ITEM_oe:screen-value in frame F-Main).

                    IF AVAILABLE items_orden_entrega THEN DO:
                        /* RELACIONO LOS TAMBORES A LA PARTE DE CONTRATO TAMBIEN */
                        ASSIGN tambores_industria.id_contrato_of        = items_orden_entrega.id_contrato
                               tambores_industria.id_tipocontrato_of    = items_orden_entrega.id_tipo_contrato
                               tambores_industria.anio_of               = items_orden_entrega.anio
                               tambores_industria.item_of               = items_orden_entrega.ITEM.
                    END.
                    v_t = v_t + 1.        
                end.
            end.
        else
            do:
                for each tambores_industria of lotes_aceite.
                    assign tambores_industria.id_orden_entrega = 
                           integer(lotes_aceite.id_orden_entrega:screen-value in frame F-Main)
                           tambores_industria.ITEM_oe = 
                           integer(lotes_aceite.ITEM_oe:screen-value in frame F-Main).
                    
                    IF AVAILABLE items_orden_entrega THEN DO:
                        /* RELACIONO LOS TAMBORES A LA PARTE DE CONTRATO TAMBIEN */
                        ASSIGN tambores_industria.id_contrato_of        = items_orden_entrega.id_contrato
                               tambores_industria.id_tipocontrato_of    = items_orden_entrega.id_tipo_contrato
                               tambores_industria.anio_of               = items_orden_entrega.anio
                               tambores_industria.item_of               = items_orden_entrega.ITEM.
                    END.
                    ELSE DO:
                        /* ACA ENTRA SI ESTA BORRANDO LA RELACION CON UNA OE EN PARTICULAR
                           ENTONCES BORRO LA RELACION A LOS CONTRATOS ??????? */
                        ASSIGN tambores_industria.id_contrato_of        = ""
                               tambores_industria.id_tipocontrato_of    = 0
                               tambores_industria.anio_of               = 0
                               tambores_industria.item_of               = 0.
                    END.

                    v_t = v_t + 1.        
                end.
            end.    
                
        message "Se han modificado " v_t " tambores." view-as alert-box.

        RUN mailingTamboresItemOE IN hLib (iEmp,
                                           iSuc,
                                           iTip,
                                           iNro,
                                           1, 
                                           v_t, 
                                           INTEGER(lotes_aceite.id_orden_entrega:SCREEN-VALUE IN FRAME F-Main), 
                                           INTEGER(lotes_aceite.ITEM_oe:SCREEN-VALUE IN FRAME F-Main)).

 
    end. /* END DEL IF CONTROL_CALIDAD */
    ELSE 
        DO:
            lotes_aceite.id_orden_entrega:screen-value in frame F-Main = "".
            lotes_aceite.id_orden_entrega = 0.
            lotes_aceite.item_oe = 0.
            message "El lote NO esta aprobado por Control de Calidad!" view-as alert-box.
        END.
END. /* END DEL IF AVAILABLE ITEMS_ORDEN_ENTREGA */
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos2 V-table-Win 
PROCEDURE descriptivos2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var v_tam_asig as integer.
define var v_tam_free as integer.


for each tambores_industria of lotes_aceite
                            WHERE tambores_industria.id_locacion_ubicacion = 4.
    if tambores_industria.id_orden_entrega = 0 then
        v_tam_free = v_tam_free + 1.
    else
        v_tam_asig = v_tam_asig + 1.
end.

tambores_asignados:screen-value in frame F-Main = string(v_tam_asig).
tambores_free:screen-value in frame F-Main = string(v_tam_free).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
define var lista_relacion as character no-undo initial "id_orden_entrega".
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
  run descriptivos2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
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

