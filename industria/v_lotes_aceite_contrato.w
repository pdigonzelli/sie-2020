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
&Scoped-Define ENABLED-FIELDS lotes_aceite.id_contrato_of ~
lotes_aceite.id_tipocontrato_of lotes_aceite.anio_of 
&Scoped-define ENABLED-TABLES lotes_aceite
&Scoped-define FIRST-ENABLED-TABLE lotes_aceite
&Scoped-Define ENABLED-OBJECTS tambores_seleccionados tambores_asignados ~
tambores_free 
&Scoped-Define DISPLAYED-FIELDS lotes_aceite.id_contrato_of ~
lotes_aceite.id_tipocontrato_of lotes_aceite.anio_of 
&Scoped-define DISPLAYED-TABLES lotes_aceite
&Scoped-define FIRST-DISPLAYED-TABLE lotes_aceite
&Scoped-Define DISPLAYED-OBJECTS tambores_seleccionados orden_fabricacion ~
nombre_cliente tambores_asignados tambores_free 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS lotes_aceite.id_tipocontrato_of ~
lotes_aceite.anio_of 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_tipotambor||y|general.lotes_aceite.id_tipotambor
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_tipotambor"':U).
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
DEFINE VARIABLE nombre_cliente AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE orden_fabricacion AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Orden de Fabricacion" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tambores_asignados AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Tambores Asignados" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tambores_free AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Tambores Disponibles" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 10 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE tambores_seleccionados AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Tambores" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lotes_aceite.id_contrato_of AT ROW 1 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     lotes_aceite.id_tipocontrato_of AT ROW 2 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     tambores_seleccionados AT ROW 2.91 COL 56 COLON-ALIGNED
     lotes_aceite.anio_of AT ROW 3 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     orden_fabricacion AT ROW 4.81 COL 27 COLON-ALIGNED
     nombre_cliente AT ROW 5.76 COL 27 COLON-ALIGNED
     tambores_asignados AT ROW 6.95 COL 27 COLON-ALIGNED
     tambores_free AT ROW 6.95 COL 61 COLON-ALIGNED
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
         HEIGHT             = 7.14
         WIDTH              = 79.6.
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

/* SETTINGS FOR FILL-IN lotes_aceite.anio_of IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN lotes_aceite.id_tipocontrato_of IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN nombre_cliente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN orden_fabricacion IN FRAME F-Main
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



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME lotes_aceite.anio_of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.anio_of V-table-Win
ON LEAVE OF lotes_aceite.anio_of IN FRAME F-Main /* Año */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_contrato_of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_contrato_of V-table-Win
ON LEAVE OF lotes_aceite.id_contrato_of IN FRAME F-Main /* Contract */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_contrato_of V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_aceite.id_contrato_of IN FRAME F-Main /* Contract */
DO:
  run carga_contrato.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_tipocontrato_of
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_tipocontrato_of V-table-Win
ON LEAVE OF lotes_aceite.id_tipocontrato_of IN FRAME F-Main /* Tipo Contrato */
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


&Scoped-define SELF-NAME orden_fabricacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_fabricacion V-table-Win
ON LEAVE OF orden_fabricacion IN FRAME F-Main /* Orden de Fabricacion */
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


&Scoped-define SELF-NAME tambores_seleccionados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_seleccionados V-table-Win
ON LEAVE OF tambores_seleccionados IN FRAME F-Main /* Tambores */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
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
RUN descriptivos1.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga_contrato V-table-Win 
PROCEDURE carga_contrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define var r1 as ROWID.
    define var v_t as integer initial 0.
    
    run wc_contratos.w (output r1).
    FIND contratos WHERE ROWID(contratos) = r1 NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN
    DO:
        lotes_aceite.id_contrato_of:screen-value in frame F-Main = string(contratos.id_contrato).
        lotes_aceite.id_tipocontrato_of:screen-value in frame F-Main  = string(contratos.id_tipo_contrato).
        lotes_aceite.anio_of:screen-value  in frame F-Main = string(contratos.anio).
        
        /*
        for each tambores_industria of lotes_aceite.
            tambores_industria.id_contrato_of = contratos.id_contrato.
            tambores_industria.id_tipocontrato_of = contratos.id_tipo_contrato.
            tambores_industria.anio_of = contratos.anio.
            v_t = v_t + 1.
        end.    
        message "Se han modificado " v_t " tambores." view-as alert-box.
        */

        orden_fabricacion:screen-value  in frame F-Main = string(contratos.orden_fabricacion). 
        FIND clientes OF contratos NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN nombre_cliente:SCREEN-VALUE in frame F-Main  = clientes.nombre.
    END.
        
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

FIND FIRST contratos WHERE contratos.id_contrato = lotes_aceite.id_contrato_of:screen-value in frame F-Main
                       AND contratos.id_tipo_contrato = integer(lotes_aceite.id_tipocontrato_of:screen-value in frame F-Main)
                       AND contratos.anio = integer(lotes_aceite.anio_of:screen-value in frame F-Main)
                        NO-LOCK NO-ERROR.
IF AVAILABLE contratos THEN DO:
    v_tambores = integer(tambores_seleccionados:screen-value in frame F-Main).    
    IF v_tambores > 0 THEN DO:
        DO i = 1 TO v_tambores: /* ACA ENTRA SI ES UNA CANTIDAD DETERMINADA DE TAMBORES */
            FIND FIRST tambores_industria WHERE tambores_industria.id_contrato_of = ""
                                            AND tambores_industria.id_tipocontrato_of = 0
                                            AND tambores_industria.anio_of = 0
                                            AND tambores_industria.id_empresa = lotes_aceite.id_empresa
                                            AND tambores_industria.id_sucursal = lotes_aceite.id_sucursal
                                            AND tambores_industria.id_tipotambor = lotes_aceite.id_tipotambor
                                            AND tambores_industria.nromov = lotes_aceite.nromov
                                            NO-ERROR.
            IF AVAILABLE tambores_industria THEN DO:
              ASSIGN iEmp = tambores_industria.id_empresa
                     iSuc = tambores_industria.id_sucursal
                     iTip = tambores_industria.id_tipotambor
                     iNro = tambores_industria.nromov.

                /* RELACIONA LOS TAMBORES CON LA PARTE DE OE*/
                ASSIGN tambores_industria.id_contrato_of = 
                       lotes_aceite.id_contrato_of:screen-value in frame F-Main
                       tambores_industria.id_tipocontrato_of = 
                       integer(lotes_aceite.id_tipocontrato_of:screen-value in frame F-Main)
                       tambores_industria.anio_of = 
                       integer(lotes_aceite.anio_of:screen-value in frame F-Main).
                v_t = v_t + 1.        
            END.
            ELSE MESSAGE "No se encontraron tambores" view-as alert-box.
        END.

        RUN libTamboresIndustria.p PERSISTENT SET hLib.
        RUN mailingAsocTamboresOF IN hLib (iEmp,
                                           iSuc,
                                           iTip,
                                           iNro,
                                           1, 
                                           v_t, 
                                           lotes_aceite.id_contrato_of:SCREEN-VALUE IN FRAME F-Main).

        MESSAGE "Se han modificado " v_t " tambores del lote." view-as alert-box.
    END.
    ELSE DO:
        /* ENTRO CUANDO QUIERO ASIGNAR TODOS LOS TAMBORES DE UN LOTE */
        FOR EACH tambores_industria OF lotes_jugo.
            ASSIGN tambores_industria.id_contrato_of = 
                   lotes_aceite.id_contrato_of:screen-value in frame F-Main
                   tambores_industria.id_tipocontrato_of = 
                   integer(lotes_aceite.id_tipocontrato_of:screen-value in frame F-Main)
                   tambores_industria.anio_of = 
                   integer(lotes_aceite.anio_of:screen-value in frame F-Main).

            v_t = v_t + 1.        
        END.
        MESSAGE "Se han modificado todos los tambores(" v_t ") del lote." view-as alert-box.
    END.  


END.
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
DEFINE VAR v_tam_asig AS INTEGER.
DEFINE VAR v_tam_free AS INTEGER.


FOR EACH tambores_industria OF lotes_aceite.
    IF tambores_industria.id_contrato_of = "" AND
       tambores_industria.id_tipocontrato_of = 0 AND
       tambores_industria.anio_of = 0 THEN
        v_tam_free = v_tam_free + 1.
    ELSE
        v_tam_asig = v_tam_asig + 1.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE limpiar_campos V-table-Win 
PROCEDURE limpiar_campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
lotes_aceite.id_contrato_of:screen-value in frame F-Main = "".
lotes_aceite.id_tipocontrato_of:screen-value in frame F-Main  = "".
lotes_aceite.anio_of:screen-value  in frame F-Main = "".
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
  RUN descriptivos2.
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
define var r as rowid.
define var h_con as handle.

run get-container (output h_con).
run paso_lote in h_con (output r).

find lotes_aceite where rowid(lotes_aceite) = r no-lock no-error.
FIND contratos    WHERE contratos.id_contrato = lotes_aceite.id_contrato_of
                          AND contratos.anio = lotes_aceite.anio_of
                          AND contratos.id_tipo_contrato = lotes_aceite.id_tipocontrato_of 
                            NO-LOCK no-error.
IF AVAILABLE contratos THEN
    DO:
        orden_fabricacion:SCREEN-VALUE IN FRAME F-Main = string(contratos.orden_fabricacion). 
    END.
     
FIND clientes OF contratos NO-LOCK NO-ERROR.
IF AVAILABLE clientes THEN
    nombre_cliente:SCREEN-VALUE IN FRAME F-Main = clientes.nombre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "id_tipotambor" "lotes_aceite" "id_tipotambor"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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

