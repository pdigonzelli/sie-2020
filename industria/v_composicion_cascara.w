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
DEFINE VAR v_del_emp AS INTEGER NO-UNDO.
DEFINE VAR v_del_suc AS INTEGER NO-UNDO.
DEFINE VAR v_del_tip AS INTEGER NO-UNDO.
DEFINE VAR v_del_nro AS INTEGER NO-UNDO.
DEFINE VAR v_del_des AS INTEGER NO-UNDO.
DEFINE VAR v_del_has AS INTEGER NO-UNDO.
DEFINE VAR iDesde    AS INTEGER NO-UNDO.
DEFINE VAR iHasta    AS INTEGER NO-UNDO.
DEFINE VAR iTotal    AS INTEGER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES composicion_lote
&Scoped-define FIRST-EXTERNAL-TABLE composicion_lote


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR composicion_lote.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS composicion_lote.Fecha ~
composicion_lote.Numeracion_desde composicion_lote.Numeracion_hasta ~
composicion_lote.cantidad_tambores composicion_lote.kilos_tambor 
&Scoped-define ENABLED-TABLES composicion_lote
&Scoped-define FIRST-ENABLED-TABLE composicion_lote
&Scoped-define DISPLAYED-TABLES composicion_lote
&Scoped-define FIRST-DISPLAYED-TABLE composicion_lote
&Scoped-Define DISPLAYED-FIELDS composicion_lote.Fecha ~
composicion_lote.Numeracion_desde composicion_lote.Numeracion_hasta ~
composicion_lote.cantidad_tambores composicion_lote.kilos_tambor 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS composicion_lote.Fecha ~
composicion_lote.Numeracion_desde composicion_lote.Numeracion_hasta ~
composicion_lote.cantidad_tambores composicion_lote.kilos_tambor 

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

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     composicion_lote.Fecha AT ROW 1 COL 21 COLON-ALIGNED FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     composicion_lote.Numeracion_desde AT ROW 2 COL 21 COLON-ALIGNED
          LABEL "Desde Bolsa" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     composicion_lote.Numeracion_hasta AT ROW 3 COL 21 COLON-ALIGNED
          LABEL "Hasta" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     composicion_lote.cantidad_tambores AT ROW 4 COL 21 COLON-ALIGNED
          LABEL "Cantidad Bolsas" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     composicion_lote.kilos_tambor AT ROW 5 COL 21 COLON-ALIGNED
          LABEL "Kilos" FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.composicion_lote
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 5
         WIDTH              = 41.6.
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

/* SETTINGS FOR FILL-IN composicion_lote.cantidad_tambores IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN composicion_lote.Fecha IN FRAME F-Main
   1 EXP-FORMAT                                                         */
/* SETTINGS FOR FILL-IN composicion_lote.kilos_tambor IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN composicion_lote.Numeracion_desde IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN composicion_lote.Numeracion_hasta IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
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

&Scoped-define SELF-NAME composicion_lote.cantidad_tambores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL composicion_lote.cantidad_tambores V-table-Win
ON LEAVE OF composicion_lote.cantidad_tambores IN FRAME F-Main /* Cantidad Bolsas */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME composicion_lote.Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL composicion_lote.Fecha V-table-Win
ON LEAVE OF composicion_lote.Fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME composicion_lote.kilos_tambor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL composicion_lote.kilos_tambor V-table-Win
ON LEAVE OF composicion_lote.kilos_tambor IN FRAME F-Main /* Kilos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME composicion_lote.Numeracion_desde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL composicion_lote.Numeracion_desde V-table-Win
ON LEAVE OF composicion_lote.Numeracion_desde IN FRAME F-Main /* Desde Bolsa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME composicion_lote.Numeracion_hasta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL composicion_lote.Numeracion_hasta V-table-Win
ON LEAVE OF composicion_lote.Numeracion_hasta IN FRAME F-Main /* Hasta */
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
DEFINE VAR v_desde AS INTEGER.
DEFINE VAR v_hasta AS INTEGER.
DEFINE VAR i AS INTEGER.
DEFINE VAR hcon AS HANDLE.
DEFINE VAR r AS ROWID.

RUN get-container (OUTPUT hcon).
RUN dame-rowid-lote IN hcon (OUTPUT r).

FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.
IF AVAILABLE lotes_jugo THEN DO:
  ASSIGN composicion_lote.id_empresa    = lotes_jugo.id_empresa
         composicion_lote.id_sucursal   = lotes_jugo.id_sucursal
         composicion_lote.id_tipotambor = lotes_jugo.id_tipotambor
         composicion_lote.nromov        = lotes_jugo.nromov
         composicion_lote.c_usuario     = USERID("userdb") 
         composicion_lote.c_fecha       = TODAY
         composicion_lote.c_hora        = STRING(TIME, "HH:MM:SS").
END.

v_desde = INTEGER(composicion_lote.numeracion_desde:SCREEN-VALUE IN FRAME F-Main).
v_hasta = INTEGER(composicion_lote.numeracion_hasta:SCREEN-VALUE IN FRAME F-Main).

DO i = v_desde TO v_hasta:
    FIND FIRST tambores_industria WHERE tambores_industria.id_tambor             = i
                                    AND tambores_industria.id_tipotambor         = 11
                                    AND tambores_industria.id_sucursal_ubicacion = lotes_jugo.id_sucursal
                                    AND tambores_industria.id_locacion_ubicacion = 4
                                    AND tambores_industria.nromov_destino        = 0
                                  NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      ASSIGN  tambores_industria.id_empresa_destino      = lotes_jugo.id_empresa
              tambores_industria.id_sucursal_destino     = lotes_jugo.id_sucursal
              tambores_industria.id_tipotambor_destino   = lotes_jugo.id_tipotambor
              tambores_industria.nromov_destino          = lotes_jugo.nromov
              .
       
       /* ANALIZAR BIEN EL MANEJO DE LAS BOLSAS EN STOCK. PORQUE NO TIENEN QUE APARECER DISPONIBLES
            PERO SI ESTAN EN STOCK(CREO) */
    END.
END.
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
DEFINE VAR i AS INTEGER.

DO i = v_del_des TO v_del_has:
    FIND FIRST tambores_industria WHERE tambores_industria.id_tambor                = i
                                    AND tambores_industria.id_tipotambor            = 11
                                    AND tambores_industria.id_empresa_destino       = v_del_emp
                                    AND tambores_industria.id_sucursal_destino      = v_del_suc
                                    AND tambores_industria.id_tipotambor_destino    = v_del_tip
                                    AND tambores_industria.nromov_destino           = v_del_nro
                                    AND tambores_industria.id_locacion_ubicacion    = 4
                                    NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
       ASSIGN tambores_industria.id_empresa_destino      = 0
              tambores_industria.id_sucursal_destino     = 0
              tambores_industria.id_tipotambor_destino   = 0
              tambores_industria.nromov_destino          = 0
              .
       /* ANALIZAR BIEN EL MANEJO DE LAS BOLSAS EN STOCK. PORQUE NO TIENEN QUE APARECER DISPONIBLES
            PERO SI ESTAN EN STOCK(CREO) */
    END.
    ELSE DO:
        MESSAGE "Ha ocurrido un error, por favor avisar a sistemas." VIEW-AS ALERT-BOX.
        RETURN "ADM-ERROR".
    END.

END.
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
DEFINE VAR vDesde AS INTEGER    NO-UNDO.
DEFINE VAR vHasta AS INTEGER    NO-UNDO.
DEFINE VAR i      AS INTEGER    NO-UNDO.
DEFINE VAR hcon   AS HANDLE     NO-UNDO.
DEFINE VAR r      AS ROWID      NO-UNDO.
DEFINE VAR strMsg AS CHARACTER  NO-UNDO.

RUN get-container (OUTPUT hcon).
RUN dame-rowid-lote IN hcon (OUTPUT r).
FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.

vDesde = INTEGER(composicion_lote.numeracion_desde:SCREEN-VALUE IN FRAME F-Main).
vHasta = INTEGER(composicion_lote.numeracion_hasta:SCREEN-VALUE IN FRAME F-Main).

DO i = vDesde TO vHasta:
  FIND FIRST tambores_industria WHERE tambores_industria.id_tambor = i
                                  AND tambores_industria.id_tipotambor = 11
                                  AND tambores_industria.id_sucursal_ubicacion = lotes_jugo.id_sucursal
                                  AND tambores_industria.id_locacion_ubicacion = 4
                                  AND tambores_industria.nromov_destino        = 0
                                NO-LOCK NO-ERROR.
  IF NOT AVAILABLE tambores_industria THEN DO:
    strMsg = "Bolsa " + STRING(tambores_industria.id_tambor) + CHR(13)
           + "id_sucursal_ubicacion " + STRING(tambores_industria.id_sucursal_ubicacion) + CHR(13)
           + "nro_mov_destino " + STRING(tambores_industria.nromov_destino) + CHR(13) + CHR(13).
  END.
END.

/*
IF strMsg <> "" THEN DO:
  MESSAGE strMsg VIEW-AS ALERT-BOX.
  RETURN "ADM-ERROR".
END.
*/

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
DEFINE VAR v_desde AS INTEGER.
DEFINE VAR v_hasta AS INTEGER.
DEFINE VAR i AS INTEGER.
DEFINE VAR hcon AS HANDLE.
DEFINE VAR r AS ROWID.

RUN get-container (OUTPUT hcon).
RUN dame-rowid-lote IN hcon (OUTPUT r).
FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.

v_del_emp = lotes_jugo.id_empresa.
v_del_suc = lotes_jugo.id_sucursal.
v_del_tip = lotes_jugo.id_tipotambor.
v_del_nro = lotes_jugo.nromov.
v_del_des = INTEGER(composicion_lote.numeracion_desde:SCREEN-VALUE IN FRAME F-Main).
v_del_has = INTEGER(composicion_lote.numeracion_hasta:SCREEN-VALUE IN FRAME F-Main).


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
  {src/adm/template/row-list.i "composicion_lote"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "composicion_lote"}

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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
  {src/adm/template/snd-list.i "composicion_lote"}

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

