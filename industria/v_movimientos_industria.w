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
&Scoped-define EXTERNAL-TABLES movimientos_industria
&Scoped-define FIRST-EXTERNAL-TABLE movimientos_industria


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR movimientos_industria.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS movimientos_industria.id_tipo_movimiento ~
movimientos_industria.id_etiqueta ~
movimientos_industria.id_tipo_mov_existente 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}id_tipo_movimiento ~{&FP2}id_tipo_movimiento ~{&FP3}~
 ~{&FP1}id_etiqueta ~{&FP2}id_etiqueta ~{&FP3}~
 ~{&FP1}id_tipo_mov_existente ~{&FP2}id_tipo_mov_existente ~{&FP3}
&Scoped-define ENABLED-TABLES movimientos_industria
&Scoped-define FIRST-ENABLED-TABLE movimientos_industria
&Scoped-Define DISPLAYED-FIELDS movimientos_industria.id_tipo_movimiento ~
movimientos_industria.id_etiqueta ~
movimientos_industria.id_tipo_mov_existente ~
movimientos_industria.id_empresa movimientos_industria.id_empresa_destino ~
movimientos_industria.id_sucursal movimientos_industria.id_sucursal_destino ~
movimientos_industria.id_locacion movimientos_industria.id_locacion_destino ~
movimientos_industria.id_posicion movimientos_industria.id_posicion_destino 

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
id_sucursal||y|general.movimientos_industria.id_sucursal
id_tipo_movimiento||y|general.movimientos_industria.id_tipo_movimiento
id_etiqueta||y|general.movimientos_industria.id_etiqueta
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_sucursal,id_tipo_movimiento,id_etiqueta"':U).
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

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     movimientos_industria.id_tipo_movimiento AT ROW 2.24 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     movimientos_industria.id_etiqueta AT ROW 3.29 COL 59 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     movimientos_industria.id_tipo_mov_existente AT ROW 3.38 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     movimientos_industria.id_empresa_origen AT ROW 6.91 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     movimientos_industria.id_empresa_destino AT ROW 6.91 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     movimientos_industria.id_sucursal_origen AT ROW 7.91 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     movimientos_industria.id_sucursal_destino AT ROW 7.91 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     movimientos_industria.id_locacion_origen AT ROW 8.91 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     movimientos_industria.id_locacion_destino AT ROW 8.91 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     movimientos_industria.id_posicion_origen AT ROW 9.91 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     movimientos_industria.id_posicion_destino AT ROW 9.91 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     "DESTINO" VIEW-AS TEXT
          SIZE 58 BY .95 AT ROW 5.57 COL 61
          BGCOLOR 1 FGCOLOR 15 
     "ORIGEN" VIEW-AS TEXT
          SIZE 58 BY .95 AT ROW 5.57 COL 2
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.movimientos_industria
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
         HEIGHT             = 10.19
         WIDTH              = 118.
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

/* SETTINGS FOR FILL-IN movimientos_industria.id_empresa_origen IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN movimientos_industria.id_empresa_destino IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN movimientos_industria.id_locacion_origen IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN movimientos_industria.id_locacion_destino IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN movimientos_industria.id_posicion_origen IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN movimientos_industria.id_posicion_destino IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN movimientos_industria.id_sucursal_origen IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN movimientos_industria.id_sucursal_destino IN FRAME F-Main
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

&Scoped-define SELF-NAME movimientos_industria.id_empresa_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_empresa_origen V-table-Win
ON LEAVE OF movimientos_industria.id_empresa_origen IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_empresa_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_empresa_destino V-table-Win
ON LEAVE OF movimientos_industria.id_empresa_destino IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_etiqueta V-table-Win
ON LEAVE OF movimientos_industria.id_etiqueta IN FRAME F-Main /* Etiqueta */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_locacion_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_locacion_origen V-table-Win
ON LEAVE OF movimientos_industria.id_locacion_origen IN FRAME F-Main /* Locacion */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_locacion_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_locacion_destino V-table-Win
ON LEAVE OF movimientos_industria.id_locacion_destino IN FRAME F-Main /* Locacion */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_posicion_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_posicion_origen V-table-Win
ON GO OF movimientos_industria.id_posicion_origen IN FRAME F-Main /* Posicion */
DO:
{custom/support/validacion.i}
     run descriptivos.
/*     run descriptivos1. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_posicion_origen V-table-Win
ON LEAVE OF movimientos_industria.id_posicion_origen IN FRAME F-Main /* Posicion */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_posicion_origen V-table-Win
ON MOUSE-SELECT-DBLCLICK OF movimientos_industria.id_posicion_origen IN FRAME F-Main /* Posicion */
do: 
define var r as rowid no-undo.
run wc_posiciones.w(output r).
find posiciones where rowid(posiciones) = r no-lock no-error.
if available posiciones then 
do:
        movimientos_industria.id_posicion_origen:screen-value = string(posiciones.id_posicion).
        movimientos_industria.id_empresa_origen:screen-value in frame F-Main = string(posiciones.id_empresa).
        movimientos_industria.id_sucursal_origen:screen-value in frame F-Main = string(posiciones.id_sucursal).
        movimientos_industria.id_locacion_origen:screen-value in frame F-Main = string(posiciones.id_locacion).
end.
else
    do:
        movimientos_industria.id_empresa_origen:screen-value in frame F-Main = string(0).
        movimientos_industria.id_sucursal_origen:screen-value in frame F-Main = string(0).
        movimientos_industria.id_locacion_origen:screen-value in frame F-Main = string(0).
    end.
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_posicion_origen V-table-Win
ON U1 OF movimientos_industria.id_posicion_origen IN FRAME F-Main /* Posicion */
DO:
{custom/support/validacion.i}
     run descriptivos.
/*     run descriptivos1. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_posicion_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_posicion_destino V-table-Win
ON GO OF movimientos_industria.id_posicion_destino IN FRAME F-Main /* Posicion */
DO:
{custom/support/validacion.i}
     run descriptivos.
/*     run descriptivos1. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_posicion_destino V-table-Win
ON LEAVE OF movimientos_industria.id_posicion_destino IN FRAME F-Main /* Posicion */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_posicion_destino V-table-Win
ON MOUSE-SELECT-DBLCLICK OF movimientos_industria.id_posicion_destino IN FRAME F-Main /* Posicion */
do: 
define var r as rowid no-undo.
run wc_posiciones.w(output r).
find posiciones where rowid(posiciones) = r no-lock no-error.
if available posiciones then 
do:
        movimientos_industria.id_posicion_destino:screen-value = string(posiciones.id_posicion).
        movimientos_industria.id_empresa_destino:screen-value in frame F-Main = string(posiciones.id_empresa).
        movimientos_industria.id_sucursal_destino:screen-value in frame F-Main = string(posiciones.id_sucursal).
        movimientos_industria.id_locacion_destino:screen-value in frame F-Main = string(posiciones.id_locacion).
end.
else
    do:
        movimientos_industria.id_empresa_destino:screen-value in frame F-Main = string(0).
        movimientos_industria.id_sucursal_destino:screen-value in frame F-Main = string(0).
        movimientos_industria.id_locacion_destino:screen-value in frame F-Main = string(0).
    end.
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_posicion_destino V-table-Win
ON U1 OF movimientos_industria.id_posicion_destino IN FRAME F-Main /* Posicion */
DO:
{custom/support/validacion.i}
     run descriptivos.
/*     run descriptivos1. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_sucursal V-table-Win
ON LEAVE OF movimientos_industria.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_sucursal_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_sucursal_destino V-table-Win
ON LEAVE OF movimientos_industria.id_sucursal_destino IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_tipo_movimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_tipo_movimiento V-table-Win
ON LEAVE OF movimientos_industria.id_tipo_movimiento IN FRAME F-Main /* Tipo Movimiento */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME movimientos_industria.id_tipo_mov_existente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL movimientos_industria.id_tipo_mov_existente V-table-Win
ON LEAVE OF movimientos_industria.id_tipo_mov_existente IN FRAME F-Main /* Tipos movimientos Existentes */
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
  {src/adm/template/row-list.i "movimientos_industria"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "movimientos_industria"}

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
find first posiciones where posiciones.id_posicion = integer(movimientos_industria.id_posicion:screen-value in frame F-Main)  no-lock no-error .
if available posiciones then 
    do:
        movimientos_industria.id_empresa:screen-value in frame F-Main = string(posiciones.id_empresa).
        movimientos_industria.id_sucursal:screen-value in frame F-Main = string(posiciones.id_sucursal).
        movimientos_industria.id_locacion:screen-value in frame F-Main = string(posiciones.id_locacion).
    end.
else
    do:
        movimientos_industria.id_empresa:screen-value in frame F-Main = string(0).
        movimientos_industria.id_sucursal:screen-value in frame F-Main = string(0).
        movimientos_industria.id_locacion:screen-value in frame F-Main = string(0).
    end.

/***********CODIGO DEL DOBLE CLICK DEL CAMPO

define var r as rowid no-undo.
run wc_posiciones.w(output r).
find posiciones where rowid(posiciones) = r no-lock no-error.
if available posiciones then 
do:
        movimientos_industria.id_posicion:screen-value = string(posiciones.id_posicion).
        movimientos_industria.id_empresa:screen-value in frame F-Main = string(posiciones.id_empresa).
        movimientos_industria.id_sucursal:screen-value in frame F-Main = string(posiciones.id_sucursal).
        movimientos_industria.id_locacion:screen-value in frame F-Main = string(posiciones.id_locacion).
end.
else
    do:
        movimientos_industria.id_empresa:screen-value in frame F-Main = string(0).
        movimientos_industria.id_sucursal:screen-value in frame F-Main = string(0).
        movimientos_industria.id_locacion:screen-value in frame F-Main = string(0).
    end.

*/

find first posiciones where posiciones.id_posicion = integer(movimientos_industria.id_posicion_destino:screen-value in frame F-Main)  no-lock no-error .
if available posiciones then 
    do:
        movimientos_industria.id_empresa_destino:screen-value in frame F-Main = string(posiciones.id_empresa).
        movimientos_industria.id_sucursal_destino:screen-value in frame F-Main = string(posiciones.id_sucursal).
        movimientos_industria.id_locacion_destino:screen-value in frame F-Main = string(posiciones.id_locacion).
    end.
else
    do:
        movimientos_industria.id_empresa_destino:screen-value in frame F-Main = string(0).
        movimientos_industria.id_sucursal_destino:screen-value in frame F-Main = string(0).
        movimientos_industria.id_locacion_destino:screen-value in frame F-Main = string(0).
    end.

/***********CODIGO DEL DOBLE-CLICK DEL CAMPO
define var r as rowid no-undo.
run wc_posiciones.w(output r).
find posiciones where rowid(posiciones) = r no-lock no-error.
if available posiciones then 
do:
        movimientos_industria.id_posicion_destino:screen-value = string(posiciones.id_posicion).
        movimientos_industria.id_empresa_destino:screen-value in frame F-Main = string(posiciones.id_empresa).
        movimientos_industria.id_sucursal_destino:screen-value in frame F-Main = string(posiciones.id_sucursal).
        movimientos_industria.id_locacion_destino:screen-value in frame F-Main = string(posiciones.id_locacion).
end.
else
    do:
        movimientos_industria.id_empresa_destino:screen-value in frame F-Main = string(0).
        movimientos_industria.id_sucursal_destino:screen-value in frame F-Main = string(0).
        movimientos_industria.id_locacion_destino:screen-value in frame F-Main = string(0).
    end.
apply 'U1' to self.
*/

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
  {src/adm/template/sndkycas.i "id_sucursal" "movimientos_industria" "id_sucursal"}
  {src/adm/template/sndkycas.i "id_tipo_movimiento" "movimientos_industria" "id_tipo_movimiento"}
  {src/adm/template/sndkycas.i "id_etiqueta" "movimientos_industria" "id_etiqueta"}

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
  {src/adm/template/snd-list.i "movimientos_industria"}

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


