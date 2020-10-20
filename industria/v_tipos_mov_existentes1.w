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
&Scoped-define EXTERNAL-TABLES tipos_mov_existentes
&Scoped-define FIRST-EXTERNAL-TABLE tipos_mov_existentes


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tipos_mov_existentes.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tipos_mov_existentes.id_tipo_movimiento ~
tipos_mov_existentes.id_tipo_mov_existente ~
tipos_mov_existentes.id_empresa_origen ~
tipos_mov_existentes.id_empresa_destino ~
tipos_mov_existentes.id_sucursal_origen ~
tipos_mov_existentes.id_sucursal_destino ~
tipos_mov_existentes.id_locacion_origen ~
tipos_mov_existentes.id_locacion_destino ~
tipos_mov_existentes.id_posicion_origen ~
tipos_mov_existentes.id_posicion_destino 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}id_tipo_movimiento ~{&FP2}id_tipo_movimiento ~{&FP3}~
 ~{&FP1}id_tipo_mov_existente ~{&FP2}id_tipo_mov_existente ~{&FP3}~
 ~{&FP1}id_empresa_origen ~{&FP2}id_empresa_origen ~{&FP3}~
 ~{&FP1}id_empresa_destino ~{&FP2}id_empresa_destino ~{&FP3}~
 ~{&FP1}id_sucursal_origen ~{&FP2}id_sucursal_origen ~{&FP3}~
 ~{&FP1}id_sucursal_destino ~{&FP2}id_sucursal_destino ~{&FP3}~
 ~{&FP1}id_locacion_origen ~{&FP2}id_locacion_origen ~{&FP3}~
 ~{&FP1}id_locacion_destino ~{&FP2}id_locacion_destino ~{&FP3}~
 ~{&FP1}id_posicion_origen ~{&FP2}id_posicion_origen ~{&FP3}~
 ~{&FP1}id_posicion_destino ~{&FP2}id_posicion_destino ~{&FP3}
&Scoped-define ENABLED-TABLES tipos_mov_existentes
&Scoped-define FIRST-ENABLED-TABLE tipos_mov_existentes
&Scoped-Define DISPLAYED-FIELDS tipos_mov_existentes.id_tipo_movimiento ~
tipos_mov_existentes.id_tipo_mov_existente ~
tipos_mov_existentes.id_empresa_origen ~
tipos_mov_existentes.id_empresa_destino ~
tipos_mov_existentes.id_sucursal_origen ~
tipos_mov_existentes.id_sucursal_destino ~
tipos_mov_existentes.id_locacion_origen ~
tipos_mov_existentes.id_locacion_destino ~
tipos_mov_existentes.id_posicion_origen ~
tipos_mov_existentes.id_posicion_destino 
&Scoped-Define DISPLAYED-OBJECTS fi-tipos_movimientos-descripcio ~
fi-posiciones-descripcion fi-posiciones-descripcion-1 

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
DEFINE VARIABLE fi-posiciones-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-posiciones-descripcion-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipos_movimientos-descripcio AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tipos_mov_existentes.id_tipo_movimiento AT ROW 1 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     fi-tipos_movimientos-descripcio AT ROW 1 COL 35 COLON-ALIGNED NO-LABEL
     tipos_mov_existentes.id_tipo_mov_existente AT ROW 2 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     tipos_mov_existentes.id_empresa_origen AT ROW 3 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     tipos_mov_existentes.id_empresa_destino AT ROW 3 COL 97 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     tipos_mov_existentes.id_sucursal_origen AT ROW 4 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     tipos_mov_existentes.id_sucursal_destino AT ROW 4 COL 97 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     tipos_mov_existentes.id_locacion_origen AT ROW 5 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     tipos_mov_existentes.id_locacion_destino AT ROW 5 COL 97 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     fi-posiciones-descripcion AT ROW 5.95 COL 35 COLON-ALIGNED NO-LABEL
     fi-posiciones-descripcion-1 AT ROW 5.95 COL 107 COLON-ALIGNED NO-LABEL
     tipos_mov_existentes.id_posicion_origen AT ROW 6 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     tipos_mov_existentes.id_posicion_destino AT ROW 6 COL 97 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.tipos_mov_existentes
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
         HEIGHT             = 6.48
         WIDTH              = 145.6.
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

/* SETTINGS FOR FILL-IN fi-posiciones-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-posiciones-descripcion-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipos_movimientos-descripcio IN FRAME F-Main
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
general.tipos_mov_existentes.id_tipo_movimiento;wc_tipos_movimientos.w;tipos_movimientos.descripcion;;
general.tipos_mov_existentes.id_posicion_origen;wc_posiciones.w;posiciones.descripcion;id_posicion;
general.tipos_mov_existentes.id_posicion_destino;wc_posiciones.w;posiciones.descripcion;id_posicion;
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

&Scoped-define SELF-NAME tipos_mov_existentes.id_empresa_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_empresa_destino V-table-Win
ON LEAVE OF tipos_mov_existentes.id_empresa_destino IN FRAME F-Main /* Empresa Destino */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_empresa_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_empresa_origen V-table-Win
ON LEAVE OF tipos_mov_existentes.id_empresa_origen IN FRAME F-Main /* Empresa Origen */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_locacion_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_locacion_destino V-table-Win
ON LEAVE OF tipos_mov_existentes.id_locacion_destino IN FRAME F-Main /* Locacion Destino */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_locacion_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_locacion_origen V-table-Win
ON LEAVE OF tipos_mov_existentes.id_locacion_origen IN FRAME F-Main /* Locación Origen */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_posicion_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_posicion_destino V-table-Win
ON GO OF tipos_mov_existentes.id_posicion_destino IN FRAME F-Main /* Posicion Destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_posicion_destino V-table-Win
ON LEAVE OF tipos_mov_existentes.id_posicion_destino IN FRAME F-Main /* Posicion Destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_posicion_destino V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tipos_mov_existentes.id_posicion_destino IN FRAME F-Main /* Posicion Destino */
do: 
define var r as rowid no-undo.
run wc_posiciones.w(output r).
find posiciones where rowid(posiciones) = r no-lock no-error.
if available posiciones then 
general.tipos_mov_existentes.id_posicion_destino:screen-value = string(posiciones.id_posicion).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_posicion_destino V-table-Win
ON U1 OF tipos_mov_existentes.id_posicion_destino IN FRAME F-Main /* Posicion Destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_posicion_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_posicion_origen V-table-Win
ON GO OF tipos_mov_existentes.id_posicion_origen IN FRAME F-Main /* Posicion Origen */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_posicion_origen V-table-Win
ON LEAVE OF tipos_mov_existentes.id_posicion_origen IN FRAME F-Main /* Posicion Origen */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_posicion_origen V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tipos_mov_existentes.id_posicion_origen IN FRAME F-Main /* Posicion Origen */
do: 
define var r as rowid no-undo.
run wc_posiciones.w(output r).
find posiciones where rowid(posiciones) = r no-lock no-error.
if available posiciones then 
general.tipos_mov_existentes.id_posicion_origen:screen-value = string(posiciones.id_posicion).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_posicion_origen V-table-Win
ON U1 OF tipos_mov_existentes.id_posicion_origen IN FRAME F-Main /* Posicion Origen */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_sucursal_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_sucursal_destino V-table-Win
ON LEAVE OF tipos_mov_existentes.id_sucursal_destino IN FRAME F-Main /* Sucursal Destino */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_sucursal_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_sucursal_origen V-table-Win
ON LEAVE OF tipos_mov_existentes.id_sucursal_origen IN FRAME F-Main /* Sucursal Origen */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_tipo_movimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_tipo_movimiento V-table-Win
ON GO OF tipos_mov_existentes.id_tipo_movimiento IN FRAME F-Main /* Id Tipo Mov */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_tipo_movimiento V-table-Win
ON LEAVE OF tipos_mov_existentes.id_tipo_movimiento IN FRAME F-Main /* Id Tipo Mov */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_tipo_movimiento V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tipos_mov_existentes.id_tipo_movimiento IN FRAME F-Main /* Id Tipo Mov */
do: 
define var r as rowid no-undo.
run wc_tipos_movimientos.w(output r).
find tipos_movimientos where rowid(tipos_movimientos) = r no-lock no-error.
if available tipos_movimientos then 
general.tipos_mov_existentes.id_tipo_movimiento:screen-value = string(tipos_movimientos.id_tipo_movimiento).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_tipo_movimiento V-table-Win
ON U1 OF tipos_mov_existentes.id_tipo_movimiento IN FRAME F-Main /* Id Tipo Mov */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tipos_mov_existentes.id_tipo_mov_existente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tipos_mov_existentes.id_tipo_mov_existente V-table-Win
ON LEAVE OF tipos_mov_existentes.id_tipo_mov_existente IN FRAME F-Main /* Id Tipos Mov Existentes */
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
  {src/adm/template/row-list.i "tipos_mov_existentes"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tipos_mov_existentes"}

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
find first tipos_movimientos where tipos_movimientos.id_tipo_movimiento = integer(tipos_mov_existentes.id_tipo_movimiento:screen-value in frame F-Main)  no-lock no-error .
if available tipos_movimientos then 
fi-tipos_movimientos-descripcio:screen-value in frame F-Main = string(tipos_movimientos.descripcion).
else
fi-tipos_movimientos-descripcio:screen-value in frame F-Main = ''.

find first posiciones where posiciones.id_posicion = integer(tipos_mov_existentes.id_posicion_origen:screen-value in frame F-Main)  no-lock no-error .
if available posiciones then 
fi-posiciones-descripcion:screen-value in frame F-Main = string(posiciones.descripcion).
else
fi-posiciones-descripcion:screen-value in frame F-Main = ''.

find first posiciones where posiciones.id_posicion = integer(tipos_mov_existentes.id_posicion_destino:screen-value in frame F-Main)  no-lock no-error .
if available posiciones then 
fi-posiciones-descripcion-1:screen-value in frame F-Main = string(posiciones.descripcion).
else
fi-posiciones-descripcion-1:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_tipo_movimiento,id_posicion_origen,id_posicion_destino".
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
  {src/adm/template/snd-list.i "tipos_mov_existentes"}

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


