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

define var del-emp as integer.
define var del-suc as integer.
define var del-tip as integer.
define var del-nro as integer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES sobrante_lotes_aceite
&Scoped-define FIRST-EXTERNAL-TABLE sobrante_lotes_aceite


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR sobrante_lotes_aceite.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS sobrante_lotes_aceite.fecha_elaboracion ~
sobrante_lotes_aceite.volumen sobrante_lotes_aceite.tipo_almacenamiento ~
sobrante_lotes_aceite.id_envase sobrante_lotes_aceite.cantidad ~
sobrante_lotes_aceite.identificacion 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}fecha_elaboracion ~{&FP2}fecha_elaboracion ~{&FP3}~
 ~{&FP1}volumen ~{&FP2}volumen ~{&FP3}~
 ~{&FP1}tipo_almacenamiento ~{&FP2}tipo_almacenamiento ~{&FP3}~
 ~{&FP1}id_envase ~{&FP2}id_envase ~{&FP3}~
 ~{&FP1}cantidad ~{&FP2}cantidad ~{&FP3}~
 ~{&FP1}identificacion ~{&FP2}identificacion ~{&FP3}
&Scoped-define ENABLED-TABLES sobrante_lotes_aceite
&Scoped-define FIRST-ENABLED-TABLE sobrante_lotes_aceite
&Scoped-Define DISPLAYED-FIELDS sobrante_lotes_aceite.fecha_elaboracion ~
sobrante_lotes_aceite.volumen sobrante_lotes_aceite.tipo_almacenamiento ~
sobrante_lotes_aceite.id_envase sobrante_lotes_aceite.cantidad ~
sobrante_lotes_aceite.identificacion 
&Scoped-Define DISPLAYED-OBJECTS fi-envases_prod-descripcion 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS sobrante_lotes_aceite.fecha_elaboracion ~
sobrante_lotes_aceite.volumen sobrante_lotes_aceite.tipo_almacenamiento ~
sobrante_lotes_aceite.id_envase sobrante_lotes_aceite.cantidad ~
sobrante_lotes_aceite.identificacion 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_tipotambor||y|general.sobrante_lotes_aceite.id_tipotambor
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
DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     sobrante_lotes_aceite.fecha_elaboracion AT ROW 1 COL 32 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     sobrante_lotes_aceite.volumen AT ROW 2 COL 32 COLON-ALIGNED
          LABEL "Volumen"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     sobrante_lotes_aceite.tipo_almacenamiento AT ROW 3 COL 32 COLON-ALIGNED
          LABEL "Tipo almacenamiento"
          VIEW-AS FILL-IN 
          SIZE 12.6 BY 1
     fi-envases_prod-descripcion AT ROW 3.95 COL 39 COLON-ALIGNED NO-LABEL
     sobrante_lotes_aceite.id_envase AT ROW 4 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     sobrante_lotes_aceite.cantidad AT ROW 5 COL 32 COLON-ALIGNED
          LABEL "Cantidad"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     sobrante_lotes_aceite.identificacion AT ROW 5 COL 64 COLON-ALIGNED
          LABEL "Identificación"
          VIEW-AS FILL-IN 
          SIZE 81 BY 1
     "~"e~" -Envase     ~"t~" -Tanque" VIEW-AS TEXT
          SIZE 41 BY .95 AT ROW 3 COL 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.sobrante_lotes_aceite
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
         HEIGHT             = 5.1
         WIDTH              = 146.
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

/* SETTINGS FOR FILL-IN sobrante_lotes_aceite.cantidad IN FRAME F-Main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN sobrante_lotes_aceite.fecha_elaboracion IN FRAME F-Main
   1 EXP-FORMAT                                                         */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sobrante_lotes_aceite.identificacion IN FRAME F-Main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN sobrante_lotes_aceite.id_envase IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN sobrante_lotes_aceite.tipo_almacenamiento IN FRAME F-Main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN sobrante_lotes_aceite.volumen IN FRAME F-Main
   1 EXP-LABEL                                                          */
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
general.sobrante_lotes_aceite.id_envase;wc_envases.w;envases_prod.descripcion;;
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

&Scoped-define SELF-NAME sobrante_lotes_aceite.cantidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.cantidad V-table-Win
ON LEAVE OF sobrante_lotes_aceite.cantidad IN FRAME F-Main /* Cantidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sobrante_lotes_aceite.fecha_elaboracion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.fecha_elaboracion V-table-Win
ON LEAVE OF sobrante_lotes_aceite.fecha_elaboracion IN FRAME F-Main /* Fecha Elaboración */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-envases_prod-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-envases_prod-descripcion V-table-Win
ON LEAVE OF fi-envases_prod-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sobrante_lotes_aceite.identificacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.identificacion V-table-Win
ON LEAVE OF sobrante_lotes_aceite.identificacion IN FRAME F-Main /* Identificación */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sobrante_lotes_aceite.id_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.id_envase V-table-Win
ON GO OF sobrante_lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.id_envase V-table-Win
ON LEAVE OF sobrante_lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.id_envase V-table-Win
ON MOUSE-SELECT-DBLCLICK OF sobrante_lotes_aceite.id_envase IN FRAME F-Main /* Envase */
do: 
define var r as rowid no-undo.
run wc_envases.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
general.sobrante_lotes_aceite.id_envase:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.id_envase V-table-Win
ON U1 OF sobrante_lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sobrante_lotes_aceite.tipo_almacenamiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.tipo_almacenamiento V-table-Win
ON LEAVE OF sobrante_lotes_aceite.tipo_almacenamiento IN FRAME F-Main /* Tipo almacenamiento */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sobrante_lotes_aceite.volumen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sobrante_lotes_aceite.volumen V-table-Win
ON LEAVE OF sobrante_lotes_aceite.volumen IN FRAME F-Main /* Volumen */
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
define var hcontainer as handle.
define var r as rowid.
define var i as integer initial 1.
define var ultimo as integer.
define var v_nromov as integer.
RUN get-container (output hcontainer).
RUN get-rowid-cabecera in hcontainer (output r).
for each lotes_aceite where rowid(lotes_aceite) = r.
    
    v_nromov = next-value(nromov).
    
    assign sobrante_lotes_aceite.id_empresa = lotes_aceite.id_empresa
           sobrante_lotes_aceite.id_sucursal = lotes_aceite.id_sucursal
           sobrante_lotes_aceite.id_lote = lotes_aceite.id_lote
           sobrante_lotes_aceite.id_tipotambor = lotes_aceite.id_tipotambor
           sobrante_lotes_aceite.nromov = lotes_aceite.nromov
           sobrante_lotes_aceite.id_tipotambor_sobrante = 8
           sobrante_lotes_aceite.nromov_sobrante = v_nromov
           sobrante_lotes_aceite.c_usuario = userid("userdb")
           sobrante_lotes_aceite.c_fecha   = today
           sobrante_lotes_aceite.c_hora    = string(time,"HH:MM:SS").
   
   if tipo_almacenamiento:screen-value in frame {&FRAME-NAME} <> "Tanque" then
    do:
        
        do i = 1 to integer(sobrante_lotes_aceite.cantidad:screen-value in frame {&FRAME-NAME}):
            create tambores_industria.
            assign tambores_industria.id_empresa = lotes_aceite.id_empresa
                   tambores_industria.id_sucursal = lotes_aceite.id_sucursal
                   tambores_industria.id_lote = lotes_aceite.id_lote
                   tambores_industria.id_tipotambor = 8
                   tambores_industria.nromov = v_nromov
                   tambores_industria.id_tambor = i
                   tambores_industria.id_envase = integer(sobrante_lotes_aceite.id_envase:screen-value in frame {&FRAME-NAME}).
                   
                   if lotes_aceite.id_sucursal = 96 then
                    assign tambores_industria.id_etiqueta = next-value(tambores).
               else
                    assign tambores_industria.id_etiqueta = next-value(tambores_famailla).
                   
            assign tambores_industria.fecha = date(sobrante_lotes_aceite.fecha_elaboracion:screen-value in frame {&FRAME-NAME})
                   tambores_industria.id_articulo = integer(string(lotes_aceite.id_articulo) + "8")
                   tambores_industria.c_usuario = userid("userdb")
                   tambores_industria.c_fecha   = today
                   tambores_industria.c_hora    = string(time,"HH:MM:SS")
                   tambores_industria.id_empresa_ubicacion = 1
                   tambores_industria.id_sucursal_ubicacion = lotes_aceite.id_sucursal
                   tambores_industria.id_locacion_ubicacion = 4
                   tambores_industria.id_posicion_ubicacion = 1. 
        end.
               
    end.
   else 
    do:
       message "Es en un tanque...No va a salir ninguna etiqueta...Grabe como tambor y vuelva a presionar el boton de impresión".
       sobrante_lotes_aceite.id_envase:screen-value in frame {&FRAME-NAME} = "0".
       assign sobrante_lotes_aceite.id_envase = 0.
    end.
end.

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
for each tambores_industria where tambores_industria.id_empresa = del-emp
                              and tambores_industria.id_sucursal = del-suc
                              and tambores_industria.id_tipotambor = del-tip
                              and tambores_industria.nromov = del-nro.
                              
        delete tambores_industria.
end.                              
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
define var r as rowid.
define var hcon as handle.

run get-container (output hcon).

run dame-rowid-aceite in hcon (output r).
find lotes_aceite where rowid(lotes_aceite) = r no-error.

find sobrante_lotes_aceite where sobrante_lotes_aceite.id_empresa = lotes_aceite.id_empresa
                             and sobrante_lotes_aceite.id_sucursal = lotes_aceite.id_sucursal
                             and sobrante_lotes_aceite.id_tipotambor = lotes_aceite.id_tipotambor
                             and sobrante_lotes_aceite.nromov = lotes_aceite.nromov.
if available sobrante_lotes_aceite then
do:
    del-emp = sobrante_lotes_aceite.id_empresa.
    del-suc = sobrante_lotes_aceite.id_sucursal.
    del-tip = sobrante_lotes_aceite.id_tipotambor_sobrante.
    del-nro = sobrante_lotes_aceite.nromov_sobrante.    
    
end.
else message "Se va a producir un error al intentar borrar. Por favor avisar a Centro de Computos".


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
  {src/adm/template/row-list.i "sobrante_lotes_aceite"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "sobrante_lotes_aceite"}

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
find first envases_prod where envases_prod.id_envase = integer(sobrante_lotes_aceite.id_envase:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion:screen-value in frame F-Main = ''.

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
  {src/adm/template/sndkycas.i "id_tipotambor" "sobrante_lotes_aceite" "id_tipotambor"}

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
  {src/adm/template/snd-list.i "sobrante_lotes_aceite"}

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


