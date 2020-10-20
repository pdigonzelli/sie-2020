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
define var del_empresa as integer.
define var del_sucursal as integer.
define var del_tipotambor as integer.
define var del_nromov as integer.
define var del_lote as integer.
define var del_fecha as date.
define var del_articulo as integer.
define var del_calidad as integer.

define var del_empresa_lote as integer.
define var del_sucursal_lote as integer.
define var del_tipotambor_lote as integer.
define var del_nromov_lote as integer.

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
&Scoped-Define ENABLED-FIELDS lotes_aceite.id_articulo lotes_aceite.fecha ~
lotes_aceite.citral lotes_aceite.id_envase lotes_aceite.Peso_neto 
&Scoped-define ENABLED-TABLES lotes_aceite
&Scoped-define FIRST-ENABLED-TABLE lotes_aceite
&Scoped-Define DISPLAYED-FIELDS lotes_aceite.id_articulo lotes_aceite.fecha ~
lotes_aceite.citral lotes_aceite.id_envase lotes_aceite.Peso_neto 
&Scoped-define DISPLAYED-TABLES lotes_aceite
&Scoped-define FIRST-DISPLAYED-TABLE lotes_aceite
&Scoped-Define DISPLAYED-OBJECTS fi-envases_prod-descripcion ~
fi-productos_terminados-descrip 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS lotes_aceite.id_articulo 

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
DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lotes_aceite.id_articulo AT ROW 1 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     lotes_aceite.fecha AT ROW 1.95 COL 13 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     lotes_aceite.citral AT ROW 2.91 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     lotes_aceite.id_envase AT ROW 3.86 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     lotes_aceite.Peso_neto AT ROW 4.81 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     fi-envases_prod-descripcion AT ROW 3.86 COL 20 COLON-ALIGNED NO-LABEL
     fi-productos_terminados-descrip AT ROW 1 COL 21 COLON-ALIGNED NO-LABEL
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
         HEIGHT             = 5
         WIDTH              = 62.8.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lotes_aceite.fecha IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lotes_aceite.id_articulo IN FRAME F-Main
   1                                                                    */
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
general.lotes_aceite.id_envase;wc_envases.w;envases_prod.descripcion;;
general.lotes_aceite.id_articulo;wc_productos_terminados.w;productos_terminados.descripcion;;
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

&Scoped-define SELF-NAME lotes_aceite.citral
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.citral V-table-Win
ON LEAVE OF lotes_aceite.citral IN FRAME F-Main /* Citral */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.fecha V-table-Win
ON LEAVE OF lotes_aceite.fecha IN FRAME F-Main /* Fecha */
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


&Scoped-define SELF-NAME fi-productos_terminados-descrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-productos_terminados-descrip V-table-Win
ON LEAVE OF fi-productos_terminados-descrip IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_articulo V-table-Win
ON GO OF lotes_aceite.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_articulo V-table-Win
ON LEAVE OF lotes_aceite.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_aceite.id_articulo IN FRAME F-Main /* Art�culo */
do: 
define var r as rowid no-undo.
run wc_productos_terminados.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.lotes_aceite.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_articulo V-table-Win
ON U1 OF lotes_aceite.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_envase V-table-Win
ON GO OF lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_envase V-table-Win
ON LEAVE OF lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_envase V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_aceite.id_envase IN FRAME F-Main /* Envase */
do: 
define var r as rowid no-undo.
run wc_envases.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
general.lotes_aceite.id_envase:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_envase V-table-Win
ON U1 OF lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.Peso_neto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.Peso_neto V-table-Win
ON LEAVE OF lotes_aceite.Peso_neto IN FRAME F-Main /* Peso tambor */
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
define var r as rowid.
define var hcon as handle.
define var v_orden as integer.
define var v_nromov as integer.
define buffer b_lote for lotes_aceite.

run get-container (output hcon).
run get-rowid-lotes_ind in hcon (output r).
find lotes_ind where rowid(lotes_ind) = r no-lock no-error.
if available lotes_ind then do:
    
    find last b_lote of lotes_ind no-error.
    if available b_lote then do:
        v_orden = b_lote.id_orden + 1.
        assign b_lote.activo = false.
    end.
    else
        v_orden = 1.
    
    v_nromov = next-value(nromov).    
    assign lotes_aceite.id_empresa      = lotes_ind.id_empresa
           lotes_aceite.id_sucursal     = lotes_ind.id_sucursal
           lotes_aceite.id_tipotambor   = 7
           lotes_aceite.id_lote         = lotes_ind.id_lote
           lotes_aceite.nromov          = v_nromov
           lotes_aceite.anio            = lotes_ind.anio
           lotes_aceite.estado_lote     = 1
           lotes_aceite.nro_partida     = lotes_ind.nro_partida
           lotes_aceite.id_orden        = v_orden
           lotes_aceite.activo          = true
           lotes_aceite.c_usuario       = userid("userdb")
           lotes_aceite.c_fecha         = today
           lotes_aceite.c_hora          = string(time,"HH:MM:SS").
    /*
    /* ACA SE LE ASIGNABAN TODOS LOS TAMBORES DEL LOTE ANTERIOR AL LOTE NUEVO */
    if available b_lote then do:
        
        for each tambores_industria of b_lote 
                                    WHERE tambores_industria.id_sucursal_ubicacion = lotes_ind.id_sucursal
                                      AND tambores_industria.id_locacion_ubicacion = 4.
            assign tambores_industria.id_empresa_destino    = lotes_ind.id_empresa
                   tambores_industria.id_sucursal_destino   = lotes_ind.id_sucursal
                   tambores_industria.id_tipotambor_destino = 6
                   tambores_industria.nromov_destino        = v_nromov
                   tambores_industria.id_sucursal_ubicacion = lotes_ind.id_sucursal
                   tambores_industria.id_locacion_ubicacion = 10.
        end.
    end.
    */
end.
else
    message "Algo anda mal" view-as alert-box.

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
define buffer b_lote for lotes_aceite.
define var hcon as handle.
define var r as rowid.
DEFINE BUFFER b_tam FOR tambores_industria.
DEFINE VAR desde AS INTEGER.
DEFINE VAR hasta AS INTEGER.

run get-container (output hcon).
run get-rowid-lotes_ind in hcon (output r).
/*
find lotes_ind where rowid(lotes_ind) = r no-lock no-error.
if available lotes_ind then do:
    find last lotes_aceite of lotes_ind no-error.
    if available lotes_aceite then do:
        lotes_aceite.activo = true.
    end.
end.
*/
/*******************************************************************************/
/********************BORRO TODAS LAS TABLAS ASOCIADAS***************************/

for each composicion_lote_aceite where composicion_lote_aceite.id_empresa = del_empresa_lote and
                                       composicion_lote_aceite.id_sucursal = del_sucursal_lote and
                                       composicion_lote_aceite.id_tipotambor = del_tipotambor_lote and
                                       composicion_lote_aceite.nromov = del_nromov_lote.
    delete composicion_lote_aceite.
end.


FIND FIRST b_tam WHERE b_tam.id_empresa     = del_empresa_lote and
                       b_tam.id_sucursal    = del_sucursal_lote and
                       b_tam.id_tipotambor  = del_tipotambor_lote and
                       b_tam.nromov         = del_nromov_lote NO-LOCK NO-ERROR.

IF AVAILABLE b_tam THEN DO:
    desde = b_tam.id_tambor.
END.
FIND LAST b_tam WHERE b_tam.id_empresa = del_empresa_lote and
                      b_tam.id_sucursal = del_sucursal_lote and
                      b_tam.id_tipotambor = del_tipotambor_lote and
                      b_tam.nromov = del_nromov_lote NO-LOCK NO-ERROR.

IF AVAILABLE b_tam THEN DO:
    hasta = b_tam.id_tambor.
END.

RUN y_gstkcre.p (input del_empresa_lote,
                 input del_sucursal_lote,
                 input del_tipotambor_lote,
                 input del_nromov_lote,
                 INPUT desde,
                 INPUT hasta,
                 input 2) "lotes_jugo".

IF return-value <> "" then do:
    message "Error en el procesamiento de movimientos de stock" view-as alert-box.
    RETURN "ADM-ERROR".
END.


for each tambores_industria where tambores_industria.id_empresa = del_empresa_lote and
                                  tambores_industria.id_sucursal = del_sucursal_lote and
                                  tambores_industria.id_tipotambor = del_tipotambor_lote and
                                  tambores_industria.nromov = del_nromov_lote.

    delete tambores_industria.
end.

FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino     = del_empresa_lote 
                              AND tambores_industria.id_sucursal_destino    = del_sucursal_lote 
                              AND tambores_industria.id_tipotambor_destino  = del_tipotambor_lote 
                              AND tambores_industria.nromov_destino         = del_nromov_lote.

    tambores_industria.id_empresa_destino       = 0.
    tambores_industria.id_sucursal_destino      = 0.
    tambores_industria.id_tipotambor_destino    = 0.
    tambores_industria.nromov_destino           = 0.
    
    tambores_industria.id_locacion_ubicacion    = 4.
    tambores_industria.id_posicion_ubicacion    = 1.

    RUN y_gstkcre.p (INPUT tambores_industria.id_empresa,
                     INPUT tambores_industria.id_sucursal,
                     INPUT tambores_industria.id_tipotambor,
                     INPUT tambores_industria.nromov,
                     INPUT tambores_industria.id_tambor,
                     INPUT tambores_industria.id_tambor,
                     INPUT 2) "lotes_aceite".
    
    IF RETURN-VALUE <> "" THEN DO:
        MESSAGE "Error en el procesamiento de movimientos de stock" VIEW-AS ALERT-BOX.
        RETURN "ADM-ERROR".
    END.
END.

CREATE auditoria_lotes.
ASSIGN auditoria_lotes.id_empresa        = del_empresa
       auditoria_lotes.id_sucursal       = del_sucursal
       auditoria_lotes.id_tipotambor     = del_tipotambor
       auditoria_lotes.nromov            = del_nromov
       auditoria_lotes.id_lote           = del_lote
       auditoria_lotes.fecha             = del_fecha
       auditoria_lotes.id_articulo       = del_articulo
       auditoria_lotes.id_calidad        = del_calidad
       auditoria_lotes.anio              = year(del_fecha)
       auditoria_lotes.c_usuario         = userid("userdb")
       auditoria_lotes.c_fecha           = today
       auditoria_lotes.c_hora            = string(time,"HH:MM:SS").
    
/***********************************************************************************/

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
define var r as rowid.
define var hcontainer as handle.
define var existe as logical.

run get-container (output hcontainer).
RUN get-rowid-cabecera in hcontainer (output r).
find lotes_aceite where rowid(lotes_aceite) = r no-error.

if available lotes_aceite then
    do:
        assign lotes_aceite.c_usuario = userid("userdb")
               lotes_aceite.c_fecha   = today
               lotes_aceite.c_hora    = string(time,"HH:MM:SS").
                
        for each tambores_industria of lotes_aceite.
            assign tambores_industria.id_articulo   = lotes_aceite.id_articulo
                   tambores_industria.fecha         = lotes_aceite.fecha
                   tambores_industria.id_lote       = lotes_aceite.id_lote
                   tambores_industria.id_envase     = lotes_aceite.id_envase.

            /* ESTE CODIGO ME ESTABA MODIFICANDO LOS TAMBORES QUE ESTAN PARCIALMENTE ENVASADOS
                   tambores_industria.kilos_tambor  = lotes_aceite.peso_neto.*/
        end.
        RUN notify ('update-record':U).
    end.
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
define var hcontainer as handle.
define var existe as logical.

/******************************************************************************/
/*************************ALMACENO LOS ID DEL REGISTRO A BORRAR****************/
run get-container (output hcontainer).
RUN get-rowid-lotes_aceite in hcontainer (output r).

find lotes_aceite where rowid(lotes_aceite) = r no-error.
if available lotes_aceite then
do:      
    del_empresa_lote     = lotes_aceite.id_empresa.
    del_sucursal_lote    = lotes_aceite.id_sucursal.
    del_tipotambor_lote  = lotes_aceite.id_tipotambor.
    del_nromov_lote      = lotes_aceite.nromov.
    del_lote             = lotes_aceite.id_lote.
    del_fecha            = lotes_aceite.fecha.
    del_articulo         = lotes_aceite.id_articulo.
END.        


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-update V-table-Win 
PROCEDURE adm-pre-update :
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
find first envases_prod where envases_prod.id_envase = integer(lotes_aceite.id_envase:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(lotes_aceite.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-fields V-table-Win 
PROCEDURE disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
define var hcon as handle.

run get-container (output hcon).
run disable-botones in hcon.

lotes_aceite.fecha:sensitive in frame F-Main        = false.
lotes_aceite.citral:sensitive in frame F-Main       = false.
lotes_aceite.id_envase:sensitive in frame F-Main    = false.
lotes_aceite.peso_neto:sensitive in frame F-Main    = false.
lotes_aceite.tanque:sensitive in frame F-Main       = false.
*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-fields V-table-Win 
PROCEDURE enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
define var hcon as handle.

run get-container (output hcon).
run enable-botones in hcon.

lotes_aceite.fecha:sensitive in frame F-Main        = true.
lotes_aceite.citral:sensitive in frame F-Main       = true.
lotes_aceite.id_envase:sensitive in frame F-Main    = true.
lotes_aceite.peso_neto:sensitive in frame F-Main    = true.
lotes_aceite.tanque:sensitive in frame F-Main       = true.
*/
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
define var lista_relacion as character no-undo initial "id_envase,id_articulo".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-pre-update V-table-Win 
PROCEDURE local-pre-update :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'pre-update':U ) .
  
      
  /* Code placed here will execute AFTER standard behavior.    */

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
  if available lotes_aceite then do:
      if lotes_aceite.activo then
        run enable-fields.
      else
        run disable-fields.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-add V-table-Win 
PROCEDURE pre-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run enable-fields.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update V-table-Win 
PROCEDURE pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
define var r as rowid.
define var hcontainer as handle.
define var existe as logical.

run get-container (output hcontainer).
RUN get-rowid-cabecera in hcontainer (output r).
find lotes_aceite where rowid(lotes_aceite) = r no-lock no-error.

if available lotes_aceite then
    do:
        if not lotes_aceite.activo then do:
            message "No se puede modificar un lote viejo" view-as alert-box.
            /* return "ADM-ERROR". */
            RUN notify ('reset-record':U).
            RUN notify ('cancel-record':U).
        end.
    end.
*/
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
         
/*    when "id_lote" then
      do: 
             
        define buffer lote for lotes_aceite.
        find lote where lote.id_lote = integer(lotes_aceite.id_lote:screen-value in frame F-Main)
                    and year(lote.fecha) = year(date(lotes_aceite.fecha:screen-value in frame F-Main)) 
                    and lote.id_articulo = lotes_aceite.peso_neto:screen-value in frame F-Main) no-lock no-error.
         if available lote then 
            do:
                mensaje = "Debe ingresar un Lote no existente!. Por favor revise si ya existe el lote ingresado.".
                return false.
            end.

        
      end. */
    when "peso_neto" then
      do: 
        if integer(lotes_aceite.peso_neto:screen-value in frame F-Main) <= 0 then 
            do:
                mensaje = "Por favor cargue los kilos de un tambor(unitario). Ej: 180 kg.".
                return false.
            end.

        
      end.
  end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
