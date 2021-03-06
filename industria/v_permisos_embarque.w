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

define var v_importe as DECIMAL.

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
&Scoped-define EXTERNAL-TABLES permisos_embarque
&Scoped-define FIRST-EXTERNAL-TABLE permisos_embarque


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR permisos_embarque.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS permisos_embarque.anio ~
permisos_embarque.tipo_pe permisos_embarque.id_aduana ~
permisos_embarque.fecha permisos_embarque.fecha_envio_tuc ~
permisos_embarque.id_permiso_embarque ~
permisos_embarque.fecha_oficializacion permisos_embarque.fecha_rec_tuc ~
permisos_embarque.fecha_venc_derecho permisos_embarque.fecha_cumplido ~
permisos_embarque.id_cliente permisos_embarque.id_orden_entrega ~
permisos_embarque.item_oe permisos_embarque.id_despachante ~
permisos_embarque.rectificado permisos_embarque.id_moneda_origen ~
permisos_embarque.observaciones permisos_embarque.importe ~
permisos_embarque.importe_reintegro permisos_embarque.importe_derecho ~
permisos_embarque.fecha_declaracion_venta ~
permisos_embarque.importe_reembolso permisos_embarque.consignacion 
&Scoped-define ENABLED-TABLES permisos_embarque
&Scoped-define FIRST-ENABLED-TABLE permisos_embarque
&Scoped-Define DISPLAYED-FIELDS permisos_embarque.anio ~
permisos_embarque.tipo_pe permisos_embarque.id_aduana ~
permisos_embarque.fecha permisos_embarque.fecha_envio_tuc ~
permisos_embarque.id_permiso_embarque ~
permisos_embarque.fecha_oficializacion permisos_embarque.fecha_rec_tuc ~
permisos_embarque.fecha_venc_derecho permisos_embarque.fecha_cumplido ~
permisos_embarque.id_cliente permisos_embarque.id_orden_entrega ~
permisos_embarque.item_oe permisos_embarque.id_despachante ~
permisos_embarque.rectificado permisos_embarque.id_moneda_origen ~
permisos_embarque.observaciones permisos_embarque.importe ~
permisos_embarque.importe_reintegro permisos_embarque.importe_derecho ~
permisos_embarque.fecha_declaracion_venta ~
permisos_embarque.importe_reembolso permisos_embarque.consignacion ~
permisos_embarque.id_posicion_arancelaria 
&Scoped-define DISPLAYED-TABLES permisos_embarque
&Scoped-define FIRST-DISPLAYED-TABLE permisos_embarque
&Scoped-Define DISPLAYED-OBJECTS fi-tipos_pe-descripcion ~
fi-aduanas-descripcion clientes-descripcion fi-despachantes-descripcion ~
fi-tipo_moneda-descripcion fi-posicion_arancelaria-descrip 

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
DEFINE BUTTON BUTTON-19 
     LABEL "Actualizar" 
     SIZE 12 BY .95.

DEFINE BUTTON BUTTON-20 
     LABEL "..." 
     SIZE 4 BY .95.

DEFINE VARIABLE clientes-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-aduanas-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-despachantes-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-posicion_arancelaria-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipos_pe-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipo_moneda-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     permisos_embarque.anio AT ROW 1 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     permisos_embarque.tipo_pe AT ROW 1 COL 94 COLON-ALIGNED
          LABEL "TipoPE"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-tipos_pe-descripcion AT ROW 1 COL 101 COLON-ALIGNED NO-LABEL
     permisos_embarque.id_aduana AT ROW 1.95 COL 19 COLON-ALIGNED
          LABEL "Aduana"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-aduanas-descripcion AT ROW 1.95 COL 26 COLON-ALIGNED NO-LABEL
     permisos_embarque.fecha AT ROW 1.95 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     permisos_embarque.fecha_envio_tuc AT ROW 1.95 COL 129 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     permisos_embarque.id_permiso_embarque AT ROW 2.91 COL 19 COLON-ALIGNED
          LABEL "Permiso Embarque" FORMAT "XX99999999X"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     permisos_embarque.fecha_oficializacion AT ROW 2.91 COL 94 COLON-ALIGNED
          LABEL "Fecha Oficializa."
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     permisos_embarque.fecha_rec_tuc AT ROW 2.91 COL 129 COLON-ALIGNED
          LABEL "Fecha Recep. Tuc"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     permisos_embarque.fecha_venc_derecho AT ROW 3.86 COL 94 COLON-ALIGNED
          LABEL "Fecha Venc Derecho"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     permisos_embarque.fecha_cumplido AT ROW 3.86 COL 129 COLON-ALIGNED
          LABEL "Fecha Embarque"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     permisos_embarque.id_cliente AT ROW 4.33 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     clientes-descripcion AT ROW 4.33 COL 30 COLON-ALIGNED NO-LABEL
     permisos_embarque.id_orden_entrega AT ROW 5.05 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     permisos_embarque.item_oe AT ROW 5.05 COL 115 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     permisos_embarque.id_despachante AT ROW 5.29 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-despachantes-descripcion AT ROW 5.29 COL 31 COLON-ALIGNED NO-LABEL
     permisos_embarque.rectificado AT ROW 6 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     permisos_embarque.id_moneda_origen AT ROW 6.24 COL 19 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     fi-tipo_moneda-descripcion AT ROW 6.24 COL 25 COLON-ALIGNED NO-LABEL
     permisos_embarque.observaciones AT ROW 6.95 COL 96 NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 100
          SIZE 52 BY 3.57
     permisos_embarque.importe AT ROW 7.43 COL 19 COLON-ALIGNED FORMAT "->>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     permisos_embarque.importe_reintegro AT ROW 8.38 COL 19 COLON-ALIGNED
          LABEL "Importe Reintegro" FORMAT "->>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     permisos_embarque.importe_derecho AT ROW 8.38 COL 57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     BUTTON-19 AT ROW 8.38 COL 93
     permisos_embarque.fecha_declaracion_venta AT ROW 8.38 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     permisos_embarque.importe_reembolso AT ROW 9.33 COL 19 COLON-ALIGNED
          LABEL "Importe Reemb."
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     permisos_embarque.consignacion AT ROW 9.33 COL 132 COLON-ALIGNED
          LABEL "Cons."
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     permisos_embarque.id_posicion_arancelaria AT ROW 9.57 COL 64 COLON-ALIGNED
          LABEL "Posicion Arancela."
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     fi-posicion_arancelaria-descrip AT ROW 9.57 COL 79 COLON-ALIGNED NO-LABEL
     BUTTON-20 AT ROW 9.57 COL 120
     "Observaciones:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 6.95 COL 81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.permisos_embarque
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
         HEIGHT             = 9.62
         WIDTH              = 147.
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

/* SETTINGS FOR BUTTON BUTTON-19 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-19:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-20 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-20:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN clientes-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN permisos_embarque.consignacion IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       permisos_embarque.consignacion:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN permisos_embarque.fecha_cumplido IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       permisos_embarque.fecha_declaracion_venta:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN permisos_embarque.fecha_oficializacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN permisos_embarque.fecha_rec_tuc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN permisos_embarque.fecha_venc_derecho IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi-aduanas-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-despachantes-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-posicion_arancelaria-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi-posicion_arancelaria-descrip:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi-tipos_pe-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo_moneda-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN permisos_embarque.id_aduana IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN permisos_embarque.id_moneda_origen IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN permisos_embarque.id_permiso_embarque IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN permisos_embarque.id_posicion_arancelaria IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       permisos_embarque.id_posicion_arancelaria:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN permisos_embarque.importe IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN permisos_embarque.importe_reembolso IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN permisos_embarque.importe_reintegro IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN permisos_embarque.tipo_pe IN FRAME F-Main
   EXP-LABEL                                                            */
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
general.permisos_embarque.id_orden_entrega;wc_orden_entrega_todos.w;orden_entrega.;;
general.permisos_embarque.id_aduana;wc_aduanas.w;aduanas.descripcion;;
general.permisos_embarque.id_moneda_origen;wc_tipo_moneda.w;tipo_moneda.descripcion;id_moneda;
general.permisos_embarque.tipo_pe;wc_tipos_pe.w;tipos_pe.descripcion;id_tipo_pe;
general.permisos_embarque.id_posicion_arancelaria;wc_posicion_arancelaria.w;posicion_arancelaria.descripcion;;
general.permisos_embarque.id_despachante;wc_despachantes.w;despachantes.descripcion;;
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

&Scoped-define SELF-NAME permisos_embarque.anio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.anio V-table-Win
ON LEAVE OF permisos_embarque.anio IN FRAME F-Main /* A�o */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 V-table-Win
ON CHOOSE OF BUTTON-19 IN FRAME F-Main /* Actualizar */
DO:
    /*
  DEFINE VAR r AS ROWID.
  DEFINE VAR hcon AS HANDLE.
  DEFINE VAR v_fob AS DECIMAL.
  RUN GET-container (OUTPUT hcon).

  RUN dame-rowid IN hcon (OUTPUT r).

  FIND FIRST permisos_embarque WHERE rowid(permisos_embarque) = r NO-LOCK NO-ERROR.
  IF AVAILABLE permisos_embarque THEN DO:
      
      IF general.permisos_embarque.tipo_pe = 1 THEN DO:
          FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = permisos_embarque.id_orden_entrega
                                   NO-LOCK NO-ERROR.
          IF AVAILABLE orden_entrega THEN DO:
              importe:SCREEN-VALUE IN FRAME F-Main = STRING(orden_entrega.fob_ton).
              importe_reintegro:SCREEN-VALUE IN FRAME F-Main = STRING(orden_entrega.fob_ton).
          END.
          ELSE MESSAGE "No se encontro la OE " permisos_embarque.id_orden_entrega:SCREEN-VALUE IN FRAME F-Main
                    VIEW-AS ALERT-BOX.
      END.
      IF general.permisos_embarque.tipo_pe = 2 THEN DO:
         FOR EACH r_subd_ventas_embarque WHERE r_subd_ventas_embarque.anio_permiso =
                                                 permisos_embarque.anio
                                             AND r_subd_ventas_embarque.id_aduana =
                                                 permisos_embarque.id_aduana
                                             AND r_subd_ventas_embarque.nro_embarque =
                                                 permisos_embarque.id_permiso_embarque
                                             NO-LOCK.
         
             FIND FIRST subd_vtas OF r_subd_ventas_embarque WHERE subd_vtas.estado
                                                            NO-LOCK NO-ERROR.
             IF AVAILABLE subd_vtas THEN DO:
                FIND FIRST r_fact_ventas WHERE r_fact_ventas.id_punto_venta_ventas = subd_vtas.id_punto_venta
                                           AND r_fact_ventas.nromov_ventas         = subd_vtas.nromov
                                          NO-LOCK NO-ERROR.
                IF NOT AVAILABLE r_fact_ventas THEN DO:
                    FOR EACH items_venta OF subd_vtas NO-LOCK.
                        v_fob = v_fob + (items_venta.cantidad * items_venta.precio_origen).
                    END.
                    permisos_embarque.importe:SCREEN-VALUE IN FRAME F-Main = STRING(v_fob).
                    permisos_embarque.importe_reintegro:SCREEN-VALUE IN FRAME F-Main = STRING(v_fob).
                END.
             END.
         
         END.
      END.
      
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-20 V-table-Win
ON CHOOSE OF BUTTON-20 IN FRAME F-Main /* ... */
DO:
  RUN w_posicion_arancelaria.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME clientes-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL clientes-descripcion V-table-Win
ON LEAVE OF clientes-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.consignacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.consignacion V-table-Win
ON LEAVE OF permisos_embarque.consignacion IN FRAME F-Main /* Cons. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.fecha V-table-Win
ON LEAVE OF permisos_embarque.fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.fecha_cumplido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.fecha_cumplido V-table-Win
ON LEAVE OF permisos_embarque.fecha_cumplido IN FRAME F-Main /* Fecha Embarque */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.fecha_declaracion_venta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.fecha_declaracion_venta V-table-Win
ON LEAVE OF permisos_embarque.fecha_declaracion_venta IN FRAME F-Main /* Fecha Declaracion Vta */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.fecha_envio_tuc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.fecha_envio_tuc V-table-Win
ON LEAVE OF permisos_embarque.fecha_envio_tuc IN FRAME F-Main /* Fecha Envio a Tuc. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.fecha_oficializacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.fecha_oficializacion V-table-Win
ON LEAVE OF permisos_embarque.fecha_oficializacion IN FRAME F-Main /* Fecha Oficializa. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.fecha_rec_tuc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.fecha_rec_tuc V-table-Win
ON LEAVE OF permisos_embarque.fecha_rec_tuc IN FRAME F-Main /* Fecha Recep. Tuc */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.fecha_venc_derecho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.fecha_venc_derecho V-table-Win
ON LEAVE OF permisos_embarque.fecha_venc_derecho IN FRAME F-Main /* Fecha Venc Derecho */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-aduanas-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-aduanas-descripcion V-table-Win
ON LEAVE OF fi-aduanas-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-despachantes-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-despachantes-descripcion V-table-Win
ON LEAVE OF fi-despachantes-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-posicion_arancelaria-descrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-posicion_arancelaria-descrip V-table-Win
ON LEAVE OF fi-posicion_arancelaria-descrip IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipos_pe-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipos_pe-descripcion V-table-Win
ON LEAVE OF fi-tipos_pe-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo_moneda-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo_moneda-descripcion V-table-Win
ON LEAVE OF fi-tipo_moneda-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.id_aduana
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_aduana V-table-Win
ON GO OF permisos_embarque.id_aduana IN FRAME F-Main /* Aduana */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_aduana V-table-Win
ON LEAVE OF permisos_embarque.id_aduana IN FRAME F-Main /* Aduana */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_aduana V-table-Win
ON MOUSE-SELECT-DBLCLICK OF permisos_embarque.id_aduana IN FRAME F-Main /* Aduana */
do: 
define var r as rowid no-undo.
run wc_aduanas.w(output r).
find aduanas where rowid(aduanas) = r no-lock no-error.
if available aduanas then 
general.permisos_embarque.id_aduana:screen-value = string(aduanas.id_aduana).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_aduana V-table-Win
ON U1 OF permisos_embarque.id_aduana IN FRAME F-Main /* Aduana */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.id_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_cliente V-table-Win
ON GO OF permisos_embarque.id_cliente IN FRAME F-Main /* CodCliente */
DO:
  run descriptivos2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_cliente V-table-Win
ON LEAVE OF permisos_embarque.id_cliente IN FRAME F-Main /* CodCliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_cliente V-table-Win
ON U1 OF permisos_embarque.id_cliente IN FRAME F-Main /* CodCliente */
DO:
  run descriptivos2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.id_despachante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_despachante V-table-Win
ON GO OF permisos_embarque.id_despachante IN FRAME F-Main /* Cod.Despachante */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_despachante V-table-Win
ON LEAVE OF permisos_embarque.id_despachante IN FRAME F-Main /* Cod.Despachante */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_despachante V-table-Win
ON MOUSE-SELECT-DBLCLICK OF permisos_embarque.id_despachante IN FRAME F-Main /* Cod.Despachante */
do: 
define var r as rowid no-undo.
run wc_despachantes.w(output r).
find despachantes where rowid(despachantes) = r no-lock no-error.
if available despachantes then 
general.permisos_embarque.id_despachante:screen-value = string(despachantes.id_despachante).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_despachante V-table-Win
ON U1 OF permisos_embarque.id_despachante IN FRAME F-Main /* Cod.Despachante */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.id_moneda_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_moneda_origen V-table-Win
ON GO OF permisos_embarque.id_moneda_origen IN FRAME F-Main /* Moneda Origen */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_moneda_origen V-table-Win
ON LEAVE OF permisos_embarque.id_moneda_origen IN FRAME F-Main /* Moneda Origen */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_moneda_origen V-table-Win
ON MOUSE-SELECT-DBLCLICK OF permisos_embarque.id_moneda_origen IN FRAME F-Main /* Moneda Origen */
do: 
define var r as rowid no-undo.
run wc_tipo_moneda.w(output r).
find tipo_moneda where rowid(tipo_moneda) = r no-lock no-error.
if available tipo_moneda then 
general.permisos_embarque.id_moneda_origen:screen-value = string(tipo_moneda.id_moneda).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_moneda_origen V-table-Win
ON U1 OF permisos_embarque.id_moneda_origen IN FRAME F-Main /* Moneda Origen */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.id_orden_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_orden_entrega V-table-Win
ON LEAVE OF permisos_embarque.id_orden_entrega IN FRAME F-Main /* OE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_orden_entrega V-table-Win
ON MOUSE-SELECT-DBLCLICK OF permisos_embarque.id_orden_entrega IN FRAME F-Main /* OE */
do: 
define var r as rowid no-undo.
run wc_orden_entrega_todos.w(output r).
find orden_entrega where rowid(orden_entrega) = r no-lock no-error.
if available orden_entrega then 
general.permisos_embarque.id_orden_entrega:screen-value = string(orden_entrega.id_orden_entrega).
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.id_permiso_embarque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_permiso_embarque V-table-Win
ON LEAVE OF permisos_embarque.id_permiso_embarque IN FRAME F-Main /* Permiso Embarque */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.id_posicion_arancelaria
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_posicion_arancelaria V-table-Win
ON GO OF permisos_embarque.id_posicion_arancelaria IN FRAME F-Main /* Posicion Arancela. */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_posicion_arancelaria V-table-Win
ON LEAVE OF permisos_embarque.id_posicion_arancelaria IN FRAME F-Main /* Posicion Arancela. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_posicion_arancelaria V-table-Win
ON MOUSE-SELECT-DBLCLICK OF permisos_embarque.id_posicion_arancelaria IN FRAME F-Main /* Posicion Arancela. */
do: 
define var r as rowid no-undo.
run wc_posicion_arancelaria.w(output r).
find posicion_arancelaria where rowid(posicion_arancelaria) = r no-lock no-error.
if available posicion_arancelaria then 
general.permisos_embarque.id_posicion_arancelaria:screen-value = string(posicion_arancelaria.id_posicion_arancelaria).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.id_posicion_arancelaria V-table-Win
ON U1 OF permisos_embarque.id_posicion_arancelaria IN FRAME F-Main /* Posicion Arancela. */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.importe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.importe V-table-Win
ON LEAVE OF permisos_embarque.importe IN FRAME F-Main /* Importe */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.importe_derecho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.importe_derecho V-table-Win
ON LEAVE OF permisos_embarque.importe_derecho IN FRAME F-Main /* Importe derecho */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.importe_reembolso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.importe_reembolso V-table-Win
ON LEAVE OF permisos_embarque.importe_reembolso IN FRAME F-Main /* Importe Reemb. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.importe_reintegro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.importe_reintegro V-table-Win
ON LEAVE OF permisos_embarque.importe_reintegro IN FRAME F-Main /* Importe Reintegro */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.item_oe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.item_oe V-table-Win
ON LEAVE OF permisos_embarque.item_oe IN FRAME F-Main /* Parte OE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.rectificado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.rectificado V-table-Win
ON LEAVE OF permisos_embarque.rectificado IN FRAME F-Main /* Rectificado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME permisos_embarque.tipo_pe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.tipo_pe V-table-Win
ON GO OF permisos_embarque.tipo_pe IN FRAME F-Main /* TipoPE */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.tipo_pe V-table-Win
ON LEAVE OF permisos_embarque.tipo_pe IN FRAME F-Main /* TipoPE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.tipo_pe V-table-Win
ON MOUSE-SELECT-DBLCLICK OF permisos_embarque.tipo_pe IN FRAME F-Main /* TipoPE */
do: 
define var r as rowid no-undo.
run wc_tipos_pe.w(output r).
find tipos_pe where rowid(tipos_pe) = r no-lock no-error.
if available tipos_pe then 
general.permisos_embarque.tipo_pe:screen-value = string(tipos_pe.id_tipo_pe).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL permisos_embarque.tipo_pe V-table-Win
ON U1 OF permisos_embarque.tipo_pe IN FRAME F-Main /* TipoPE */
DO:
{custom/support/validacion.i}
     run descriptivos.
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
DEFINE VAR hcon AS HANDLE.
DEFINE VAR v_tipo_pe AS INTEGER.
DEFINE VAR v_moneda AS INTEGER.

RUN get-container (OUTPUT hcon).
RUN GET-p_tipo_pe IN hcon (OUTPUT v_tipo_pe).

ASSIGN /* permisos_embarque.tipo_pe   = v_tipo_pe */
       permisos_embarque.c_usuario = userid("userdb")
       permisos_embarque.c_fecha   = today
       permisos_embarque.c_hora    = string(time,"HH:MM:SS").

v_moneda = INTEGER(permisos_embarque.id_moneda_origen:SCREEN-VALUE IN FRAME F-Main).

FIND FIRST tipo_moneda WHERE tipo_moneda.id_moneda = v_moneda NO-LOCK NO-ERROR.
IF v_moneda = 0 OR NOT AVAILABLE tipo_moneda THEN DO:
    MESSAGE "Por favor cargue la moneda origen del permiso de embarque." VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
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
IF permisos_embarque.id_cliente = 0 THEN DO:
    FIND FIRST r_subd_ventas_embarque WHERE r_subd_ventas_embarque.anio_permiso =
                                            permisos_embarque.anio
                                        AND r_subd_ventas_embarque.id_aduana =
                                            permisos_embarque.id_aduana
                                        AND r_subd_ventas_embarque.nro_embarque =
                                            permisos_embarque.id_permiso_embarque
                                        NO-LOCK NO-ERROR.
    IF AVAILABLE r_subd_ventas_embarque THEN DO:
        FIND FIRST subd_vtas OF r_subd_ventas_embarque NO-LOCK NO-ERROR.
        IF AVAILABLE subd_vtas THEN DO:
            ASSIGN permisos_embarque.id_cliente = subd_vtas.id_cliente.
        END.
    END.
END.
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
  {src/adm/template/row-list.i "permisos_embarque"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "permisos_embarque"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo-fecha-decla-vta V-table-Win 
PROCEDURE calculo-fecha-decla-vta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_r_pe AS ROWID.
FIND permisos_embarque WHERE ROWID(permisos_embarque) = p_r_pe NO-ERROR.
IF AVAILABLE permisos_embarque THEN DO:
    IF permisos_embarque.consignacion THEN DO:
        permisos_embarque.consignacion:HIDDEN IN FRAME F-Main            = FALSE.
        permisos_embarque.fecha_declaracion_venta:HIDDEN IN FRAME F-Main = FALSE.
        IF permisos_embarque.fecha_declaracion_venta = ? THEN DO:
            permisos_embarque.fecha_declaracion_venta = 
                permisos_embarque.fecha_oficializacion + 180.
        END.
    END.
    
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos V-table-Win 
PROCEDURE descriptivos :
find first aduanas where aduanas.id_aduana = integer(permisos_embarque.id_aduana:screen-value in frame F-Main)  no-lock no-error .
if available aduanas then 
fi-aduanas-descripcion:screen-value in frame F-Main = string(aduanas.descripcion).
else
fi-aduanas-descripcion:screen-value in frame F-Main = ''.

find first tipo_moneda where tipo_moneda.id_moneda = integer(permisos_embarque.id_moneda_origen:screen-value in frame F-Main)  no-lock no-error .
if available tipo_moneda then 
fi-tipo_moneda-descripcion:screen-value in frame F-Main = string(tipo_moneda.descripcion).
else
fi-tipo_moneda-descripcion:screen-value in frame F-Main = ''.

find first tipos_pe where tipos_pe.id_tipo_pe = integer(permisos_embarque.tipo_pe:screen-value in frame F-Main)  no-lock no-error .
if available tipos_pe then 
fi-tipos_pe-descripcion:screen-value in frame F-Main = string(tipos_pe.descripcion).
else
fi-tipos_pe-descripcion:screen-value in frame F-Main = ''.

find first posicion_arancelaria where posicion_arancelaria.id_posicion_arancelaria = permisos_embarque.id_posicion_arancelaria:screen-value in frame F-Main  no-lock no-error .
if available posicion_arancelaria then 
fi-posicion_arancelaria-descrip:screen-value in frame F-Main = string(posicion_arancelaria.descripcion).
else
fi-posicion_arancelaria-descrip:screen-value in frame F-Main = ''.

find first despachantes where despachantes.id_despachante = integer(permisos_embarque.id_despachante:screen-value in frame F-Main)  no-lock no-error .
if available despachantes then 
fi-despachantes-descripcion:screen-value in frame F-Main = string(despachantes.descripcion).
else
fi-despachantes-descripcion:screen-value in frame F-Main = ''.

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
define var r as rowid no-undo.

run wc_items_orden_entrega.w(output r).
find items_orden_entrega where rowid(items_orden_entrega) = r no-lock no-error.
if available items_orden_entrega then 
    do:
        permisos_embarque.id_orden_entrega:screen-value in frame F-Main = string(items_orden_entrega.id_orden_entrega).
        permisos_embarque.ITEM_oe:screen-value in frame F-Main          = string(items_orden_entrega.ITEM_oe).
        permisos_embarque.importe:screen-value in frame F-Main          = string(items_orden_entrega.fob_ton).
    end.
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
find first clientes where clientes.id_cliente = integer(permisos_embarque.id_cliente:screen-value in frame F-Main)  no-lock no-error .
if available clientes then 
clientes-descripcion:screen-value in frame F-Main = string(clientes.nombre).
else
clientes-descripcion:screen-value in frame F-Main = ''.
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
define var lista_relacion as character no-undo initial "id_orden_entrega,id_aduana,id_moneda_origen,tipo_pe,id_posicion_arancelaria,id_despachante".
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
  permisos_embarque.id_posicion_arancelaria:SCREEN-VALUE IN FRAME F-Main = "000000000000".
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
  permisos_embarque.id_posicion_arancelaria:SCREEN-VALUE IN FRAME F-Main = "000000000000".
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
  {src/adm/template/snd-list.i "permisos_embarque"}

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
      WHEN "anio" THEN DO:
          IF INTEGER(valor) > YEAR(TODAY)  THEN DO:
              mensaje = "EL VALOR INGRESADO ES MAYOR AL ANIO EN CURSO!!!".
              RETURN FALSE.
          END.

          IF INTEGER(valor) < (YEAR(TODAY) - 1)  THEN DO:
              mensaje = "EL ANIO INGRESADO ES INCORRECTO!!".
              RETURN FALSE.
          END.
     END.
     WHEN "id_aduana" THEN DO:
         FIND aduanas WHERE aduanas.id_aduana = INTEGER(valor) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aduanas THEN DO:
             mensaje = "LA ADUANA INGRESADA ES INCORRECTA!!".
             RETURN FALSE.
         END.

     END.
     WHEN "importe" THEN DO:
        IF DECIMAL(valor) = 0 THEN DO:
           mensaje = "EL IMPORTE INGRESADO DEBE SER DISTINTO DE CERO!!".
           RETURN FALSE.
        END.
     END.

     WHEN "id_permiso_embarque" THEN DO:

        DEFINE VARIABLE v1 AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE v2 AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE v3 AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE i  AS INTEGER    NO-UNDO.
        DEFINE VARIABLE vn AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE ppe AS CHARACTER    NO-UNDO.

        ppe = valor.

        /* CONTROL DE FORMATO */
        IF LENGTH(ppe) <> 11 THEN DO:
            mensaje = "DEBE INGRESAR UN FORMATO DE PERMISO VALIDO, CHEQUEE LA CANTIDAD DE CARACTERES!!".
            RETURN FALSE.
        END.

        v1 = SUBSTRING(ppe,1,1).
        v2 = SUBSTRING(ppe,2,1).
        v3 = SUBSTRING(ppe,LENGTH(ppe),1).
        
        IF LOOKUP(v1,"0,1,2,3,4,5,6,7,8,9") <> 0 THEN DO:
            mensaje = "LA PRIMERA POSICION DEL CODIGO DEL PERMISO DEBE SER UNA LETRA !!".
            RETURN FALSE.
        END.

        IF LOOKUP(v2,"0,1,2,3,4,5,6,7,8,9") <> 0 THEN DO:
            mensaje = "LA SEGUNDA POSICION DEL CODIGO DEL PERMISO DEBE SER UNA LETRA !!".
            RETURN FALSE.
        END.

        IF LOOKUP(v3,"0,1,2,3,4,5,6,7,8,9") <> 0 THEN DO:
            mensaje = "LA ULTIMA POSICION DEL CODIGO DEL PERMISO DEBE SER UNA LETRA !!".
            RETURN FALSE.
        END.
            
        DO i = 3 TO 10:
            vn = substring(ppe,i,1).
            IF LOOKUP(vn,"0,1,2,3,4,5,6,7,8,9") = 0 THEN DO:
                mensaje = "LOS CARACTERES DEL 3 AL 10 DEBEN SER NUMEROS!!".  
                RETURN FALSE.
            END.
        END.

     END.

    when "id_moneda_origen" THEN DO:
        FIND FIRST tipo_moneda WHERE tipo_moneda.id_moneda = INTEGER(valor) NO-LOCK NO-ERROR.
        IF INTEGER(valor) = 0 OR NOT AVAILABLE tipo_moneda THEN DO:
            mensaje = "Por favor cargue la moneda origen del permiso de embarque.".
            return false.
         end.
     END.
     when "tipo_pe" then
        if integer(valor) < 1 OR INTEGER(valor) > 3 then 
        do:
            mensaje = "Por favor ingrese un Tipo de PE valido.".
            return false.
         end.
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

