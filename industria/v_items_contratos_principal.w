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
define var v_id_contrato as char.
define var v_id_tipo_contrato as integer.
define var v_anio as integer.
define var v_item as integer.

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
&Scoped-define EXTERNAL-TABLES items_contratos
&Scoped-define FIRST-EXTERNAL-TABLE items_contratos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR items_contratos.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS items_contratos.item items_contratos.fecha ~
items_contratos.semana_entrega items_contratos.semana_entrega_hasta ~
items_contratos.anio_semana_entrega items_contratos.cantidad ~
items_contratos.id_articulo items_contratos.id_articulo_cliente[1] ~
items_contratos.id_po_cliente[1] items_contratos.numero_release[1] ~
items_contratos.id_clausula items_contratos.id_calidad ~
items_contratos.marca_tambores items_contratos.id_envase ~
items_contratos.id_tipo_unidad_venta items_contratos.observaciones ~
items_contratos.embarque_estimado items_contratos.arribo_estimado ~
items_contratos.id_from items_contratos.id_destino ~
items_contratos.id_deposito_final items_contratos.destino_final ~
items_contratos.id_puerto_ent items_contratos.id_tipo_venta ~
items_contratos.id_vapor items_contratos.estado items_contratos.pendiente ~
items_contratos.cert_fito items_contratos.id_puerto_sal 
&Scoped-define ENABLED-TABLES items_contratos
&Scoped-define FIRST-ENABLED-TABLE items_contratos
&Scoped-Define ENABLED-OBJECTS BUTTON-2 BUTTON-1 
&Scoped-Define DISPLAYED-FIELDS items_contratos.item items_contratos.fecha ~
items_contratos.semana_entrega items_contratos.semana_entrega_hasta ~
items_contratos.anio_semana_entrega items_contratos.cantidad ~
items_contratos.id_articulo items_contratos.id_articulo_cliente[1] ~
items_contratos.id_po_cliente[1] items_contratos.numero_release[1] ~
items_contratos.id_clausula items_contratos.id_calidad ~
items_contratos.marca_tambores items_contratos.id_envase ~
items_contratos.id_tipo_unidad_venta items_contratos.observaciones ~
items_contratos.embarque_estimado items_contratos.arribo_estimado ~
items_contratos.id_from items_contratos.id_destino ~
items_contratos.id_deposito_final items_contratos.destino_final ~
items_contratos.id_puerto_ent items_contratos.id_tipo_venta ~
items_contratos.id_vapor items_contratos.estado items_contratos.pendiente ~
items_contratos.cert_fito items_contratos.id_puerto_sal 
&Scoped-define DISPLAYED-TABLES items_contratos
&Scoped-define FIRST-DISPLAYED-TABLE items_contratos
&Scoped-Define DISPLAYED-OBJECTS fi-productos_terminados-descrip ~
fi-clausulas-descripcion fi-calidades-descripcion envases_prod-descripcion ~
fi-tipo_unidad_venta-descripcio fi-lugar_descarga-descripcion ~
fi-destinos-descripcion-2 fi-destinos-descripcion-1 fi-destinos-descripcion ~
fi-puertos-nombre fi-tipo_venta-descripcion fi-vapores-descripcion ~
fi-puertos-nombre-1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS items_contratos.item 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_tipo_contrato||y|general.items_contratos.id_tipo_contrato
id_tipo_unidad_venta||y|general.items_contratos.id_tipo_unidad_venta
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_tipo_contrato,id_tipo_unidad_venta"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valida V-table-Win  _DB-REQUIRED
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "Button 1" 
     SIZE 8 BY .95.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "Button 2" 
     SIZE 8 BY .95.

DEFINE VARIABLE envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-calidades-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-clausulas-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-destinos-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-destinos-descripcion-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-destinos-descripcion-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-lugar_descarga-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-puertos-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-puertos-nombre-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipo_unidad_venta-descripcio AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipo_venta-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-vapores-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     items_contratos.item AT ROW 1 COL 14 COLON-ALIGNED
          LABEL "Parte" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     items_contratos.fecha AT ROW 1 COL 35 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     items_contratos.semana_entrega AT ROW 1 COL 79 COLON-ALIGNED
          LABEL "Semana"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     items_contratos.semana_entrega_hasta AT ROW 1 COL 87 COLON-ALIGNED
          LABEL "-"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     items_contratos.anio_semana_entrega AT ROW 1 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     items_contratos.cantidad AT ROW 1 COL 119 COLON-ALIGNED FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     items_contratos.id_articulo AT ROW 1.95 COL 14 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-productos_terminados-descrip AT ROW 1.95 COL 21 COLON-ALIGNED NO-LABEL
     items_contratos.id_articulo_cliente[1] AT ROW 1.95 COL 87 COLON-ALIGNED
          LABEL "Cod Prod Cliente"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     items_contratos.id_po_cliente[1] AT ROW 1.95 COL 119 COLON-ALIGNED
          LABEL "PO Cliente"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     items_contratos.numero_release[1] AT ROW 2.91 COL 14 COLON-ALIGNED
          LABEL "Nro Release"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     items_contratos.id_clausula AT ROW 2.91 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-clausulas-descripcion AT ROW 2.91 COL 94 COLON-ALIGNED NO-LABEL
     items_contratos.id_calidad AT ROW 3.86 COL 14 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-calidades-descripcion AT ROW 3.86 COL 21 COLON-ALIGNED NO-LABEL
     items_contratos.marca_tambores AT ROW 4.1 COL 89 NO-LABEL
          VIEW-AS EDITOR
          SIZE 50 BY 2
     items_contratos.id_envase AT ROW 4.81 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     envases_prod-descripcion AT ROW 4.81 COL 22 COLON-ALIGNED NO-LABEL
     items_contratos.id_tipo_unidad_venta AT ROW 5.76 COL 14 COLON-ALIGNED
          LABEL "U. de Peso"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-tipo_unidad_venta-descripcio AT ROW 5.76 COL 26 COLON-ALIGNED NO-LABEL
     BUTTON-2 AT ROW 5.76 COL 71
     items_contratos.observaciones AT ROW 6.48 COL 89 NO-LABEL
          VIEW-AS EDITOR
          SIZE 50 BY 2
     items_contratos.embarque_estimado AT ROW 6.71 COL 14 COLON-ALIGNED
          LABEL "Embarque Est."
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     items_contratos.arribo_estimado AT ROW 6.71 COL 44 COLON-ALIGNED
          LABEL "Arribo Est"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     items_contratos.id_from AT ROW 7.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-lugar_descarga-descripcion AT ROW 7.67 COL 26 COLON-ALIGNED NO-LABEL
     BUTTON-1 AT ROW 7.67 COL 71
     items_contratos.id_destino AT ROW 8.62 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     fi-destinos-descripcion-2 AT ROW 8.62 COL 22 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     items_contratos.id_deposito_final AT ROW 8.62 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     fi-destinos-descripcion-1 AT ROW 8.62 COL 95 COLON-ALIGNED NO-LABEL
     items_contratos.destino_final AT ROW 9.57 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-destinos-descripcion AT ROW 9.57 COL 26 COLON-ALIGNED NO-LABEL
     items_contratos.id_puerto_ent AT ROW 9.57 COL 87 COLON-ALIGNED
          LABEL "PuertoEntrada"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     fi-puertos-nombre AT ROW 9.57 COL 96 COLON-ALIGNED NO-LABEL
     items_contratos.id_tipo_venta AT ROW 10.52 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     fi-tipo_venta-descripcion AT ROW 10.52 COL 25 COLON-ALIGNED NO-LABEL
     items_contratos.id_vapor AT ROW 10.52 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-vapores-descripcion AT ROW 10.52 COL 99 COLON-ALIGNED NO-LABEL
     items_contratos.estado AT ROW 11.48 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     items_contratos.pendiente AT ROW 11.48 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     items_contratos.cert_fito AT ROW 11.48 COL 67 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     items_contratos.id_puerto_sal AT ROW 11.48 COL 87 COLON-ALIGNED
          LABEL "PuertoSal"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-puertos-nombre-1 AT ROW 11.48 COL 99 COLON-ALIGNED NO-LABEL
     "Observ. Grales:" VIEW-AS TEXT
          SIZE 15 BY .95 AT ROW 6.71 COL 73
     "Marca Tambores:" VIEW-AS TEXT
          SIZE 17 BY .95 AT ROW 4.1 COL 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.items_contratos
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
         HEIGHT             = 11.48
         WIDTH              = 138.
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

/* SETTINGS FOR FILL-IN items_contratos.arribo_estimado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_contratos.cantidad IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_contratos.embarque_estimado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-calidades-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-clausulas-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-destinos-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-destinos-descripcion-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-destinos-descripcion-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lugar_descarga-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-puertos-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-puertos-nombre-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo_unidad_venta-descripcio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo_venta-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vapores-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN items_contratos.id_articulo IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_contratos.id_articulo_cliente[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_contratos.id_calidad IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_contratos.id_po_cliente[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_contratos.id_puerto_ent IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_contratos.id_puerto_sal IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_contratos.id_tipo_unidad_venta IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_contratos.item IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN items_contratos.numero_release[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_contratos.semana_entrega IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_contratos.semana_entrega_hasta IN FRAME F-Main
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
general.items_contratos.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
general.items_contratos.destino_final;wc_destinos.w;destinos.descripcion;id_destino;
general.items_contratos.id_tipo_venta;wc_tipo_venta.w;tipo_venta.descripcion;;
general.items_contratos.id_deposito_final;wc_destinos_depositos.w;destinos.descripcion;id_destino;
general.items_contratos.id_vapor;wc_vapores.w;vapores.descripcion;;
general.items_contratos.id_puerto_ent;wc_puertos.w;puertos.nombre;id_puerto;
general.items_contratos.id_puerto_sal;wc_puertos.w;puertos.nombre;id_puerto;
general.items_contratos.id_from;wc_lugar_descarga.w;lugar_descarga.descripcion;id_lugdes;
general.items_contratos.id_destino;wc_destinos.w;destinos.descripcion;;
general.items_contratos.id_tipo_unidad_venta;wc_tipo_unidad_venta.w;tipo_unidad_venta.descripcion;;
general.items_contratos.id_calidad;wc_calidades.w;calidades.descripcion;;
general.items_contratos.id_clausula;wc_clausulas.w;clausulas.descripcion;;
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

&Scoped-define SELF-NAME items_contratos.anio_semana_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.anio_semana_entrega V-table-Win
ON LEAVE OF items_contratos.anio_semana_entrega IN FRAME F-Main /* A�o */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.arribo_estimado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.arribo_estimado V-table-Win
ON LEAVE OF items_contratos.arribo_estimado IN FRAME F-Main /* Arribo Est */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 V-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  run w_destinos.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 V-table-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
  run w_tipo_unidad_venta.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.cantidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.cantidad V-table-Win
ON LEAVE OF items_contratos.cantidad IN FRAME F-Main /* Cantidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.cert_fito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.cert_fito V-table-Win
ON LEAVE OF items_contratos.cert_fito IN FRAME F-Main /* Cert.Fitosanitario */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.destino_final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.destino_final V-table-Win
ON GO OF items_contratos.destino_final IN FRAME F-Main /* Destino Final */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.destino_final V-table-Win
ON LEAVE OF items_contratos.destino_final IN FRAME F-Main /* Destino Final */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.destino_final V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.destino_final IN FRAME F-Main /* Destino Final */
do: 
define var r as rowid no-undo.
run wc_destinos.w(output r).
find destinos where rowid(destinos) = r no-lock no-error.
if available destinos then 
general.items_contratos.destino_final:screen-value = string(destinos.id_destino).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.destino_final V-table-Win
ON U1 OF items_contratos.destino_final IN FRAME F-Main /* Destino Final */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.embarque_estimado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.embarque_estimado V-table-Win
ON LEAVE OF items_contratos.embarque_estimado IN FRAME F-Main /* Embarque Est. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME envases_prod-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL envases_prod-descripcion V-table-Win
ON LEAVE OF envases_prod-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.estado V-table-Win
ON LEAVE OF items_contratos.estado IN FRAME F-Main /* Estado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.fecha V-table-Win
ON LEAVE OF items_contratos.fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-calidades-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-calidades-descripcion V-table-Win
ON LEAVE OF fi-calidades-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-clausulas-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-clausulas-descripcion V-table-Win
ON LEAVE OF fi-clausulas-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-destinos-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-destinos-descripcion V-table-Win
ON LEAVE OF fi-destinos-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-destinos-descripcion-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-destinos-descripcion-1 V-table-Win
ON LEAVE OF fi-destinos-descripcion-1 IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-destinos-descripcion-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-destinos-descripcion-2 V-table-Win
ON LEAVE OF fi-destinos-descripcion-2 IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-lugar_descarga-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-lugar_descarga-descripcion V-table-Win
ON LEAVE OF fi-lugar_descarga-descripcion IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-puertos-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-puertos-nombre V-table-Win
ON LEAVE OF fi-puertos-nombre IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-puertos-nombre-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-puertos-nombre-1 V-table-Win
ON LEAVE OF fi-puertos-nombre-1 IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo_unidad_venta-descripcio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo_unidad_venta-descripcio V-table-Win
ON LEAVE OF fi-tipo_unidad_venta-descripcio IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo_venta-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo_venta-descripcion V-table-Win
ON LEAVE OF fi-tipo_venta-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-vapores-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-vapores-descripcion V-table-Win
ON LEAVE OF fi-vapores-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_articulo V-table-Win
ON GO OF items_contratos.id_articulo IN FRAME F-Main /* Articulo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_articulo V-table-Win
ON LEAVE OF items_contratos.id_articulo IN FRAME F-Main /* Articulo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_articulo IN FRAME F-Main /* Articulo */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.items_contratos.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_articulo V-table-Win
ON U1 OF items_contratos.id_articulo IN FRAME F-Main /* Articulo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_articulo_cliente[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_articulo_cliente[1] V-table-Win
ON LEAVE OF items_contratos.id_articulo_cliente[1] IN FRAME F-Main /* Cod Prod Cliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_calidad V-table-Win
ON GO OF items_contratos.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_calidad V-table-Win
ON LEAVE OF items_contratos.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_calidad V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_calidad IN FRAME F-Main /* Calidad */
do: 
define var r as rowid no-undo.
run wc_calidades.w(output r).
find calidades where rowid(calidades) = r no-lock no-error.
if available calidades then 
general.items_contratos.id_calidad:screen-value = string(calidades.id_calidad).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_calidad V-table-Win
ON U1 OF items_contratos.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_clausula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_clausula V-table-Win
ON GO OF items_contratos.id_clausula IN FRAME F-Main /* Clausula */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_clausula V-table-Win
ON LEAVE OF items_contratos.id_clausula IN FRAME F-Main /* Clausula */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_clausula V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_clausula IN FRAME F-Main /* Clausula */
do: 
define var r as rowid no-undo.
run wc_clausulas.w(output r).
find clausulas where rowid(clausulas) = r no-lock no-error.
if available clausulas then 
general.items_contratos.id_clausula:screen-value = string(clausulas.id_clausula).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_clausula V-table-Win
ON U1 OF items_contratos.id_clausula IN FRAME F-Main /* Clausula */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_deposito_final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_deposito_final V-table-Win
ON GO OF items_contratos.id_deposito_final IN FRAME F-Main /* Cod.Deposito */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_deposito_final V-table-Win
ON LEAVE OF items_contratos.id_deposito_final IN FRAME F-Main /* Cod.Deposito */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_deposito_final V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_deposito_final IN FRAME F-Main /* Cod.Deposito */
do: 
define var r as rowid no-undo.
run wc_destinos_depositos.w(output r).
find destinos where rowid(destinos) = r no-lock no-error.
if available destinos then 
general.items_contratos.id_deposito_final:screen-value = string(destinos.id_destino).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_deposito_final V-table-Win
ON U1 OF items_contratos.id_deposito_final IN FRAME F-Main /* Cod.Deposito */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_destino V-table-Win
ON GO OF items_contratos.id_destino IN FRAME F-Main /* Destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_destino V-table-Win
ON LEAVE OF items_contratos.id_destino IN FRAME F-Main /* Destino */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_destino V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_destino IN FRAME F-Main /* Destino */
do: 
define var r as rowid no-undo.
run wc_destinos.w(output r).
find destinos where rowid(destinos) = r no-lock no-error.
if available destinos then 
general.items_contratos.id_destino:screen-value = string(destinos.id_destino).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_destino V-table-Win
ON U1 OF items_contratos.id_destino IN FRAME F-Main /* Destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_envase V-table-Win
ON GO OF items_contratos.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos-envases_prod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_envase V-table-Win
ON LEAVE OF items_contratos.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_envase V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_envase IN FRAME F-Main /* Envase */
do: 
define var r as rowid no-undo.
DEFINE VAR hcon AS HANDLE.
DEFINE VAR v_tipo_con AS INTEGER.
DEFINE VAR v_con AS CHAR.
DEFINE VAR v_anio AS INTEGER.

RUN get-container (OUTPUT hcon).
RUN dame_datos_contrato IN hcon (OUTPUT v_con,
                                 OUTPUT v_tipo_con,
                                 OUTPUT v_anio).
/*
IF v_tipo_con = 3 THEN DO:
    run wc_envases.w(output r).
    find envases_prod where rowid(envases_prod) = r no-lock no-error.
    if available envases_prod then 
        general.items_contratos.id_envase:screen-value = string(envases_prod.id_envase).
END.
ELSE DO:*/
    run wc_envases_new.w(INPUT INTEGER(items_contratos.id_articulo:SCREEN-VALUE IN FRAME F-Main),
                         INPUT INTEGER(items_contratos.id_calidad:SCREEN-VALUE IN FRAME F-Main),
                         output r).
    find envases_prod where rowid(envases_prod) = r no-lock no-error.
    if available envases_prod then 
        general.items_contratos.id_envase:screen-value = string(envases_prod.id_envase).
/*END.*/

apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_envase V-table-Win
ON U1 OF items_contratos.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos-envases_prod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_from
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_from V-table-Win
ON GO OF items_contratos.id_from IN FRAME F-Main /* Origen */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_from V-table-Win
ON LEAVE OF items_contratos.id_from IN FRAME F-Main /* Origen */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_from V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_from IN FRAME F-Main /* Origen */
do: 
define var r as rowid no-undo.
run wc_lugar_descarga.w(output r).
find lugar_descarga where rowid(lugar_descarga) = r no-lock no-error.
if available lugar_descarga then 
general.items_contratos.id_from:screen-value = string(lugar_descarga.id_lugdes).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_from V-table-Win
ON U1 OF items_contratos.id_from IN FRAME F-Main /* Origen */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_po_cliente[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_po_cliente[1] V-table-Win
ON LEAVE OF items_contratos.id_po_cliente[1] IN FRAME F-Main /* PO Cliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_puerto_ent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_puerto_ent V-table-Win
ON GO OF items_contratos.id_puerto_ent IN FRAME F-Main /* PuertoEntrada */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_puerto_ent V-table-Win
ON LEAVE OF items_contratos.id_puerto_ent IN FRAME F-Main /* PuertoEntrada */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_puerto_ent V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_puerto_ent IN FRAME F-Main /* PuertoEntrada */
do: 
define var r as rowid no-undo.
run wc_puertos.w(output r).
find puertos where rowid(puertos) = r no-lock no-error.
if available puertos then 
general.items_contratos.id_puerto_ent:screen-value = string(puertos.id_puerto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_puerto_ent V-table-Win
ON U1 OF items_contratos.id_puerto_ent IN FRAME F-Main /* PuertoEntrada */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_puerto_sal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_puerto_sal V-table-Win
ON GO OF items_contratos.id_puerto_sal IN FRAME F-Main /* PuertoSal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_puerto_sal V-table-Win
ON LEAVE OF items_contratos.id_puerto_sal IN FRAME F-Main /* PuertoSal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_puerto_sal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_puerto_sal IN FRAME F-Main /* PuertoSal */
do: 
define var r as rowid no-undo.
run wc_puertos.w(output r).
find puertos where rowid(puertos) = r no-lock no-error.
if available puertos then 
general.items_contratos.id_puerto_sal:screen-value = string(puertos.id_puerto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_puerto_sal V-table-Win
ON U1 OF items_contratos.id_puerto_sal IN FRAME F-Main /* PuertoSal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_tipo_unidad_venta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_tipo_unidad_venta V-table-Win
ON GO OF items_contratos.id_tipo_unidad_venta IN FRAME F-Main /* U. de Peso */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_tipo_unidad_venta V-table-Win
ON LEAVE OF items_contratos.id_tipo_unidad_venta IN FRAME F-Main /* U. de Peso */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_tipo_unidad_venta V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_tipo_unidad_venta IN FRAME F-Main /* U. de Peso */
do: 
define var r as rowid no-undo.
run wc_tipo_unidad_venta.w(output r).
find tipo_unidad_venta where rowid(tipo_unidad_venta) = r no-lock no-error.
if available tipo_unidad_venta then 
general.items_contratos.id_tipo_unidad_venta:screen-value = string(tipo_unidad_venta.id_tipo_unidad_venta).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_tipo_unidad_venta V-table-Win
ON U1 OF items_contratos.id_tipo_unidad_venta IN FRAME F-Main /* U. de Peso */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_tipo_venta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_tipo_venta V-table-Win
ON GO OF items_contratos.id_tipo_venta IN FRAME F-Main /* CodTipoVenta */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_tipo_venta V-table-Win
ON LEAVE OF items_contratos.id_tipo_venta IN FRAME F-Main /* CodTipoVenta */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_tipo_venta V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_tipo_venta IN FRAME F-Main /* CodTipoVenta */
do: 
define var r as rowid no-undo.
run wc_tipo_venta.w(output r).
find tipo_venta where rowid(tipo_venta) = r no-lock no-error.
if available tipo_venta then 
general.items_contratos.id_tipo_venta:screen-value = string(tipo_venta.id_tipo_venta).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_tipo_venta V-table-Win
ON U1 OF items_contratos.id_tipo_venta IN FRAME F-Main /* CodTipoVenta */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.id_vapor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_vapor V-table-Win
ON GO OF items_contratos.id_vapor IN FRAME F-Main /* Vapor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_vapor V-table-Win
ON LEAVE OF items_contratos.id_vapor IN FRAME F-Main /* Vapor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_vapor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_contratos.id_vapor IN FRAME F-Main /* Vapor */
do: 
define var r as rowid no-undo.
run wc_vapores.w(output r).
find vapores where rowid(vapores) = r no-lock no-error.
if available vapores then 
general.items_contratos.id_vapor:screen-value = string(vapores.id_vapor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.id_vapor V-table-Win
ON U1 OF items_contratos.id_vapor IN FRAME F-Main /* Vapor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.item V-table-Win
ON LEAVE OF items_contratos.item IN FRAME F-Main /* Parte */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.numero_release[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.numero_release[1] V-table-Win
ON LEAVE OF items_contratos.numero_release[1] IN FRAME F-Main /* Nro Release */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.pendiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.pendiente V-table-Win
ON LEAVE OF items_contratos.pendiente IN FRAME F-Main /* Pendiente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.semana_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.semana_entrega V-table-Win
ON LEAVE OF items_contratos.semana_entrega IN FRAME F-Main /* Semana */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_contratos.semana_entrega_hasta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_contratos.semana_entrega_hasta V-table-Win
ON LEAVE OF items_contratos.semana_entrega_hasta IN FRAME F-Main /* - */
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
define var h_con as handle.
define var v_id_contrato as char.
define var v_id_tipo_contrato as integer.
define var v_anio as integer.
define var r_item as rowid.
define buffer b_item for items_contratos.
define buffer b_gastos_items for r_gastos_items_contrato.
define var v_item as integer.
define var bandera as integer.

run get-container (output h_con).

run dame_datos_contrato in h_con (output v_id_contrato,
                                  output v_id_tipo_contrato,
                                  output v_anio).
                                  
if v_id_tipo_contrato > 3 then
  do:  
    find tipos_contratos where tipos_contratos.id_tipo_contrato = v_id_tipo_contrato no-lock no-error.
    assign items_contratos.id_articulo = tipos_contratos.id_articulo
           items_contratos.id_calidad = tipos_contratos.id_calidad.                                   
  end.

assign items_contratos.id_contrato = v_id_contrato
       items_contratos.id_tipo_contrato = v_id_tipo_contrato
       items_contratos.anio = v_anio
       items_contratos.c_usuario = userid("userdb")
       items_contratos.c_fecha   = today
       items_contratos.c_hora    = string(time,"HH:MM:SS").

v_item = integer(items_contratos.item:screen-value in frame F-Main).       
run pasar-rowid-copy in h_con (output r_item, output bandera).       
if bandera = 1 then
    do:
        find b_item where rowid(b_item) = r_item no-lock no-error.
        if available b_item then
            do:
                for each b_gastos_items of b_item.
                    create r_gastos_items_contrato.
                    buffer-copy b_gastos_items except item to r_gastos_items_contrato.
                    assign r_gastos_items_contrato.item = v_item.
                end.
            end.
    end.
 else message "bandera = " bandera view-as alert-box.
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

/* AUDITO LA BORRADA */
create baja_contrato.
assign baja_contrato.id_contrato        = v_id_contrato
       baja_contrato.id_tipo_contrato   = v_id_tipo_contrato
       baja_contrato.anio               = v_anio
       baja_contrato.item               = v_item
       baja_contrato.c_usuario          = userid("userdb")
       baja_contrato.c_fecha            = today
       baja_contrato.c_hora             = string(time,"HH:MM:SS").
       

for each r_gastos_items_contrato where r_gastos_items_contrato.id_contrato = v_id_contrato
                                   and r_gastos_items_contrato.id_tipo_contrato = v_id_tipo_contrato
                                   and r_gastos_items_contrato.anio = v_anio
                                   and r_gastos_items_contrato.item = v_item.
    delete r_gastos_items_contrato.        
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
RUN confirma-deposito.
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
define var h_con as handle.
define var r as rowid.

run get-container (output h_con).
run dame_rowid_item in h_con (output r).

find items_contratos where rowid(items_contratos) = r no-lock no-error.
if available items_contratos then
    do:
        v_id_contrato = items_contrato.id_contrato.
        v_id_tipo_contrato = items_contrato.id_tipo_contrato.
        v_anio = items_contrato.anio.
        v_item = items_contrato.item.
    end.
    
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
  {src/adm/template/row-list.i "items_contratos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "items_contratos"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga_datos_iniciales V-table-Win 
PROCEDURE carga_datos_iniciales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter p_fecha as date.
define input parameter p_po as char.
define input parameter p_item as integer.
define var h_con as handle.
define var v_id_contrato as char.
define var v_id_tipo_contrato as integer.
define var v_anio as integer.

run get-container (output h_con).

run dame_datos_contrato in h_con (output v_id_contrato,
                                  output v_id_tipo_contrato,
                                  output v_anio).


if v_id_tipo_contrato > 4 then
  do:  
    find tipos_contratos where tipos_contratos.id_tipo_contrato = v_id_tipo_contrato no-lock no-error.
    items_contratos.id_articulo:screen-value in frame F-Main = string(tipos_contratos.id_articulo).
    items_contratos.id_calidad:screen-value in frame F-Main = string(tipos_contratos.id_calidad).                                   
  end.



items_contratos.fecha:screen-value in frame F-Main = string(p_fecha).
items_contratos.id_po_cliente[1]:screen-value in frame F-Main = p_po.
items_contratos.item:screen-value in frame F-Main = string(p_item).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE confirma-deposito V-table-Win 
PROCEDURE confirma-deposito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF general.items_contratos.id_tipo_venta:SCREEN-VALUE IN FRAME F-Main = "3" THEN DO:
    IF items_contratos.id_deposito:SCREEN-VALUE IN FRAME F-Main = "0" THEN DO:
        MESSAGE "Por favor ingrese el deposito destino de la mercaderia." VIEW-AS ALERT-BOX.
        RETURN "ADM-ERROR".
        RETURN "error".
    END.
END.

IF (items_contratos.id_destino:SCREEN-VALUE IN FRAME F-Main = "0") OR
   (items_contratos.destino_final:SCREEN-VALUE IN FRAME F-Main = "0")  THEN DO:
    MESSAGE "Por favor ingrese los destinos de la mercaderia.Este es un dato obligatorio" VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
    RETURN "error".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos V-table-Win 
PROCEDURE descriptivos :
find first productos_terminados where productos_terminados.id_articulo = integer(items_contratos.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first destinos where destinos.id_destino = integer(items_contratos.destino_final:screen-value in frame F-Main)  no-lock no-error .
if available destinos then 
fi-destinos-descripcion:screen-value in frame F-Main = string(destinos.descripcion).
else
fi-destinos-descripcion:screen-value in frame F-Main = ''.

find first tipo_venta where tipo_venta.id_tipo_venta = integer(items_contratos.id_tipo_venta:screen-value in frame F-Main)  no-lock no-error .
if available tipo_venta then 
fi-tipo_venta-descripcion:screen-value in frame F-Main = string(tipo_venta.descripcion).
else
fi-tipo_venta-descripcion:screen-value in frame F-Main = ''.

find first destinos where destinos.id_destino = integer(items_contratos.id_deposito_final:screen-value in frame F-Main)  no-lock no-error .
if available destinos then 
fi-destinos-descripcion-1:screen-value in frame F-Main = string(destinos.descripcion).
else
fi-destinos-descripcion-1:screen-value in frame F-Main = ''.

find first vapores where vapores.id_vapor = integer(items_contratos.id_vapor:screen-value in frame F-Main)  no-lock no-error .
if available vapores then 
fi-vapores-descripcion:screen-value in frame F-Main = string(vapores.descripcion).
else
fi-vapores-descripcion:screen-value in frame F-Main = ''.

find first puertos where puertos.id_puerto = integer(items_contratos.id_puerto_ent:screen-value in frame F-Main)  no-lock no-error .
if available puertos then 
fi-puertos-nombre:screen-value in frame F-Main = string(puertos.nombre).
else
fi-puertos-nombre:screen-value in frame F-Main = ''.

find first puertos where puertos.id_puerto = integer(items_contratos.id_puerto_sal:screen-value in frame F-Main)  no-lock no-error .
if available puertos then 
fi-puertos-nombre-1:screen-value in frame F-Main = string(puertos.nombre).
else
fi-puertos-nombre-1:screen-value in frame F-Main = ''.

find first lugar_descarga where lugar_descarga.id_lugdes = integer(items_contratos.id_from:screen-value in frame F-Main)  no-lock no-error .
if available lugar_descarga then 
fi-lugar_descarga-descripcion:screen-value in frame F-Main = string(lugar_descarga.descripcion).
else
fi-lugar_descarga-descripcion:screen-value in frame F-Main = ''.

find first destinos where destinos.id_destino = integer(items_contratos.id_destino:screen-value in frame F-Main)  no-lock no-error .
if available destinos then 
fi-destinos-descripcion-2:screen-value in frame F-Main = string(destinos.descripcion).
else
fi-destinos-descripcion-2:screen-value in frame F-Main = ''.

find first tipo_unidad_venta where tipo_unidad_venta.id_tipo_unidad_venta = integer(items_contratos.id_tipo_unidad_venta:screen-value in frame F-Main)  no-lock no-error .
if available tipo_unidad_venta then 
fi-tipo_unidad_venta-descripcio:screen-value in frame F-Main = string(tipo_unidad_venta.descripcion).
else
fi-tipo_unidad_venta-descripcio:screen-value in frame F-Main = ''.

find first calidades where calidades.id_calidad = integer(items_contratos.id_calidad:screen-value in frame F-Main)  no-lock no-error .
if available calidades then 
fi-calidades-descripcion:screen-value in frame F-Main = string(calidades.descripcion).
else
fi-calidades-descripcion:screen-value in frame F-Main = ''.

find first clausulas where clausulas.id_clausula = integer(items_contratos.id_clausula:screen-value in frame F-Main)  no-lock no-error .
if available clausulas then 
fi-clausulas-descripcion:screen-value in frame F-Main = string(clausulas.descripcion).
else
fi-clausulas-descripcion:screen-value in frame F-Main = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos-envases_prod V-table-Win 
PROCEDURE descriptivos-envases_prod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first envases_prod where envases_prod.id_envase = integer(items_contratos.id_envase:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
envases_prod-descripcion:screen-value in frame F-Main = ''.
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
define var h_con as handle.
define var v_id_contrato as char.
define var v_id_tipo_contrato as integer.
define var v_anio as integer.

run get-container (output h_con).

run dame_datos_contrato in h_con (output v_id_contrato,
                                  output v_id_tipo_contrato,
                                  output v_anio).
                                  
IF v_id_tipo_contrato = 1 OR
   v_id_tipo_contrato = 2 OR
   v_id_tipo_contrato = 3 OR
   v_id_tipo_contrato = 4 THEN DO:  
    items_contratos.id_articulo:visible in frame F-Main = true.
    items_contratos.id_calidad:visible in frame F-Main = true.                                 
  end.
else
  do:
    items_contratos.id_articulo:visible in frame F-Main = false.
    items_contratos.id_calidad:visible in frame F-Main = false.
  end.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilito_producto_calidad V-table-Win 
PROCEDURE deshabilito_producto_calidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
items_contratos.id_articulo:visible in frame F-Main = false.
items_contratos.id_calidad:visible in frame F-Main = false.
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
define var lista_relacion as character no-undo initial "id_articulo,destino_final,id_tipo_venta,id_deposito_final,id_vapor,id_puerto_ent,id_puerto_sal,id_from,id_destino,id_tipo_unidad_venta,id_calidad,id_clausula".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilito_producto_calidad V-table-Win 
PROCEDURE habilito_producto_calidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
items_contratos.id_articulo:visible in frame F-Main = true.
items_contratos.id_calidad:visible in frame F-Main = true.
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
  run descriptivos1.
  RUN descriptivos-envases_prod.
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
  {src/adm/template/sndkycas.i "id_tipo_contrato" "items_contratos" "id_tipo_contrato"}
  {src/adm/template/sndkycas.i "id_tipo_unidad_venta" "items_contratos" "id_tipo_unidad_venta"}

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
  {src/adm/template/snd-list.i "items_contratos"}

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
        when "id_envase" THEN DO:
            DEFINE VAR hcon AS HANDLE.
            DEFINE VAR v_tipo_con AS INTEGER.
            DEFINE VAR v_con AS CHAR.
            DEFINE VAR v_anio AS INTEGER.
            
            RUN get-container (OUTPUT hcon).
            RUN dame_datos_contrato IN hcon (OUTPUT v_con,
                                             OUTPUT v_tipo_con,
                                             OUTPUT v_anio).
            FIND FIRST r_productos_calidad_envase 
                WHERE r_productos_calidad_envase.id_articulo = INTEGER(items_contratos.id_articulo:SCREEN-VALUE IN FRAME F-Main)
                  AND r_productos_calidad_envase.id_calidad  = INTEGER(items_contratos.id_calidad:SCREEN-VALUE IN FRAME F-Main)
                  AND r_productos_calidad_envase.id_envase   = INTEGER(valor) 
              NO-LOCK NO-ERROR.
            IF NOT AVAILABLE r_productos_calidad_envase THEN 
            DO:
                mensaje = "Por favor use el envase correcto!".
                return false.
            END.
        END.
        when "id_destino" THEN DO:
            IF INTEGER(valor) = 0 THEN DO:
                mensaje = "Por favor cargue el destino. Este es un dato obligatorio".
                return false.
            END.
        END.
        when "destino_final" THEN DO:
            IF INTEGER(valor) = 0 THEN DO:
                mensaje = "Por favor cargue el destino. Este es un dato obligatorio".
                return false.
            END.
        END.
    end CASE.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
