&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
CREATE WIDGET-POOL.

define var v_item as integer.
define var v_anio as integer.
define var v_id_tipo_contrato as integer.
define var v_semana_entrega as integer.
define var v_id_clausula as integer.
define var v_id_orden_entrega as integer.
define var v_alta as integer initial 0.

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
&Scoped-define EXTERNAL-TABLES orden_entrega
&Scoped-define FIRST-EXTERNAL-TABLE orden_entrega


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR orden_entrega.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS orden_entrega.id_orden_entrega ~
orden_entrega.id_agencia orden_entrega.id_vapor orden_entrega.fecha ~
orden_entrega.id_contrato orden_entrega.id_destino orden_entrega.id_cliente ~
orden_entrega.id_calidad orden_entrega.id_articulo ~
orden_entrega.grados_brix orden_entrega.modo_actualizacion ~
orden_entrega.cantidad_tambores orden_entrega.kgs_netos_tambores ~
orden_entrega.kgs_brutos_tambores orden_entrega.semana_embarque ~
orden_entrega.fecha_embarque orden_entrega.plazo ~
orden_entrega.id_tipo_plazo orden_entrega.id_instrumento_pago ~
orden_entrega.id_condicion_venta orden_entrega.id_estado ~
orden_entrega.fecha_arribo orden_entrega.id_lugdes ~
orden_entrega.id_despachante orden_entrega.id_tipo_contenedor ~
orden_entrega.contenedores orden_entrega.total_factura ~
orden_entrega.total_galones orden_entrega.importe_comisiones ~
orden_entrega.fob_ton orden_entrega.fob_unitario 
&Scoped-define ENABLED-TABLES orden_entrega
&Scoped-define FIRST-ENABLED-TABLE orden_entrega
&Scoped-define DISPLAYED-TABLES orden_entrega
&Scoped-define FIRST-DISPLAYED-TABLE orden_entrega
&Scoped-Define DISPLAYED-FIELDS orden_entrega.id_orden_entrega ~
orden_entrega.id_agencia orden_entrega.id_vapor orden_entrega.fecha ~
orden_entrega.id_contrato orden_entrega.item orden_entrega.anio ~
orden_entrega.id_tipo_contrato orden_entrega.semana_entrega ~
orden_entrega.id_destino orden_entrega.id_cliente orden_entrega.id_calidad ~
orden_entrega.id_articulo orden_entrega.grados_brix ~
orden_entrega.modo_actualizacion orden_entrega.cantidad_tambores ~
orden_entrega.kgs_netos_tambores orden_entrega.kgs_brutos_tambores ~
orden_entrega.semana_embarque orden_entrega.fecha_embarque ~
orden_entrega.plazo orden_entrega.id_tipo_plazo ~
orden_entrega.id_instrumento_pago orden_entrega.id_condicion_venta ~
orden_entrega.id_estado orden_entrega.fecha_arribo orden_entrega.id_lugdes ~
orden_entrega.id_despachante orden_entrega.id_tipo_contenedor ~
orden_entrega.contenedores orden_entrega.total_factura ~
orden_entrega.total_galones orden_entrega.importe_comisiones ~
orden_entrega.fob_ton orden_entrega.fob_unitario 
&Scoped-Define DISPLAYED-OBJECTS fi-agencias-descripcion ~
fi-vapores-descripcion fi-destinos-descripcion fi-clientes-nombre ~
fi-calidades-descripcion fi-productos_terminados-descrip ~
fi-tipos_plazo-descripcion fi-instrumentos_pagos-descripci ~
fi-clausulas-descripcion fi-estados_oe-descripcion ~
fi-lugar_descarga-descripcion fi-despachantes-descripcion ~
fi-tipo_contenedor-descripcion 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS orden_entrega.id_orden_entrega 

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
DEFINE VARIABLE fi-agencias-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-calidades-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-clausulas-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-clientes-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-despachantes-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-destinos-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-estados_oe-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-instrumentos_pagos-descripci AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-lugar_descarga-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipos_plazo-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipo_contenedor-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-vapores-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     orden_entrega.id_orden_entrega AT ROW 1 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     orden_entrega.id_agencia AT ROW 1 COL 36 COLON-ALIGNED
          LABEL "Agencia"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-agencias-descripcion AT ROW 1 COL 43 COLON-ALIGNED NO-LABEL
     orden_entrega.id_vapor AT ROW 1 COL 96 COLON-ALIGNED
          LABEL "Vapor"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-vapores-descripcion AT ROW 1 COL 108 COLON-ALIGNED NO-LABEL
     orden_entrega.fecha AT ROW 1.95 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     orden_entrega.id_contrato AT ROW 1.95 COL 36 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     orden_entrega.item AT ROW 1.95 COL 59 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     orden_entrega.anio AT ROW 1.95 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     orden_entrega.id_tipo_contrato AT ROW 1.95 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     orden_entrega.semana_entrega AT ROW 1.95 COL 135 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     orden_entrega.id_destino AT ROW 2.91 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-destinos-descripcion AT ROW 2.91 COL 24 COLON-ALIGNED NO-LABEL
     orden_entrega.id_cliente AT ROW 2.91 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi-clientes-nombre AT ROW 2.91 COL 92 COLON-ALIGNED NO-LABEL
     orden_entrega.id_calidad AT ROW 3.86 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     fi-calidades-descripcion AT ROW 3.86 COL 20 COLON-ALIGNED NO-LABEL
     orden_entrega.id_articulo AT ROW 3.86 COL 75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     fi-productos_terminados-descrip AT ROW 3.86 COL 80 COLON-ALIGNED NO-LABEL
     orden_entrega.grados_brix AT ROW 3.86 COL 131 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     orden_entrega.modo_actualizacion AT ROW 4.81 COL 12.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     orden_entrega.cantidad_tambores AT ROW 4.81 COL 36 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     orden_entrega.kgs_netos_tambores AT ROW 4.81 COL 57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     orden_entrega.kgs_brutos_tambores AT ROW 4.81 COL 85 COLON-ALIGNED FORMAT ">,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     orden_entrega.semana_embarque AT ROW 4.81 COL 114 COLON-ALIGNED
          LABEL "Sem.Emb."
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     orden_entrega.fecha_embarque AT ROW 4.81 COL 128 COLON-ALIGNED
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     orden_entrega.plazo AT ROW 5.76 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     orden_entrega.id_tipo_plazo AT ROW 5.76 COL 36 COLON-ALIGNED FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     fi-tipos_plazo-descripcion AT ROW 5.76 COL 46 COLON-ALIGNED NO-LABEL
     orden_entrega.id_instrumento_pago AT ROW 5.76 COL 96 COLON-ALIGNED
          LABEL "InsPago" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 7.8 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     fi-instrumentos_pagos-descripci AT ROW 5.76 COL 104 COLON-ALIGNED NO-LABEL
     orden_entrega.id_condicion_venta AT ROW 6.71 COL 12 COLON-ALIGNED
          LABEL "Cond. Venta"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-clausulas-descripcion AT ROW 6.71 COL 24 COLON-ALIGNED NO-LABEL
     orden_entrega.id_estado AT ROW 6.71 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-estados_oe-descripcion AT ROW 6.71 COL 80 COLON-ALIGNED NO-LABEL
     orden_entrega.fecha_arribo AT ROW 7.67 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     orden_entrega.id_lugdes AT ROW 7.67 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-lugar_descarga-descripcion AT ROW 7.67 COL 85 COLON-ALIGNED NO-LABEL
     orden_entrega.id_despachante AT ROW 8.62 COL 13 COLON-ALIGNED
          LABEL "Despachante"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     fi-despachantes-descripcion AT ROW 8.62 COL 22 COLON-ALIGNED NO-LABEL
     orden_entrega.id_tipo_contenedor AT ROW 8.62 COL 73 COLON-ALIGNED
          LABEL "Cont."
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-tipo_contenedor-descripcion AT ROW 8.62 COL 80 COLON-ALIGNED NO-LABEL
     orden_entrega.contenedores AT ROW 8.62 COL 131 COLON-ALIGNED
          LABEL "Cantidad"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     orden_entrega.total_factura AT ROW 9.57 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     orden_entrega.total_galones AT ROW 9.57 COL 46 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     orden_entrega.importe_comisiones AT ROW 9.57 COL 72 COLON-ALIGNED
          LABEL "Comisiones" FORMAT "->>,>>9.999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     orden_entrega.fob_ton AT ROW 9.57 COL 99 COLON-ALIGNED
          LABEL "Precio Fob"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     orden_entrega.fob_unitario AT ROW 9.57 COL 131 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.orden_entrega
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
         HEIGHT             = 9.62
         WIDTH              = 143.
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

/* SETTINGS FOR FILL-IN orden_entrega.anio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.contenedores IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.fecha_embarque IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi-agencias-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-calidades-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-clausulas-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-clientes-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-despachantes-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-destinos-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estados_oe-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-instrumentos_pagos-descripci IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lugar_descarga-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipos_plazo-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo_contenedor-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vapores-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.fob_ton IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.id_agencia IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.id_condicion_venta IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.id_despachante IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.id_instrumento_pago IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN orden_entrega.id_orden_entrega IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN orden_entrega.id_tipo_contenedor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.id_tipo_contrato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.id_tipo_plazo IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN orden_entrega.id_vapor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.importe_comisiones IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN orden_entrega.item IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.kgs_brutos_tambores IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN orden_entrega.semana_embarque IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.semana_entrega IN FRAME F-Main
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
general.orden_entrega.id_agencia;wc_agencias.w;agencias.descripcion;;
general.orden_entrega.id_instrumento_pago;wc_instrumentos_pagos.w;instrumentos_pagos.descripcion;;
general.orden_entrega.id_tipo_plazo;wc_tipos_plazo.w;tipos_plazo.descripcion;;
general.orden_entrega.id_despachante;wc_despachantes.w;despachantes.descripcion;;
general.orden_entrega.id_tipo_contenedor;wc_tipo_contenedor.w;tipo_contenedor.descripcion;;
general.orden_entrega.id_lugdes;wc_lugar_descarga.w;lugar_descarga.descripcion;;
general.orden_entrega.id_estado;wc_estados_oe.w;estados_oe.descripcion;;
general.orden_entrega.id_destino;wc_destinos.w;destinos.descripcion;;
general.orden_entrega.id_cliente;wc_clientes.w;clientes.nombre;;
general.orden_entrega.id_calidad;wc_calidades.w;calidades.descripcion;;
general.orden_entrega.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
general.orden_entrega.id_condicion_venta;wc_clausulas.w;clausulas.descripcion;id_clausula;
general.orden_entrega.id_contrato;wc_items_contratos.w;contratos.;;
general.orden_entrega.id_vapor;wc_vapores.w;vapores.descripcion;;
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

&Scoped-define SELF-NAME orden_entrega.anio
&Scoped-define SELF-NAME orden_entrega.cantidad_tambores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.cantidad_tambores V-table-Win 
ON LEAVE OF orden_entrega.cantidad_tambores IN FRAME F-Main /* Tambores */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME orden_entrega.contenedores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.contenedores V-table-Win
ON LEAVE OF orden_entrega.contenedores IN FRAME F-Main /* Cantidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.fecha
&Scoped-define SELF-NAME orden_entrega.fecha_arribo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.fecha_arribo V-table-Win
ON LEAVE OF orden_entrega.fecha_arribo IN FRAME F-Main /* Fecha arribo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.fecha_embarque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.fecha_embarque V-table-Win
ON LEAVE OF orden_entrega.fecha_embarque IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-agencias-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-agencias-descripcion V-table-Win
ON LEAVE OF fi-agencias-descripcion IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-clientes-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-clientes-nombre V-table-Win
ON LEAVE OF fi-clientes-nombre IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-destinos-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-destinos-descripcion V-table-Win
ON LEAVE OF fi-destinos-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-estados_oe-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-estados_oe-descripcion V-table-Win
ON LEAVE OF fi-estados_oe-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-instrumentos_pagos-descripci
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-instrumentos_pagos-descripci V-table-Win
ON LEAVE OF fi-instrumentos_pagos-descripci IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-tipos_plazo-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipos_plazo-descripcion V-table-Win
ON LEAVE OF fi-tipos_plazo-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo_contenedor-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo_contenedor-descripcion V-table-Win
ON LEAVE OF fi-tipo_contenedor-descripcion IN FRAME F-Main
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


&Scoped-define SELF-NAME orden_entrega.fob_ton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.fob_ton V-table-Win
ON LEAVE OF orden_entrega.fob_ton IN FRAME F-Main /* Precio Fob */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.fob_unitario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.fob_unitario V-table-Win
ON LEAVE OF orden_entrega.fob_unitario IN FRAME F-Main /* Fob Unitario */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.grados_brix
&Scoped-define SELF-NAME orden_entrega.id_agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_agencia V-table-Win
ON GO OF orden_entrega.id_agencia IN FRAME F-Main /* Agencia */
DO:

     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_agencia V-table-Win
ON LEAVE OF orden_entrega.id_agencia IN FRAME F-Main /* Agencia */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_agencia V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_agencia IN FRAME F-Main /* Agencia */
do: 
define var r as rowid no-undo.
run wc_agencias.w(output r).
find agencias where rowid(agencias) = r no-lock no-error.
if available agencias then 
general.orden_entrega.id_agencia:screen-value = string(agencias.id_agencia).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_agencia V-table-Win
ON U1 OF orden_entrega.id_agencia IN FRAME F-Main /* Agencia */
DO:

     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_articulo V-table-Win
ON GO OF orden_entrega.id_articulo IN FRAME F-Main /* Articulo */
DO:

     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_articulo V-table-Win
ON LEAVE OF orden_entrega.id_articulo IN FRAME F-Main /* Articulo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_articulo IN FRAME F-Main /* Articulo */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.orden_entrega.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_articulo V-table-Win
ON U1 OF orden_entrega.id_articulo IN FRAME F-Main /* Articulo */
DO:

     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_calidad V-table-Win
ON GO OF orden_entrega.id_calidad IN FRAME F-Main /* Calidad */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_calidad V-table-Win
ON LEAVE OF orden_entrega.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_calidad V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_calidad IN FRAME F-Main /* Calidad */
do: 
define var r as rowid no-undo.
run wc_calidades.w(output r).
find calidades where rowid(calidades) = r no-lock no-error.
if available calidades then 
general.orden_entrega.id_calidad:screen-value = string(calidades.id_calidad).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_calidad V-table-Win
ON U1 OF orden_entrega.id_calidad IN FRAME F-Main /* Calidad */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_cliente V-table-Win
ON GO OF orden_entrega.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_cliente V-table-Win
ON LEAVE OF orden_entrega.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_cliente V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_cliente IN FRAME F-Main /* Cliente */
do: 
define var r as rowid no-undo.
run wc_clientes.w(output r).
find clientes where rowid(clientes) = r no-lock no-error.
if available clientes then 
general.orden_entrega.id_cliente:screen-value = string(clientes.id_cliente).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_cliente V-table-Win
ON U1 OF orden_entrega.id_cliente IN FRAME F-Main /* Cliente */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_condicion_venta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_condicion_venta V-table-Win
ON GO OF orden_entrega.id_condicion_venta IN FRAME F-Main /* Cond. Venta */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_condicion_venta V-table-Win
ON LEAVE OF orden_entrega.id_condicion_venta IN FRAME F-Main /* Cond. Venta */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_condicion_venta V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_condicion_venta IN FRAME F-Main /* Cond. Venta */
do: 
define var r as rowid no-undo.
run wc_clausulas.w(output r).
find clausulas where rowid(clausulas) = r no-lock no-error.
if available clausulas then 
general.orden_entrega.id_condicion_venta:screen-value = string(clausulas.id_clausula).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_condicion_venta V-table-Win
ON U1 OF orden_entrega.id_condicion_venta IN FRAME F-Main /* Cond. Venta */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_contrato V-table-Win
ON LEAVE OF orden_entrega.id_contrato IN FRAME F-Main /* Contract */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_contrato V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_contrato IN FRAME F-Main /* Contract */
do: 
run descriptivos1.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_despachante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_despachante V-table-Win
ON GO OF orden_entrega.id_despachante IN FRAME F-Main /* Despachante */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_despachante V-table-Win
ON LEAVE OF orden_entrega.id_despachante IN FRAME F-Main /* Despachante */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_despachante V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_despachante IN FRAME F-Main /* Despachante */
do: 
define var r as rowid no-undo.
run wc_despachantes.w(output r).
find despachantes where rowid(despachantes) = r no-lock no-error.
if available despachantes then 
general.orden_entrega.id_despachante:screen-value = string(despachantes.id_despachante).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_despachante V-table-Win
ON U1 OF orden_entrega.id_despachante IN FRAME F-Main /* Despachante */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_destino V-table-Win
ON GO OF orden_entrega.id_destino IN FRAME F-Main /* Cod.Destino */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_destino V-table-Win
ON LEAVE OF orden_entrega.id_destino IN FRAME F-Main /* Cod.Destino */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_destino V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_destino IN FRAME F-Main /* Cod.Destino */
do: 
define var r as rowid no-undo.
run wc_destinos.w(output r).
find destinos where rowid(destinos) = r no-lock no-error.
if available destinos then 
general.orden_entrega.id_destino:screen-value = string(destinos.id_destino).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_destino V-table-Win
ON U1 OF orden_entrega.id_destino IN FRAME F-Main /* Cod.Destino */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_estado V-table-Win
ON GO OF orden_entrega.id_estado IN FRAME F-Main /* Estado */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_estado V-table-Win
ON LEAVE OF orden_entrega.id_estado IN FRAME F-Main /* Estado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_estado V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_estado IN FRAME F-Main /* Estado */
do: 
define var r as rowid no-undo.
run wc_estados_oe.w(output r).
find estados_oe where rowid(estados_oe) = r no-lock no-error.
if available estados_oe then 
general.orden_entrega.id_estado:screen-value = string(estados_oe.id_estado).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_estado V-table-Win
ON U1 OF orden_entrega.id_estado IN FRAME F-Main /* Estado */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_instrumento_pago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_instrumento_pago V-table-Win
ON GO OF orden_entrega.id_instrumento_pago IN FRAME F-Main /* InsPago */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_instrumento_pago V-table-Win
ON LEAVE OF orden_entrega.id_instrumento_pago IN FRAME F-Main /* InsPago */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_instrumento_pago V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_instrumento_pago IN FRAME F-Main /* InsPago */
do: 
define var r as rowid no-undo.
run wc_instrumentos_pagos.w(output r).
find instrumentos_pagos where rowid(instrumentos_pagos) = r no-lock no-error.
if available instrumentos_pagos then 
general.orden_entrega.id_instrumento_pago:screen-value = string(instrumentos_pagos.id_instrumento_pago).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_instrumento_pago V-table-Win
ON U1 OF orden_entrega.id_instrumento_pago IN FRAME F-Main /* InsPago */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_lugdes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_lugdes V-table-Win
ON GO OF orden_entrega.id_lugdes IN FRAME F-Main /* Cod. Lug. Descarga */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_lugdes V-table-Win
ON LEAVE OF orden_entrega.id_lugdes IN FRAME F-Main /* Cod. Lug. Descarga */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_lugdes V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_lugdes IN FRAME F-Main /* Cod. Lug. Descarga */
do: 
define var r as rowid no-undo.
run wc_lugar_descarga.w(output r).
find lugar_descarga where rowid(lugar_descarga) = r no-lock no-error.
if available lugar_descarga then 
general.orden_entrega.id_lugdes:screen-value = string(lugar_descarga.id_lugdes).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_lugdes V-table-Win
ON U1 OF orden_entrega.id_lugdes IN FRAME F-Main /* Cod. Lug. Descarga */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_orden_entrega
&Scoped-define SELF-NAME orden_entrega.id_tipo_contenedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_contenedor V-table-Win
ON GO OF orden_entrega.id_tipo_contenedor IN FRAME F-Main /* Cont. */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_contenedor V-table-Win
ON LEAVE OF orden_entrega.id_tipo_contenedor IN FRAME F-Main /* Cont. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_contenedor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_tipo_contenedor IN FRAME F-Main /* Cont. */
do: 
define var r as rowid no-undo.
run wc_tipo_contenedor.w(output r).
find tipo_contenedor where rowid(tipo_contenedor) = r no-lock no-error.
if available tipo_contenedor then 
general.orden_entrega.id_tipo_contenedor:screen-value = string(tipo_contenedor.id_tipo_contenedor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_contenedor V-table-Win
ON U1 OF orden_entrega.id_tipo_contenedor IN FRAME F-Main /* Cont. */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_tipo_contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_contrato V-table-Win 
ON LEAVE OF orden_entrega.id_tipo_contrato IN FRAME F-Main /* Tipo Contrato */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME orden_entrega.id_tipo_plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_plazo V-table-Win
ON GO OF orden_entrega.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_plazo V-table-Win
ON LEAVE OF orden_entrega.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_plazo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
do: 
define var r as rowid no-undo.
run wc_tipos_plazo.w(output r).
find tipos_plazo where rowid(tipos_plazo) = r no-lock no-error.
if available tipos_plazo then 
general.orden_entrega.id_tipo_plazo:screen-value = string(tipos_plazo.id_tipo_plazo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_plazo V-table-Win
ON U1 OF orden_entrega.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_vapor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_vapor V-table-Win
ON GO OF orden_entrega.id_vapor IN FRAME F-Main /* Vapor */
DO:
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_vapor V-table-Win
ON LEAVE OF orden_entrega.id_vapor IN FRAME F-Main /* Vapor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_vapor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_vapor IN FRAME F-Main /* Vapor */
do: 
define var r as rowid no-undo.
run wc_vapores.w(output r).
find vapores where rowid(vapores) = r no-lock no-error.
if available vapores then 
general.orden_entrega.id_vapor:screen-value = string(vapores.id_vapor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_vapor V-table-Win
ON U1 OF orden_entrega.id_vapor IN FRAME F-Main /* Vapor */
DO:

     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.importe_comisiones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.importe_comisiones V-table-Win 
ON LEAVE OF orden_entrega.importe_comisiones IN FRAME F-Main /* Comisiones */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME orden_entrega.item
&Scoped-define SELF-NAME orden_entrega.kgs_brutos_tambores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.kgs_brutos_tambores V-table-Win
ON LEAVE OF orden_entrega.kgs_brutos_tambores IN FRAME F-Main /* Kgs Brutos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.kgs_netos_tambores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.kgs_netos_tambores V-table-Win 
ON LEAVE OF orden_entrega.kgs_netos_tambores IN FRAME F-Main /* Kgs Netos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME orden_entrega.modo_actualizacion
&Scoped-define SELF-NAME orden_entrega.plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.plazo V-table-Win
ON LEAVE OF orden_entrega.plazo IN FRAME F-Main /* Plazo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.semana_embarque
&Scoped-define SELF-NAME orden_entrega.semana_entrega
&Scoped-define SELF-NAME orden_entrega.total_factura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.total_factura V-table-Win
ON LEAVE OF orden_entrega.total_factura IN FRAME F-Main /* Total factura */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.total_galones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.total_galones V-table-Win
ON LEAVE OF orden_entrega.total_galones IN FRAME F-Main /* T. Galones */
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
{i_post_create.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-delete V-table-Win 
PROCEDURE adm-post-delete :
{i_post_delete.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-update V-table-Win 
PROCEDURE adm-post-update :
{i_post_update.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-create V-table-Win 
PROCEDURE adm-pre-create :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-delete V-table-Win 
PROCEDURE adm-pre-delete :
{i_pre_delete.i}
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
  {src/adm/template/row-list.i "orden_entrega"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "orden_entrega"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE agregar V-table-Win 
PROCEDURE agregar :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo-gastos V-table-Win 
PROCEDURE calculo-gastos :
{i_calculo_gastos.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambia-fila V-table-Win 
PROCEDURE cambia-fila :
v_id_clausula = integer(orden_entrega.id_condicion_venta:screen-value in frame F-Main).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-fob V-table-Win 
PROCEDURE carga-fob :
{i_carga_fob.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos V-table-Win 
PROCEDURE descriptivos :
{i_descriptivos.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos1 V-table-Win 
PROCEDURE descriptivos1 :
{i_descriptivos1.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_campos V-table-Win 
PROCEDURE deshabilita_campos :
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
define var lista_relacion as character no-undo initial "id_agencia,id_instrumento_pago,id_tipo_plazo,id_despachante,id_tipo_contenedor,id_lugdes,id_estado,id_destino,id_cliente,id_calidad,id_articulo,id_condicion_venta,id_contrato,id_vapor".
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
  run rebizo-estado-oe.
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
  
run cambia-fila. /* ESTO SE EJECUTA CUANDO CAMBIO DE FILA, LA NUEVA FILA */

  /* Code placed here will execute AFTER standard behavior.    */
  run descriptivos.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-add V-table-Win 
PROCEDURE post-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rebizo-estado-oe V-table-Win 
PROCEDURE rebizo-estado-oe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
for each orden_entrega.
    if general.orden_entrega.fecha_arribo <= today and 
       general.orden_entrega.id_estado < 5 then general.orden_entrega.id_estado = 5.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcular-datos V-table-Win 
PROCEDURE recalcular-datos :
{i_recalcular_datos.i}
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
  {src/adm/template/snd-list.i "orden_entrega"}

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

