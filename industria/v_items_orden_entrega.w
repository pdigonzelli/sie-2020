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

define var v_item as integer.
define var v_anio as integer.
define var v_id_tipo_contrato as integer.
define var v_semana_entrega as integer.
define var v_id_clausula as integer.
define var v_id_orden_entrega as integer.
DEFINE VAR v_item_oe AS INTEGER.
define var v_alta as integer initial 0.
DEFINE VAR v_moneda LIKE items_contratos.id_moneda_origen.
DEFINE VAR v_tipo_unidad LIKE items_contratos.id_tipo_unidad_venta_origen.
DEFINE VAR v_precio LIKE items_contratos.precio_origen.
DEFINE VAR v_tipo_venta LIKE items_contratos.id_tipo_venta.

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
&Scoped-define EXTERNAL-TABLES items_orden_entrega
&Scoped-define FIRST-EXTERNAL-TABLE items_orden_entrega


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR items_orden_entrega.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS items_orden_entrega.item_oe ~
items_orden_entrega.fecha items_orden_entrega.modo_actualizacion ~
items_orden_entrega.cert_fito items_orden_entrega.cerrado ~
items_orden_entrega.id_programa_despacho items_orden_entrega.id_contrato ~
items_orden_entrega.id_envase items_orden_entrega.id_cliente ~
items_orden_entrega.id_articulo items_orden_entrega.id_calidad ~
items_orden_entrega.grados_brix items_orden_entrega.cajas_x_pallets ~
items_orden_entrega.tambores_pedidos items_orden_entrega.cantidad_pallets ~
items_orden_entrega.cantidad_tambores ~
items_orden_entrega.kgs_netos_tambores ~
items_orden_entrega.kgs_brutos_tambores items_orden_entrega.total_cajas ~
items_orden_entrega.precio_x_caja items_orden_entrega.id_tipo_pallet ~
items_orden_entrega.plazo items_orden_entrega.id_tipo_plazo ~
items_orden_entrega.id_instrumento_pago ~
items_orden_entrega.id_condicion_venta items_orden_entrega.id_estado ~
items_orden_entrega.id_tipo_contenedor items_orden_entrega.contenedores ~
items_orden_entrega.id_marca items_orden_entrega.total_factura ~
items_orden_entrega.id_moneda_cambio items_orden_entrega.tipo_cambio ~
items_orden_entrega.importe_factura_dolar items_orden_entrega.total_galones ~
items_orden_entrega.importe_comisiones items_orden_entrega.fob_ton ~
items_orden_entrega.importe_fob_dolar items_orden_entrega.fob_unitario ~
items_orden_entrega.coeficiente items_orden_entrega.valor_aduana_derechos ~
items_orden_entrega.valor_aduana_reintegro ~
items_orden_entrega.importe_derechos_exportacion ~
items_orden_entrega.importe_reintegro_fijo 
&Scoped-define ENABLED-TABLES items_orden_entrega
&Scoped-define FIRST-ENABLED-TABLE items_orden_entrega
&Scoped-Define DISPLAYED-FIELDS items_orden_entrega.item_oe ~
items_orden_entrega.fecha items_orden_entrega.modo_actualizacion ~
items_orden_entrega.cert_fito items_orden_entrega.cerrado ~
items_orden_entrega.id_programa_despacho items_orden_entrega.id_contrato ~
items_orden_entrega.id_envase items_orden_entrega.item ~
items_orden_entrega.anio items_orden_entrega.id_tipo_contrato ~
items_orden_entrega.semana_entrega items_orden_entrega.id_cliente ~
items_orden_entrega.id_articulo items_orden_entrega.id_calidad ~
items_orden_entrega.grados_brix items_orden_entrega.cajas_x_pallets ~
items_orden_entrega.tambores_pedidos items_orden_entrega.cantidad_pallets ~
items_orden_entrega.cantidad_tambores ~
items_orden_entrega.kgs_netos_tambores ~
items_orden_entrega.kgs_brutos_tambores items_orden_entrega.total_cajas ~
items_orden_entrega.precio_x_caja items_orden_entrega.id_tipo_pallet ~
items_orden_entrega.plazo items_orden_entrega.id_tipo_plazo ~
items_orden_entrega.id_instrumento_pago ~
items_orden_entrega.id_condicion_venta items_orden_entrega.id_estado ~
items_orden_entrega.id_tipo_contenedor items_orden_entrega.contenedores ~
items_orden_entrega.id_marca items_orden_entrega.total_factura ~
items_orden_entrega.id_moneda_cambio items_orden_entrega.tipo_cambio ~
items_orden_entrega.importe_factura_dolar items_orden_entrega.total_galones ~
items_orden_entrega.importe_comisiones items_orden_entrega.fob_ton ~
items_orden_entrega.importe_fob_dolar items_orden_entrega.fob_unitario ~
items_orden_entrega.coeficiente items_orden_entrega.valor_aduana_derechos ~
items_orden_entrega.valor_aduana_reintegro ~
items_orden_entrega.importe_derechos_exportacion ~
items_orden_entrega.importe_reintegro_fijo 
&Scoped-define DISPLAYED-TABLES items_orden_entrega
&Scoped-define FIRST-DISPLAYED-TABLE items_orden_entrega
&Scoped-Define DISPLAYED-OBJECTS fi-envases_prod-descripcion ~
fi-clientes-nombre fi-productos_terminados-descrip descripcion_calidad ~
fi-tipo_pallets-descripcion fi-tipos_plazo-descripcion ~
fi-instrumentos_pagos-descripci fi-clausulas-descripcion ~
fi-estados_oe-descripcion fi-tipo_contenedor-descripcion ~
fi-marcas_prod-descripcion fi-tipo_moneda-descripcion 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS items_orden_entrega.item_oe 

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
DEFINE VARIABLE descripcion_calidad AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE fi-clausulas-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-clientes-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-estados_oe-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-instrumentos_pagos-descripci AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-marcas_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipos_plazo-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipo_contenedor-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipo_moneda-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipo_pallets-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     items_orden_entrega.item_oe AT ROW 1 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     items_orden_entrega.fecha AT ROW 1 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     items_orden_entrega.modo_actualizacion AT ROW 1 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     items_orden_entrega.cert_fito AT ROW 1 COL 111 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     items_orden_entrega.cerrado AT ROW 1 COL 144 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     items_orden_entrega.id_programa_despacho AT ROW 1.95 COL 13 COLON-ALIGNED
          LABEL "Pedido" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     items_orden_entrega.id_contrato AT ROW 1.95 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     items_orden_entrega.id_envase AT ROW 1.95 COL 34 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     items_orden_entrega.item AT ROW 1.95 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     fi-envases_prod-descripcion AT ROW 1.95 COL 41 COLON-ALIGNED NO-LABEL
     items_orden_entrega.anio AT ROW 1.95 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     items_orden_entrega.id_tipo_contrato AT ROW 1.95 COL 64 COLON-ALIGNED
          LABEL "TCont"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     items_orden_entrega.semana_entrega AT ROW 1.95 COL 79 COLON-ALIGNED
          LABEL "Semana"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     items_orden_entrega.id_cliente AT ROW 1.95 COL 94 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     fi-clientes-nombre AT ROW 1.95 COL 108 COLON-ALIGNED NO-LABEL
     items_orden_entrega.id_articulo AT ROW 3.14 COL 13 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     fi-productos_terminados-descrip AT ROW 3.14 COL 23 COLON-ALIGNED NO-LABEL
     items_orden_entrega.id_calidad AT ROW 3.14 COL 72 COLON-ALIGNED
          LABEL "Calidad"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     descripcion_calidad AT ROW 3.14 COL 80 COLON-ALIGNED NO-LABEL
     items_orden_entrega.grados_brix AT ROW 3.14 COL 134 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     items_orden_entrega.cajas_x_pallets AT ROW 4.1 COL 13 COLON-ALIGNED
          LABEL "Cajas x pallet"
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     items_orden_entrega.tambores_pedidos AT ROW 4.1 COL 13 COLON-ALIGNED
          LABEL "Tam.Pedidos" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     items_orden_entrega.cantidad_pallets AT ROW 4.1 COL 48 COLON-ALIGNED
          LABEL "Pallets"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     items_orden_entrega.cantidad_tambores AT ROW 4.1 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     items_orden_entrega.kgs_netos_tambores AT ROW 4.1 COL 72 COLON-ALIGNED FORMAT "->>>>>>9.999"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     items_orden_entrega.kgs_brutos_tambores AT ROW 4.1 COL 107 COLON-ALIGNED FORMAT "->>>>>>9.999"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     items_orden_entrega.total_cajas AT ROW 5.05 COL 13 COLON-ALIGNED
          LABEL "Total Cajas"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     items_orden_entrega.precio_x_caja AT ROW 5.05 COL 48 COLON-ALIGNED
          LABEL "Precio x caja"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     items_orden_entrega.id_tipo_pallet AT ROW 5.05 COL 72 COLON-ALIGNED
          LABEL "Tipo Pallet"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     fi-tipo_pallets-descripcion AT ROW 5.05 COL 81 COLON-ALIGNED NO-LABEL
     items_orden_entrega.plazo AT ROW 6.24 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     items_orden_entrega.id_tipo_plazo AT ROW 6.24 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi-tipos_plazo-descripcion AT ROW 6.24 COL 48 COLON-ALIGNED NO-LABEL
     items_orden_entrega.id_instrumento_pago AT ROW 6.24 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi-instrumentos_pagos-descripci AT ROW 6.24 COL 106 COLON-ALIGNED NO-LABEL
     items_orden_entrega.id_condicion_venta AT ROW 7.19 COL 13 COLON-ALIGNED
          LABEL "Cond. Venta"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-clausulas-descripcion AT ROW 7.19 COL 20 COLON-ALIGNED NO-LABEL
     items_orden_entrega.id_estado AT ROW 7.19 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-estados_oe-descripcion AT ROW 7.19 COL 79 COLON-ALIGNED NO-LABEL
     items_orden_entrega.id_tipo_contenedor AT ROW 8.14 COL 13 COLON-ALIGNED
          LABEL "Cont."
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-tipo_contenedor-descripcion AT ROW 8.14 COL 20 COLON-ALIGNED NO-LABEL
     items_orden_entrega.contenedores AT ROW 8.14 COL 72 COLON-ALIGNED
          LABEL "Cantidad"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     items_orden_entrega.id_marca AT ROW 8.14 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     fi-marcas_prod-descripcion AT ROW 8.14 COL 107 COLON-ALIGNED NO-LABEL
     items_orden_entrega.total_factura AT ROW 9.1 COL 13 COLON-ALIGNED FORMAT "->>>>>>>9.999"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     items_orden_entrega.id_moneda_cambio AT ROW 9.1 COL 45 COLON-ALIGNED
          LABEL "Moneda" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     fi-tipo_moneda-descripcion AT ROW 9.1 COL 50 COLON-ALIGNED NO-LABEL
     items_orden_entrega.tipo_cambio AT ROW 9.1 COL 103 COLON-ALIGNED FORMAT "->>>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     items_orden_entrega.importe_factura_dolar AT ROW 9.1 COL 135 COLON-ALIGNED
          LABEL "Total Fac U$S" FORMAT "->>>>>9.999"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     items_orden_entrega.total_galones AT ROW 10.05 COL 13 COLON-ALIGNED FORMAT "->>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     items_orden_entrega.importe_comisiones AT ROW 10.05 COL 45 COLON-ALIGNED
          LABEL "Comisiones" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     items_orden_entrega.fob_ton AT ROW 10.05 COL 72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     items_orden_entrega.importe_fob_dolar AT ROW 10.05 COL 101 COLON-ALIGNED
          LABEL "Fob Dolar" FORMAT "->>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     items_orden_entrega.fob_unitario AT ROW 10.05 COL 131 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     items_orden_entrega.coeficiente AT ROW 11 COL 13 COLON-ALIGNED
          LABEL "Coef."
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     items_orden_entrega.valor_aduana_derechos AT ROW 11 COL 45 COLON-ALIGNED
          LABEL "DerechoAduana"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     items_orden_entrega.valor_aduana_reintegro AT ROW 11 COL 79 COLON-ALIGNED
          LABEL "ReintegroAduana"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     items_orden_entrega.importe_derechos_exportacion AT ROW 11 COL 107 COLON-ALIGNED
          LABEL "Derechos"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     items_orden_entrega.importe_reintegro_fijo AT ROW 11 COL 134 COLON-ALIGNED
          LABEL "Reintegro"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.items_orden_entrega
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
         HEIGHT             = 11.14
         WIDTH              = 150.6.
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

/* SETTINGS FOR FILL-IN items_orden_entrega.anio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.cajas_x_pallets IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.cantidad_pallets IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.coeficiente IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.contenedores IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN descripcion_calidad IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-clausulas-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-clientes-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estados_oe-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-instrumentos_pagos-descripci IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-marcas_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipos_plazo-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo_contenedor-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo_moneda-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo_pallets-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.id_articulo IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_orden_entrega.id_calidad IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.id_condicion_venta IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.id_moneda_cambio IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN items_orden_entrega.id_programa_despacho IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN items_orden_entrega.id_tipo_contenedor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.id_tipo_contrato IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN items_orden_entrega.id_tipo_pallet IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.importe_comisiones IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN items_orden_entrega.importe_derechos_exportacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.importe_factura_dolar IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN items_orden_entrega.importe_fob_dolar IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN items_orden_entrega.importe_reintegro_fijo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.item IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.item_oe IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN items_orden_entrega.kgs_brutos_tambores IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_orden_entrega.kgs_netos_tambores IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_orden_entrega.precio_x_caja IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.semana_entrega IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN items_orden_entrega.tambores_pedidos IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN items_orden_entrega.tipo_cambio IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_orden_entrega.total_cajas IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.total_factura IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_orden_entrega.total_galones IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN items_orden_entrega.valor_aduana_derechos IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN items_orden_entrega.valor_aduana_reintegro IN FRAME F-Main
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
general.items_orden_entrega.id_cliente;wc_clientes.w;clientes.nombre;;
general.items_orden_entrega.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
general.items_orden_entrega.id_tipo_pallet;wc_tipo_pallets.w;tipo_pallets.descripcion;;
general.items_orden_entrega.id_envase;wc_envases_ff.w;envases_prod.descripcion;;
general.items_orden_entrega.id_moneda_cambio;wc_monedas.w;tipo_moneda.descripcion;id_moneda;
general.items_orden_entrega.id_marca;wc_marcas.w;marcas_prod.descripcion;;
general.items_orden_entrega.id_condicion_venta;wc_clausulas.w;clausulas.descripcion;id_clausula;
general.items_orden_entrega.id_tipo_plazo;wc_tipos_plazo.w;tipos_plazo.descripcion;;
general.items_orden_entrega.id_instrumento_pago;wc_instrumentos_pagos.w;instrumentos_pagos.descripcion;;
general.items_orden_entrega.id_estado;wc_estados_oe.w;estados_oe.descripcion;;
general.items_orden_entrega.id_tipo_contenedor;wc_tipo_contenedor.w;tipo_contenedor.descripcion;;
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

&Scoped-define SELF-NAME items_orden_entrega.anio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.anio V-table-Win
ON LEAVE OF items_orden_entrega.anio IN FRAME F-Main /* Año */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.cajas_x_pallets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.cajas_x_pallets V-table-Win
ON LEAVE OF items_orden_entrega.cajas_x_pallets IN FRAME F-Main /* Cajas x pallet */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.cantidad_pallets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.cantidad_pallets V-table-Win
ON LEAVE OF items_orden_entrega.cantidad_pallets IN FRAME F-Main /* Pallets */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.cantidad_tambores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.cantidad_tambores V-table-Win
ON LEAVE OF items_orden_entrega.cantidad_tambores IN FRAME F-Main /* Tambores */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.cerrado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.cerrado V-table-Win
ON LEAVE OF items_orden_entrega.cerrado IN FRAME F-Main /* Cerrado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.cert_fito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.cert_fito V-table-Win
ON LEAVE OF items_orden_entrega.cert_fito IN FRAME F-Main /* Cert.Fitosanitario */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.coeficiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.coeficiente V-table-Win
ON LEAVE OF items_orden_entrega.coeficiente IN FRAME F-Main /* Coef. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.contenedores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.contenedores V-table-Win
ON LEAVE OF items_orden_entrega.contenedores IN FRAME F-Main /* Cantidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME descripcion_calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL descripcion_calidad V-table-Win
ON LEAVE OF descripcion_calidad IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.fecha V-table-Win
ON LEAVE OF items_orden_entrega.fecha IN FRAME F-Main /* Fecha */
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


&Scoped-define SELF-NAME fi-envases_prod-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-envases_prod-descripcion V-table-Win
ON LEAVE OF fi-envases_prod-descripcion IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-marcas_prod-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-marcas_prod-descripcion V-table-Win
ON LEAVE OF fi-marcas_prod-descripcion IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-tipo_moneda-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo_moneda-descripcion V-table-Win
ON LEAVE OF fi-tipo_moneda-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo_pallets-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo_pallets-descripcion V-table-Win
ON LEAVE OF fi-tipo_pallets-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.fob_ton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.fob_ton V-table-Win
ON LEAVE OF items_orden_entrega.fob_ton IN FRAME F-Main /* Fob/Ton */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.fob_unitario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.fob_unitario V-table-Win
ON LEAVE OF items_orden_entrega.fob_unitario IN FRAME F-Main /* Fob Unitario */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.grados_brix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.grados_brix V-table-Win
ON LEAVE OF items_orden_entrega.grados_brix IN FRAME F-Main /* Grados Brix */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_articulo V-table-Win
ON GO OF items_orden_entrega.id_articulo IN FRAME F-Main /* Articulo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_articulo V-table-Win
ON LEAVE OF items_orden_entrega.id_articulo IN FRAME F-Main /* Articulo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_articulo IN FRAME F-Main /* Articulo */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.items_orden_entrega.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_articulo V-table-Win
ON U1 OF items_orden_entrega.id_articulo IN FRAME F-Main /* Articulo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_calidad V-table-Win
ON GO OF items_orden_entrega.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
     run descriptivos-calidad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_calidad V-table-Win
ON LEAVE OF items_orden_entrega.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_calidad V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_calidad IN FRAME F-Main /* Calidad */
do: 
DEFINE VAR h_con AS HANDLE.
DEFINE VAR v_r_oe AS ROWID.
define var r as rowid no-undo.

RUN get-container (OUTPUT h_con).
RUN get-rowid-oe IN h_con (OUTPUT v_r_oe).

FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = v_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    CASE orden_entrega.id_tipo_orden_entrega:
        WHEN 1 THEN DO: /* INDUSTRIA */
            run wc_calidades.w(output r).
            find calidades where rowid(calidades) = r no-lock no-error.
            if available calidades then 
            general.items_orden_entrega.id_calidad:screen-value = string(calidades.id_calidad).
        END.
        WHEN 2 THEN DO: /* FRUTA FRESCA */
            run wc_variedades.w(output r).
            find variedades where rowid(variedades) = r no-lock no-error.
            if available variedades then 
            general.items_orden_entrega.id_calidad:screen-value = string(variedades.id_variedad).
        END.
    END CASE.
END.         

apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_calidad V-table-Win
ON U1 OF items_orden_entrega.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
     run descriptivos-calidad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_cliente V-table-Win
ON GO OF items_orden_entrega.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_cliente V-table-Win
ON LEAVE OF items_orden_entrega.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_cliente V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_cliente IN FRAME F-Main /* Cliente */
do: 
define var r as rowid no-undo.
run wc_clientes.w(output r).
find clientes where rowid(clientes) = r no-lock no-error.
if available clientes then 
general.items_orden_entrega.id_cliente:screen-value = string(clientes.id_cliente).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_cliente V-table-Win
ON U1 OF items_orden_entrega.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_condicion_venta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_condicion_venta V-table-Win
ON GO OF items_orden_entrega.id_condicion_venta IN FRAME F-Main /* Cond. Venta */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_condicion_venta V-table-Win
ON LEAVE OF items_orden_entrega.id_condicion_venta IN FRAME F-Main /* Cond. Venta */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_condicion_venta V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_condicion_venta IN FRAME F-Main /* Cond. Venta */
do: 
define var r as rowid no-undo.
run wc_clausulas.w(output r).
find clausulas where rowid(clausulas) = r no-lock no-error.
if available clausulas then 
general.items_orden_entrega.id_condicion_venta:screen-value = string(clausulas.id_clausula).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_condicion_venta V-table-Win
ON U1 OF items_orden_entrega.id_condicion_venta IN FRAME F-Main /* Cond. Venta */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_contrato V-table-Win
ON LEAVE OF items_orden_entrega.id_contrato IN FRAME F-Main /* Contract */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_contrato V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_contrato IN FRAME F-Main /* Contract */
DO:
  RUN descriptivos1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_envase V-table-Win
ON GO OF items_orden_entrega.id_envase IN FRAME F-Main /* CodEnvase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_envase V-table-Win
ON LEAVE OF items_orden_entrega.id_envase IN FRAME F-Main /* CodEnvase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_envase V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_envase IN FRAME F-Main /* CodEnvase */
do: 
define var r as rowid no-undo.
run wc_envases_ff.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
general.items_orden_entrega.id_envase:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_envase V-table-Win
ON U1 OF items_orden_entrega.id_envase IN FRAME F-Main /* CodEnvase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_estado V-table-Win
ON GO OF items_orden_entrega.id_estado IN FRAME F-Main /* Estado */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_estado V-table-Win
ON LEAVE OF items_orden_entrega.id_estado IN FRAME F-Main /* Estado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_estado V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_estado IN FRAME F-Main /* Estado */
do: 
define var r as rowid no-undo.
run wc_estados_oe.w(output r).
find estados_oe where rowid(estados_oe) = r no-lock no-error.
if available estados_oe then 
general.items_orden_entrega.id_estado:screen-value = string(estados_oe.id_estado).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_estado V-table-Win
ON U1 OF items_orden_entrega.id_estado IN FRAME F-Main /* Estado */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_instrumento_pago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_instrumento_pago V-table-Win
ON GO OF items_orden_entrega.id_instrumento_pago IN FRAME F-Main /* Cod. Ins. Pago */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_instrumento_pago V-table-Win
ON LEAVE OF items_orden_entrega.id_instrumento_pago IN FRAME F-Main /* Cod. Ins. Pago */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_instrumento_pago V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_instrumento_pago IN FRAME F-Main /* Cod. Ins. Pago */
do: 
define var r as rowid no-undo.
run wc_instrumentos_pagos.w(output r).
find instrumentos_pagos where rowid(instrumentos_pagos) = r no-lock no-error.
if available instrumentos_pagos then 
general.items_orden_entrega.id_instrumento_pago:screen-value = string(instrumentos_pagos.id_instrumento_pago).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_instrumento_pago V-table-Win
ON U1 OF items_orden_entrega.id_instrumento_pago IN FRAME F-Main /* Cod. Ins. Pago */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_marca V-table-Win
ON GO OF items_orden_entrega.id_marca IN FRAME F-Main /* Cod.Marca */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_marca V-table-Win
ON LEAVE OF items_orden_entrega.id_marca IN FRAME F-Main /* Cod.Marca */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_marca V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_marca IN FRAME F-Main /* Cod.Marca */
do: 
define var r as rowid no-undo.
run wc_marcas.w(output r).
find marcas_prod where rowid(marcas_prod) = r no-lock no-error.
if available marcas_prod then 
general.items_orden_entrega.id_marca:screen-value = string(marcas_prod.id_marca).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_marca V-table-Win
ON U1 OF items_orden_entrega.id_marca IN FRAME F-Main /* Cod.Marca */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_moneda_cambio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_moneda_cambio V-table-Win
ON GO OF items_orden_entrega.id_moneda_cambio IN FRAME F-Main /* Moneda */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_moneda_cambio V-table-Win
ON LEAVE OF items_orden_entrega.id_moneda_cambio IN FRAME F-Main /* Moneda */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_moneda_cambio V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_moneda_cambio IN FRAME F-Main /* Moneda */
do: 
define var r as rowid no-undo.
run wc_monedas.w(output r).
find tipo_moneda where rowid(tipo_moneda) = r no-lock no-error.
if available tipo_moneda then 
general.items_orden_entrega.id_moneda_cambio:screen-value = string(tipo_moneda.id_moneda).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_moneda_cambio V-table-Win
ON U1 OF items_orden_entrega.id_moneda_cambio IN FRAME F-Main /* Moneda */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_programa_despacho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_programa_despacho V-table-Win
ON LEAVE OF items_orden_entrega.id_programa_despacho IN FRAME F-Main /* Pedido */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_programa_despacho V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_programa_despacho IN FRAME F-Main /* Pedido */
DO:
  RUN descriptivos1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_tipo_contenedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_contenedor V-table-Win
ON GO OF items_orden_entrega.id_tipo_contenedor IN FRAME F-Main /* Cont. */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_contenedor V-table-Win
ON LEAVE OF items_orden_entrega.id_tipo_contenedor IN FRAME F-Main /* Cont. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_contenedor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_tipo_contenedor IN FRAME F-Main /* Cont. */
do: 
define var r as rowid no-undo.
run wc_tipo_contenedor.w(output r).
find tipo_contenedor where rowid(tipo_contenedor) = r no-lock no-error.
if available tipo_contenedor then 
general.items_orden_entrega.id_tipo_contenedor:screen-value = string(tipo_contenedor.id_tipo_contenedor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_contenedor V-table-Win
ON U1 OF items_orden_entrega.id_tipo_contenedor IN FRAME F-Main /* Cont. */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_tipo_contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_contrato V-table-Win
ON LEAVE OF items_orden_entrega.id_tipo_contrato IN FRAME F-Main /* TCont */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_tipo_pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_pallet V-table-Win
ON GO OF items_orden_entrega.id_tipo_pallet IN FRAME F-Main /* Tipo Pallet */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_pallet V-table-Win
ON LEAVE OF items_orden_entrega.id_tipo_pallet IN FRAME F-Main /* Tipo Pallet */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_pallet V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_tipo_pallet IN FRAME F-Main /* Tipo Pallet */
do: 
define var r as rowid no-undo.
run wc_tipo_pallets.w(output r).
find tipo_pallets where rowid(tipo_pallets) = r no-lock no-error.
if available tipo_pallets then 
general.items_orden_entrega.id_tipo_pallet:screen-value = string(tipo_pallets.id_tipo_pallet).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_pallet V-table-Win
ON U1 OF items_orden_entrega.id_tipo_pallet IN FRAME F-Main /* Tipo Pallet */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.id_tipo_plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_plazo V-table-Win
ON GO OF items_orden_entrega.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_plazo V-table-Win
ON LEAVE OF items_orden_entrega.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_plazo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_orden_entrega.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
do: 
define var r as rowid no-undo.
run wc_tipos_plazo.w(output r).
find tipos_plazo where rowid(tipos_plazo) = r no-lock no-error.
if available tipos_plazo then 
general.items_orden_entrega.id_tipo_plazo:screen-value = string(tipos_plazo.id_tipo_plazo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.id_tipo_plazo V-table-Win
ON U1 OF items_orden_entrega.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.importe_comisiones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.importe_comisiones V-table-Win
ON LEAVE OF items_orden_entrega.importe_comisiones IN FRAME F-Main /* Comisiones */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.importe_derechos_exportacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.importe_derechos_exportacion V-table-Win
ON LEAVE OF items_orden_entrega.importe_derechos_exportacion IN FRAME F-Main /* Derechos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.importe_factura_dolar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.importe_factura_dolar V-table-Win
ON LEAVE OF items_orden_entrega.importe_factura_dolar IN FRAME F-Main /* Total Fac U$S */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.importe_fob_dolar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.importe_fob_dolar V-table-Win
ON LEAVE OF items_orden_entrega.importe_fob_dolar IN FRAME F-Main /* Fob Dolar */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.importe_reintegro_fijo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.importe_reintegro_fijo V-table-Win
ON LEAVE OF items_orden_entrega.importe_reintegro_fijo IN FRAME F-Main /* Reintegro */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.item V-table-Win
ON LEAVE OF items_orden_entrega.item IN FRAME F-Main /* Parte */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.item_oe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.item_oe V-table-Win
ON LEAVE OF items_orden_entrega.item_oe IN FRAME F-Main /* Parte OE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.kgs_brutos_tambores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.kgs_brutos_tambores V-table-Win
ON LEAVE OF items_orden_entrega.kgs_brutos_tambores IN FRAME F-Main /* Kgs Brutos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.kgs_netos_tambores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.kgs_netos_tambores V-table-Win
ON LEAVE OF items_orden_entrega.kgs_netos_tambores IN FRAME F-Main /* Kgs Netos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.modo_actualizacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.modo_actualizacion V-table-Win
ON LEAVE OF items_orden_entrega.modo_actualizacion IN FRAME F-Main /* ModoActual. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.plazo V-table-Win
ON LEAVE OF items_orden_entrega.plazo IN FRAME F-Main /* Plazo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.precio_x_caja
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.precio_x_caja V-table-Win
ON LEAVE OF items_orden_entrega.precio_x_caja IN FRAME F-Main /* Precio x caja */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.semana_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.semana_entrega V-table-Win
ON LEAVE OF items_orden_entrega.semana_entrega IN FRAME F-Main /* Semana */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.tambores_pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.tambores_pedidos V-table-Win
ON LEAVE OF items_orden_entrega.tambores_pedidos IN FRAME F-Main /* Tam.Pedidos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.tipo_cambio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.tipo_cambio V-table-Win
ON LEAVE OF items_orden_entrega.tipo_cambio IN FRAME F-Main /* Tipo cambio */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.total_cajas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.total_cajas V-table-Win
ON LEAVE OF items_orden_entrega.total_cajas IN FRAME F-Main /* Total Cajas */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.total_factura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.total_factura V-table-Win
ON LEAVE OF items_orden_entrega.total_factura IN FRAME F-Main /* Total factura */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.total_galones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.total_galones V-table-Win
ON LEAVE OF items_orden_entrega.total_galones IN FRAME F-Main /* T. Galones */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.valor_aduana_derechos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.valor_aduana_derechos V-table-Win
ON LEAVE OF items_orden_entrega.valor_aduana_derechos IN FRAME F-Main /* DerechoAduana */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_orden_entrega.valor_aduana_reintegro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_orden_entrega.valor_aduana_reintegro V-table-Win
ON LEAVE OF items_orden_entrega.valor_aduana_reintegro IN FRAME F-Main /* ReintegroAduana */
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
{i_post_create_item_oe.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-delete V-table-Win 
PROCEDURE adm-post-delete :
{i_post_delete_item_oe.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-update V-table-Win 
PROCEDURE adm-post-update :
{i_post_update_item_oe.i}

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
{i_pre_delete_item_oe.i}
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
  {src/adm/template/row-list.i "items_orden_entrega"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "items_orden_entrega"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo-gastos V-table-Win 
PROCEDURE calculo-gastos :
{i_calculo_gastos_item_oe.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambia-fila V-table-Win 
PROCEDURE cambia-fila :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
v_id_clausula = INTEGER(items_orden_entrega.id_condicion_venta:SCREEN-VALUE IN FRAME F-Main).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE campos-industria-ff V-table-Win 
PROCEDURE campos-industria-ff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{i_campos_industria_ff.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-fob V-table-Win 
PROCEDURE carga-fob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{i_carga_fob_item_oe.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos V-table-Win 
PROCEDURE descriptivos :
find first clientes where clientes.id_cliente = integer(items_orden_entrega.id_cliente:screen-value in frame F-Main)  no-lock no-error .
if available clientes then 
fi-clientes-nombre:screen-value in frame F-Main = string(clientes.nombre).
else
fi-clientes-nombre:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(items_orden_entrega.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first tipo_pallets where tipo_pallets.id_tipo_pallet = integer(items_orden_entrega.id_tipo_pallet:screen-value in frame F-Main)  no-lock no-error .
if available tipo_pallets then 
fi-tipo_pallets-descripcion:screen-value in frame F-Main = string(tipo_pallets.descripcion).
else
fi-tipo_pallets-descripcion:screen-value in frame F-Main = ''.

find first envases_prod where envases_prod.id_envase = integer(items_orden_entrega.id_envase:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion:screen-value in frame F-Main = ''.

find first tipo_moneda where tipo_moneda.id_moneda = integer(items_orden_entrega.id_moneda_cambio:screen-value in frame F-Main)  no-lock no-error .
if available tipo_moneda then 
fi-tipo_moneda-descripcion:screen-value in frame F-Main = string(tipo_moneda.descripcion).
else
fi-tipo_moneda-descripcion:screen-value in frame F-Main = ''.

find first marcas_prod where marcas_prod.id_marca = integer(items_orden_entrega.id_marca:screen-value in frame F-Main)  no-lock no-error .
if available marcas_prod then 
fi-marcas_prod-descripcion:screen-value in frame F-Main = string(marcas_prod.descripcion).
else
fi-marcas_prod-descripcion:screen-value in frame F-Main = ''.

find first clausulas where clausulas.id_clausula = integer(items_orden_entrega.id_condicion_venta:screen-value in frame F-Main)  no-lock no-error .
if available clausulas then 
fi-clausulas-descripcion:screen-value in frame F-Main = string(clausulas.descripcion).
else
fi-clausulas-descripcion:screen-value in frame F-Main = ''.

find first tipos_plazo where tipos_plazo.id_tipo_plazo = integer(items_orden_entrega.id_tipo_plazo:screen-value in frame F-Main)  no-lock no-error .
if available tipos_plazo then 
fi-tipos_plazo-descripcion:screen-value in frame F-Main = string(tipos_plazo.descripcion).
else
fi-tipos_plazo-descripcion:screen-value in frame F-Main = ''.

find first instrumentos_pagos where instrumentos_pagos.id_instrumento_pago = integer(items_orden_entrega.id_instrumento_pago:screen-value in frame F-Main)  no-lock no-error .
if available instrumentos_pagos then 
fi-instrumentos_pagos-descripci:screen-value in frame F-Main = string(instrumentos_pagos.descripcion).
else
fi-instrumentos_pagos-descripci:screen-value in frame F-Main = ''.

find first estados_oe where estados_oe.id_estado = integer(items_orden_entrega.id_estado:screen-value in frame F-Main)  no-lock no-error .
if available estados_oe then 
fi-estados_oe-descripcion:screen-value in frame F-Main = string(estados_oe.descripcion).
else
fi-estados_oe-descripcion:screen-value in frame F-Main = ''.

find first tipo_contenedor where tipo_contenedor.id_tipo_contenedor = integer(items_orden_entrega.id_tipo_contenedor:screen-value in frame F-Main)  no-lock no-error .
if available tipo_contenedor then 
fi-tipo_contenedor-descripcion:screen-value in frame F-Main = string(tipo_contenedor.descripcion).
else
fi-tipo_contenedor-descripcion:screen-value in frame F-Main = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos-calidad V-table-Win 
PROCEDURE descriptivos-calidad :
DEFINE VAR h_con AS HANDLE.
DEFINE VAR v_r_oe AS ROWID.
define var r as rowid no-undo.

RUN get-container (OUTPUT h_con).
RUN get-rowid-oe IN h_con (OUTPUT v_r_oe).

FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = v_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    CASE orden_entrega.id_tipo_orden_entrega:
        WHEN 1 THEN DO: /* INDUSTRIA */
            find calidades where calidades.id_calidad = 
                                INTEGER(general.items_orden_entrega.id_calidad:SCREEN-VALUE IN FRAME F-Main)
                           no-lock no-error.
            if available calidades then 
            descripcion_calidad:screen-value = calidades.descripcion.
        END.
        WHEN 2 THEN DO: /* FRUTA FRESCA */
            find variedades where variedades.id_variedad = 
                                INTEGER(general.items_orden_entrega.id_calidad:screen-value IN FRAME F-Main)
                           no-lock no-error.
            if available variedades then 
            descripcion_calidad:screen-value = variedades.descripcion.
        END.
    END CASE.
END.
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
{i_descriptivos1_item_oe.i}
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
define var lista_relacion as character no-undo initial "id_cliente,id_articulo,id_tipo_pallet,id_envase,id_moneda_cambio,id_marca,id_condicion_venta,id_tipo_plazo,id_instrumento_pago,id_estado,id_tipo_contenedor".
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
  RUN rebizo-estado-oe.
  RUN campos-industria-ff.
  run descriptivos-calidad.

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
  RUN cambia-fila.
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
/*
for each items_orden_entrega.
    if general.items_orden_entrega.fecha_arribo <= today and 
       general.items_orden_entrega.id_estado < 5 then general.items_orden_entrega.id_estado = 5.
end.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcular-datos V-table-Win 
PROCEDURE recalcular-datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE viKilos           AS INTEGER    NO-UNDO.
DEFINE VARIABLE viKilosBrutos     AS INTEGER    NO-UNDO.
DEFINE VARIABLE vdPrecioTotal     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdFobDolar        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdCoef            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdDerechosAduana  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdReintegroAduana AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdTotalDerechos   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdTotalReintegros AS DECIMAL    NO-UNDO.

/*interrupcion para debugger*/
/*
DEBUGGER:INITIATE().
DEBUGGER:SET-BREAK().
*/

IF integer(items_orden_entrega.id_articulo:SCREEN-VALUE IN FRAME F-Main) <> 54 THEN DO:
  {i_recalcular_datos_item_oe.i}
END.
ELSE DO:  
  RUN i_recalcular_datos_item_oe_cascara.i (INPUT items_orden_entrega.id_orden_entrega, 
                                            INPUT items_orden_entrega.ITEM_oe, 
                                            OUTPUT viKilos, 
                                            OUTPUT viKilosBrutos,   
                                            OUTPUT vdPrecioTotal, 
                                            OUTPUT vdFobDolar, 
                                            OUTPUT vdCoef, 
                                            OUTPUT vdDerechosAduana, 
                                            OUTPUT vdReintegroAduana, 
                                            OUTPUT vdTotalDerechos,
                                            OUTPUT vdTotalReintegros).
  
  items_orden_entrega.TOTAL_factura:SCREEN-VALUE IN FRAME F-Main                = STRING(vdPrecioTotal).
  items_orden_entrega.importe_fob_dolar:SCREEN-VALUE IN FRAME F-Main            = STRING(vdFobDolar).
  items_orden_entrega.fob_unitario:SCREEN-VALUE IN FRAME F-Main                 = STRING(540).
  items_orden_entrega.coeficiente:SCREEN-VALUE IN FRAME F-Main                  = STRING(vdCoef).
  items_orden_entrega.valor_aduana_derechos:SCREEN-VALUE IN FRAME F-Main        = STRING(vdDerechosAduana).
  items_orden_entrega.valor_aduana_reintegro:SCREEN-VALUE IN FRAME F-Main       = STRING(vdReintegroAduana).
  items_orden_entrega.importe_derechos_exportacion:SCREEN-VALUE IN FRAME F-Main = STRING(vdTotalDerechos).
  items_orden_entrega.importe_reintegro_fijo:SCREEN-VALUE IN FRAME F-Main       = STRING(vdTotalReintegros).
  items_orden_entrega.kgs_netos_tambores:SCREEN-VALUE IN FRAME F-Main           = STRING(viKilos).
  items_orden_entrega.kgs_brutos_tambores:SCREEN-VALUE IN FRAME F-Main          = STRING(viKilosBrutos).
END.
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
  {src/adm/template/snd-list.i "items_orden_entrega"}

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

