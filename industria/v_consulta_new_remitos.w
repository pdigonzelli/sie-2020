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
&Scoped-define EXTERNAL-TABLES remitos
&Scoped-define FIRST-EXTERNAL-TABLE remitos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR remitos.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS remitos.id_sucursal remitos.id_tipo_movsto ~
remitos.nro remitos.fecha remitos.id_punto_venta remitos.nro_comprobante ~
remitos.tipo_remito remitos.mercado remitos.id_orden_entrega ~
remitos.item_oe remitos.nro_orden_fab remitos.id_destino remitos.id_lugdes ~
remitos.id_vapor remitos.id_cliente remitos.id_proveedor ~
remitos.nro_ord_carga remitos.chofer remitos.pat_chasis remitos.Pat_acopla ~
remitos.Peso_bruto remitos.Peso_neto remitos.nro_precinto remitos.bultos ~
remitos.Valor_declarado remitos.id_ciaseg remitos.anio_produccion ~
remitos.nro_per_embarque remitos.nro_contenedor remitos.fecha_proceso ~
remitos.nro_iascav remitos.orden_fabricacion remitos.c_usuario ~
remitos.anio_of remitos.c_fecha remitos.c_hora 
&Scoped-define ENABLED-TABLES remitos
&Scoped-define FIRST-ENABLED-TABLE remitos
&Scoped-Define ENABLED-OBJECTS EDITOR-1 
&Scoped-Define DISPLAYED-FIELDS remitos.id_sucursal remitos.id_tipo_movsto ~
remitos.nro remitos.fecha remitos.id_punto_venta remitos.nro_comprobante ~
remitos.tipo_remito remitos.mercado remitos.id_orden_entrega ~
remitos.item_oe remitos.nro_orden_fab remitos.id_destino remitos.id_lugdes ~
remitos.id_vapor remitos.id_cliente remitos.id_proveedor ~
remitos.nro_ord_carga remitos.chofer remitos.pat_chasis remitos.Pat_acopla ~
remitos.Peso_bruto remitos.Peso_neto remitos.nro_precinto remitos.bultos ~
remitos.Valor_declarado remitos.id_ciaseg remitos.anio_produccion ~
remitos.nro_per_embarque remitos.nro_contenedor remitos.fecha_proceso ~
remitos.nro_iascav remitos.orden_fabricacion remitos.c_usuario ~
remitos.anio_of remitos.c_fecha remitos.c_hora 
&Scoped-define DISPLAYED-TABLES remitos
&Scoped-define FIRST-DISPLAYED-TABLE remitos
&Scoped-Define DISPLAYED-OBJECTS fi-destinos-descripcion ~
fi-lugar_descarga-descripcion fi-clientes-razon_social ~
fi-vapores-descripcion fi-proveedores-nombre fi-cia_seguro-descripcion ~
EDITOR-1 

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
DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 63 BY 2.86 NO-UNDO.

DEFINE VARIABLE fi-cia_seguro-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-clientes-razon_social AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-destinos-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-lugar_descarga-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-proveedores-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-vapores-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     remitos.id_sucursal AT ROW 1 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     remitos.id_tipo_movsto AT ROW 1 COL 39 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     remitos.nro AT ROW 1 COL 55 COLON-ALIGNED FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     remitos.fecha AT ROW 1 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     remitos.id_punto_venta AT ROW 1 COL 106 COLON-ALIGNED
          LABEL "PtoVenta"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     remitos.nro_comprobante AT ROW 1 COL 125 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     remitos.tipo_remito AT ROW 1.95 COL 21 COLON-ALIGNED
          LABEL "Tipo Remito"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     remitos.mercado AT ROW 1.95 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     remitos.id_orden_entrega AT ROW 1.95 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     remitos.item_oe AT ROW 1.95 COL 108 COLON-ALIGNED
          LABEL "Item OE" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     remitos.nro_orden_fab AT ROW 1.95 COL 125 COLON-ALIGNED
          LABEL "OF" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     remitos.id_destino AT ROW 2.91 COL 21 COLON-ALIGNED
          LABEL "Destino" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-destinos-descripcion AT ROW 2.91 COL 28 COLON-ALIGNED NO-LABEL
     remitos.id_lugdes AT ROW 2.91 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-lugar_descarga-descripcion AT ROW 2.91 COL 95 COLON-ALIGNED NO-LABEL
     fi-clientes-razon_social AT ROW 3.86 COL 33 COLON-ALIGNED NO-LABEL
     remitos.id_vapor AT ROW 3.86 COL 88 COLON-ALIGNED
          LABEL "Vapor" FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     fi-vapores-descripcion AT ROW 3.86 COL 98 COLON-ALIGNED NO-LABEL
     remitos.id_cliente AT ROW 3.91 COL 21 COLON-ALIGNED
          LABEL "Cliente" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     remitos.id_proveedor AT ROW 4.81 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-proveedores-nombre AT ROW 4.81 COL 33 COLON-ALIGNED NO-LABEL
     remitos.nro_ord_carga AT ROW 4.81 COL 88 COLON-ALIGNED
          LABEL "N� Orden Carga" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     remitos.chofer AT ROW 5.76 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     remitos.pat_chasis AT ROW 5.76 COL 88 COLON-ALIGNED
          LABEL "Chasis/Acoplado"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     remitos.Pat_acopla AT ROW 5.76 COL 106 COLON-ALIGNED NO-LABEL FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     remitos.Peso_bruto AT ROW 6.71 COL 21 COLON-ALIGNED
          LABEL "Peso Bruto"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     remitos.Peso_neto AT ROW 6.71 COL 52 COLON-ALIGNED
          LABEL "Peso Neto" FORMAT "->>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     remitos.nro_precinto AT ROW 6.71 COL 88 COLON-ALIGNED
          LABEL "Nro Precinto" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     remitos.bultos AT ROW 6.71 COL 134 COLON-ALIGNED FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     remitos.Valor_declarado AT ROW 7.67 COL 21 COLON-ALIGNED FORMAT "->>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     remitos.id_ciaseg AT ROW 7.67 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-cia_seguro-descripcion AT ROW 7.67 COL 59 COLON-ALIGNED NO-LABEL
     remitos.anio_produccion AT ROW 8.62 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     remitos.nro_per_embarque AT ROW 8.62 COL 52 COLON-ALIGNED
          LABEL "Nro Perm Embarque" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     remitos.nro_contenedor AT ROW 8.62 COL 87 COLON-ALIGNED
          LABEL "Nro Contenedor"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     remitos.fecha_proceso AT ROW 8.86 COL 135 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     remitos.nro_iascav AT ROW 9.57 COL 21 COLON-ALIGNED
          LABEL "IASCAV" FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 63 BY 1
     remitos.orden_fabricacion AT ROW 10.05 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     EDITOR-1 AT ROW 10.52 COL 23 NO-LABEL
     remitos.c_usuario AT ROW 10.52 COL 135 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     remitos.anio_of AT ROW 11 COL 99 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     remitos.c_fecha AT ROW 11.48 COL 135 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     remitos.c_hora AT ROW 12.43 COL 135 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     "Observaciones:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 9.81 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.remitos
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
         HEIGHT             = 12.43
         WIDTH              = 150.2.
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

/* SETTINGS FOR FILL-IN remitos.bultos IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN fi-cia_seguro-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-clientes-razon_social IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-destinos-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lugar_descarga-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-proveedores-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vapores-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN remitos.id_cliente IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.id_destino IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.id_punto_venta IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN remitos.id_vapor IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.item_oe IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.nro IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN remitos.nro_contenedor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN remitos.nro_iascav IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.nro_orden_fab IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.nro_ord_carga IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.nro_per_embarque IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.nro_precinto IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.Pat_acopla IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.pat_chasis IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN remitos.Peso_bruto IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN remitos.Peso_neto IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN remitos.tipo_remito IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN remitos.Valor_declarado IN FRAME F-Main
   EXP-FORMAT                                                           */
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
general.remitos.id_destino;wc_destinos.w;destinos.descripcion;;
general.remitos.id_cliente;wc_clientes.w;clientes.razon_social;;
general.remitos.id_lugdes;wc_lugar_descarga.w;lugar_descarga.descripcion;;
general.remitos.id_proveedor;wc_proveedores.w;proveedores.nombre;;
general.remitos.id_ciaseg;wc_ciaseg.w;cia_seguro.descripcion;;
general.remitos.id_vapor;wc_vapores.w;vapores.descripcion;;
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

&Scoped-define SELF-NAME remitos.anio_produccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.anio_produccion V-table-Win
ON LEAVE OF remitos.anio_produccion IN FRAME F-Main /* A�o producci�n */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.bultos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.bultos V-table-Win
ON LEAVE OF remitos.bultos IN FRAME F-Main /* Bultos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.chofer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.chofer V-table-Win
ON LEAVE OF remitos.chofer IN FRAME F-Main /* Nombre del chofer */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.c_fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.c_fecha V-table-Win
ON LEAVE OF remitos.c_fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.c_hora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.c_hora V-table-Win
ON LEAVE OF remitos.c_hora IN FRAME F-Main /* Hora */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.c_usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.c_usuario V-table-Win
ON LEAVE OF remitos.c_usuario IN FRAME F-Main /* Usuario */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.fecha V-table-Win
ON LEAVE OF remitos.fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.fecha_proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.fecha_proceso V-table-Win
ON LEAVE OF remitos.fecha_proceso IN FRAME F-Main /* Fecha Proc. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cia_seguro-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cia_seguro-descripcion V-table-Win
ON LEAVE OF fi-cia_seguro-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-clientes-razon_social
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-clientes-razon_social V-table-Win
ON LEAVE OF fi-clientes-razon_social IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-lugar_descarga-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-lugar_descarga-descripcion V-table-Win
ON LEAVE OF fi-lugar_descarga-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-proveedores-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-proveedores-nombre V-table-Win
ON LEAVE OF fi-proveedores-nombre IN FRAME F-Main
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


&Scoped-define SELF-NAME remitos.id_ciaseg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_ciaseg V-table-Win
ON GO OF remitos.id_ciaseg IN FRAME F-Main /* C�a.seguro */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_ciaseg V-table-Win
ON LEAVE OF remitos.id_ciaseg IN FRAME F-Main /* C�a.seguro */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_ciaseg V-table-Win
ON MOUSE-SELECT-DBLCLICK OF remitos.id_ciaseg IN FRAME F-Main /* C�a.seguro */
do: 
define var r as rowid no-undo.
run wc_ciaseg.w(output r).
find cia_seguro where rowid(cia_seguro) = r no-lock no-error.
if available cia_seguro then 
general.remitos.id_ciaseg:screen-value = string(cia_seguro.id_ciaseg).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_ciaseg V-table-Win
ON U1 OF remitos.id_ciaseg IN FRAME F-Main /* C�a.seguro */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_cliente V-table-Win
ON GO OF remitos.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_cliente V-table-Win
ON LEAVE OF remitos.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_cliente V-table-Win
ON MOUSE-SELECT-DBLCLICK OF remitos.id_cliente IN FRAME F-Main /* Cliente */
do: 
define var r as rowid no-undo.
run wc_clientes.w(output r).
find clientes where rowid(clientes) = r no-lock no-error.
if available clientes then 
general.remitos.id_cliente:screen-value = string(clientes.id_cliente).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_cliente V-table-Win
ON U1 OF remitos.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_destino V-table-Win
ON GO OF remitos.id_destino IN FRAME F-Main /* Destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_destino V-table-Win
ON LEAVE OF remitos.id_destino IN FRAME F-Main /* Destino */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_destino V-table-Win
ON MOUSE-SELECT-DBLCLICK OF remitos.id_destino IN FRAME F-Main /* Destino */
do: 
define var r as rowid no-undo.
run wc_destinos.w(output r).
find destinos where rowid(destinos) = r no-lock no-error.
if available destinos then 
general.remitos.id_destino:screen-value = string(destinos.id_destino).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_destino V-table-Win
ON U1 OF remitos.id_destino IN FRAME F-Main /* Destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_lugdes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_lugdes V-table-Win
ON GO OF remitos.id_lugdes IN FRAME F-Main /* Lugar descarga */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_lugdes V-table-Win
ON LEAVE OF remitos.id_lugdes IN FRAME F-Main /* Lugar descarga */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_lugdes V-table-Win
ON MOUSE-SELECT-DBLCLICK OF remitos.id_lugdes IN FRAME F-Main /* Lugar descarga */
do: 
define var r as rowid no-undo.
run wc_lugar_descarga.w(output r).
find lugar_descarga where rowid(lugar_descarga) = r no-lock no-error.
if available lugar_descarga then 
general.remitos.id_lugdes:screen-value = string(lugar_descarga.id_lugdes).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_lugdes V-table-Win
ON U1 OF remitos.id_lugdes IN FRAME F-Main /* Lugar descarga */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_orden_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_orden_entrega V-table-Win
ON LEAVE OF remitos.id_orden_entrega IN FRAME F-Main /* Orden Entrega */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_proveedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_proveedor V-table-Win
ON GO OF remitos.id_proveedor IN FRAME F-Main /* C�d.Proveedor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_proveedor V-table-Win
ON LEAVE OF remitos.id_proveedor IN FRAME F-Main /* C�d.Proveedor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_proveedor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF remitos.id_proveedor IN FRAME F-Main /* C�d.Proveedor */
do: 
define var r as rowid no-undo.
run wc_proveedores.w(output r).
find proveedores where rowid(proveedores) = r no-lock no-error.
if available proveedores then 
general.remitos.id_proveedor:screen-value = string(proveedores.id_proveedor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_proveedor V-table-Win
ON U1 OF remitos.id_proveedor IN FRAME F-Main /* C�d.Proveedor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_punto_venta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_punto_venta V-table-Win
ON LEAVE OF remitos.id_punto_venta IN FRAME F-Main /* PtoVenta */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_sucursal V-table-Win
ON LEAVE OF remitos.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_tipo_movsto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_tipo_movsto V-table-Win
ON LEAVE OF remitos.id_tipo_movsto IN FRAME F-Main /* T/Movsto */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.id_vapor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_vapor V-table-Win
ON GO OF remitos.id_vapor IN FRAME F-Main /* Vapor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_vapor V-table-Win
ON LEAVE OF remitos.id_vapor IN FRAME F-Main /* Vapor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_vapor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF remitos.id_vapor IN FRAME F-Main /* Vapor */
do: 
define var r as rowid no-undo.
run wc_vapores.w(output r).
find vapores where rowid(vapores) = r no-lock no-error.
if available vapores then 
general.remitos.id_vapor:screen-value = string(vapores.id_vapor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.id_vapor V-table-Win
ON U1 OF remitos.id_vapor IN FRAME F-Main /* Vapor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.item_oe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.item_oe V-table-Win
ON LEAVE OF remitos.item_oe IN FRAME F-Main /* Item OE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.mercado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.mercado V-table-Win
ON LEAVE OF remitos.mercado IN FRAME F-Main /* Mercado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.nro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.nro V-table-Win
ON LEAVE OF remitos.nro IN FRAME F-Main /* N�mero */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.nro_comprobante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.nro_comprobante V-table-Win
ON LEAVE OF remitos.nro_comprobante IN FRAME F-Main /* Comp.Legal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.nro_contenedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.nro_contenedor V-table-Win
ON LEAVE OF remitos.nro_contenedor IN FRAME F-Main /* Nro Contenedor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.nro_iascav
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.nro_iascav V-table-Win
ON LEAVE OF remitos.nro_iascav IN FRAME F-Main /* IASCAV */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.nro_orden_fab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.nro_orden_fab V-table-Win
ON LEAVE OF remitos.nro_orden_fab IN FRAME F-Main /* OF */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.nro_ord_carga
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.nro_ord_carga V-table-Win
ON LEAVE OF remitos.nro_ord_carga IN FRAME F-Main /* N� Orden Carga */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.nro_per_embarque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.nro_per_embarque V-table-Win
ON LEAVE OF remitos.nro_per_embarque IN FRAME F-Main /* Nro Perm Embarque */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.nro_precinto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.nro_precinto V-table-Win
ON LEAVE OF remitos.nro_precinto IN FRAME F-Main /* Nro Precinto */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.Pat_acopla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.Pat_acopla V-table-Win
ON LEAVE OF remitos.Pat_acopla IN FRAME F-Main /* Patente acoplado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.pat_chasis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.pat_chasis V-table-Win
ON LEAVE OF remitos.pat_chasis IN FRAME F-Main /* Chasis/Acoplado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.Peso_bruto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.Peso_bruto V-table-Win
ON LEAVE OF remitos.Peso_bruto IN FRAME F-Main /* Peso Bruto */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.Peso_neto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.Peso_neto V-table-Win
ON LEAVE OF remitos.Peso_neto IN FRAME F-Main /* Peso Neto */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.tipo_remito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.tipo_remito V-table-Win
ON LEAVE OF remitos.tipo_remito IN FRAME F-Main /* Tipo Remito */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME remitos.Valor_declarado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL remitos.Valor_declarado V-table-Win
ON LEAVE OF remitos.Valor_declarado IN FRAME F-Main /* Valor declarado */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualizo-kilos V-table-Win 
PROCEDURE actualizo-kilos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR v_kilos AS DECIMAL.
DEFINE VAR v_kilos_bruto AS DECIMAL.

FOR EACH items_factura OF remitos NO-LOCK.
    FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_remito = items_factura.id_sucursal
                                  AND tambores_industria.id_tipo_movsto     = items_factura.id_tipo_movsto
                                  AND tambores_industria.nro_remito         = items_factura.nro
                                  AND tambores_industria.item_factura       = items_factura.ITEM
                                NO-LOCK.
        v_kilos = v_kilos + tambores_industria.kilos_tambor.
        v_kilos_bruto = v_kilos_bruto + tambores_industria.kilos_tambor + tambores_industria.tara.
    END.
END.

IF v_kilos <> remitos.peso_neto THEN DO:
    ASSIGN remitos.peso_neto = v_kilos
           remitos.peso_bruto= v_kilos_bruto.
    remitos.peso_neto:SCREEN-VALUE IN FRAME F-Main = STRING(v_kilos).
    remitos.peso_bruto:SCREEN-VALUE IN FRAME F-Main = STRING(v_kilos_bruto).
END.

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
DEFINE VAR p_suc AS INTEGER.
DEFINE VAR p_tipo_movsto AS INTEGER.
DEFINE VAR hcon AS HANDLE.
DEFINE VAR v_nro AS INTEGER.

RUN get-container (OUTPUT hcon).
RUN get-sucursal IN hcon (OUTPUT p_suc,
                          OUTPUT p_tipo_movsto). 
  
FIND FIRST tipo_numero WHERE tipo_numero.id_sucursal = p_suc
                         AND tipo_numero.id_tipo_movsto   = p_tipo_movsto
                        NO-ERROR.

IF AVAILABLE tipo_numero THEN DO:
    v_nro = tipo_numero.nro + 1.
    tipo_numero.nro = v_nro.

    ASSIGN remitos.id_sucursal      = p_suc
           remitos.c_fecha          = TODAY
           remitos.c_usuario        = USERID("userdb")
           remitos.c_hora           = STRING(TIME,"hh:mm:ss")
           remitos.id_punto_venta   = tipo_numero.id_punto_venta
           remitos.id_tipo_movsto   = p_tipo_movsto
           remitos.nro              = v_nro
           remitos.tipo_remito      = IF p_tipo_movsto = 122 THEN FALSE ELSE TRUE
           .
END.
ELSE DO:
    MESSAGE "Hay un error en la sucursal o tipo venta seleccionado"
            VIEW-AS ALERT-BOX.
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

IF DATE(remitos.fecha:SCREEN-VALUE IN FRAME F-Main) <> TODAY THEN DO:
    MESSAGE "Por favor cargue la fecha de hoy en el remito." VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.

IF INTEGER(remitos.id_destino:SCREEN-VALUE IN FRAME F-Main) = 0 THEN DO:
    MESSAGE "Por favor cargue el destino del remito." VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.

IF INTEGER(remitos.id_lugdes:SCREEN-VALUE IN FRAME F-Main) = 0 THEN DO:
    MESSAGE "Por favor cargue el lugar de descarga del remito." VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.

IF INTEGER(remitos.id_cliente:SCREEN-VALUE IN FRAME F-Main) = 0 THEN DO:
    MESSAGE "Por favor cargue el cliente del remito." VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.

IF INTEGER(remitos.id_proveedor:SCREEN-VALUE IN FRAME F-Main) = 0 THEN DO:
    MESSAGE "Por favor cargue el Transportista del remito." VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.

IF remitos.chofer:SCREEN-VALUE IN FRAME F-Main = "" THEN DO:
    MESSAGE "Por favor cargue el nombre del chofer en el remito." VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.

IF INTEGER(remitos.id_ciaseg:SCREEN-VALUE IN FRAME F-Main) = 0 THEN DO:
    MESSAGE "Por favor cargue la compania de seguro del remito." VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.

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
  {src/adm/template/row-list.i "remitos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "remitos"}

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
find first destinos where destinos.id_destino = integer(remitos.id_destino:screen-value in frame F-Main)  no-lock no-error .
if available destinos then 
fi-destinos-descripcion:screen-value in frame F-Main = string(destinos.descripcion).
else
fi-destinos-descripcion:screen-value in frame F-Main = ''.

find first clientes where clientes.id_cliente = integer(remitos.id_cliente:screen-value in frame F-Main)  no-lock no-error .
if available clientes then 
fi-clientes-razon_social:screen-value in frame F-Main = string(clientes.razon_social).
else
fi-clientes-razon_social:screen-value in frame F-Main = ''.

find first lugar_descarga where lugar_descarga.id_lugdes = integer(remitos.id_lugdes:screen-value in frame F-Main)  no-lock no-error .
if available lugar_descarga then 
fi-lugar_descarga-descripcion:screen-value in frame F-Main = string(lugar_descarga.descripcion).
else
fi-lugar_descarga-descripcion:screen-value in frame F-Main = ''.

find first proveedores where proveedores.id_proveedor = integer(remitos.id_proveedor:screen-value in frame F-Main)  no-lock no-error .
if available proveedores then 
fi-proveedores-nombre:screen-value in frame F-Main = string(proveedores.nombre).
else
fi-proveedores-nombre:screen-value in frame F-Main = ''.

find first cia_seguro where cia_seguro.id_ciaseg = integer(remitos.id_ciaseg:screen-value in frame F-Main)  no-lock no-error .
if available cia_seguro then 
fi-cia_seguro-descripcion:screen-value in frame F-Main = string(cia_seguro.descripcion).
else
fi-cia_seguro-descripcion:screen-value in frame F-Main = ''.

find first vapores where vapores.id_vapor = integer(remitos.id_vapor:screen-value in frame F-Main)  no-lock no-error .
if available vapores then 
fi-vapores-descripcion:screen-value in frame F-Main = string(vapores.descripcion).
else
fi-vapores-descripcion:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_destino,id_cliente,id_lugdes,id_proveedor,id_ciaseg,id_vapor".
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
  {src/adm/template/snd-list.i "remitos"}

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

