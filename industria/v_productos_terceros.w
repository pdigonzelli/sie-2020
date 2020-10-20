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
define var r-del as rowid.
define var del-emp as integer.
define var del-suc as integer.
define var del-tip as integer.
define var del-nro as integer.
define var del-lot as integer.

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
&Scoped-define EXTERNAL-TABLES productos_terceros
&Scoped-define FIRST-EXTERNAL-TABLE productos_terceros


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR productos_terceros.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS productos_terceros.id_empresa ~
productos_terceros.observaciones productos_terceros.id_sucursal ~
productos_terceros.id_proveedor productos_terceros.id_articulo ~
productos_terceros.id_calidad productos_terceros.fecha ~
productos_terceros.kilos2 productos_terceros.id_envase ~
productos_terceros.cantidad productos_terceros.estado ~
productos_terceros.nro_orden_compra 
&Scoped-define ENABLED-TABLES productos_terceros
&Scoped-define FIRST-ENABLED-TABLE productos_terceros
&Scoped-Define ENABLED-OBJECTS fiEnvase2 fiKilos2 fiCantidad2 
&Scoped-Define DISPLAYED-FIELDS productos_terceros.id_empresa ~
productos_terceros.kilos productos_terceros.observaciones ~
productos_terceros.id_sucursal productos_terceros.id_lote ~
productos_terceros.id_proveedor productos_terceros.anio_lote ~
productos_terceros.id_articulo productos_terceros.id_calidad ~
productos_terceros.codigo_lote productos_terceros.fecha ~
productos_terceros.kilos2 productos_terceros.id_envase ~
productos_terceros.cantidad productos_terceros.estado ~
productos_terceros.nro_orden_compra 
&Scoped-define DISPLAYED-TABLES productos_terceros
&Scoped-define FIRST-DISPLAYED-TABLE productos_terceros
&Scoped-Define DISPLAYED-OBJECTS fi-empresas-razon_social ~
fi-sucursales-nombre fi-proveedores-nombre fi-productos_terminados-descrip ~
fi-calidades-descrip fi-envases_prod-descripcion fiEnvase2 fiKilos2 ~
fiCantidad2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS productos_terceros.id_empresa ~
productos_terceros.id_sucursal productos_terceros.id_lote ~
productos_terceros.id_proveedor productos_terceros.id_articulo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_envase||y|general.productos_terceros.id_envase
nromov||y|general.productos_terceros.nromov
id_sucursal||y|general.productos_terceros.id_sucursal
id_tipotambor||y|general.productos_terceros.id_tipotambor
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_envase,nromov,id_sucursal,id_tipotambor"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroLote V-table-Win 
FUNCTION getNextNroLote RETURNS CHARACTER
  (piSuc AS INTEGER,
   piArt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valida V-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-calidades-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-empresas-razon_social AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-proveedores-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-sucursales-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fiCantidad2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad 2" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiEnvase2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Envase 2" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiKilos2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kilos 2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     productos_terceros.id_empresa AT ROW 1 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     fi-empresas-razon_social AT ROW 1 COL 18 COLON-ALIGNED NO-LABEL
     productos_terceros.kilos AT ROW 1 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     productos_terceros.observaciones AT ROW 1.71 COL 105 NO-LABEL
          VIEW-AS EDITOR
          SIZE 36 BY 9.52
     fi-sucursales-nombre AT ROW 1.95 COL 23 COLON-ALIGNED NO-LABEL
     productos_terceros.id_sucursal AT ROW 2 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     productos_terceros.id_lote AT ROW 2.19 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     productos_terceros.id_proveedor AT ROW 3 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-proveedores-nombre AT ROW 3 COL 25 COLON-ALIGNED NO-LABEL
     productos_terceros.anio_lote AT ROW 3.38 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     fi-productos_terminados-descrip AT ROW 3.95 COL 30 COLON-ALIGNED NO-LABEL
     productos_terceros.id_articulo AT ROW 4 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     productos_terceros.id_calidad AT ROW 5.05 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi-calidades-descrip AT ROW 5.05 COL 24 COLON-ALIGNED NO-LABEL
     productos_terceros.codigo_lote AT ROW 6.05 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     productos_terceros.fecha AT ROW 6.24 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     productos_terceros.kilos2 AT ROW 7.1 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     productos_terceros.id_envase AT ROW 8.1 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi-envases_prod-descripcion AT ROW 8.1 COL 30 COLON-ALIGNED NO-LABEL
     productos_terceros.cantidad AT ROW 8.1 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fiEnvase2 AT ROW 9.1 COL 13 COLON-ALIGNED
     fiKilos2 AT ROW 9.1 COL 43 COLON-ALIGNED
     fiCantidad2 AT ROW 9.1 COL 80 COLON-ALIGNED
     productos_terceros.estado AT ROW 10.19 COL 83.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.6 BY 1
     productos_terceros.nro_orden_compra AT ROW 10.29 COL 13 COLON-ALIGNED
          LABEL "Ord. Comp."
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     "Observasciones" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1 COL 124
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.productos_terceros
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
         HEIGHT             = 10.29
         WIDTH              = 141.4.
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

/* SETTINGS FOR FILL-IN productos_terceros.anio_lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN productos_terceros.codigo_lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-calidades-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-empresas-razon_social IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-proveedores-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sucursales-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN productos_terceros.id_articulo IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN productos_terceros.id_empresa IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN productos_terceros.id_lote IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN productos_terceros.id_proveedor IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN productos_terceros.id_sucursal IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN productos_terceros.kilos IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       productos_terceros.kilos:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN productos_terceros.nro_orden_compra IN FRAME F-Main
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
general.productos_terceros.id_sucursal;wc_sucursales.w;sucursales.nombre;;
general.productos_terceros.id_envase;wc_envases.w;envases_prod.descripcion;;
general.productos_terceros.id_proveedor;wc_proveedores.w;proveedores.nombre;;
general.productos_terceros.id_empresa;wc_empreas.w;empresas.razon_social;;
general.productos_terceros.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
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

&Scoped-define SELF-NAME productos_terceros.anio_lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.anio_lote V-table-Win
ON LEAVE OF productos_terceros.anio_lote IN FRAME F-Main /* Año Lote */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.cantidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.cantidad V-table-Win
ON LEAVE OF productos_terceros.cantidad IN FRAME F-Main /* Cantidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.codigo_lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.codigo_lote V-table-Win
ON LEAVE OF productos_terceros.codigo_lote IN FRAME F-Main /* codigo_lote */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.estado V-table-Win
ON LEAVE OF productos_terceros.estado IN FRAME F-Main /* Estado */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.fecha V-table-Win
ON LEAVE OF productos_terceros.fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-calidades-descrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-calidades-descrip V-table-Win
ON LEAVE OF fi-calidades-descrip IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-empresas-razon_social
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresas-razon_social V-table-Win
ON LEAVE OF fi-empresas-razon_social IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-proveedores-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-proveedores-nombre V-table-Win
ON LEAVE OF fi-proveedores-nombre IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-sucursales-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sucursales-nombre V-table-Win
ON LEAVE OF fi-sucursales-nombre IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCantidad2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCantidad2 V-table-Win
ON LEAVE OF fiCantidad2 IN FRAME F-Main /* Cantidad 2 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEnvase2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEnvase2 V-table-Win
ON LEAVE OF fiEnvase2 IN FRAME F-Main /* Envase 2 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKilos2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKilos2 V-table-Win
ON LEAVE OF fiKilos2 IN FRAME F-Main /* Kilos 2 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_articulo V-table-Win
ON GO OF productos_terceros.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_articulo V-table-Win
ON LEAVE OF productos_terceros.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF productos_terceros.id_articulo IN FRAME F-Main /* Artículo */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.productos_terceros.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_articulo V-table-Win
ON U1 OF productos_terceros.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.id_calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_calidad V-table-Win
ON LEAVE OF productos_terceros.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_calidad V-table-Win
ON MOUSE-SELECT-DBLCLICK OF productos_terceros.id_calidad IN FRAME F-Main /* Calidad */
DO:
  define var r as rowid no-undo.
  run wc_calidades.w(output r).
  find calidades where rowid(calidades) = r no-lock no-error.
  if available calidades then 
  general.productos_terceros.id_calidad:screen-value = string(calidades.id_calidad).
  apply 'U1' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.id_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_empresa V-table-Win
ON GO OF productos_terceros.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_empresa V-table-Win
ON LEAVE OF productos_terceros.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF productos_terceros.id_empresa IN FRAME F-Main /* Empresa */
do: 
define var r as rowid no-undo.
run wc_empreas.w(output r).
find empresas where rowid(empresas) = r no-lock no-error.
if available empresas then 
general.productos_terceros.id_empresa:screen-value = string(empresas.id_empresa).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_empresa V-table-Win
ON U1 OF productos_terceros.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.id_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_envase V-table-Win
ON GO OF productos_terceros.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_envase V-table-Win
ON LEAVE OF productos_terceros.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_envase V-table-Win
ON MOUSE-SELECT-DBLCLICK OF productos_terceros.id_envase IN FRAME F-Main /* Envase */
do: 
define var r as rowid no-undo.
run wc_envases.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
general.productos_terceros.id_envase:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_envase V-table-Win
ON U1 OF productos_terceros.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.id_lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_lote V-table-Win
ON LEAVE OF productos_terceros.id_lote IN FRAME F-Main /* Lote */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.id_proveedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_proveedor V-table-Win
ON GO OF productos_terceros.id_proveedor IN FRAME F-Main /* Proveedor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_proveedor V-table-Win
ON LEAVE OF productos_terceros.id_proveedor IN FRAME F-Main /* Proveedor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_proveedor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF productos_terceros.id_proveedor IN FRAME F-Main /* Proveedor */
do: 
define var r as rowid no-undo.
run wc_proveedores.w(output r).
find proveedores where rowid(proveedores) = r no-lock no-error.
if available proveedores then 
general.productos_terceros.id_proveedor:screen-value = string(proveedores.id_proveedor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_proveedor V-table-Win
ON U1 OF productos_terceros.id_proveedor IN FRAME F-Main /* Proveedor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.id_sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_sucursal V-table-Win
ON GO OF productos_terceros.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_sucursal V-table-Win
ON LEAVE OF productos_terceros.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_sucursal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF productos_terceros.id_sucursal IN FRAME F-Main /* Sucursal */
do: 
define var r as rowid no-undo.
run wc_sucursales.w(output r).
find sucursales where rowid(sucursales) = r no-lock no-error.
if available sucursales then 
general.productos_terceros.id_sucursal:screen-value = string(sucursales.id_sucursal).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.id_sucursal V-table-Win
ON U1 OF productos_terceros.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.kilos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.kilos V-table-Win
ON LEAVE OF productos_terceros.kilos IN FRAME F-Main /* Kilos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.kilos2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.kilos2 V-table-Win
ON LEAVE OF productos_terceros.kilos2 IN FRAME F-Main /* kilos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME productos_terceros.nro_orden_compra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL productos_terceros.nro_orden_compra V-table-Win
ON LEAVE OF productos_terceros.nro_orden_compra IN FRAME F-Main /* Ord. Comp. */
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
DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
define var v_nro as integer.


v_nro = next-value(nromov).

assign productos_terceros.id_tipotambor = 9
       productos_terceros.nromov = v_nro.       

       
/*************SE CREAN LOS TAMBORES******************************/
define var r as rowid. 
define var i as integer.
define var n as integer.

n = integer(productos_terceros.cantidad:screen-value in frame {&FRAME-NAME}).
do i = 1 to n:
      create tambores_industria.
      assign tambores_industria.id_empresa  = integer(productos_terceros.id_empresa:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_sucursal        = integer(productos_terceros.id_sucursal:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_tipotambor      = 9
      tambores_industria.nromov             = v_nro
      tambores_industria.id_lote            = integer(productos_terceros.id_lote:screen-value in frame {&FRAME-NAME})
      tambores_industria.anio               = YEAR(TODAY)
      tambores_industria.id_proveedor       = integer(productos_terceros.id_proveedor:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_tambor          = i   /*ultimo*/
      tambores_industria.fecha              = date(productos_terceros.fecha:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_envase          = integer(productos_terceros.id_envase:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_articulo        = integer(productos_terceros.id_articulo:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_etiqueta        = next-value(tambores)
      tambores_industria.indice_tambor      = next-value(tambores_productos_terceros)
      tambores_industria.c_usuario          = userid("userdb")
      tambores_industria.c_fecha            = today
      tambores_industria.c_hora             = string(time,"HH:MM:SS")
      tambores_industria.id_empresa_ubicacion = integer(productos_terceros.id_empresa:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_sucursal_ubicacion = integer(productos_terceros.id_sucursal:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_locacion_ubicacion = 4
      tambores_industria.id_posicion_ubicacion = 1
      tambores_industria.kilos_tambor       = integer(productos_terceros.kilos2:screen-value in frame {&FRAME-NAME})
      tambores_industria.codigo_lote        = productos_terceros.codigo_lote:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      tambores_industria.id_estado          = 4 /*lote terminado*/
      .
      
      if productos_terceros.estado:screen-value in frame {&FRAME-NAME} = "Nuestro" then
        assign tambores_industria.estado = true.
      else 
        assign tambores_industria.estado = false.
end.

IF n > 0 THEN DO:
    
    RUN y_gstkcre.p (input integer(productos_terceros.id_empresa:screen-value in frame {&FRAME-NAME}),
                     input integer(productos_terceros.id_sucursal:screen-value in frame {&FRAME-NAME}),
                     input 9,
                     input v_nro,
                     INPUT 1,
                     INPUT n,
                     input 1) "productos_terceros".
    IF return-value <> "" then do:
        message "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "error".
    END.
END.



do i = n + 1 to (n + integer(fiCantidad2:screen-value in frame {&FRAME-NAME})):
      create tambores_industria.
      assign tambores_industria.id_empresa  = integer(productos_terceros.id_empresa:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_sucursal        = integer(productos_terceros.id_sucursal:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_tipotambor      = 9
      tambores_industria.nromov             = v_nro
      tambores_industria.id_lote            = integer(productos_terceros.id_lote:screen-value in frame {&FRAME-NAME})
      tambores_industria.anio               = YEAR(TODAY)
      tambores_industria.id_proveedor       = integer(productos_terceros.id_proveedor:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_tambor          = i   /*ultimo*/
      tambores_industria.fecha              = date(productos_terceros.fecha:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_envase          = integer(fiEnvase2:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_articulo        = integer(productos_terceros.id_articulo:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_etiqueta        = next-value(tambores)
      tambores_industria.indice_tambor      = next-value(tambores_productos_terceros)
      tambores_industria.c_usuario          = userid("userdb")
      tambores_industria.c_fecha            = today
      tambores_industria.c_hora             = string(time,"HH:MM:SS")
      tambores_industria.id_empresa_ubicacion = integer(productos_terceros.id_empresa:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_sucursal_ubicacion = integer(productos_terceros.id_sucursal:screen-value in frame {&FRAME-NAME})
      tambores_industria.id_locacion_ubicacion = 4
      tambores_industria.id_posicion_ubicacion = 1
      tambores_industria.kilos_tambor       = integer(fiKilos2:screen-value in frame {&FRAME-NAME})
      tambores_industria.codigo_lote        = productos_terceros.codigo_lote:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      tambores_industria.id_estado          = 4 /*lote terminado*/
      .
      
      if productos_terceros.estado:screen-value in frame {&FRAME-NAME} = "Nuestro" then
        assign tambores_industria.estado = true.
      else 
        assign tambores_industria.estado = false.
end.

IF integer(fiCantidad2:screen-value in frame {&FRAME-NAME}) > 0 THEN DO:
    
    RUN y_gstkcre.p (input integer(productos_terceros.id_empresa:screen-value in frame {&FRAME-NAME}),
                     input integer(productos_terceros.id_sucursal:screen-value in frame {&FRAME-NAME}),
                     input 9,
                     input v_nro,
                     INPUT n,
                     INPUT n + integer(fiCantidad2:screen-value in frame {&FRAME-NAME}),
                     input 1) "productos_terceros".
    IF return-value <> "" then do:
        message "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "error".
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

for each tambores_industria where tambores_industria.id_empresa = del-emp and 
                                  tambores_industria.id_sucursal = del-suc and
                                  tambores_industria.id_tipotambor = del-tip and
                                  tambores_industria.nromov = del-nro .

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
define var hcontainer as handle.
DEFINE VAR hasta AS INTEGER.
DEFINE BUFFER b_tam FOR tambores_industria.

run get-container (output hcontainer).
if hcontainer <> ? then do:
        run dame-rowid in hcontainer (output r-del).
        find productos_terceros where rowid(productos_terceros) = r-del no-lock no-error.
        if available productos_terceros THEN do:
                del-emp = productos_terceros.id_empresa.
                del-suc = productos_terceros.id_sucursal.
                del-tip = productos_terceros.id_tipotambor.
                del-nro = productos_terceros.nromov.
                FOR EACH b_tam WHERE b_tam.id_empresa        = del-emp and
                                     b_tam.id_sucursal       = del-suc and
                                     b_tam.id_tipotambor     = del-tip and
                                     b_tam.nromov            = del-nro .
                
                    ASSIGN b_tam.id_calidad = productos_terceros.id_calidad.
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
define var hcontainer as handle.
DEFINE VAR hasta AS INTEGER.
DEFINE BUFFER b_tam FOR tambores_industria.

run get-container (output hcontainer).
if hcontainer <> ? then 
    do:
        run dame-rowid in hcontainer (output r-del).
        find productos_terceros where rowid(productos_terceros) = r-del no-lock no-error.
        if available productos_terceros then
            do:
                del-emp = productos_terceros.id_empresa.
                del-suc = productos_terceros.id_sucursal.
                del-tip = productos_terceros.id_tipotambor.
                del-nro = productos_terceros.nromov.
                hasta = 0.
                FOR EACH b_tam WHERE b_tam.id_empresa        = del-emp and
                                     b_tam.id_sucursal       = del-suc and
                                     b_tam.id_tipotambor     = del-tip and
                                     b_tam.nromov            = del-nro NO-LOCK.
                
                    hasta = hasta + 1.
                END.
                
                RUN y_gstkcre.p (input del-emp,
                                 input del-suc,
                                 input del-tip,
                                 input del-nro,
                                 INPUT 1,
                                 INPUT hasta,
                                 input 2) "productos_terceros".
                
                IF return-value <> "" then do:
                    message "Error en el procesamiento de movimientos de stock" view-as alert-box.
                    RETURN "ADM-ERROR".
                end.
            end.
    end.
else 
    do:
        message "Se produjo un error al tratar de borrar".
        run dispatch "ADM-ERROR".
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
  {src/adm/template/row-list.i "productos_terceros"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "productos_terceros"}

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
find first sucursales where sucursales.id_sucursal = integer(productos_terceros.id_sucursal:screen-value in frame F-Main)  no-lock no-error .
if available sucursales then 
fi-sucursales-nombre:screen-value in frame F-Main = string(sucursales.nombre).
else
fi-sucursales-nombre:screen-value in frame F-Main = ''.

find first envases_prod where envases_prod.id_envase = integer(productos_terceros.id_envase:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion:screen-value in frame F-Main = ''.

find first proveedores where proveedores.id_proveedor = integer(productos_terceros.id_proveedor:screen-value in frame F-Main)  no-lock no-error .
if available proveedores then 
fi-proveedores-nombre:screen-value in frame F-Main = string(proveedores.nombre).
else
fi-proveedores-nombre:screen-value in frame F-Main = ''.

find first empresas where empresas.id_empresa = integer(productos_terceros.id_empresa:screen-value in frame F-Main)  no-lock no-error .
if available empresas then 
fi-empresas-razon_social:screen-value in frame F-Main = string(empresas.razon_social).
else
fi-empresas-razon_social:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(productos_terceros.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first calidades where calidades.id_calidad = integer(productos_terceros.id_calidad:screen-value in frame F-Main)  no-lock no-error .
if available calidades then 
fi-calidades-descrip:screen-value in frame F-Main = string(calidades.descripcion).
else
fi-calidades-descrip:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_sucursal,id_envase,id_proveedor,id_empresa,id_articulo".
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
  {src/adm/template/sndkycas.i "id_envase" "productos_terceros" "id_envase"}
  {src/adm/template/sndkycas.i "nromov" "productos_terceros" "nromov"}
  {src/adm/template/sndkycas.i "id_sucursal" "productos_terceros" "id_sucursal"}
  {src/adm/template/sndkycas.i "id_tipotambor" "productos_terceros" "id_tipotambor"}

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
  {src/adm/template/snd-list.i "productos_terceros"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroLote V-table-Win 
FUNCTION getNextNroLote RETURNS CHARACTER
  (piSuc AS INTEGER,
   piArt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cLot    AS CHARACTER  NO-UNDO.

  productos_terceros.id_lote:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.

  cLot = DYNAMIC-FUNCTION('getNextNroLote' IN hLibTam, piSuc, piArt, YEAR(TODAY)).
  productos_terceros.codigo_lote:SCREEN-VALUE IN FRAME F-Main = cLot.
  cLot = ENTRY(4, cLot, ".").
  
  productos_terceros.id_lote:SCREEN-VALUE IN FRAME F-Main     = ENTRY(1, cLot, "/").
  productos_terceros.anio:SCREEN-VALUE IN FRAME F-Main        = STRING(YEAR(TODAY)).

  
  RETURN cLot.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

      WHEN "id_articulo" THEN 
        getNextNroLote(INTEGER(productos_terceros.id_sucursal:SCREEN-VALUE IN FRAME {&FRAME-NAME}), 
                       INTEGER(productos_terceros.id_articulo:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

