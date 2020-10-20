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

DEFINE VAR v_id_orden_entrega AS INTEGER.

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
orden_entrega.fecha orden_entrega.pedido_fondos ~
orden_entrega.id_tipo_orden_entrega orden_entrega.cotizacion ~
orden_entrega.id_agencia orden_entrega.id_vapor ~
orden_entrega.id_despachante orden_entrega.id_destino ~
orden_entrega.fecha_arribo orden_entrega.semana_embarque ~
orden_entrega.fecha_embarque orden_entrega.id_lugdes ~
orden_entrega.observaciones 
&Scoped-define ENABLED-TABLES orden_entrega
&Scoped-define FIRST-ENABLED-TABLE orden_entrega
&Scoped-Define DISPLAYED-FIELDS orden_entrega.id_orden_entrega ~
orden_entrega.fecha orden_entrega.pedido_fondos ~
orden_entrega.id_tipo_orden_entrega orden_entrega.cotizacion ~
orden_entrega.id_agencia orden_entrega.id_vapor ~
orden_entrega.id_despachante orden_entrega.id_destino ~
orden_entrega.fecha_arribo orden_entrega.semana_embarque ~
orden_entrega.fecha_embarque orden_entrega.id_lugdes ~
orden_entrega.observaciones 
&Scoped-define DISPLAYED-TABLES orden_entrega
&Scoped-define FIRST-DISPLAYED-TABLE orden_entrega
&Scoped-Define DISPLAYED-OBJECTS fi-tipos_orden_entrega-descripc ~
fi-agencia-descripcion fi-vapores-descripcion fi-despachantes-descripcion ~
fi-destinos-descripcion fi-lugar_descarga-descripcion 

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
DEFINE VARIABLE fi-agencia-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-despachantes-descripcion AS CHARACTER FORMAT "X(256)":U 
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

DEFINE VARIABLE fi-tipos_orden_entrega-descripc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-vapores-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.19
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     orden_entrega.id_orden_entrega AT ROW 1 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     orden_entrega.fecha AT ROW 1 COL 47 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     orden_entrega.pedido_fondos AT ROW 1 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     orden_entrega.id_tipo_orden_entrega AT ROW 1.95 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi-tipos_orden_entrega-descripc AT ROW 1.95 COL 33 COLON-ALIGNED NO-LABEL
     orden_entrega.cotizacion AT ROW 1.95 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     fi-agencia-descripcion AT ROW 2.91 COL 33 COLON-ALIGNED NO-LABEL
     orden_entrega.id_agencia AT ROW 2.95 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi-vapores-descripcion AT ROW 3.86 COL 33 COLON-ALIGNED NO-LABEL
     orden_entrega.id_vapor AT ROW 3.95 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     orden_entrega.id_despachante AT ROW 5.05 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi-despachantes-descripcion AT ROW 5.05 COL 33 COLON-ALIGNED NO-LABEL
     orden_entrega.id_destino AT ROW 6 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     fi-destinos-descripcion AT ROW 6 COL 33 COLON-ALIGNED NO-LABEL
     orden_entrega.fecha_arribo AT ROW 6.95 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     orden_entrega.semana_embarque AT ROW 7 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     orden_entrega.fecha_embarque AT ROW 7 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     fi-lugar_descarga-descripcion AT ROW 7.91 COL 33 COLON-ALIGNED NO-LABEL
     orden_entrega.id_lugdes AT ROW 7.95 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     orden_entrega.observaciones AT ROW 8.86 COL 24 NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 100
          SIZE 50 BY 2
     "Observaciones:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 9.1 COL 7
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
         HEIGHT             = 9.95
         WIDTH              = 101.
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

/* SETTINGS FOR FILL-IN fi-agencia-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-despachantes-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-destinos-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lugar_descarga-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipos_orden_entrega-descripc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vapores-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN orden_entrega.id_orden_entrega IN FRAME F-Main
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
general.orden_entrega.id_agencia;wc_agencias.w;agencia.descripcion;;
general.orden_entrega.id_vapor;wc_vapores.w;vapores.descripcion;;
general.orden_entrega.id_tipo_orden_entrega;wc_tipos_orden_entrega.w;tipos_orden_entrega.descripcion;;
general.orden_entrega.id_destino;wc_destinos.w;destinos.descripcion;;
general.orden_entrega.id_lugdes;wc_lugar_descarga.w;lugar_descarga.descripcion;;
general.orden_entrega.id_despachante;wc_despachantes.w;despachantes.descripcion;;
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

&Scoped-define SELF-NAME orden_entrega.cotizacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.cotizacion V-table-Win
ON LEAVE OF orden_entrega.cotizacion IN FRAME F-Main /* Cotizacion */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.fecha V-table-Win
ON LEAVE OF orden_entrega.fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
ON LEAVE OF orden_entrega.fecha_embarque IN FRAME F-Main /* Fecha embarque */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-agencia-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-agencia-descripcion V-table-Win
ON LEAVE OF fi-agencia-descripcion IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-lugar_descarga-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-lugar_descarga-descripcion V-table-Win
ON LEAVE OF fi-lugar_descarga-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipos_orden_entrega-descripc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipos_orden_entrega-descripc V-table-Win
ON LEAVE OF fi-tipos_orden_entrega-descripc IN FRAME F-Main
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


&Scoped-define SELF-NAME orden_entrega.id_agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_agencia V-table-Win
ON GO OF orden_entrega.id_agencia IN FRAME F-Main /* Cod. Agencia */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_agencia V-table-Win
ON LEAVE OF orden_entrega.id_agencia IN FRAME F-Main /* Cod. Agencia */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_agencia V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_agencia IN FRAME F-Main /* Cod. Agencia */
do: 
define var r as rowid no-undo.
run wc_agencias.w(output r).
find agencia where rowid(agencia) = r no-lock no-error.
if available agencia then 
general.orden_entrega.id_agencia:screen-value = string(agencia.id_agencia).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_agencia V-table-Win
ON U1 OF orden_entrega.id_agencia IN FRAME F-Main /* Cod. Agencia */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_despachante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_despachante V-table-Win
ON GO OF orden_entrega.id_despachante IN FRAME F-Main /* Cod.Despachante */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_despachante V-table-Win
ON LEAVE OF orden_entrega.id_despachante IN FRAME F-Main /* Cod.Despachante */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_despachante V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_despachante IN FRAME F-Main /* Cod.Despachante */
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
ON U1 OF orden_entrega.id_despachante IN FRAME F-Main /* Cod.Despachante */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_destino V-table-Win
ON GO OF orden_entrega.id_destino IN FRAME F-Main /* Cod.Destino */
DO:
{custom/support/validacion.i}
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
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_lugdes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_lugdes V-table-Win
ON GO OF orden_entrega.id_lugdes IN FRAME F-Main /* Cod. Lug. Descarga */
DO:
{custom/support/validacion.i}
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
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_orden_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_orden_entrega V-table-Win
ON LEAVE OF orden_entrega.id_orden_entrega IN FRAME F-Main /* OE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_tipo_orden_entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_orden_entrega V-table-Win
ON GO OF orden_entrega.id_tipo_orden_entrega IN FRAME F-Main /* Cod.TipoOE */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_orden_entrega V-table-Win
ON LEAVE OF orden_entrega.id_tipo_orden_entrega IN FRAME F-Main /* Cod.TipoOE */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_orden_entrega V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_tipo_orden_entrega IN FRAME F-Main /* Cod.TipoOE */
do: 
define var r as rowid no-undo.
run wc_tipos_orden_entrega.w(output r).
find tipos_orden_entrega where rowid(tipos_orden_entrega) = r no-lock no-error.
if available tipos_orden_entrega then 
general.orden_entrega.id_tipo_orden_entrega:screen-value = string(tipos_orden_entrega.id_tipo_orden_entrega).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_tipo_orden_entrega V-table-Win
ON U1 OF orden_entrega.id_tipo_orden_entrega IN FRAME F-Main /* Cod.TipoOE */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.id_vapor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_vapor V-table-Win
ON GO OF orden_entrega.id_vapor IN FRAME F-Main /* Cod. Vapor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_vapor V-table-Win
ON LEAVE OF orden_entrega.id_vapor IN FRAME F-Main /* Cod. Vapor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.id_vapor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF orden_entrega.id_vapor IN FRAME F-Main /* Cod. Vapor */
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
ON U1 OF orden_entrega.id_vapor IN FRAME F-Main /* Cod. Vapor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.pedido_fondos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.pedido_fondos V-table-Win
ON LEAVE OF orden_entrega.pedido_fondos IN FRAME F-Main /* Pedido de Fondos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orden_entrega.semana_embarque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orden_entrega.semana_embarque V-table-Win
ON LEAVE OF orden_entrega.semana_embarque IN FRAME F-Main /* Semana embarque */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualiza_estado_oe V-table-Win 
PROCEDURE actualiza_estado_oe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR r AS ROWID.
DEFINE VAR hcon AS HANDLE.

RUN GET-container (OUTPUT hcon).
RUN dame_datos_orden IN hcon (OUTPUT r).
FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = r NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    IF orden_entrega.fecha_arribo <= TODAY AND 
       orden_entrega.id_estado = 4 THEN DO:
        FOR EACH items_orden_entrega OF orden_entrega.
            items_orden_entrega.id_estado = 5.
        END.
        ASSIGN orden_entrega.id_estado = 5.
    END.
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
IF AVAILABLE orden_entrega THEN DO:
    orden_entrega.anio = YEAR(TODAY).
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
MESSAGE "Se borraran las partes de la OE " v_id_orden_entrega VIEW-AS ALERT-BOX.

FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = v_id_orden_entrega.
    DELETE items_orden_entrega.
END.

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
DEFINE VAR v_fecha AS DATE.
DEFINE VAR v_eta AS DATE.
DEFINE VAR v_orden_entrega AS INTEGER.

v_orden_entrega = INTEGER(orden_entrega.id_orden_entrega:SCREEN-VALUE IN FRAME F-Main).
FIND FIRST packing_list WHERE packing_list.id_orden_entrega = v_orden_entrega NO-LOCK NO-ERROR.
IF AVAILABLE packing_list THEN DO:
    v_fecha = ventas.packing_list.fecha_salida_vapor.
    v_eta   = ventas.packing_list.fecha_salida_vapor + ventas.packing_list.dias_transito.
    orden_entrega.fecha_embarque:SCREEN-VALUE IN FRAME F-Main = STRING(v_fecha).
    orden_entrega.fecha_arribo:SCREEN-VALUE IN FRAME F-Main = STRING(v_eta).
    ASSIGN orden_entrega.fecha_embarque = v_fecha
           orden_entrega.fecha_arribo   = v_eta.
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
v_id_orden_entrega = orden_entrega.id_orden_entrega.

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
find first agencia where agencia.id_agencia = integer(orden_entrega.id_agencia:screen-value in frame F-Main)  no-lock no-error .
if available agencia then 
fi-agencia-descripcion:screen-value in frame F-Main = string(agencia.descripcion).
else
fi-agencia-descripcion:screen-value in frame F-Main = ''.

find first vapores where vapores.id_vapor = integer(orden_entrega.id_vapor:screen-value in frame F-Main)  no-lock no-error .
if available vapores then 
fi-vapores-descripcion:screen-value in frame F-Main = string(vapores.descripcion).
else
fi-vapores-descripcion:screen-value in frame F-Main = ''.

find first tipos_orden_entrega where tipos_orden_entrega.id_tipo_orden_entrega = integer(orden_entrega.id_tipo_orden_entrega:screen-value in frame F-Main)  no-lock no-error .
if available tipos_orden_entrega then 
fi-tipos_orden_entrega-descripc:screen-value in frame F-Main = string(tipos_orden_entrega.descripcion).
else
fi-tipos_orden_entrega-descripc:screen-value in frame F-Main = ''.

find first destinos where destinos.id_destino = integer(orden_entrega.id_destino:screen-value in frame F-Main)  no-lock no-error .
if available destinos then 
fi-destinos-descripcion:screen-value in frame F-Main = string(destinos.descripcion).
else
fi-destinos-descripcion:screen-value in frame F-Main = ''.

find first lugar_descarga where lugar_descarga.id_lugdes = integer(orden_entrega.id_lugdes:screen-value in frame F-Main)  no-lock no-error .
if available lugar_descarga then 
fi-lugar_descarga-descripcion:screen-value in frame F-Main = string(lugar_descarga.descripcion).
else
fi-lugar_descarga-descripcion:screen-value in frame F-Main = ''.

find first despachantes where despachantes.id_despachante = integer(orden_entrega.id_despachante:screen-value in frame F-Main)  no-lock no-error .
if available despachantes then 
fi-despachantes-descripcion:screen-value in frame F-Main = string(despachantes.descripcion).
else
fi-despachantes-descripcion:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_agencia,id_vapor,id_tipo_orden_entrega,id_destino,id_lugdes,id_despachante".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-cancel V-table-Win 
PROCEDURE post-cancel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
DEFINE VAR v_corto AS INTEGER.
DEFINE VAR v_largo AS INTEGER.
DEFINE BUFFER bb_oe FOR orden_entrega.

FIND LAST bb_oe WHERE bb_oe.id_orden_entrega > 1000 NO-LOCK NO-ERROR.
IF AVAILABLE bb_oe THEN v_largo = bb_oe.id_orden_entrega + 1.
FIND LAST bb_oe WHERE bb_oe.id_orden_entrega < 1000 NO-LOCK NO-ERROR.
IF AVAILABLE bb_oe THEN v_corto = bb_oe.id_orden_entrega + 1.

MESSAGE "Nueva OE exportacion " v_largo ". O nueva OE mercado interno " v_corto "."
    VIEW-AS ALERT-BOX.

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
    when "fecha_embarque" then
        if YEAR(DATE(valor)) <> YEAR(TODAY) AND MONTH(TODAY) <> 12 then 
        do:
            mensaje = "Por favor ingrese una fecha de Embarque Valida.Gracias".
            return false.
         end.
  end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

