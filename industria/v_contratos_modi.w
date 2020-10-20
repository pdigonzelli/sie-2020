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

define var del_id_contrato like contratos.id_contrato.
define var del_id_tipo_contrato like contratos.id_tipo_contrato.
define var del_anio like contratos.anio.
define var v_alta as integer initial 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES contratos
&Scoped-define FIRST-EXTERNAL-TABLE contratos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR contratos.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS contratos.id_tipo_contrato ~
contratos.id_contrato contratos.orden_fabricacion contratos.anio ~
contratos.fecha contratos.id_po_cliente[1] contratos.direccion_envio ~
contratos.plazo contratos.id_cliente contratos.id_tipo_plazo ~
contratos.id_broker contratos.id_instrumento_pago contratos.id_consignee ~
contratos.id_banco contratos.id_notify contratos.id_cliente_destino ~
contratos.detalle_vta contratos.id_documentacion contratos.email_pl ~
contratos.otros_doc contratos.otros_productos 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}id_tipo_contrato ~{&FP2}id_tipo_contrato ~{&FP3}~
 ~{&FP1}id_contrato ~{&FP2}id_contrato ~{&FP3}~
 ~{&FP1}orden_fabricacion ~{&FP2}orden_fabricacion ~{&FP3}~
 ~{&FP1}anio ~{&FP2}anio ~{&FP3}~
 ~{&FP1}fecha ~{&FP2}fecha ~{&FP3}~
 ~{&FP1}id_po_cliente[1] ~{&FP2}id_po_cliente[1] ~{&FP3}~
 ~{&FP1}direccion_envio ~{&FP2}direccion_envio ~{&FP3}~
 ~{&FP1}plazo ~{&FP2}plazo ~{&FP3}~
 ~{&FP1}id_cliente ~{&FP2}id_cliente ~{&FP3}~
 ~{&FP1}id_tipo_plazo ~{&FP2}id_tipo_plazo ~{&FP3}~
 ~{&FP1}id_broker ~{&FP2}id_broker ~{&FP3}~
 ~{&FP1}id_instrumento_pago ~{&FP2}id_instrumento_pago ~{&FP3}~
 ~{&FP1}id_consignee ~{&FP2}id_consignee ~{&FP3}~
 ~{&FP1}id_banco ~{&FP2}id_banco ~{&FP3}~
 ~{&FP1}id_notify ~{&FP2}id_notify ~{&FP3}~
 ~{&FP1}id_cliente_destino ~{&FP2}id_cliente_destino ~{&FP3}~
 ~{&FP1}id_documentacion ~{&FP2}id_documentacion ~{&FP3}~
 ~{&FP1}email_pl ~{&FP2}email_pl ~{&FP3}
&Scoped-define ENABLED-TABLES contratos
&Scoped-define FIRST-ENABLED-TABLE contratos
&Scoped-Define ENABLED-OBJECTS BUTTON-5 BUTTON-2 BUTTON-3 BUTTON-6 BUTTON-4 
&Scoped-Define DISPLAYED-FIELDS contratos.id_tipo_contrato ~
contratos.id_contrato contratos.orden_fabricacion contratos.anio ~
contratos.fecha contratos.id_po_cliente[1] contratos.direccion_envio ~
contratos.plazo contratos.id_cliente contratos.id_tipo_plazo ~
contratos.id_broker contratos.id_instrumento_pago contratos.id_consignee ~
contratos.id_banco contratos.id_notify contratos.id_cliente_destino ~
contratos.detalle_vta contratos.id_documentacion contratos.email_pl ~
contratos.otros_doc contratos.otros_productos 
&Scoped-Define DISPLAYED-OBJECTS fi-tipos_contratos-descripcion ~
fi-clientes-razon_social fi-tipos_plazo-descripcion_ingl ~
fi-contactos_industria-nombre-2 fi-instrumentos_pagos-descripci ~
fi-contactos_industria-nombre-3 fi-bancos-nombre ~
fi-contactos_industria-nombre-4 fi-contactos_industria-nombre ~
fi-contactos_industria-nombre-1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_tipo_contrato||y|general.contratos.id_tipo_contrato
id_tipo_plazo||y|general.contratos.id_tipo_plazo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_tipo_contrato,id_tipo_plazo"':U).
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
DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "Tipos Plazos" 
     SIZE 7 BY .95.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "Tipos Plazos" 
     SIZE 7 BY .95.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "Tipos Plazos" 
     SIZE 7 BY .95.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "Tipos Plazos" 
     SIZE 7 BY .95.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "Tipos Plazos" 
     SIZE 7 BY .95.

DEFINE VARIABLE fi-bancos-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-clientes-razon_social AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-contactos_industria-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-contactos_industria-nombre-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-contactos_industria-nombre-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-contactos_industria-nombre-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-contactos_industria-nombre-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-instrumentos_pagos-descripci AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipos_contratos-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipos_plazo-descripcion_ingl AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     contratos.id_tipo_contrato AT ROW 1 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-tipos_contratos-descripcion AT ROW 1 COL 21 COLON-ALIGNED NO-LABEL
     BUTTON-5 AT ROW 1 COL 64
     contratos.id_contrato AT ROW 1 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     contratos.orden_fabricacion AT ROW 1 COL 104 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     contratos.anio AT ROW 1 COL 118 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     contratos.fecha AT ROW 1 COL 134 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     contratos.id_po_cliente[1] AT ROW 1.95 COL 14 COLON-ALIGNED
          LABEL "PO Cliente"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     contratos.direccion_envio AT ROW 1.95 COL 65.8
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     contratos.plazo AT ROW 2.91 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     contratos.id_cliente AT ROW 3.38 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     fi-clientes-razon_social AT ROW 3.38 COL 96 COLON-ALIGNED NO-LABEL
     contratos.id_tipo_plazo AT ROW 3.86 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     fi-tipos_plazo-descripcion_ingl AT ROW 3.86 COL 22 COLON-ALIGNED NO-LABEL
     BUTTON-2 AT ROW 3.86 COL 66
     contratos.id_broker AT ROW 4.33 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     fi-contactos_industria-nombre-2 AT ROW 4.33 COL 92 COLON-ALIGNED NO-LABEL
     BUTTON-3 AT ROW 4.33 COL 139
     contratos.id_instrumento_pago AT ROW 4.81 COL 14 COLON-ALIGNED
          LABEL "Cod. Ins.Pago"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     fi-instrumentos_pagos-descripci AT ROW 4.81 COL 22 COLON-ALIGNED NO-LABEL
     BUTTON-6 AT ROW 4.81 COL 66
     contratos.id_consignee AT ROW 5.29 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     fi-contactos_industria-nombre-3 AT ROW 5.29 COL 92 COLON-ALIGNED NO-LABEL
     contratos.id_banco AT ROW 5.76 COL 11 COLON-ALIGNED
          LABEL "Banco" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     fi-bancos-nombre AT ROW 5.76 COL 22 COLON-ALIGNED NO-LABEL
     BUTTON-4 AT ROW 5.76 COL 66
     contratos.id_notify AT ROW 6.24 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     fi-contactos_industria-nombre-4 AT ROW 6.24 COL 92 COLON-ALIGNED NO-LABEL
     contratos.id_cliente_destino AT ROW 7.19 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     fi-contactos_industria-nombre AT ROW 7.19 COL 93 COLON-ALIGNED NO-LABEL
     contratos.detalle_vta AT ROW 7.91 COL 4 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 65 BY 5.24
     contratos.id_documentacion AT ROW 8.14 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-contactos_industria-nombre-1 AT ROW 8.14 COL 95 COLON-ALIGNED NO-LABEL
     contratos.email_pl AT ROW 9.1 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 53 BY 1
     contratos.otros_doc AT ROW 10.29 COL 85 NO-LABEL
          VIEW-AS EDITOR
          SIZE 60 BY 5
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     contratos.otros_productos AT ROW 13.38 COL 13 NO-LABEL
          VIEW-AS EDITOR
          SIZE 56 BY 2
     "Comentario" VIEW-AS TEXT
          SIZE 11 BY .71 AT ROW 13.62 COL 1
     "Other" VIEW-AS TEXT
          SIZE 11 BY .71 AT ROW 10.29 COL 72
     "Calidad:" VIEW-AS TEXT
          SIZE 11 BY .71 AT ROW 14.33 COL 1
     "Documents:" VIEW-AS TEXT
          SIZE 11 BY .71 AT ROW 11 COL 72
     "Detalle de Venta:" VIEW-AS TEXT
          SIZE 17 BY .95 AT ROW 6.95 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.contratos
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
         HEIGHT             = 14.38
         WIDTH              = 151.2.
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

/* SETTINGS FOR FILL-IN contratos.direccion_envio IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-bancos-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-clientes-razon_social IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-contactos_industria-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-contactos_industria-nombre-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-contactos_industria-nombre-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-contactos_industria-nombre-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-contactos_industria-nombre-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-instrumentos_pagos-descripci IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipos_contratos-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipos_plazo-descripcion_ingl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contratos.id_banco IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN contratos.id_instrumento_pago IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN contratos.id_po_cliente[1] IN FRAME F-Main
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
general.contratos.id_tipo_contrato;wc_tipos_contratos.w;tipos_contratos.descripcion;;
general.contratos.id_cliente_destino;wc_contactos_industria.w;contactos_industria.nombre;id_contacto;
general.contratos.id_documentacion;wc_contactos_industria.w;contactos_industria.nombre;id_contacto;
general.contratos.id_instrumento_pago;wc_instrumentos_pagos.w;instrumentos_pagos.descripcion_ingles;;
general.contratos.id_tipo_plazo;wc_tipos_plazo.w;tipos_plazo.descripcion_ingles;;
general.contratos.id_banco;wc_bancos.w;bancos.nombre;;
general.contratos.id_broker;wc_contactos_industria.w;contactos_industria.nombre;id_contacto;
general.contratos.id_consignee;wc_contactos_industria.w;contactos_industria.nombre;id_contacto;
general.contratos.id_notify;wc_contactos_industria.w;contactos_industria.nombre;id_contacto;
general.contratos.id_cliente;wc_clientes.w;clientes.razon_social;;
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

&Scoped-define SELF-NAME contratos.anio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.anio V-table-Win
ON LEAVE OF contratos.anio IN FRAME F-Main /* A�o */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 V-table-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Tipos Plazos */
DO:
  run w_tipos_plazo.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 V-table-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Tipos Plazos */
DO:
  run w_contactos_industria.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 V-table-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Tipos Plazos */
DO:
  run w_bancos.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 V-table-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Tipos Plazos */
DO:
  run w_tipos_contratos.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 V-table-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Tipos Plazos */
DO:
  run w_instrumentos_pagos.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.direccion_envio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.direccion_envio V-table-Win
ON LEAVE OF contratos.direccion_envio IN FRAME F-Main /* Direcci�n de Envio */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.email_pl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.email_pl V-table-Win
ON LEAVE OF contratos.email_pl IN FRAME F-Main /* Email */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.fecha V-table-Win
ON LEAVE OF contratos.fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-bancos-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-bancos-nombre V-table-Win
ON LEAVE OF fi-bancos-nombre IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-contactos_industria-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-contactos_industria-nombre V-table-Win
ON LEAVE OF fi-contactos_industria-nombre IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-contactos_industria-nombre-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-contactos_industria-nombre-1 V-table-Win
ON LEAVE OF fi-contactos_industria-nombre-1 IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-contactos_industria-nombre-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-contactos_industria-nombre-2 V-table-Win
ON LEAVE OF fi-contactos_industria-nombre-2 IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-contactos_industria-nombre-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-contactos_industria-nombre-3 V-table-Win
ON LEAVE OF fi-contactos_industria-nombre-3 IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-contactos_industria-nombre-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-contactos_industria-nombre-4 V-table-Win
ON LEAVE OF fi-contactos_industria-nombre-4 IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-tipos_contratos-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipos_contratos-descripcion V-table-Win
ON LEAVE OF fi-tipos_contratos-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipos_plazo-descripcion_ingl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipos_plazo-descripcion_ingl V-table-Win
ON LEAVE OF fi-tipos_plazo-descripcion_ingl IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_banco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_banco V-table-Win
ON GO OF contratos.id_banco IN FRAME F-Main /* Banco */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_banco V-table-Win
ON LEAVE OF contratos.id_banco IN FRAME F-Main /* Banco */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_banco V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_banco IN FRAME F-Main /* Banco */
do: 
define var r as rowid no-undo.
run wc_bancos.w(output r).
find bancos where rowid(bancos) = r no-lock no-error.
if available bancos then 
general.contratos.id_banco:screen-value = string(bancos.id_banco).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_banco V-table-Win
ON U1 OF contratos.id_banco IN FRAME F-Main /* Banco */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_broker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_broker V-table-Win
ON GO OF contratos.id_broker IN FRAME F-Main /* Broker */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_broker V-table-Win
ON LEAVE OF contratos.id_broker IN FRAME F-Main /* Broker */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_broker V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_broker IN FRAME F-Main /* Broker */
do: 
define var r as rowid no-undo.
run wc_contactos_industria.w(output r).
find contactos_industria where rowid(contactos_industria) = r no-lock no-error.
if available contactos_industria then 
general.contratos.id_broker:screen-value = string(contactos_industria.id_contacto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_broker V-table-Win
ON U1 OF contratos.id_broker IN FRAME F-Main /* Broker */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_cliente V-table-Win
ON GO OF contratos.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_cliente V-table-Win
ON LEAVE OF contratos.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_cliente V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_cliente IN FRAME F-Main /* Cliente */
do: 
define var r as rowid no-undo.
run wc_clientes.w(output r).
find clientes where rowid(clientes) = r no-lock no-error.
if available clientes then 
general.contratos.id_cliente:screen-value = string(clientes.id_cliente).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_cliente V-table-Win
ON U1 OF contratos.id_cliente IN FRAME F-Main /* Cliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_cliente_destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_cliente_destino V-table-Win
ON GO OF contratos.id_cliente_destino IN FRAME F-Main /* Cliente destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_cliente_destino V-table-Win
ON LEAVE OF contratos.id_cliente_destino IN FRAME F-Main /* Cliente destino */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_cliente_destino V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_cliente_destino IN FRAME F-Main /* Cliente destino */
do: 
define var r as rowid no-undo.
run wc_contactos_industria.w(output r).
find contactos_industria where rowid(contactos_industria) = r no-lock no-error.
if available contactos_industria then 
general.contratos.id_cliente_destino:screen-value = string(contactos_industria.id_contacto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_cliente_destino V-table-Win
ON U1 OF contratos.id_cliente_destino IN FRAME F-Main /* Cliente destino */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_consignee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_consignee V-table-Win
ON GO OF contratos.id_consignee IN FRAME F-Main /* Consignee */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_consignee V-table-Win
ON LEAVE OF contratos.id_consignee IN FRAME F-Main /* Consignee */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_consignee V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_consignee IN FRAME F-Main /* Consignee */
do: 
define var r as rowid no-undo.
run wc_contactos_industria.w(output r).
find contactos_industria where rowid(contactos_industria) = r no-lock no-error.
if available contactos_industria then 
general.contratos.id_consignee:screen-value = string(contactos_industria.id_contacto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_consignee V-table-Win
ON U1 OF contratos.id_consignee IN FRAME F-Main /* Consignee */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_contrato V-table-Win
ON LEAVE OF contratos.id_contrato IN FRAME F-Main /* Contract */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_documentacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_documentacion V-table-Win
ON GO OF contratos.id_documentacion IN FRAME F-Main /* Documentos */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_documentacion V-table-Win
ON LEAVE OF contratos.id_documentacion IN FRAME F-Main /* Documentos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_documentacion V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_documentacion IN FRAME F-Main /* Documentos */
do: 
define var r as rowid no-undo.
run wc_contactos_industria.w(output r).
find contactos_industria where rowid(contactos_industria) = r no-lock no-error.
if available contactos_industria then 
general.contratos.id_documentacion:screen-value = string(contactos_industria.id_contacto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_documentacion V-table-Win
ON U1 OF contratos.id_documentacion IN FRAME F-Main /* Documentos */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_instrumento_pago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_instrumento_pago V-table-Win
ON GO OF contratos.id_instrumento_pago IN FRAME F-Main /* Cod. Ins.Pago */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_instrumento_pago V-table-Win
ON LEAVE OF contratos.id_instrumento_pago IN FRAME F-Main /* Cod. Ins.Pago */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_instrumento_pago V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_instrumento_pago IN FRAME F-Main /* Cod. Ins.Pago */
do: 
define var r as rowid no-undo.
run wc_instrumentos_pagos.w(output r).
find instrumentos_pagos where rowid(instrumentos_pagos) = r no-lock no-error.
if available instrumentos_pagos then 
general.contratos.id_instrumento_pago:screen-value = string(instrumentos_pagos.id_instrumento_pago).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_instrumento_pago V-table-Win
ON U1 OF contratos.id_instrumento_pago IN FRAME F-Main /* Cod. Ins.Pago */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_notify
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_notify V-table-Win
ON GO OF contratos.id_notify IN FRAME F-Main /* Notify */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_notify V-table-Win
ON LEAVE OF contratos.id_notify IN FRAME F-Main /* Notify */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_notify V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_notify IN FRAME F-Main /* Notify */
do: 
define var r as rowid no-undo.
run wc_contactos_industria.w(output r).
find contactos_industria where rowid(contactos_industria) = r no-lock no-error.
if available contactos_industria then 
general.contratos.id_notify:screen-value = string(contactos_industria.id_contacto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_notify V-table-Win
ON U1 OF contratos.id_notify IN FRAME F-Main /* Notify */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_po_cliente[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_po_cliente[1] V-table-Win
ON LEAVE OF contratos.id_po_cliente[1] IN FRAME F-Main /* PO Cliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_tipo_contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_tipo_contrato V-table-Win
ON GO OF contratos.id_tipo_contrato IN FRAME F-Main /* Tipo Contrato */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_tipo_contrato V-table-Win
ON LEAVE OF contratos.id_tipo_contrato IN FRAME F-Main /* Tipo Contrato */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_tipo_contrato V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_tipo_contrato IN FRAME F-Main /* Tipo Contrato */
do: 
define var r as rowid no-undo.
run wc_tipos_contratos.w(output r).
find tipos_contratos where rowid(tipos_contratos) = r no-lock no-error.
if available tipos_contratos then 
general.contratos.id_tipo_contrato:screen-value = string(tipos_contratos.id_tipo_contrato).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_tipo_contrato V-table-Win
ON U1 OF contratos.id_tipo_contrato IN FRAME F-Main /* Tipo Contrato */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.id_tipo_plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_tipo_plazo V-table-Win
ON GO OF contratos.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_tipo_plazo V-table-Win
ON LEAVE OF contratos.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_tipo_plazo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contratos.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
do: 
define var r as rowid no-undo.
run wc_tipos_plazo.w(output r).
find tipos_plazo where rowid(tipos_plazo) = r no-lock no-error.
if available tipos_plazo then 
general.contratos.id_tipo_plazo:screen-value = string(tipos_plazo.id_tipo_plazo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.id_tipo_plazo V-table-Win
ON U1 OF contratos.id_tipo_plazo IN FRAME F-Main /* Tipo Plazo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.orden_fabricacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.orden_fabricacion V-table-Win
ON LEAVE OF contratos.orden_fabricacion IN FRAME F-Main /* OF */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contratos.plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contratos.plazo V-table-Win
ON LEAVE OF contratos.plazo IN FRAME F-Main /* Plazo */
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
v_alta = 1.
assign contratos.c_usuario = userid("userdb")
       contratos.c_fecha   = today
       contratos.c_hora    = string(time,"HH:MM:SS").

/* run p_correo_aviso_contrato.p (input 1, input rowid(contratos), input "creado"). */
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
/* BORRO LOS ITEMS DE CONTRATO */
for each items_contrato where items_contrato.id_contrato = del_id_contrato
                          and items_contrato.id_tipo_contrato = del_id_tipo_contrato
                          and items_contrato.anio = del_anio.
    
    delete items_contratos.
end.
/* AUDITO LA BORRADA */
create baja_contrato.
assign baja_contrato.id_contrato        = del_id_contrato
       baja_contrato.id_tipo_contrato   = del_id_tipo_contrato
       baja_contrato.anio               = del_anio
       baja_contrato.c_usuario          = userid("userdb")
       baja_contrato.c_fecha            = today
       baja_contrato.c_hora             = string(time,"HH:MM:SS").
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


define var v1 like contratos.id_contrato.
define var v2 like contratos.id_tipo_contrato.
define var v3 like contratos.anio.

define var hcontainer as handle.
define var existe as logical.

run get-container (output hcontainer).
RUN get-rowid-browser in hcontainer (output v1, output v2, output v3, output r).

find contratos where rowid(contratos) = r no-lock no-error.
if available contratos then
    do:
        for each items_contratos where items_contratos.id_contrato      = v1
                                   and items_contratos.id_tipo_contrato = v2
                                   and items_contratos.anio             = v3.
            assign items_contratos.id_contrato = contratos.id_contrato
                   items_contratos.id_tipo_contrato = contratos.id_tipo_contrato
                   items_contratos.anio = contratos.anio.
        end.
    end.

/*    
if v_alta = 0 then run p_correo_aviso_contrato.p (input 1, input rowid(contratos), input "modificado").
else */ v_alta = 0.
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

run get-container (output hcontainer).
RUN get-rowid-browser-2 in hcontainer (output r).

find contratos where rowid(contratos) = r no-lock no-error.
if available contratos then
    do:
        del_id_contrato = contratos.id_contrato.
        del_id_tipo_contrato = contratos.id_tipo_contrato.
        del_anio = contratos.anio.
    end.
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
  {src/adm/template/row-list.i "contratos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "contratos"}

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
find first tipos_contratos where tipos_contratos.id_tipo_contrato = integer(contratos.id_tipo_contrato:screen-value in frame F-Main)  no-lock no-error .
if available tipos_contratos then 
fi-tipos_contratos-descripcion:screen-value in frame F-Main = string(tipos_contratos.descripcion).
else
fi-tipos_contratos-descripcion:screen-value in frame F-Main = ''.

find first contactos_industria where contactos_industria.id_contacto = integer(contratos.id_cliente_destino:screen-value in frame F-Main)  no-lock no-error .
if available contactos_industria then 
fi-contactos_industria-nombre:screen-value in frame F-Main = string(contactos_industria.nombre).
else
fi-contactos_industria-nombre:screen-value in frame F-Main = ''.

find first contactos_industria where contactos_industria.id_contacto = integer(contratos.id_documentacion:screen-value in frame F-Main)  no-lock no-error .
if available contactos_industria then 
fi-contactos_industria-nombre-1:screen-value in frame F-Main = string(contactos_industria.nombre).
else
fi-contactos_industria-nombre-1:screen-value in frame F-Main = ''.

find first instrumentos_pagos where instrumentos_pagos.id_instrumento_pago = integer(contratos.id_instrumento_pago:screen-value in frame F-Main)  no-lock no-error .
if available instrumentos_pagos then 
fi-instrumentos_pagos-descripci:screen-value in frame F-Main = string(instrumentos_pagos.descripcion_ingles).
else
fi-instrumentos_pagos-descripci:screen-value in frame F-Main = ''.

find first tipos_plazo where tipos_plazo.id_tipo_plazo = integer(contratos.id_tipo_plazo:screen-value in frame F-Main)  no-lock no-error .
if available tipos_plazo then 
fi-tipos_plazo-descripcion_ingl:screen-value in frame F-Main = string(tipos_plazo.descripcion_ingles).
else
fi-tipos_plazo-descripcion_ingl:screen-value in frame F-Main = ''.

find first bancos where bancos.id_banco = integer(contratos.id_banco:screen-value in frame F-Main)  no-lock no-error .
if available bancos then 
fi-bancos-nombre:screen-value in frame F-Main = string(bancos.nombre).
else
fi-bancos-nombre:screen-value in frame F-Main = ''.

find first contactos_industria where contactos_industria.id_contacto = integer(contratos.id_broker:screen-value in frame F-Main)  no-lock no-error .
if available contactos_industria then 
fi-contactos_industria-nombre-2:screen-value in frame F-Main = string(contactos_industria.nombre).
else
fi-contactos_industria-nombre-2:screen-value in frame F-Main = ''.

find first contactos_industria where contactos_industria.id_contacto = integer(contratos.id_consignee:screen-value in frame F-Main)  no-lock no-error .
if available contactos_industria then 
fi-contactos_industria-nombre-3:screen-value in frame F-Main = string(contactos_industria.nombre).
else
fi-contactos_industria-nombre-3:screen-value in frame F-Main = ''.

find first contactos_industria where contactos_industria.id_contacto = integer(contratos.id_notify:screen-value in frame F-Main)  no-lock no-error .
if available contactos_industria then 
fi-contactos_industria-nombre-4:screen-value in frame F-Main = string(contactos_industria.nombre).
else
fi-contactos_industria-nombre-4:screen-value in frame F-Main = ''.

find first clientes where clientes.id_cliente = integer(contratos.id_cliente:screen-value in frame F-Main)  no-lock no-error .
if available clientes then 
fi-clientes-razon_social:screen-value in frame F-Main = string(clientes.razon_social).
else
fi-clientes-razon_social:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_tipo_contrato,id_cliente_destino,id_documentacion,id_instrumento_pago,id_tipo_plazo,id_banco,id_broker,id_consignee,id_notify,id_cliente".
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
  {src/adm/template/sndkycas.i "id_tipo_contrato" "contratos" "id_tipo_contrato"}
  {src/adm/template/sndkycas.i "id_tipo_plazo" "contratos" "id_tipo_plazo"}

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
  {src/adm/template/snd-list.i "contratos"}

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


