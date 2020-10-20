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
DEFINE VAR v_id_prot AS INTEGER.
DEFINE VAR v_anio AS INTEGER.
DEFINE VAR v_id_arti AS INTEGER.

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
&Scoped-define EXTERNAL-TABLES protocolos
&Scoped-define FIRST-EXTERNAL-TABLE protocolos


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR protocolos.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS protocolos.id_protocolo protocolos.anio ~
protocolos.id_articulo protocolos.id_tipo_protocolo protocolos.code ~
protocolos.id_contramarca protocolos.fecha protocolos.id_color ~
protocolos.id_body protocolos.con_galones protocolos.id_smell_flavor ~
protocolos.con_code protocolos.con_pyg protocolos.id_quimico 
&Scoped-define ENABLED-TABLES protocolos
&Scoped-define FIRST-ENABLED-TABLE protocolos
&Scoped-Define ENABLED-OBJECTS RECT-23 
&Scoped-Define DISPLAYED-FIELDS protocolos.id_protocolo protocolos.anio ~
protocolos.id_articulo protocolos.id_tipo_protocolo protocolos.code ~
protocolos.id_contramarca protocolos.fecha protocolos.fecha_aprobacion ~
protocolos.id_color protocolos.id_body protocolos.con_galones ~
protocolos.id_smell_flavor protocolos.con_code protocolos.con_pyg ~
protocolos.id_quimico 
&Scoped-define DISPLAYED-TABLES protocolos
&Scoped-define FIRST-DISPLAYED-TABLE protocolos
&Scoped-Define DISPLAYED-OBJECTS fi-productos_terminados-descrip ~
fi-tipos_protocolos-descripcion fi-contramarcas-descripcion ~
fi-colores_prot-descripcion fi-cuerpo_prot-descripcion ~
fi-aromas_sabores_prot-descripc fi-quimicos-nombre_ingles 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS protocolos.id_protocolo protocolos.anio ~
protocolos.id_articulo 

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
DEFINE VARIABLE fi-aromas_sabores_prot-descripc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-colores_prot-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-contramarcas-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-cuerpo_prot-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-quimicos-nombre_ingles AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipos_protocolos-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 29.8 BY 3.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     protocolos.id_protocolo AT ROW 1 COL 13 COLON-ALIGNED
          LABEL "Protocolo"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     protocolos.anio AT ROW 1 COL 28 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     protocolos.id_articulo AT ROW 1 COL 45 COLON-ALIGNED
          LABEL "Producto" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-productos_terminados-descrip AT ROW 1 COL 52 COLON-ALIGNED NO-LABEL
     protocolos.id_tipo_protocolo AT ROW 1.95 COL 13 COLON-ALIGNED
          LABEL "Tipo Prot."
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     fi-tipos_protocolos-descripcion AT ROW 1.95 COL 18 COLON-ALIGNED NO-LABEL
     protocolos.code AT ROW 1.95 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     protocolos.id_contramarca AT ROW 2.91 COL 13 COLON-ALIGNED
          LABEL "Contramarca" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-contramarcas-descripcion AT ROW 2.91 COL 20 COLON-ALIGNED NO-LABEL
     protocolos.fecha AT ROW 3.86 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     protocolos.fecha_aprobacion AT ROW 3.86 COL 78 COLON-ALIGNED
          LABEL "FechaAprob"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     protocolos.id_color AT ROW 4.81 COL 13 COLON-ALIGNED
          LABEL "Color"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-colores_prot-descripcion AT ROW 4.81 COL 20 COLON-ALIGNED NO-LABEL
     protocolos.id_body AT ROW 5.76 COL 13 COLON-ALIGNED
          LABEL "Body"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-cuerpo_prot-descripcion AT ROW 5.76 COL 20 COLON-ALIGNED NO-LABEL
     protocolos.con_galones AT ROW 6.24 COL 85 COLON-ALIGNED
          LABEL "Galones"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     protocolos.id_smell_flavor AT ROW 6.71 COL 13 COLON-ALIGNED
          LABEL "Smell/Flavor"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-aromas_sabores_prot-descripc AT ROW 6.71 COL 20 COLON-ALIGNED NO-LABEL
     protocolos.con_code AT ROW 7.43 COL 71 COLON-ALIGNED
          LABEL "Code"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     protocolos.con_pyg AT ROW 7.43 COL 85 COLON-ALIGNED
          LABEL "P y G"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     protocolos.id_quimico AT ROW 7.67 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-quimicos-nombre_ingles AT ROW 7.67 COL 20 COLON-ALIGNED NO-LABEL
     RECT-23 AT ROW 5.05 COL 64
     "Para Reportes:" VIEW-AS TEXT
          SIZE 22 BY .71 AT ROW 5.29 COL 65
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.protocolos
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
         HEIGHT             = 7.91
         WIDTH              = 92.8.
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

/* SETTINGS FOR FILL-IN protocolos.anio IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN protocolos.con_code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN protocolos.con_galones IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN protocolos.con_pyg IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN protocolos.fecha_aprobacion IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fi-aromas_sabores_prot-descripc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-colores_prot-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-contramarcas-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cuerpo_prot-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-quimicos-nombre_ingles IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipos_protocolos-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN protocolos.id_articulo IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN protocolos.id_body IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN protocolos.id_color IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN protocolos.id_contramarca IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN protocolos.id_protocolo IN FRAME F-Main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN protocolos.id_smell_flavor IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN protocolos.id_tipo_protocolo IN FRAME F-Main
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
general.protocolos.id_color;wc_colores_prot.w;colores_prot.descripcion;;
general.protocolos.id_body;wc_cuerpo_prot.w;cuerpo_prot.descripcion;id_cuerpo;
general.protocolos.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
general.protocolos.id_contramarca;wc_contramarcas.w;contramarcas.descripcion;;
general.protocolos.id_quimico;wc_quimicos.w;quimicos.nombre_ingles;;
general.protocolos.id_tipo_protocolo;wc_tipos_protocolos.w;tipos_protocolos.descripcion;;
general.protocolos.id_smell_flavor;wc_aromas_sabores_prot.w;aromas_sabores_prot.descripcion;id_aroma_sabor;
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

&Scoped-define SELF-NAME protocolos.anio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.anio V-table-Win
ON LEAVE OF protocolos.anio IN FRAME F-Main /* Año */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.code V-table-Win
ON LEAVE OF protocolos.code IN FRAME F-Main /* Code */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.con_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.con_code V-table-Win
ON LEAVE OF protocolos.con_code IN FRAME F-Main /* Code */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.con_galones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.con_galones V-table-Win
ON LEAVE OF protocolos.con_galones IN FRAME F-Main /* Galones */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.con_pyg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.con_pyg V-table-Win
ON LEAVE OF protocolos.con_pyg IN FRAME F-Main /* P y G */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.fecha V-table-Win
ON LEAVE OF protocolos.fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.fecha_aprobacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.fecha_aprobacion V-table-Win
ON LEAVE OF protocolos.fecha_aprobacion IN FRAME F-Main /* FechaAprob */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-aromas_sabores_prot-descripc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-aromas_sabores_prot-descripc V-table-Win
ON LEAVE OF fi-aromas_sabores_prot-descripc IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-colores_prot-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-colores_prot-descripcion V-table-Win
ON LEAVE OF fi-colores_prot-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-contramarcas-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-contramarcas-descripcion V-table-Win
ON LEAVE OF fi-contramarcas-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cuerpo_prot-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cuerpo_prot-descripcion V-table-Win
ON LEAVE OF fi-cuerpo_prot-descripcion IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-quimicos-nombre_ingles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-quimicos-nombre_ingles V-table-Win
ON LEAVE OF fi-quimicos-nombre_ingles IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipos_protocolos-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipos_protocolos-descripcion V-table-Win
ON LEAVE OF fi-tipos_protocolos-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_articulo V-table-Win
ON GO OF protocolos.id_articulo IN FRAME F-Main /* Producto */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_articulo V-table-Win
ON LEAVE OF protocolos.id_articulo IN FRAME F-Main /* Producto */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF protocolos.id_articulo IN FRAME F-Main /* Producto */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.protocolos.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_articulo V-table-Win
ON U1 OF protocolos.id_articulo IN FRAME F-Main /* Producto */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.id_body
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_body V-table-Win
ON GO OF protocolos.id_body IN FRAME F-Main /* Body */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_body V-table-Win
ON LEAVE OF protocolos.id_body IN FRAME F-Main /* Body */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_body V-table-Win
ON MOUSE-SELECT-DBLCLICK OF protocolos.id_body IN FRAME F-Main /* Body */
do: 
define var r as rowid no-undo.
run wc_cuerpo_prot.w(output r).
find cuerpo_prot where rowid(cuerpo_prot) = r no-lock no-error.
if available cuerpo_prot then 
general.protocolos.id_body:screen-value = string(cuerpo_prot.id_cuerpo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_body V-table-Win
ON U1 OF protocolos.id_body IN FRAME F-Main /* Body */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.id_color
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_color V-table-Win
ON GO OF protocolos.id_color IN FRAME F-Main /* Color */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_color V-table-Win
ON LEAVE OF protocolos.id_color IN FRAME F-Main /* Color */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_color V-table-Win
ON MOUSE-SELECT-DBLCLICK OF protocolos.id_color IN FRAME F-Main /* Color */
do: 
define var r as rowid no-undo.
run wc_colores_prot.w(output r).
find colores_prot where rowid(colores_prot) = r no-lock no-error.
if available colores_prot then 
general.protocolos.id_color:screen-value = string(colores_prot.id_color).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_color V-table-Win
ON U1 OF protocolos.id_color IN FRAME F-Main /* Color */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.id_contramarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_contramarca V-table-Win
ON GO OF protocolos.id_contramarca IN FRAME F-Main /* Contramarca */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_contramarca V-table-Win
ON LEAVE OF protocolos.id_contramarca IN FRAME F-Main /* Contramarca */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_contramarca V-table-Win
ON MOUSE-SELECT-DBLCLICK OF protocolos.id_contramarca IN FRAME F-Main /* Contramarca */
do: 
define var r as rowid no-undo.
run wc_contramarcas.w(output r).
find contramarcas where rowid(contramarcas) = r no-lock no-error.
if available contramarcas then 
general.protocolos.id_contramarca:screen-value = string(contramarcas.id_contramarca).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_contramarca V-table-Win
ON U1 OF protocolos.id_contramarca IN FRAME F-Main /* Contramarca */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.id_protocolo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_protocolo V-table-Win
ON LEAVE OF protocolos.id_protocolo IN FRAME F-Main /* Protocolo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.id_quimico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_quimico V-table-Win
ON GO OF protocolos.id_quimico IN FRAME F-Main /* Quimico */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_quimico V-table-Win
ON LEAVE OF protocolos.id_quimico IN FRAME F-Main /* Quimico */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_quimico V-table-Win
ON MOUSE-SELECT-DBLCLICK OF protocolos.id_quimico IN FRAME F-Main /* Quimico */
do: 
define var r as rowid no-undo.
run wc_quimicos.w(output r).
find quimicos where rowid(quimicos) = r no-lock no-error.
if available quimicos then 
general.protocolos.id_quimico:screen-value = string(quimicos.id_quimico).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_quimico V-table-Win
ON U1 OF protocolos.id_quimico IN FRAME F-Main /* Quimico */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.id_smell_flavor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_smell_flavor V-table-Win
ON GO OF protocolos.id_smell_flavor IN FRAME F-Main /* Smell/Flavor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_smell_flavor V-table-Win
ON LEAVE OF protocolos.id_smell_flavor IN FRAME F-Main /* Smell/Flavor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_smell_flavor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF protocolos.id_smell_flavor IN FRAME F-Main /* Smell/Flavor */
do: 
define var r as rowid no-undo.
run wc_aromas_sabores_prot.w(output r).
find aromas_sabores_prot where rowid(aromas_sabores_prot) = r no-lock no-error.
if available aromas_sabores_prot then 
general.protocolos.id_smell_flavor:screen-value = string(aromas_sabores_prot.id_aroma_sabor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_smell_flavor V-table-Win
ON U1 OF protocolos.id_smell_flavor IN FRAME F-Main /* Smell/Flavor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME protocolos.id_tipo_protocolo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_tipo_protocolo V-table-Win
ON GO OF protocolos.id_tipo_protocolo IN FRAME F-Main /* Tipo Prot. */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_tipo_protocolo V-table-Win
ON LEAVE OF protocolos.id_tipo_protocolo IN FRAME F-Main /* Tipo Prot. */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_tipo_protocolo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF protocolos.id_tipo_protocolo IN FRAME F-Main /* Tipo Prot. */
do: 
define var r as rowid no-undo.
run wc_tipos_protocolos.w(output r).
find tipos_protocolos where rowid(tipos_protocolos) = r no-lock no-error.
if available tipos_protocolos then 
general.protocolos.id_tipo_protocolo:screen-value = string(tipos_protocolos.id_tipo_protocolo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL protocolos.id_tipo_protocolo V-table-Win
ON U1 OF protocolos.id_tipo_protocolo IN FRAME F-Main /* Tipo Prot. */
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

ASSIGN protocolos.c_usuario         = USERID("userdb")
       protocolos.c_fecha           = TODAY
       protocolos.c_hora            = STRING(TIME,"HH:MM:SS").

RUN creo_items_protocolos.
  

RUN get-container (OUTPUT hcon).
RUN actualiza_items_protocolos IN hcon.
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
FOR EACH items_protocolos WHERE items_protocolos.id_protocolo = v_id_prot
                            AND items_protocolos.anio         = v_anio
                            AND items_protocolos.id_articulo  = v_id_arti.
    DELETE items_protocolos.
END.
FOR EACH r_muestras_protocolos WHERE r_muestras_protocolos.id_protocolo = v_id_prot
                                 AND r_muestras_protocolos.anio         = v_anio
                                 AND r_muestras_protocolos.id_articulo  = v_id_arti.
    DELETE r_muestras_protocolos.
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
v_id_prot = protocolos.id_protocolo.
v_anio    = protocolos.anio.
v_id_arti = protocolos.id_articulo.
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
  {src/adm/template/row-list.i "protocolos"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "protocolos"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creo_items_protocolos V-table-Win 
PROCEDURE creo_items_protocolos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH r_caracteristicas_articulos WHERE r_caracteristicas_articulo.id_articulo = 
                                           INTEGER(protocolos.id_articulo:SCREEN-VALUE IN FRAME F-Main)
                                       AND r_caracteristicas_articulo.id_tipo_protocolo = 
                                           INTEGER(protocolos.id_tipo_protocolo:SCREEN-VALUE IN FRAME F-Main)
                                       NO-LOCK.
    
    CREATE items_protocolos.
    ASSIGN items_protocolos.id_protocolo       = INTEGER(protocolos.id_protocolo:SCREEN-VALUE IN FRAME F-Main)
           items_protocolos.anio               = INTEGER(protocolos.anio:SCREEN-VALUE IN FRAME F-Main)
           items_protocolos.id_articulo        = INTEGER(protocolos.id_articulo:SCREEN-VALUE IN FRAME F-Main)
           items_protocolos.id_tipo_protocolo  = protocolos.id_tipo_protocolo
           items_protocolos.id_caracteristica  = r_caracteristicas_articulos.id_caracteristica
           items_protocolos.orden              = r_caracteristicas_articulos.orden
           items_protocolos.valor_caracter     = ""
           items_protocolos.c_usuario          = USERID("userdb")
           items_protocolos.c_fecha            = TODAY
           items_protocolos.c_hora             = STRING(TIME,"HH:MM:SS").

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos V-table-Win 
PROCEDURE descriptivos :
find first colores_prot where colores_prot.id_color = integer(protocolos.id_color:screen-value in frame F-Main)  no-lock no-error .
if available colores_prot then 
fi-colores_prot-descripcion:screen-value in frame F-Main = string(colores_prot.descripcion).
else
fi-colores_prot-descripcion:screen-value in frame F-Main = ''.

find first cuerpo_prot where cuerpo_prot.id_cuerpo = integer(protocolos.id_body:screen-value in frame F-Main)  no-lock no-error .
if available cuerpo_prot then 
fi-cuerpo_prot-descripcion:screen-value in frame F-Main = string(cuerpo_prot.descripcion).
else
fi-cuerpo_prot-descripcion:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(protocolos.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first contramarcas where contramarcas.id_contramarca = integer(protocolos.id_contramarca:screen-value in frame F-Main)  no-lock no-error .
if available contramarcas then 
fi-contramarcas-descripcion:screen-value in frame F-Main = string(contramarcas.descripcion).
else
fi-contramarcas-descripcion:screen-value in frame F-Main = ''.

find first quimicos where quimicos.id_quimico = integer(protocolos.id_quimico:screen-value in frame F-Main)  no-lock no-error .
if available quimicos then 
fi-quimicos-nombre_ingles:screen-value in frame F-Main = string(quimicos.nombre_ingles).
else
fi-quimicos-nombre_ingles:screen-value in frame F-Main = ''.

find first tipos_protocolos where tipos_protocolos.id_tipo_protocolo = integer(protocolos.id_tipo_protocolo:screen-value in frame F-Main)  no-lock no-error .
if available tipos_protocolos then 
fi-tipos_protocolos-descripcion:screen-value in frame F-Main = string(tipos_protocolos.descripcion).
else
fi-tipos_protocolos-descripcion:screen-value in frame F-Main = ''.

find first aromas_sabores_prot where aromas_sabores_prot.id_aroma_sabor = integer(protocolos.id_smell_flavor:screen-value in frame F-Main)  no-lock no-error .
if available aromas_sabores_prot then 
fi-aromas_sabores_prot-descripc:screen-value in frame F-Main = string(aromas_sabores_prot.descripcion).
else
fi-aromas_sabores_prot-descripc:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_color,id_body,id_articulo,id_contramarca,id_quimico,id_tipo_protocolo,id_smell_flavor".
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
  {src/adm/template/snd-list.i "protocolos"}

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

