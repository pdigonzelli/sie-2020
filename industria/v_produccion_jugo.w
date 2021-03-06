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

define var mi-alta as logical initial false.


DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES produccion_jugo
&Scoped-define FIRST-EXTERNAL-TABLE produccion_jugo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR produccion_jugo.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS produccion_jugo.id_empresa ~
produccion_jugo.id_sucursal produccion_jugo.Fecha ~
produccion_jugo.id_produccion produccion_jugo.litros ~
produccion_jugo.id_articulo produccion_jugo.id_calidad ~
produccion_jugo.id_envase_1 produccion_jugo.id_envase_2 ~
produccion_jugo.Bx_20_20 produccion_jugo.Bx_correg ~
produccion_jugo.Acidez_w_w produccion_jugo.Acidez_w_v produccion_jugo.Pulpa ~
produccion_jugo.Sodio produccion_jugo.cantidad_1 produccion_jugo.kilos_1 ~
produccion_jugo.cantidad_2 produccion_jugo.kilos_2 produccion_jugo.abs520 ~
produccion_jugo.abs_8_430 produccion_jugo.benzoato ~
produccion_jugo.bisulfito produccion_jugo.nitrogeno ~
produccion_jugo.pulpa_85 produccion_jugo.ratio produccion_jugo.t_600 ~
produccion_jugo.vitaminac produccion_jugo.pesticida 
&Scoped-define ENABLED-TABLES produccion_jugo
&Scoped-define FIRST-ENABLED-TABLE produccion_jugo
&Scoped-Define DISPLAYED-FIELDS produccion_jugo.id_empresa ~
produccion_jugo.id_sucursal produccion_jugo.Fecha ~
produccion_jugo.id_produccion produccion_jugo.litros ~
produccion_jugo.id_articulo produccion_jugo.id_calidad ~
produccion_jugo.id_envase_1 produccion_jugo.id_envase_2 ~
produccion_jugo.Bx_20_20 produccion_jugo.Bx_correg ~
produccion_jugo.Acidez_w_w produccion_jugo.Acidez_w_v produccion_jugo.Pulpa ~
produccion_jugo.Sodio produccion_jugo.cantidad_1 produccion_jugo.kilos_1 ~
produccion_jugo.cantidad_2 produccion_jugo.kilos_2 produccion_jugo.abs520 ~
produccion_jugo.abs_8_430 produccion_jugo.benzoato ~
produccion_jugo.bisulfito produccion_jugo.nitrogeno ~
produccion_jugo.pulpa_85 produccion_jugo.ratio produccion_jugo.t_600 ~
produccion_jugo.vitaminac produccion_jugo.pesticida 
&Scoped-define DISPLAYED-TABLES produccion_jugo
&Scoped-define FIRST-DISPLAYED-TABLE produccion_jugo
&Scoped-Define DISPLAYED-OBJECTS fi-empresas-razon_social ~
fi-sucursales-nombre fi-productos_terminados-descrip ~
fi-calidades-descripcion fi-envases_prod-descripcion ~
fi-envases_prod-descripcion-1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS produccion_jugo.id_empresa ~
produccion_jugo.id_sucursal produccion_jugo.id_produccion 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_tipotambor||y|general.produccion_jugo.id_tipotambor
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
DEFINE VARIABLE fi-calidades-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-empresas-razon_social AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-envases_prod-descripcion-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-sucursales-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     produccion_jugo.id_empresa AT ROW 1 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     produccion_jugo.id_sucursal AT ROW 2 COL 15 COLON-ALIGNED
          LABEL "Sucursal"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     produccion_jugo.Fecha AT ROW 2.91 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.id_produccion AT ROW 3.95 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     produccion_jugo.litros AT ROW 3.86 COL 35 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.id_articulo AT ROW 5.05 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     produccion_jugo.id_calidad AT ROW 6.1 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     produccion_jugo.id_envase_1 AT ROW 7.19 COL 15 COLON-ALIGNED
          LABEL "Envase 1"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     produccion_jugo.id_envase_2 AT ROW 8.14 COL 15 COLON-ALIGNED
          LABEL "Envase 2"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-empresas-razon_social AT ROW 1 COL 20 COLON-ALIGNED NO-LABEL
     produccion_jugo.Bx_20_20 AT ROW 1 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi-sucursales-nombre AT ROW 1.95 COL 21 COLON-ALIGNED NO-LABEL
     produccion_jugo.Bx_correg AT ROW 2.1 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.Acidez_w_w AT ROW 3.1 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.Acidez_w_v AT ROW 4.1 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi-productos_terminados-descrip AT ROW 5.05 COL 24 COLON-ALIGNED NO-LABEL
     produccion_jugo.Pulpa AT ROW 5.14 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi-calidades-descripcion AT ROW 6.1 COL 23 COLON-ALIGNED NO-LABEL
     produccion_jugo.Sodio AT ROW 6.14 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi-envases_prod-descripcion AT ROW 7.19 COL 22 COLON-ALIGNED NO-LABEL
     produccion_jugo.cantidad_1 AT ROW 7.19 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     produccion_jugo.kilos_1 AT ROW 7.19 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     fi-envases_prod-descripcion-1 AT ROW 8.14 COL 22 COLON-ALIGNED NO-LABEL
     produccion_jugo.cantidad_2 AT ROW 8.14 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     produccion_jugo.kilos_2 AT ROW 8.14 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     produccion_jugo.abs520 AT ROW 1 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.abs_8_430 AT ROW 2 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.benzoato AT ROW 3 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.bisulfito AT ROW 4 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.nitrogeno AT ROW 8.14 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     produccion_jugo.pulpa_85 AT ROW 5.05 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.ratio AT ROW 6.05 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.t_600 AT ROW 7.05 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.vitaminac AT ROW 7.19 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.pesticida AT ROW 8.14 COL 132 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.produccion_jugo
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
         HEIGHT             = 8.14
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-calidades-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-empresas-razon_social IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sucursales-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN produccion_jugo.id_empresa IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN produccion_jugo.id_envase_1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN produccion_jugo.id_envase_2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN produccion_jugo.id_produccion IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN produccion_jugo.id_sucursal IN FRAME F-Main
   1 EXP-LABEL                                                          */
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
general.produccion_jugo.id_articulo;wc_productos_terminados.w;productos_terminados.descripcion;;
general.produccion_jugo.id_envase_1;wc_envases.w;envases_prod.descripcion;id_envase;
general.produccion_jugo.id_calidad;wc_calidades.w;calidades.descripcion;;
general.produccion_jugo.id_envase_2;wc_envases.w;envases_prod.descripcion;id_envase;
general.produccion_jugo.id_sucursal;wc_sucursales.w;sucursales.nombre;;
general.produccion_jugo.id_empresa;ec_empreas.w;empresas.razon_social;;
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

&Scoped-define SELF-NAME produccion_jugo.abs520
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.abs520 V-table-Win
ON LEAVE OF produccion_jugo.abs520 IN FRAME F-Main /* Abs520 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.abs_8_430
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.abs_8_430 V-table-Win
ON LEAVE OF produccion_jugo.abs_8_430 IN FRAME F-Main /* Abs_8_430 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.Acidez_w_v
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.Acidez_w_v V-table-Win
ON LEAVE OF produccion_jugo.Acidez_w_v IN FRAME F-Main /* Acidez_p/v */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.Acidez_w_w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.Acidez_w_w V-table-Win
ON LEAVE OF produccion_jugo.Acidez_w_w IN FRAME F-Main /* Acidez_p/p */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.benzoato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.benzoato V-table-Win
ON LEAVE OF produccion_jugo.benzoato IN FRAME F-Main /* benzoato */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.bisulfito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.bisulfito V-table-Win
ON LEAVE OF produccion_jugo.bisulfito IN FRAME F-Main /* bisulfito */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.Bx_20_20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.Bx_20_20 V-table-Win
ON LEAVE OF produccion_jugo.Bx_20_20 IN FRAME F-Main /* Bx_20/20 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.Bx_correg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.Bx_correg V-table-Win
ON LEAVE OF produccion_jugo.Bx_correg IN FRAME F-Main /* Bx correg */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.cantidad_1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.cantidad_1 V-table-Win
ON LEAVE OF produccion_jugo.cantidad_1 IN FRAME F-Main /* Cantidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.cantidad_2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.cantidad_2 V-table-Win
ON LEAVE OF produccion_jugo.cantidad_2 IN FRAME F-Main /* Cantidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.Fecha V-table-Win
ON LEAVE OF produccion_jugo.Fecha IN FRAME F-Main /* Fecha */
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


&Scoped-define SELF-NAME fi-envases_prod-descripcion-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-envases_prod-descripcion-1 V-table-Win
ON LEAVE OF fi-envases_prod-descripcion-1 IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-sucursales-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sucursales-nombre V-table-Win
ON LEAVE OF fi-sucursales-nombre IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_articulo V-table-Win
ON GO OF produccion_jugo.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_articulo V-table-Win
ON LEAVE OF produccion_jugo.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF produccion_jugo.id_articulo IN FRAME F-Main /* Art�culo */
do: 
define var r as rowid no-undo.
run wc_productos_terminados.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.produccion_jugo.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_articulo V-table-Win
ON U1 OF produccion_jugo.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_calidad V-table-Win
ON GO OF produccion_jugo.id_calidad IN FRAME F-Main /* Cod.Calidad */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_calidad V-table-Win
ON LEAVE OF produccion_jugo.id_calidad IN FRAME F-Main /* Cod.Calidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_calidad V-table-Win
ON MOUSE-SELECT-DBLCLICK OF produccion_jugo.id_calidad IN FRAME F-Main /* Cod.Calidad */
do: 
define var r as rowid no-undo.
run wc_calidades.w(output r).
find calidades where rowid(calidades) = r no-lock no-error.
if available calidades then 
general.produccion_jugo.id_calidad:screen-value = string(calidades.id_calidad).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_calidad V-table-Win
ON U1 OF produccion_jugo.id_calidad IN FRAME F-Main /* Cod.Calidad */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_empresa V-table-Win
ON GO OF produccion_jugo.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_empresa V-table-Win
ON LEAVE OF produccion_jugo.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF produccion_jugo.id_empresa IN FRAME F-Main /* Empresa */
do: 
define var r as rowid no-undo.
run ec_empreas.w(output r).
find empresas where rowid(empresas) = r no-lock no-error.
if available empresas then 
general.produccion_jugo.id_empresa:screen-value = string(empresas.id_empresa).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_empresa V-table-Win
ON U1 OF produccion_jugo.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_envase_1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_1 V-table-Win
ON GO OF produccion_jugo.id_envase_1 IN FRAME F-Main /* Envase 1 */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_1 V-table-Win
ON LEAVE OF produccion_jugo.id_envase_1 IN FRAME F-Main /* Envase 1 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_1 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF produccion_jugo.id_envase_1 IN FRAME F-Main /* Envase 1 */
do: 
define var r as rowid no-undo.
run wc_envases.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
general.produccion_jugo.id_envase_1:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_1 V-table-Win
ON U1 OF produccion_jugo.id_envase_1 IN FRAME F-Main /* Envase 1 */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_envase_2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_2 V-table-Win
ON GO OF produccion_jugo.id_envase_2 IN FRAME F-Main /* Envase 2 */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_2 V-table-Win
ON LEAVE OF produccion_jugo.id_envase_2 IN FRAME F-Main /* Envase 2 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_2 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF produccion_jugo.id_envase_2 IN FRAME F-Main /* Envase 2 */
do: 
define var r as rowid no-undo.
run wc_envases.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
general.produccion_jugo.id_envase_2:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_2 V-table-Win
ON U1 OF produccion_jugo.id_envase_2 IN FRAME F-Main /* Envase 2 */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_produccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_produccion V-table-Win
ON LEAVE OF produccion_jugo.id_produccion IN FRAME F-Main /* Produccion */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_sucursal V-table-Win
ON GO OF produccion_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_sucursal V-table-Win
ON LEAVE OF produccion_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_sucursal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF produccion_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
do: 
define var r as rowid no-undo.
run wc_sucursales.w(output r).
find sucursales where rowid(sucursales) = r no-lock no-error.
if available sucursales then 
general.produccion_jugo.id_sucursal:screen-value = string(sucursales.id_sucursal).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_sucursal V-table-Win
ON U1 OF produccion_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.kilos_1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.kilos_1 V-table-Win
ON LEAVE OF produccion_jugo.kilos_1 IN FRAME F-Main /* Kilos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.kilos_2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.kilos_2 V-table-Win
ON LEAVE OF produccion_jugo.kilos_2 IN FRAME F-Main /* Kilos */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.litros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.litros V-table-Win
ON LEAVE OF produccion_jugo.litros IN FRAME F-Main /* Litros */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.nitrogeno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.nitrogeno V-table-Win
ON LEAVE OF produccion_jugo.nitrogeno IN FRAME F-Main /* Nitrogeno */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.pesticida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.pesticida V-table-Win
ON LEAVE OF produccion_jugo.pesticida IN FRAME F-Main /* Pesticida */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.Pulpa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.Pulpa V-table-Win
ON LEAVE OF produccion_jugo.Pulpa IN FRAME F-Main /* Pulpa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.pulpa_85
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.pulpa_85 V-table-Win
ON LEAVE OF produccion_jugo.pulpa_85 IN FRAME F-Main /* Pulpa_8.5 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.ratio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.ratio V-table-Win
ON LEAVE OF produccion_jugo.ratio IN FRAME F-Main /* Ratio */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.Sodio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.Sodio V-table-Win
ON LEAVE OF produccion_jugo.Sodio IN FRAME F-Main /* Na */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.t_600
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.t_600 V-table-Win
ON LEAVE OF produccion_jugo.t_600 IN FRAME F-Main /* T%600 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.vitaminac
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.vitaminac V-table-Win
ON LEAVE OF produccion_jugo.vitaminac IN FRAME F-Main /* VitaminaC */
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



/**********EMPIEZA-TIPO-DETALLE*********/
  run set-attribute-list ('tipo-detalle=cabecera'). 
/**********TERMINA-TIPO-DETALLE*********/

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
DEFINE BUFFER bb_prod FOR produccion_jugo.
FIND FIRST bb_prod WHERE bb_prod.id_sucursal    = INTEGER(produccion_jugo.id_sucursal:SCREEN-VALUE IN FRAME F-Main)
                     AND bb_prod.id_produccion  = INTEGER(produccion_jugo.id_produccion:SCREEN-VALUE IN FRAME F-Main)
                     AND bb_prod.id_articulo    = INTEGER(produccion_jugo.id_articulo:SCREEN-VALUE IN FRAME F-Main)
                     AND bb_prod.anio           = YEAR(DATE(produccion_jugo.fecha:SCREEN-VALUE IN FRAME F-Main))
                    NO-LOCK NO-ERROR.
IF AVAILABLE bb_prod THEN DO:
    MESSAGE "Ya existe la produccion " bb_prod.id_produccion
            " de la sucursal " bb_prod.id_sucursal
            " del a�o " YEAR(bb_prod.fecha)
            " con articulo " bb_prod.id_articulo
            " . Por favor cargue una produccion nueva!" VIEW-AS ALERT-BOX.
    RETURN "ADM-ERROR".
END.
ELSE DO:
    assign produccion_jugo.id_tipotambor = 1
           produccion_jugo.nromov        = next-value(nromov)
           produccion_jugo.c_usuario     = userid("userdb")
           produccion_jugo.c_fecha       = today
           produccion_jugo.c_hora        = string(time,"HH:MM:SS")
           produccion_jugo.anio          = year(date(produccion_jugo.fecha:screen-value in frame F-Main)).
    
    /*************SE CREAN LOS TAMBORES******************************/
    define var r as rowid.
    define var i as integer.
    define var j as integer initial 1.
    define var n as integer.
    DEFINE VAR v_tam AS INTEGER.
    
    n = integer(produccion_jugo.cantidad_1:screen-value in frame {&FRAME-NAME}).
    do i = 1 to n:
          /* message produccion_jugo.id_tipotambor produccion_jugo.nromov view-as alert-box. */
          create tambores_industria.
          assign tambores_industria.id_empresa           = integer(produccion_jugo.id_empresa:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_sucursal          = integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_tipotambor        = produccion_jugo.id_tipotambor
                 tambores_industria.nromov               = produccion_jugo.nromov
                 tambores_industria.id_lote              = integer(produccion_jugo.id_produccion:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_tambor            = j   /*ultimo*/
                 tambores_industria.fecha                = date(produccion_jugo.fecha:screen-value in frame {&FRAME-NAME})
                 tambores_industria.anio                 = YEAR(date(produccion_jugo.fecha:screen-value in frame {&FRAME-NAME}))
                 tambores_industria.id_envase            = integer(produccion_jugo.id_envase_1:screen-value in frame {&FRAME-NAME}) 
                 tambores_industria.kilos_tambor         = integer(produccion_jugo.kilos_1:screen-value in frame {&FRAME-NAME}) 
                 tambores_industria.id_articulo          = integer(produccion_jugo.id_articulo:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_calidad           = integer(produccion_jugo.id_calidad:screen-value in frame {&FRAME-NAME}).
                 if integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME}) = 96 then
                       assign tambores_industria.id_etiqueta = next-value(tambores).
                 else
                       assign tambores_industria.id_etiqueta = next-value(tambores_famailla).
          assign tambores_industria.c_usuario            = userid("userdb")
                 tambores_industria.c_fecha              = today
                 tambores_industria.c_hora               = string(time,"HH:MM:SS")
                 tambores_industria.id_empresa_ubicacion = 1
                 tambores_industria.id_sucursal_ubicacion = integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_locacion_ubicacion = 4
                 tambores_industria.id_posicion_ubicacion = 1
                 tambores_industria.id_estado             = 5.
                 
         j = j + 1.
    end.
    
    IF n > 0 THEN DO:
        RUN y_gstkcre.p (input integer(produccion_jugo.id_empresa:screen-value in frame {&FRAME-NAME}),
                         input integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME}),
                         input produccion_jugo.id_tipotambor,
                         input produccion_jugo.nromov,
                         INPUT 1,
                         INPUT n,
                         input 1) "produccion_jugo".
        
        IF return-value <> "" then do:
            message "Error en el procesamiento de movimientos de stock" view-as alert-box.
            RETURN "error".
        END.

        /*mail a deposito*/
        RUN mailingInsumos IN hLib (produccion_jugo.id_empresa, 
                                    integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME}), 
                                    produccion_jugo.id_tipotambor, 
                                    produccion_jugo.nromov).
    END.
    
    /*  LO MISMO A LO ANTERIOR PERO PARA EL ENVASE 2  */
    v_tam = j.
    n = integer(produccion_jugo.cantidad_2:screen-value in frame {&FRAME-NAME}).
    do i = 1 to n:
          /* message produccion_jugo.id_tipotambor produccion_jugo.nromov view-as alert-box. */
          create tambores_industria.
          assign tambores_industria.id_empresa               = integer(produccion_jugo.id_empresa:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_sucursal              = integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_tipotambor            = produccion_jugo.id_tipotambor
                 tambores_industria.nromov                   = produccion_jugo.nromov
                 tambores_industria.id_lote                  = integer(produccion_jugo.id_produccion:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_tambor                = j   /*ultimo*/
                 tambores_industria.fecha                    = date(produccion_jugo.fecha:screen-value in frame {&FRAME-NAME})
                 tambores_industria.anio                     = YEAR(date(produccion_jugo.fecha:screen-value in frame {&FRAME-NAME}))
                 tambores_industria.id_envase                = integer(produccion_jugo.id_envase_2:screen-value in frame {&FRAME-NAME}) 
                 tambores_industria.kilos_tambor             = integer(produccion_jugo.kilos_2:screen-value in frame {&FRAME-NAME})              
                 tambores_industria.id_articulo              = integer(produccion_jugo.id_articulo:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_calidad               = integer(produccion_jugo.id_calidad:screen-value in frame {&FRAME-NAME}).
                 if integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME}) = 96 then
                       assign tambores_industria.id_etiqueta = next-value(tambores).
                 else
                       assign tambores_industria.id_etiqueta = next-value(tambores_famailla).
          assign tambores_industria.c_usuario                = userid("userdb")
                 tambores_industria.c_fecha                  = today
                 tambores_industria.c_hora                   = string(time,"HH:MM:SS")
                 tambores_industria.id_empresa_ubicacion     = 1
                 tambores_industria.id_sucursal_ubicacion    = integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME})
                 tambores_industria.id_locacion_ubicacion    = 4
                 tambores_industria.id_posicion_ubicacion    = 1
                 tambores_industria.id_estado                = 5.
                 
         j = j + 1.
    end.
    
    IF n > 0 THEN DO:
        RUN y_gstkcre.p (input integer(produccion_jugo.id_empresa:screen-value in frame {&FRAME-NAME}),
                         input integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME}),
                         input produccion_jugo.id_tipotambor,
                         input produccion_jugo.nromov,
                         INPUT v_tam,
                         INPUT j - 1,
                         input 1) "produccion_jugo".
        
        IF return-value <> "" then do:
            message "Error en el procesamiento de movimientos de stock" view-as alert-box.
            RETURN "error".
        END.

          /*mail a deposito*/
        RUN mailingInsumos IN hLib (produccion_jugo.id_empresa, 
                                    integer(produccion_jugo.id_sucursal:screen-value in frame {&FRAME-NAME}), 
                                    produccion_jugo.id_tipotambor, 
                                    produccion_jugo.nromov).
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
for each tambores_industria where tambores_industria.id_empresa = del_empresa and
                                  tambores_industria.id_sucursal = del_sucursal and
                                  tambores_industria.id_tipotambor = del_tipotambor and
                                  tambores_industria.nromov = del_nromov.
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
if integer(kilos_1:screen-value in frame F-Main) <= 0 and
   integer(kilos_2:screen-value in frame F-Main) <= 0 then
do:
    message "Debe cargar el peso de los tambores." view-as alert-box.
    run dispatch ("cancel-record").
end.

produccion_jugo.id_envase_1:SCREEN-VALUE IN FRAME F-Main = "555".
produccion_jugo.id_envase_2:SCREEN-VALUE IN FRAME F-Main = "556".

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

define var sob_empresa as integer.
define var sob_sucursal as integer.
define var sob_tipotambor as integer.
define var sob_nromov as integer.
DEFINE VAR desde AS INTEGER.
DEFINE VAR hasta AS INTEGER.
DEFINE BUFFER b_tam FOR tambores_industria.


/******************************************************************************/
/*************************ALMACENO LOS ID DEL REGISTRO A BORRAR****************/
run get-container (output hcontainer).
RUN get-rowid-produccion in hcontainer (output r).

for each produccion_jugo where rowid(produccion_jugo) = r.
    del_empresa     = produccion_jugo.id_empresa.
    del_sucursal    = produccion_jugo.id_sucursal.
    del_tipotambor  = produccion_jugo.id_tipotambor.
    del_nromov      = produccion_jugo.nromov.  
      
end.

hasta = 0.
FOR EACH b_tam WHERE b_tam.id_empresa        = del_empresa and
                     b_tam.id_sucursal       = del_sucursal and
                     b_tam.id_tipotambor     = del_tipotambor and
                     b_tam.nromov            = del_nromov NO-LOCK.

    hasta = hasta + 1.
END.

RUN y_gstkcre.p (input del_empresa,
                 input del_sucursal,
                 input del_tipotambor,
                 input del_nromov,
                 INPUT 1,
                 INPUT hasta,
                 input 2) "produccion_jugo".

IF return-value <> "" then do:
    message "Error en el procesamiento de movimientos de stock" view-as alert-box.
    RETURN "ADM-ERROR".
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
  {src/adm/template/row-list.i "produccion_jugo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "produccion_jugo"}

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
find first productos_terminados where productos_terminados.id_articulo = integer(produccion_jugo.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first envases_prod where envases_prod.id_envase = integer(produccion_jugo.id_envase_1:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion:screen-value in frame F-Main = ''.

find first calidades where calidades.id_calidad = integer(produccion_jugo.id_calidad:screen-value in frame F-Main)  no-lock no-error .
if available calidades then 
fi-calidades-descripcion:screen-value in frame F-Main = string(calidades.descripcion).
else
fi-calidades-descripcion:screen-value in frame F-Main = ''.

find first envases_prod where envases_prod.id_envase = integer(produccion_jugo.id_envase_2:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion-1:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion-1:screen-value in frame F-Main = ''.

find first sucursales where sucursales.id_sucursal = integer(produccion_jugo.id_sucursal:screen-value in frame F-Main)  no-lock no-error .
if available sucursales then 
fi-sucursales-nombre:screen-value in frame F-Main = string(sucursales.nombre).
else
fi-sucursales-nombre:screen-value in frame F-Main = ''.

find first empresas where empresas.id_empresa = integer(produccion_jugo.id_empresa:screen-value in frame F-Main)  no-lock no-error .
if available empresas then 
fi-empresas-razon_social:screen-value in frame F-Main = string(empresas.razon_social).
else
fi-empresas-razon_social:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_articulo,id_envase_1,id_calidad,id_envase_2,id_sucursal,id_empresa".
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
  
  /*RUN set-attribute-list('tipo-validacion=FRAME').*/
  
  /* Code placed here will execute AFTER standard behavior.    */
  run habilitar_relacion.

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-add V-table-Win 
PROCEDURE post-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
mi-alta = false.
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
mi-alta = true.
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
  {src/adm/template/sndkycas.i "id_tipotambor" "produccion_jugo" "id_tipotambor"}

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
  {src/adm/template/snd-list.i "produccion_jugo"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-articulo V-table-Win 
PROCEDURE valida-articulo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*if not mi-alta then
do.
find productos_terminados where productos_terminados.id_articulo = 
                                integer(produccion_jugo.id_articulo:screen-value in frame {&FRAME-NAME}) no-lock no-error.
         if not available productos_terminados then 
            do:
                message "Debe ingresar un Producto terminado v�lido!".
                return "ADM-ERROR".
            end.
end.*/
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
    /*by facundo 08/09/2004*/
    WHEN "id_envase_1" THEN DO:
      IF INTEGER(valor) = 0 THEN DO:
        mensaje = "No se puede continuar si no se carga el envase".
        RETURN FALSE.
      END.
    END.
    WHEN "id_calidad" THEN DO:
      IF INTEGER(valor) = 0 THEN DO:
        mensaje = "no se puede continuar la operacion sin un valor correcto para el campo calidad.".
        RETURN FALSE.
      END.
      ELSE DO:
        FIND FIRST calidades WHERE calidades.id_calidad = INTEGER(valor) 
                             NO-LOCK NO-ERROR.
        IF NOT AVAILABLE calidades THEN DO:
          mensaje = "no se puede continuar la operacion sin un valor correcto para el campo calidad.".
          RETURN FALSE.
        END.
      END.
      
    END.
    WHEN "id_produccion" THEN DO:      
      DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
      DEFINE BUFFER prod FOR produccion_jugo.

      FIND FIRST prod WHERE prod.id_produccion = INTEGER(valor)
                        AND prod.id_sucursal   = INTEGER(produccion_jugo.id_sucursal:SCREEN-VALUE IN FRAME F-Main)
                        AND prod.id_articulo   = INTEGER(produccion_jugo.id_articulo:SCREEN-VALUE IN FRAME F-Main) 
                        AND YEAR(prod.fecha)   = YEAR(DATE(produccion_jugo.fecha:SCREEN-VALUE IN FRAME F-Main))
                        AND prod.fecha        >= DATE("01/01/2002")
                        AND prod.id_tipotambor = 1
                      NO-LOCK NO-ERROR.
      IF AVAILABLE prod THEN DO:
        cMes = "Produccion Existente " + CHR(10) +
               "  Nro. de Produccion: " + valor + CHR(10) + 
               "  Articulo: " + produccion_jugo.id_articulo:SCREEN-VALUE IN FRAME F-Main + CHR(10) + 
               "  Sucursal: " + produccion_jugo.id_sucursal:SCREEN-VALUE IN FRAME F-Main + CHR(10) + 
               "  Fecha: " + STRING(prod.fecha, "99/99/9999") + CHR(10) + CHR(10) +
               "Creada por el usuario " + prod.c_usuario + " el " + STRING(prod.fecha, "99/99/9999") + " a las " + STRING(prod.c_hora).
        /*MESSAGE cMes VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        /*mensaje = "No puede ingresar un numero de producci�n existente!".*/
        mensaje = cMes.
        RETURN FALSE.
      END.
    END.
    /*
    when "id_produccion" then
      do: 
        define buffer prod for produccion_jugo.
        for each prod where prod.id_sucursal = integer(produccion_jugo.id_sucursal:screen-value in frame F-Main).
         if integer(valor) = prod.id_produccion then 
            do:
                mensaje = "No puede ingresar un numero de producci�n existente!".
                return false.
            end.
        end.
      end. 
      */
     when "id_articulo" then
      do: 
        if not mi-alta then
        do:
        find productos_terminados where productos_terminados.id_articulo = integer(valor) and productos_terminados.id_tipo_articulo = 2 no-lock no-error.
         if not available productos_terminados then 
            do:
                mensaje = "Debe ingresar un Producto terminado v�lido!".
                return false.
            end.
        end.
      end.
     when "kilos_1" then
      do: 
             
      end.
      when "kilos_2" then
      do: 
        
      end.
     when "id_sucursal" then
      do: 
        find sucursales where sucursales.id_sucursal = integer(valor) no-lock no-error.
         if not available sucursales then 
            do:
                mensaje = "Debe ingresar una Sucursal v�lida!".
                return false.
            end.
        
      end.
     when "fecha" then
      do: 
        IF DATE(valor) <> TODAY AND DATE(valor) <> (TODAY - 1) THEN 
            do:
                mensaje = "No puede cargar producciones anteriores a " + STRING(TODAY - 1).
                return false.
            end.
        
      end.
     when "id_empresa" then
      do: 
        find empresas where empresas.id_empresa = integer(valor) no-lock no-error.
         if not available empresas then 
            do:
                mensaje = "Debe ingresar una Empresa v�lida!".
                return false.
            end.
        
      end.
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

