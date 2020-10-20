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
define var del_lote as integer.
define var del_fecha as date.
define var del_articulo as integer.
define var del_calidad as integer.

define var mi-alta as logical initial false.

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
&Scoped-define EXTERNAL-TABLES lotes_jugo
&Scoped-define FIRST-EXTERNAL-TABLE lotes_jugo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lotes_jugo.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS lotes_jugo.id_tipolimon lotes_jugo.id_empresa ~
lotes_jugo.id_sucursal lotes_jugo.Fecha lotes_jugo.id_lote ~
lotes_jugo.id_articulo lotes_jugo.Pulpa lotes_jugo.Jugo_pomelo ~
lotes_jugo.id_calidad lotes_jugo.id_envase lotes_jugo.Hora_comienzo ~
lotes_jugo.Fecha_comienzo lotes_jugo.Hora_finalizacion ~
lotes_jugo.Fecha_finalizacion lotes_jugo.id_tanque lotes_jugo.Balanza_usada ~
lotes_jugo.Peso_neto lotes_jugo.Calibracion lotes_jugo.Control_pesas ~
lotes_jugo.Hora_comienzo_envase lotes_jugo.Fecha_comienzo_envase ~
lotes_jugo.Hora_fin_envase lotes_jugo.Fecha_finalizacion_envase ~
lotes_jugo.id_legajo_capataz 
&Scoped-define ENABLED-TABLES lotes_jugo
&Scoped-define FIRST-ENABLED-TABLE lotes_jugo
&Scoped-Define ENABLED-OBJECTS RECT-18 
&Scoped-Define DISPLAYED-FIELDS lotes_jugo.id_tipolimon ~
lotes_jugo.id_empresa lotes_jugo.id_sucursal lotes_jugo.Fecha ~
lotes_jugo.id_lote lotes_jugo.id_articulo lotes_jugo.Pulpa ~
lotes_jugo.Jugo_pomelo lotes_jugo.id_calidad lotes_jugo.id_envase ~
lotes_jugo.Hora_comienzo lotes_jugo.Fecha_comienzo ~
lotes_jugo.Hora_finalizacion lotes_jugo.Fecha_finalizacion ~
lotes_jugo.id_tanque lotes_jugo.Balanza_usada lotes_jugo.Peso_neto ~
lotes_jugo.Calibracion lotes_jugo.Control_pesas ~
lotes_jugo.Hora_comienzo_envase lotes_jugo.Fecha_comienzo_envase ~
lotes_jugo.Hora_fin_envase lotes_jugo.Fecha_finalizacion_envase ~
lotes_jugo.id_legajo_capataz 
&Scoped-define DISPLAYED-TABLES lotes_jugo
&Scoped-define FIRST-DISPLAYED-TABLE lotes_jugo
&Scoped-Define DISPLAYED-OBJECTS fi-calidades-descripcion ~
fi-empresas-razon_social fi-envases_prod-descripcion fi-legajo-nomb-12 ~
fi-productos_terminados-descrip fi-sucursales-nombre ~
fi-tipos_limon-descripcion anio 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS lotes_jugo.id_empresa ~
lotes_jugo.id_sucursal lotes_jugo.id_lote 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_tipotambor||y|general.lotes_jugo.id_tipotambor
id_tipolimon||y|general.lotes_jugo.id_tipolimon
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_tipotambor,id_tipolimon"':U).
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
DEFINE VARIABLE anio AS CHARACTER FORMAT "99":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.05 NO-UNDO.

DEFINE VARIABLE fi-calidades-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-empresas-razon_social AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 54 BY .86
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-legajo-nomb-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-sucursales-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tipos_limon-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 140 BY .14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-calidades-descripcion AT ROW 3.33 COL 26 COLON-ALIGNED NO-LABEL
     fi-empresas-razon_social AT ROW 1 COL 18 COLON-ALIGNED NO-LABEL
     fi-envases_prod-descripcion AT ROW 3.38 COL 91 COLON-ALIGNED NO-LABEL
     fi-legajo-nomb-12 AT ROW 5.14 COL 113 COLON-ALIGNED NO-LABEL
     fi-productos_terminados-descrip AT ROW 2.14 COL 49 COLON-ALIGNED NO-LABEL
     fi-sucursales-nombre AT ROW 1 COL 79 COLON-ALIGNED NO-LABEL
     fi-tipos_limon-descripcion AT ROW 5 COL 29 COLON-ALIGNED NO-LABEL
     lotes_jugo.id_tipolimon AT ROW 5 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     lotes_jugo.id_empresa AT ROW 1 COL 9 COLON-ALIGNED
          LABEL "Empresa"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     lotes_jugo.id_sucursal AT ROW 1 COL 69 COLON-ALIGNED
          LABEL "Sucursal"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     lotes_jugo.Fecha AT ROW 1 COL 131 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     lotes_jugo.id_lote AT ROW 2.14 COL 9 COLON-ALIGNED
          LABEL "Lote"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     anio AT ROW 2.14 COL 22 COLON-ALIGNED NO-LABEL
     lotes_jugo.id_articulo AT ROW 2.14 COL 39 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     lotes_jugo.Pulpa AT ROW 2.14 COL 98 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     lotes_jugo.Jugo_pomelo AT ROW 2.24 COL 139 COLON-ALIGNED
          LABEL "Jugo de pomelo"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     lotes_jugo.id_calidad AT ROW 3.33 COL 9 COLON-ALIGNED
          LABEL "Calidad"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lotes_jugo.id_envase AT ROW 3.33 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1.05
     lotes_jugo.Hora_comienzo AT ROW 6.43 COL 18 COLON-ALIGNED
          LABEL "Hora de comienzo"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     lotes_jugo.Fecha_comienzo AT ROW 6.43 COL 49 COLON-ALIGNED
          LABEL "Fecha de comienzo" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     lotes_jugo.Hora_finalizacion AT ROW 6.48 COL 87 COLON-ALIGNED
          LABEL "Hora de finalizaci�n"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     lotes_jugo.Fecha_finalizacion AT ROW 6.43 COL 124 COLON-ALIGNED
          LABEL "Fecha de finalizaci�n" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lotes_jugo.id_tanque AT ROW 7.52 COL 13 COLON-ALIGNED
          LABEL "Tanque"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     lotes_jugo.Balanza_usada AT ROW 7.57 COL 36 COLON-ALIGNED
          LABEL "Balanza usada"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     lotes_jugo.Peso_neto AT ROW 7.52 COL 70 COLON-ALIGNED
          LABEL "Peso neto"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     lotes_jugo.Calibracion AT ROW 7.57 COL 97 COLON-ALIGNED
          LABEL "Calibraci�n"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     lotes_jugo.Control_pesas AT ROW 7.43 COL 125 COLON-ALIGNED
          LABEL "Control pesas"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     lotes_jugo.Hora_comienzo_envase AT ROW 8.67 COL 28 COLON-ALIGNED
          LABEL "Comienzo de envase"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     lotes_jugo.Fecha_comienzo_envase AT ROW 8.67 COL 46 COLON-ALIGNED
          LABEL "Fecha" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     lotes_jugo.Hora_fin_envase AT ROW 8.67 COL 95 COLON-ALIGNED
          LABEL "Finalizaci�n de envase"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     lotes_jugo.Fecha_finalizacion_envase AT ROW 8.67 COL 113 COLON-ALIGNED
          LABEL "Fecha" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     lotes_jugo.id_legajo_capataz AT ROW 5.14 COL 99 COLON-ALIGNED
          LABEL "Legajo del capataz"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     RECT-18 AT ROW 4.67 COL 5
     "/" VIEW-AS TEXT
          SIZE 1.6 BY 1.14 AT ROW 2.05 COL 22
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.lotes_jugo
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
         HEIGHT             = 8.81
         WIDTH              = 149.6.
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

/* SETTINGS FOR FILL-IN anio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Balanza_usada IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Calibracion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Control_pesas IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Fecha_comienzo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN lotes_jugo.Fecha_comienzo_envase IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN lotes_jugo.Fecha_finalizacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN lotes_jugo.Fecha_finalizacion_envase IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi-calidades-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-empresas-razon_social IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-legajo-nomb-12 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sucursales-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipos_limon-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Hora_comienzo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Hora_comienzo_envase IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Hora_finalizacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Hora_fin_envase IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.id_calidad IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.id_empresa IN FRAME F-Main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN lotes_jugo.id_legajo_capataz IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.id_lote IN FRAME F-Main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN lotes_jugo.id_sucursal IN FRAME F-Main
   1 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN lotes_jugo.id_tanque IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Jugo_pomelo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lotes_jugo.Peso_neto IN FRAME F-Main
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
general.lotes_jugo.id_sucursal;wc_sucursales.w;sucursales.nombre;;
general.lotes_jugo.id_tipolimon;wc_tipos_limon.w;tipos_limon.descripcion;;
general.lotes_jugo.id_legajo_capataz;wc_legajo.w;legajo.nomb-12;lega-12;
general.lotes_jugo.id_articulo;wc_productos_terminados.w;productos_terminados.descripcion;;
general.lotes_jugo.id_calidad;wc_calidades.w;calidades.descripcion;;
general.lotes_jugo.id_empresa;wc_empreas.w;empresas.razon_social;;
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

&Scoped-define SELF-NAME anio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL anio V-table-Win
ON LEAVE OF anio IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Balanza_usada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Balanza_usada V-table-Win
ON LEAVE OF lotes_jugo.Balanza_usada IN FRAME F-Main /* Balanza usada */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Calibracion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Calibracion V-table-Win
ON LEAVE OF lotes_jugo.Calibracion IN FRAME F-Main /* Calibraci�n */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Control_pesas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Control_pesas V-table-Win
ON LEAVE OF lotes_jugo.Control_pesas IN FRAME F-Main /* Control pesas */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Fecha V-table-Win
ON LEAVE OF lotes_jugo.Fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Fecha_comienzo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Fecha_comienzo V-table-Win
ON LEAVE OF lotes_jugo.Fecha_comienzo IN FRAME F-Main /* Fecha de comienzo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Fecha_comienzo_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Fecha_comienzo_envase V-table-Win
ON LEAVE OF lotes_jugo.Fecha_comienzo_envase IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Fecha_finalizacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Fecha_finalizacion V-table-Win
ON LEAVE OF lotes_jugo.Fecha_finalizacion IN FRAME F-Main /* Fecha de finalizaci�n */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Fecha_finalizacion_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Fecha_finalizacion_envase V-table-Win
ON LEAVE OF lotes_jugo.Fecha_finalizacion_envase IN FRAME F-Main /* Fecha */
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


&Scoped-define SELF-NAME fi-legajo-nomb-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-legajo-nomb-12 V-table-Win
ON LEAVE OF fi-legajo-nomb-12 IN FRAME F-Main
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


&Scoped-define SELF-NAME fi-tipos_limon-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipos_limon-descripcion V-table-Win
ON LEAVE OF fi-tipos_limon-descripcion IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Hora_comienzo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Hora_comienzo V-table-Win
ON LEAVE OF lotes_jugo.Hora_comienzo IN FRAME F-Main /* Hora de comienzo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Hora_comienzo_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Hora_comienzo_envase V-table-Win
ON LEAVE OF lotes_jugo.Hora_comienzo_envase IN FRAME F-Main /* Comienzo de envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Hora_finalizacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Hora_finalizacion V-table-Win
ON LEAVE OF lotes_jugo.Hora_finalizacion IN FRAME F-Main /* Hora de finalizaci�n */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Hora_fin_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Hora_fin_envase V-table-Win
ON LEAVE OF lotes_jugo.Hora_fin_envase IN FRAME F-Main /* Finalizaci�n de envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_articulo V-table-Win
ON GO OF lotes_jugo.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_articulo V-table-Win
ON LEAVE OF lotes_jugo.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_jugo.id_articulo IN FRAME F-Main /* Art�culo */
do: 
define var r as rowid no-undo.
run wc_productos_terminados.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.lotes_jugo.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_articulo V-table-Win
ON U1 OF lotes_jugo.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_calidad V-table-Win
ON GO OF lotes_jugo.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_calidad V-table-Win
ON LEAVE OF lotes_jugo.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_calidad V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_jugo.id_calidad IN FRAME F-Main /* Calidad */
do: 
define var r as rowid no-undo.

/*if integer(lotes_jugo.id_articulo:screen-value) = 52 then
    run wc_calidades_turbio.w(output r).
else
    do:
        if integer(lotes_jugo.id_articulo:screen-value) = 53 then
            run wc_calidades_claro.w(output r).
        else 
         do:
            message "Ingreso un articulo que puede causar problemas con el rnpa, por favor avisar a Sistemas" .           
            run wc_calidades.w(output r).
         end.
    end.
  */

RUN wc_productos_calidades.w (INPUT INTEGER(lotes_jugo.id_articulo:SCREEN-VALUE),OUTPUT r).
find r_productos_calidad where rowid(r_productos_calidad) = r no-lock no-error.
if available r_productos_calidad then 
    DO:
        FIND calidades OF r_productos_calidad NO-LOCK NO-ERROR.
        IF AVAILABLE calidades THEN
            general.lotes_jugo.id_calidad:screen-value = string(calidades.id_calidad).

    END.
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_calidad V-table-Win
ON U1 OF lotes_jugo.id_calidad IN FRAME F-Main /* Calidad */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_empresa V-table-Win
ON GO OF lotes_jugo.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_empresa V-table-Win
ON LEAVE OF lotes_jugo.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_jugo.id_empresa IN FRAME F-Main /* Empresa */
do: 
define var r as rowid no-undo.
run wc_empreas.w(output r).
find empresas where rowid(empresas) = r no-lock no-error.
if available empresas then 
general.lotes_jugo.id_empresa:screen-value = string(empresas.id_empresa).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_empresa V-table-Win
ON U1 OF lotes_jugo.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_envase V-table-Win
ON GO OF lotes_jugo.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
     run descriptivos1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_envase V-table-Win
ON LEAVE OF lotes_jugo.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_envase V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_jugo.id_envase IN FRAME F-Main /* Envase */
do: 
define var r as rowid no-undo.
run wc_envases.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
lotes_jugo.id_envase:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_envase V-table-Win
ON U1 OF lotes_jugo.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
     run descriptivos1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_legajo_capataz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_legajo_capataz V-table-Win
ON GO OF lotes_jugo.id_legajo_capataz IN FRAME F-Main /* Legajo del capataz */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_legajo_capataz V-table-Win
ON LEAVE OF lotes_jugo.id_legajo_capataz IN FRAME F-Main /* Legajo del capataz */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_legajo_capataz V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_jugo.id_legajo_capataz IN FRAME F-Main /* Legajo del capataz */
do: 
define var r as rowid no-undo.
run wc_legajo.w(output r).
find legajo where rowid(legajo) = r no-lock no-error.
if available legajo then 
general.lotes_jugo.id_legajo_capataz:screen-value = string(legajo.lega-12).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_legajo_capataz V-table-Win
ON U1 OF lotes_jugo.id_legajo_capataz IN FRAME F-Main /* Legajo del capataz */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_lote V-table-Win
ON LEAVE OF lotes_jugo.id_lote IN FRAME F-Main /* Lote */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_sucursal V-table-Win
ON GO OF lotes_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_sucursal V-table-Win
ON LEAVE OF lotes_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_sucursal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
do: 
define var r as rowid no-undo.
run wc_sucursales.w(output r).
find sucursales where rowid(sucursales) = r no-lock no-error.
if available sucursales then 
general.lotes_jugo.id_sucursal:screen-value = string(sucursales.id_sucursal).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_sucursal V-table-Win
ON U1 OF lotes_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_tanque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_tanque V-table-Win
ON LEAVE OF lotes_jugo.id_tanque IN FRAME F-Main /* Tanque */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.id_tipolimon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_tipolimon V-table-Win
ON GO OF lotes_jugo.id_tipolimon IN FRAME F-Main /* Tipo Limon */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_tipolimon V-table-Win
ON LEAVE OF lotes_jugo.id_tipolimon IN FRAME F-Main /* Tipo Limon */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_tipolimon V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_jugo.id_tipolimon IN FRAME F-Main /* Tipo Limon */
do: 
define var r as rowid no-undo.
run wc_tipos_limon.w(output r).
find tipos_limon where rowid(tipos_limon) = r no-lock no-error.
if available tipos_limon then 
general.lotes_jugo.id_tipolimon:screen-value = string(tipos_limon.id_tipolimon).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.id_tipolimon V-table-Win
ON U1 OF lotes_jugo.id_tipolimon IN FRAME F-Main /* Tipo Limon */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Jugo_pomelo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Jugo_pomelo V-table-Win
ON LEAVE OF lotes_jugo.Jugo_pomelo IN FRAME F-Main /* Jugo de pomelo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Peso_neto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Peso_neto V-table-Win
ON LEAVE OF lotes_jugo.Peso_neto IN FRAME F-Main /* Peso neto */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_jugo.Pulpa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_jugo.Pulpa V-table-Win
ON LEAVE OF lotes_jugo.Pulpa IN FRAME F-Main /* Pulpa */
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
/*  run set-attribute-list ('tipo-detalle=cabecera'). */
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

assign lotes_jugo.id_tipotambor  = 3
       lotes_jugo.nromov         = next-value(nromov)
       lotes_jugo.estado_lote    = 1
       lotes_jugo.anio           = year(date(lotes_jugo.fecha:screen-value in frame F-Main))
       lotes_jugo.c_usuario      = userid("userdb")
       lotes_jugo.c_fecha        = today
       lotes_jugo.c_hora         = string(time,"HH:MM:SS").
       
   
/****************************HABILITO OBJETOS***************************************/

define var hcontainer as handle.
run get-container (output hcontainer).

if hcontainer <> ? then run resetea-registro in hcontainer (input ?).

/**********************************************************************************/





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-delete V-table-Win 
PROCEDURE adm-post-delete :
/*******************************************************************************/
/********************BORRO TODAS LAS TABLAS ASOCIADAS***************************/
DEFINE BUFFER b_tam FOR tambores_industria.
DEFINE VAR desde AS INTEGER.
DEFINE VAR hasta AS INTEGER.
desde = 0.
hasta = 0.
FOR EACH inspecciones_lote WHERE inspecciones_lote.id_empresa = del_empresa and
                                 inspecciones_lote.id_sucursal = del_sucursal and
                                 inspecciones_lote.id_tipotambor = del_tipotambor and
                                 inspecciones_lote.nromov = del_nromov.
    delete inspecciones_lote.
end.                                 


FOR EACH sobrante_lote where sobrante_lote.id_empresa = del_empresa and
                             sobrante_lote.id_sucursal = del_sucursal and
                             sobrante_lote.id_tipotambor = del_tipotambor and
                             sobrante_lote.nromov = del_nromov.
    
    /* ACA DOY DE BAJA DEL STOCK LOS TAMBORES SOBRANTE DEL LOTE */
    FIND FIRST b_tam WHERE b_tam.id_empresa     = del_empresa and
                           b_tam.id_sucursal    = del_sucursal and
                           b_tam.id_tipotambor  = sobrante_lote.id_tipotambor_sobrante and
                           b_tam.nromov         = sobrante_lote.nromov_sobrante NO-LOCK NO-ERROR.
    
    IF AVAILABLE b_tam THEN DO:
       desde = b_tam.id_tambor.
    END.
    FIND LAST b_tam WHERE b_tam.id_empresa     = del_empresa and
                           b_tam.id_sucursal   = del_sucursal and
                           b_tam.id_tipotambor = sobrante_lote.id_tipotambor_sobrante and
                           b_tam.nromov        = sobrante_lote.nromov_sobrante  NO-LOCK NO-ERROR.
    
    IF AVAILABLE b_tam THEN DO:
        hasta = b_tam.id_tambor.
    END.
    
    RUN y_gstkcre.p (input del_empresa,
                     input del_sucursal,
                     input sobrante_lote.id_tipotambor_sobrante,
                     input sobrante_lote.nromov_sobrante,
                     INPUT desde,
                     INPUT hasta,
                     input 2) "sobrante".
    
    IF return-value <> "" then do:
        message "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "ADM-ERROR".
    end.
    /*******************************************************************************************/

    FOR each tambores_industria where tambores_industria.id_empresa = sobrante_lote.id_empresa and
                                      tambores_industria.id_sucursal = sobrante_lote.id_sucursal and
                                      tambores_industria.id_tipotambor = sobrante_lote.id_tipotambor_sobrante and
                                      tambores_industria.nromov = sobrante_lote.nromov_sobrante.
        delete tambores_industria.
    END.                                 

    DELETE sobrante_lote.
END.


FOR EACH composicion_lote where composicion_lote.id_empresa = del_empresa and
                                composicion_lote.id_sucursal = del_sucursal and
                                composicion_lote.id_tipotambor = del_tipotambor and
                                composicion_lote.nromov = del_nromov.
    delete composicion_lote.
end.

/* ACA DOY DE BAJA DEL STOCK LOS TAMBORES DEL LOTE */
desde = 0.
hasta = 0.
FIND FIRST b_tam WHERE b_tam.id_empresa = del_empresa and
                       b_tam.id_sucursal = del_sucursal and
                       b_tam.id_tipotambor = del_tipotambor and
                       b_tam.nromov = del_nromov NO-LOCK NO-ERROR.

IF AVAILABLE b_tam THEN DO:
   desde = b_tam.id_tambor.
END.
FIND LAST b_tam WHERE b_tam.id_empresa = del_empresa and
                      b_tam.id_sucursal = del_sucursal and
                      b_tam.id_tipotambor = del_tipotambor and
                      b_tam.nromov = del_nromov  NO-LOCK NO-ERROR.

IF AVAILABLE b_tam THEN DO:
    hasta = b_tam.id_tambor.
END.

RUN y_gstkcre.p (input del_empresa,
                 input del_sucursal,
                 input del_tipotambor,
                 input del_nromov,
                 INPUT desde,
                 INPUT hasta,
                 input 2) "lotes_jugo".

IF return-value <> "" then do:
    message "Error en el procesamiento de movimientos de stock" view-as alert-box.
    RETURN "error".
end.


FOR each tambores_industria where tambores_industria.id_empresa = del_empresa and
                                  tambores_industria.id_sucursal = del_sucursal and
                                  tambores_industria.id_tipotambor = del_tipotambor and
                                  tambores_industria.nromov = del_nromov.
    delete tambores_industria.
END.

    
FOR EACH datos_jugo_clarificado where datos_jugo_clarificado.id_empresa = del_empresa and
                                      datos_jugo_clarificado.id_sucursal = del_sucursal and
                                      datos_jugo_clarificado.id_tipotambor = del_tipotambor and
                                      datos_jugo_clarificado.nromov = del_nromov.

    delete datos_jugo_clarificado.
end.

FOR EACH arrastre_lote where arrastre_lote.id_empresa = del_empresa and
                             arrastre_lote.id_sucursal = del_sucursal and
                             arrastre_lote.id_tipotambor = del_tipotambor and
                             arrastre_lote.nromov = del_nromov.
    
    /* ACA DOY DE BAJA DEL STOCK LOS TAMBORES DE ARRASTRE DEL LOTE */
    desde = 0.
    hasta = 0.
    FIND FIRST b_tam WHERE b_tam.id_empresa     = del_empresa and
                           b_tam.id_sucursal    = del_sucursal and
                           b_tam.id_tipotambor  = arrastre_lote.id_tipotambor_arrastre and
                           b_tam.nromov         = arrastre_lote.nromov_arrastre NO-LOCK NO-ERROR.
    
    IF AVAILABLE b_tam THEN DO:
       desde = b_tam.id_tambor.
    END.
    FIND LAST b_tam WHERE b_tam.id_empresa     = del_empresa and
                           b_tam.id_sucursal   = del_sucursal and
                           b_tam.id_tipotambor = arrastre_lote.id_tipotambor_arrastre and
                           b_tam.nromov        = arrastre_lote.nromov_arrastre  NO-LOCK NO-ERROR.
    
    IF AVAILABLE b_tam THEN DO:
        hasta = b_tam.id_tambor.
    END.
    
    RUN y_gstkcre.p (input del_empresa,
                     input del_sucursal,
                     input arrastre_lote.id_tipotambor_arrastre,
                     input arrastre_lote.nromov_arrastre,
                     INPUT desde,
                     INPUT hasta,
                     input 2) "arrastre_lote".
    
    IF return-value <> "" then do:
        message "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "ADM-ERROR".
    end.
    /*******************************************************************************************/

    for each tambores_industria where tambores_industria.id_empresa = del_empresa and
                                      tambores_industria.id_sucursal = del_sucursal and
                                      tambores_industria.id_tipotambor = arrastre_lote.id_tipotambor_arrastre and
                                      tambores_industria.nromov = arrastre_lote.nromov_arrastre.

            delete tambores_industria.
    end.

    delete arrastre_lote.
end.


for each tambores_industria where tambores_industria.id_empresa_destino = del_empresa and
                                  tambores_industria.id_sucursal_destino = del_sucursal and
                                  tambores_industria.id_tipotambor_destino = del_tipotambor and
                                  tambores_industria.nromov_destino = del_nromov.

    tambores_industria.id_empresa_destino = 0.
    tambores_industria.id_sucursal_destino = 0.
    tambores_industria.id_tipotambor_destino = 0.                           
    tambores_industria.nromov_destino = 0.
    
end.                               




create auditoria_lotes.
assign auditoria_lotes.id_empresa        = del_empresa
       auditoria_lotes.id_sucursal       = del_sucursal
       auditoria_lotes.id_tipotambor     = del_tipotambor
       auditoria_lotes.nromov            = del_nromov
       auditoria_lotes.id_lote           = del_lote
       auditoria_lotes.fecha             = del_fecha
       auditoria_lotes.id_articulo       = del_articulo
       auditoria_lotes.id_calidad        = del_calidad
       auditoria_lotes.anio              = year(del_fecha)
       auditoria_lotes.c_usuario         = userid("userdb")
       auditoria_lotes.c_fecha           = today
       auditoria_lotes.c_hora            = string(time,"HH:MM:SS").
       
/***********************************************************************************/



/***********************************************************************************/



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
define var hcontainer as handle.
define var existe as logical.

run get-container (output hcontainer).
RUN get-rowid-cabecera in hcontainer (output r).
FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.

IF AVAILABLE lotes_jugo THEN DO:
    FIND FIRST envases_prod WHERE envases_prod.id_envase = lotes_jugo.id_envase NO-LOCK NO-ERROR.

    FOR EACH tambores_industria OF lotes_jugo.
        ASSIGN tambores_industria.id_articulo    = lotes_jugo.id_articulo
               tambores_industria.id_calidad     = lotes_jugo.id_calidad
               tambores_industria.fecha          = lotes_jugo.fecha
               tambores_industria.id_lote        = lotes_jugo.id_lote
               tambores_industria.id_envase      = lotes_jugo.id_envase
               tambores_industria.kilos_tambor   = lotes_jugo.peso_neto
               tambores_industria.tara           = IF AVAILABLE envases_prod THEN envases_prod.tara ELSE 0.
    end.
end.

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
define var existe as logical.

define var sob_empresa as integer.
define var sob_sucursal as integer.
define var sob_tipotambor as integer.
define var sob_nromov as integer.


/******************************************************************************/
/*************************ALMACENO LOS ID DEL REGISTRO A BORRAR****************/
run get-container (output hcontainer).
RUN get-rowid-cabecera in hcontainer (output r).

FOR EACH lotes_jugo WHERE ROWID(lotes_jugo) = r.
        del_empresa     = lotes_jugo.id_empresa.
        del_sucursal    = lotes_jugo.id_sucursal.
        del_tipotambor  = lotes_jugo.id_tipotambor.
        del_nromov      = lotes_jugo.nromov.
        del_lote        = lotes_jugo.id_lote.
        del_fecha       = lotes_jugo.fecha.
        del_articulo    = lotes_jugo.id_articulo.
        del_calidad     = lotes_jugo.id_calidad.    
end.

existe = false.
FOR EACH sobrante_lote WHERE sobrante_lote.id_empresa = del_empresa
                         AND sobrante_lote.id_sucursal = del_sucursal
                         AND sobrante_lote.id_tipotambor = del_tipotambor
                         AND sobrante_lote.nromov = del_nromov.
    
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa     = sobrante_lote.id_empresa 
                                  AND tambores_industria.id_sucursal    = sobrante_lote.id_sucursal 
                                  AND tambores_industria.id_tipotambor  = sobrante_lote.id_tipotambor_sobrante 
                                  AND tambores_industria.nromov         = sobrante_lote.nromov_sobrante.
    
        IF tambores_industria.id_empresa_destino <> 0 AND
           tambores_industria.id_sucursal_destino <> 0 AND
           tambores_industria.id_tipotambor_destino <> 0 AND
           tambores_industria.nromov_destino <> 0 THEN 
            DO: 
                existe = true.
                UNDO, RETURN.
            END.
    END.
    IF existe THEN DO:
        MESSAGE "No puede borrar el lote porque el sobrante del mismo esta siendo utilizado."  view-as alert-box.
        RETURN "ADM-ERROR".            
    END.

END.


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
  {src/adm/template/row-list.i "lotes_jugo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "lotes_jugo"}

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
find first sucursales where sucursales.id_sucursal = integer(lotes_jugo.id_sucursal:screen-value in frame F-Main)  no-lock no-error .
if available sucursales then 
fi-sucursales-nombre:screen-value in frame F-Main = string(sucursales.nombre).
else
fi-sucursales-nombre:screen-value in frame F-Main = ''.

find first tipos_limon where tipos_limon.id_tipolimon = integer(lotes_jugo.id_tipolimon:screen-value in frame F-Main)  no-lock no-error .
if available tipos_limon then 
fi-tipos_limon-descripcion:screen-value in frame F-Main = string(tipos_limon.descripcion).
else
fi-tipos_limon-descripcion:screen-value in frame F-Main = ''.

find first legajo where legajo.lega-12 = integer(lotes_jugo.id_legajo_capataz:screen-value in frame F-Main)  no-lock no-error .
if available legajo then 
fi-legajo-nomb-12:screen-value in frame F-Main = string(legajo.nomb-12).
else
fi-legajo-nomb-12:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(lotes_jugo.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first calidades where calidades.id_calidad = integer(lotes_jugo.id_calidad:screen-value in frame F-Main)  no-lock no-error .
if available calidades then 
fi-calidades-descripcion:screen-value in frame F-Main = string(calidades.descripcion).
else
fi-calidades-descripcion:screen-value in frame F-Main = ''.

find first empresas where empresas.id_empresa = integer(lotes_jugo.id_empresa:screen-value in frame F-Main)  no-lock no-error .
if available empresas then 
fi-empresas-razon_social:screen-value in frame F-Main = string(empresas.razon_social).
else
fi-empresas-razon_social:screen-value in frame F-Main = ''.

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
find first envases_prod where envases_prod.id_envase = integer(lotes_jugo.id_envase:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion:screen-value in frame F-Main = ''. 

anio:screen-value in frame {&FRAME-NAME} = substring(lotes_jugo.fecha:screen-value in frame F-Main,9,2).
/* message "Anio" anio:screen-value in frame {&FRAME-NAME} view-as alert-box. */
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
if (date(lotes_jugo.fecha_finalizacion:screen-value in frame {&FRAME-NAME}) + 2) < date(lotes_jugo.fecha_finalizacion_envase:screen-value in frame {&FRAME-NAME}) then 
    do:
            message "Esta seguro que la fecha de finalizaci�n de envase fue hecha dos dias posteriores a la fecha del armado del lote?"
                     view-as alert-box.
            return no-apply.
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
define var lista_relacion as character no-undo initial "id_sucursal,id_tipolimon,id_legajo_capataz,id_articulo,id_calidad,id_empresa".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit V-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
run dispatch ('update-record').
message "local-exit antes" view-as alert-box.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

message "local-exit despues" view-as alert-box.

  /* Code placed here will execute AFTER standard behavior.    */

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
  /*message "local-row-available" view-as alert-box.*/
  
  /* define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo (output r).
  for each lotes_jugo where rowid(lotes_jugo) = r.
    find first tambores_lote where tambores_lote.id_empresa = lotes_jugo.id_empresa and
                                   tambores_lote.id_sucursal = lotes_jugo.id_sucursal and
                                   tambores_lote.id_lote = lotes_jugo.id_lote no-lock no-error.
    if available tambores_lote then
        do:
            fi-envases_prod-descripcion:screen-value in frame {&FRAME-NAME} = string(tambores_lote.id_envase).
        end.
    
  end. */


  /* Code placed here will execute AFTER standard behavior.    */
  run descriptivos.
  run descriptivos1.
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
define var hcontainer as handle.
mi-alta = false.
run get-container (output hcontainer).
if hcontainer <> ? then
    do:
        run oculta-objetos in hcontainer.
        /*message "espero que haya ocultado" view-as alert-box.*/
    
    
        /*run adeuib/_accsect.p ("set",. */
    
    
    
    end.


/*message "post-add" view-as alert-box.*/
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
/*message "pre-add" view-as alert-box.*/
mi-alta = true.
/* anio:screen-value in frame {&FRAME-NAME} = substring(string(year(today)),3,2). */
anio:screen-value in frame {&FRAME-NAME} = substring(string(year(today)),3,2).

/* message "Hoy " year(today) view-as alert-box. */

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
  {src/adm/template/sndkycas.i "id_tipotambor" "lotes_jugo" "id_tipotambor"}
  {src/adm/template/sndkycas.i "id_tipolimon" "lotes_jugo" "id_tipolimon"}

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
  {src/adm/template/snd-list.i "lotes_jugo"}

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
define var segundos as integer.

  case nombre:
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
       
        DEFINE VARIABLE iLote AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iArti AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iAnio AS INTEGER    NO-UNDO.
        DEFINE VARIABLE iSucu AS INTEGER    NO-UNDO.

        define buffer lote for lotes_jugo.
        iLote = integer(lotes_jugo.id_lote:screen-value in frame F-Main).
        iArti = integer(valor).
        iAnio = year(date(lotes_jugo.fecha:screen-value in frame F-Main)) .
        iSucu = integer(lotes_jugo.id_sucursal:SCREEN-VALUE IN FRAME F-Main).

        FIND FIRST lote where lote.id_lote     = iLote
                          and lote.id_articulo = iArti
                          and year(lote.fecha) = iAnio
                          AND lote.id_sucursal = iSucu
                        no-lock no-error.
         if available lote then do:
                mensaje = "Debe ingresar un Lote no existente!".
                return false.
            end.

        
      end.
    when "id_sucursal" then
        if integer(valor) = 0 then 
        do:
            mensaje = "error".
            return false.
         end.
    when "fecha" then
        if DATE(valor) <> TODAY AND DATE(valor) <> TODAY - 1 then 
        do:
            mensaje = "No puede cargar un lote con fecha anterior al " + STRING(TODAY - 1).
            return false.
         end.
    when "peso_neto" then
        if integer(valor) <= 0 then 
        do:
            mensaje = "Por favor ingrese el peso neto de los tambores que van conformar el lote".
            return false.
         end.
    when "id_calidad" then
      do:
        if integer(lotes_jugo.id_articulo:screen-value in frame F-main) = 53 or 
           integer(lotes_jugo.id_articulo:screen-value in frame F-main) = 52 then
           do:
              find r_productos_calidad where 
                   r_productos_calidad.id_articulo = integer(lotes_jugo.id_articulo:screen-value in frame F-main) 
                        and
                   r_productos_calidad.id_calidad  = integer(valor) no-lock no-error.
              if not available r_productos_calidad then
                do:
                    mensaje = "Ingrese la calidad correcta. Si tiene dudas haga doble-click sobre el campo.Si sigue con problemas avisar a Dto. Sistemas.".
                    return false.
                end.
           end.       
      end.
/*    when "id_articulo" then
      do: 
        define buffer lote for lotes_jugo.
        find lote where lote.id_lote = integer(lotes_jugo.id_lote:screen-value in frame F-Main)
                    and lote.id_articulo = integer(valor)
                    and year(lote.fecha) = year(date(lotes_jugo.fecha:screen-value in frame F-Main)) no-lock no-error.
         if available lote then 
            do:
                mensaje = "Debe ingresar un Lote no existente!".
                return false.
            end.
    end.         
    
    when "fecha_finalizacion" then
      do:
        if date(lotes_jugo.fecha_comienzo:screen-value in frame {&FRAME-NAME}) > date(valor) then 
        do:
            mensaje = "La Fecha de Finalizaci�n no puede ser anterior a la Fecha de comienzo".
            return false.
         end.
         
      end.
    when "fecha_finalizacion_envase" then
        if date(lotes_jugo.fecha_comienzo_envase:screen-value in frame {&FRAME-NAME}) > date(valor) then 
        do:
            mensaje = "La Fecha de Finalizaci�n no puede ser anterior a la Fecha de comienzo de envase".
            return false.
         end.
    when "fecha_comienzo_envase" then
        if date(lotes_jugo.fecha_comienzo:screen-value in frame {&FRAME-NAME}) > date(valor) then 
        do:
            mensaje = "La Fecha de Comienzo de envase no puede ser anterior a la Fecha de comienzo".
            return false.
         end. */

   when "hora_fin_envase" then
    do:
        if integer(substring(valor,1,2)) > 24 or integer(substring(valor,4,2)) > 60 then 
            do:
                mensaje = "Debe ingresar una hora v�lida".
                    return false.
            end.
      /*  else
            do:
                if integer(substring(valor,1,2) + substring(valor,4,2)) < integer(substring(hora_comienzo_envase:screen-value in frame {&FRAME-NAME},1,2) + substring(hora_comienzo_envase:screen-value in frame {&FRAME-NAME},4,2)) then 
                do:
                    mensaje = "Debe ingresar una hora de finalizaci�n posterior a la hora de inicio".
                    return false.
                end.
            end. */
    end. 
    when "hora_comienzo_envase" then
    do:
        if integer(substring(valor,1,2)) > 24 or integer(substring(valor,4,2)) > 60 then 
            do:
                mensaje = "Debe ingresar una hora v�lida".
                return false.
            end.
    end.

    when "hora_comienzo" then
        do:
        if integer(substring(valor,1,2)) > 24 or integer(substring(valor,4,2)) > 60 then 
            do:
                mensaje = "Debe ingresar una hora v�lida".
                return false.
            end.
        end.
   when "hora_finalizacion" then
        do:
        if integer(substring(valor,1,2)) > 24 or integer(substring(valor,4,2)) > 60 then 
            do:
                mensaje = "Debe ingresar una hora v�lida".
                return false.
            end.
       /* else
            do:
                if date(lotes_jugo.fecha_comienzo:screen-value in frame {&FRAME-NAME}) = date(lotes_jugo.fecha_finalizacion:screen-value in frame {&FRAME-NAME}) then
                    do:
                        if integer(substring(valor,1,2) + substring(valor,4,2)) < integer(substring(hora_comienzo:screen-value in frame {&FRAME-NAME},1,2) + substring(hora_comienzo:screen-value in frame {&FRAME-NAME},4,2)) then 
                            do:
                                mensaje = "Debe ingresar una hora de finalizaci�n posterior a la hora de inicio".
                                return false.
                            end.
                    end.
            end. */

        end.

    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
