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

DEFINE VAR v_id_muestra AS INTEGER.
DEFINE VAR v_anio_muestra AS INTEGER.

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
&Scoped-define EXTERNAL-TABLES muestras
&Scoped-define FIRST-EXTERNAL-TABLE muestras


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR muestras.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS muestras.id_muestra muestras.anio_muestra ~
muestras.fecha muestras.id_articulo muestras.id_destinatario ~
muestras.id_solicitante muestras.id_prioridad muestras.directo_cliente ~
muestras.id_cliente 
&Scoped-define ENABLED-TABLES muestras
&Scoped-define FIRST-ENABLED-TABLE muestras
&Scoped-Define ENABLED-OBJECTS BUTTON-1 
&Scoped-Define DISPLAYED-FIELDS muestras.id_muestra muestras.anio_muestra ~
muestras.fecha muestras.id_articulo muestras.id_destinatario ~
muestras.id_solicitante muestras.id_prioridad muestras.directo_cliente ~
muestras.id_cliente 
&Scoped-define DISPLAYED-TABLES muestras
&Scoped-define FIRST-DISPLAYED-TABLE muestras
&Scoped-Define DISPLAYED-OBJECTS fi-productos_terminados-descrip ~
fi-contactos_muestras-nombre fi-contactos_muestras-nombre-1 ~
fi-prioridades-descripcion fi-clientes-nombre 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS muestras.id_muestra muestras.anio_muestra 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_estado||y|general.muestras.id_estado
id_articulo||y|general.muestras.id_articulo
id_prioridad||y|general.muestras.id_prioridad
id_cliente||y|general.muestras.id_cliente
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_estado,id_articulo,id_prioridad,id_cliente"':U).
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
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "custom/imagen/iconos/im-check.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.14.

DEFINE VARIABLE fi-clientes-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-contactos_muestras-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-contactos_muestras-nombre-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-prioridades-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     muestras.id_muestra AT ROW 1 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     muestras.anio_muestra AT ROW 1 COL 31 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     muestras.fecha AT ROW 1 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     muestras.id_articulo AT ROW 1.95 COL 13 COLON-ALIGNED FORMAT ">99"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     fi-productos_terminados-descrip AT ROW 1.95 COL 19 COLON-ALIGNED NO-LABEL
     BUTTON-1 AT ROW 2.67 COL 62
     muestras.id_destinatario AT ROW 2.91 COL 13 COLON-ALIGNED
          LABEL "Destinatario"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     fi-contactos_muestras-nombre AT ROW 2.91 COL 19 COLON-ALIGNED NO-LABEL
     muestras.id_solicitante AT ROW 3.86 COL 13 COLON-ALIGNED
          LABEL "Solicitante"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     fi-contactos_muestras-nombre-1 AT ROW 3.86 COL 19 COLON-ALIGNED NO-LABEL
     muestras.id_prioridad AT ROW 4.81 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     fi-prioridades-descripcion AT ROW 4.81 COL 19 COLON-ALIGNED NO-LABEL
     muestras.directo_cliente AT ROW 5.76 COL 13.4 COLON-ALIGNED
          LABEL "DirectoCliente"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     muestras.id_cliente AT ROW 6.71 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     fi-clientes-nombre AT ROW 6.71 COL 25 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.muestras
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
         HEIGHT             = 7
         WIDTH              = 65.
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

/* SETTINGS FOR FILL-IN muestras.anio_muestra IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN muestras.directo_cliente IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi-clientes-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-contactos_muestras-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-contactos_muestras-nombre-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-prioridades-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN muestras.id_articulo IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN muestras.id_destinatario IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN muestras.id_muestra IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN muestras.id_solicitante IN FRAME F-Main
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
general.muestras.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
general.muestras.id_destinatario;wc_destinatarios.w;contactos_muestras.nombre;id_contacto;
general.muestras.id_cliente;wc_clientes.w;clientes.nombre;;
general.muestras.id_prioridad;wc_prioridades.w;prioridades.descripcion;;
general.muestras.id_solicitante;wc_solicitantes.w;contactos_muestras.nombre;id_contacto;
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

&Scoped-define SELF-NAME muestras.anio_muestra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.anio_muestra V-table-Win
ON LEAVE OF muestras.anio_muestra IN FRAME F-Main /* Año */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 V-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  RUN w_contactos_muestras.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME muestras.directo_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.directo_cliente V-table-Win
ON LEAVE OF muestras.directo_cliente IN FRAME F-Main /* DirectoCliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME muestras.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.fecha V-table-Win
ON LEAVE OF muestras.fecha IN FRAME F-Main /* Fecha */
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



&Scoped-define SELF-NAME fi-contactos_muestras-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-contactos_muestras-nombre V-table-Win 
ON LEAVE OF fi-contactos_muestras-nombre IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME fi-contactos_muestras-nombre-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-contactos_muestras-nombre-1 V-table-Win 
ON LEAVE OF fi-contactos_muestras-nombre-1 IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME fi-prioridades-descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-prioridades-descripcion V-table-Win 
ON LEAVE OF fi-prioridades-descripcion IN FRAME F-Main
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



&Scoped-define SELF-NAME muestras.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_articulo V-table-Win
ON GO OF muestras.id_articulo IN FRAME F-Main /* Cod Articulo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_articulo V-table-Win
ON LEAVE OF muestras.id_articulo IN FRAME F-Main /* Cod Articulo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF muestras.id_articulo IN FRAME F-Main /* Cod Articulo */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.muestras.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_articulo V-table-Win
ON U1 OF muestras.id_articulo IN FRAME F-Main /* Cod Articulo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME muestras.id_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_cliente V-table-Win
ON GO OF muestras.id_cliente IN FRAME F-Main /* CodCliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_cliente V-table-Win
ON LEAVE OF muestras.id_cliente IN FRAME F-Main /* CodCliente */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_cliente V-table-Win
ON MOUSE-SELECT-DBLCLICK OF muestras.id_cliente IN FRAME F-Main /* CodCliente */
do: 
define var r as rowid no-undo.
run wc_clientes.w(output r).
find clientes where rowid(clientes) = r no-lock no-error.
if available clientes then 
general.muestras.id_cliente:screen-value = string(clientes.id_cliente).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_cliente V-table-Win
ON U1 OF muestras.id_cliente IN FRAME F-Main /* CodCliente */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME muestras.id_destinatario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_destinatario V-table-Win
ON GO OF muestras.id_destinatario IN FRAME F-Main /* Destinatario */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_destinatario V-table-Win
ON LEAVE OF muestras.id_destinatario IN FRAME F-Main /* Destinatario */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_destinatario V-table-Win
ON MOUSE-SELECT-DBLCLICK OF muestras.id_destinatario IN FRAME F-Main /* Destinatario */
do: 
define var r as rowid no-undo.
run wc_destinatarios.w(output r).
find contactos_muestras where rowid(contactos_muestras) = r no-lock no-error.
if available contactos_muestras then 
general.muestras.id_destinatario:screen-value = string(contactos_muestras.id_contacto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_destinatario V-table-Win
ON U1 OF muestras.id_destinatario IN FRAME F-Main /* Destinatario */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME muestras.id_muestra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_muestra V-table-Win
ON LEAVE OF muestras.id_muestra IN FRAME F-Main /* Cod Muestra */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME muestras.id_prioridad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_prioridad V-table-Win
ON GO OF muestras.id_prioridad IN FRAME F-Main /* CodPrioridad */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_prioridad V-table-Win
ON LEAVE OF muestras.id_prioridad IN FRAME F-Main /* CodPrioridad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_prioridad V-table-Win
ON MOUSE-SELECT-DBLCLICK OF muestras.id_prioridad IN FRAME F-Main /* CodPrioridad */
do: 
define var r as rowid no-undo.
run wc_prioridades.w(output r).
find prioridades where rowid(prioridades) = r no-lock no-error.
if available prioridades then 
general.muestras.id_prioridad:screen-value = string(prioridades.id_prioridad).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_prioridad V-table-Win
ON U1 OF muestras.id_prioridad IN FRAME F-Main /* CodPrioridad */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME muestras.id_solicitante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_solicitante V-table-Win
ON GO OF muestras.id_solicitante IN FRAME F-Main /* Solicitante */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_solicitante V-table-Win
ON LEAVE OF muestras.id_solicitante IN FRAME F-Main /* Solicitante */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_solicitante V-table-Win
ON MOUSE-SELECT-DBLCLICK OF muestras.id_solicitante IN FRAME F-Main /* Solicitante */
do: 
define var r as rowid no-undo.
run wc_solicitantes.w(output r).
find contactos_muestras where rowid(contactos_muestras) = r no-lock no-error.
if available contactos_muestras then 
general.muestras.id_solicitante:screen-value = string(contactos_muestras.id_contacto).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL muestras.id_solicitante V-table-Win
ON U1 OF muestras.id_solicitante IN FRAME F-Main /* Solicitante */
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
ASSIGN muestras.c_usuario   = USERID("userdb")
       muestras.c_fecha     = TODAY
       muestras.c_hora      = STRING(TIME,"HH:MM:SS").
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
FOR EACH items_muestras WHERE items_muestras.id_muestra = v_id_muestra
                          AND items_muestras.anio_muestra   = v_anio_muestra.
    DELETE items_muestras.
END.
FOR EACH r_muestras_protocolos WHERE r_muestras_protocolos.id_muestra   = v_id_muestra
                                 AND r_muestras_protocolos.anio_muestra = v_anio_muestra.
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
v_id_muestra    = muestras.id_muestra.
v_anio_muestra  = muestras.anio_muestra.
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
  {src/adm/template/row-list.i "muestras"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "muestras"}

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
find first productos_terminados where productos_terminados.id_articulo = integer(muestras.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first contactos_muestras where contactos_muestras.id_contacto = integer(muestras.id_destinatario:screen-value in frame F-Main)  no-lock no-error .
if available contactos_muestras then 
fi-contactos_muestras-nombre:screen-value in frame F-Main = string(contactos_muestras.nombre).
else
fi-contactos_muestras-nombre:screen-value in frame F-Main = ''.

find first clientes where clientes.id_cliente = integer(muestras.id_cliente:screen-value in frame F-Main)  no-lock no-error .
if available clientes then 
fi-clientes-nombre:screen-value in frame F-Main = string(clientes.nombre).
else
fi-clientes-nombre:screen-value in frame F-Main = ''.

find first prioridades where prioridades.id_prioridad = integer(muestras.id_prioridad:screen-value in frame F-Main)  no-lock no-error .
if available prioridades then 
fi-prioridades-descripcion:screen-value in frame F-Main = string(prioridades.descripcion).
else
fi-prioridades-descripcion:screen-value in frame F-Main = ''.

find first contactos_muestras where contactos_muestras.id_contacto = integer(muestras.id_solicitante:screen-value in frame F-Main)  no-lock no-error .
if available contactos_muestras then 
fi-contactos_muestras-nombre-1:screen-value in frame F-Main = string(contactos_muestras.nombre).
else
fi-contactos_muestras-nombre-1:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_articulo,id_destinatario,id_cliente,id_prioridad,id_solicitante".
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
  {src/adm/template/sndkycas.i "id_estado" "muestras" "id_estado"}
  {src/adm/template/sndkycas.i "id_articulo" "muestras" "id_articulo"}
  {src/adm/template/sndkycas.i "id_prioridad" "muestras" "id_prioridad"}
  {src/adm/template/sndkycas.i "id_cliente" "muestras" "id_cliente"}

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
  {src/adm/template/snd-list.i "muestras"}

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

