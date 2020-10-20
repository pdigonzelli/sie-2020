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
define var del_empresa as integer.
define var del_sucursal as integer.
define var del_tipotambor as integer.
define var del_nromov as integer.
define var del_lote as integer.
define var del_fecha as date.
define var del_articulo as integer.
define var del_calidad as integer.

define var del_empresa_lote as integer.
define var del_sucursal_lote as integer.
define var del_tipotambor_lote as integer.
define var del_nromov_lote as integer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES lotes_aceite
&Scoped-define FIRST-EXTERNAL-TABLE lotes_aceite


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lotes_aceite.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS lotes_aceite.Peso_neto ~
lotes_aceite.id_empresa lotes_aceite.fecha lotes_aceite.id_sucursal ~
lotes_aceite.id_lote lotes_aceite.Fecha_comienzo ~
lotes_aceite.Fecha_finalizacion lotes_aceite.id_articulo ~
lotes_aceite.citral lotes_aceite.id_envase 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}Peso_neto ~{&FP2}Peso_neto ~{&FP3}~
 ~{&FP1}id_empresa ~{&FP2}id_empresa ~{&FP3}~
 ~{&FP1}fecha ~{&FP2}fecha ~{&FP3}~
 ~{&FP1}id_sucursal ~{&FP2}id_sucursal ~{&FP3}~
 ~{&FP1}id_lote ~{&FP2}id_lote ~{&FP3}~
 ~{&FP1}Fecha_comienzo ~{&FP2}Fecha_comienzo ~{&FP3}~
 ~{&FP1}Fecha_finalizacion ~{&FP2}Fecha_finalizacion ~{&FP3}~
 ~{&FP1}id_articulo ~{&FP2}id_articulo ~{&FP3}~
 ~{&FP1}citral ~{&FP2}citral ~{&FP3}~
 ~{&FP1}id_envase ~{&FP2}id_envase ~{&FP3}
&Scoped-define ENABLED-TABLES lotes_aceite
&Scoped-define FIRST-ENABLED-TABLE lotes_aceite
&Scoped-Define DISPLAYED-FIELDS lotes_aceite.Peso_neto ~
lotes_aceite.id_empresa lotes_aceite.fecha lotes_aceite.id_sucursal ~
lotes_aceite.id_lote lotes_aceite.Fecha_comienzo ~
lotes_aceite.Fecha_finalizacion lotes_aceite.id_articulo ~
lotes_aceite.citral lotes_aceite.id_envase 
&Scoped-Define DISPLAYED-OBJECTS fi-empresas-razon_social ~
fi-sucursales-nombre fi-productos_terminados-descrip ~
fi-envases_prod-descripcion 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS lotes_aceite.id_empresa ~
lotes_aceite.id_sucursal lotes_aceite.id_lote 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_tipotambor||y|general.lotes_aceite.id_tipotambor
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
DEFINE VARIABLE fi-empresas-razon_social AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-sucursales-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     lotes_aceite.Peso_neto AT ROW 7.91 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     lotes_aceite.id_empresa AT ROW 1.86 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     fi-empresas-razon_social AT ROW 1.86 COL 53 COLON-ALIGNED NO-LABEL
     lotes_aceite.fecha AT ROW 1.86 COL 126 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     fi-sucursales-nombre AT ROW 2.81 COL 65 COLON-ALIGNED NO-LABEL
     lotes_aceite.id_sucursal AT ROW 2.86 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lotes_aceite.id_lote AT ROW 3.86 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lotes_aceite.Fecha_comienzo AT ROW 4.91 COL 48 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lotes_aceite.Fecha_finalizacion AT ROW 4.81 COL 87 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     lotes_aceite.id_articulo AT ROW 5.86 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lotes_aceite.citral AT ROW 5.76 COL 112 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     fi-productos_terminados-descrip AT ROW 5.86 COL 65 COLON-ALIGNED NO-LABEL
     lotes_aceite.id_envase AT ROW 6.86 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-envases_prod-descripcion AT ROW 6.81 COL 55 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.lotes_aceite
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
         HEIGHT             = 7.91
         WIDTH              = 144.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lotes_aceite.fecha IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN lotes_aceite.Fecha_comienzo IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN lotes_aceite.Fecha_finalizacion IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN fi-empresas-razon_social IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sucursales-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lotes_aceite.id_empresa IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN lotes_aceite.id_lote IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN lotes_aceite.id_sucursal IN FRAME F-Main
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
general.lotes_aceite.id_envase;wc_envases.w;envases_prod.descripcion;;
general.lotes_aceite.id_sucursal;wc_sucursales.w;sucursales.nombre;;
general.lotes_aceite.id_empresa;wc_empreas.w;empresas.razon_social;;
general.lotes_aceite.id_articulo;wc_productos_terminados.w;productos_terminados.descripcion;;
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

&Scoped-define SELF-NAME lotes_aceite.citral
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.citral V-table-Win
ON LEAVE OF lotes_aceite.citral IN FRAME F-Main /* Citral */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.fecha V-table-Win
ON LEAVE OF lotes_aceite.fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.Fecha_comienzo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.Fecha_comienzo V-table-Win
ON LEAVE OF lotes_aceite.Fecha_comienzo IN FRAME F-Main /* Fecha_comienzo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.Fecha_finalizacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.Fecha_finalizacion V-table-Win
ON LEAVE OF lotes_aceite.Fecha_finalizacion IN FRAME F-Main /* Fecha_finalizacion */
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


&Scoped-define SELF-NAME fi-sucursales-nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sucursales-nombre V-table-Win
ON LEAVE OF fi-sucursales-nombre IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_articulo V-table-Win
ON GO OF lotes_aceite.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_articulo V-table-Win
ON LEAVE OF lotes_aceite.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_aceite.id_articulo IN FRAME F-Main /* Art�culo */
do: 
define var r as rowid no-undo.
run wc_productos_terminados.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.lotes_aceite.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_articulo V-table-Win
ON U1 OF lotes_aceite.id_articulo IN FRAME F-Main /* Art�culo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_empresa V-table-Win
ON GO OF lotes_aceite.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_empresa V-table-Win
ON LEAVE OF lotes_aceite.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_aceite.id_empresa IN FRAME F-Main /* Empresa */
do: 
define var r as rowid no-undo.
run wc_empreas.w(output r).
find empresas where rowid(empresas) = r no-lock no-error.
if available empresas then 
general.lotes_aceite.id_empresa:screen-value = string(empresas.id_empresa).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_empresa V-table-Win
ON U1 OF lotes_aceite.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_envase V-table-Win
ON GO OF lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_envase V-table-Win
ON LEAVE OF lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_envase V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_aceite.id_envase IN FRAME F-Main /* Envase */
do: 
define var r as rowid no-undo.
run wc_envases.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
general.lotes_aceite.id_envase:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_envase V-table-Win
ON U1 OF lotes_aceite.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_lote V-table-Win
ON LEAVE OF lotes_aceite.id_lote IN FRAME F-Main /* Lote */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.id_sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_sucursal V-table-Win
ON GO OF lotes_aceite.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_sucursal V-table-Win
ON LEAVE OF lotes_aceite.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_sucursal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF lotes_aceite.id_sucursal IN FRAME F-Main /* Sucursal */
do: 
define var r as rowid no-undo.
run wc_sucursales.w(output r).
find sucursales where rowid(sucursales) = r no-lock no-error.
if available sucursales then 
general.lotes_aceite.id_sucursal:screen-value = string(sucursales.id_sucursal).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.id_sucursal V-table-Win
ON U1 OF lotes_aceite.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lotes_aceite.Peso_neto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lotes_aceite.Peso_neto V-table-Win
ON LEAVE OF lotes_aceite.Peso_neto IN FRAME F-Main /* Peso tambor */
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
assign lotes_aceite.id_tipotambor   = 6
       lotes_aceite.nromov          = next-value(nromov)
       lotes_aceite.anio            = year(date(lotes_aceite.fecha:screen-value in frame F-Main))
       lotes_aceite.estado_lote     = 1.
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
/*    message "OJO que estoy por borrar todas las tablas relacionadas!!!!!!" view-as alert-box.*/
/*******************************************************************************/
/********************BORRO TODAS LAS TABLAS ASOCIADAS***************************/

find sobrante_lotes_aceite where sobrante_lotes_aceite.id_empresa = del_empresa_lote and
                                 sobrante_lotes_aceite.id_sucursal = del_sucursal_lote and
                                 sobrante_lotes_aceite.id_tipotambor = del_tipotambor_lote and
                                 sobrante_lotes_aceite.nromov = del_nromov_lote no-error.
    if available sobrante_lotes_aceite then
    do:
        
        del_empresa = sobrante_lotes_aceite.id_empresa.
        del_sucursal = sobrante_lotes_aceite.id_sucursal.
        del_tipotambor = sobrante_lotes_aceite.id_tipotambor_sobrante.
        del_nromov = sobrante_lotes_aceite.nromov_sobrante.
        
        For each tambores_industria where tambores_industria.id_empresa = del_empresa and
                                          tambores_industria.id_sucursal = del_sucursal and
                                          tambores_industria.id_tipotambor = del_tipotambor and
                                          tambores_industria.nromov = del_nromov.
                delete tambores_industria.
        end.
        
        
        delete sobrante_lotes_aceite.
    end.



for each composicion_lote_aceite where composicion_lote_aceite.id_empresa = del_empresa_lote and
                                composicion_lote_aceite.id_sucursal = del_sucursal_lote and
                                composicion_lote_aceite.id_tipotambor = del_tipotambor_lote and
                                composicion_lote_aceite.nromov = del_nromov_lote.

    delete composicion_lote_aceite.
end.


for each tambores_industria where tambores_industria.id_empresa = del_empresa_lote and
                                  tambores_industria.id_sucursal = del_sucursal_lote and
                                  tambores_industria.id_tipotambor = del_tipotambor_lote and
                                  tambores_industria.nromov = del_nromov_lote.

    delete tambores_industria.
end.

for each tambores_industria where tambores_industria.id_empresa_destino = del_empresa_lote and
                                  tambores_industria.id_sucursal_destino = del_sucursal_lote and
                                  tambores_industria.id_tipotambor_destino = del_tipotambor_lote and
                                  tambores_industria.nromov_destino = del_nromov_lote.

    tambores_industria.id_empresa_destino = 0.
    tambores_industria.id_sucursal_destino = 0.
    tambores_industria.id_tipotambor_destino = 0.
    tambores_industria.nromov_destino =  0.                           
    
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
find lotes_aceite where rowid(lotes_aceite) = r no-lock no-error.

if available lotes_aceite then
    do:
        for each tambores_industria of lotes_aceite.
                assign tambores_industria.id_articulo   = lotes_aceite.id_articulo
                       tambores_industria.fecha         = lotes_aceite.fecha
                       tambores_industria.id_lote       = lotes_aceite.id_lote
                       tambores_industria.id_envase     = lotes_aceite.id_envase
                       tambores_industria.kilos_tambor  = lotes_aceite.peso_neto.
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

/******************************************************************************/
/*************************ALMACENO LOS ID DEL REGISTRO A BORRAR****************/
run get-container (output hcontainer).
RUN get-rowid-cabecera in hcontainer (output r).

find lotes_aceite where rowid(lotes_aceite) = r no-error.
if available lotes_aceite then
do:      
    del_empresa_lote     = lotes_aceite.id_empresa.
    del_sucursal_lote    = lotes_aceite.id_sucursal.
    del_tipotambor_lote  = lotes_aceite.id_tipotambor.
    del_nromov_lote      = lotes_aceite.nromov.
    del_lote             = lotes_aceite.id_lote.
    del_fecha            = lotes_aceite.fecha.
    del_articulo         = lotes_aceite.id_articulo.
    
    existe = false.
    find sobrante_lotes_aceite where sobrante_lotes_aceite.id_empresa = lotes_aceite.id_empresa
                                 and sobrante_lotes_aceite.id_sucursal = lotes_aceite.id_sucursal
                                 and sobrante_lotes_aceite.id_tipotambor = lotes_aceite.id_tipotambor
                                 and sobrante_lotes_aceite.nromov = lotes_aceite.nromov no-error.
    if available sobrante_lotes_aceite then
    do:
        
        del_empresa = sobrante_lotes_aceite.id_empresa.
        del_sucursal = sobrante_lotes_aceite.id_sucursal.
        del_tipotambor = sobrante_lotes_aceite.id_tipotambor_sobrante.
        del_nromov = sobrante_lotes_aceite.nromov_sobrante.
        
        For each tambores_industria where tambores_industria.id_empresa = del_empresa and
                                          tambores_industria.id_sucursal = del_sucursal and
                                          tambores_industria.id_tipotambor = del_tipotambor and
                                          tambores_industria.nromov = del_nromov.

             if tambores_industria.id_empresa_destino <> 0 and
                tambores_industria.id_sucursal_destino <> 0 and
                tambores_industria.id_tipotambor_destino <> 0 and
                tambores_industria.nromov_destino <> 0 then existe = true.
        end.
        
    end.
end.        
if existe then 
 do:
    message "No puede borrar el lote porque el sobrante del mismo esta siendo utilizado" view-as alert-box.
    return "ADM-ERROR".            
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
  {src/adm/template/row-list.i "lotes_aceite"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "lotes_aceite"}

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
find first envases_prod where envases_prod.id_envase = integer(lotes_aceite.id_envase:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion:screen-value in frame F-Main = ''.

find first sucursales where sucursales.id_sucursal = integer(lotes_aceite.id_sucursal:screen-value in frame F-Main)  no-lock no-error .
if available sucursales then 
fi-sucursales-nombre:screen-value in frame F-Main = string(sucursales.nombre).
else
fi-sucursales-nombre:screen-value in frame F-Main = ''.

find first empresas where empresas.id_empresa = integer(lotes_aceite.id_empresa:screen-value in frame F-Main)  no-lock no-error .
if available empresas then 
fi-empresas-razon_social:screen-value in frame F-Main = string(empresas.razon_social).
else
fi-empresas-razon_social:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(lotes_aceite.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_envase,id_sucursal,id_empresa,id_articulo".
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
  {src/adm/template/sndkycas.i "id_tipotambor" "lotes_aceite" "id_tipotambor"}

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
  {src/adm/template/snd-list.i "lotes_aceite"}

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
         
/*    when "id_lote" then
      do: 
             
        define buffer lote for lotes_aceite.
        find lote where lote.id_lote = integer(lotes_aceite.id_lote:screen-value in frame F-Main)
                    and year(lote.fecha) = year(date(lotes_aceite.fecha:screen-value in frame F-Main)) 
                    and lote.id_articulo = lotes_aceite.peso_neto:screen-value in frame F-Main) no-lock no-error.
         if available lote then 
            do:
                mensaje = "Debe ingresar un Lote no existente!. Por favor revise si ya existe el lote ingresado.".
                return false.
            end.

        
      end. */
    when "peso_neto" then
      do: 
        if integer(lotes_aceite.peso_neto:screen-value in frame F-Main) <= 0 then 
            do:
                mensaje = "Por favor cargue los kilos de un tambor(unitario). Ej: 180 kg.".
                return false.
            end.

        
      end.
  end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

