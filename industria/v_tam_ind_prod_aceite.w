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

define buffer b_tam for tambores_industria.
DEFINE VAR FLAG-STOCK AS LOGICAL INITIAL FALSE.
DEFINE VAR v_nromov AS INTEGER.

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
&Scoped-define EXTERNAL-TABLES tambores_industria
&Scoped-define FIRST-EXTERNAL-TABLE tambores_industria


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tambores_industria.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tambores_industria.id_empresa ~
tambores_industria.id_sucursal tambores_industria.id_tambor ~
tambores_industria.id_articulo tambores_industria.id_envase ~
tambores_industria.kilos_tambor tambores_industria.Fecha ~
tambores_industria.kilos_agregados tambores_industria.Fecha_cierre ~
tambores_industria.citral tambores_industria.origen_water 
&Scoped-define ENABLED-TABLES tambores_industria
&Scoped-define FIRST-ENABLED-TABLE tambores_industria
&Scoped-Define DISPLAYED-FIELDS tambores_industria.id_empresa ~
tambores_industria.id_sucursal tambores_industria.id_tambor ~
tambores_industria.id_articulo tambores_industria.id_envase ~
tambores_industria.kilos_tambor tambores_industria.Fecha ~
tambores_industria.kilos_agregados tambores_industria.Fecha_cierre ~
tambores_industria.citral tambores_industria.origen_water 
&Scoped-define DISPLAYED-TABLES tambores_industria
&Scoped-define FIRST-DISPLAYED-TABLE tambores_industria
&Scoped-Define DISPLAYED-OBJECTS fi-empresas-razon_social ~
fi-sucursales-nombre fi-productos_terminados-descrip ~
fi-envases_prod-descripcion 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS tambores_industria.id_empresa ~
tambores_industria.id_sucursal tambores_industria.id_tambor 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_etiqueta||y|general.tambores_industria.id_etiqueta
id_tipotambor||y|general.tambores_industria.id_tipotambor
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_etiqueta,id_tipotambor"':U).
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
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-sucursales-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tambores_industria.id_empresa AT ROW 1 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     fi-empresas-razon_social AT ROW 1 COL 20 COLON-ALIGNED NO-LABEL
     fi-sucursales-nombre AT ROW 1.95 COL 26 COLON-ALIGNED NO-LABEL
     tambores_industria.id_sucursal AT ROW 2 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tambores_industria.id_tambor AT ROW 3 COL 15 COLON-ALIGNED FORMAT ">>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     fi-productos_terminados-descrip AT ROW 3.95 COL 27 COLON-ALIGNED NO-LABEL
     tambores_industria.id_articulo AT ROW 4 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     tambores_industria.id_envase AT ROW 5 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     fi-envases_prod-descripcion AT ROW 5 COL 27 COLON-ALIGNED NO-LABEL
     tambores_industria.kilos_tambor AT ROW 6 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     tambores_industria.Fecha AT ROW 6.05 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     tambores_industria.kilos_agregados AT ROW 7 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     tambores_industria.Fecha_cierre AT ROW 7.05 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     tambores_industria.citral AT ROW 8.05 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     tambores_industria.origen_water AT ROW 9.1 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.tambores_industria
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
         HEIGHT             = 9.19
         WIDTH              = 90.6.
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

/* SETTINGS FOR FILL-IN fi-empresas-razon_social IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sucursales-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tambores_industria.id_empresa IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tambores_industria.id_sucursal IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN tambores_industria.id_tambor IN FRAME F-Main
   1 EXP-FORMAT                                                         */
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
general.tambores_industria.id_empresa;wc_empreas.w;empresas.razon_social;;
general.tambores_industria.id_sucursal;wc_sucursales.w;sucursales.nombre;;
general.tambores_industria.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
general.tambores_industria.id_envase;wc_envases.w;envases_prod.descripcion;;
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

&Scoped-define SELF-NAME tambores_industria.citral
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.citral V-table-Win
ON LEAVE OF tambores_industria.citral IN FRAME F-Main /* Citral */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.Fecha V-table-Win
ON LEAVE OF tambores_industria.Fecha IN FRAME F-Main /* Fecha */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.Fecha_cierre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.Fecha_cierre V-table-Win
ON LEAVE OF tambores_industria.Fecha_cierre IN FRAME F-Main /* Fecha Cierre */
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


&Scoped-define SELF-NAME tambores_industria.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_articulo V-table-Win
ON GO OF tambores_industria.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_articulo V-table-Win
ON LEAVE OF tambores_industria.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tambores_industria.id_articulo IN FRAME F-Main /* Artículo */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.tambores_industria.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_articulo V-table-Win
ON U1 OF tambores_industria.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.id_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_empresa V-table-Win
ON GO OF tambores_industria.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_empresa V-table-Win
ON LEAVE OF tambores_industria.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tambores_industria.id_empresa IN FRAME F-Main /* Empresa */
do: 
define var r as rowid no-undo.
run wc_empreas.w(output r).
find empresas where rowid(empresas) = r no-lock no-error.
if available empresas then 
general.tambores_industria.id_empresa:screen-value = string(empresas.id_empresa).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_empresa V-table-Win
ON U1 OF tambores_industria.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.id_envase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_envase V-table-Win
ON GO OF tambores_industria.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_envase V-table-Win
ON LEAVE OF tambores_industria.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_envase V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tambores_industria.id_envase IN FRAME F-Main /* Envase */
do: 
define var r as rowid no-undo.
run wc_envases.w(output r).
find envases_prod where rowid(envases_prod) = r no-lock no-error.
if available envases_prod then 
general.tambores_industria.id_envase:screen-value = string(envases_prod.id_envase).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_envase V-table-Win
ON U1 OF tambores_industria.id_envase IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.id_sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_sucursal V-table-Win
ON GO OF tambores_industria.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_sucursal V-table-Win
ON LEAVE OF tambores_industria.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_sucursal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tambores_industria.id_sucursal IN FRAME F-Main /* Sucursal */
do: 
define var r as rowid no-undo.
run wc_sucursales.w(output r).
find sucursales where rowid(sucursales) = r no-lock no-error.
if available sucursales then 
general.tambores_industria.id_sucursal:screen-value = string(sucursales.id_sucursal).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_sucursal V-table-Win
ON U1 OF tambores_industria.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.id_tambor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.id_tambor V-table-Win
ON LEAVE OF tambores_industria.id_tambor IN FRAME F-Main /* Tambor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.kilos_agregados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.kilos_agregados V-table-Win
ON LEAVE OF tambores_industria.kilos_agregados IN FRAME F-Main /* lleno */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.kilos_tambor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.kilos_tambor V-table-Win
ON LEAVE OF tambores_industria.kilos_tambor IN FRAME F-Main /* Kilos tambor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tambores_industria.origen_water
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tambores_industria.origen_water V-table-Win
ON LEAVE OF tambores_industria.origen_water IN FRAME F-Main /* origen_water */
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
DEFINE VARIABLE iEtq AS INTEGER    NO-UNDO.
DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.

RUN libCommonFunctions.p PERSISTENT SET hLibCom.

IF INTEGER(tambores_industria.id_articulo:screen-value in frame F-Main) > 400 AND
   INTEGER(tambores_industria.kilos_tambor:screen-value in frame F-Main) > 0  THEN DO:

    FIND FIRST b_tam WHERE b_tam.anio        = YEAR(DATE(tambores_industria.fecha:screen-value in frame F-Main))
                       AND b_tam.id_sucursal = INTEGER(tambores_industria.id_sucursal:screen-value in frame F-Main)
                       AND b_tam.id_articulo = INTEGER(tambores_industria.id_articulo:screen-value in frame F-Main)
                       AND b_tam.id_tambor   = INTEGER(tambores_industria.id_tambor:screen-value in frame F-Main)
                    NO-LOCK NO-ERROR.
                 
    if available b_tam then do:
        message "El tambor " integer(tambores_industria.id_tambor:screen-value in frame F-Main)
                " con el articulo " integer(tambores_industria.id_articulo:screen-value in frame F-Main)
                " de la sucursal " integer(tambores_industria.id_sucursal:screen-value in frame F-Main)
                " del año " year(date(tambores_industria.fecha:screen-value in frame F-Main))
                " YA EXISTE!. Señor " userid("userdb") " pregunte si desea crear este tambor de todas maneras"
                view-as alert-box.
       return "ADM-ERROR".
    end.
    ELSE DO:
        v_nromov = next-value(nromov).
        iEtq = DYNAMIC-FUNCTION('getNextSequence' IN hLibCom, 'tambores_industria', 'id_etiqueta').

        ASSIGN tambores_industria.nromov                 = v_nromov
               tambores_industria.id_tipotambor          = 2
               tambores_industria.id_lote                = INTEGER(tambores_industria.id_tambor:screen-value in frame F-Main)
               tambores_industria.c_usuario              = USERID("userdb")
               tambores_industria.c_fecha                = TODAY
               tambores_industria.c_hora                 = STRING(time,"HH:MM:SS")
               tambores_industria.id_empresa_ubicacion   = 1
               tambores_industria.id_sucursal_ubicacion  = INTEGER(tambores_industria.id_sucursal:screen-value in frame F-Main)
               tambores_industria.id_locacion_ubicacion  = 4
               tambores_industria.id_posicion_ubicacion  = 1
               tambores_industria.anio                   = YEAR(DATE(tambores_industria.fecha:screen-value in frame F-Main))  
               tambores_industria.id_estado              = 6
               tambores_industria.id_etiqueta            = IF INTEGER(tambores_industria.id_sucursal:screen-value in frame F-Main) = 96 THEN
                                                           NEXT-VALUE(tambores) ELSE NEXT-VALUE(tambores_famailla).
        
        FLAG-STOCK = TRUE.
    END.

END.
ELSE DO:
    IF INTEGER(tambores_industria.id_articulo:screen-value in frame F-Main) <= 400 THEN DO:
        MESSAGE "Por favor ingrese un codigo de PRODUCCION,no ingrese codigos de PRODUCTOS TERMINADOS.Cualquier duda consultar con Sistemas."
            VIEW-AS ALERT-BOX.
        RETURN "ADM-Error".
    END.
    ELSE DO:
        MESSAGE "Debe ingresar el peso del tambor."
            VIEW-AS ALERT-BOX.
        RETURN "ADM-Error".
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
IF FLAG-STOCK  THEN DO:
    RUN y_gstkcre.p (input integer(tambores_industria.id_empresa:screen-value in frame F-Main),
                     input integer(tambores_industria.id_sucursal:screen-value in frame F-Main),
                     INPUT 2,
                     input v_nromov,
                     INPUT integer(tambores_industria.id_tambor:screen-value in frame F-Main),
                     INPUT integer(tambores_industria.id_tambor:screen-value in frame F-Main),
                     input 1) "tambores_industria".

    IF return-value <> "" then do:
        message "Error en el procesamiento de movimientos de stock" view-as alert-box.
        DELETE tambores_industria. 
    END.
    FLAG-STOCK = FALSE.
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
if integer(tambores_industria.kilos_tambor:screen-value in frame F-Main) <= 0 then
    do:
        message "Debe ingresar el peso del tambor." view-as alert-box.
        run dispatch ('cancel-record').
    end.
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
     RUN y_gstkcre.p (input tambores_industria.id_empresa,
                      input tambores_industria.id_sucursal,
                      input tambores_industria.id_tipotambor,
                      input tambores_industria.nromov,
                      INPUT tambores_industria.id_tambor,
                      INPUT tambores_industria.id_tambor,
                      input 2) "tambores_industria".

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
  {src/adm/template/row-list.i "tambores_industria"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tambores_industria"}

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
find first empresas where empresas.id_empresa = integer(tambores_industria.id_empresa:screen-value in frame F-Main)  no-lock no-error .
if available empresas then 
fi-empresas-razon_social:screen-value in frame F-Main = string(empresas.razon_social).
else
fi-empresas-razon_social:screen-value in frame F-Main = ''.

find first sucursales where sucursales.id_sucursal = integer(tambores_industria.id_sucursal:screen-value in frame F-Main)  no-lock no-error .
if available sucursales then 
fi-sucursales-nombre:screen-value in frame F-Main = string(sucursales.nombre).
else
fi-sucursales-nombre:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(tambores_industria.id_articulo:screen-value in frame F-Main)  no-lock no-error .
if available productos_terminados then 
fi-productos_terminados-descrip:screen-value in frame F-Main = string(productos_terminados.descripcion).
else
fi-productos_terminados-descrip:screen-value in frame F-Main = ''.

find first envases_prod where envases_prod.id_envase = integer(tambores_industria.id_envase:screen-value in frame F-Main)  no-lock no-error .
if available envases_prod then 
fi-envases_prod-descripcion:screen-value in frame F-Main = string(envases_prod.descripcion).
else
fi-envases_prod-descripcion:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_empresa,id_sucursal,id_articulo,id_envase".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-add V-table-Win 
PROCEDURE post-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
tambores_industria.fecha_cierre:screen-value in frame {&FRAME-NAME} = string(today).
tambores_industria.fecha:screen-value in frame {&FRAME-NAME} = string(today).
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
  {src/adm/template/sndkycas.i "id_etiqueta" "tambores_industria" "id_etiqueta"}
  {src/adm/template/sndkycas.i "id_tipotambor" "tambores_industria" "id_tipotambor"}

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
  {src/adm/template/snd-list.i "tambores_industria"}

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
    when "id_articulo" then
        if integer(valor) <= 400 then 
        do: /*
            mensaje = "Por favor ingrese un codigo de PRODUCCION,no ingrese codigos de PRODUCTOS TERMINADOS.Cualquier duda consultar con Sistemas.".
            return false. */
         end.
    when "kilos_tambor" then
        if integer(valor) <= 0 then 
        do: /*
            mensaje = "Debe ingresar el peso del tambor.".
            return false. */
         end.
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

