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
DEFINE VAR vDesde      AS INTEGER NO-UNDO.
DEFINE VAR vHasta      AS INTEGER NO-UNDO.
DEFINE VAR vCantidad   AS INTEGER NO-UNDO.
DEFINE VAR vSucursal   AS INTEGER NO-UNDO.
DEFINE VAR vProduccion AS INTEGER NO-UNDO.
DEFINE VAR vNroMov     AS INTEGER NO-UNDO.
DEFINE VAR vEmpresa    AS INTEGER NO-UNDO.

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
&Scoped-Define ENABLED-FIELDS produccion_jugo.id_sucursal ~
produccion_jugo.anio produccion_jugo.id_produccion produccion_jugo.Fecha ~
produccion_jugo.kilos_1 produccion_jugo.cantidad_1 produccion_jugo.id_turno ~
produccion_jugo.desde_bolsa produccion_jugo.hasta_bolsa ~
produccion_jugo.id_articulo produccion_jugo.id_envase_1 
&Scoped-define ENABLED-TABLES produccion_jugo
&Scoped-define FIRST-ENABLED-TABLE produccion_jugo
&Scoped-Define DISPLAYED-FIELDS produccion_jugo.id_sucursal ~
produccion_jugo.anio produccion_jugo.id_produccion produccion_jugo.Fecha ~
produccion_jugo.kilos_1 produccion_jugo.cantidad_1 produccion_jugo.id_turno ~
produccion_jugo.desde_bolsa produccion_jugo.hasta_bolsa ~
produccion_jugo.id_articulo produccion_jugo.id_envase_1 
&Scoped-define DISPLAYED-TABLES produccion_jugo
&Scoped-define FIRST-DISPLAYED-TABLE produccion_jugo
&Scoped-Define DISPLAYED-OBJECTS fi-productos_terminados-descrip ~
fi-envases_prod-descripcion 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS produccion_jugo.anio ~
produccion_jugo.id_produccion 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
id_articulo||y|general.produccion_jugo.id_articulo
id_tipotambor||y|general.produccion_jugo.id_tipotambor
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_articulo,id_tipotambor"':U).
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
DEFINE VARIABLE fi-envases_prod-descripcion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-productos_terminados-descrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     produccion_jugo.id_sucursal AT ROW 1 COL 14 COLON-ALIGNED
          LABEL "Sucursal" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     produccion_jugo.anio AT ROW 1 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     produccion_jugo.id_produccion AT ROW 2.19 COL 14 COLON-ALIGNED
          LABEL "Produccion" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     produccion_jugo.Fecha AT ROW 2.19 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.kilos_1 AT ROW 3.38 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     produccion_jugo.cantidad_1 AT ROW 3.38 COL 58 COLON-ALIGNED
          LABEL "Cantidad" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     produccion_jugo.id_turno AT ROW 4.57 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     produccion_jugo.desde_bolsa AT ROW 4.57 COL 38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY .95
     produccion_jugo.hasta_bolsa AT ROW 4.57 COL 57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     produccion_jugo.id_articulo AT ROW 5.76 COL 14 COLON-ALIGNED
          LABEL "Artículo" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     fi-productos_terminados-descrip AT ROW 5.76 COL 21 COLON-ALIGNED NO-LABEL
     produccion_jugo.id_envase_1 AT ROW 6.95 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     fi-envases_prod-descripcion AT ROW 6.95 COL 21 COLON-ALIGNED NO-LABEL
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
         HEIGHT             = 6.95
         WIDTH              = 76.2.
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

/* SETTINGS FOR FILL-IN produccion_jugo.anio IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN produccion_jugo.cantidad_1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi-envases_prod-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN produccion_jugo.id_articulo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN produccion_jugo.id_produccion IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN produccion_jugo.id_sucursal IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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
general.produccion_jugo.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
general.produccion_jugo.id_envase_1;wc_envases.w;envases_prod.descripcion;id_envase;
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

&Scoped-define SELF-NAME produccion_jugo.anio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.anio V-table-Win
ON LEAVE OF produccion_jugo.anio IN FRAME F-Main /* Año */
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


&Scoped-define SELF-NAME produccion_jugo.desde_bolsa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.desde_bolsa V-table-Win
ON LEAVE OF produccion_jugo.desde_bolsa IN FRAME F-Main /* Desde bolsa */
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


&Scoped-define SELF-NAME produccion_jugo.hasta_bolsa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.hasta_bolsa V-table-Win
ON LEAVE OF produccion_jugo.hasta_bolsa IN FRAME F-Main /* hasta */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_articulo V-table-Win
ON GO OF produccion_jugo.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_articulo V-table-Win
ON LEAVE OF produccion_jugo.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF produccion_jugo.id_articulo IN FRAME F-Main /* Artículo */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.produccion_jugo.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_articulo V-table-Win
ON U1 OF produccion_jugo.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_envase_1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_1 V-table-Win
ON GO OF produccion_jugo.id_envase_1 IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_1 V-table-Win
ON LEAVE OF produccion_jugo.id_envase_1 IN FRAME F-Main /* Envase */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_envase_1 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF produccion_jugo.id_envase_1 IN FRAME F-Main /* Envase */
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
ON U1 OF produccion_jugo.id_envase_1 IN FRAME F-Main /* Envase */
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
ON LEAVE OF produccion_jugo.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME produccion_jugo.id_turno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL produccion_jugo.id_turno V-table-Win
ON LEAVE OF produccion_jugo.id_turno IN FRAME F-Main /* CodTurno */
DO:
{custom/support/validacion.i}
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
  Purpose:     Completar valores que faltan en la cabecera de produccion_jugo y 
               Crear las bolsas de cascara en tambores_industria
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR i           AS INTEGER   NO-UNDO.
DEFINE VAR vExisten    AS INTEGER   NO-UNDO.
DEFINE VAR vAntes      AS INTEGER   NO-UNDO.
DEFINE VAR vDespues    AS INTEGER   NO-UNDO.
DEFINE VAR vTiempo     AS CHARACTER NO-UNDO.

DEFINE VAR vLastId     AS INTEGER NO-UNDO.


vSucursal = INTEGER(produccion_jugo.id_sucursal:SCREEN-VALUE IN FRAME F-Main).
ASSIGN produccion_jugo.id_empresa       = 1
       produccion_jugo.id_tipotambor    = 11
       produccion_jugo.nromov           = NEXT-VALUE(nromov)
       produccion_jugo.id_sucursal      = vSucursal.

/*obtengo el ultimo id de bolsa*/

vLastId = 0.

FOR EACH produccion_jugo WHERE produccion_jugo.id_sucursal = vSucursal 
                           AND produccion_jugo.id_tipotambor = 11
                         BY produccion_jugo.hasta_bolsa DESC.
  vLastId = produccion_jugo.hasta_bolsa.
  LEAVE.
END.

/* no hago el foreach en tambores industria porque demora 10" y en produccion jugo demora 0"
FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_tipotambor = 11
                                      AND tambores_industria.id_sucursal = vSucursal
                                    BY tambores_industria.id_tambor DESC.
  vLastId = tambores_industria.id_tambor.
  LEAVE.
END.
*/




vDesde      = vLastId + 1.
vHasta      = vLastId + INTEGER(cantidad_1:SCREEN-VALUE IN FRAME F-Main).
desde_bolsa:SCREEN-VALUE IN FRAME F-Main = STRING(vDesde).
hasta_bolsa:SCREEN-VALUE IN FRAME F-Main = STRING(vHasta).
ASSIGN produccion_jugo.desde_bolsa = vDesde
       produccion_jugo.hasta_bolsa = vHasta.

vProduccion = INTEGER(produccion_jugo.id_produccion:SCREEN-VALUE IN FRAME F-Main).

/*
/* VALIDAR QUE REALMENTE NO EXISTEN ESAS BOLSAS */
FOR EACH tambores_industria WHERE tambores_industria.id_tipotambor = 11
                              AND tambores_industria.id_empresa    = 1
                              AND tambores_industria.id_sucursal   = vSucursal
                              AND tambores_industria.id_tambor     >= vDesde 
                              AND tambores_industria.id_tambor     <= vHasta
                            NO-LOCK.
  vExisten = vExisten + 1.
END.
*/
/* comentado porque lo pase al appserver*/
/*
vExisten = 0.
IF vExisten <= 0 THEN DO:
  DO i = vDesde TO vHasta.
    CREATE tambores_industria.
    ASSIGN
      tambores_industria.id_empresa             = produccion_jugo.id_empresa
      tambores_industria.id_sucursal            = vSucursal
      tambores_industria.id_tipotambor          = 11
      tambores_industria.nromov                 = produccion_jugo.nromov
      tambores_industria.id_tambor              = i

      tambores_industria.id_articulo            = INTEGER(produccion_jugo.id_articulo:SCREEN-VALUE IN FRAME F-Main)
      tambores_industria.id_envase              = INTEGER(produccion_jugo.id_envase_1:SCREEN-VALUE IN FRAME F-Main)
      tambores_industria.id_etiqueta            = IF produccion_jugo.id_sucursal = 96 THEN NEXT-VALUE(tambores) ELSE NEXT-VALUE(tambores_famailla)
      tambores_industria.fecha_cierre           = DATE(produccion_jugo.fecha:SCREEN-VALUE IN FRAME F-Main)

      tambores_industria.id_empresa_destino     = 0
      tambores_industria.id_sucursal_destino    = 0
      tambores_industria.id_tipotambor_destino  = 0
      tambores_industria.nromov_destino         = 0
      
      tambores_industria.id_empresa_ubicacion   = 1
      tambores_industria.id_sucursal_ubicacion  = vSucursal
      tambores_industria.id_locacion_ubicacion  = 4
      tambores_industria.id_posicion_ubicacion  = 1
      
      tambores_industria.c_usuario              = USERID("userdb")
      tambores_industria.c_fecha                = TODAY
      tambores_industria.c_hora                 = STRING(TIME,"HH:MM:SS")

      tambores_industria.kilos_tambor           = INTEGER(produccion_jugo.kilos_1:SCREEN-VALUE IN FRAME F-Main)
      tambores_industria.anio                   = INTEGER(produccion_jugo.anio:SCREEN-VALUE IN FRAME F-Main)

    .
      RELEASE tambores_industria.
  END. /*do*/

  */


  vAntes = TIME.
  /*registro de movimientos de stock*/
  DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
  DEFINE VAR ret      AS LOGICAL NO-UNDO.

  CREATE SERVER hAppSrv.
  
  ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").
  
  RUN appCascInsertTamboresIndustria.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT vDesde, 
                                                                               INPUT vHasta, 
                                                                               INPUT vSucursal, 
                                                                               INPUT INTEGER(produccion_jugo.kilos_1:SCREEN-VALUE IN FRAME F-Main), 
                                                                               INPUT INTEGER(produccion_jugo.anio:SCREEN-VALUE IN FRAME F-Main), 
                                                                               INPUT produccion_jugo.id_empresa, 
                                                                               INPUT INTEGER(produccion_jugo.id_articulo:SCREEN-VALUE IN FRAME F-Main), 
                                                                               INPUT produccion_jugo.nromov, 
                                                                               INPUT INTEGER(produccion_jugo.id_envase_1:SCREEN-VALUE IN FRAME F-Main), 
                                                                               INPUT DATE(produccion_jugo.fecha:SCREEN-VALUE IN FRAME F-Main)) NO-ERROR.


  IF ERROR-STATUS:ERROR THEN DO:
    ret = hAppSrv:DISCONNECT().
    RETURN NO-APPLY RETURN-VALUE.
  END.
  
  RUN y_gstkcre_cas.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT 1,
                                                              INPUT vSucursal,
                                                              INPUT 11,
                                                              INPUT vDesde,
                                                              INPUT vHasta,
                                                              INPUT 18) NO-ERROR.
  
  IF ERROR-STATUS:ERROR THEN DO:
    ret = hAppSrv:DISCONNECT().
    RETURN NO-APPLY RETURN-VALUE.
  END.

  ret = hAppSrv:DISCONNECT().
  DELETE OBJECT hAppSrv.
  /*fin registro stock*/
  
  vDespues = TIME.

  vTiempo = STRING(vDespues - vAntes, "HH:MM:SS").
  MESSAGE "Se crearon las bolsas satisfactoriamente! en " +  string(vTiempo) VIEW-AS ALERT-BOX.   

/*
END. /*if vExisten*/

ELSE DO:
  MESSAGE "Alguna de las bolsas comprendidas en el intervalo y estan asociadas a otro lote" VIEW-AS ALERT-BOX.
END.
*/


    
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

/*colocar aqui el codigo para borrar los registros de tambores_industria */

  /*registro de movimientos de stock*/
  DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
  DEFINE VAR ret      AS LOGICAL NO-UNDO.
  CREATE SERVER hAppSrv.
  
  ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").
  
  RUN y_gstkcre_cas.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT 1,
                                                              INPUT vSucursal,
                                                              INPUT 11,
                                                              INPUT vDesde,
                                                              INPUT vHasta,
                                                              INPUT 2).
  
  IF ERROR-STATUS:ERROR THEN DO:
    ret = hAppSrv:DISCONNECT().
    RETURN NO-APPLY RETURN-VALUE.
  END.
  /*fin registro stock*/
  /*elimino en tambores_industria*/
  
  RUN appCascDeleteTamboresIndustria.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT vEmpresa, 
                                                                               INPUT vSucursal, 
                                                                               INPUT vNroMov) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    ret = hAppSrv:DISCONNECT().
    RETURN NO-APPLY RETURN-VALUE.
  END.

  ret = hAppSrv:DISCONNECT().
  DELETE OBJECT hAppSrv.
  

  /*
  FOR EACH tambores_industria WHERE id_empresa    =  vEmpresa
                                AND id_sucursal   =  vSucursal
                                AND id_tipotambor =  11
                                AND nromov        =  vNroMov.
                            /*  AND id_tambor     >= vDesde
                                AND id_tambor     <= vHasta*/
    DELETE tambores_industria.
  END.
  */

  MESSAGE "Se borraron las bolsas satisfactoriamente!" VIEW-AS ALERT-BOX.

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

/***** PREGUNTAR A ADRIAN QUE HACE ACA *******/


/*
DEFINE VAR v_desde AS INTEGER.
DEFINE VAR v_hasta AS INTEGER.
DEFINE VAR i AS INTEGER.

v_desde = INTEGER(produccion_jugo.desde_bolsa:SCREEN-VALUE IN FRAME F-Main).
v_hasta = INTEGER(produccion_jugo.hasta_bolsa:SCREEN-VALUE IN FRAME F-Main).

DO i = v_desde TO v_hasta.
    CREATE tambores_industria.

END.

*/
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
vEmpresa  = produccion_jugo.id_empresa.
vSucursal = produccion_jugo.id_sucursal.
vNroMov   = produccion_jugo.nromov.
vDesde    = produccion_jugo.desde_bolsa.
vHasta    = produccion_jugo.hasta_bolsa.


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
define var lista_relacion as character no-undo initial "id_articulo,id_envase_1".
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

  desde_bolsa:HIDDEN IN FRAME F-Main = TRUE.
  hasta_bolsa:HIDDEN IN FRAME F-Main = TRUE.

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
produccion_jugo.kilos_1:SCREEN-VALUE IN FRAME F-Main = "50".
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
  {src/adm/template/sndkycas.i "id_articulo" "produccion_jugo" "id_articulo"}
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

