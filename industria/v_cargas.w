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

DEFINE VAR v_del_emp AS INTEGER.
DEFINE VAR v_del_suc AS INTEGER.
DEFINE VAR v_del_tip AS INTEGER.
DEFINE VAR v_del_nro AS INTEGER.

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
&Scoped-define EXTERNAL-TABLES cargas
&Scoped-define FIRST-EXTERNAL-TABLE cargas


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cargas.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cargas.id_empresa cargas.Bx_20_20 ~
cargas.id_sucursal cargas.Bx_correg cargas.id_carga cargas.Acidez_w_v ~
cargas.id_articulo cargas.Acidez_w_w cargas.litros cargas.Pulpa ~
cargas.id_tanque cargas.Sodio 
&Scoped-define ENABLED-TABLES cargas
&Scoped-define FIRST-ENABLED-TABLE cargas
&Scoped-Define DISPLAYED-FIELDS cargas.id_empresa cargas.Bx_20_20 ~
cargas.id_sucursal cargas.Bx_correg cargas.Fecha cargas.id_carga ~
cargas.Acidez_w_v cargas.id_articulo cargas.Acidez_w_w cargas.litros ~
cargas.Pulpa cargas.id_tanque cargas.Sodio 
&Scoped-define DISPLAYED-TABLES cargas
&Scoped-define FIRST-DISPLAYED-TABLE cargas
&Scoped-Define DISPLAYED-OBJECTS fi-empresas-razon_social ~
fi-sucursales-nombre fi-productos_terminados-descrip 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS cargas.id_empresa cargas.id_sucursal ~
cargas.id_carga 

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
DEFINE VARIABLE fi-empresas-razon_social AS CHARACTER FORMAT "X(256)":U 
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
     cargas.id_empresa AT ROW 1 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     fi-empresas-razon_social AT ROW 1 COL 19 COLON-ALIGNED NO-LABEL
     cargas.Bx_20_20 AT ROW 1 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi-sucursales-nombre AT ROW 1.95 COL 20 COLON-ALIGNED NO-LABEL
     cargas.id_sucursal AT ROW 2 COL 14 COLON-ALIGNED
          LABEL "Sucursal"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     cargas.Bx_correg AT ROW 2 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     cargas.Fecha AT ROW 2.91 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     cargas.id_carga AT ROW 3 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     cargas.Acidez_w_v AT ROW 3 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     cargas.id_articulo AT ROW 3.86 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi-productos_terminados-descrip AT ROW 3.86 COL 31 COLON-ALIGNED NO-LABEL
     cargas.Acidez_w_w AT ROW 4 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     cargas.litros AT ROW 4.81 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     cargas.Pulpa AT ROW 5 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     cargas.id_tanque AT ROW 5.76 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     cargas.Sodio AT ROW 5.95 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.cargas
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
         HEIGHT             = 6.1
         WIDTH              = 104.6.
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

/* SETTINGS FOR FILL-IN cargas.Fecha IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-empresas-razon_social IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-productos_terminados-descrip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sucursales-nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cargas.id_carga IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN cargas.id_empresa IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN cargas.id_sucursal IN FRAME F-Main
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
general.cargas.id_empresa;wc_empreas.w;empresas.razon_social;;
general.cargas.id_sucursal;wc_sucursales.w;sucursales.nombre;;
general.cargas.id_articulo;wc_articulos.w;productos_terminados.descripcion;;
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

&Scoped-define SELF-NAME cargas.Acidez_w_v
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.Acidez_w_v V-table-Win
ON LEAVE OF cargas.Acidez_w_v IN FRAME F-Main /* Acidez_p/v */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.Acidez_w_w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.Acidez_w_w V-table-Win
ON LEAVE OF cargas.Acidez_w_w IN FRAME F-Main /* Acidez_p/p */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.Bx_20_20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.Bx_20_20 V-table-Win
ON LEAVE OF cargas.Bx_20_20 IN FRAME F-Main /* Bx_20/20 */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.Bx_correg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.Bx_correg V-table-Win
ON LEAVE OF cargas.Bx_correg IN FRAME F-Main /* Bx correg */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.Fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.Fecha V-table-Win
ON LEAVE OF cargas.Fecha IN FRAME F-Main /* Fecha */
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


&Scoped-define SELF-NAME cargas.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_articulo V-table-Win
ON GO OF cargas.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_articulo V-table-Win
ON LEAVE OF cargas.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_articulo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cargas.id_articulo IN FRAME F-Main /* Artículo */
do: 
define var r as rowid no-undo.
run wc_articulos.w(output r).
find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
if available productos_terminados then 
general.cargas.id_articulo:screen-value = string(productos_terminados.id_articulo).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_articulo V-table-Win
ON U1 OF cargas.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.id_carga
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_carga V-table-Win
ON LEAVE OF cargas.id_carga IN FRAME F-Main /* NroCarga */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.id_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_empresa V-table-Win
ON GO OF cargas.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_empresa V-table-Win
ON LEAVE OF cargas.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cargas.id_empresa IN FRAME F-Main /* Empresa */
do: 
define var r as rowid no-undo.
run wc_empreas.w(output r).
find empresas where rowid(empresas) = r no-lock no-error.
if available empresas then 
general.cargas.id_empresa:screen-value = string(empresas.id_empresa).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_empresa V-table-Win
ON U1 OF cargas.id_empresa IN FRAME F-Main /* Empresa */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.id_sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_sucursal V-table-Win
ON GO OF cargas.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_sucursal V-table-Win
ON LEAVE OF cargas.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_sucursal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cargas.id_sucursal IN FRAME F-Main /* Sucursal */
do: 
define var r as rowid no-undo.
run wc_sucursales.w(output r).
find sucursales where rowid(sucursales) = r no-lock no-error.
if available sucursales then 
general.cargas.id_sucursal:screen-value = string(sucursales.id_sucursal).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_sucursal V-table-Win
ON U1 OF cargas.id_sucursal IN FRAME F-Main /* Sucursal */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.id_tanque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.id_tanque V-table-Win
ON LEAVE OF cargas.id_tanque IN FRAME F-Main /* NroTanque */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.litros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.litros V-table-Win
ON LEAVE OF cargas.litros IN FRAME F-Main /* Litros */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.Pulpa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.Pulpa V-table-Win
ON LEAVE OF cargas.Pulpa IN FRAME F-Main /* Pulpa */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cargas.Sodio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cargas.Sodio V-table-Win
ON LEAVE OF cargas.Sodio IN FRAME F-Main /* Na */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-create V-table-Win 
PROCEDURE adm-post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign cargas.id_tipotambor = 10
       cargas.nromov        = next-value(nromov)
       cargas.fecha         = TODAY
       cargas.c_usuario     = userid("userdb")
       cargas.c_fecha       = today
       cargas.c_hora        = string(time,"HH:MM:SS")
       cargas.anio          = year(date(cargas.fecha:screen-value in frame F-Main)).
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
FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino      = v_del_emp 
                              AND tambores_industria.id_sucursal_destino     = v_del_suc 
                              AND tambores_industria.id_tipotambor_destino   = v_del_tip 
                              AND tambores_industria.nromov_destino          = v_del_nro 
                              .
    
    ASSIGN tambores_industria.id_empresa_destino    = 0
           tambores_industria.id_sucursal_destino   = 0 
           tambores_industria.id_tipotambor_destino = 0
           tambores_industria.nromov_destino        = 0
           tambores_industria.id_locacion_ubicacion = 4.

    RUN y_gstkcre.p (input tambores_industria.id_empresa,
                     input tambores_industria.id_sucursal,
                     input tambores_industria.id_tipotambor,
                     input tambores_industria.nromov,
                     INPUT tambores_industria.id_tambor,
                     INPUT tambores_industria.id_tambor,
                     input 17) "tambores_industria".
    
    IF RETURN-VALUE <> "" then do:
        MESSAGE "Error en el procesamiento de movimientos de stock" view-as alert-box.
        RETURN "ADM-ERROR".
    END.
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
IF AVAILABLE cargas THEN DO:
    v_del_emp   = cargas.id_empresa.
    v_del_suc   = cargas.id_sucursal.
    v_del_tip   = cargas.id_tipotambor.
    v_del_nro   = cargas.nromov.
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
  {src/adm/template/row-list.i "cargas"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cargas"}

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
find first empresas where empresas.id_empresa = integer(cargas.id_empresa:screen-value in frame F-Main)  no-lock no-error .
if available empresas then 
fi-empresas-razon_social:screen-value in frame F-Main = string(empresas.razon_social).
else
fi-empresas-razon_social:screen-value in frame F-Main = ''.

find first sucursales where sucursales.id_sucursal = integer(cargas.id_sucursal:screen-value in frame F-Main)  no-lock no-error .
if available sucursales then 
fi-sucursales-nombre:screen-value in frame F-Main = string(sucursales.nombre).
else
fi-sucursales-nombre:screen-value in frame F-Main = ''.

find first productos_terminados where productos_terminados.id_articulo = integer(cargas.id_articulo:screen-value in frame F-Main)  no-lock no-error .
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
define var lista_relacion as character no-undo initial "id_empresa,id_sucursal,id_articulo".
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
  {src/adm/template/snd-list.i "cargas"}

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

