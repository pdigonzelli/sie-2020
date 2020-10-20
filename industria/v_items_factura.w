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

DEFINE TEMP-TABLE tt_tambores
    FIELD id_empresa     AS INTEGER  COLUMN-LABEL "Empresa"
    FIELD id_sucursal     AS INTEGER  COLUMN-LABEL "Sucursal"
    FIELD id_tipotambor     AS INTEGER  COLUMN-LABEL "Tipotambor"
    FIELD nromov     AS INTEGER  COLUMN-LABEL "Nromov"
    FIELD id_lote     AS INTEGER  COLUMN-LABEL "Lote"
    FIELD id_tambor     AS INTEGER  COLUMN-LABEL "Tambor"
    FIELD id_articulo   AS INTEGER  COLUMN-LABEL "Articulo"
    FIELD id_calidad    AS INTEGER  COLUMN-LABEL "Calidad"
    FIELD id_envase     AS INTEGER  COLUMN-LABEL "Envase"
    FIELD kilos_tambor  AS DECIMAL  COLUMN-LABEL "Peso Neto"
    FIELD anio          AS INTEGER  COLUMN-LABEL "Año"
    FIELD elegido       AS LOGICAL  COLUMN-LABEL "Seleccionado" FORMAT "SI/" INITIAL FALSE
    FIELD id_reg        AS ROWID.

DEFINE VAR p_lote AS INTEGER.
DEFINE VAR p_anio AS INTEGER.
DEFINE VAR v_kilos_total AS DECIMAL.
DEFINE VAR v_kilos_unitarios AS DECIMAL.
DEFINE VAR v_cantidad_bolsas AS INTEGER.

DEFINE VAR v_del_suc AS INTEGER.
DEFINE VAR v_del_tip AS INTEGER.
DEFINE VAR v_del_nro AS INTEGER.
DEFINE VAR v_del_ite AS INTEGER.

DEFINE VARIABLE viEmpresa    AS INTEGER    NO-UNDO.
DEFINE VARIABLE viSucursal   AS INTEGER    NO-UNDO.
DEFINE VARIABLE viTipoTambor AS INTEGER    NO-UNDO.
DEFINE VARIABLE viNroMov     AS INTEGER    NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES items_factura
&Scoped-define FIRST-EXTERNAL-TABLE items_factura


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR items_factura.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS items_factura.id_tipotambor ~
items_factura.id_articulo items_factura.cantidad items_factura.kilos 
&Scoped-define ENABLED-TABLES items_factura
&Scoped-define FIRST-ENABLED-TABLE items_factura
&Scoped-Define ENABLED-OBJECTS v_lote v_anio BUTTON-1 
&Scoped-Define DISPLAYED-FIELDS items_factura.id_tipotambor ~
items_factura.id_articulo items_factura.cantidad items_factura.kilos 
&Scoped-define DISPLAYED-TABLES items_factura
&Scoped-define FIRST-DISPLAYED-TABLE items_factura
&Scoped-Define DISPLAYED-OBJECTS fi-tipostambor-abreviatura v_lote v_anio 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS items_factura.id_tipotambor ~
items_factura.id_articulo items_factura.cantidad 

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
DEFINE BUTTON BUTTON-1 
     LABEL "Tambores" 
     SIZE 13 BY 1.

DEFINE VARIABLE fi-tipostambor-abreviatura AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE v_anio AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE v_lote AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     items_factura.id_tipotambor AT ROW 1 COL 11 COLON-ALIGNED
          LABEL "Tipotambor" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     fi-tipostambor-abreviatura AT ROW 1 COL 16 COLON-ALIGNED NO-LABEL
     items_factura.id_articulo AT ROW 1 COL 61 COLON-ALIGNED
          LABEL "Artículo" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     v_lote AT ROW 1 COL 73 COLON-ALIGNED
     v_anio AT ROW 1 COL 87 COLON-ALIGNED
     BUTTON-1 AT ROW 1 COL 97
     items_factura.cantidad AT ROW 1 COL 118 COLON-ALIGNED FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     items_factura.kilos AT ROW 1 COL 137 COLON-ALIGNED
          LABEL "Kilos Total" FORMAT ">>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: general.items_factura
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
         HEIGHT             = 1
         WIDTH              = 151.2.
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

/* SETTINGS FOR FILL-IN items_factura.cantidad IN FRAME F-Main
   1 EXP-FORMAT                                                         */
/* SETTINGS FOR FILL-IN fi-tipostambor-abreviatura IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN items_factura.id_articulo IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN items_factura.id_tipotambor IN FRAME F-Main
   1 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN items_factura.kilos IN FRAME F-Main
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
general.items_factura.id_tipotambor;wc_tipostambor.w;tipostambor.abreviatura;;
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

&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 V-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Tambores */
DO:
    DEFINE VAR hcon AS HANDLE.
    DEFINE VAR v_suc AS INTEGER.
    DEFINE VAR v_tip AS INTEGER.
    DEFINE VAR i AS INTEGER.
    DEFINE VAR v_r_remito AS ROWID.

    
    DEFINE VARIABLE viCantidad   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE viLote       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE viAnio       AS INTEGER    NO-UNDO.

    RUN get-container (OUTPUT hcon).
    RUN get-sucursal IN hcon (OUTPUT v_suc,
                              OUTPUT v_tip).
    RUN get-rowid-remito IN hcon (OUTPUT v_r_remito).

      FOR EACH tt_tambores.
          DELETE tt_tambores.
      END.
      IF INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main) <> 11 THEN DO:
          RUN wc_tambores_disponibles.w (INPUT INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main), /*tipotambor */
                                         INPUT INTEGER(items_factura.id_articulo:SCREEN-VALUE IN FRAME F-Main), /* articulo */ 
                                         INPUT INTEGER(v_lote:SCREEN-VALUE IN FRAME F-Main), /* lote */
                                         INPUT INTEGER(v_anio:SCREEN-VALUE IN FRAME F-Main), /* anio */
                                         INPUT v_suc, 
                                         INPUT v_r_remito, 
                                         INPUT-OUTPUT TABLE tt_tambores).
          i = 0.
          v_kilos_total = 0.
          FOR EACH tt_tambores WHERE elegido.
            i = i + 1.
            v_kilos_unitarios = tt_tambores.kilos_tambor.
            v_kilos_total =  v_kilos_total + tt_tambores.kilos_tambor.
          END.
      END.
      ELSE DO: /*cascara*/
        PROPATH = PROPATH + ";m:;src".
        RUN wGetLoteCascaraRemito.w (v_suc, 
                                     OUTPUT viEmpresa, 
                                     OUTPUT viSucursal, 
                                     OUTPUT viTipoTambor, 
                                     OUTPUT viNroMov, 
                                     OUTPUT viCantidad, 
                                     OUTPUT viLote, 
                                     OUTPUT viAnio).
        i = viCantidad.
        v_cantidad_bolsas = i.
        v_kilos_total = 50.
        v_kilos_unitarios = 50.
        v_lote = viLote.
        v_anio = viAnio.
        
        v_lote:SCREEN-VALUE IN FRAME F-Main                 = STRING(viLote).
        v_anio:SCREEN-VALUE IN FRAME F-Main                 = STRING(viAnio).
        items_factura.cantidad:SCREEN-VALUE IN FRAME F-Main = STRING(i).
        items_factura.kilos:SCREEN-VALUE IN FRAME F-Main    = STRING(v_kilos_total).
      END.      
      items_factura.cantidad:SCREEN-VALUE IN FRAME F-Main = STRING(i).
      items_factura.kilos:SCREEN-VALUE IN FRAME F-Main = STRING(v_kilos_total).
      
    /*END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_factura.cantidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_factura.cantidad V-table-Win
ON LEAVE OF items_factura.cantidad IN FRAME F-Main /* Cantidad */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipostambor-abreviatura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipostambor-abreviatura V-table-Win
ON LEAVE OF fi-tipostambor-abreviatura IN FRAME F-Main
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_factura.id_articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_factura.id_articulo V-table-Win
ON LEAVE OF items_factura.id_articulo IN FRAME F-Main /* Artículo */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_factura.id_tipotambor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_factura.id_tipotambor V-table-Win
ON GO OF items_factura.id_tipotambor IN FRAME F-Main /* Tipotambor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_factura.id_tipotambor V-table-Win
ON LEAVE OF items_factura.id_tipotambor IN FRAME F-Main /* Tipotambor */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_factura.id_tipotambor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF items_factura.id_tipotambor IN FRAME F-Main /* Tipotambor */
do: 
define var r as rowid no-undo.
run wc_tipostambor.w(output r).
find tipostambor where rowid(tipostambor) = r no-lock no-error.
if available tipostambor then 
general.items_factura.id_tipotambor:screen-value = string(tipostambor.id_tipotambor).
apply 'U1' to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_factura.id_tipotambor V-table-Win
ON U1 OF items_factura.id_tipotambor IN FRAME F-Main /* Tipotambor */
DO:
{custom/support/validacion.i}
     run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME items_factura.kilos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL items_factura.kilos V-table-Win
ON LEAVE OF items_factura.kilos IN FRAME F-Main /* Kilos Total */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_anio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_anio V-table-Win
ON LEAVE OF v_anio IN FRAME F-Main /* Año */
DO:
{custom/support/validacion.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_lote V-table-Win
ON LEAVE OF v_lote IN FRAME F-Main /* Lote */
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
DEFINE VAR v_r_remito AS ROWID.
DEFINE VAR hcon AS HANDLE.
DEFINE VAR v_item AS INTEGER.
DEFINE BUFFER bb_items FOR items_factura.
DEFINE BUFFER bb_remi FOR remitos.
DEFINE VAR v_desde AS INTEGER.
DEFINE VAR v_hasta AS INTEGER.
DEFINE VAR v_calidad AS INTEGER.
DEFINE VAR v_envase AS INTEGER.
DEFINE VAR vv_lote AS CHAR.
DEFINE VAR i AS INTEGER.
DEFINE VAR v_kgs_tambor AS DECIMAL.
DEFINE VAR vPesoTambor AS DECIMAL.
DEFINE VAR vPrimerItem AS INTEGER INITIAL 0.
DEFINE VAR vCantidadTamboresVentana AS INTEGER.
DEFINE VAR vLoteVentana AS CHAR.
DEFINE VAR vbultos AS INTEGER.
DEFINE VAR vArticulo AS INTEGER.
DEFINE VAR vTipoTambor AS INTEGER.

DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.


DEFINE BUFFER bb_tam_por_kilos FOR tt_tambores.
DEFINE BUFFER bbTamboresPorItem FOR tt_tambores.
DEFINE BUFFER bbPrimerItemsFactura FOR items_factura.
DEFINE BUFFER bbItemsFactura FOR items_factura.


RUN get-container (OUTPUT hcon).
/*RUN get-rowid-cabecera in hcon (output v_r_remito). */

RUN get-rowid-remito IN hcon (OUTPUT v_r_remito).

vCantidadTamboresVentana = INTEGER(items_factura.cantidad:SCREEN-VALUE IN FRAME F-Main).
vLoteVentana             = STRING(INTEGER(v_lote:SCREEN-VALUE IN FRAME F-Main),"9999") + "/" + 
                           STRING(SUBSTRING(v_anio:SCREEN-VALUE IN FRAME F-Main,3,2),"99").
vArticulo                = INTEGER(items_factura.id_articulo:SCREEN-VALUE IN FRAME F-Main).
vTipoTambor              = INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main).

IF INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main) = 11 THEN DO:
    /* PROCEDIMIENTO COMPLETO DE CASCARA MODIFICADO POR ADRIAN EL 01/07/04 */
    /* PARA PODER SEPARAR DE ACUERDO A LOS KILOS DE LOS TAMBORES EN DISTINTOS */
    /* items_factura */
    /* SIMPLEMENTE AGRUPE TODA LA GESTION DE REMITO DE CASCARA JUNTO. AUNQUE SE */
    /* REPITA CODIGO */

    v_hasta     = INTEGER(items_factura.cantidad:SCREEN-VALUE IN FRAME F-Main).  
    v_kilos_unitarios = IF DECIMAL(items_factura.kilos:SCREEN-VALUE IN FRAME F-Main) > 0 THEN
                           DECIMAL(items_factura.kilos:SCREEN-VALUE IN FRAME F-Main) ELSE
                          (DECIMAL(items_factura.cantidad:SCREEN-VALUE IN FRAME F-Main) * 50).
    v_desde     = 1.
    v_calidad   = 626.
    v_envase    = 14.
    
    FIND FIRST r_productos_calidad_envase WHERE r_productos_calidad_envase.id_articulo = INTEGER(items_factura.id_articulo:SCREEN-VALUE IN FRAME F-Main) 
                                            NO-LOCK NO-ERROR.
    IF AVAILABLE r_productos_calidad_envase THEN DO:
      v_calidad   = r_productos_calidad_envase.id_calidad.
      v_envase    = r_productos_calidad_envase.id_envase.
    END.
    ELSE DO:
        MESSAGE "No existe la combinacion de Articulo, Calidad y Envase para el codigo " 
                items_factura.id_articulo:SCREEN-VALUE IN FRAME F-Main ". Por favor avisar a Sistemas."
                VIEW-AS ALERT-BOX.
        RETURN "ADM-ERROR".
    END.
    
    FIND FIRST bb_remi WHERE ROWID(bb_remi) = v_r_remito NO-ERROR.
    IF AVAILABLE bb_remi THEN DO:
        
        FIND LAST bb_items OF bb_remi NO-LOCK NO-ERROR.
        IF AVAILABLE bb_items THEN v_item = bb_items.ITEM + 1 .
        ELSE v_item = 1.
        ASSIGN items_factura.id_sucursal    = bb_remi.id_sucursal
               items_factura.id_tipo_movsto = bb_remi.id_tipo_movsto
               items_factura.nro            = bb_remi.nro
               items_factura.ITEM           = v_item
               items_factura.fecha          = bb_remi.fecha
               items_factura.c_usuario      = USERID("userdb")
               items_factura.c_fecha        = TODAY
               items_factura.c_hora         = STRING(TIME,"HH:MM:SS")
               items_factura.desde_lote     = v_desde
               items_factura.hasta_lote     = v_hasta
               items_factura.bultos         = INTEGER(items_factura.cantidad:SCREEN-VALUE IN FRAME F-Main)
               items_factura.peso           = v_kilos_unitarios
               items_factura.id_calidad     = v_calidad
               items_factura.id_envase      = v_envase
               items_factura.nro_lote       = STRING(INTEGER(v_lote:SCREEN-VALUE IN FRAME F-Main),"9999") + "/" + 
                                              STRING(SUBSTRING(v_anio:SCREEN-VALUE IN FRAME F-Main,3,2),"99")
               items_factura.id_marca       = 1
               .
        /*CASCARA*/
        
        IF viNroMov = 0 THEN DO:
          FIND FIRST lotes_ubicacion WHERE lotes_ubicacion.id_lote                = INTEGER(v_lote:SCREEN-VALUE IN FRAME F-Main)
                                       AND lotes_ubicacion.id_sucursal_ubicacion  = bb_remi.id_sucursal
                                       AND lotes_ubicacion.id_tipotambor          = 11
                                     NO-ERROR.
        END.
        ELSE DO:
          FIND FIRST lotes_ubicacion WHERE lotes_ubicacion.id_empresa    = viEmpresa
                                       AND lotes_ubicacion.id_sucursal   = viSucursal
                                       AND lotes_ubicacion.id_tipotambor = viTipotambor
                                       AND lotes_ubicacion.nromov        = viNromov
                                     NO-ERROR.
        END.

        IF AVAILABLE lotes_ubicacion THEN DO:
            FIND FIRST r_lote_cascara_remito WHERE r_lote_cascara_remito.id_sucursal_remito = bb_remi.id_sucursal
                                               AND r_lote_cascara_remito.id_tipo_movsto     = bb_remi.id_tipo_movsto
                                               AND r_lote_cascara_remito.nro_remito         = bb_remi.nro
                                               AND r_lote_cascara_remito.id_sucursal        = lotes_ubicacion.id_sucursal
                                               AND r_lote_cascara_remito.id_empresa         = lotes_ubicacion.id_empresa
                                               AND r_lote_cascara_remito.id_tipotambor      = lotes_ubicacion.id_tipotambor
                                               AND r_lote_cascara_remito.nromov             = lotes_ubicacion.nromov
                                             NO-ERROR.
            IF NOT AVAILABLE r_lote_cascara_remito THEN 
              CREATE r_lote_cascara_remito.
            
            ASSIGN r_lote_cascara_remito.id_sucursal_remito    = bb_remi.id_sucursal
                   r_lote_cascara_remito.id_tipo_movsto        = bb_remi.id_tipo_movsto
                   r_lote_cascara_remito.nro_remito            = bb_remi.nro
                   r_lote_cascara_remito.id_empresa            = lotes_ubicacion.id_empresa
                   r_lote_cascara_remito.id_sucursal           = lotes_ubicacion.id_sucursal
                   r_lote_cascara_remito.id_tipotambor         = lotes_ubicacion.id_tipotambor
                   r_lote_cascara_remito.nromov                = lotes_ubicacion.nromov
                   r_lote_cascara_remito.ITEM_factura          = v_item.

            /*actualizo datos del peso en el remito*/
            ASSIGN bb_remi.peso_neto = DECIMAL(items_factura.cantidad:SCREEN-VALUE IN FRAME F-Main) * 50.
        END.
        ELSE DO:
          IF INTEGER(items_factura.id_articulo:SCREEN-VALUE IN FRAME F-Main) = 22 OR INTEGER(items_factura.id_articulo:SCREEN-VALUE IN FRAME F-Main) = 40  THEN DO:
            /*dejo pasar los remitos de polvillo*/
            
          END.
          ELSE DO:
            MESSAGE "Imposible de Encontrar lote_cascara." + v_lote:SCREEN-VALUE IN FRAME F-Main + "-" + v_anio:SCREEN-VALUE IN FRAME F-Main + "-" + STRING(bb_remi.id_sucursal) + "-" + items_factura.id_articulo:SCREEN-VALUE IN FRAME F-Main + "-" + string(viNroMov) VIEW-AS ALERT-BOX TITLE "v_items_factura.adm-post-create()".
            RUN logIndustria.p ("Imposible de Encontrar lote_cascara.",
                                "rto: " + STRING(bb_remi.nro) + " Lot: " + v_lote:SCREEN-VALUE IN FRAME F-Main + "/" + v_anio:SCREEN-VALUE IN FRAME F-Main + " Suc: " + STRING(bb_remi.id_sucursal) + " Art: " + items_factura.id_articulo:SCREEN-VALUE IN FRAME F-Main + " Mov: " + STRING(viNroMov), 
                                "v_items_factura.adm_post_create()",
                                USERID("userdb")).
            RETURN "ADM-ERROR".
          END.

        END.
    END.
    ELSE DO:
        MESSAGE "No se encontro el remito" VIEW-AS ALERT-BOX.
        RETURN "ADM-ERROR".
    END.
END.
ELSE DO:
    /* INDUSTRIA */
 IF INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main) = 51 OR
    INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main) = 52 OR
    INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main) = 53 OR
    INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main) = 58 OR
    INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main) = 71 THEN DO:
   MESSAGE "Utilizar Version Nueva para estos articulos" SKIP "OJO MUÑOZ!!"
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN "ADM-ERROR".
 END.

    /* NUEVO */
    FOR EACH bb_tam_por_kilos WHERE bb_tam_por_kilos.elegido
                                BREAK BY bb_tam_por_kilos.kilos_tambor.

        IF FIRST-OF(bb_tam_por_kilos.kilos_tambor) THEN DO:
            v_desde =   bb_tam_por_kilos.id_tambor.
            v_calidad = bb_tam_por_kilos.id_calidad.
            v_envase  = bb_tam_por_kilos.id_envase.
            i = 0.
            v_kgs_tambor = 0.
        END.
    
        i = i + 1.
        v_kgs_tambor = v_kgs_tambor + bb_tam_por_kilos.kilos_tambor.
        vPesoTambor  = bb_tam_por_kilos.kilos_tambor.


        IF LAST-OF(bb_tam_por_kilos.kilos_tambor) THEN DO:

          cLot = DYNAMIC-FUNCTION('getComposeNroLote' IN hLib, bb_tam_por_kilos.id_sucursal,
                                                               bb_tam_por_kilos.id_articulo,
                                                               bb_tam_por_kilos.id_lote,
                                                               bb_tam_por_kilos.anio).
    
            v_hasta = v_desde + i - 1.
        
            FIND FIRST bb_remi WHERE ROWID(bb_remi) = v_r_remito NO-ERROR.
            IF AVAILABLE bb_remi THEN DO:
                
                FIND LAST bb_items OF bb_remi NO-LOCK NO-ERROR.
                IF AVAILABLE bb_items THEN v_item = bb_items.ITEM + 1 .
                ELSE v_item = 1.
                
                IF vPrimerItem = 0 THEN DO:
                    vPrimerItem = v_item.


                    ASSIGN items_factura.id_sucursal    = bb_remi.id_sucursal
                           items_factura.id_tipo_movsto = bb_remi.id_tipo_movsto
                           items_factura.nro            = bb_remi.nro
                           items_factura.ITEM           = v_item
                           items_factura.fecha          = bb_remi.fecha
                           items_factura.c_usuario      = USERID("userdb")
                           items_factura.c_fecha        = TODAY
                           items_factura.c_hora         = STRING(TIME,"HH:MM:SS")
                           items_factura.desde_lote     = v_desde
                           items_factura.hasta_lote     = v_hasta
                           items_factura.bultos         = vCantidadTamboresVentana
                           items_factura.peso           = vPesoTambor
                           items_factura.id_calidad     = v_calidad
                           items_factura.id_envase      = v_envase
                           items_factura.nro_lote       = vLoteVentana
                           items_factura.id_marca       = 1
                           items_factura.cantidad       = i
                           items_factura.kilos          = v_kgs_tambor
                           items_factura.descripcion    = cLot.

                           items_factura.cantidad:SCREEN-VALUE IN FRAME F-Main = STRING(i).
                           items_factura.kilos:SCREEN-VALUE IN FRAME F-Main    = STRING(v_kgs_tambor).
                           
                    
                END.
                ELSE DO:
                    FIND LAST bbPrimerItemsFactura OF bb_remi
                                                WHERE bbPrimerItemsFactura.ITEM = vPrimerItem
                                                NO-LOCK NO-ERROR.
                    IF AVAILABLE bbPrimerItemsFactura THEN DO: 
                        CREATE bbItemsFactura.
                        BUFFER-COPY bbPrimerItemsFactura EXCEPT ITEM peso cantidad kilos TO bbItemsFactura.
                        ASSIGN bbItemsFactura.ITEM              = v_item
                               bbItemsFactura.peso              = vPesoTambor
                               bbItemsFactura.cantidad          = i
                               bbItemsFactura.kilos             = v_kgs_tambor
                               bbItemsFactura.bultos            = i
                               bbItemsFactura.id_articulo       = vArticulo
                               bbItemsFactura.id_tipotambor     = vTipoTambor
                               bbItemsFactura.desde_lote        = v_desde
                               bbItemsFactura.hasta_lote        = v_hasta.
                    END.
                END.

                FOR EACH bbTamboresPorItem WHERE elegido
                                             AND bbTamboresPorItem.kilos_tambor = bb_tam_por_kilos.kilos_tambor.
                    FIND FIRST tambores_industria WHERE ROWID(tambores_industria) = bbTamboresPorItem.id_reg NO-ERROR.
                    IF AVAILABLE tambores_industria THEN DO:
                        ASSIGN tambores_industria.id_sucursal_remito    = bb_remi.id_sucursal
                               tambores_industria.id_tipo_movsto        = bb_remi.id_tipo_movsto
                               tambores_industria.nro_remito            = bb_remi.nro
                               tambores_industria.ITEM_factura          = v_item.
                        /*by facundo 11/04/2005*/
                        /*grabo la relacion tambor_remito*/
                        CREATE r_tambor_remito.
                        ASSIGN r_tambor_remito.id_sucursal_remito = bb_remi.id_sucursal
                               r_tambor_remito.id_tipo_movsto     = bb_remi.id_tipo_movsto
                               r_tambor_remito.nro_remito         = bb_remi.nro
                               r_tambor_remito.ITEM_factura       = v_item
                               r_tambor_remito.id_empresa         = tambores_industria.id_empresa
                               r_tambor_remito.id_sucursal        = tambores_industria.id_sucursal
                               r_tambor_remito.id_tipotambor      = tambores_industria.id_tipotambor
                               r_tambor_remito.nromov             = tambores_industria.nromov
                               r_tambor_remito.id_tambor          = tambores_industria.id_tambor
                               r_tambor_remito.fecha              = TODAY
                               r_tambor_remito.c_usuario          = USERID("userdb")
                               r_tambor_remito.c_fecha            = TODAY
                               r_tambor_remito.c_hora             = STRING(TIME, "HH:MM:SS").


                    END.
                END.
            END.
            ELSE DO:
                MESSAGE "No se encontro el remito" VIEW-AS ALERT-BOX.
                RETURN "ADM-ERROR".
            END.

    /* NUEVO */
        END.
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
FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_remito = v_del_suc
                              AND tambores_industria.id_tipo_movsto     = v_del_tip
                              AND tambores_industria.nro_remito         = v_del_nro
                              AND tambores_industria.ITEM_factura       = v_del_ite.
    ASSIGN tambores_industria.id_sucursal_remito = 0
           tambores_industria.id_tipo_movsto     = 0
           tambores_industria.nro_remito         = 0
           tambores_industria.ITEM_factura       = 0.
    /*eliminar ralacion - by facundo 11/04/2005*/
    FIND FIRST r_tambor_remito WHERE r_tambor_remito.id_empresa         = tambores_industria.id_empresa
                                 AND r_tambor_remito.id_sucursal        = tambores_industria.id_sucursal
                                 AND r_tambor_remito.id_tipotambor      = tambores_industria.id_tipotambor 
                                 AND r_tambor_remito.nromov             = tambores_industria.nromov
                                 AND r_tambor_remito.id_tambor          = tambores_industria.id_tambor
                                 AND r_tambor_remito.id_sucursal_remito = v_del_suc
                                 AND r_tambor_remito.id_tipo_movsto     = v_del_tip
                                 AND r_tambor_remito.nro_remito         = v_del_nro
                                 AND r_tambor_remito.ITEM_factura       = v_del_ite
                               NO-ERROR.
    IF AVAILABLE r_tambor_remito THEN
      DELETE r_tambor_remito.
                            
END.

DEFINE VAR hcon AS HANDLE.

RUN get-container (OUTPUT hcon).
RUN actualizo-kilos IN hcon.



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
DEFINE VAR hcon AS HANDLE.

RUN get-container (OUTPUT hcon).
RUN actualizo-kilos IN hcon.
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
DEFINE VAR vCorrelativos AS LOGICAL NO-UNDO.
DEFINE VAR i AS INTEGER NO-UNDO.

IF INTEGER(items_factura.id_tipotambor:SCREEN-VALUE IN FRAME F-Main) <> 11 THEN DO:
    FIND FIRST tt_tambores WHERE elegido NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt_tambores THEN DO:
        MESSAGE "No ha elegido ningun tambor para despachar." VIEW-AS ALERT-BOX.
        RETURN "ADM-ERROR".
    END.
    ELSE DO:
        FOR EACH tt_tambores WHERE elegido.
            IF i = 0 THEN i = tt_tambores.id_tambor.
            ELSE DO:
                i = i + 1.
                /*MESSAGE "i " i " tambor " VIEW-AS ALERT-BOX.*/
                IF i <> tt_tambores.id_tambor THEN DO:
                    MESSAGE "Ha elegido tambores que no respetan correlatividad." VIEW-AS ALERT-BOX.
                    RETURN "ADM-ERROR".
                    LEAVE.
                END.
            END.
        END.
    END.
END.
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
v_del_suc = items_factura.id_sucursal.
v_del_tip = items_factura.id_tipo_movsto.
v_del_nro = items_factura.nro.
v_del_ite = items_factura.ITEM.
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
  {src/adm/template/row-list.i "items_factura"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "items_factura"}

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
find first tipostambor where tipostambor.id_tipotambor = integer(items_factura.id_tipotambor:screen-value in frame F-Main)  no-lock no-error .
if available tipostambor then 
fi-tipostambor-abreviatura:screen-value in frame F-Main = string(tipostambor.abreviatura).
else
fi-tipostambor-abreviatura:screen-value in frame F-Main = ''.

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
define var lista_relacion as character no-undo initial "id_tipotambor".
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
  IF AVAILABLE items_factura THEN DO:
      v_lote:SCREEN-VALUE IN FRAME F-Main = SUBSTRING(STRING(items_factura.nro_lote),1,4).
      v_anio:SCREEN-VALUE IN FRAME F-Main = STRING( 2000 + INTEGER(SUBSTRING(STRING(items_factura.nro_lote),6,2))).
  END.
  
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
v_lote:READ-ONLY IN FRAME F-Main = FALSE.
v_anio:READ-ONLY IN FRAME F-Main = FALSE.
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
  {src/adm/template/snd-list.i "items_factura"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-anio V-table-Win 
PROCEDURE valida-anio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
    when "v_anio" then
        if integer(valor) < YEAR(TODAY) - 3 then 
        do:
            mensaje = "cargo mal el año".
            /* return false. */
         end.
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

