&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

DEFINE TEMP-TABLE ttBolsasLoteDispo
    RCODE-INFORMATION
    FIELD desde                      AS INTEGER  COLUMN-LABEL "Nro Bolsa Desde"
    FIELD hasta                      AS INTEGER  COLUMN-LABEL "Nro Bolsa Hasta"
    FIELD cantidad                   AS INTEGER  COLUMN-LABEL "Cantidad"
    FIELD id_empresa_destino         AS INTEGER  COLUMN-LABEL "Cod. Empresa"
    FIELD id_sucursal_destino        AS INTEGER  COLUMN-LABEL "Cod. Sucursal"
    FIELD id_tipotambor_destino      AS INTEGER  COLUMN-LABEL "TipoTambor"
    FIELD id_lote                    AS INTEGER  COLUMN-LABEL "Cod. Lote"
    FIELD nromov_destino             AS INTEGER  COLUMN-LABEL "NroMov"
    FIELD id_orden_entrega           AS INTEGER  COLUMN-LABEL "Cod. Orden Entrega"
    FIELD item_oe                    AS INTEGER  COLUMN-LABEL "ItemOE"
    FIELD applyChanges               AS INTEGER  COLUMN-LABEL "Aplicar Cambios"
    FIELD eliminado                  AS INTEGER  COLUMN-LABEL "Eliminado".

DEFINE TEMP-TABLE ttBolsasOE
    RCODE-INFORMATION
    FIELD desde                      AS INTEGER  COLUMN-LABEL "Nro Bolsa Desde"
    FIELD hasta                      AS INTEGER  COLUMN-LABEL "Nro Bolsa Hasta"
    FIELD cantidad                   AS INTEGER  COLUMN-LABEL "Cantidad"
    FIELD id_empresa_destino         AS INTEGER  COLUMN-LABEL "Cod. Empresa"
    FIELD id_sucursal_destino        AS INTEGER  COLUMN-LABEL "Cod. Sucursal"
    FIELD id_tipotambor_destino      AS INTEGER  COLUMN-LABEL "TipoTambor"
    FIELD id_lote                    AS INTEGER  COLUMN-LABEL "Cod. Lote"
    FIELD nromov_destino             AS INTEGER  COLUMN-LABEL "NroMov"
    FIELD id_orden_entrega           AS INTEGER  COLUMN-LABEL "Cod. Orden Entrega"
    FIELD item_oe                    AS INTEGER  COLUMN-LABEL "ItemOE"
    FIELD applyChanges               AS INTEGER  COLUMN-LABEL "Aplicar Cambios"
    FIELD eliminado                  AS INTEGER  COLUMN-LABEL "Eliminado".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME brwBolsasOE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBolsasOE ttBolsasLoteDispo

/* Definitions for BROWSE brwBolsasOE                                   */
&Scoped-define FIELDS-IN-QUERY-brwBolsasOE ttBolsasOE.id_lote ttBolsasOE.desde ttBolsasOE.hasta ttBolsasOE.cantidad   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwBolsasOE   
&Scoped-define SELF-NAME brwBolsasOE
&Scoped-define QUERY-STRING-brwBolsasOE FOR EACH ttBolsasOE WHERE ttBolsasOE.eliminado <> 1
&Scoped-define OPEN-QUERY-brwBolsasOE OPEN QUERY {&SELF-NAME} FOR EACH ttBolsasOE WHERE ttBolsasOE.eliminado <> 1.
&Scoped-define TABLES-IN-QUERY-brwBolsasOE ttBolsasOE
&Scoped-define FIRST-TABLE-IN-QUERY-brwBolsasOE ttBolsasOE


/* Definitions for BROWSE brwLotesDispo                                 */
&Scoped-define FIELDS-IN-QUERY-brwLotesDispo ttBolsasLoteDispo.id_lote ttBolsasLoteDispo.desde ttBolsasLoteDispo.hasta ttBolsasLoteDispo.cantidad   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwLotesDispo   
&Scoped-define SELF-NAME brwLotesDispo
&Scoped-define QUERY-STRING-brwLotesDispo FOR EACH ttBolsasLoteDispo WHERE ttBolsasLoteDispo.eliminado <> 1
&Scoped-define OPEN-QUERY-brwLotesDispo OPEN QUERY {&SELF-NAME} FOR EACH ttBolsasLoteDispo WHERE ttBolsasLoteDispo.eliminado <> 1.
&Scoped-define TABLES-IN-QUERY-brwLotesDispo ttBolsasLoteDispo
&Scoped-define FIRST-TABLE-IN-QUERY-brwLotesDispo ttBolsasLoteDispo


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-brwBolsasOE}~
    ~{&OPEN-QUERY-brwLotesDispo}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnVer cmbSucursal FILL-IN-2 brwLotesDispo ~
brwBolsasOE fillCantBolsas btnAsociar btnDesvincular fillTotDispo ~
fillTotAsociado btnApplyChantes btnExit lblMsg RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS cmbSucursal FILL-IN-2 fillCantBolsas ~
fillTotDispo fillTotAsociado lblMsg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_brwcascordenentregaitems AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_sdocascordenentregaitems AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApplyChantes 
     LABEL "Aplicar Cambios" 
     SIZE 17 BY 1.14.

DEFINE BUTTON btnAsociar 
     LABEL "Vincular >>" 
     SIZE 17 BY 1.14.

DEFINE BUTTON btnDesvincular 
     LABEL "<< Desvincular" 
     SIZE 17 BY 1.14.

DEFINE BUTTON btnExit 
     LABEL "Cancelar" 
     SIZE 16.2 BY 1.14.

DEFINE BUTTON btnVer 
     LABEL "Ver Asignacion" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbSucursal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sucursal" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todas","0",
                     "Lavalle","96",
                     "Famailla","95",
                     "Mercotuc","92"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Asignacion de Bolsas de Lote a Orden de Entrega" 
     VIEW-AS FILL-IN 
     SIZE 145 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fillCantBolsas AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fillTotAsociado AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Total Asociado" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fillTotDispo AS INTEGER FORMAT ">>,>>,>>9":U INITIAL 0 
     LABEL "Total Disponible" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lblMsg AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 93 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144.6 BY 9.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 5.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwBolsasOE FOR 
      ttBolsasOE SCROLLING.

DEFINE QUERY brwLotesDispo FOR 
      ttBolsasLoteDispo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwBolsasOE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwBolsasOE wWin _FREEFORM
  QUERY brwBolsasOE DISPLAY
      ttBolsasOE.id_lote   FORMAT ">>>>>>>9":U   WIDTH 10.0
  ttBolsasOE.desde     FORMAT ">>>>>>>9":U   WIDTH 10.6   
  ttBolsasOE.hasta     FORMAT ">>>>>>>9":U   WIDTH 10.6
  ttBolsasOE.cantidad  FORMAT ">>,>>>,>>9":U WIDTH 15.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 6.91
         TITLE "Bolsas Asociadas a OE" EXPANDABLE.

DEFINE BROWSE brwLotesDispo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwLotesDispo wWin _FREEFORM
  QUERY brwLotesDispo DISPLAY
      ttBolsasLoteDispo.id_lote   FORMAT ">>>>>>>9":U   WIDTH 10.0
  ttBolsasLoteDispo.desde     FORMAT ">>>>>>>9":U   WIDTH 10.6   
  ttBolsasLoteDispo.hasta     FORMAT ">>>>>>>9":U   WIDTH 10.6
  ttBolsasLoteDispo.cantidad  FORMAT ">>,>>>,>>9":U WIDTH 15.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 6.91
         TITLE "Bolsas en Lotes Disponibles" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnVer AT ROW 1.14 COL 131
     cmbSucursal AT ROW 1.24 COL 112.8 COLON-ALIGNED
     FILL-IN-2 AT ROW 7.95 COL 1.6 NO-LABEL
     brwLotesDispo AT ROW 9.57 COL 3
     brwBolsasOE AT ROW 9.62 COL 93.8
     fillCantBolsas AT ROW 11.24 COL 57 COLON-ALIGNED NO-LABEL
     btnAsociar AT ROW 11.24 COL 71
     btnDesvincular AT ROW 13.62 COL 64
     fillTotDispo AT ROW 16.71 COL 38 COLON-ALIGNED
     fillTotAsociado AT ROW 16.71 COL 129 COLON-ALIGNED
     btnApplyChantes AT ROW 18.62 COL 112
     btnExit AT ROW 18.62 COL 130
     lblMsg AT ROW 18.62 COL 1 COLON-ALIGNED NO-LABEL
     RECT-4 AT ROW 9.19 COL 1.8
     RECT-5 AT ROW 2.38 COL 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 146.2 BY 19.1
         CANCEL-BUTTON btnExit.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Asociar Bolsas Lote a OEs"
         HEIGHT             = 18.91
         WIDTH              = 146.2
         MAX-HEIGHT         = 34.33
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.33
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
                                                                        */
/* BROWSE-TAB brwLotesDispo FILL-IN-2 fMain */
/* BROWSE-TAB brwBolsasOE brwLotesDispo fMain */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME fMain
   ALIGN-L                                                              */
ASSIGN 
       fillTotAsociado:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fillTotDispo:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       lblMsg:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwBolsasOE
/* Query rebuild information for BROWSE brwBolsasOE
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBolsasOE WHERE ttBolsasOE.eliminado <> 1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwBolsasOE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwLotesDispo
/* Query rebuild information for BROWSE brwLotesDispo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBolsasLoteDispo WHERE ttBolsasLoteDispo.eliminado <> 1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwLotesDispo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Asociar Bolsas Lote a OEs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Asociar Bolsas Lote a OEs */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwLotesDispo
&Scoped-define SELF-NAME brwLotesDispo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwLotesDispo wWin
ON VALUE-CHANGED OF brwLotesDispo IN FRAME fMain /* Bolsas en Lotes Disponibles */
DO:
  BROWSE brwLotesDispo:FETCH-SELECTED-ROW(1).
  fillCantBolsas:SCREEN-VALUE IN FRAME fMain = STRING(ttBolsasLoteDispo.cantidad).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApplyChantes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApplyChantes wWin
ON CHOOSE OF btnApplyChantes IN FRAME fMain /* Aplicar Cambios */
DO:
  DEFINE VAR vAntes   AS INTEGER   NO-UNDO.
  DEFINE VAR vDespues AS INTEGER   NO-UNDO.
  DEFINE VAR vTiempo  AS CHARACTER NO-UNDO.

  
  vAntes = TIME.
  lblMsg:SCREEN-VALUE IN FRAME fMain = "Espere...".
  

   /*vincular las bolsas a la oe*/
  FOR EACH ttBolsasOE WHERE ttBolsasOE.eliminado <> 1 
                      NO-LOCK.
    FOR EACH tambores_industria WHERE tambores_industria.id_tipotambor  = 11
                                  AND tambores_industria.nromov_destino = ttBolsasOE.nromov_destino
                                  AND tambores_industria.id_tambor     >= ttBolsasOE.desde
                                  AND tambores_industria.id_tambor     <= ttBolsasOE.hasta.
      ASSIGN tambores_industria.id_orden_entrega = ttBolsasOE.id_orden_entrega
             tambores_industria.ITEM_oe          = ttBolsasOE.ITEM_oe
             tambores_industria.id_lote          = ttBolsasOE.id_lote.
    END.
    FIND FIRST lotes_jugo WHERE lotes_jugo.nromov = ttBolsasOE.nromov_destino 
                          NO-ERROR.
    IF AVAILABLE lotes_jugo THEN DO:
      ASSIGN lotes_jugo.id_orden_entrega = ttBolsasOE.id_orden_entrega
             lotes_jugo.ITEM_oe          = ttBolsasOE.ITEM_oe.
    END.
  END.

  /*desvincular las bolsas de las oes*/
  FOR EACH ttBolsasLoteDispo WHERE ttBolsasLoteDispo.applyChanges = 1
                             NO-LOCK.
    FOR EACH tambores_industria WHERE tambores_industria.id_tipotambor = 11
                                  AND tambores_industria.id_tambor    >= ttBolsasLoteDispo.desde
                                  AND tambores_industria.id_tambor    <= ttBolsasLoteDispo.hasta.
      ASSIGN tambores_industria.id_orden_entrega = 0
             tambores_industria.ITEM_oe          = 0.
            
    END.
    FIND FIRST lotes_jugo WHERE lotes_jugo.nromov = ttBolsasLoteDispo.nromov_destino 
                          NO-ERROR.
    IF AVAILABLE lotes_jugo THEN DO:
      ASSIGN lotes_jugo.id_orden_entrega = 0
             lotes_jugo.ITEM_oe          = 0.
    END.
  END.
  
  
  /*
  /*appsrv*/
   DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
   DEFINE VAR ret      AS LOGICAL NO-UNDO.
   CREATE SERVER hAppSrv.
   ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").
   RUN appCascApplyChangesAsocLotes.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT-OUTPUT TABLE ttBolsasLoteDispo, 
                                                                              INPUT-OUTPUT TABLE ttBolsasOE).
   IF ERROR-STATUS:ERROR THEN DO:
     ret = hAppSrv:DISCONNECT().
     RETURN NO-APPLY RETURN-VALUE.
   END.
   ret = hAppSrv:DISCONNECT().
   DELETE OBJECT hAppSrv.
   /*/appsrv*/
   */

  vDespues = TIME.
  vTiempo = STRING(vDespues - vAntes, "HH:MM:SS").
  lblMsg:SCREEN-VALUE IN FRAME fMain = "Listo (" + vTiempo + ")".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAsociar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAsociar wWin
ON CHOOSE OF btnAsociar IN FRAME fMain /* Vincular >> */
DO:
  DEFINE VAR vCantidad AS INTEGER NO-UNDO.
  DEFINE VAR vDesde    AS INTEGER NO-UNDO.
  DEFINE VAR vHasta    AS INTEGER NO-UNDO.
  DEFINE VAR vLote     AS INTEGER NO-UNDO.
  DEFINE VAR vNroMov   AS INTEGER NO-UNDO.
  DEFINE VAR vIdOE     AS INTEGER NO-UNDO.
  DEFINE VAR vItemOE   AS INTEGER NO-UNDO.
  DEFINE VAR xRow      AS HANDLE  NO-UNDO.

  vCantidad = INTEGER(fillCantBolsas:SCREEN-VALUE IN FRAME fMain).
  vDesde    = ttBolsasLoteDispo.desde.
  vHasta    = ttBolsasLoteDispo.hasta.
  vLote     = ttBolsasLoteDispo.id_lote.
  vNroMov   = ttBolsasLoteDispo.nromov.
      
  xRow      = DYNAMIC-FUNCTION('getRowObject' IN h_sdocascordenentregaitems ).    
  vIdOE     = xRow:BUFFER-FIELD('id_orden_entrega'):BUFFER-VALUE.  
  vItemOE   = xRow:BUFFER-FIELD('item_oe'):BUFFER-VALUE.

  /*controles de tipo*/
  IF vCantidad <= 0 THEN DO:
    MESSAGE "La cantidad debe ser mayor que 0" VIEW-AS ALERT-BOX.
    RETURN.
  END.
  IF vCantidad > ttBolsasLoteDispo.cantidad THEN DO:
    MESSAGE "La cantidad no pude ser mayor que la cantidad del rango seleccionado." VIEW-AS ALERT-BOX.
    RETURN.
  END.
  /*fin controles de tipo*/
  
  
  IF vCantidad <> ttBolsasLoteDispo.cantidad THEN DO: /*rango incompleto*/
    vCantidad = INTEGER(fillCantBolsas:SCREEN-VALUE IN FRAME fMain).
    vHasta    = vDesde + vCantidad.
    ASSIGN ttBolsasLoteDispo.cantidad = ttBolsasLoteDispo.cantidad - vCantidad.
    ASSIGN ttBolsasLoteDispo.desde    = vHasta.
  END.
  ELSE DO: /*rango completo - eliminar de bolsas disponibles*/
    /*DELETE tt-bolsas_disponibles.*/
    ASSIGN ttBolsasLoteDispo.eliminado = 1.
    brwLotesDispo:DELETE-CURRENT-ROW().
  END.

  CREATE ttBolsasOE.
    ASSIGN
    ttBolsasOE.desde            = vDesde
    ttBolsasOE.hasta            = vDesde + vCantidad - 1
    ttBolsasOE.cantidad         = vCantidad
    ttBolsasOE.id_lote          = vLote
    ttBolsasOE.id_orden_entrega = vIdOE
    ttBolsasOE.ITEM_oe          = vItemOE
    ttBolsasOE.nromov           = vNroMov.
    
  {&OPEN-QUERY-brwBolsasOE}
  {&OPEN-QUERY-brwLotesDispo}
  RUN getTotals.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDesvincular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDesvincular wWin
ON CHOOSE OF btnDesvincular IN FRAME fMain /* << Desvincular */
DO:
  DEFINE VAR vCantidad AS INTEGER NO-UNDO.
  DEFINE VAR vDesde    AS INTEGER NO-UNDO.
  DEFINE VAR vHasta    AS INTEGER NO-UNDO.
  DEFINE VAR vLote     AS INTEGER NO-UNDO.
  DEFINE VAR vNroMov   AS INTEGER NO-UNDO.

  vCantidad = ttBolsasOE.cantidad.
  vDesde    = ttBolsasOE.desde.
  vHasta    = ttBolsasOE.hasta.
  vLote     = ttBolsasOE.id_lote.
  vNroMov   = ttBolsasOE.nromov.

  /*rango completo - eliminar de bolsas lote*/
  /*DELETE tt-bolsas_lote NO-ERROR.*/
  ASSIGN ttBolsasOE.eliminado = 1.
  brwBolsasOE:DELETE-CURRENT-ROW().

  CREATE ttBolsasLoteDispo.
  ASSIGN
    ttBolsasLoteDispo.id_lote      = vLote
    ttBolsasLoteDispo.desde        = vDesde 
    ttBolsasLoteDispo.hasta        = vDesde + vCantidad - 1
    ttBolsasLoteDispo.cantidad     = vCantidad
    ttBolsasLoteDispo.applyChanges = 1.
  {&OPEN-QUERY-brwLotesDispo}
  RUN getTotals.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit wWin
ON CHOOSE OF btnExit IN FRAME fMain /* Cancelar */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVer wWin
ON CHOOSE OF btnVer IN FRAME fMain /* Ver Asignacion */
DO:

   DEFINE VAR xRow      AS HANDLE  NO-UNDO.
   DEFINE VAR vBreak    AS CHAR    NO-UNDO.
   DEFINE VAR vCount    AS INTEGER NO-UNDO.
   DEFINE VAR vIdDesde  AS INTEGER NO-UNDO.
   DEFINE VAR vIdHasta  AS INTEGER NO-UNDO.
   DEFINE VAR vIdOE     AS INTEGER NO-UNDO.
   DEFINE VAR vIdItemOE AS INTEGER NO-UNDO.
   DEFINE VAR vIdSuc    AS INTEGER NO-UNDO.
   
   DEFINE VAR vAntes   AS INTEGER   NO-UNDO.
   DEFINE VAR vDespues AS INTEGER   NO-UNDO.
   DEFINE VAR vTiempo  AS CHARACTER NO-UNDO.
   DEFINE VAR iTime    AS INTEGER   NO-UNDO.

   vIdSuc    = INTEGER(cmbSucursal:SCREEN-VALUE IN FRAME fMain).
   xRow      = DYNAMIC-FUNCTION('getRowObject' IN h_sdocascordenentregaitems ).    
   vIdOE     = xRow:BUFFER-FIELD('id_orden_entrega'):BUFFER-VALUE.  
   vIdItemOE = xRow:BUFFER-FIELD('item_oe'):BUFFER-VALUE.

   vAntes = TIME.
   ETIME(TRUE).
   lblMsg:SCREEN-VALUE IN FRAME fMain = "Espere...".
   
   /*appsrv*/
   DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
   DEFINE VAR ret      AS LOGICAL NO-UNDO.
   CREATE SERVER hAppSrv.
   ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").
   RUN appCascFillTempTablesLotesDispoAsoc.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT-OUTPUT TABLE ttBolsasLoteDispo, 
                                                                                     INPUT-OUTPUT TABLE ttBolsasOE, 
                                                                                     INPUT vIdSuc, 
                                                                                     INPUT vIdOE, 
                                                                                     INPUT vIdItemOE).
   IF ERROR-STATUS:ERROR THEN DO:
     ret = hAppSrv:DISCONNECT().
     RETURN NO-APPLY RETURN-VALUE.
   END.
   ret = hAppSrv:DISCONNECT().
   DELETE OBJECT hAppSrv.
   /*/appsrv*/

   {&OPEN-QUERY-brwLotesDispo}
   {&OPEN-QUERY-brwBolsasOE}
   RUN getTotals.
   iTime = ETIME.
   vDespues = TIME.
   vTiempo = STRING(vDespues - vAntes, "HH:MM:SS").
   lblMsg:SCREEN-VALUE IN FRAME fMain = "Listo (" + vTiempo + ") " + STRING(iTime / 1000) + " milisegundos".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwBolsasOE
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdocascordenentregaitems.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamesdocascordenentregaitemsUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_sdocascordenentregaitems ).
       RUN repositionObject IN h_sdocascordenentregaitems ( 1.95 , 76.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'brwcascordenentregaitems.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_brwcascordenentregaitems ).
       RUN repositionObject IN h_brwcascordenentregaitems ( 1.00 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_brwcascordenentregaitems ( 6.67 , 69.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsNavigationSubModulesTableIOTypeSupportedLinksNavigation-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.05 , 72.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 33.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsid_orden_entrega,item_oe,fecha,orden_fabricacionOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesfechaRANGEFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsfecha0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 2.57 , 73.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.86 , 72.00 ) NO-ERROR.

       /* Links to SmartDataObject h_sdocascordenentregaitems. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_sdocascordenentregaitems ).
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_sdocascordenentregaitems ).

       /* Links to SmartDataBrowser h_brwcascordenentregaitems. */
       RUN addLink ( h_sdocascordenentregaitems , 'Data':U , h_brwcascordenentregaitems ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_brwcascordenentregaitems ,
             btnVer:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_dyntoolbar ,
             h_brwcascordenentregaitems , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynfilter ,
             cmbSucursal:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cmbSucursal FILL-IN-2 fillCantBolsas fillTotDispo fillTotAsociado 
          lblMsg 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE btnVer cmbSucursal FILL-IN-2 brwLotesDispo brwBolsasOE fillCantBolsas 
         btnAsociar btnDesvincular fillTotDispo fillTotAsociado btnApplyChantes 
         btnExit lblMsg RECT-4 RECT-5 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTotals wWin 
PROCEDURE getTotals :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR vTotDispo AS INTEGER NO-UNDO.
DEFINE VAR vTotLote  AS INTEGER NO-UNDO.

FOR EACH ttBolsasLoteDispo NO-LOCK.
  vTotDispo = vTotDispo + ttBolsasLoteDispo.cantidad.
END.
FOR EACH ttBolsasOE NO-LOCK.
  vTotLote = vTotLote + ttBolsasOE.cantidad.
END.

fillTotDispo:SCREEN-VALUE IN FRAME fMain = STRING(vTotDispo).
fillTotAsociado:SCREEN-VALUE IN FRAME fMain  = STRING(vTotLote).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

