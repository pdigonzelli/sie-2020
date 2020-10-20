&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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
define var alta as logical no-undo initial false.

DEFINE TEMP-TABLE tt-bolsas_disponibles
    RCODE-INFORMATION
    FIELD desde              AS INTEGER  COLUMN-LABEL "Nro Bolsa Desde"
    FIELD hasta              AS INTEGER  COLUMN-LABEL "Nro Bolsa Hasta"
    FIELD cantidad           AS INTEGER  COLUMN-LABEL "Cantidad"
    FIELD id_tambor_desde    AS INTEGER  COLUMN-LABEL "id_bolsa_desde"
    FIELD id_tambor_hasta    AS INTEGER  COLUMN-LABEL "id_bolsa_hasta"
    FIELD eliminado          AS INTEGER  COLUMN-LABEL "Registro Borrado"
    FIELD applyChanges       AS INTEGER  COLUMN-LABEL "Aplicar Cambios".

DEFINE TEMP-TABLE tt-bolsas_lote
    RCODE-INFORMATION
    FIELD desde              AS INTEGER  COLUMN-LABEL "Nro Bolsa Desde"
    FIELD hasta              AS INTEGER  COLUMN-LABEL "Nro Bolsa Hasta"
    FIELD cantidad           AS INTEGER  COLUMN-LABEL "Cantidad"
    FIELD id_tambor_desde    AS INTEGER  COLUMN-LABEL "id_bolsa_desde"
    FIELD id_tambor_hasta    AS INTEGER  COLUMN-LABEL "id_bolsa_hasta"
    FIELD eliminado          AS INTEGER  COLUMN-LABEL "Registro Borrado".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-bolsas_disponibles tt-bolsas_lote

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-bolsas_disponibles.desde tt-bolsas_disponibles.hasta tt-bolsas_disponibles.cantidad   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-bolsas_disponibles WHERE tt-bolsas_disponibles.eliminado <> 1                                                        NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-bolsas_disponibles WHERE tt-bolsas_disponibles.eliminado <> 1                                                        NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-bolsas_disponibles
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-bolsas_disponibles


/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-bolsas_lote.desde tt-bolsas_lote.hasta tt-bolsas_lote.cantidad   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-bolsas_lote WHERE tt-bolsas_lote.eliminado <> 1                                                 NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-bolsas_lote WHERE tt-bolsas_lote.eliminado <> 1                                                 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-bolsas_lote
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-bolsas_lote


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-35 BROWSE-1 BROWSE-2 BUTTON-2 ~
edtBolsasAgregar BUTTON-3 edtBolsasEliminar edtDispoTotal edtLoteTotal ~
BUTTON-34 lblMsg d RECT-2 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 edtBolsasAgregar ~
edtBolsasEliminar edtDispoTotal edtLoteTotal lblMsg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_lotes_cascara AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_lotes_cascara AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     LABEL ">>" 
     SIZE 7 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "<<" 
     SIZE 7 BY 1.14.

DEFINE BUTTON BUTTON-34 
     LABEL "Aplicar Cambios" 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-35  NO-CONVERT-3D-COLORS
     LABEL "Ver Asignacion de Bolsas" 
     SIZE 27 BY 1.14
     BGCOLOR 3 FGCOLOR 15 .

DEFINE VARIABLE edtBolsasAgregar AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE edtBolsasEliminar AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE edtDispoTotal AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Total Bolsas Disponibles" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE edtLoteTotal AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Total Bolsas Lote" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Asignacion de Bolsas" 
     VIEW-AS FILL-IN 
     SIZE 151 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lblMsg AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 77 BY .95 NO-UNDO.

DEFINE RECTANGLE d
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 151 BY 9.05.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 399 BY 50
     BGCOLOR 7 FGCOLOR 7 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 7.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-bolsas_disponibles SCROLLING.

DEFINE QUERY BROWSE-2 FOR 
      tt-bolsas_lote SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-bolsas_disponibles.desde     FORMAT ">>>>>9":U WIDTH 15.6
  tt-bolsas_disponibles.hasta     FORMAT ">>>>>9":U WIDTH 15.6
  tt-bolsas_disponibles.cantidad  FORMAT ">>>>>9":U WIDTH 15.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 6.91
         TITLE "Bolsas Disponibles" EXPANDABLE.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      tt-bolsas_lote.desde     FORMAT ">>>>>9":U WIDTH 15.6
  tt-bolsas_lote.hasta     FORMAT ">>>>>9":U WIDTH 15.6
  tt-bolsas_lote.cantidad  FORMAT ">>>>>9":U WIDTH 15.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 6.91
         TITLE "Bolsas Lote" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-35 AT ROW 1.71 COL 84
     FILL-IN-1 AT ROW 11.24 COL 2 NO-LABEL
     BROWSE-1 AT ROW 12.91 COL 4
     BROWSE-2 AT ROW 12.91 COL 100
     BUTTON-2 AT ROW 14.57 COL 66.4
     edtBolsasAgregar AT ROW 14.57 COL 73 COLON-ALIGNED NO-LABEL
     BUTTON-3 AT ROW 17.67 COL 66.4
     edtBolsasEliminar AT ROW 17.67 COL 73 COLON-ALIGNED NO-LABEL
     edtDispoTotal AT ROW 20.05 COL 54 RIGHT-ALIGNED
     edtLoteTotal AT ROW 20.05 COL 150 RIGHT-ALIGNED
     BUTTON-34 AT ROW 21.71 COL 134
     lblMsg AT ROW 21.71 COL 2 NO-LABEL
     d AT ROW 12.43 COL 2
     RECT-2 AT Y 0 X 5
     RECT-3 AT ROW 3.62 COL 84
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152.8 BY 22.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Lotes de Cascara"
         HEIGHT             = 22.05
         WIDTH              = 152.8
         MAX-HEIGHT         = 29.43
         MAX-WIDTH          = 203.4
         VIRTUAL-HEIGHT     = 29.43
         VIRTUAL-WIDTH      = 203.4
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "visibles" W-Win _INLINE
/* Actions: ? custom/support/cusvis.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" W-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* BROWSE-TAB BROWSE-1 FILL-IN-1 F-Main */
/* BROWSE-TAB BROWSE-2 BROWSE-1 F-Main */
/* SETTINGS FOR FILL-IN edtDispoTotal IN FRAME F-Main
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN edtLoteTotal IN FRAME F-Main
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblMsg IN FRAME F-Main
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-bolsas_disponibles WHERE tt-bolsas_disponibles.eliminado <> 1
                                                       NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-bolsas_lote WHERE tt-bolsas_lote.eliminado <> 1
                                                NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Lotes de Cascara */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Lotes de Cascara */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON VALUE-CHANGED OF BROWSE-1 IN FRAME F-Main /* Bolsas Disponibles */
DO:
  BROWSE BROWSE-1:FETCH-SELECTED-ROW(1).
  edtBolsasAgregar:SCREEN-VALUE IN FRAME F-Main = string(tt-bolsas_disponibles.cantidad).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 W-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME F-Main /* Bolsas Lote */
DO:
  BROWSE BROWSE-2:FETCH-SELECTED-ROW(1).
  edtBolsasEliminar:SCREEN-VALUE IN FRAME F-Main = string(tt-bolsas_lote.cantidad).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* >> */
DO:
  DEFINE VAR vCantidad AS INTEGER NO-UNDO.
  DEFINE VAR vDesde    AS INTEGER NO-UNDO.
  DEFINE VAR vHasta    AS INTEGER NO-UNDO.

  vCantidad = INTEGER(edtBolsasAgregar:SCREEN-VALUE IN FRAME F-Main).
  vDesde    = tt-bolsas_disponibles.desde.
  vHasta    = tt-bolsas_disponibles.hasta.
  
  /*controles de tipo*/
  IF vCantidad <= 0 THEN DO:
    MESSAGE "La cantidad debe ser mayor que 0" VIEW-AS ALERT-BOX.
    RETURN.
  END.
  IF vCantidad > tt-bolsas_disponibles.cantidad THEN DO:
    MESSAGE "La cantidad no pude ser mayor que la cantidad del rango seleccionado." VIEW-AS ALERT-BOX.
    RETURN.
  END.
  /*fin controles de tipo*/
  
  
  IF vCantidad <> tt-bolsas_disponibles.cantidad THEN DO: /*rango incompleto*/
    vCantidad = INTEGER(edtBolsasAgregar:SCREEN-VALUE IN FRAME F-Main).
    vHasta    = vDesde + vCantidad.
    ASSIGN tt-bolsas_disponibles.cantidad = tt-bolsas_disponibles.cantidad - vCantidad.
    ASSIGN tt-bolsas_disponibles.desde    = vHasta.
    {&OPEN-QUERY-BROWSE-1}
  END.
  ELSE DO: /*rango completo - eliminar de bolsas disponibles*/
    /*DELETE tt-bolsas_disponibles.*/
    ASSIGN tt-bolsas_disponibles.eliminado = 1.
    BROWSE-1:DELETE-CURRENT-ROW().
  END.

  CREATE tt-bolsas_lote.
    ASSIGN
    tt-bolsas_lote.desde    = vDesde
    tt-bolsas_lote.hasta    = vDesde + vCantidad - 1
    tt-bolsas_lote.cantidad = vCantidad.
  
  {&OPEN-QUERY-BROWSE-2}
  RUN getTotals.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* << */
DO:
  DEFINE VAR vCantidad AS INTEGER NO-UNDO.
  DEFINE VAR vDesde    AS INTEGER NO-UNDO.
  DEFINE VAR vHasta    AS INTEGER NO-UNDO.

  vCantidad = INTEGER(edtBolsasEliminar:SCREEN-VALUE IN FRAME F-Main).
  vDesde    = tt-bolsas_lote.desde.
  vHasta    = tt-bolsas_lote.hasta.

  /*controles de tipo*/
  IF vCantidad <= 0 THEN DO:
    MESSAGE "La cantidad debe ser mayor que 0" VIEW-AS ALERT-BOX.
    RETURN.
  END.
  IF vCantidad > tt-bolsas_lote.cantidad THEN DO:
    MESSAGE "La cantidad no pude ser mayor que la cantidad del rango seleccionado." VIEW-AS ALERT-BOX.
    RETURN.
  END.
  /*fin controles de tipo*/


  IF vCantidad <> tt-bolsas_lote.cantidad THEN DO: /*rango incompleto*/
    vDesde = vDesde.
    ASSIGN tt-bolsas_lote.cantidad = tt-bolsas_lote.cantidad - vCantidad.
    ASSIGN tt-bolsas_lote.desde    = vDesde + vCantidad.
    {&OPEN-QUERY-BROWSE-2}
  END.
  ELSE DO: /*rango completo - eliminar de bolsas lote*/
    /*DELETE tt-bolsas_lote NO-ERROR.*/
    ASSIGN tt-bolsas_lote.eliminado = 1.
    BROWSE-2:DELETE-CURRENT-ROW().
  END.

  CREATE tt-bolsas_disponibles.
  ASSIGN
    tt-bolsas_disponibles.desde        = vDesde 
    tt-bolsas_disponibles.hasta        = vDesde + vCantidad - 1
    tt-bolsas_disponibles.cantidad     = vCantidad
    tt-bolsas_disponibles.applyChanges = 1.
  {&OPEN-QUERY-BROWSE-1}
  RUN getTotals.

  /*Actualizar stock con bolsas desvinculadas del lote*/
  DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
  DEFINE VAR ret      AS LOGICAL NO-UNDO.
  DEFINE VAR vSuc     AS INTEGER NO-UNDO.
  DEFINE VAR vNroMov  AS INTEGER NO-UNDO.
  DEFINE VAR vEmpresa AS INTEGER NO-UNDO.
  DEFINE VAR r        AS ROWID   NO-UNDO.

  lblMsg:SCREEN-VALUE IN FRAME F-Main = "Espere...".
  vHasta = vDesde + vCantidad - 1.
  RUN get-rowid1 IN h_b_lotes_cascara (OUTPUT r).
  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
    vSuc     = lotes_jugo.id_sucursal.
    vNromov  = lotes_jugo.nromov.
    vEmpresa = lotes_jugo.id_empresa.
  END.

  CREATE SERVER hAppSrv.
  
  ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").
  
  RUN y_gstkcre_cas.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT vEmpresa, 
                                                              INPUT vSuc,
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
  /*Fin actualizacion de stock*/
  lblMsg:SCREEN-VALUE IN FRAME F-Main = "Listo".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-34
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-34 W-Win
ON CHOOSE OF BUTTON-34 IN FRAME F-Main /* Aplicar Cambios */
DO:
  DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
  DEFINE VAR ret      AS LOGICAL NO-UNDO.
  DEFINE VAR v        AS INTEGER NO-UNDO.
  DEFINE VAR r        AS ROWID   NO-UNDO.

  DEFINE VAR vSuc     AS INTEGER NO-UNDO.
  DEFINE VAR vNromov  AS INTEGER NO-UNDO.
  DEFINE VAR vEmpresa AS INTEGER NO-UNDO.
  DEFINE VAR vLote    AS INTEGER NO-UNDO.

  DEFINE VAR vAntes   AS INTEGER   NO-UNDO.
  DEFINE VAR vDespues AS INTEGER   NO-UNDO.
  DEFINE VAR vTiempo  AS CHARACTER NO-UNDO.

  
  vAntes = TIME.
  
  lblMsg:SCREEN-VALUE IN FRAME F-Main = "Espere...".
  CREATE SERVER hAppSrv.
  
  ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").
  
  RUN get-rowid1 IN h_b_lotes_cascara (OUTPUT r).
  W-Win:LOAD-MOUSE-POINTER("WAIT").
  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r
                        NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
    vSuc     = lotes_jugo.id_sucursal.
    vNromov  = lotes_jugo.nromov.
    vEmpresa = lotes_jugo.id_empresa.
    vLote    = lotes_jugo.id_lote.

    RUN appCascApplyChanges.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT-OUTPUT TABLE tt-bolsas_disponibles, 
                                                                      INPUT-OUTPUT TABLE tt-bolsas_lote, 
                                                                      INPUT vSuc, 
                                                                      INPUT vNroMov, 
                                                                      INPUT vEmpresa,
                                                                      INPUT vLote).
    IF ERROR-STATUS:ERROR THEN DO:
      ret = hAppSrv:DISCONNECT().
      RETURN NO-APPLY RETURN-VALUE.
    END.

    ret = hAppSrv:DISCONNECT().
    DELETE OBJECT hAppSrv.
  END.
  W-Win:LOAD-MOUSE-POINTER("ARROW").
  {&OPEN-QUERY-BROWSE-1}
  {&OPEN-QUERY-BROWSE-2}
  RUN getTotals.
  vDespues = TIME.
  vTiempo = STRING(vDespues - vAntes, "HH:MM:SS").
  lblMsg:SCREEN-VALUE IN FRAME F-Main = "Listo (" + vTiempo + ")".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-35 W-Win
ON CHOOSE OF BUTTON-35 IN FRAME F-Main /* Ver Asignacion de Bolsas */
DO:
  DEFINE VAR vAntes   AS INTEGER   NO-UNDO.
  DEFINE VAR vDespues AS INTEGER   NO-UNDO.
  DEFINE VAR vTiempo  AS CHARACTER NO-UNDO.
  DEFINE VAR iTime    AS INTEGER   NO-UNDO.

  
  vAntes = TIME.
  lblMsg:SCREEN-VALUE IN FRAME F-Main = "Espere...".
  ETIME(TRUE).
  RUN fillTempTables.
  iTime = ETIME.
  vDespues = TIME.
  vTiempo = STRING(vDespues - vAntes, "HH:MM:SS").
  
  lblMsg:SCREEN-VALUE IN FRAME F-Main = "Listo (" + vTiempo + "), " + STRING(iTime / 1000) + " milisegundos".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/method/winitialize.i}
{custom/method/ctitulo.i}
run deshabilita_viewer.
run habilitar_relacion_viewer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav ).
       RUN set-position IN h_cus-updsav ( 1.24 , 2.20 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.29 , 36.40 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 24.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.29 , 59.40 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_lotes_cascara.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_lotes_cascara ).
       RUN set-position IN h_b_lotes_cascara ( 3.62 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b_lotes_cascara ( 7.14 , 81.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_lotes_cascara.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_lotes_cascara ).
       RUN set-position IN h_v_lotes_cascara ( 4.10 , 85.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.00 , 67.00 ) */

       /* Links to csmartbrowser h_b_lotes_cascara. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_lotes_cascara ).

       /* Links to SmartViewer h_v_lotes_cascara. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_cascara , 'Record':U , h_v_lotes_cascara ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_lotes_cascara ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-updsav ,
             BUTTON-35:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-navico ,
             h_cus-updsav , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             h_cus-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_lotes_cascara ,
             BUTTON-35:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v_lotes_cascara ,
             h_b_lotes_cascara , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appFillTempTables W-Win 
PROCEDURE appFillTempTables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
DEFINE VAR ret      AS LOGICAL NO-UNDO.
DEFINE VAR v        AS INTEGER NO-UNDO.

CREATE SERVER hAppSrv.

ret = hAppSrv:CONNECT("-AppService asindustria -H progress2001 -S 5162").

ret = SESSION:EXPORT().
ret = SESSION:EXPORT("appTest01.p").

RUN appTest01.p ON SERVER hAppSrv TRANSACTION DISTINCT (OUTPUT v).
IF ERROR-STATUS:ERROR THEN DO:
  ret = hAppSrv:DISCONNECT().
  RETURN ERROR RETURN-VALUE.
END.
DISP v.
ret = hAppSrv:DISCONNECT().
DELETE OBJECT hAppSrv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros W-Win 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consulta W-Win 
PROCEDURE consulta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_consultas as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_consultas = "" then
    message "No hay consultas disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_consultas,output cresult).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame-rowid-lote W-Win 
PROCEDURE dame-rowid-lote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER r AS ROWID.

RUN GET-rowid1 IN h_b_lotes_cascara (OUTPUT r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer W-Win 
PROCEDURE deshabilita_viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-1 edtBolsasAgregar edtBolsasEliminar edtDispoTotal 
          edtLoteTotal lblMsg 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-35 BROWSE-1 BROWSE-2 BUTTON-2 edtBolsasAgregar BUTTON-3 
         edtBolsasEliminar edtDispoTotal edtLoteTotal BUTTON-34 lblMsg d RECT-2 
         RECT-3 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTempTables W-Win 
PROCEDURE fillTempTables :
/********************************************************
Purpose:       Define TT de bolsas disponibles.
Parameters:    
Author:        Facundo Juarez
Last Modified: 25/06/2003 11:32 am 
**********************************************************/  
  
  DEFINE VAR hAppSrv  AS HANDLE  NO-UNDO.
  DEFINE VAR ret      AS LOGICAL NO-UNDO.
  DEFINE VAR v        AS INTEGER NO-UNDO.
  DEFINE VAR r        AS ROWID   NO-UNDO.

  DEFINE VAR vSuc     AS INTEGER NO-UNDO.
  DEFINE VAR vNromov  AS INTEGER NO-UNDO.
  DEFINE VAR vEmpresa AS INTEGER NO-UNDO.
  DEFINE VAR vLote    AS INTEGER NO-UNDO.
  
  CREATE SERVER hAppSrv.
  
  ret = hAppSrv:CONNECT("-AppService asindustria -H tucuman1 -S 5162").
  
  RUN get-rowid1 IN h_b_lotes_cascara (OUTPUT r).

  W-Win:LOAD-MOUSE-POINTER("WAIT").
  
  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r
                      NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
    vSuc     = lotes_jugo.id_sucursal.
    vNromov  = lotes_jugo.nromov.
    vEmpresa = lotes_jugo.id_empresa.
    vLote    = lotes_jugo.id_lote.

    RUN appCascFillTempTables.p ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT-OUTPUT TABLE tt-bolsas_disponibles, 
                                                                        INPUT-OUTPUT TABLE tt-bolsas_lote, 
                                                                        INPUT vSuc, 
                                                                        INPUT vNroMov, 
                                                                        INPUT vEmpresa,
                                                                        INPUT vLote).
    IF ERROR-STATUS:ERROR THEN DO:
      ret = hAppSrv:DISCONNECT().
      RETURN NO-APPLY RETURN-VALUE.
    END.

    ret = hAppSrv:DISCONNECT().
    DELETE OBJECT hAppSrv.
  END.
  W-Win:LOAD-MOUSE-POINTER("ARROW").
  {&OPEN-QUERY-BROWSE-1}
  {&OPEN-QUERY-BROWSE-2}
  RUN getTotals.

END.













/*
/********************************************************
Purpose:       Define TT de bolsas disponibles.
Parameters:    
Author:        Facundo Juarez
Last Modified: 25/06/2003 11:32 am 
*********************************************************/

DEFINE VAR i LIKE tambores_industria.id_tambor  NO-UNDO.
DEFINE VAR j LIKE tambores_industria.id_tambor  NO-UNDO.
DEFINE VAR k LIKE tambores_industria.id_tambor  NO-UNDO.
DEFINE VAR p LIKE tambores_industria.id_tambor  NO-UNDO.
DEFINE VAR q LIKE tambores_industria.id_tambor  NO-UNDO.
DEFINE VAR s AS CHARACTER NO-UNDO.
DEFINE VAR r AS ROWID     NO-UNDO.

DEFINE VAR vSuc     AS INTEGER NO-UNDO.
DEFINE VAR vNromov  AS INTEGER NO-UNDO.
DEFINE VAR vEmpresa AS INTEGER NO-UNDO.
DEFINE VAR vLote    AS INTEGER NO-UNDO.
DEFINE VAR v1Vez    AS INTEGER NO-UNDO.
DEFINE VAR vIdDesde AS INTEGER NO-UNDO.
DEFINE VAR vIdHasta AS INTEGER NO-UNDO.

FOR EACH tt-bolsas_disponibles.
  DELETE tt-bolsas_disponibles.
END.
FOR EACH tt-bolsas_lote.
  DELETE tt-bolsas_lote.
END.
RUN get-rowid1 IN h_b_lotes_cascara (OUTPUT r).

FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r
                      NO-LOCK NO-ERROR.
IF AVAILABLE lotes_jugo THEN DO:
  vSuc     = lotes_jugo.id_sucursal.
  vNromov  = lotes_jugo.nromov.
  vEmpresa = lotes_jugo.id_empresa.
  vLote    = lotes_jugo.id_lote.
END.


/*encuentro los rangos  de tambores_industria - algoritmo de pablo (mas eficiente)*/

/*encuentro primer disponible*/
FOR EACH tambores_industria WHERE tambores_industria.id_tipotambor         = 11
                              AND tambores_industria.id_sucursal_ubicacion = vSuc
                              AND tambores_industria.id_locacion_ubicacion = 4
                              AND tambores_industria.nromov_destino        = 0
                            BY tambores_industria.id_tambor.
  j = tambores_industria.id_tambor.
  vIdDesde = tambores_industria.id_tambor.
  LEAVE.
END. /*for each find first*/

/*encuentro ultimo disponible*/
FOR EACH tambores_industria WHERE tambores_industria.id_tipotambor          = 11
                              AND tambores_industria.id_sucursal_ubicacion = vSuc
                              AND tambores_industria.id_locacion_ubicacion = 4
                              AND tambores_industria.nromov_destino        = 0
                             BY tambores_industria.id_tambor DESC.
  k = tambores_industria.id_tambor.
  q = tambores_industria.id_tambor.
  vIdHasta = tambores_industria.id_tambor.
  LEAVE.
END. /*for each find first*/

/*filtro los no disponibles*/
i = 0.
FOR EACH tambores_industria FIELDS(tambores_industria.id_tipotambor 
                                   tambores_industria.id_sucursal_ubicacion
                                   tambores_industria.id_locacion_ubicacion 
                                   tambores_industria.nromov_destino
                                   tambores_industria.id_tambor)
                            WHERE tambores_industria.id_tipotambor         =  11
                              AND tambores_industria.id_sucursal_ubicacion =  vSuc
                              AND tambores_industria.id_locacion_ubicacion <> 4
                              AND tambores_industria.nromov_destino        <> 0
                              AND tambores_industria.id_tambor             >  j
                            BY tambores_industria.id_tambor.
  IF i = 0 AND k <> 0 AND q <> 0 THEN DO: /*para el primer rango*/  
    k = tambores_industria.id_tambor - 1.
    vIdHasta = k.
    CREATE tt-bolsas_disponibles.
    ASSIGN
      tt-bolsas_disponibles.desde           = j
      tt-bolsas_disponibles.hasta           = k
      tt-bolsas_disponibles.cantidad        = k - j + 1
      tt-bolsas_disponibles.id_tambor_desde = vIdDesde 
      tt-bolsas_disponibles.id_tambor_hasta = vIdHasta.
  END. /*if i=0*/
  IF tambores_industria.id_tambor = k + 1 THEN DO:
    k = tambores_industria.id_tambor.
  END.
  ELSE DO:
    vIdHasta = tambores_industria.id_tambor - 1.
    CREATE tt-bolsas_disponibles.
    ASSIGN
      tt-bolsas_disponibles.desde           = k + 1
      tt-bolsas_disponibles.hasta           = tambores_industria.id_tambor - 1
      tt-bolsas_disponibles.cantidad        = tt-bolsas_disponibles.hasta - tt-bolsas_disponibles.desde + 1
      tt-bolsas_disponibles.id_tambor_desde = vIdDesde 
      tt-bolsas_disponibles.id_tambor_hasta = vIdHasta.
    k = tambores_industria.id_tambor.
  END. /*if*/
  i = i + 1.
END. /*for each find first*/

IF q > k AND k <> tt-bolsas_disponibles.desde THEN DO: /*hay bolsas disponibles luego del ultimo rango ocupado*/
  CREATE tt-bolsas_disponibles.
    ASSIGN
      tt-bolsas_disponibles.desde           = k + 1
      tt-bolsas_disponibles.hasta           = q
      tt-bolsas_disponibles.cantidad        = tt-bolsas_disponibles.hasta - tt-bolsas_disponibles.desde + 1
      tt-bolsas_disponibles.id_tambor_desde = k + q 
      tt-bolsas_disponibles.id_tambor_hasta = q.
END.

IF i = 0  AND k <> 0 THEN DO: /*significa que no entro al for each porque todas las bolsas que hay estan disponibles.*/
  CREATE tt-bolsas_disponibles.
    ASSIGN
      tt-bolsas_disponibles.desde           = j
      tt-bolsas_disponibles.hasta           = k
      tt-bolsas_disponibles.cantidad        = tt-bolsas_disponibles.hasta - tt-bolsas_disponibles.desde + 1
      tt-bolsas_disponibles.id_tambor_desde = vIdDesde 
      tt-bolsas_disponibles.id_tambor_hasta = vIdHasta.
END.


/*fin rangos disponibles*/
/********************************************************************************************/
/*encuentro rangos de tambores_industria. asociados al lote seleccionado*/

j = 0.
k = 0.
q = 0.
/*encuentro primer asociado a algun lote*/
FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino    = vEmpresa
                              AND tambores_industria.id_sucursal_destino   = vSuc
                              AND tambores_industria.id_tipotambor_destino = 11
                              AND tambores_industria.nromov_destino        = vNroMov
                              AND tambores_industria.id_lote               = vLote
                            BY tambores_industria.id_tambor.
  j = tambores_industria.id_tambor.
  k = tambores_industria.id_tambor - 1.
  vIdDesde = tambores_industria.id_tambor.
  LEAVE.
END.

/*encuentro ultimo asociado a algun lote*/
FOR EACH tambores_industria WHERE tambores_industria.id_empresa_destino    = vEmpresa
                              AND tambores_industria.id_sucursal_destino   = vSuc
                              AND tambores_industria.id_tipotambor_destino = 11
                              AND tambores_industria.nromov_destino        = vNromov
                              AND tambores_industria.id_lote               = vLote
                            BY tambores_industria.id_tambor DESC.
  q = tambores_industria.id_tambor.
  vIdHasta = tambores_industria.id_tambor.
  LEAVE.
END.

i = 0.
FOR EACH tambores_industria FIELDS(industria.tambores_industria.id_empresa_destino
                                   general.tambores_industria.id_sucursal_destino   
                                   general.tambores_industria.id_tipotambor_destino 
                                   general.tambores_industria.nromov_destino        
                                   tambores_industria.id_lote
                                   tambores_industria.id_tambor)
                            WHERE general.tambores_industria.id_empresa_destino    = vEmpresa
                              AND general.tambores_industria.id_sucursal_destino   = vSuc
                              AND general.tambores_industria.id_tipotambor_destino = 11
                              AND general.tambores_industria.nromov_destino        = vNromov
                              AND tambores_industria.id_lote                         = vLote
                            BY tambores_industria.id_tambor.
  IF tambores_industria.id_tambor = k + 1  THEN DO:
    k = tambores_industria.id_tambor.
  END.
  ELSE DO:
    CREATE tt-bolsas_lote.
    ASSIGN
      tt-bolsas_lote.desde           = j
      tt-bolsas_lote.hasta           = k
      tt-bolsas_lote.cantidad        = k - j + 1
      tt-bolsas_lote.id_tambor_desde = j 
      tt-bolsas_lote.id_tambor_hasta = k.
    j = tambores_industria.id_tambor.
    k = tambores_industria.id_tambor.
    i = i + 1.
  END.
  
END.


IF i = 0  AND j <> 0 AND k <> 0 THEN DO: /*significa que no entro al for each porque todas las bolsas que hay estan en lotes, o entro al foreach y salio sin entrar por el else lo que significa que  hay un solo rango en el lote*/
  CREATE tt-bolsas_lote.
  ASSIGN
    tt-bolsas_lote.desde           = j
    tt-bolsas_lote.hasta           = k
    tt-bolsas_lote.cantidad        = k - j + 1
    tt-bolsas_lote.id_tambor_desde = j 
    tt-bolsas_lote.id_tambor_hasta = k.
END.

IF k > j AND i <> 0 THEN DO: /*queda un ultimo rango en el lote*/
  CREATE tt-bolsas_lote.
  ASSIGN
    tt-bolsas_lote.desde           = j
    tt-bolsas_lote.hasta           = k
    tt-bolsas_lote.cantidad        = k - j + 1
    tt-bolsas_lote.id_tambor_desde = j 
    tt-bolsas_lote.id_tambor_hasta = q.
END.


/*fin rangos lote*/

{&OPEN-QUERY-BROWSE-1}
{&OPEN-QUERY-BROWSE-2}
RUN getTotals.

END PROCEDURE.

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-deshabilitados-paleta W-Win 
PROCEDURE get-deshabilitados-paleta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter hprograma as handle no-undo.
define output parameter estados as character no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTotals W-Win 
PROCEDURE getTotals :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR vTotDispo AS INTEGER NO-UNDO.
DEFINE VAR vTotLote  AS INTEGER NO-UNDO.

FOR EACH tt-bolsas_disponibles WHERE tt-bolsas_disponibles.eliminado <> 1
                               NO-LOCK.
  vTotDispo = vTotDispo + tt-bolsas_disponibles.cantidad.
END.
FOR EACH tt-bolsas_lote WHERE tt-bolsas_lote.eliminado <> 1
                        NO-LOCK.
  vTotLote = vTotLote + tt-bolsas_lote.cantidad.
END.

edtDispoTotal:SCREEN-VALUE IN FRAME F-Main = STRING(vTotDispo).
edtLoteTotal:SCREEN-VALUE IN FRAME F-Main  = STRING(vTotLote).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresion W-Win 
PROCEDURE impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_reportes,output cresult).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy W-Win 
PROCEDURE post-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create W-Win 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete W-Win 
PROCEDURE post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update W-Win 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy W-Win 
PROCEDURE pre-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
alta = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create W-Win 
PROCEDURE pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
alta = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete W-Win 
PROCEDURE pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update W-Win 
PROCEDURE pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetea-registro W-Win 
PROCEDURE resetea-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-bolsas_lote"}
  {src/adm/template/snd-list.i "tt-bolsas_disponibles"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

