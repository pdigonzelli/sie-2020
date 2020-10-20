&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
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

DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.


DEFINE TEMP-TABLE tt-oe 
    RCODE-INFORMATION
    FIELD id_orden_entrega              AS INTEGER  COLUMN-LABEL "OE"
    FIELD anio_oe                       AS INTEGER  COLUMN-LABEL "Año"
    FIELD semana                        AS INTEGER  COLUMN-LABEL "Semana"
    FIELD id_despachante                AS INTEGER  COLUMN-LABEL "Cod.Despachante"
    FIELD despachante                   AS CHAR     COLUMN-LABEL "Despachante"
    FIELD id_vapor                      LIKE vapores.id_vapor COLUMN-LABEL "Cod.Vapor"
    FIELD vapor                         AS CHARACT  COLUMN-LABEL "Vapor"
    FIELD fecha_salida                  AS DATE     COLUMN-LABEL "Fecha Salida"
    FIELD id_agencia                    AS INTEGER  COLUMN-LABEL "Cod.Agencia"
    FIELD agencia                       AS CHAR        COLUMN-LABEL "Agencia"
    FIELD id_destino                    AS INTEGER  COLUMN-LABEL "Cod.Destino"
    FIELD destino                       AS CHAR        COLUMN-LABEL "Destino"
    FIELD cantidad_tambores             AS integer  COLUMN-LABEL "Tambores"
    FIELD total_factura                 AS decimal  COLUMN-LABEL "Total factura"            DECIMALS 4
    FIELD contenedores                  AS decimal  COLUMN-LABEL "Contenedores"             DECIMALS 2
    FIELD importe_comisiones            AS DECIMAL  COLUMN-LABEL "Comision"                 DECIMALS 2
    FIELD fob_ton                       AS decimal  COLUMN-LABEL "Fob Total"                DECIMALS 4
    FIELD plazo                         AS integer  COLUMN-LABEL "Plazo"

    FIELD GastoClausulaEntry            AS DECIMAL COLUMN-LABEL "Entry por Clausula"        DECIMALS 4
    FIELD GastoClausulaFlete            AS DECIMAL COLUMN-LABEL "Flete por Clausula"        DECIMALS 4
    FIELD GastoClausulaSeguro           AS DECIMAL COLUMN-LABEL "Seguro por Clausula"       DECIMALS 4
    FIELD GastoClausulaGenerico         AS DECIMAL COLUMN-LABEL "Generico por Clausula"     DECIMALS 4
    FIELD GastoClausulaBunker           AS DECIMAL COLUMN-LABEL "Bunker por Clausula"       DECIMALS 4
    FIELD GastoClausulaThcOrigen        AS DECIMAL COLUMN-LABEL "THC Origen por Clausula"   DECIMALS 4
    FIELD GastoClausulaThcDestino       AS DECIMAL COLUMN-LABEL "THC Destino por Clausula"  DECIMALS 4
    FIELD GastoClausulaInbSur           AS DECIMAL COLUMN-LABEL "Inbalance por Clausula"    DECIMALS 4
    FIELD GastoClausulaDcr              AS DECIMAL COLUMN-LABEL "DCR por Clausula"          DECIMALS 4
    FIELD GastoClausulaEbaf             AS DECIMAL COLUMN-LABEL "EBAF por Clausula"         DECIMALS 4
    FIELD GastoAgenciaEntry             AS DECIMAL COLUMN-LABEL "Entry por Agencia"         DECIMALS 4
    FIELD GastoAgenciaFlete             AS DECIMAL COLUMN-LABEL "Flete por Agencia"         DECIMALS 4
    FIELD GastoAgenciaThc               AS DECIMAL COLUMN-LABEL "THC por Agencia"           DECIMALS 4
    FIELD GastoAgenciaSeguro            AS DECIMAL COLUMN-LABEL "Seguro por Agencia"        DECIMALS 4
    FIELD GastoAgenciaVarios            AS DECIMAL COLUMN-LABEL "Varios  por Agencia"       DECIMALS 4
    FIELD GastoAgenciaToll              AS DECIMAL COLUMN-LABEL "Toll por Agencia"          DECIMALS 4
    FIELD GastoAgenciaHandling          AS DECIMAL COLUMN-LABEL "Handling por Agencia"      DECIMALS 4
    FIELD GastoAgenciaInLand            AS DECIMAL COLUMN-LABEL "InLand por Agencia"        DECIMALS 4
    FIELD GastoAgenciaBunker            AS DECIMAL COLUMN-LABEL "Bunker por Agencia"        DECIMALS 4
    FIELD GastoAgenciaBL                AS DECIMAL COLUMN-LABEL "BL por Agencia"            DECIMALS 4
    FIELD GastoAgenciaThcDestino        AS DECIMAL COLUMN-LABEL "THC Destino por Agencia"   DECIMALS 4
    FIELD GastoAgenciaInbSur            AS DECIMAL COLUMN-LABEL "Inbalance por Agencia"     DECIMALS 4
    FIELD GastoAgenciaDcr               AS DECIMAL COLUMN-LABEL "DCR por Agencia"           DECIMALS 4
    FIELD GastoAgenciaAgp               AS DECIMAL COLUMN-LABEL "AGP por Agencia"           DECIMALS 4
    FIELD GastoAgenciaT7                AS DECIMAL COLUMN-LABEL "T.7 por Agencia"           DECIMALS 4
    FIELD GastoAgenciaEbaf              AS DECIMAL COLUMN-LABEL "EBAF por Agencia"          DECIMALS 4
    FIELD GastoAgenciaAms               AS DECIMAL COLUMN-LABEL "AMS por Agencia"           DECIMALS 4.



DEFINE TEMP-TABLE tt-oe-2 
    RCODE-INFORMATION
    FIELD id_orden_entrega              AS INTEGER  COLUMN-LABEL "OE"
    FIELD anio_oe                       AS INTEGER  COLUMN-LABEL "Año"
    FIELD factura                       AS CHARACTER COLUMN-LABEL "Facturas"  
    FIELD semana                        AS INTEGER  COLUMN-LABEL "Semana"
    FIELD id_cliente                    AS CHAR COLUMN-LABEL "Cod.Cliente"
    FIELD razon_social                  LIKE clientes.razon_social COLUMN-LABEL "Razón Social"
    FIELD producto                      AS CHARACTER COLUMN-LABEL "Productos" 
    FIELD id_clausula                   AS CHAR COLUMN-LABEL "Cod.Clausula"
    FIELD clausula                      AS CHARACTER COLUMN-LABEL "Clausula"                
    FIELD id_vapor                      LIKE vapores.id_vapor COLUMN-LABEL "Cod.Vapor"
    FIELD vapor                         AS CHARACT  COLUMN-LABEL "Vapor"
    FIELD fecha_salida                  AS DATE     COLUMN-LABEL "ETD"
    FIELD fecha_arribo                  AS DATE     COLUMN-LABEL "ETA"
    FIELD id_agencia                    AS INTEGER  COLUMN-LABEL "Cod.Agencia"
    FIELD agencia                       AS CHAR     COLUMN-LABEL "Agencia"
    FIELD id_destino                    AS INTEGER  COLUMN-LABEL "Cod.Destino"
    FIELD destino                       AS CHAR     COLUMN-LABEL "Destino"
    FIELD pais_destino                  AS CHAR     COLUMN-LABEL "Pais Destino"
    FIELD cant_tambores                 AS INTEGER COLUMN-LABEL "Tambores"
    FIELD cant_contenedores             AS INTEGER COLUMN-LABEL "Contenedores"
    FIELD broker                       AS CHAR     COLUMN-LABEL "Broker"
    FIELD tot_comision  AS DECIMAL COLUMN-LABEL "Comision"
    FIELD tot_service_free AS DECIMAL COLUMN-LABEL "Service Free"
    FIELD vto_factura AS DATE  COLUMN-LABEL "Vto Factura"
    FIELD tot_gastos_maritima AS DECIMAL COLUMN-LABEL "Gtos Maritima"
    FIELD vto_gastos_maritima AS DATE  COLUMN-LABEL "Vto GM"
    FIELD tot_derechos_exp AS DECIMAL COLUMN-LABEL "Derechos Exp"
    FIELD vto_derechos_exp AS DATE COLUMN-LABEL  "Vto DE"
    FIELD id_despachante                AS INTEGER  COLUMN-LABEL "Cod.Desp"
    FIELD despachante                   AS CHAR     COLUMN-LABEL "Despachante"
    FIELD tot_gastos_puerto AS DECIMAL COLUMN-LABEL "Gtos Puerto Origen"
    FIELD vto_gastos_puerto AS DATE COLUMN-LABEL  "Vto GP"
    FIELD tot_duties AS DECIMAL COLUMN-LABEL "Duties"
    FIELD vto_duties AS DATE COLUMN-LABEL  "Vto Duties"
    FIELD despachante_destino  AS CHAR     COLUMN-LABEL "Despachante Destino"
    FIELD tot_entry_free AS DECIMAL COLUMN-LABEL "Entry Free"
    FIELD vto_entry_free AS DATE COLUMN-LABEL  "Vto EF"
    FIELD tot_delivery AS DECIMAL COLUMN-LABEL "Delivery en destino"
    FIELD vto_delivery AS DATE COLUMN-LABEL  "Vto Delivery"
    FIELD tot_fca AS DECIMAL COLUMN-LABEL "FCA Costs"
    FIELD vto_fca AS DATE COLUMN-LABEL  "Vto FCA"
    FIELD tot_warehouse AS DECIMAL COLUMN-LABEL "Warehouse"
    FIELD vto_warehouse AS DATE COLUMN-LABEL  "Vto Warehouse"
    FIELD tot_palletizing AS DECIMAL COLUMN-LABEL "Palletizing"
    FIELD vto_palletizing AS DATE COLUMN-LABEL  "Vto Palletizing"
    .





DEFINE VARIABLE vfechadesde AS DATE       NO-UNDO.
DEFINE VARIABLE vfechahasta AS DATE       NO-UNDO.
DEFINE VARIABLE vcliente    AS INTEGER    NO-UNDO.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOE fi-OE fi-desde fi-hasta BUTTON-1 ~
BUTTON-2 BUTTON-3 
&Scoped-Define DISPLAYED-OBJECTS fi-OE fi-desde fi-hasta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD queryName wWin 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-fMain 
       MENU-ITEM m_Query_Constructor LABEL "Query Constructor".


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOE 
     LABEL "Consultar Gastos OE" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-1 
     LABEL "&Exportar 1" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "&Cancelar" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "&Exportar 2" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE fi-desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Desde" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi-hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Hasta" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi-OE AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Orden Entrega" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnOE AT ROW 8.05 COL 51
     fi-OE AT ROW 8.05 COL 18 COLON-ALIGNED
     fi-desde AT ROW 3.38 COL 18 COLON-ALIGNED
     fi-hasta AT ROW 4.57 COL 18 COLON-ALIGNED
     BUTTON-1 AT ROW 1.95 COL 51
     BUTTON-2 AT ROW 4.81 COL 51
     BUTTON-3 AT ROW 3.14 COL 51
     "  Filtro" VIEW-AS TEXT
          SIZE 45 BY .71 AT ROW 1.95 COL 5
          BGCOLOR 1 FGCOLOR 15 
     "Consutla Rapida de Presupuetos de Gastos OE" VIEW-AS TEXT
          SIZE 45 BY .71 AT ROW 7.05 COL 5
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.6 BY 10.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Exportacion a Excell OE"
         HEIGHT             = 10.76
         WIDTH              = 76.6
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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
   Custom                                                               */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Exportacion a Excell OE */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Exportacion a Excell OE */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOE wWin
ON CHOOSE OF btnOE IN FRAME fMain /* Consultar Gastos OE */
DO:
DEFINE VAR vTotalFactura            LIKE orden_entrega.total_factura    NO-UNDO.
DEFINE VAR v_Fob                    AS DECIMAL DECIMALS 4               NO-UNDO.
DEFINE VAR vKilos                   AS INTEGER                          NO-UNDO.
DEFINE VAR vTambores                AS INTEGER                          NO-UNDO.
DEFINE VAR vComision                AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaEntry      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaFlete      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaSeguro     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaGenerico   AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaBunker     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaThcOrigen  AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaThcDestino AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaInbSur     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaDcr        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaEbaf       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgencia            AS CHARACTER                        NO-UNDO.
DEFINE VAR vRes                     AS CHARACTER                        NO-UNDO.
DEFINE VAR vGastoAgenciaEntry       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaFlete       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaThc         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaSeguro      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaVarios      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaToll        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaHandling    AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaInLand      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaBunker      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaBL          AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaThcDestino  AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaInbSur      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaDcr         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaAgp         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaT7          AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaEbaf        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaAms         AS DECIMAL                          NO-UNDO.

DEFINE VARIABLE iOE AS INTEGER    NO-UNDO.

iOE = INTEGER(fi-OE:SCREEN-VALUE).


  RUN p_prespupesto_oe.p (
      INPUT iOE, 
      OUTPUT vTotalFactura,
      OUTPUT v_Fob,
      OUTPUT vKilos,
      OUTPUT vTambores,
      OUTPUT vComision,
      OUTPUT vGastoClausulaEntry,
      OUTPUT vGastoClausulaFlete,
      OUTPUT vGastoClausulaSeguro,
      OUTPUT vGastoClausulaGenerico,
      OUTPUT vGastoClausulaBunker,
      OUTPUT vGastoClausulaThcOrigen,
      OUTPUT vGastoClausulaThcDestino,
      OUTPUT vGastoClausulaInbSur,
      OUTPUT vGastoClausulaDcr,
      OUTPUT vGastoClausulaEbaf,
      OUTPUT vGastoAgenciaEntry,
      OUTPUT vGastoAgenciaFlete,
      OUTPUT vGastoAgenciaThc,
      OUTPUT vGastoAgenciaSeguro,
      OUTPUT vGastoAgenciaVarios,
      OUTPUT vGastoAgenciaToll,
      OUTPUT vGastoAgenciaHandling,
      OUTPUT vGastoAgenciaInLand,
      OUTPUT vGastoAgenciaBunker,
      OUTPUT vGastoAgenciaBL,
      OUTPUT vGastoAgenciaThcDestino,
      OUTPUT vGastoAgenciaInbSur,
      OUTPUT vGastoAgenciaDcr,
      OUTPUT vGastoAgenciaAgp,
      OUTPUT vGastoAgenciaT7,
      OUTPUT vGastoAgenciaEbaf,
      OUTPUT vGastoAgenciaAms,
      OUTPUT vGastoAgencia).

vRes =  "Total Factura: "  + STRING(vTotalFactura)            + CHR(13) +
        "Kilos: "          + STRING(vKilos)                   + CHR(13) +
        "Tambores: "       + STRING(vTambores)                + CHR(13) +
        "Comision: "       + STRING(vComision)                + CHR(13) +
        "Cl Entry: "       + STRING(vGastoClausulaEntry)      + CHR(13) +
        "Cl Flete: "       + STRING(vGastoClausulaFlete)      + CHR(13) +
        "Cl Seguro: "      + STRING(vGastoClausulaSeguro)     + CHR(13) +
        "Cl Generico: "    + STRING(vGastoClausulaGenerico)   + CHR(13) +
        "Cl Bunker: "      + STRING(vGastoClausulaBunker)     + CHR(13) +
        "Cl THC Origen: "  + STRING(vGastoClausulaThcOrigen)  + CHR(13) +
        "Cl THC Destino: " + STRING(vGastoClausulaThcDestino) + CHR(13) +
        "Cl Inbalance: "   + STRING(vGastoClausulaInbSur)     + CHR(13) +
        "Cl DCR: "         + STRING(vGastoClausulaDcr)        + CHR(13) +
        "Cl EBAF: "        + STRING(vGastoClausulaEbaf)       + CHR(13) +
        "Ag Entry: "       + STRING(vGastoAgenciaEntry)       + CHR(13) +
        "Ag Flete: "       + STRING(vGastoAgenciaFlete)       + CHR(13) +
        "Ag Thc: "         + STRING(vGastoAgenciaThc)         + CHR(13) +
        "Ag Seguro: "      + STRING(vGastoAgenciaSeguro)      + CHR(13) +
        "Ag Varios: "      + STRING(vGastoAgenciaVarios)      + CHR(13) +
        "Ag Toll: "        + STRING(vGastoAgenciaToll)        + CHR(13) +
        "Ag Handling: "    + STRING(vGastoAgenciaHandling)    + CHR(13) +
        "Ag InLand: "      + STRING(vGastoAgenciaInLand)      + CHR(13) +
        "Ag Bunker: "      + STRING(vGastoAgenciaBunker)      + CHR(13) +
        "Ag BL: "          + STRING(vGastoAgenciaBL)          + CHR(13) +
        "Ag Thc Destino: " + STRING(vGastoAgenciaThcDestino)  + CHR(13) +
        "Ag Inb Sur: "     + STRING(vGastoAgenciaInbSur)      + CHR(13) +
        "Ag Dcr: "         + STRING(vGastoAgenciaDcr)         + CHR(13) +
        "Ag Agp: "         + STRING(vGastoAgenciaAgp)         + CHR(13) +
        "Ag T7: "          + STRING(vGastoAgenciaT7)          + CHR(13) +
        "Ag Ebaf: "        + STRING(vGastoAgenciaEbaf)        + CHR(13) +
        "Ag Ams: "         + STRING(vGastoAgenciaAms)         + CHR(13) +
        "Gastos Agencia "  + chr(13) + vGastoAgencia.
MESSAGE vRes VIEW-AS ALERT-BOX.

 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Exportar 1 */
DO:
   vfechadesde = DATE(fi-desde:SCREEN-VALUE).
   vfechahasta = DATE(fi-hasta:SCREEN-VALUE).
   
   RUN creaTT.
   RUN generateExcel.p (INPUT TABLE TT-oe,
                        INPUT " Presupuestos de OE",
                        INPUT " Fecha Desde: " + fi-desde:SCREEN-VALUE + " - " + "Fecha Hasta: " + fi-hasta:SCREEN-VALUE  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Century Gothic",
                        INPUT 7).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Cancelar */
DO:
  APPLY "window-close" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME fMain /* Exportar 2 */
DO:
   vfechadesde = DATE(fi-desde:SCREEN-VALUE).
   vfechahasta = DATE(fi-hasta:SCREEN-VALUE).
   

   RUN creaTT-2.
   RUN generateExcel.p (INPUT TABLE TT-oe-2,
                        INPUT " Presupuestos de OE",
                        INPUT " Fecha Desde: " + fi-desde:SCREEN-VALUE + " - " + "Fecha Hasta: " + fi-hasta:SCREEN-VALUE  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Century Gothic",
                        INPUT 7).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Query_Constructor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Query_Constructor wWin
ON CHOOSE OF MENU-ITEM m_Query_Constructor /* Query Constructor */
DO:
    DEFINE VAR xSDOS        AS CHARACTER    NO-UNDO.
    DEFINE VAR i            AS INTEGER      NO-UNDO.
    DEFINE VAR iPage        AS INTEGER      NO-UNDO.
    DEFINE VAR iActualPage  AS INTEGER      NO-UNDO.
    DEFINE VAR xDataSource  AS CHARACTER    NO-UNDO.
    DEFINE VAR hDataSource  AS HANDLE       NO-UNDO.

    xSDOS = DYNAMIC-FUNCTION ('getSDO').
    {get CurrentPage iActualPage}.

    DO i = 1 TO NUM-ENTRIES(xSDOS):
        qh = WIDGET-HANDLE(ENTRY(i,xSDOS)).
        {get ObjectPage iPage qh}.
        {get DataSource xDataSource qh}.
        hDataSource = WIDGET-HANDLE(xDataSource).
        IF ( iPage = iActualPage OR iPage = 0 ) AND NOT valid-handle(hDataSource)THEN
            RUN adm2/support/wquery.w ( INPUT qh ).
        ELSE
            MESSAGE 'No puede ejecutar consulta en el detalle' VIEW-AS ALERT-BOX WARNING.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTT wWin 
PROCEDURE creaTT :
/********************************************************
Purpose:       Lanzar el programa que estima los datos de la OE.
Parameters:    INPUT  pIdOrdenEntrega id_orden_entrega
               el resto son los parametros de salida con los gastos de la OE
Author:        Facundo Juarez
Last Modified: 21/05/2003 11:32 am 
*********************************************************/


DEFINE VAR vTotalFactura            LIKE orden_entrega.total_factura    NO-UNDO.
DEFINE VAR v_Fob                    AS DECIMAL DECIMALS 4               NO-UNDO.
DEFINE VAR vKilos                   AS INTEGER                          NO-UNDO.
DEFINE VAR vTambores                AS INTEGER                          NO-UNDO.
DEFINE VAR vComision                AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaEntry      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaFlete      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaSeguro     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaGenerico   AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaBunker     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaThcOrigen  AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaThcDestino AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaInbSur     AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaDcr        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoClausulaEbaf       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgencia            AS CHARACTER                        NO-UNDO.
DEFINE VAR vRes                     AS CHARACTER                        NO-UNDO.
DEFINE VAR vGastoAgenciaEntry       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaFlete       AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaThc         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaSeguro      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaVarios      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaToll        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaHandling    AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaInLand      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaBunker      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaBL          AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaThcDestino  AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaInbSur      AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaDcr         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaAgp         AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaT7          AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaEbaf        AS DECIMAL                          NO-UNDO.
DEFINE VAR vGastoAgenciaAms         AS DECIMAL                          NO-UNDO.


FOR EACH tt-oe.
    DELETE tt-oe.
END.

FOR EACH orden_entrega WHERE orden_entrega.fecha >= vfechadesde
                         AND orden_entrega.fecha <= vfechahasta
                         AND orden_entrega.id_orden_entrega > 1000
                         NO-LOCK.

  RUN p_prespupesto_oe.p (
      INPUT orden_entrega.id_orden_entrega, 
      OUTPUT vTotalFactura,
      OUTPUT v_Fob,
      OUTPUT vKilos,
      OUTPUT vTambores,
      OUTPUT vComision,
      OUTPUT vGastoClausulaEntry,
      OUTPUT vGastoClausulaFlete,
      OUTPUT vGastoClausulaSeguro,
      OUTPUT vGastoClausulaGenerico,
      OUTPUT vGastoClausulaBunker,
      OUTPUT vGastoClausulaThcOrigen,
      OUTPUT vGastoClausulaThcDestino,
      OUTPUT vGastoClausulaInbSur,
      OUTPUT vGastoClausulaDcr,
      OUTPUT vGastoClausulaEbaf,
      OUTPUT vGastoAgenciaEntry,
      OUTPUT vGastoAgenciaFlete,
      OUTPUT vGastoAgenciaThc,
      OUTPUT vGastoAgenciaSeguro,
      OUTPUT vGastoAgenciaVarios,
      OUTPUT vGastoAgenciaToll,
      OUTPUT vGastoAgenciaHandling,
      OUTPUT vGastoAgenciaInLand,
      OUTPUT vGastoAgenciaBunker,
      OUTPUT vGastoAgenciaBL,
      OUTPUT vGastoAgenciaThcDestino,
      OUTPUT vGastoAgenciaInbSur,
      OUTPUT vGastoAgenciaDcr,
      OUTPUT vGastoAgenciaAgp,
      OUTPUT vGastoAgenciaT7,
      OUTPUT vGastoAgenciaEbaf,
      OUTPUT vGastoAgenciaAms,
      OUTPUT vGastoAgencia).

  FIND FIRST agencias     WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
  FIND FIRST tipos_plazo  OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST vapores      OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST destinos     OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST despachantes OF orden_entrega NO-LOCK NO-ERROR.
  
  CREATE tt-oe.
  ASSIGN 
    tt-oe.id_orden_entrega              = orden_entrega.id_orden_entrega
    tt-oe.anio_oe                       = YEAR(orden_entrega.fecha_embarque)
    tt-oe.semana                        = orden_entrega.semana_embarque
    tt-oe.id_despachante                = orden_entrega.id_despachante
    tt-oe.despachante                   = IF AVAILABLE despachantes THEN despachantes.descripcion ELSE "NONE"
    tt-oe.id_vapor                      = orden_entrega.id_vapor
    tt-oe.vapor                         = IF AVAILABLE vapores THEN vapores.abreviatura ELSE "NONE"
    tt-oe.fecha_salida                  = orden_entrega.fecha_embarque
    tt-oe.id_agencia                    = orden_entrega.id_agencia
    tt-oe.agencia                       = IF AVAILABLE agencias THEN agencias.abreviatura ELSE "NONE"
    tt-oe.id_destino                    = orden_entrega.id_destino
    tt-oe.destino                       = IF AVAILABLE destinos THEN destinos.abreviatura ELSE "NONE"
    tt-oe.total_factura                 = vTotalFactura
    tt-oe.cantidad_tambores             = vTambores
    tt-oe.importe_comisiones            = vComision
    tt-oe.fob_ton                       = v_Fob
    tt-oe.plazo                         = orden_entrega.plazo

    tt-oe.GastoClausulaEntry            = vGastoClausulaEntry
    tt-oe.GastoClausulaFlete            = vGastoClausulaFlete 
    tt-oe.GastoClausulaSeguro           = vGastoClausulaSeguro 
    tt-oe.GastoClausulaGenerico         = vGastoClausulaGenerico 
    tt-oe.GastoClausulaBunker           = vGastoClausulaBunker 
    tt-oe.GastoClausulaThcOrigen        = vGastoClausulaThcOrigen 
    tt-oe.GastoClausulaThcDestino       = vGastoClausulaThcDestino 
    tt-oe.GastoClausulaInbSur           = vGastoClausulaInbSur 
    tt-oe.GastoClausulaDcr              = vGastoClausulaDcr 
    tt-oe.GastoClausulaEbaf             = vGastoClausulaEbaf 
    tt-oe.GastoAgenciaEntry             = vGastoAgenciaEntry 
    tt-oe.GastoAgenciaFlete             = vGastoAgenciaFlete
    tt-oe.GastoAgenciaThc               = vGastoAgenciaThc 
    tt-oe.GastoAgenciaSeguro            = vGastoAgenciaSeguro 
    tt-oe.GastoAgenciaVarios            = vGastoAgenciaVarios 
    tt-oe.GastoAgenciaToll              = vGastoAgenciaToll 
    tt-oe.GastoAgenciaHandling          = vGastoAgenciaHandling 
    tt-oe.GastoAgenciaInLand            = vGastoAgenciaInLand 
    tt-oe.GastoAgenciaBunker            = vGastoAgenciaBunker 
    tt-oe.GastoAgenciaBL                = vGastoAgenciaBL 
    tt-oe.GastoAgenciaThcDestino        = vGastoAgenciaThcDestino 
    tt-oe.GastoAgenciaInbSur            = vGastoAgenciaInbSur 
    tt-oe.GastoAgenciaDcr               = vGastoAgenciaDcr 
    tt-oe.GastoAgenciaAgp               = vGastoAgenciaAgp 
    tt-oe.GastoAgenciaT7                = vGastoAgenciaT7 
    tt-oe.GastoAgenciaEbaf              = vGastoAgenciaEbaf 
    tt-oe.GastoAgenciaAms               = vGastoAgenciaAms 
    .


END. /*for each*/


vRes =  "Total Factura: "  + STRING(vTotalFactura)            + CHR(13) +
        "Kilos: "          + STRING(vKilos)                   + CHR(13) +
        "Tambores: "       + STRING(vTambores)                + CHR(13) +
        "Comision: "       + STRING(vComision)                + CHR(13) +
        "Cl Entry: "       + STRING(vGastoClausulaEntry)      + CHR(13) +
        "Cl Flete: "       + STRING(vGastoClausulaFlete)      + CHR(13) +
        "Cl Seguro: "      + STRING(vGastoClausulaSeguro)     + CHR(13) +
        "Cl Generico: "    + STRING(vGastoClausulaGenerico)   + CHR(13) +
        "Cl Bunker: "      + STRING(vGastoClausulaBunker)     + CHR(13) +
        "Cl THC Origen: "  + STRING(vGastoClausulaThcOrigen)  + CHR(13) +
        "Cl THC Destino: " + STRING(vGastoClausulaThcDestino) + CHR(13) +
        "Cl Inbalance: "   + STRING(vGastoClausulaInbSur)     + CHR(13) +
        "Cl DCR: "         + STRING(vGastoClausulaDcr)        + CHR(13) +
        "Cl EBAF: "        + STRING(vGastoClausulaEbaf)       + CHR(13) +
        "Ag Entry: "       + STRING(vGastoAgenciaEntry)       + CHR(13) +
        "Ag Flete: "       + STRING(vGastoAgenciaFlete)       + CHR(13) +
        "Ag Thc: "         + STRING(vGastoAgenciaThc)         + CHR(13) +
        "Ag Seguro: "      + STRING(vGastoAgenciaSeguro)      + CHR(13) +
        "Ag Varios: "      + STRING(vGastoAgenciaVarios)      + CHR(13) +
        "Ag Toll: "        + STRING(vGastoAgenciaToll)        + CHR(13) +
        "Ag Handling: "    + STRING(vGastoAgenciaHandling)    + CHR(13) +
        "Ag InLand: "      + STRING(vGastoAgenciaInLand)      + CHR(13) +
        "Ag Bunker: "      + STRING(vGastoAgenciaBunker)      + CHR(13) +
        "Ag BL: "          + STRING(vGastoAgenciaBL)          + CHR(13) +
        "Ag Thc Destino: " + STRING(vGastoAgenciaThcDestino)  + CHR(13) +
        "Ag Inb Sur: "     + STRING(vGastoAgenciaInbSur)      + CHR(13) +
        "Ag Dcr: "         + STRING(vGastoAgenciaDcr)         + CHR(13) +
        "Ag Agp: "         + STRING(vGastoAgenciaAgp)         + CHR(13) +
        "Ag T7: "          + STRING(vGastoAgenciaT7)          + CHR(13) +
        "Ag Ebaf: "        + STRING(vGastoAgenciaEbaf)        + CHR(13) +
        "Ag Ams: "         + STRING(vGastoAgenciaAms)         + CHR(13) +
        "Gastos Agencia "  + chr(13) + vGastoAgencia.
/*MESSAGE vRes VIEW-AS ALERT-BOX.*/

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTT-2 wWin 
PROCEDURE creaTT-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR vTotalFactura            LIKE orden_entrega.total_factura    NO-UNDO.
DEFINE VAR v_Fob                    AS DECIMAL DECIMALS 4               NO-UNDO.
DEFINE VAR vKilos                   AS INTEGER                          NO-UNDO.
DEFINE VAR v_id_cliente AS CHARACTER.
DEFINE VAR v_razon_social LIKE clientes.razon_social.
DEFINE VAR v_productos AS CHARACTER.
DEFINE VAR v_facturas AS CHARACTER.
DEFINE VAR v_nro_comp LIKE subd_vtas.nro_comp.
DEFINE VAR v_id_clausula AS CHARACTER.
DEFINE VAR v_clausula LIKE clausula.descripcion.
DEFINE VAR v_fecha_salida AS DATE.
DEFINE VAR v_fecha_arribo AS DATE.
DEFINE VAR v_Tambores AS INTEGER  NO-UNDO.
DEFINE VAR v_contenedores AS INTEGER NO-UNDO.
DEFINE VAR v_Comision     AS DECIMAL NO-UNDO.                          
DEFINE VAR v_service_free AS DECIMAL NO-UNDO.  
DEFINE VAR v_gastos_maritima AS DECIMAL NO-UNDO.  
DEFINE VAR v_duties AS DECIMAL NO-UNDO.  
DEFINE VAR v_entry_free AS DECIMAL NO-UNDO.  
DEFINE VAR v_delivery AS DECIMAL NO-UNDO.  
DEFINE VAR v_fca AS DECIMAL NO-UNDO.  
DEFINE VAR v_warehouse AS DECIMAL NO-UNDO.  
DEFINE VAR v_palletizing AS DECIMAL NO-UNDO. 
DEFINE VAR v_vto_factura AS DATE NO-UNDO.
DEFINE VAR v_derechos_exp AS DECIMAL NO-UNDO. 


FOR EACH tt-oe-2.
    DELETE tt-oe-2.
END.

FOR EACH orden_entrega WHERE orden_entrega.fecha >= vfechadesde
                         AND orden_entrega.fecha <= vfechahasta
                         AND orden_entrega.id_orden_entrega > 1000 AND
                         orden_entrega.id_tipo_orden_entrega = 1 /* Industria */
                         NO-LOCK BY semana_embarque:


  FIND FIRST agencias     WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
  FIND FIRST tipos_plazo  OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST vapores      OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST destinos     OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST despachantes OF orden_entrega NO-LOCK NO-ERROR.
  FIND FIRST destinos_grupo OF destinos NO-LOCK NO-ERROR.
  FIND FIRST zonas_clientes OF destinos NO-LOCK NO-ERROR.
    
  v_fecha_salida = ?.
  v_fecha_arribo = ?.
  FIND LAST packing_list WHERE packing_list.id_orden_entrega = orden_entrega.id_orden NO-LOCK NO-ERROR.
  IF AVAILABLE packing_list THEN
     DO:
      v_fecha_salida = packing_list.fecha_salida_vapor.
      v_fecha_arribo = packing_list.fecha_salida_vapor + dias_transito.
     END.
    ELSE
    DO:
      v_fecha_salida = orden_entrega.fecha_embarque.
      v_fecha_arribo = orden_entrega.fecha_arribo.
    END.

   /* Total de tambores y contenedores */
    v_tambores = 0.
    v_contenedores = 0.
    v_comision = 0.
    v_derechos_exp = 0.
    FOR EACH items_orden_entrega  OF orden_entrega NO-LOCK:
         v_comision = v_comision + items_orden_entrega.importe_comisiones.
         v_tambores = v_tambores + items_orden_entrega.cantidad_tambores.
         v_contenedores = v_contenedores + items_orden_entrega.contenedores.
         v_derechos_exp = v_derechos_exp +
             general.items_orden_entrega.importe_derechos_exportacion.
    END.

  v_id_cliente = "".
  v_razon_social = "".
  FOR EACH items_orden_entrega  OF orden_entrega where
      items_orden_entrega.id_cliente <> 0 NO-LOCK BREAK BY items_orden_entrega.id_cliente:
      IF LAST-OF(items_orden_entrega.id_cliente) THEN
      DO:
              FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
              IF AVAILABLE clientes THEN
              DO:
                  IF v_id_cliente <> "" THEN v_id_cliente = v_id_cliente + ";".
                  IF v_razon_social <> "" THEN v_razon_social = v_razon_social + ";".
                  v_id_cliente = v_id_cliente + string(clientes.id_cliente).
                  v_razon_social = v_razon_social + clientes.razon_social.
              END.
      END.
  END.
  
  v_id_clausula = "".
  v_clausula = "".
  FOR EACH items_orden_entrega  OF orden_entrega where
        items_orden_entrega.id_condicion_venta <> 0 NO-LOCK BREAK BY items_orden_entrega.id_condicion_venta:
        IF LAST-OF(items_orden_entrega.id_condicion_venta) THEN
        DO:
            FIND FIRST clausulas WHERE clausulas.id_clausula = items_orden_entrega.id_condicion_venta NO-LOCK NO-ERROR.
            IF AVAILABLE clausulas THEN
                DO:
                    IF v_id_clausula <> "" THEN v_id_clausula = v_id_clausula + ";".
                    IF v_clausula <> "" THEN v_clausula = v_clausula + ";".
                    v_id_clausula = v_id_clausula + string(clausulas.id_clausula).
                    v_clausula = v_clausula + clausulas.descripcion.
                END.
        END.
    END.


   v_productos = "".
    FOR EACH items_orden_entrega  OF orden_entrega 
        WHERE items_orden_entrega.id_articulo <> 0 NO-LOCK BREAK BY items_orden_entrega.id_articulo:
        IF LAST-OF(items_orden_entrega.id_articulo) THEN
        DO:
                FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
                IF AVAILABLE productos_terminados THEN
                DO:
                    IF v_productos <> "" THEN v_productos = v_productos + ";".
                    v_productos = v_productos + productos_terminados.abreviatura.
                END.
        END.
    END.

 v_facturas = "".
 v_vto_factura = ?.
 FOR EACH aux_subd_ventas WHERE aux_subd_ventas.id_punto_venta = 81 AND
     aux_subd_ventas.nro_leg_exp = orden_entrega.id_orden_entrega NO-LOCK,
     FIRST subd_vtas OF aux_subd_ventas WHERE subd_vtas.estado
     NO-LOCK BREAK BY subd_vtas.nromov:
     IF LAST-OF(subd_vtas.nromov) THEN
     DO:
         IF subd_vtas.nro_proforma <> 0 THEN v_nro_comp =  subd_vtas.nro_proforma.
                                        ELSE v_nro_comp = subd_vtas.nro_comp. 
         IF v_facturas <> "" THEN v_facturas = v_facturas + ";".
         v_facturas = v_facturas + STRING(v_nro_comp).
         v_vto_factura = subd_vtas.vencimiento.
     END.
 END.

 /* Acumulo Gastos de las partes */
 v_service_free = 0. 
 v_gastos_maritima = 0.
 v_duties = 0. 
 v_delivery = 0.
 v_service_free = 0. 
 v_fca = 0.
 v_warehouse = 0. 
 v_palletizing = 0.
 
 FOR EACH items_orden_entrega  OF orden_entrega NO-LOCK:
     FOR EACH gastos_items_orden_entrega OF items_orden_entrega NO-LOCK:
           CASE gastos_items_orden_entrega.id_gasto:
               WHEN 35 THEN
                   v_service_free = v_service_free + gastos_items_orden_entrega.importe.
               WHEN 5 OR WHEN 9 OR WHEN 12 OR WHEN 14 OR
                    WHEN 15 OR WHEN 16 OR WHEN 17 OR WHEN 19 OR
                    WHEN 20 OR WHEN 26 OR WHEN 31 OR WHEN 32   THEN
                   v_gastos_maritima = v_gastos_maritima + gastos_items_orden_entrega.importe.
               WHEN 3 THEN
                   v_duties = v_duties + gastos_items_orden_entrega.importe.
               WHEN 6 THEN
                   v_delivery = v_delivery + gastos_items_orden_entrega.importe.
               WHEN 22 THEN
                   v_fca = v_fca + gastos_items_orden_entrega.importe.
               WHEN 28 THEN
                   v_warehouse = v_warehouse + gastos_items_orden_entrega.importe.
               WHEN 24 THEN
                   v_palletizing = v_palletizing + gastos_items_orden_entrega.importe.

           END CASE.
     END.
 END.

   IF v_clausula MATCHES ("*DDP*") or
      v_clausula MATCHES ("*Consig*") THEN v_entry_free = 200.
                                       ELSE v_entry_free = 0.
 
  CREATE tt-oe-2.
  ASSIGN 
    tt-oe-2.id_orden_entrega              = orden_entrega.id_orden_entrega
    tt-oe-2.anio_oe                       = YEAR(orden_entrega.fecha_embarque)
    tt-oe-2.semana                        = orden_entrega.semana_embarque
    tt-oe-2.factura                      = v_facturas
    tt-oe-2.id_cliente                    = v_id_cliente
    tt-oe-2.razon_social                  = v_razon_social
    tt-oe-2.producto                     = v_productos
    tt-oe-2.id_vapor                      = orden_entrega.id_vapor
    tt-oe-2.vapor                         = IF AVAILABLE vapores THEN vapores.abreviatura ELSE "NONE"
    tt-oe-2.fecha_salida                  = v_fecha_salida
    tt-oe-2.fecha_arribo                  = v_fecha_arribo
    tt-oe-2.id_agencia                    = orden_entrega.id_agencia
    tt-oe-2.agencia                       = IF AVAILABLE agencias THEN agencias.abreviatura ELSE "NONE"
    tt-oe-2.id_destino                    = orden_entrega.id_destino
    tt-oe-2.destino                       = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE"
    tt-oe-2.pais_destino                  = IF AVAILABLE destinos_grupo THEN destinos_grupo.descripcion ELSE "NONE"
    tt-oe-2.id_clausula                   = v_id_clausula
    tt-oe-2.clausula                      = IF AVAILABLE clausulas THEN clausulas.descripcion ELSE "NONE"
    tt-oe-2.cant_tambores                = v_tambores
    tt-oe-2.cant_contenedores            = v_contenedores
    tt-oe-2.tot_comision                 = v_comision
    tt-oe-2.tot_service_free             = v_service_free
    tt-oe-2.vto_factura                  = v_vto_factura
    tt-oe-2.tot_gastos_maritima          = v_gastos_maritima
    tt-oe-2.vto_gastos_maritima          = (IF v_gastos_maritima > 0 THEN v_fecha_salida + 7  ELSE ?)
    tt-oe-2.tot_derechos_exp             = v_derechos_exp
    tt-oe-2.vto_derechos_exp             = (IF v_derechos_exp > 0 THEN v_fecha_salida + 15  ELSE ?)
    tt-oe-2.id_despachante               = orden_entrega.id_despachante
    tt-oe-2.despachante                  = IF AVAILABLE despachantes THEN despachantes.descripcion ELSE "NONE"
    tt-oe-2.tot_gastos_puerto = (v_contenedores * 160)
    tt-oe-2.vto_gastos_puerto = (IF tot_gastos_puerto > 0 THEN v_fecha_salida ELSE ?)
    tt-oe-2.tot_duties          = v_duties
    tt-oe-2.vto_duties          = ( IF v_duties > 0 THEN
                                       (IF AVAILABLE zonas_clientes THEN 
                                           IF zonas_clientes.descripcion MATCHES ("*usa*") 
                                              THEN v_fecha_arribo + 10
                                              ELSE IF zonas_clientes.descripcion MATCHES ("*euro*") 
                                                     THEN v_fecha_arribo + 45
                                                     ELSE ?        
                                          ELSE ?) 
                                    ELSE ?)
    tt-oe-2.tot_entry_free          = v_entry_free
    tt-oe-2.vto_entry_free          = ( IF v_entry_free > 0 THEN
                                           (IF AVAILABLE zonas_clientes THEN 
                                               IF zonas_clientes.descripcion MATCHES ("*usa*") 
                                                  THEN v_fecha_arribo + 10
                                                  ELSE IF zonas_clientes.descripcion MATCHES ("*euro*") 
                                                         THEN v_fecha_arribo + 45
                                                         ELSE ?        
                                              ELSE ?) 
                                        ELSE ?)
    tt-oe-2.tot_delivery          = v_delivery
    tt-oe-2.vto_delivery = (IF v_delivery > 0 THEN v_fecha_arribo + 10 ELSE ?)
    tt-oe-2.tot_fca          = v_fca
    tt-oe-2.vto_fca = (IF v_fca > 0 THEN v_fecha_arribo + 10 ELSE ?)
    tt-oe-2.tot_warehouse          = v_warehouse
    tt-oe-2.vto_warehouse = (IF v_warehouse > 0 THEN v_fecha_arribo + 15 ELSE ?)
    tt-oe-2.tot_palletizing          = v_palletizing
    tt-oe-2.vto_palletizing = (IF v_palletizing > 0 THEN v_fecha_arribo + 45 ELSE ?)
        .

     


END. /*for each*/



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
  DISPLAY fi-OE fi-desde fi-hasta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE btnOE fi-OE fi-desde fi-hasta BUTTON-1 BUTTON-2 BUTTON-3 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCobranza wWin 
PROCEDURE getCobranza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCobranzaCuenta wWin 
PROCEDURE getCobranzaCuenta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getGastos wWin 
PROCEDURE getGastos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLiq wWin 
PROCEDURE getLiq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQuery wWin 
PROCEDURE getQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER queryText AS CHARACTER NO-UNDO.
    queryText = queryText1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSort wWin 
PROCEDURE getSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER querySort AS CHARACTER NO-UNDO.
    querySort = querySort1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  fi-desde:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "01/01/" + STRING(YEAR(TODAY),"9999").
  fi-hasta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSort wWin 
PROCEDURE setSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xSort AS CHARACTER NO-UNDO.

querySort1 = xSort.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION queryName wWin 
FUNCTION queryName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

