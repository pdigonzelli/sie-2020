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

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE hApp AS HANDLE     NO-UNDO.
DEFINE VARIABLE hAsy AS HANDLE     NO-UNDO.
DEFINE VARIABLE hPro AS HANDLE     NO-UNDO.
DEFINE VARIABLE lFlg AS LOGICAL    NO-UNDO.


DEFINE VAR queryText1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR querysort1   AS CHARACTER INITIAL " "NO-UNDO.
DEFINE VAR qh           AS HANDLE NO-UNDO.

DEFINE VAR iOldPage     AS INTEGER INITIAL 0.

DEFINE TEMP-TABLE tt-reproceso 
    RCODE-INFORMATION
    FIELD id_lote                   AS INTEGER  COLUMN-LABEL "Lote"
    FIELD anio                      AS INTEGER  COLUMN-LABEL "Anio"
    FIELD id_sucursal               AS INTEGER  COLUMN-LABEL "Suc"
    FIELD id_tipotambor             AS INTEGER  COLUMN-LABEL "Ttambor"
    FIELD id_articulo               AS INTEGER  COLUMN-LABEL "Art."
    FIELD articulo                  AS CHAR     COLUMN-LABEL "Articulo"
    FIELD id_calidad                LIKE vapores.id_vapor COLUMN-LABEL "Cal"
    FIELD calidad                   AS CHARACT  COLUMN-LABEL "Calidad"
    FIELD fecha                     AS DATE     COLUMN-LABEL "Fecha"
    FIELD cantidad_tambores         AS INTEGER  COLUMN-LABEL "Tbs"
    FIELD kilos                     AS DECIMAL  COLUMN-LABEL "Kgs"
    FIELD kilos_400                 AS DECIMAL  COLUMN-LABEL "Kgs 400"
    
    FIELD id_lote_destino           AS INTEGER     COLUMN-LABEL "Lote Des"
    FIELD anio_destino              AS INTEGER  COLUMN-LABEL "Anio"
    FIELD id_sucursal_destino       AS INTEGER  COLUMN-LABEL "Suc"
    FIELD id_tipotambor_destino     AS INTEGER  COLUMN-LABEL "Ttambor"
    FIELD id_articulo_destino       AS INTEGER  COLUMN-LABEL "Art."
    FIELD articulo_destino          AS CHAR     COLUMN-LABEL "Articulo"
    FIELD id_calidad_destino        LIKE vapores.id_vapor COLUMN-LABEL "Cal"
    FIELD calidad_destino           AS CHARACT  COLUMN-LABEL "Calidad"
    FIELD fecha_destino             AS DATE     COLUMN-LABEL "Fecha"
    FIELD kilos_destino             AS DECIMAL  COLUMN-LABEL "Kgs"
    FIELD kilos_sobrante            AS DECIMAL  COLUMN-LABEL "Kgs Sob"
    FIELD merma                     AS DECIMAL  COLUMN-LABEL "Merma"
    FIELD porcentaje                AS INTEGER  COLUMN-LABEL "%".
    

DEFINE VARIABLE vfechadesde AS DATE       NO-UNDO.
DEFINE VARIABLE vfechahasta AS DATE       NO-UNDO.
DEFINE VARIABLE vcliente    AS INTEGER    NO-UNDO.



DEFINE TEMP-TABLE ttReproceso 
    RCODE-INFORMATION
    FIELD id_lote                   AS INTEGER  COLUMN-LABEL "Lote"
    FIELD anio                      AS INTEGER  COLUMN-LABEL "Anio"
    FIELD tambores                  AS INTEGER  COLUMN-LABEL "Tambores"
    FIELD kilos                     AS DECIMAL  COLUMN-LABEL "Kgs"
    FIELD kilos_400                 AS DECIMAL  COLUMN-LABEL "Kgs 400"
    FIELD articulo                  AS CHAR     COLUMN-LABEL "Articulo"
    FIELD calidad                   AS CHARACT  COLUMN-LABEL "Calidad"
    FIELD envase                    AS CHARACT  COLUMN-LABEL "Envase"
    FIELD fecha                     AS DATE     COLUMN-LABEL "Fecha"
    FIELD condicion                 AS CHARACT  COLUMN-LABEL "Condicion"    
    FIELD id_sucursal               AS INTEGER  COLUMN-LABEL "Suc"
    FIELD tipotambor                AS CHARACT  COLUMN-LABEL "Tipo Tambor"       
    FIELD id_lote_origen            AS INTEGER  COLUMN-LABEL "Lote Ori"
    FIELD anio_origen               AS INTEGER  COLUMN-LABEL "Anio Ori"
    FIELD tambores_origen           AS INTEGER  COLUMN-LABEL "Tambores Ori"
    FIELD kilos_origen              AS DECIMAL  COLUMN-LABEL "Kgs Ori"
    FIELD kilos_400_origen          AS DECIMAL  COLUMN-LABEL "Kgs 400 Ori"    
    FIELD articulo_origen           AS CHAR     COLUMN-LABEL "Articulo Ori"
    FIELD calidad_origen            AS CHARACT  COLUMN-LABEL "Calidad Ori"  
    FIELD envase_origen             AS CHARACT  COLUMN-LABEL "Envase Ori"
    FIELD fecha_origen              AS DATE     COLUMN-LABEL "Fecha Ori"  
    FIELD tipotambor_origen         AS CHARACT  COLUMN-LABEL "Tipo Tambor Ori"    
    FIELD nromov                    AS INTEGER  COLUMN-LABEL "Nromov"
    FIELD nromov_origen             AS INTEGER  COLUMN-LABEL "nromov_origen".

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
&Scoped-Define ENABLED-OBJECTS chkCargas optTipoTambor fi-desde fi-hasta ~
fiSucursal BUTTON-46 BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-47 BUTTON-48 
&Scoped-Define DISPLAYED-OBJECTS chkCargas optTipoTambor fi-desde fi-hasta ~
fiSucursal 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD totalKilosDestino wWin 
FUNCTION totalKilosDestino RETURNS DECIMAL
  (INPUT vEmpresa AS INTEGER,
   INPUT vSucursal AS INTEGER,
   INPUT vTipotambor AS INTEGER,
   INPUT vNromov AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD totalKilosDestinoSobrante wWin 
FUNCTION totalKilosDestinoSobrante RETURNS DECIMAL
  (INPUT vEmpresa AS INTEGER,
   INPUT vSucursal AS INTEGER,
   INPUT vTipotambor AS INTEGER,
   INPUT vNromov AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD totalKilosOrigen wWin 
FUNCTION totalKilosOrigen RETURNS DECIMAL
  (INPUT vEmpresa AS INTEGER,
   INPUT vSucursal AS INTEGER,
   INPUT vTipotambor AS INTEGER,
   INPUT vNromov AS INTEGER,
   INPUT vTipotamborOrigen AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-fMain 
       MENU-ITEM m_Query_Constructor LABEL "Query Constructor".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "&Exportar Todos  Prod" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "&Cancelar" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "&Exportar Anterior" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-46 
     LABEL "Exportar c/fecha p/roque" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-47 
     LABEL "Exportar" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-48 
     LABEL "Ajustes Post Cierre" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE fi-desde AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-hasta AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiSucursal AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE optTipoTambor AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Produccion Jugo", 1,
"Lotes Jugo", 3,
"Sobrante Jugo", 4,
"Arrastre Jugo", 5,
"Prod. Terceros", 9,
"Todos", 0
     SIZE 25 BY 7.14 NO-UNDO.

DEFINE VARIABLE chkCargas AS LOGICAL INITIAL no 
     LABEL "Incluye Cargas" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     chkCargas AT ROW 10.76 COL 2
     optTipoTambor AT ROW 3.14 COL 2 NO-LABEL
     fi-desde AT ROW 3.38 COL 42 COLON-ALIGNED
     fi-hasta AT ROW 4.57 COL 42 COLON-ALIGNED
     fiSucursal AT ROW 5.76 COL 42 COLON-ALIGNED
     BUTTON-46 AT ROW 9.33 COL 44
     BUTTON-1 AT ROW 4.81 COL 70
     BUTTON-2 AT ROW 6.24 COL 70
     BUTTON-3 AT ROW 7.91 COL 44
     BUTTON-47 AT ROW 3.38 COL 70
     BUTTON-48 AT ROW 7.67 COL 70
     "  Filtro" VIEW-AS TEXT
          SIZE 59 BY .71 AT ROW 1.95 COL 2
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93.6 BY 11.19.


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
         TITLE              = "Exportacion a Excell Reprocesos"
         HEIGHT             = 11.19
         WIDTH              = 93.6
         MAX-HEIGHT         = 33.05
         MAX-WIDTH          = 203.2
         VIRTUAL-HEIGHT     = 33.05
         VIRTUAL-WIDTH      = 203.2
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
/* SETTINGS FOR FRAME fMain
   Custom                                                               */
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

ASSIGN 
       BUTTON-1:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       BUTTON-3:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       BUTTON-46:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Exportacion a Excell Reprocesos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON U1 OF wWin /* Exportacion a Excell Reprocesos */
DO:
  WAIT-FOR F10 OF SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Exportacion a Excell Reprocesos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */

  IF VALID-HANDLE(hApp) THEN DO:      
    hApp:DISCONNECT().
    DELETE OBJECT hApp.
    DELETE OBJECT hAsy.
  END.

  RUN beforeExit.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Exportar Todos  Prod */
DO:
   vfechadesde = DATE(fi-desde:SCREEN-VALUE).
   vfechahasta = DATE(fi-hasta:SCREEN-VALUE).
   
   RUN creaTT2.
   RUN generateExcel.p (INPUT TABLE TT-reproceso,
                        INPUT " Exportacion de Reprocesos de todos los productos",
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
  RUN beforeExit.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME fMain /* Exportar Anterior */
DO:
   vfechadesde = DATE(fi-desde:SCREEN-VALUE).
   vfechahasta = DATE(fi-hasta:SCREEN-VALUE).
   
   RUN creaTT.
   RUN generateExcel.p (INPUT TABLE TT-reproceso,
                        INPUT " Exportacion de Reprocesos",
                        INPUT " Fecha Desde: " + fi-desde:SCREEN-VALUE + " - " + "Fecha Hasta: " + fi-hasta:SCREEN-VALUE  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Century Gothic",
                        INPUT 7).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-46
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-46 wWin
ON CHOOSE OF BUTTON-46 IN FRAME fMain /* Exportar c/fecha p/roque */
DO:
  SELF:SENSITIVE = NOT SELF:SENSITIVE.

  PROCESS EVENTS.

  IF VALID-HANDLE(hApp) THEN DO:    
    RUN callReporteReprocesos IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (DATE(fi-Desde:SCREEN-VALUE), 
                                                                                             DATE(fi-Hasta:SCREEN-VALUE), 
                                                                                             INTEGER(fiSucursal:SCREEN-VALUE), 
                                                                                             INTEGER(optTipoTambor:SCREEN-VALUE), 
                                                                                             chkCargas:SCREEN-VALUE).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callReporteReprocesos IN hLib (DATE(fi-Desde:SCREEN-VALUE), 
                                       DATE(fi-Hasta:SCREEN-VALUE), 
                                       INTEGER(fiSucursal:SCREEN-VALUE), 
                                       INTEGER(optTipoTambor:SCREEN-VALUE), 
                                       chkCargas:SCREEN-VALUE).
  RUN exportExcelReproc IN hPro.
  
  /*
  RUN generateExcel.p (INPUT TABLE compo_lote_jugo,
                        INPUT " Exportacion de Reprocesos",
                        INPUT " Fecha Desde: " + fi-desde:SCREEN-VALUE + " - " + "Fecha Hasta: " + fi-hasta:SCREEN-VALUE  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Century Gothic",
                        INPUT 7).
                        */

  SELF:SENSITIVE = NOT SELF:SENSITIVE.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-47
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-47 wWin
ON CHOOSE OF BUTTON-47 IN FRAME fMain /* Exportar */
DO:
  DEFINE VARIABLE hLibRep AS HANDLE     NO-UNDO.

  RUN libReportes.p PERSISTENT SET hLibRep.

  SELF:SENSITIVE = FALSE.
  PROCESS EVENTS.

 

  IF VALID-HANDLE(hApp) THEN DO:
    RUN callReporteReprocesos IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                       (DATE(fi-Desde:SCREEN-VALUE), 
                                        DATE(fi-Hasta:SCREEN-VALUE), 
                                        INTEGER(fiSucursal:SCREEN-VALUE), 
                                        FALSE).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE DO:
    RUN callReporteReprocesos IN hLib (DATE(fi-Desde:SCREEN-VALUE), 
                                       DATE(fi-Hasta:SCREEN-VALUE), 
                                       INTEGER(fiSucursal:SCREEN-VALUE), 
                                       FALSE).

  END.

  SELF:SENSITIVE = TRUE.


  RUN exportExcelReprocesos IN hLibRep.

  IF LOGICAL(chkCargas:SCREEN-VALUE) THEN DO:
    /* reporte de ajustes post cierre */
  END.




END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-48
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-48 wWin
ON CHOOSE OF BUTTON-48 IN FRAME fMain /* Ajustes Post Cierre */
DO:
  DEFINE VARIABLE hLibRep AS HANDLE     NO-UNDO.

  RUN libReportes.p PERSISTENT SET hLibRep.

  SELF:SENSITIVE = FALSE.
  PROCESS EVENTS.

  IF VALID-HANDLE(hApp) THEN DO:
    RUN callReporteReprocesos IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                       (DATE(fi-Desde:SCREEN-VALUE), 
                                        DATE(fi-Hasta:SCREEN-VALUE), 
                                        INTEGER(fiSucursal:SCREEN-VALUE), 
                                        TRUE).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE DO:
    RUN callReporteReprocesos IN hLib (DATE(fi-Desde:SCREEN-VALUE), 
                                       DATE(fi-Hasta:SCREEN-VALUE), 
                                       INTEGER(fiSucursal:SCREEN-VALUE), 
                                       TRUE).

  END.

  SELF:SENSITIVE = TRUE.


  RUN exportExcelReprocesos IN hLibRep.

  


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeExit wWin 
PROCEDURE beforeExit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libApiMenu.p').
  DELETE OBJECT hLibCom.
  
  RUN cleanUpInfoInParent IN hLib.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callCompleted wWin 
PROCEDURE callCompleted :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  APPLY "F10" TO TARGET-PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectApp wWin 
PROCEDURE connectApp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  hApp = DYNAMIC-FUNCTION('connectApp' IN hLibCom).

  IF VALID-HANDLE(hApp) THEN 
    RUN libReportes.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
  ELSE  
    RUN libReportes.p PERSISTENT SET hLib .

  RUN libReportes.p PERSISTENT SET hPro .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTT wWin 
PROCEDURE creaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER tt_origen FOR tambores_industria.
DEFINE BUFFER tt_tam_carga FOR tambores_industria.
DEFINE VAR v_tambores AS INTEGER.
DEFINE VAR v_kilos AS DECIMAL.
DEFINE VAR v_kilos_400 AS DECIMAL.
DEFINE VAR vTotalLoteDestino AS DECIMAL.
DEFINE VAR vTotalOrigen AS DECIMAL.
DEFINE VAR vParcialOrigen AS DECIMAL.
DEFINE VAR vParcialLoteDestino AS DECIMAL.
DEFINE VAR vMerma AS DECIMAL.
DEFINE VAR vTotalSobrante AS DECIMAL.
DEFINE BUFFER tt_lotes FOR lotes_jugo.


FOR EACH tt-reproceso.
    DELETE tt-reproceso.
END.

FOR EACH lotes_jugo WHERE lotes_jugo.id_tipotambor = 3
                      AND lotes_jugo.fecha >= vfechadesde
                      AND lotes_jugo.fecha <= vfechahasta
                      NO-LOCK.
    
    vTotalLoteDestino = totalKilosDestino(lotes_jugo.id_empresa,
                                          lotes_jugo.id_sucursal,
                                          lotes_jugo.id_tipotambor,
                                          lotes_jugo.nromov).

    vTotalSobrante = totalKilosDestinoSobrante(lotes_jugo.id_empresa,
                                               lotes_jugo.id_sucursal,
                                               lotes_jugo.id_tipotambor,
                                               lotes_jugo.nromov).
    
    vTotalOrigen = totalKilosOrigen(lotes_jugo.id_empresa,
                                    lotes_jugo.id_sucursal,
                                    lotes_jugo.id_tipotambor,
                                    lotes_jugo.nromov,
                                    3).

    
    FOR EACH tt_origen WHERE tt_origen.id_empresa_destino    = lotes_jugo.id_empresa
                         AND tt_origen.id_sucursal_destino   = lotes_jugo.id_sucursal
                         AND tt_origen.id_tipotambor_destino = lotes_jugo.id_tipotambor
                         AND tt_origen.nromov_destino        = lotes_jugo.nromov
                         AND tt_origen.id_tipotambor         = 3
                        NO-LOCK
                        BREAK BY tt_origen.nromov.

        v_tambores = v_tambores + 1.
        v_kilos = v_kilos + tt_origen.kilos_tambor.

        IF LAST-OF(tt_origen.nromov) THEN DO:

            FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = tt_origen.id_articulo
                                             AND r_productos_calidad.id_calidad  = tt_origen.id_calidad
                                            NO-LOCK NO-ERROR.
            
            IF AVAILABLE r_productos_calidad THEN DO:
                v_kilos_400 = v_kilos * r_productos_calidad.coeficiente.
            END.

            FIND FIRST productos_terminados OF tt_origen NO-LOCK NO-ERROR.
            FIND FIRST calidades OF tt_origen NO-LOCK NO-ERROR.
    
            CREATE tt-reproceso.
            ASSIGN tt-reproceso.id_lote          = tt_origen.id_lote
                   tt-reproceso.anio             = tt_origen.anio
                   tt-reproceso.id_sucursal      = tt_origen.id_sucursal
                   tt-reproceso.id_tipotambor    = tt_origen.id_tipotambor
                   tt-reproceso.id_articulo      = tt_origen.id_articulo
                   tt-reproceso.id_calidad       = tt_origen.id_calidad
                   tt-reproceso.fecha            = tt_origen.fecha
                   tt-reproceso.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                   tt-reproceso.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                   tt-reproceso.cantidad_tambores = v_tambores
                   tt-reproceso.kilos             = v_kilos
                   tt-reproceso.kilos_400         = v_kilos_400.
            
            RELEASE productos_terminados.
            RELEASE calidades.
    
            FIND FIRST productos_terminados OF lotes_jugo NO-LOCK NO-ERROR.
            FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
            
            vParcialLoteDestino = (vTotalLoteDestino * v_kilos) / vTotalOrigen.
            vMerma              = vParcialLoteDestino - v_kilos.
            ASSIGN tt-reproceso.id_lote_destino          = lotes_jugo.id_lote
                   tt-reproceso.anio_destino             = lotes_jugo.anio
                   tt-reproceso.id_sucursal_destino      = lotes_jugo.id_sucursal
                   tt-reproceso.id_tipotambor_destino    = lotes_jugo.id_tipotambor
                   tt-reproceso.id_articulo_destino      = lotes_jugo.id_articulo
                   tt-reproceso.id_calidad_destino       = lotes_jugo.id_calidad
                   tt-reproceso.fecha_destino            = lotes_jugo.fecha
                   tt-reproceso.articulo_destino         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                   tt-reproceso.calidad_destino          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                   tt-reproceso.kilos_destino            = vParcialLoteDestino
                   tt-reproceso.kilos_sobrante           = vTotalSobrante
                   tt-reproceso.merma                    = vMerma
                   tt-reproceso.porcentaje               = INTEGER((vMerma * 100) / v_kilos).

            vParcialLoteDestino = 0.
            v_tambores = 0.
            v_kilos = 0.
            v_kilos_400 = 0.
        END.
    END.
END.


/* PRIMERO RECORRO LAS CARGAS */
FOR EACH cargas WHERE cargas.fecha >= vfechadesde
                  AND cargas.fecha <= vfechahasta
                  NO-LOCK.
    
    vTotalLoteDEstino = totalKilosDestino(cargas.id_empresa,
                                          cargas.id_sucursal,
                                          cargas.id_tipotambor,
                                          cargas.nromov).
    
    vTotalOrigen = totalKilosOrigen(cargas.id_empresa,
                                    cargas.id_sucursal,
                                    cargas.id_tipotambor,
                                    cargas.nromov,
                                    3).

    /* RECORRO LOS TAMBORES QUE SON ORIGEN DE CADA CARGA */
    FOR EACH tt_origen WHERE tt_origen.id_empresa_destino    = cargas.id_empresa
                         AND tt_origen.id_sucursal_destino   = cargas.id_sucursal
                         AND tt_origen.id_tipotambor_destino = cargas.id_tipotambor
                         AND tt_origen.nromov_destino        = cargas.nromov
                         AND tt_origen.id_tipotambor         = 3
                        NO-LOCK
                        BREAK BY tt_origen.nromov.

        v_tambores = v_tambores + 1.
        v_kilos = v_kilos + tt_origen.kilos_tambor.

        IF LAST-OF(tt_origen.nromov) THEN DO:

            FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = tt_origen.id_articulo
                                             AND r_productos_calidad.id_calidad  = tt_origen.id_calidad
                                            NO-LOCK NO-ERROR.
            
            IF AVAILABLE r_productos_calidad THEN DO:
                v_kilos_400 = v_kilos * r_productos_calidad.coeficiente.
            END.

            FIND FIRST productos_terminados OF tt_origen NO-LOCK NO-ERROR.
            FIND FIRST calidades OF tt_origen NO-LOCK NO-ERROR.
    
            CREATE tt-reproceso.
            ASSIGN tt-reproceso.id_lote          = tt_origen.id_lote
                   tt-reproceso.anio             = tt_origen.anio
                   tt-reproceso.id_sucursal      = tt_origen.id_sucursal
                   tt-reproceso.id_tipotambor    = tt_origen.id_tipotambor
                   tt-reproceso.id_articulo      = tt_origen.id_articulo
                   tt-reproceso.id_calidad       = tt_origen.id_calidad
                   tt-reproceso.fecha            = tt_origen.fecha
                   tt-reproceso.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                   tt-reproceso.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                   tt-reproceso.cantidad_tambores = v_tambores
                   tt-reproceso.kilos             = v_kilos
                   tt-reproceso.kilos_400         = v_kilos_400.
            
            RELEASE productos_terminados.
            RELEASE calidades.
            
            FIND FIRST productos_terminados OF cargas NO-LOCK NO-ERROR.
            FIND FIRST calidades OF cargas NO-LOCK NO-ERROR.
            
            vParcialLoteDestino = (vTotalLoteDestino * v_kilos) / vTotalOrigen.
            vMerma              = vParcialLoteDestino - v_kilos.
            ASSIGN tt-reproceso.id_lote_destino          = cargas.id_carga
                   tt-reproceso.anio_destino             = cargas.anio
                   tt-reproceso.id_sucursal_destino      = cargas.id_sucursal
                   tt-reproceso.id_tipotambor_destino    = cargas.id_tipotambor
                   tt-reproceso.id_articulo_destino      = cargas.id_articulo
                   tt-reproceso.id_calidad_destino       = cargas.id_calidad
                   tt-reproceso.fecha_destino            = cargas.fecha
                   tt-reproceso.articulo_destino         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                   tt-reproceso.calidad_destino          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                   tt-reproceso.kilos_destino            = vParcialLoteDestino
                   tt-reproceso.merma                    = vMerma
                   tt-reproceso.porcentaje               = INTEGER((vMerma * 100) / v_kilos).
            
            vParcialLoteDestino = 0.
            v_tambores = 0.
            v_kilos = 0.
            v_kilos_400 = 0.
            
        END.
    END.
END.


 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTT2 wWin 
PROCEDURE creaTT2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER tt_origen FOR tambores_industria.
DEFINE BUFFER tt_tam_carga FOR tambores_industria.
DEFINE VAR v_tambores AS INTEGER.
DEFINE VAR v_kilos AS DECIMAL.
DEFINE VAR v_kilos_400 AS DECIMAL.
DEFINE VAR vTotalLoteDestino AS DECIMAL.
DEFINE VAR vTotalOrigen AS DECIMAL.
DEFINE VAR vParcialOrigen AS DECIMAL.
DEFINE VAR vParcialLoteDestino AS DECIMAL.
DEFINE VAR vMerma AS DECIMAL.
DEFINE VAR vTotalSobrante AS DECIMAL.
DEFINE BUFFER tt_lotes FOR lotes_jugo.


FOR EACH tt-reproceso.
    DELETE tt-reproceso.
END.

FOR EACH lotes_jugo WHERE lotes_jugo.id_tipotambor = 3
                      AND lotes_jugo.fecha >= vfechadesde
                      AND lotes_jugo.fecha <= vfechahasta
                      NO-LOCK.
    
    vTotalLoteDEstino = totalKilosDestino(lotes_jugo.id_empresa,
                                          lotes_jugo.id_sucursal,
                                          lotes_jugo.id_tipotambor,
                                          lotes_jugo.nromov).
    
    vTotalSobrante = totalKilosDestinoSobrante(lotes_jugo.id_empresa,
                                               lotes_jugo.id_sucursal,
                                               lotes_jugo.id_tipotambor,
                                               lotes_jugo.nromov).

    vTotalOrigen = totalKilosOrigen(lotes_jugo.id_empresa,
                                    lotes_jugo.id_sucursal,
                                    lotes_jugo.id_tipotambor,
                                    lotes_jugo.nromov,
                                    0 /* TODOS */ ).

    FOR EACH tt_origen WHERE tt_origen.id_empresa_destino    = lotes_jugo.id_empresa
                         AND tt_origen.id_sucursal_destino   = lotes_jugo.id_sucursal
                         AND tt_origen.id_tipotambor_destino = lotes_jugo.id_tipotambor
                         AND tt_origen.nromov_destino        = lotes_jugo.nromov
                         NO-LOCK
                        BREAK BY tt_origen.nromov.

        v_tambores = v_tambores + 1.
        v_kilos = v_kilos + tt_origen.kilos_tambor.

        IF LAST-OF(tt_origen.nromov) THEN DO:

            FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = tt_origen.id_articulo
                                             AND r_productos_calidad.id_calidad  = tt_origen.id_calidad
                                            NO-LOCK NO-ERROR.
            
            IF AVAILABLE r_productos_calidad THEN DO:
                v_kilos_400 = v_kilos * r_productos_calidad.coeficiente.
            END.

            FIND FIRST productos_terminados OF tt_origen NO-LOCK NO-ERROR.
            FIND FIRST calidades OF tt_origen NO-LOCK NO-ERROR.
    
            CREATE tt-reproceso.
            ASSIGN tt-reproceso.id_lote          = tt_origen.id_lote
                   tt-reproceso.anio             = tt_origen.anio
                   tt-reproceso.id_sucursal      = tt_origen.id_sucursal
                   tt-reproceso.id_tipotambor    = tt_origen.id_tipotambor
                   tt-reproceso.id_articulo      = tt_origen.id_articulo
                   tt-reproceso.id_calidad       = tt_origen.id_calidad
                   tt-reproceso.fecha            = tt_origen.fecha
                   tt-reproceso.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                   tt-reproceso.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                   tt-reproceso.cantidad_tambores = v_tambores
                   tt-reproceso.kilos             = v_kilos
                   tt-reproceso.kilos_400         = v_kilos_400.
            
            RELEASE productos_terminados.
            RELEASE calidades.
    
            FIND FIRST productos_terminados OF lotes_jugo NO-LOCK NO-ERROR.
            FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
            
            vParcialLoteDestino = (vTotalLoteDestino * v_kilos) / vTotalOrigen.
            vMerma              = vParcialLoteDestino - v_kilos.

            ASSIGN tt-reproceso.id_lote_destino          = lotes_jugo.id_lote
                   tt-reproceso.anio_destino             = lotes_jugo.anio
                   tt-reproceso.id_sucursal_destino      = lotes_jugo.id_sucursal
                   tt-reproceso.id_tipotambor_destino    = lotes_jugo.id_tipotambor
                   tt-reproceso.id_articulo_destino      = lotes_jugo.id_articulo
                   tt-reproceso.id_calidad_destino       = lotes_jugo.id_calidad
                   tt-reproceso.fecha_destino            = lotes_jugo.fecha
                   tt-reproceso.articulo_destino         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                   tt-reproceso.calidad_destino          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                   tt-reproceso.kilos_destino            = vParcialLoteDestino
                   tt-reproceso.merma                    = vMerma
                   tt-reproceso.kilos_sobrante           = vTotalSobrante
                   tt-reproceso.porcentaje               = INTEGER((vMerma * 100) / v_kilos).

            vParcialLoteDestino = 0.
            v_tambores = 0.
            v_kilos = 0.
            v_kilos_400 = 0.
        END.
    END.
END.


/* PRIMERO RECORRO LAS CARGAS */
FOR EACH cargas WHERE cargas.fecha >= vfechadesde
                  AND cargas.fecha <= vfechahasta
                  NO-LOCK.
    
    vTotalLoteDEstino = totalKilosDestino(cargas.id_empresa,
                                          cargas.id_sucursal,
                                          cargas.id_tipotambor,
                                          cargas.nromov).
    
    vTotalOrigen = totalKilosOrigen(cargas.id_empresa,
                                    cargas.id_sucursal,
                                    cargas.id_tipotambor,
                                    cargas.nromov,
                                    0 /* TODOS */ ).

    /* RECORRO LOS TAMBORES QUE SON ORIGEN DE CADA CARGA */
    FOR EACH tt_origen WHERE tt_origen.id_empresa_destino    = cargas.id_empresa
                         AND tt_origen.id_sucursal_destino   = cargas.id_sucursal
                         AND tt_origen.id_tipotambor_destino = cargas.id_tipotambor
                         AND tt_origen.nromov_destino        = cargas.nromov
                         NO-LOCK
                        BREAK BY tt_origen.nromov.

        v_tambores = v_tambores + 1.
        v_kilos = v_kilos + tt_origen.kilos_tambor.

        IF LAST-OF(tt_origen.nromov) THEN DO:

            FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = tt_origen.id_articulo
                                             AND r_productos_calidad.id_calidad  = tt_origen.id_calidad
                                            NO-LOCK NO-ERROR.
            
            IF AVAILABLE r_productos_calidad THEN DO:
                v_kilos_400 = v_kilos * r_productos_calidad.coeficiente.
            END.

            FIND FIRST productos_terminados OF tt_origen NO-LOCK NO-ERROR.
            FIND FIRST calidades OF tt_origen NO-LOCK NO-ERROR.
    
            CREATE tt-reproceso.
            ASSIGN tt-reproceso.id_lote          = tt_origen.id_lote
                   tt-reproceso.anio             = tt_origen.anio
                   tt-reproceso.id_sucursal      = tt_origen.id_sucursal
                   tt-reproceso.id_tipotambor    = tt_origen.id_tipotambor
                   tt-reproceso.id_articulo      = tt_origen.id_articulo
                   tt-reproceso.id_calidad       = tt_origen.id_calidad
                   tt-reproceso.fecha            = tt_origen.fecha
                   tt-reproceso.articulo         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                   tt-reproceso.calidad          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                   tt-reproceso.cantidad_tambores = v_tambores
                   tt-reproceso.kilos             = v_kilos
                   tt-reproceso.kilos_400         = v_kilos_400.
            
            RELEASE productos_terminados.
            RELEASE calidades.
            
            FIND FIRST productos_terminados OF cargas NO-LOCK NO-ERROR.
            FIND FIRST calidades OF cargas NO-LOCK NO-ERROR.
            
            vParcialLoteDestino = (vTotalLoteDestino * v_kilos) / vTotalOrigen.
            vMerma              = vParcialLoteDestino - v_kilos.
            ASSIGN tt-reproceso.id_lote_destino          = cargas.id_carga
                   tt-reproceso.anio_destino             = cargas.anio
                   tt-reproceso.id_sucursal_destino      = cargas.id_sucursal
                   tt-reproceso.id_tipotambor_destino    = cargas.id_tipotambor
                   tt-reproceso.id_articulo_destino      = cargas.id_articulo
                   tt-reproceso.id_calidad_destino       = cargas.id_calidad
                   tt-reproceso.fecha_destino            = cargas.fecha
                   tt-reproceso.articulo_destino         = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
                   tt-reproceso.calidad_destino          = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE"
                   tt-reproceso.kilos_destino            = vParcialLoteDestino
                   tt-reproceso.merma                    = vMerma
                   tt-reproceso.porcentaje               = INTEGER((vMerma * 100) / v_kilos).
            
            vParcialLoteDestino = 0.
            v_tambores = 0.
            v_kilos = 0.
            v_kilos_400 = 0.
            
        END.
    END.
END.

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
  DISPLAY chkCargas optTipoTambor fi-desde fi-hasta fiSucursal 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE chkCargas optTipoTambor fi-desde fi-hasta fiSucursal BUTTON-46 
         BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-47 BUTTON-48 
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

  RUN connectApp.

  button-1:HIDDEN IN FRAME fMain = TRUE.
  button-3:HIDDEN IN FRAME fMain = TRUE.
  button-46:HIDDEN IN FRAME fMain = TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION totalKilosDestino wWin 
FUNCTION totalKilosDestino RETURNS DECIMAL
  (INPUT vEmpresa AS INTEGER,
   INPUT vSucursal AS INTEGER,
   INPUT vTipotambor AS INTEGER,
   INPUT vNromov AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE BUFFER tt_tam_destino FOR tambores_industria.
DEFINE BUFFER ttArrastre FOR arrastre_lote.
DEFINE BUFFER ttSobrante FOR sobrante.
DEFINE VAR vTotalLoteDestino AS DECIMAL.

vTotalLoteDestino = 0.
FOR EACH tt_tam_destino WHERE tt_tam_destino.id_empresa    = vEmpresa
                          AND tt_tam_destino.id_sucursal   = vSucursal
                          AND tt_tam_destino.id_tipotambor = vTipotambor
                          AND tt_tam_destino.nromov        = vNromov NO-LOCK.

    vTotalLoteDestino = vTotalLoteDestino + tt_tam_destino.kilos_tambor.
END.

FOR EACH ttArrastre WHERE ttArrastre.id_empresa     = vEmpresa
                      AND ttArrastre.id_sucursal    = vSucursal
                      AND ttArrastre.id_tipotambor  = vTipotambor
                      AND ttArrastre.nromov         = vNromov
                    NO-LOCK.
    FOR EACH tt_tam_destino WHERE tt_tam_destino.id_empresa     = vEmpresa
                              AND tt_tam_destino.id_sucursal    = vSucursal
                              AND tt_tam_destino.id_tipotambor  = ttArrastre.id_tipotambor_arrastre
                              AND tt_tam_destino.nromov         = ttArrastre.nromov_arrastre
                            NO-LOCK.
        vTotalLoteDestino = vTotalLoteDestino + tt_tam_destino.kilos_tambor.
    END.
END.
  
FOR EACH ttSobrante WHERE ttSobrante.id_empresa     = vEmpresa
                      AND ttSobrante.id_sucursal    = vSucursal
                      AND ttSobrante.id_tipotambor  = vTipotambor
                      AND ttSobrante.nromov         = vNromov
                    NO-LOCK.
    FOR EACH tt_tam_destino WHERE tt_tam_destino.id_empresa     = vEmpresa
                              AND tt_tam_destino.id_sucursal    = vSucursal
                              AND tt_tam_destino.id_tipotambor  = ttSobrante.id_tipotambor_sobrante
                              AND tt_tam_destino.nromov         = ttSobrante.nromov_sobrante
                            NO-LOCK.
        vTotalLoteDestino = vTotalLoteDestino + tt_tam_destino.kilos_tambor.
    END.
END.


RETURN vTotalLoteDestino.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION totalKilosDestinoSobrante wWin 
FUNCTION totalKilosDestinoSobrante RETURNS DECIMAL
  (INPUT vEmpresa AS INTEGER,
   INPUT vSucursal AS INTEGER,
   INPUT vTipotambor AS INTEGER,
   INPUT vNromov AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE BUFFER tt_tam_destino FOR tambores_industria.
DEFINE BUFFER ttArrastre FOR arrastre_lote.
DEFINE BUFFER ttSobrante FOR sobrante.
DEFINE VAR vTotalLoteDestino AS DECIMAL.

FOR EACH ttArrastre WHERE ttArrastre.id_empresa     = vEmpresa
                      AND ttArrastre.id_sucursal    = vSucursal
                      AND ttArrastre.id_tipotambor  = vTipotambor
                      AND ttArrastre.nromov         = vNromov
                    NO-LOCK.
    FOR EACH tt_tam_destino WHERE tt_tam_destino.id_empresa     = vEmpresa
                              AND tt_tam_destino.id_sucursal    = vSucursal
                              AND tt_tam_destino.id_tipotambor  = ttArrastre.id_tipotambor_arrastre
                              AND tt_tam_destino.nromov         = ttArrastre.nromov_arrastre
                            NO-LOCK.
        vTotalLoteDestino = vTotalLoteDestino + tt_tam_destino.kilos_tambor.
    END.
END.
  
FOR EACH ttSobrante WHERE ttSobrante.id_empresa     = vEmpresa
                      AND ttSobrante.id_sucursal    = vSucursal
                      AND ttSobrante.id_tipotambor  = vTipotambor
                      AND ttSobrante.nromov         = vNromov
                    NO-LOCK.
    FOR EACH tt_tam_destino WHERE tt_tam_destino.id_empresa     = vEmpresa
                              AND tt_tam_destino.id_sucursal    = vSucursal
                              AND tt_tam_destino.id_tipotambor  = ttSobrante.id_tipotambor_sobrante
                              AND tt_tam_destino.nromov         = ttSobrante.nromov_sobrante
                            NO-LOCK.
        vTotalLoteDestino = vTotalLoteDestino + tt_tam_destino.kilos_tambor.
    END.
END.

RETURN vTotalLoteDestino.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION totalKilosOrigen wWin 
FUNCTION totalKilosOrigen RETURNS DECIMAL
  (INPUT vEmpresa AS INTEGER,
   INPUT vSucursal AS INTEGER,
   INPUT vTipotambor AS INTEGER,
   INPUT vNromov AS INTEGER,
   INPUT vTipotamborOrigen AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE BUFFER ttOrigenTotal FOR tambores_industria.
DEFINE VAR vTotalOrigen AS DECIMAL.

    FOR EACH ttOrigenTotal WHERE ttOrigenTotal.id_empresa_destino    = vEmpresa
                             AND ttOrigenTotal.id_sucursal_destino   = vSucursal
                             AND ttOrigenTotal.id_tipotambor_destino = vTipotambor
                             AND ttOrigenTotal.nromov_destino        = vNromov
                             AND IF vTipotamborOrigen = 0 THEN TRUE ELSE ttOrigenTotal.id_tipotambor = vTipotamborOrigen
                            NO-LOCK.
        vTotalOrigen = vTotalOrigen + ttOrigenTotal.kilos_tambor.
    END.
  
    RETURN vTotalOrigen.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

