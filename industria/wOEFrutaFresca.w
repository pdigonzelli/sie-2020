&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS BUTTON-20 BUTTON-21 BUTTON-22 BUTTON-1 ~
BUTTON-23 totalPallet BUTTON-5 vpalletsElegidos BUTTON-2 BUTTON-19 BUTTON-3 
&Scoped-Define DISPLAYED-OBJECTS totalPallet vpalletsElegidos 

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


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bitemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemspedidospacking AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bpedidospacking AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemspedidospacking AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dordenentrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dpedidospacking AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynfilter-2 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Crear Parte de OE" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-19 
     LABEL "Imprimir" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "Quitar Pallets de la Parte de OE" 
     SIZE 31 BY 1.14.

DEFINE BUTTON BUTTON-20 
     LABEL "clientes Envases Precios" 
     SIZE 32 BY 1.14.

DEFINE BUTTON BUTTON-21 
     LABEL "Pallets y Contenedores" 
     SIZE 32 BY 1.14.

DEFINE BUTTON BUTTON-22 
     LABEL "clientes en Cons c/Flete" 
     SIZE 32 BY 1.14.

DEFINE BUTTON BUTTON-23 
     LABEL "Pedido Completo" 
     SIZE 20 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Borrar Item OE" 
     SIZE 31 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "Agregar Pallets a Parte OE" 
     SIZE 31 BY 1.14.

DEFINE VARIABLE totalPallet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Total de Pallets del Pedido Packing" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 14 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE vpalletsElegidos AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Pallets" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-20 AT ROW 1 COL 119
     BUTTON-21 AT ROW 2.19 COL 119
     BUTTON-22 AT ROW 3.38 COL 119
     BUTTON-1 AT ROW 12.67 COL 111
     BUTTON-23 AT ROW 12.67 COL 135
     totalPallet AT ROW 12.91 COL 96 COLON-ALIGNED
     BUTTON-5 AT ROW 13.86 COL 111
     vpalletsElegidos AT ROW 14.33 COL 96 COLON-ALIGNED
     BUTTON-2 AT ROW 15.05 COL 111
     BUTTON-19 AT ROW 16 COL 62
     BUTTON-3 AT ROW 16.24 COL 111
     "Partes Pedidos Packing" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 5.29 COL 97
          FGCOLOR 9 
     "Pedidos Packing" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 5.29 COL 2
          FGCOLOR 9 
     "OEs" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 17.43 COL 3
          FGCOLOR 12 
     "Partes OE" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 17.43 COL 54
          FGCOLOR 12 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155 BY 23.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "OE Fruta Fresca"
         HEIGHT             = 23.91
         WIDTH              = 155
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 156.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 156.2
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
ASSIGN 
       FRAME fMain:POPUP-MENU       = MENU POPUP-MENU-fMain:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* OE Fruta Fresca */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* OE Fruta Fresca */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Crear Parte de OE */
DO:
    DEFINE VAR hRowObjectOE AS HANDLE NO-UNDO.
    DEFINE VAR rIPP AS ROWID NO-UNDO.
    
    IF INTEGER(vPalletsElegidos:SCREEN-VALUE IN FRAME FMain) > 0 THEN DO:
        
        hRowObjectOE = DYNAMIC-FUNCTION ('getRowObject' IN h_dOrdenEntrega).
        
        rIPP = DYNAMIC-FUNCTION('getRowid':U IN h_ditemspedidospacking).
        
        DYNAMIC-FUNCTION('createItemOrdenEntrega' IN h_ditemsordenentrega, 
                         rIPP,
                         INTEGER(hRowObjectOE:BUFFER-FIELD('id_orden_entrega'):BUFFER-VALUE),
                         INTEGER(vPalletsElegidos:SCREEN-VALUE IN FRAME FMain)).
        
        DYNAMIC-FUNCTION('openQuery' IN h_dItemsOrdenEntrega).
        IF RETURN-VALUE <> "" THEN DO:
          MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
        END.
        RUN refreshRow IN h_dItemsOrdenEntrega.
    END.
    ELSE DO:
        MESSAGE "No a ingresado una cantidad de Pallets" VIEW-AS ALERT-BOX.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 wWin
ON CHOOSE OF BUTTON-19 IN FRAME fMain /* Imprimir */
DO:
    DEFINE VAR rOE AS ROWID NO-UNDO.
    
    rOE = DYNAMIC-FUNCTION('getRowid':U IN h_dOrdenEntrega).

    /* RUN p_rep_oe_ff_fax.p (INPUT rOE). */
    
    SESSION:NUMERIC-FORMAT = "AMERICAN".

    RUN p_rep_oe_ff_fax.p (INPUT rOE).
    RUN p_reportes_9.p (input "newOEFFxClixEnv",
                        input "Reporte de Orden de Entregas de FF",
                        input "",
                        input ""). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Quitar Pallets de la Parte de OE */
DO:
    RUN modifyPallets(INPUT INTEGER(vPalletsElegidos:SCREEN-VALUE IN FRAME FMain),
                      INPUT "-").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-20 wWin
ON CHOOSE OF BUTTON-20 IN FRAME fMain /* clientes Envases Precios */
DO:
  RUN wrClientesEnvasesPrecios.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-21 wWin
ON CHOOSE OF BUTTON-21 IN FRAME fMain /* Pallets y Contenedores */
DO:
  RUN wrPalletsContenedores.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-22 wWin
ON CHOOSE OF BUTTON-22 IN FRAME fMain /* clientes en Cons c/Flete */
DO:
  RUN wrFletesClienteCons.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-23 wWin
ON CHOOSE OF BUTTON-23 IN FRAME fMain /* Pedido Completo */
DO:
    DEFINE VAR hRowObjectOE AS HANDLE NO-UNDO.
    DEFINE VAR rIPP AS ROWID NO-UNDO.
    DEFINE VAR rPedidosPacking AS ROWID NO-UNDO.
    
    hRowObjectOE = DYNAMIC-FUNCTION ('getRowObject' IN h_dOrdenEntrega).
    rPedidosPacking = DYNAMIC-FUNCTION('getRowid':U IN h_dpedidospacking).
    
    
    DYNAMIC-FUNCTION('createItemOEPedidosPackingCompleto' IN h_ditemsordenentrega, 
                      rPedidosPacking,
                      INTEGER(hRowObjectOE:BUFFER-FIELD('id_orden_entrega'):BUFFER-VALUE)).
        
    DYNAMIC-FUNCTION('openQuery' IN h_dItemsOrdenEntrega).
    IF RETURN-VALUE <> "" THEN DO:
      MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
    END.
    RUN refreshRow IN h_dItemsOrdenEntrega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME fMain /* Borrar Item OE */
DO:
    DEFINE VAR vColor AS CHAR NO-UNDO.
    DEFINE VAR vRowidIPP AS ROWID NO-UNDO.

    vRowidIPP = DYNAMIC-FUNCTION('deleteItemOrdenEntrega' IN h_ditemsordenentrega).

    /* vColor = DYNAMIC-FUNCTION('setEstadoPedidoPacking' IN h_dPedidosPacking,
                               INPUT vRowidIPP).

     DYNAMIC-FUNCTION('setColorFila' IN h_bPedidosPacking,
                     INPUT vColor).
       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 wWin
ON CHOOSE OF BUTTON-5 IN FRAME fMain /* Agregar Pallets a Parte OE */
DO:
  RUN modifyPallets(INPUT INTEGER(vPalletsElegidos:SCREEN-VALUE IN FRAME FMain),
                    INPUT "+").
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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dpedidospacking.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedpedidospackingUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dpedidospacking ).
       RUN repositionObject IN h_dpedidospacking ( 1.00 , 77.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemspedidospacking.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_pedidos_packing.id_empresa,id_empresa,items_pedidos_packing.id_orden,id_orden,items_pedidos_packing.id_punto_emisor,id_punto_emisorRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemspedidospackingUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemspedidospacking ).
       RUN repositionObject IN h_ditemspedidospacking ( 3.14 , 107.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemspedidospacking.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemspedidospacking ).
       RUN repositionObject IN h_bitemspedidospacking ( 6.00 , 90.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemspedidospacking ( 6.67 , 65.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bpedidospacking.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bpedidospacking ).
       RUN repositionObject IN h_bpedidospacking ( 6.00 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bpedidospacking ( 6.67 , 87.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'dordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dordenentrega ).
       RUN repositionObject IN h_dordenentrega ( 18.62 , 45.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemsordenentrega.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_orden_entrega.id_orden_entrega,id_orden_entregaRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsordenentregaUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsordenentrega ).
       RUN repositionObject IN h_ditemsordenentrega ( 15.05 , 142.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bitemsordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsordenentrega ).
       RUN repositionObject IN h_bitemsordenentrega ( 18.14 , 54.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsordenentrega ( 6.67 , 98.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bordenentrega.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bordenentrega ).
       RUN repositionObject IN h_bordenentrega ( 18.14 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bordenentrega ( 6.67 , 45.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsano,semana,puerto_origenOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth16DefaultEditorLines1ViewAsFieldsFieldOperatorStylessemanaRANGEFieldFormatsFieldWidthsFieldLabelsFieldToolTipsFieldHelpIdsid_puerto_sal0semana0ano0puerto_origen0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter ).
       RUN repositionObject IN h_dynfilter ( 1.00 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter ( 4.43 , 71.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsanio,semana_embarqueOperatorStyleImplicitOperatorViewAsCombo-boxOperator=UseBeginsyesUseContainsyesDefaultWidth16DefaultCharWidth20DefaultEditorLines1ViewAsFieldsFieldOperatorStylesFieldFormatsFieldWidthsFieldLabelssemana_embarqueSemanaFieldToolTipsFieldHelpIdsanio0semana_embarque0DesignDataObjectFieldColumn20HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynfilter-2 ).
       RUN repositionObject IN h_dynfilter-2 ( 13.38 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynfilter-2 ( 3.71 , 56.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dpedidospacking. */
       RUN addLink ( h_dynfilter , 'Filter':U , h_dpedidospacking ).

       /* Links to SmartDataObject h_ditemspedidospacking. */
       RUN addLink ( h_dpedidospacking , 'Data':U , h_ditemspedidospacking ).

       /* Links to SmartDataBrowser h_bitemspedidospacking. */
       RUN addLink ( h_ditemspedidospacking , 'Data':U , h_bitemspedidospacking ).

       /* Links to SmartDataBrowser h_bpedidospacking. */
       RUN addLink ( h_dpedidospacking , 'Data':U , h_bpedidospacking ).

       /* Links to SmartDataObject h_dordenentrega. */
       RUN addLink ( h_dynfilter-2 , 'Filter':U , h_dordenentrega ).

       /* Links to SmartDataObject h_ditemsordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_ditemsordenentrega ).

       /* Links to SmartDataBrowser h_bitemsordenentrega. */
       RUN addLink ( h_ditemsordenentrega , 'Data':U , h_bitemsordenentrega ).

       /* Links to SmartDataBrowser h_bordenentrega. */
       RUN addLink ( h_dordenentrega , 'Data':U , h_bordenentrega ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynfilter ,
             BUTTON-20:HANDLE IN FRAME fMain , 'BEFORE':U ).
       RUN adjustTabOrder ( h_bpedidospacking ,
             BUTTON-22:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_bitemspedidospacking ,
             h_bpedidospacking , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynfilter-2 ,
             totalPallet:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_bordenentrega ,
             BUTTON-3:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_bitemsordenentrega ,
             h_bordenentrega , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  {adm2/support/changePage.i}.  
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

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
  DISPLAY totalPallet vpalletsElegidos 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BUTTON-20 BUTTON-21 BUTTON-22 BUTTON-1 BUTTON-23 totalPallet BUTTON-5 
         vpalletsElegidos BUTTON-2 BUTTON-19 BUTTON-3 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE modifyPallets wWin 
PROCEDURE modifyPallets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER vPalletsElegidos AS INTEGER.
    DEFINE INPUT PARAMETER vSigno AS CHAR.
    DEFINE VAR rIPP AS ROWID NO-UNDO.
    DEFINE VAR vColor AS CHAR NO-UNDO.
    
    IF vPalletsElegidos > 0 THEN DO:
        
        rIPP = DYNAMIC-FUNCTION('getRowid':U IN h_ditemspedidospacking).
        DYNAMIC-FUNCTION ('modifyCantidadPallets' IN h_dItemsOrdenEntrega,
                          INPUT rIPP,
                          INPUT vPalletsElegidos,
                          INPUT vSigno).
        
        /* vColor = DYNAMIC-FUNCTION('setEstadoPedidoPacking' IN h_dPedidosPacking).
    
        DYNAMIC-FUNCTION('setColorFila' IN h_bPedidosPacking,
                         INPUT vColor).
          */

        DYNAMIC-FUNCTION('openQuery' IN h_dItemsOrdenEntrega).
        
        IF RETURN-VALUE <> "" THEN DO:
          MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
        END.
        
        RUN refreshRow IN h_dItemsOrdenEntrega.
        RUN refreshRow IN h_dPedidosPacking.
    END.
    ELSE DO:
        MESSAGE "No a ingresado una cantidad de Pallets" VIEW-AS ALERT-BOX.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTotalPallets wWin 
PROCEDURE setTotalPallets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pTotalPallets AS INTEGER.

totalPallet:SCREEN-VALUE IN FRAME FMain = STRING(pTotalPallets).

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

