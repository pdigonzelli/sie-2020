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

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

DEFINE TEMP-TABLE ttRptRelease
  RCODE-INFORMATION
  FIELD nro_release         AS CHARACTER COLUMN-LABEL "Nro Release"
  FIELD fecha_creacion      AS CHARACTER COLUMN-LABEL "Fecha Creacion"
  FIELD fecha_confirmacion  AS CHARACTER COLUMN-LABEL "Fecha Confirmacion"
  FIELD fecha_limite_frio   AS CHARACTER COLUMN-LABEL "Fecha Limite Frio"
  FIELD cliente             AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD warehouse           AS CHARACTER COLUMN-LABEL "Warehouse"
  FIELD delivery_carrier    AS CHARACTER COLUMN-LABEL "Delivery Carrier"
  FIELD ITEM_release        AS CHARACTER COLUMN-LABEL "Parte Release"
  FIELD lote_sm             AS CHARACTER COLUMN-LABEL "Lote SM"
  FIELD lote_whs            AS CHARACTER COLUMN-LABEL "Lote WHS"
  FIELD cantidad            AS CHARACTER COLUMN-LABEL "Cantidad"
  FIELD fecha_ingreso       AS CHARACTER COLUMN-LABEL "Fecha Ingreso"
  FIELD proforma            AS CHARACTER COLUMN-LABEL "Proforma"
  FIELD calidad             AS CHARACTER COLUMN-LABEL "Calidad"
  FIELD envase              AS CHARACTER COLUMN-LABEL "Envase"
  FIELD vapor               AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD contenedor          AS CHARACTER COLUMN-LABEL "Contenedor"
  FIELD contrato            AS CHARACTER COLUMN-LABEL "Contrato"
  FIELD fecha_eta           AS CHARACTER COLUMN-LABEL "ETA"
  FIELD fecha_salida_origen AS CHARACTER COLUMN-LABEL "Fecha Salida Origen"
  FIELD fecha_eta_real      AS CHARACTER COLUMN-LABEL "ETA Real"
  FIELD gastos              AS CHARACTER COLUMN-LABEL "Gastos".

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
&Scoped-Define ENABLED-OBJECTS BUTTON-43 

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
DEFINE BUTTON BUTTON-43 
     LABEL "Button 43" 
     SIZE 15 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-43 AT ROW 3.86 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 67.6 BY 8.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 8.81
         WIDTH              = 67.6
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" wWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-43
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-43 wWin
ON CHOOSE OF BUTTON-43 IN FRAME fMain /* Button 43 */
DO:
  RUN fillTempTable.
  RUN generateExcel.p (INPUT TABLE ttRptRelease,
                        INPUT " Releases ",
                        INPUT " ",
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
  ENABLE BUTTON-43 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTempTable wWin 
PROCEDURE fillTempTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRelease   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFCreacion AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFEntrega  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLimiteFri AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCliente   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cWarehouse AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDCarrier  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cObs       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cParteRel  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLoteSM    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLoteWHS   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCantidad  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFIngreso  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cProforma  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCalidad   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnvase    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVapor     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCont      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cContrato  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEta       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEtaReal   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFSalidaOr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGastos    AS CHARACTER  NO-UNDO.
  
  
  DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet    AS CHARACTER  NO-UNDO INITIAL ",,,,,,".
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  
  FOR EACH release_delivery NO-LOCK.
    FIND FIRST clientes WHERE clientes.id_cliente = RELEASE_delivery.id_cliente NO-LOCK NO-ERROR.
    FIND FIRST sucursales WHERE sucursales.id_sucursal = RELEASE_delivery.id_sucursal_ubicacion NO-LOCK NO-ERROR.
    FIND FIRST contactos_industria WHERE id_contacto = RELEASE_delivery.id_delivery_carrier NO-LOCK NO-ERROR.
    ASSIGN cRelease   = STRING(RELEASE_delivery.numero_release)
           cFCreacion = STRING(RELEASE_delivery.fecha_creacion, "99/99/9999")
           cFEntrega  = STRING(RELEASE_delivery.fecha_confirmacion_entrega, "99/99/9999")
           cLimiteFri = STRING(RELEASE_delivery.fecha_limite_frio, "99/99/9999")
           cObs       = RELEASE_delivery.observaciones
           cCliente   = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NO CUST INFO"
           cWarehouse = IF AVAILABLE sucursales THEN sucursales.nombre ELSE "NO WAREHOUSE INFO"
           cDCarrier  = IF AVAILABLE contactos_industria THEN contactos_industria.razon_social ELSE "NO D. CARRIER INFO".
    FOR EACH items_release_delivery OF RELEASE_delivery NO-LOCK.
      RUN getLoteDeposito IN hLib (items_release_delivery.id_empresa,
                                   items_release_delivery.id_sucursal, 
                                   items_release_delivery.id_tipotambor, 
                                   items_release_delivery.nromov, 
                                   OUTPUT cLoteWHS).      
      RUN getFechaIngresoADeposito IN hLib (items_release_delivery.id_empresa,
                                            items_release_delivery.id_sucursal, 
                                            items_release_delivery.id_tipotambor, 
                                            items_release_delivery.nromov, 
                                            OUTPUT cFIngreso).            
      RUN getPackingList IN hLib (items_release_delivery.id_empresa, 
                                  items_release_delivery.id_sucursal, 
                                  items_release_delivery.id_tipotambor, 
                                  items_release_delivery.nromov, 
                                  OUTPUT cRet).
      cCont = STRING(ENTRY(4, cRet)).
      cProforma = ENTRY(6, cRet).            
      RUN getDatosEnvio IN hLib(items_release_delivery.id_empresa, 
                                items_release_delivery.id_sucursal, 
                                items_release_delivery.id_tipotambor, 
                                items_release_delivery.nromov, 
                                OUTPUT cRet).
      cEta = ENTRY(2, cRet).
      cFSalidaOr = ENTRY(3, cRet).
      cEtaReal = ENTRY(4, cRet).      
      RUN getCalidad IN hLib(items_release_delivery.id_empresa, 
                             items_release_delivery.id_sucursal, 
                             items_release_delivery.id_tipotambor, 
                             items_release_delivery.nromov, 
                             OUTPUT cCalidad). 
      RUN getEnvase IN hLib(items_release_delivery.id_empresa, 
                            items_release_delivery.id_sucursal, 
                            items_release_delivery.id_tipotambor, 
                            items_release_delivery.nromov, 
                            OUTPUT cEnvase).       
      FIND FIRST lotes_ubicacion WHERE items_release_delivery.nromov = lotes_ubicacion.nromov NO-LOCK NO-ERROR.
      FIND FIRST vapores WHERE vapores.id_vapor = items_release_delivery.id_vapor NO-LOCK NO-ERROR.
      
      ASSIGN cParteRel = STRING(items_release_delivery.ITEM_release_delivery)
             cLoteSM   = IF AVAILABLE lotes_ubicacion THEN lotes_ubicacion.lote ELSE STRING(items_release_delivery.id_lote)
             cCantidad = STRING(items_release_delivery.tambores)
             cVapor    = IF AVAILABLE vapores THEN vapores.descripcion ELSE "NO VESSELL INFO"
             cContrato = items_release_delivery.id_contrato + " Parte: " + STRING(items_release_delivery.ITEM_contrato).
      
      CREATE ttRptRelease.
      ASSIGN ttRptRelease.nro_release         = cRelease  
             ttRptRelease.fecha_creacion      = cFCreacion  
             ttRptRelease.fecha_confirmacion  = cFEntrega
             ttRptRelease.fecha_limite_frio   = cLimiteFri
             ttRptRelease.cliente             = cCliente            
             ttRptRelease.warehouse           = cWarehouse
             ttRptRelease.delivery_carrier    = cDCarrier
             ttRptRelease.item_release        = cParteRel
             ttRptRelease.lote_sm             = cLoteSM            
             ttRptRelease.lote_whs            = cLoteWHS
             ttRptRelease.cantidad            = cCantidad
             ttRptRelease.fecha_ingreso       = cFIngreso
             ttRptRelease.proforma            = cProforma
             ttRptRelease.calidad             = cCalidad
             ttRptRelease.envase              = cEnvase
             ttRptRelease.vapor               = cVapor
             ttRptRelease.contenedor          = cCont
             ttRptRelease.contrato            = cContrato
             ttRptRelease.fecha_eta           = cEta
             ttRptRelease.fecha_salida_origen = cFSalidaOr
             ttRptRelease.fecha_eta_real      = cEtaReal
             ttRptRelease.gastos              = "NOT IMPLEMENTED".


    END.
    

  END.
  
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
  RUN libItemsReleaseDelivery.p PERSISTENT SET hLib.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */



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

