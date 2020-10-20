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

DEFINE VAR vcMode       AS CHARACTER NO-UNDO.

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

DEFINE TEMP-TABLE ttRptRelease
  RCODE-INFORMATION
  FIELD warehouse           AS CHARACTER COLUMN-LABEL "Warehouse"
  FIELD nro_release         AS CHARACTER COLUMN-LABEL "Nro Release"
  FIELD fecha_creacion      AS CHARACTER COLUMN-LABEL "Fecha Creacion Release"
  FIELD fecha_entrega       AS CHARACTER COLUMN-LABEL "Fecha Entrega"
  FIELD fecha_confirmacion  AS CHARACTER COLUMN-LABEL "Fecha Confirmacion"
  FIELD fecha_limite_frio   AS CHARACTER COLUMN-LABEL "Fecha Limite Frio"
  FIELD cliente             AS CHARACTER COLUMN-LABEL "Cliente"
  FIELD delivery_carrier    AS CHARACTER COLUMN-LABEL "Delivery Carrier"
  FIELD ITEM_release        AS CHARACTER COLUMN-LABEL "Parte Release"
  FIELD lote_sm             AS CHARACTER COLUMN-LABEL "Lote SM" FORMAT "999999/9999"
  FIELD lote_whs            AS CHARACTER COLUMN-LABEL "Lote WHS"
  FIELD cantidad            AS CHARACTER COLUMN-LABEL "Cantidad"
  FIELD fecha_ingreso       AS CHARACTER COLUMN-LABEL "Fecha Ingreso"
  FIELD proforma            AS CHARACTER COLUMN-LABEL "Proforma"
  FIELD calidad             AS CHARACTER COLUMN-LABEL "Calidad"
  FIELD envase              AS CHARACTER COLUMN-LABEL "Envase"
  FIELD vapor               AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD contenedor          AS CHARACTER COLUMN-LABEL "Contenedor"
  FIELD contrato            AS CHARACTER COLUMN-LABEL "Contrato"
  FIELD gastos              AS CHARACTER COLUMN-LABEL "Gastos".
  /*
  FIELD fecha_eta           AS CHARACTER COLUMN-LABEL "ETA"
  FIELD fecha_salida_origen AS CHARACTER COLUMN-LABEL "Fecha Salida Origen"
  FIELD fecha_eta_real      AS CHARACTER COLUMN-LABEL "ETA Real"
  */

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
&Scoped-Define ENABLED-OBJECTS btnAddItem RECT-1 RECT-2 RECT-3 RECT-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createButtons wWin 
FUNCTION createButtons RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoteUbicacionHandle wWin 
FUNCTION getLoteUbicacionHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
DEFINE VARIABLE h_bidcontrato AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemscontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bitemsrelease AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bqdetallelote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_breleasedelivery AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dcontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemscontratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ditemsreleasedelivery AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dloteubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dqdetallelote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dreleasedelivery AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_freleasedelivery AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vitemsreleasedelivery AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vqdetallelote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vreleasedelivery AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddItem  NO-FOCUS FLAT-BUTTON
     LABEL "Agregar Item >>" 
     SIZE 16 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 9.76.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 82 BY 8.33.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 148 BY 13.81.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnAddItem AT ROW 20.05 COL 67
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 2.43 COL 67
     RECT-3 AT ROW 11 COL 1
     RECT-4 AT ROW 19.86 COL 63
     "Releases" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1 COL 3
          FONT 6
     "Datos" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.29 COL 69
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148 BY 23.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Administracion de Releases y Deliveries"
         HEIGHT             = 23.86
         WIDTH              = 148
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
ON END-ERROR OF wWin /* Administracion de Releases y Deliveries */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Administracion de Releases y Deliveries */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddItem wWin
ON CHOOSE OF btnAddItem IN FRAME fMain /* Agregar Item >> */
DO:
  DEFINE VAR cRet  AS CHARACTER NO-UNDO.
  DEFINE VAR cRows AS CHARACTER NO-UNDO.
  DEFINE VAR hRowObject AS HANDLE NO-UNDO.

  hRowObject = DYNAMIC-FUNCTION ('getRowObject' IN h_ditemscontratos).

  cRows =  DYNAMIC-FUNCTION('getSelectedRows' IN h_bLoteUbicacion).

  IF cRows <> "" THEN DO:
    DYNAMIC-FUNCTION('createItemReleaseFromLUbicacionContrato' IN h_ditemsreleasedelivery, 
                     h_dLoteubicacion, 
                     cRows, 
                     hRowObject:BUFFER-FIELD('id_contrato'):BUFFER-VALUE,
                     hRowObject:BUFFER-FIELD('item'):BUFFER-VALUE, 
                     hRowObject:BUFFER-FIELD('id_tipo_contrato'):BUFFER-VALUE, 
                     hRowObject:BUFFER-FIELD('anio'):BUFFER-VALUE).

    DYNAMIC-FUNCTION('openQuery' IN h_dloteubicacion).
    IF RETURN-VALUE <> "" THEN DO:
      MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
    END.
    RUN refreshRow IN h_dreleasedelivery.
  END.



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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addToolbarButton wWin 
PROCEDURE addToolbarButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR xcColumns AS CHARACTER INITIAL "Name,Caption,Image,Type,OnChoose,AccessType,Parent".

&SCOP dlmt + CHR(1) +
/*
/* only to define a new Action Group */
  DYNAMIC-FUNCTION( "defineAction" IN h_dyntoolbar,
                                      "FUNCTION",           /* action group */
                                      "Name, 
                                      Caption",
                                      "myButtons" {&dlmt}   /* Name */
                                      "myButtons" {&dlmt}   /*Caption */
                                      "").
*/  
  /* define an action for my button */
  DYNAMIC-FUNCTION( "defineAction" IN h_dyntoolbar,
                                      "myButtonAction", /* Action */
                                      xcColumns,
                                      "tlbImprimir" {&dlmt} /* Name */
                                      "Imprimir" {&dlmt} /* Caption*/
                                      "print.bmp" {&dlmt} /* Image */
                                      "PUBLISH" {&dlmt} /* TYPE */
                                      /* when you press the button the smart toolbar */
                                      /* will PUBLISH myButtonAction */
                                      "tlbImprimir" {&dlmt} /* OnChoose */
                                      "READ" {&dlmt} /* AccessType */
                                      "FUNCTION" {&dlmt} /* parent *//* Parent - change it to FUNCTION if you don't want a new group */
                                      "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
             INPUT  'dreleasedelivery.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedreleasedeliveryUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dreleasedelivery ).
       RUN repositionObject IN h_dreleasedelivery ( 3.62 , 62.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'breleasedelivery.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_breleasedelivery ).
       RUN repositionObject IN h_breleasedelivery ( 1.71 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_breleasedelivery ( 8.81 , 61.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vreleasedelivery.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vreleasedelivery ).
       RUN repositionObject IN h_vreleasedelivery ( 2.67 , 74.00 ) NO-ERROR.
       /* Size in AB:  ( 7.95 , 69.40 ) */

       RUN constructObject (
             INPUT  'dloteubicacion.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldslotes_ubicacion.id_sucursal_ubicacion,id_sucursal_ubicacionRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedloteubicacionUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dloteubicacion ).
       RUN repositionObject IN h_dloteubicacion ( 22.19 , 69.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dqdetallelote.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldslotes_ubicacion.id_sucursal_ubicacion,id_sucursal_ubicacionRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedqdetalleloteUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dqdetallelote ).
       RUN repositionObject IN h_dqdetallelote ( 22.43 , 117.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'dcontratos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedcontratosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_dcontratos ).
       RUN repositionObject IN h_dcontratos ( 12.43 , 63.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'ditemscontratos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_contratos.id_contrato,id_contratoRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemscontratosUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemscontratos ).
       RUN repositionObject IN h_ditemscontratos ( 12.43 , 73.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Function,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsMybuttonaction,CopyHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 67.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 82.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Detalle Release|Stock en Deposito' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 11.24 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 13.33 , 146.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('2') NO-ERROR.

       /* Links to SmartDataObject h_dreleasedelivery. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dreleasedelivery ).
       RUN addLink ( h_freleasedelivery , 'Filter':U , h_dreleasedelivery ).

       /* Links to SmartDataBrowser h_breleasedelivery. */
       RUN addLink ( h_dreleasedelivery , 'Data':U , h_breleasedelivery ).
       RUN addLink ( h_breleasedelivery , 'Update':U , h_freleasedelivery ).

       /* Links to SmartDataViewer h_vreleasedelivery. */
       RUN addLink ( h_dreleasedelivery , 'Data':U , h_vreleasedelivery ).
       RUN addLink ( h_vreleasedelivery , 'Update':U , h_dreleasedelivery ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vreleasedelivery ).

       /* Links to SmartDataObject h_dloteubicacion. */
       RUN addLink ( h_dreleasedelivery , 'Data':U , h_dloteubicacion ).

       /* Links to SmartDataObject h_dqdetallelote. */
       RUN addLink ( h_dreleasedelivery , 'Data':U , h_dqdetallelote ).

       /* Links to SmartDataObject h_ditemscontratos. */
       RUN addLink ( h_dcontratos , 'Data':U , h_ditemscontratos ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'ditemsreleasedelivery.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsitems_release_delivery.id_release_delivery,id_release_deliveryRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameditemsreleasedeliveryUpdateFromSourcenoToggleDataTargetsyesOpenOnInityes':U ,
             OUTPUT h_ditemsreleasedelivery ).
       RUN repositionObject IN h_ditemsreleasedelivery ( 17.43 , 68.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'vitemsreleasedelivery.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vitemsreleasedelivery ).
       RUN repositionObject IN h_vitemsreleasedelivery ( 13.86 , 86.00 ) NO-ERROR.
       /* Size in AB:  ( 5.81 , 60.60 ) */

       RUN constructObject (
             INPUT  'bitemsrelease.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemsrelease ).
       RUN repositionObject IN h_bitemsrelease ( 19.81 , 87.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemsrelease ( 4.52 , 60.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bloteubicacion.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bloteubicacion ).
       RUN repositionObject IN h_bloteubicacion ( 17.67 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bloteubicacion ( 6.67 , 61.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bitemscontratos.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bitemscontratos ).
       RUN repositionObject IN h_bitemscontratos ( 12.43 , 21.00 ) NO-ERROR.
       RUN resizeObject IN h_bitemscontratos ( 5.00 , 43.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'bidcontrato.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bidcontrato ).
       RUN repositionObject IN h_bidcontrato ( 12.43 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bidcontrato ( 5.00 , 17.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,NavigationSubModulesTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsCopy,AddHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsCopy,AddNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 12.43 , 86.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 60.20 ) NO-ERROR.

       /* Links to SmartDataObject h_ditemsreleasedelivery. */
       RUN addLink ( h_dreleasedelivery , 'Data':U , h_ditemsreleasedelivery ).
       RUN addLink ( h_dyntoolbar-2 , 'Navigation':U , h_ditemsreleasedelivery ).

       /* Links to SmartDataViewer h_vitemsreleasedelivery. */
       RUN addLink ( h_ditemsreleasedelivery , 'Data':U , h_vitemsreleasedelivery ).
       RUN addLink ( h_vitemsreleasedelivery , 'Update':U , h_ditemsreleasedelivery ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vitemsreleasedelivery ).

       /* Links to SmartDataBrowser h_bitemsrelease. */
       RUN addLink ( h_ditemsreleasedelivery , 'Data':U , h_bitemsrelease ).

       /* Links to SmartDataBrowser h_bloteubicacion. */
       RUN addLink ( h_dloteubicacion , 'Data':U , h_bloteubicacion ).

       /* Links to SmartDataBrowser h_bitemscontratos. */
       RUN addLink ( h_ditemscontratos , 'Data':U , h_bitemscontratos ).

       /* Links to SmartDataBrowser h_bidcontrato. */
       RUN addLink ( h_dcontratos , 'Data':U , h_bidcontrato ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'freleasedelivery.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_freleasedelivery ).
       /* Position in AB:  ( 22.29 , 129.80 ) */
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'bqdetallelote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bqdetallelote ).
       RUN repositionObject IN h_bqdetallelote ( 13.38 , 3.00 ) NO-ERROR.
       RUN resizeObject IN h_bqdetallelote ( 8.81 , 73.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'vqdetallelote.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vqdetallelote ).
       RUN repositionObject IN h_vqdetallelote ( 13.38 , 76.00 ) NO-ERROR.
       /* Size in AB:  ( 8.57 , 62.60 ) */

       /* Links to SmartWindow h_freleasedelivery. */
       RUN addLink ( h_dreleasedelivery , 'Data':U , h_freleasedelivery ).

       /* Links to SmartDataBrowser h_bqdetallelote. */
       RUN addLink ( h_dqdetallelote , 'Data':U , h_bqdetallelote ).

       /* Links to SmartDataViewer h_vqdetallelote. */
       RUN addLink ( h_dqdetallelote , 'Data':U , h_vqdetallelote ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnExcel wWin 
PROCEDURE btnExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN fillTempTable.
  RUN generateExcel.p (INPUT TABLE ttRptRelease,
                        INPUT " Releases ",
                        INPUT " ",
                        INPUT 7,
                        INPUT 8,
                        INPUT "Century Gothic",
                        INPUT 7).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnGraph wWin 
PROCEDURE btnGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wGraphDepExt.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnMail wWin 
PROCEDURE btnMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cUsuarios AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSubject  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBody     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iNroRelease AS INTEGER    NO-UNDO.

    iNroRelease = DYNAMIC-FUNCTION('columnValue':U IN h_dReleaseDelivery, INPUT 'numero_release').
                  
    /* MANDO EL MAIL AVISANDO LA CONFIRMACION DEL RELEASE  A LA GENTE DE FINANZAS*/ 
               FOR EACH usuarios_listas WHERE usuarios_listas.id_lista = 11 NO-LOCK.
                    cUsuarios = cUsuarios + "," + usuarios_listas.email.            
               END.
                
               IF cUsuarios <> "" then cUsuarios = substring(cUsuarios,2,length(cUsuarios) - 1).
        
               cSubject = "Se ha creado el Release : " + STRING(iNroRelease).
               cBody = "No se que datos quieren tener en el body del mail".
                        
               IF cUsuarios <> "" THEN DO:
                   RUN SendMail.p(INPUT "",           /* SIEMPRE TIENE QUE IR */
                                  INPUT 2,            /* PRIORIDAD */
                                  INPUT cSubject,     /* SUBJECT */
                                  INPUT cBody,        /* BODY     */
                                  INPUT cUsuarios,    /* DEST. SEP COMAS */
                                  INPUT ""            /* ARCHIVOS ATTACHED SEP POR COMAS */
                                 ).
               END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnWizItems wWin 
PROCEDURE btnWizItems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN wReleaseAsistente.w (DYNAMIC-FUNCTION('columnValue' IN h_dReleaseDelivery, "id_release_delivery")).
  RUN refreshRow IN h_dReleaseDelivery.
  DYNAMIC-FUNCTION('openQuery' IN h_dItemsReleaseDelivery).
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


/*
  IF iPage = 3 THEN DO:
    chGraphPie:VISIBLE = TRUE.
  END.
  ELSE DO:
    chGraphPie:VISIBLE = FALSE.
  END.
    
  */
  /* Code placed here will execute AFTER standard behavior.    */
/*
IF iPage = 0 THEN
    BtnAddItem:VISIBLE IN FRAME {&FRAME-NAME} = FALSE.
ELSE
    BtnAddItem:VISIBLE IN FRAME {&FRAME-NAME} = TRUE.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContratoChanged wWin 
PROCEDURE ContratoChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR vhRowObject     AS HANDLE    NO-UNDO.
DEFINE VAR viArticulo      AS INTEGER   NO-UNDO.
DEFINE VAR viSucursal      AS INTEGER   NO-UNDO.

viArticulo = DYNAMIC-FUNCTION('columnValue' IN h_dItemsContratos , "id_articulo").
viSucursal = DYNAMIC-FUNCTION('columnValue' IN h_dReleaseDelivery, "id_sucursal_ubicacion").

IF viArticulo <> ? AND viSucursal <> ?  THEN DO:
  RUN openQueryForContrato IN h_dLoteUbicacion (viSucursal , viArticulo).
  RUN paintBrowser.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects wWin 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  createbuttons().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableReleaseUpdate wWin 
PROCEDURE enableReleaseUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bRel FOR RELEASE_delivery.
DEFINE INPUT PARAMETER piRelease AS INTEGER NO-UNDO.

DEFINE VARIABLE hBro  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hCol  AS HANDLE     NO-UNDO.
DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
DEFINE VARIABLE j     AS INTEGER    NO-UNDO.
DEFINE VARIABLE k     AS INTEGER    NO-UNDO.


FIND FIRST bRel WHERE bRel.id_release_delivery = piRelease NO-LOCK NO-ERROR.
IF AVAILABLE bRel THEN DO:
  IF bRel.fecha_confirmacion_entrega <> ? THEN DO:
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "add").
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "update").
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "copy").
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "delete").
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar, "wizItemsAction").
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar-2, "update").
    DYNAMIC-FUNCTION('disableActions' IN h_dyntoolbar-2, "delete").
    BtnAddItem:VISIBLE IN FRAME {&FRAME-NAME} = FALSE.
  END.
  ELSE DO:
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "add").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "update").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "copy").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "delete").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "wizItemsAction").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "update").
    DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar-2, "delete").
    BtnAddItem:VISIBLE IN FRAME {&FRAME-NAME} = TRUE.
  END.
END.



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
  ENABLE btnAddItem RECT-1 RECT-2 RECT-3 RECT-4 
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
  DEFINE VARIABLE cFConfirma AS CHARACTER  NO-UNDO.
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
  
  FOR EACH release_delivery BY id_sucursal_ubicacion BY numero_release.
    FIND FIRST clientes WHERE clientes.id_cliente = RELEASE_delivery.id_cliente NO-LOCK NO-ERROR.
    FIND FIRST sucursales WHERE sucursales.id_sucursal = RELEASE_delivery.id_sucursal_ubicacion NO-LOCK NO-ERROR.
    FIND FIRST contactos_industria WHERE id_contacto = RELEASE_delivery.id_delivery_carrier NO-LOCK NO-ERROR.
    ASSIGN cRelease   = STRING(RELEASE_delivery.numero_release)
           cFCreacion = STRING(RELEASE_delivery.fecha_creacion, "99/99/9999")
           cFConfirma = STRING(RELEASE_delivery.fecha_confirmacion_entrega, "99/99/9999")
           cFEntrega  = STRING(RELEASE_delivery.fecha_entrega, "99/99/9999")
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
             ttRptRelease.fecha_confirmacion  = cFConfirma
             ttRptRelease.fecha_entrega       = cFEntrega
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
             ttRptRelease.gastos              = "NOT IMPLEMENTED YET".
             /*
             ttRptRelease.fecha_eta           = cEta
             ttRptRelease.fecha_salida_origen = cFSalidaOr
             ttRptRelease.fecha_eta_real      = cEtaReal
             */


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
  

  DEFINE VARIABLE cQr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lQr AS LOGICAL    NO-UNDO.

  cQr = "lotes_ubicacion.cantidad_comprometida > 0 AND lotes_ubicacion.id_sucursal_ubicacion = 0".
  lQr = DYNAMIC-FUNCTION ('addQueryWhere' IN h_dloteubicacion, cQr, "", "AND").
  DYNAMIC-FUNCTION ('openQuery' IN h_dloteubicacion).
  
  RUN openQueryLoteUbicacion (DYNAMIC-FUNCTION('columnValue' IN h_dReleaseDelivery, 'id_cliente')).
  RUN refreshRow IN h_ditemsreleasedelivery.
  
  /* enable my action */
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "PrintAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "MailAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "WizItemsAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "ExcelAction").
  DYNAMIC-FUNCTION('enableActions' IN h_dyntoolbar, "GraphAction").
  
  /* subscribe the container to act at my action */
  SUBSCRIBE TO "tlbImprimir"        IN h_dyntoolbar.
  SUBSCRIBE TO "btnMail"            IN h_dyntoolbar.
  SUBSCRIBE TO "btnWizItems"        IN h_dyntoolbar.
  SUBSCRIBE TO "btnExcel"           IN h_dyntoolbar.
  SUBSCRIBE TO "btnGraph"           IN h_dyntoolbar.

  SUBSCRIBE TO "ContratoChanged"    IN h_bItemsContratos.
  SUBSCRIBE TO "openLoteUbicacion"  IN h_dContratos.
  SUBSCRIBE TO "UpdateBeginAccion"  IN h_vReleaseDelivery.
  SUBSCRIBE TO "UpdateEndAccion"    IN h_vReleaseDelivery.
  
  

  RUN selectPage(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openLoteUbicacion wWin 
PROCEDURE openLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION('openDesdeSucursal' IN h_dLoteUbicacion , 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryLoteUbicacion wWin 
PROCEDURE openQueryLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piCliente AS INTEGER NO-UNDO.

  RUN openQueryFromCliente IN h_dContratos (INPUT piCliente).

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paintBrowser wWin 
PROCEDURE paintBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hBro  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hCol  AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cCal  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j     AS INTEGER    NO-UNDO.

  cCal = DYNAMIC-FUNCTION('columnValue' IN h_dItemsContratos, 'calidad').
  cEnv = DYNAMIC-FUNCTION('columnValue' IN h_dItemsContratos, 'envase').

  hBro = DYNAMIC-FUNCTION('getBrowseHandle' IN h_bLoteUbicacion).
  IF hBro:NUM-ITERATIONS > 0 THEN DO:
    DO i = 1 TO hBro:NUM-ITERATIONS:
      hBro:SELECT-ROW(i).
      hCol = hBro:GET-BROWSE-COLUMN(4).
      IF cCal = hCol:SCREEN-VALUE THEN DO:
        DO j = 1 TO hBro:NUM-COLUMNS:
          hCol = hBro:GET-BROWSE-COLUMN(j).
          hCol:BGCOLOR = 10.
        END.
      END.
    END.
  hBro:DESELECT-ROWS().
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshBrowserRelease wWin 
PROCEDURE refreshBrowserRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN refreshRow IN h_dreleasedelivery.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlbImprimir wWin 
PROCEDURE tlbImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRelease AS INTEGER    NO-UNDO.

  iRelease = DYNAMIC-FUNCTION('columnValue'IN h_dReleaseDelivery, 'id_release_delivery').

  RUN p_reportes_9.p (INPUT "release_delivery",
                      INPUT "Reporte de Release",
                      INPUT "release_delivery.id_release_delivery = " + STRING(iRelease),
                      INPUT "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateBeginAccion wWin 
PROCEDURE UpdateBeginAccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN selectPage(0).



btnAddItem:VISIBLE IN FRAME {&FRAME-NAME} = FALSE.

vcMode = 'UpdateBegin'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateEndAccion wWin 
PROCEDURE UpdateEndAccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN selectPage(1).

btnAddItem:VISIBLE IN FRAME {&FRAME-NAME} = TRUE.

vcMode = ''.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createButtons wWin 
FUNCTION createButtons RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*Boton de Impresion*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "PrintAction", 
                          "tlbImprimir", 
                          "Imprimir", 
                          "print.bmp", 
                          "tlbImprimir", 
                          "FUNCTION").
  /*Boton de Mail*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "MailAction", 
                          "btnSendMail", 
                          "Mail a Finanzas", 
                          "Mail.gif", 
                          "btnMail", 
                          "FUNCTION").
  /*Boton de Asistnte Items*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "WizItemsAction", 
                          "btnWizItems", 
                          "Asistente de Creacion de Items", 
                          "Items.gif", 
                          "btnWizItems", 
                          "FUNCTION").
  
  /*Boton de Exportacion a Excel*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "ExcelAction", 
                          "btnExcel", 
                          "Reporte Excel de Releases", 
                          "excel.gif", 
                          "btnExcel", 
                          "FUNCTION").
  
  /*Boton de Consulta de Graficos*/
  RUN ToolBarButtonAdd.p (h_dyntoolbar, 
                          "GraphAction", 
                          "btnGraph", 
                          "Graficos de Stock", 
                          "Chart.bmp", 
                          "btnGraph", 
                          "FUNCTION").

  

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoteUbicacionHandle wWin 
FUNCTION getLoteUbicacionHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN h_dLoteUBicacion.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

