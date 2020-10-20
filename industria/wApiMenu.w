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

DEFINE VARIABLE vhLib AS HANDLE     NO-UNDO.

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS UTTON-2 BUTTON-8 BUTTON-9 BUTTON-10 Menu ~
BUTTON-12 BUTTON-13 BUTTON-14 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD exploreMenu wWin 
FUNCTION exploreMenu RETURNS CHARACTER
  (INPUT pcLetraInicial  AS CHARACTER, 
   INPUT pcItem          AS CHARACTER, 
   INPUT piItemPrimero   AS INTEGER,
   INPUT-OUTPUT pcMenu   AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPhater wWin 
FUNCTION getPhater RETURNS CHARACTER
  (INPUT pcLetra        AS CHARACTER, 
   INPUT pcItem         AS CHARACTER,
   INPUT-OUTPUT pcPath  AS CHARACTER )  FORWARD.

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
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-10 
     LABEL "Button 10" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-12 
     LABEL "Menu Propio" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-13 
     LABEL "Tree Walk" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-14 
     LABEL "Menu Indust" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-8 
     LABEL "Button 8" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-9 
     LABEL "Button 9" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Menu 
     LABEL "Menu Aqui" 
     SIZE 15 BY 1.14.

DEFINE BUTTON UTTON-2 
     LABEL "Button 2" 
     SIZE 15 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     UTTON-2 AT ROW 4.33 COL 4
     BUTTON-8 AT ROW 4.33 COL 21
     BUTTON-9 AT ROW 4.33 COL 38
     BUTTON-10 AT ROW 4.33 COL 55
     Menu AT ROW 6.71 COL 3
     BUTTON-12 AT ROW 6.71 COL 20
     BUTTON-13 AT ROW 6.71 COL 37
     BUTTON-14 AT ROW 6.71 COL 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.4 BY 8.43.


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
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 8.43
         WIDTH              = 75.4
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


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 wWin
ON CHOOSE OF BUTTON-10 IN FRAME fMain /* Button 10 */
DO:
  RUN createProgressShortCut IN vhLib ({&WINDOW-NAME}, 
                                       "\Cascara\", 
                                       "Prueba de Creacion de ShortCut", 
                                       "wLotesCascara.w").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 wWin
ON CHOOSE OF BUTTON-12 IN FRAME fMain /* Menu Propio */
DO:
  DEFINE VARIABLE hMenu     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE h01       AS HANDLE     NO-UNDO.
  DEFINE VARIABLE h02       AS HANDLE     NO-UNDO.
  DEFINE VARIABLE h03       AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hMenuItem AS HANDLE     NO-UNDO.

  hMenu = DYNAMIC-FUNCTION('createMenuTop' IN hLib).
  h01 = DYNAMIC-FUNCTION('createMenuSub' IN hLib, hMenu, "sub1", "sub1"). 
  h02 = DYNAMIC-FUNCTION('createMenuSub' IN hLib, h01, "sub2", "sub2").
  h03 = DYNAMIC-FUNCTION('createMenuSub' IN hLib, h02, "sub3", "sub3").

  hMenuItem = DYNAMIC-FUNCTION('createMenuItem' IN hLib, h03, "item1", "item1","i", "wMovimientosCamara.w", FALSE ).



  DYNAMIC-FUNCTION('displayMenu' IN hLib, CURRENT-WINDOW, hMenu).



  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 wWin
ON CHOOSE OF BUTTON-13 IN FRAME fMain /* Tree Walk */
DO:
  RUN TreeWalk (frame FMain:FIRST-CHILD,0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 wWin
ON CHOOSE OF BUTTON-14 IN FRAME fMain /* Menu Indust */
DO:
  DEFINE VARIABLE hMenu AS HANDLE     NO-UNDO.
  
  hMenu = DYNAMIC-FUNCTION('populateMenu' IN hLib).

  DYNAMIC-FUNCTION('displayMenu' IN hLib, CURRENT-WINDOW, hMenu).  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 wWin
ON CHOOSE OF BUTTON-8 IN FRAME fMain /* Button 8 */
DO:
  DEFINE VARIABLE vcRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE c     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  
  /*
  FOR EACH par_menu_grupos WHERE letra_inicial = "r".
    c = DYNAMIC-FUNCTION('exploreMenu' IN vhLib , "r", 
                                                  par_menu_grupos.ITEM_menu, 
                                                  i, 
                                                  INPUT-OUTPUT vcRet, 
                                                  SELF:HANDLE).      
  END.
  */
  FOR EACH par_menu_grupos WHERE letra_inicial = "y".
    c = DYNAMIC-FUNCTION('exploreMenu' IN vhLib , par_menu_grupos.letra_inicial, 
                                                  par_menu_grupos.ITEM_menu, 
                                                  i, 
                                                  INPUT-OUTPUT vcRet, 
                                                  SELF:HANDLE).      
  END.
  
  /*
  FOR EACH par_menu_grupos WHERE letra_inicial = "y"
                           BY ITEM_menu .
    /*i = INTEGER(par_menu_grupos.accion_seleccion) NO-ERROR.*/
    
    /*IF NOT ERROR-STATUS:ERROR AND i <> 0 THEN DO:  /*es una carpeta */*/
      c = DYNAMIC-FUNCTION('exploreMenu' IN vhLib , "y", 
                                                    par_menu_grupos.ITEM_menu, 
                                                    i, 
                                                    INPUT-OUTPUT vcRet, 
                                                    SELF:HANDLE).      
    /*
    END.                                            
    ELSE 
      NEXT.
      */
  END.
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON CHOOSE OF BUTTON-9 IN FRAME fMain /* Button 9 */
DO:
  RUN mover IN vhLib ({&WINDOW-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Menu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Menu wWin
ON CHOOSE OF Menu IN FRAME fMain /* Menu Aqui */
DO:
  /*
  CURRENT-WINDOW:HEIGHT-CHARS = 18.
  CURRENT-WINDOW:WIDTH-CHARS = 132.*/
  
  DEFINE VARIABLE hMenu     AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE hSubMenu  AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE hMenuItem AS WIDGET-HANDLE NO-UNDO.
  
  DEFINE VARIABLE cSubMenues AS CHARACTER INITIAL "File,Edit" NO-UNDO.
  DEFINE VARIABLE cMenuItem1 AS CHARACTER INITIAL "New,Open,Close,Save,Exit" NO-UNDO.
  DEFINE VARIABLE cMenuItem2 AS CHARACTER INITIAL "Undo,Copy,Cut,Paste" NO-UNDO.
  
  
  DEFINE VARIABLE iAuxlr1 AS INTEGER NO-UNDO.
  DEFINE VARIABLE iAuxlr2 AS INTEGER NO-UNDO.
  DEFINE VARIABLE iItems  AS INTEGER NO-UNDO.
  DEFINE VARIABLE cItems  AS CHARACTER NO-UNDO.
  
  
  CREATE MENU hMenu.
  
  DO iAuxlr1 = 1 TO NUM-ENTRIES(cSubMenues):
  
    CREATE SUB-MENU hSubMenu
    ASSIGN PARENT = hMenu
    NAME = ENTRY(iAuxlr1, cSubMenues)
    LABEL = ENTRY(iAuxlr1, cSubMenues).
    
    CASE hSubMenu:NAME:
    
      WHEN "File" THEN 
        ASSIGN iItems = NUM-ENTRIES(cMenuItem1)
               cItems = cMenuItem1.
      WHEN "Edit" THEN 
        ASSIGN iItems = NUM-ENTRIES(cMenuItem2)
               cItems = cMenuItem2.
    END. /*CASE hSubMenu:NAME:*/
    
    
    DO iAuxlr2 = 1 TO iItems:
      CREATE MENU-ITEM hMenuItem
      ASSIGN PARENT = hSubMenu
      NAME = hSubMenu:NAME + ENTRY(iAuxlr2, cItems)
      LABEL = ENTRY(iAuxlr2, cItems)
      /* This IS WHERE the ACCELERATOR KEYS are DEFINED */
      ACCELERATOR = SUBSTRING(ENTRY(iAuxlr2, cItems),2,1)
      /*This version just declares the 2nd letter AS the ACCELERATOR.
      You may remove the above LINE IF you DO NOT want TO USE
      ACCELERATORS. */
      TRIGGERS:
        ON CHOOSE PERSISTENT RUN MenuItemChoose IN THIS-PROCEDURE (INPUT hMenuItem:NAME).
      END TRIGGERS.
      
      /*Example of how to disable a Menu-Item*/
      IF hMenuItem:NAME = "EditUndo" THEN 
        ASSIGN hMenuItem:SENSITIVE = FALSE.
    
    END. /*DO iAuxlr2 = 1 TO iItems:*/
  
  END. /*DO iAuxlr1 = 1 TO NUM-ENTRIES(cSubMenues):*/
  
  ASSIGN CURRENT-WINDOW:MENUBAR = hMenu:HANDLE.
  
  /*DEFINE FRAME F1
  WITH SIZE 132 BY 18 THREE-D NO-LABELS.
  
  ENABLE ALL WITH FRAME F1.
  WAIT-FOR CLOSE OF CURRENT-WINDOW.*/


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


&Scoped-define SELF-NAME UTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL UTTON-2 wWin
ON CHOOSE OF UTTON-2 IN FRAME fMain /* Button 2 */
DO:
  /*
  DEFINE VAR dbg AS LOGICAL.
  dbg = DEBUGGER:INITIATE().
  dbg = DEBUGGER:SET-BREAK().
  */
  /*
  DEFINE VARIABLE c1          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itm         AS CHARACTER.
  DEFINE VARIABLE icono       AS CHARACTER.
  DEFINE VARIABLE item-old    AS INTEGER .
  DEFINE VARIABLE v_accion    AS INTEGER.
  DEFINE VARIABLE vcModulo    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcUsuario   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcShortCut  AS CHARACTER  NO-UNDO.

  DEFINE BUFFER pm  FOR par_menu_grupos.
  DEFINE BUFFER pm1 FOR par_menu_grupos.
  DEFINE BUFFER pm2 FOR par_menu_grupos.
  
  
    
  vcUsuario = 'y_adrianca'.
  vcModulo  = "industri".
  CREATE par_usuarios_logueo.
  ASSIGN par_usuarios_logueo.id_usuario = SUBSTRING(vcUsuario,INDEX(vcUsuario,'_') + 1, LENGTH(vcUsuario) - INDEX(vcUsuario,'_'))
         par_usuarios_logueo.modulo     = vcModulo
         par_usuarios_logueo.c_fecha    = TODAY.
  RELEASE par_usuarios_logueo.

  FOR EACH par_usuarios_logueo WHERE par_usuarios_logueo.id_usuario = vcUsuario
                               NO-LOCK.
    FOR EACH par_grupos WHERE par_grupos.nombre_grupo MATCHES par_usuarios_logueo.modulo.
                        NO-LOCK.
      FIND FIRST par_menu_grupos WHERE par_menu_grupos.letra_inicial = par_grupos.letra_inicial 
                                 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE par_menu_grupos THEN NEXT.
      vcShortCut = 'GM' + par_grupos.letra_inicial + " " + par_grupos.nombre_grupo.
      DISP vcShortCut.
      
      /*run crea-item-tree('' ,'' , 'GM' + par_grupos.letra_inicial , par_grupos.nombre_grupo ,'cerrado').*/
    end.
  END.    
  */


  
  
  
  
  
  RUN createShortCut IN vhLib ({&WINDOW-NAME}, "\Actividades\").
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
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsBanda2,Banda3,Banda1SubModulesTableIOTypeSupportedLinksToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdateHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 67.20 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             UTTON-2:HANDLE IN FRAME fMain , 'BEFORE':U ).
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
  ENABLE UTTON-2 BUTTON-8 BUTTON-9 BUTTON-10 Menu BUTTON-12 BUTTON-13 BUTTON-14 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  RUN libApiMenu.p PERSISTENT SET vhLib.

  RUN libApiMenu.p PERSISTENT SET hLib.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuItemChoose wWin 
PROCEDURE MenuItemChoose :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cMenuItemName AS CHARACTER NO-UNDO.

MESSAGE "Item Selected:" SKIP(1)
"'" + cMenuItemName + "'" VIEW-AS ALERT-BOX INFORMATION.

IF cMenuItemName = 'FileExit' THEN QUIT.



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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE treeWalk wWin 
PROCEDURE treeWalk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER Grp AS WIDGET-HANDLE.
  DEF INPUT PARAMETER IntFlag AS INT.

  DEF VAR Flw AS WIDGET-HANDLE.

DO:
    Flw=Grp:FIRST-CHILD.
        DO WHILE (Flw <> ?).
            MESSAGE INTEGER(Grp) Grp:NAME Grp:TYPE SKIP
                    INTEGER(Flw) Flw:NAME Flw:TYPE SKIP
                    IntFlag.

            CASE IntFlag:
                WHEN 0 THEN RUN TreeWalk (Flw,1).
                WHEN 1 THEN RUN TreeWalk (Flw,2).
            END CASE.

            Flw=Flw:NEXT-SIBLING.       
        END.
    Grp=Grp:NEXT-SIBLING.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION exploreMenu wWin 
FUNCTION exploreMenu RETURNS CHARACTER
  (INPUT pcLetraInicial  AS CHARACTER, 
   INPUT pcItem          AS CHARACTER, 
   INPUT piItemPrimero   AS INTEGER,
   INPUT-OUTPUT pcMenu   AS CHARACTER):


  DEFINE VARIABLE viItemPrimero   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE c               AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMenu          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcItem          AS CHARACTER  NO-UNDO.

  vcMenu = pcMenu.
  FIND FIRST par_menu_grupos WHERE par_menu_grupos.letra_inicial  = pcLetraInicial
                               AND par_menu_grupos.ITEM_menu      = pcItem
                             NO-LOCK NO-ERROR.
  IF AVAILABLE par_menu_grupos THEN DO:
    viItemPrimero = INTEGER(par_menu_grupos.accion_seleccion) NO-ERROR.
    vcItem        = par_menu_grupos.ITEM_posterior.
    
    IF NOT ERROR-STATUS:ERROR AND viItemPrimero <> 0 THEN DO:  /*es una carpeta */
      vcMenu        = pcMenu + "\" + par_menu_grupos.dato_menu.
      viItemPrimero = INTEGER(par_menu_grupos.accion_seleccion).
      vcItem        = STRING(viItemPrimero, "999").
    END.      
    ELSE DO:  /*es un programa*/
      viItemPrimero = piItemPrimero.
    /*  
     DEFINE VAR dbg AS LOGICAL.
     dbg = DEBUGGER:INITIATE().
     dbg = DEBUGGER:SET-BREAK().
    */


      RUN createProgressShortCut IN vhLib ({&WINDOW-NAME}, 
                                           "\" + vcMenu + "\", 
                                           par_menu_grupos.dato_menu,
                                           par_menu_grupos.accion_seleccion_gui).
    END.
      
      
  END.
  IF par_menu_grupos.ITEM_posterior = STRING(piItemPrimero, "999") THEN DO:  /*recorrio todos los hijos*/
    RETURN pcMenu.  
  END.
  ELSE DO:
    /*
    DEFINE VAR dbg AS LOGICAL.
    dbg = DEBUGGER:INITIATE().
    dbg = DEBUGGER:SET-BREAK().
    */
    c = exploreMenu(pcLetraInicial, vcItem, viItemPrimero, INPUT-OUTPUT vcMenu).
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPhater wWin 
FUNCTION getPhater RETURNS CHARACTER
  (INPUT pcLetra        AS CHARACTER, 
   INPUT pcItem         AS CHARACTER,
   INPUT-OUTPUT pcPath  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  dado un nodo cualquiera busca las carpetas de las que cuelga hasta 
            llegar al primer nivel 
    Notes:  
------------------------------------------------------------------------------*/
  
  DEFINE VARIABLE i      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vcItem AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE c      AS CHARACTER  NO-UNDO.
  DEFINE BUFFER pm FOR par_menu_grupos.
  /*
  DEFINE VAR dbg AS LOGICAL.
  dbg = DEBUGGER:INITIATE().
  dbg = DEBUGGER:SET-BREAK().
  */
  /*busco el item*/
  FIND FIRST par_menu_grupos WHERE par_menu_grupos.letra_inicial = pcLetra
                               AND par_menu_grupo.ITEM_menu      = pcItem
                             NO-LOCK NO-ERROR.
  /*armo condicion de parada*/
  i = INTEGER(par_menu_grupos.accion_seleccion) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR AND i <> 0 THEN  DO: /*es una carpeta */
    /*pcPath    = pcPath + " - " + par_menu_grupos.dato_menu.*/
  END.
  IF NOT ERROR-STATUS:ERROR AND i <> 0 AND INTEGER(par_menu_grupos.ITEM_menu) <= 10 THEN DO:  
    RETURN pcPath.
  END.
  vcItem = par_menu_grupos.ITEM_anterior.
  IF AVAILABLE par_menu_grupos THEN DO:
    /*busco un item cuyo valor de accion_seleccion_gui = item_menu, es decir, la carpeta padre*/
    FIND FIRST pm WHERE pm.letra_inicial        = par_menu_grupos.letra_inicial
                    AND pm.accion_seleccion_gui = par_menu_grupos.ITEM_menu
                  NO-LOCK NO-ERROR.
    IF AVAILABLE pm THEN DO: /*es la carpeta padre*/
      pcPath    = pm.dato_menu + "\" + pcPath.
      vcItem    = pm.ITEM_anterior.
    END.
    c = getPhater(pcLetra, vcItem, pcPath).
  END.
  ELSE
    RETURN "Error".

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

