&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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

/*define input parameter usuario as character.*/

/* Local Variable Definitions ---                                       */
define var chtree as com-handle.

define var chListV as com-handle.

DEFINE VARIABLE chGraph AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chChart AS COM-HANDLE     NO-UNDO.

define temp-table nodos 
    field item as character format '999'
    index i as primary unique 
    item.
    
define temp-table hijos 
    field item as character format '999'
    index i as primary unique 
    item.
define var cadena as character format 'x(150)'.
define var h_programa as handle.
define var v_ancho as decimal.
define var v_alto as decimal .

define new shared var vlc_pinicio as handle.

vlc_pinicio = this-procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE CustomMenu
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME frame-principal

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-6 BUTTON-5 BUTTON-7 BUTTON-8 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-8 FILL-IN-15 FILL-IN-7 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD busca-anterior W-Win 
FUNCTION busca-anterior RETURNS CHARACTER
  ( input  cta as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD devuelve-item W-Win 
FUNCTION devuelve-item RETURNS ROWID
  ( input clave as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-4 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-4 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-5 
     LABEL ">>" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-6 
     LABEL "<<" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-7 
     LABEL "&Salir" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-8 
     LABEL "Button 8" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FILL-IN-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 148 BY 9.05
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 32 BY 1.43
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 57 BY 1.48
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1.48
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17 BY 1.48
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 57 BY 9.52
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 83 BY 1.19
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 13 BY 1.48
     BGCOLOR 8  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-principal
     FILL-IN-3 AT ROW 1.24 COL 2 NO-LABEL DEBLANK 
     FILL-IN-4 AT ROW 1.24 COL 35 NO-LABEL
     FILL-IN-5 AT ROW 1.24 COL 93 NO-LABEL
     FILL-IN-6 AT ROW 1.24 COL 118 NO-LABEL
     FILL-IN-9 AT ROW 1.24 COL 136 NO-LABEL
     BUTTON-6 AT ROW 2.91 COL 2
     BUTTON-5 AT ROW 2.91 COL 17
     BUTTON-7 AT ROW 2.91 COL 32
     BUTTON-8 AT ROW 2.91 COL 49
     FILL-IN-8 AT ROW 3.14 COL 65 COLON-ALIGNED NO-LABEL
     FILL-IN-15 AT ROW 4.57 COL 2 NO-LABEL
     FILL-IN-7 AT ROW 4.57 COL 91 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 163.4 BY 28.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: CustomMenu
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "SIE - Sistema Informatico para Empresas"
         HEIGHT             = 28.57
         WIDTH              = 149.4
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 163.4
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 163.4
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frame-principal
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-15 IN FRAME frame-principal
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME frame-principal
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME frame-principal
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME frame-principal
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME frame-principal
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME frame-principal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME frame-principal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME frame-principal
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME frame-principal:HANDLE
       ROW             = 4.57
       COLUMN          = 2
       HEIGHT          = 8.33
       WIDTH           = 90
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME frame-principal:HANDLE
       ROW             = 13.86
       COLUMN          = 2
       HEIGHT          = 15.71
       WIDTH           = 68
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-4 ASSIGN
       FRAME           = FRAME frame-principal:HANDLE
       ROW             = 14.33
       COLUMN          = 78
       HEIGHT          = 13.1
       WIDTH           = 69
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME frame-principal:HANDLE
       ROW             = 25.05
       COLUMN          = 67
       HEIGHT          = 1.81
       WIDTH           = 7.6
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {6C14B5B4-8A97-475B-A8D7-55D9275B2C55} type: Draw */
      CtrlFrame-4:NAME = "CtrlFrame-4":U .
/* CtrlFrame-4 OCXINFO:CREATE-CONTROL from: {827E9F50-96A4-11CF-823E-000021570103} type: Graph */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      CtrlFrame:MOVE-AFTER(FILL-IN-8:HANDLE IN FRAME frame-principal).
      CtrlFrame-3:MOVE-AFTER(FILL-IN-7:HANDLE IN FRAME frame-principal).
      CtrlFrame-4:MOVE-AFTER(CtrlFrame-3).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame-4).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* SIE - Sistema Informatico para Empresas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* SIE - Sistema Informatico para Empresas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  if valid-handle(h_programa) then
            run dispatch in h_programa ('destroy').
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Win
ON CHOOSE OF BUTTON-5 IN FRAME frame-principal /* >> */
DO:
      ctrlFrame:width-chars = ctrlFrame:width-chars + 1.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 W-Win
ON CHOOSE OF BUTTON-6 IN FRAME frame-principal /* << */
DO:
   if ctrlFrame:width-chars > 1 Then
   ctrlFrame:width-chars = ctrlFrame:width-chars - 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 W-Win
ON CHOOSE OF BUTTON-7 IN FRAME frame-principal /* Salir */
DO:
  apply 'window-close' to current-window.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 W-Win
ON CHOOSE OF BUTTON-8 IN FRAME frame-principal /* Button 8 */
DO:
  DEFINE VARIABLE chNode AS COM-HANDLE     NO-UNDO.

  chNode = chTree:Nodes:ADD().
  chNode:TEXT = "Primer Nodo".


    DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  
  chGraph:GraphType = 0.
  chGraph:ShowLabel = TRUE.
  chGraph:ShowNumbers = TRUE.
  chGraph:ShowPercent = TRUE.
  chGraph:ShowLegend = TRUE.
  chGraph:UseRNDColor = TRUE.
  DO i = 1 TO 5:
    chGraph:AddData ("Item" + STRING(i), STRING(i), i - 1).
  END.
  chGraph:DrawGraph.

  chChart:GraphTitle = "Funciona que te pario".
  chChart:DrawMode = 3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame W-Win OCX.Collapse
PROCEDURE CtrlFrame.TreeView.Collapse .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.

p-node:Image = 'cerrado'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame W-Win OCX.Expand
PROCEDURE CtrlFrame.TreeView.Expand .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.


p-node:Image = 'abierto'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame W-Win OCX.KeyDown
PROCEDURE CtrlFrame.TreeView.KeyDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.

message keyfunction(p-keycode) p-shift view-as alert-box.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame W-Win OCX.NodeClick
PROCEDURE CtrlFrame.TreeView.NodeClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.
define var iprograma as integer.
define var rrowid as rowid.
define var h as handle.
define var nombre_programa as character.
/*
if substring(p-node:key,1,3) = 'CGM' and p-node:children = 0 then
   run carga-parcial-item ( substring(p-node:key,4,length(p-node:key) - 3 ), substring(p-node:key,2,length(p-node:key) - 1 )).
else
    if p-node:children = 0 then
    do:
        rrowid = to-rowid(substring(p-node:key,2,length(p-node:key) - 1)).

        find par_menu_grupos where rowid(par_menu_grupos) = rrowid no-lock.
        nombre_programa = par_menu_grupos.accion_seleccion_gui.
        find first par_permisos where par_permisos.nombre_programa = 
                   par_menu_grupos.accion_seleccion_gui no-lock no-error.
  
        if available par_permisos then
        do:
            if not can-do(trim(replace(puede_ejecutar,' ', '')) , usuario) then
            do:
                message 'Ud. no esta autorizado para ejecutar esta opcion'
                view-as alert-box error.
                return.    
            end.
        end.
        else
        do:
            message 'Ud. no esta autorizado para ejecutar esta opcion'
            view-as alert-box error.
            return.    
        end.    
  
        if valid-handle(h_programa) then
            run dispatch in h_programa ('destroy').
        cadena = nombre_programa + ' - ' + mensaje_menu.
        run crea-texto ( fill-in-8:handle in frame {&FRAME-NAME},cadena).    
        if nombre_programa begins 'f' then
        do:
            RUN init-object IN THIS-PROCEDURE (
             INPUT  nombre_programa ,
             INPUT  FRAME frame-principal:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_programa ) no-error.

            RUN get-size IN h_programa ( output v_ancho , output v_alto ) no-error.
            if v_ancho > 85 then
            do:
                self:visible = false.
                fill-in-7:visible in frame {&FRAME-NAME} = false.
                fill-in-15:visible in frame {&FRAME-NAME} = true.
                RUN set-position IN h_programa ( 5.1 , 2.5 ) NO-ERROR no-error.
            end.    
            else
            do:
                self:visible = true.
                fill-in-7:visible in frame {&FRAME-NAME} = true.
                fill-in-15:visible in frame {&FRAME-NAME} = false.
                RUN set-position IN h_programa ( 5.1 , 65  ) NO-ERROR.
            end.

           /* Size in UIB:  ( 10.24 , 80.00 ) */
            run dispatch in h_programa ('initialize') .
        end.
        else
            run value(nombre_programa) no-error.
    end.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

{src/adm/template/windowmn.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-parcial-item W-Win 
PROCEDURE carga-parcial-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input parameter p-grupo as character no-undo.
  define input parameter ref as character no-undo.
  
  define var c1 as character no-undo.
  define buffer pm for par_menu_grupos.
  define var item-old as integer .
  define var itm as character.
  define var v_accion as integer.
  define buffer pm1 for par_menu_grupos.
  define buffer pm2 for par_menu_grupos.
  define var icono as character.
  
  find first par_menu_grupos where par_menu_grupos.letra_inicial = p-grupo no-lock no-error.
  if not available par_menu_grupos then next.

  find pm where rowid(pm) = rowid(par_menu_grupos).

  repeat:
    find first par_menu_usuarios where par_menu_usuarios.usuario = 
              p-grupo + '_' + usuario and 
              par_menu_usuarios.item_invalido = pm.item_menu no-lock no-error.
   
    if not available par_menu_usuarios then
    do:
     if pm.accion_seleccion_gui <> '' then 
     do:   
        create nodos.
        assign nodos.item = pm.item_menu.

        v_accion = integer(pm.accion_seleccion) no-error.
        if not error-status:error and v_accion <> 0 then
            icono = 'cerrado'.
        else
            icono = 'programa'.
        

        run crea-item-tree(ref , 4 ,string(rowid(pm)),pm.dato_menu ,icono).
      end.  
    end.
    itm = pm.item_posterior.
    find pm where pm.letra_inicial = par_menu_grupos.letra_inicial and
                  pm.item_menu = itm no-lock no-error.
    if rowid(pm) = rowid(par_menu_grupos) then leave.    
  end. 
  
repeat:
  for each nodos :
        find pm where pm.letra_inicial = par_menu_grupos.letra_inicial and
                      pm.item_menu = nodos.item.
        v_accion = integer(pm.accion_seleccion) no-error.
        if not error-status:error and v_accion <> 0 then
        do:
            find pm1 where pm1.letra_inicial = par_menu_grupos.letra_inicial and 
                           pm1.item_menu = string(v_accion,'999').
            find pm2 where rowid(pm2) = rowid(pm1).               
            repeat:
                find first par_menu_usuarios where par_menu_usuarios.usuario = 
                      p-grupo + '_' + usuario and 
                      par_menu_usuarios.item_invalido = pm2.item_menu no-lock no-error.
                if not available par_menu_usuarios then
                do:
                if pm2.accion_seleccion_gui <> '' then
                do:
                    create hijos.
                    assign hijos.item = pm2.item_menu.
                    v_accion = integer(pm2.accion_seleccion) no-error.
                    if not error-status:error and v_accion <> 0 then
                        icono = 'cerrado'.
                    else
                        icono = 'programa'.
                    run crea-item-tree(string(rowid(pm)) , 4 ,string(rowid(pm2)),pm2.dato_menu ,icono).
                end.
                end.    
                itm = pm2.item_posterior.
                find pm2 where pm2.letra_inicial = par_menu_grupos.letra_inicial and
                          pm2.item_menu = itm no-lock no-error.
                if rowid(pm2) = rowid(pm1) then leave.    
            end.  
        end.              
    end.
    for each nodos:
        delete nodos.
    end.
    find first hijos no-error .
    if not available hijos then leave.
    for each hijos :
        create nodos.
        assign nodos.item = hijos.item.
        delete hijos.
    end.
    
end.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load W-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "wocx.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
    chCtrlFrame-4 = CtrlFrame-4:COM-HANDLE
    UIB_S = chCtrlFrame-4:LoadControls( OCXFile, "CtrlFrame-4":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "wocx.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea-item-tree W-Win 
PROCEDURE crea-item-tree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter ref as character.
define input parameter tipo as character. /* 1 last 2 next 3 previo 4 hijo */
define input parameter clave as character .
define input parameter nombre as character.
define input parameter icono as character.

define var h as com-handle.

clave = 'C' + clave.

if ref = '' then
do:
    if tipo = '' then
        h = chtree:nodes:add(  ,   , clave , nombre, icono ).
    else
        h = chtree:nodes:add(  , tipo  , clave , nombre, icono ).
end.
else
do:
    ref = 'C' + ref.
    if tipo = '' then
        h = chtree:nodes:add( ref  ,   , clave , nombre, icono ).
    else
        h = chtree:nodes:add( ref  , tipo  , clave , nombre, icono ).

end.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea-objeto W-Win 
PROCEDURE crea-objeto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter nombre_programa as character no-undo.


    RUN init-object IN THIS-PROCEDURE (
             INPUT  nombre_programa ,
             INPUT  FRAME {&FRAME-NAME}:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_programa ) no-error.

    RUN get-size IN h_programa ( output v_ancho , output v_alto ) no-error.
    if v_ancho > 85 then
    do:
        ctrlFrame:hidden = true.
        fill-in-7:visible in frame {&FRAME-NAME} = false.
        fill-in-15:visible in frame {&FRAME-NAME} = true.
        RUN set-position IN h_programa ( 5.1 , 2.5 ) NO-ERROR .
    end.    
    else
    do:
        ctrlFrame:hidden = true.
        fill-in-7:visible in frame {&FRAME-NAME} = true.
        fill-in-15:visible in frame {&FRAME-NAME} = false.
        RUN set-position IN h_programa ( 5.1 , 65  ) NO-ERROR.
    end.

           /* Size in UIB:  ( 10.24 , 80.00 ) */
    run dispatch in h_programa ('initialize').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea-texto W-Win 
PROCEDURE crea-texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter fillhdl as handle.
define input parameter texto as character format 'x(15)'.

  define var t1 as handle.
  create text t1
    assign 
           column = fillhdl:column + 1
           row = fillhdl:row + 0.2
           width = fillhdl:width - 2
           height = fillhdl:height - 0.4
           visible = true
           frame = frame {&FRAME-NAME}:handle
           sensitive = true
           font = 6.

  t1:format = 'x(150)'.
  t1:auto-resize = true.
  t1:visible = true.
  t1:screen-value = texto.
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
  DISPLAY FILL-IN-8 FILL-IN-15 FILL-IN-7 
      WITH FRAME frame-principal IN WINDOW W-Win.
  ENABLE BUTTON-6 BUTTON-5 BUTTON-7 BUTTON-8 
      WITH FRAME frame-principal IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-frame-principal}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE estado-inicial W-Win 
PROCEDURE estado-inicial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN get-size IN h_programa ( output v_ancho , output v_alto ) .
if v_ancho > 85 then
do:
    ctrlframe:visible = true.
    fill-in-7:visible in frame {&FRAME-NAME} = true.
    fill-in-15:visible in frame {&FRAME-NAME} = false.
end.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls W-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
chtree = chctrlframe:TreeView.
chListV = chctrlframe-2:ImageList.
chtree:ImageList = chListV. 
chGraph = chctrlframe-3:Draw.
chChart = chctrlframe-4:Graph.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-tree W-Win 
PROCEDURE load-tree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define var c1 as character no-undo.
  define buffer pm for par_menu_grupos.
  define var item-old as integer .
  define var itm as character.
  define var v_accion as integer.
  define buffer pm1 for par_menu_grupos.
  define buffer pm2 for par_menu_grupos.
  define var icono as character.
  define var modulo as character.

/*
for each par_usuarios_logueo where par_usuarios_logueo.id_usuario = usuario no-lock:
    for each par_grupos where par_grupos.nombre_grupo matches par_usuarios_logueo.modulo :
      find first par_menu_grupos where par_menu_grupos.letra_inicial = par_grupos.letra_inicial no-lock no-error.
      if not available par_menu_grupos then next.
      run crea-item-tree('' ,'' , 'GM' + par_grupos.letra_inicial , par_grupos.nombre_grupo ,'cerrado').
    end.
end.    
*/
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
  find first par_usuario no-lock.
  cadena = par_usuario.nombre_comercial.
  run crea-texto(fill-in-3:handle in frame {&FRAME-NAME} , cadena).
  cadena = string(today,'99/99/9999').
  run crea-texto(fill-in-6:handle in frame {&FRAME-NAME} , cadena).
  cadena = string(time,'hh:mm:ss').
  run crea-texto(fill-in-9:handle in frame {&FRAME-NAME} , cadena).
  cadena = string(userid('userdb')).
  run crea-texto(fill-in-5:handle in frame {&FRAME-NAME} , cadena).

  /* Code placed here will execute AFTER standard behavior.    */
  run dispatch ('load-tree').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE salir-programa W-Win 
PROCEDURE salir-programa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if valid-handle(h_programa) then delete procedure h_programa.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this CustomMenu, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION busca-anterior W-Win 
FUNCTION busca-anterior RETURNS CHARACTER
  ( input  cta as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

define var c1 as character no-undo.
define var i as integer no-undo.

do i = length(c1) to 1 by -1 :
        if substring(c1,i,1) <> "0" then
          leave.
end.


if i > 1 then
    c1 = substring(c1, 1 , i - 2 ).


if i = 1 then
    return ?.
else
    return (c1 + fill('0',8 - length(c1) + 1)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION devuelve-item W-Win 
FUNCTION devuelve-item RETURNS ROWID
  ( input clave as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

