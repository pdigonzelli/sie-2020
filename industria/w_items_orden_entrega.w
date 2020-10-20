&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
DEFINE INPUT PARAMETER p_r_oe AS ROWID.

/* Local Variable Definitions ---                                       */
define var alta as logical no-undo initial false.
DEFINE VAR v_oe AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-16 BUTTON-1 BUTTON-19 RECT-4 RECT-8 ~
RECT-9 
&Scoped-Define DISPLAYED-OBJECTS total_tambores total_factura total_kilos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_gastos_items_orden_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_items_orden_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cfolder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_items_orden_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_items_orden_entrega_1 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Actualizar" 
     SIZE 33 BY 1.91.

DEFINE BUTTON BUTTON-16  NO-FOCUS
     LABEL "Enviar Mail" 
     SIZE 33 BY 1.91.

DEFINE BUTTON BUTTON-19 
     LABEL "Recalculo Manual" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE total_factura AS DECIMAL FORMAT "->,>>>,>>9.999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE total_kilos AS DECIMAL FORMAT "->,>>>,>>9.999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE total_tambores AS INTEGER FORMAT "->>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 14 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 2.38.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36 BY 2.38.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-16 AT ROW 1.24 COL 4
     BUTTON-1 AT ROW 1.24 COL 122
     total_tambores AT ROW 10.52 COL 92 NO-LABEL
     total_factura AT ROW 10.52 COL 122 RIGHT-ALIGNED NO-LABEL
     total_kilos AT ROW 10.52 COL 138 RIGHT-ALIGNED NO-LABEL
     BUTTON-19 AT ROW 10.52 COL 139
     RECT-4 AT ROW 1 COL 39
     RECT-8 AT ROW 1 COL 2
     RECT-9 AT ROW 1 COL 120
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.4 BY 23.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Partes de Orden de Entrega"
         HEIGHT             = 23.48
         WIDTH              = 156.4
         MAX-HEIGHT         = 24.38
         MAX-WIDTH          = 156.4
         VIRTUAL-HEIGHT     = 24.38
         VIRTUAL-WIDTH      = 156.4
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
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN total_factura IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       total_factura:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN total_kilos IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       total_kilos:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN total_tambores IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       total_tambores:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Partes de Orden de Entrega */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Partes de Orden de Entrega */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Actualizar */
DO:
  run recalcular-datos in h_v_items_orden_entrega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 W-Win
ON CHOOSE OF BUTTON-16 IN FRAME F-Main /* Enviar Mail */
DO:
    define var r as rowid.
    run get-rowid1 in h_b_items_orden_entrega (output r).
  run p_correo_aviso_contrato_item_oe.p (input 2, input r, input "creado o modificado").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 W-Win
ON CHOOSE OF BUTTON-19 IN FRAME F-Main /* Recalculo Manual */
DO:
  define var r as rowid.
  DEFINE VARIABLE v_tipo_cambio AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE v_comision AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE v_coef AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE v_derecho_aduana AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE v_fob_ton AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE v_reintegro_aduana AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE v_derecho AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE v_reintegro AS DECIMAL    NO-UNDO.




  run get-rowid1 in h_b_items_orden_entrega (output r).
  FIND FIRST items_orden_entrega WHERE ROWID(items_orden_entrega) = r NO-LOCK NO-ERROR.
  IF AVAILABLE items_orden_entrega THEN  DO:

    RUN carga-fob IN h_v_items_orden_entrega (items_orden_entrega.id_orden_entrega,
                                              items_orden_entrega.ITEM_oe).
/*
    /**derechos y reintegros **/
    items_orden_entrega.coeficiente                   = DECIMAL(items_orden_entrega.coeficiente:SCREEN-VALUE IN FRAME F-Main).
    items_orden_entrega.valor_aduana_derechos         = DECIMAL(items_orden_entrega.valor_aduana_derechos:SCREEN-VALUE IN FRAME F-Main).
    items_orden_entrega.valor_aduana_reintegro        = DECIMAL(items_orden_entrega.valor_aduana_reintegro:SCREEN-VALUE IN FRAME F-Main).
    items_orden_entrega.importe_derechos_exportacion  = DECIMAL(items_orden_entrega.importe_derechos_exportacion:SCREEN-VALUE IN FRAME F-Main).
    items_orden_entrega.importe_reintegro_fijo        = DECIMAL(items_orden_entrega.importe_reintegro_fijo:SCREEN-VALUE IN FRAME F-Main).
*/    
    /*
    items_orden_entrega.tipo_cambio                   = INTEGER(items_orden_entrega.tipo_cambio:SCREEN-VALUE IN FRAME F-Main)
    items_orden_entrega.importe_factura_dolar         = DECIMAL(items_orden_entrega.importe_factura_dolar)
    */

    
    /****/
  END.

  
  MESSAGE "Recalculo Realizado" SKIP "Recuerde No Grabar" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/method/winitialize.i}
hide current-window.
run deshabilita-viewer-paginas.
run habilita-relacion-viewer-pagina.
{custom/method/ctitulo.i}
view current-window.
run select-page(1).

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
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav ).
       RUN set-position IN h_cus-updsav ( 1.24 , 40.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.24 , 74.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 23.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 96.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 20.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_items_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_items_orden_entrega ).
       RUN set-position IN h_b_items_orden_entrega ( 3.62 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b_items_orden_entrega ( 6.71 , 153.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/template/cfolder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Principal|Varios|Gastos' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_cfolder ).
       RUN set-position IN h_cfolder ( 11.71 , 1.00 ) NO-ERROR.
       RUN set-size IN h_cfolder ( 12.62 , 156.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_items_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_items_orden_entrega ).

       /* Links to SmartFolder h_cfolder. */
       RUN add-link IN adm-broker-hdl ( h_cfolder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_items_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_items_orden_entrega ).
       RUN set-position IN h_v_items_orden_entrega ( 13.14 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.00 , 150.00 ) */

       /* Links to SmartViewer h_v_items_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_b_items_orden_entrega , 'Record':U , h_v_items_orden_entrega ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_items_orden_entrega ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_items_orden_entrega_1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_items_orden_entrega_1 ).
       RUN set-position IN h_v_items_orden_entrega_1 ( 14.33 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.86 , 134.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_v_items_orden_entrega_1. */
       RUN add-link IN adm-broker-hdl ( h_b_items_orden_entrega , 'Record':U , h_v_items_orden_entrega_1 ).
       RUN add-link IN adm-broker-hdl ( h_v_items_orden_entrega , 'group-assign':U , h_v_items_orden_entrega_1 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-2 ).
       RUN set-position IN h_cus-updsav-2 ( 13.14 , 6.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_gastos_items_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_gastos_items_orden_entrega ).
       RUN set-position IN h_b_gastos_items_orden_entrega ( 15.05 , 7.00 ) NO-ERROR.
       RUN set-size IN h_b_gastos_items_orden_entrega ( 6.71 , 80.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_gastos_items_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_b_items_orden_entrega , 'Record':U , h_b_gastos_items_orden_entrega ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_b_gastos_items_orden_entrega ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo-totales W-Win 
PROCEDURE calculo-totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR v_total_tambores AS INTEGER.
DEFINE VAR v_total_factura AS DECIMAL.
DEFINE VAR v_total_kilos AS DECIMAL.
DEFINE BUFFER b_oe FOR orden_entrega.

FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = p_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK.
        v_total_tambores = v_total_tambores + items_orden_entrega.cantidad_tambores.
        v_total_factura  = v_total_factura  + items_orden_entrega.total_factura.
        v_total_kilos    = v_total_kilos    + items_orden_entrega.kgs_netos_tambores.
    END.
END.

total_tambores:SCREEN-VALUE IN FRAME F-Main = STRING(v_total_tambores).
total_factura:SCREEN-VALUE IN FRAME F-Main = STRING(v_total_factura).
total_kilos:SCREEN-VALUE IN FRAME F-Main = STRING(v_total_kilos).

FIND FIRST b_oe WHERE ROWID(b_oe) = p_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE b_oe THEN DO:
    IF b_oe.id_tipo_orden_entrega = 2 THEN DO:
        button-1:HIDDEN IN FRAME F-Main = TRUE.
    END.
    ELSE button-1:HIDDEN IN FRAME F-Main = FALSE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar-datos-moneda W-Win 
PROCEDURE cargar-datos-moneda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_id_mon AS INTEGER.
DEFINE INPUT PARAMETER p_id_tip AS INTEGER.
DEFINE INPUT PARAMETER p_imp AS DECIMAL.

RUN cargar-datos-moneda IN h_v_items_orden_entrega_1 (INPUT p_id_mon,
                                                      INPUT p_id_tip,
                                                      INPUT p_imp).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame_datos_orden W-Win 
PROCEDURE dame_datos_orden :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_r AS ROWID.
RUN get-rowid1 IN h_b_items_orden_entrega (OUTPUT p_r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita-viewer-paginas W-Win 
PROCEDURE deshabilita-viewer-paginas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var ch as character no-undo.
define var h as handle no-undo.
define var nro-paginas as integer no-undo.
define var i as integer no-undo.

run get-link-handle in adm-broker-hdl ( input this-procedure ,
                                    input "PAGE-SOURCE" ,
                                    OUTPUT ch). 
if ch <> "" then
do:
    h=widget-handle(ch).
    run nro-paginas in h ( output nro-paginas).
    do i = 1 to nro-paginas :
        run select-page(i).
        run deshabilita_viewer.
    end.
end.        
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
  DISPLAY total_tambores total_factura total_kilos 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-16 BUTTON-1 BUTTON-19 RECT-4 RECT-8 RECT-9 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-oe W-Win 
PROCEDURE get-oe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_oe AS INTEGER.
FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = p_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN
    p_oe = orden_entrega.id_orden_entrega.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-item-oe W-Win 
PROCEDURE get-rowid-item-oe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_r AS ROWID.

RUN GET-rowid1 IN h_b_items_orden_entrega (OUTPUT p_r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-oe W-Win 
PROCEDURE get-rowid-oe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_rowid_oe AS ROWID.
p_rowid_oe = p_r_oe.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tipo-oe W-Win 
PROCEDURE get-tipo-oe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_oe AS INTEGER.
FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = p_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN
    p_oe = orden_entrega.id_tipo_orden_entrega.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-relacion-viewer-pagina W-Win 
PROCEDURE habilita-relacion-viewer-pagina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var ch as character no-undo.
define var h as handle no-undo.
define var nro-paginas as integer no-undo.
define var i as integer no-undo.

run get-link-handle in adm-broker-hdl ( input this-procedure ,
                                    input "PAGE-SOURCE" ,
                                    OUTPUT ch). 
if ch <> "" then
do:
    h=widget-handle(ch).
    run nro-paginas in h ( output nro-paginas).
    do i = 1 to nro-paginas :
        run select-page(i).
        run habilitar_relacion_viewer.
    end.
end.        

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
DEFINE VAR r_oe AS ROWID NO-UNDO.
define var v_oe as integer.
DEFINE VAR v_item_oe AS INTEGER.
define var v_filtro as character.
define var v_fecha as character.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
define var v_importe_gasto as decimal.

run devuelve-rowid (output r).
RUN GET-rowid-oe (OUTPUT r_oe).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

find items_orden_entrega where rowid(items_orden_entrega) = r no-lock no-error.
if available items_orden_entrega then
DO:
    v_oe = items_orden_entrega.id_orden_entrega.
    v_item_oe = items_orden_entrega.ITEM_oe.
END.
ELSE DO:
    v_oe = 0.
    v_item_oe = 0.
END.                

/*lista_reportes = "Orden Entrega".*/

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    do:
        run custom/support/cfun.w(input lista_reportes,output cresult).
        case cresult:
/*            when "Orden Entrega" then 
                do:
                    run p_reportes_9.p (input "items_orden_entrega_fax",
                                      input "Reporte de Orden de Entregas",
                                      input "orden_entrega.id_orden_entrega = " + string(v_oe),
                                      input "").                    
                end.
            when "Orden Entrega" then 
                do:
                    RUN p_rep_oe_fax.p (INPUT r_oe).
                    run p_reportes_9.p (input "new_orden_entrega_fax",
                                      input "Reporte de Orden de Entregas",
                                      input "",
                                      input "").                    
                end.*/
       end case.
    end.
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

FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = p_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    FIND FIRST items_orden_entrega OF orden_entrega
                                   WHERE ITEM_oe = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE items_orden_entrega THEN DO:
        FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
        IF AVAILABLE items_contratos THEN DO:
            IF items_contratos.id_destino <> orden_entrega.id_destino THEN DO:
                MESSAGE "No coinciden el destino de esta OE con la del contrato"
                    VIEW-AS ALERT-BOX.
                MESSAGE "El destino del contrato es " items_contrato.id_destino 
                    VIEW-AS ALERT-BOX.
            END.
                
        END.
    END.
END.


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
  
  RUN calculo-totales.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  RUN calculo-totales.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresca-browser-gastos W-Win 
PROCEDURE refresca-browser-gastos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run dispatch in h_b_gastos_items_orden_entrega ('open-query':U) NO-ERROR.

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
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

