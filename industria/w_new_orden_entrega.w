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
/* Local Variable Definitions ---                                       */
define var alta as logical no-undo initial false.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-18 RECT-2 RECT-8 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_gastos_orden_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_new_orden_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_new_orden_entrega AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-18 
     LABEL "Partes" 
     SIZE 18 BY 1.43.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 399 BY 50
     BGCOLOR 7 FGCOLOR 7 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 118 BY 10.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-18 AT ROW 1.48 COL 107
     RECT-2 AT Y 0 X 110
     RECT-8 AT ROW 10.76 COL 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.2 BY 26.38.


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
         TITLE              = "Orden Entrega"
         HEIGHT             = 26.38
         WIDTH              = 127.2
         MAX-HEIGHT         = 27.33
         MAX-WIDTH          = 137.4
         VIRTUAL-HEIGHT     = 27.33
         VIRTUAL-WIDTH      = 137.4
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Orden Entrega */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Orden Entrega */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 W-Win
ON CHOOSE OF BUTTON-18 IN FRAME F-Main /* Partes */
DO:
  DEFINE VARIABLE v_r_oe AS ROWID.
  RUN GET-rowid1 IN h_b_new_orden_entrega (OUTPUT v_r_oe).
  RUN w_items_orden_entrega.w (INPUT v_r_oe).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
       RUN set-position IN h_cus-updsav ( 1.24 , 23.80 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.24 , 57.80 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 24.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 80.80 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_new_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_new_orden_entrega ).
       RUN set-position IN h_b_new_orden_entrega ( 3.62 , 5.00 ) NO-ERROR.
       RUN set-size IN h_b_new_orden_entrega ( 6.91 , 118.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_new_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_new_orden_entrega ).
       RUN set-position IN h_v_new_orden_entrega ( 11.00 , 16.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.86 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'c:/dlc83b/custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-2 ).
       RUN set-position IN h_cus-updsav-2 ( 21.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_gastos_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_gastos_orden_entrega ).
       RUN set-position IN h_b_gastos_orden_entrega ( 21.48 , 41.00 ) NO-ERROR.
       RUN set-size IN h_b_gastos_orden_entrega ( 5.71 , 87.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_new_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_new_orden_entrega ).

       /* Links to SmartViewer h_v_new_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_b_new_orden_entrega , 'Record':U , h_v_new_orden_entrega ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_new_orden_entrega ).

       /* Links to csmartbrowser h_b_gastos_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_b_new_orden_entrega , 'record':U , h_b_gastos_orden_entrega ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_b_gastos_orden_entrega ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-updsav ,
             BUTTON-18:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-navico ,
             h_cus-updsav , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             h_cus-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_new_orden_entrega ,
             BUTTON-18:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v_new_orden_entrega ,
             h_b_new_orden_entrega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-updsav-2 ,
             h_v_new_orden_entrega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_gastos_orden_entrega ,
             h_cus-updsav-2 , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame_datos_orden W-Win 
PROCEDURE dame_datos_orden :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_orden_entrega AS ROWID.
RUN GET-rowid1 IN h_b_new_orden_entrega (OUTPUT p_orden_entrega).
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
  ENABLE BUTTON-18 RECT-2 RECT-8 
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
define var v_oe as integer.
define var v_filtro as character.
define var v_fecha as character.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
define var v_importe_gasto as decimal.
DEFINE VAR v_fecha_desde AS DATE.
DEFINE VAR v_fecha_hasta AS DATE.
DEFINE VAR v_clientes AS CHAR.
DEFINE VAR v_productos AS CHAR.
DEFINE VAR v_clausula AS CHAR.
DEFINE VAR v_contenedores AS DECIMAL.
DEFINE VAR v_semana AS INTEGER.
DEFINE VAR v_anio AS INTEGER.
DEFINE VAR v_importe_coma AS CHAR.



run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    
FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = r NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN v_oe = orden_entrega.id_orden_entrega.

lista_reportes = "Orden Entrega,Listado contenedores,Listado contenedores entre fecha,Rep Pedidos Pendientes,Pedido de fondos,Pedido fondos(Local),Pedido fondos(Int),Exportacion Excell".

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    do:
        run custom/support/cfun.w(input lista_reportes,output cresult).
        
        case cresult:
            when "Orden Entrega" then 
                do:
                    IF orden_entrega.id_tipo_orden_entrega = 1 THEN DO:
                        RUN p_rep_oe_fax.p (INPUT r).
                        run p_reportes_9.p (input "new_orden_entrega_fax",
                                            input "Reporte de Orden de Entregas",
                                            input "",
                                            input "").                    
                    END.
                    ELSE DO:
                        RUN p_rep_oe_fax.p (INPUT r).
                        RUN p_reportes_9.p (input "new_orden_entrega_fax_ff",
                                            input "Reporte de Orden de Entregas de FF",
                                            input "",
                                            input "").                    
                    END.
                    
                end.
            when "Listado contenedores" then 
                do:
                    
                    run p_reportes.p (input "oe_cont_destino_agencia",
                                      input "Reporte de contenedores usados en OE",
                                      input "year(orden_entrega.fecha) = 2002 and id_orden_entrega > 1000",
                                      input "").                    
                end.
           when "Listado contenedores entre fecha" then 
                do:
                    RUN wc_sel_rango_fecha.w (OUTPUT v_fecha_desde,
                                              OUTPUT v_fecha_hasta).
                    MESSAGE "Fechas " v_fecha_desde v_fecha_hasta VIEW-AS ALERT-BOX.

                    IF v_fecha_desde <> ? THEN DO:
                        run p_reportes.p (input "oe_cont_destino_agencia",
                                          input "Reporte de contenedores usados en OE",
                                          input "year(orden_entrega.fecha) = 2002 and id_orden_entrega > 1000" +
                                                " and orden_entrega.fecha >= date('" + STRING(v_fecha_desde) + "')" +
                                                " and orden_entrega.fecha <= date('" + STRING(v_fecha_hasta) + "')",
                                          input "").                    
                    END.
                    
                end.
           when "Rep Pedidos Pendientes" then 
                do:
                    RUN wc_sel_semana.w (OUTPUT v_semana,
                                         OUTPUT v_anio).

                    run p_reportes_9.p (input "oe_pend_ped_fondos",
                                        input "Reporte de Orden de Entregas",
                                        input "orden_entrega.semana_embarque >= " + STRING(v_semana) 
                                              + " and year(orden_entrega.fecha_embarque) = " 
                                              + STRING(v_anio) 
                                              + " and orden_entrega.pedido_fondos = false and orden_entrega.id_orden_entrega > 1000",
                                        input "").                    
                end.
           when "Pedido de fondos" then 
                do:
                    for each gastos_orden_entrega of orden_entrega no-lock.
                        if gastos_orden_entrega.id_gasto <> 3 /* DUTY DDP */ and
                           gastos_orden_entrega.id_gasto <> 11 /* SEGUROS */ and 
                           gastos_orden_entrega.id_gasto <> 12 /* VARIOS */ and
                           gastos_orden_entrega.id_gasto <> 27 /* DCR */ then
                           v_importe_gasto = v_importe_gasto + gastos_orden_entrega.importe.
                    end.
                    v_clientes = "".
                    FOR EACH items_orden_entrega OF orden_entrega
                                                 BREAK BY items_orden_entrega.id_cliente.
                        IF LAST-OF(items_orden_entrega.id_cliente) THEN DO:
                            FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
                            IF AVAILABLE clientes THEN DO:
                                v_clientes = v_clientes + clientes.nombre.
                            END.
                        END.
                    END.
                    v_productos = "".
                    FOR EACH items_orden_entrega OF orden_entrega
                                                 BREAK BY items_orden_entrega.id_articulo.
                        IF LAST-OF(items_orden_entrega.id_articulo) THEN DO:
                            FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
                            IF AVAILABLE productos_terminados THEN DO:
                                v_productos = v_productos + productos_terminados.abreviatura.
                            END.
                        END.
                    END.
                    FIND FIRST items_orden_entrega OF orden_entrega NO-LOCK NO-ERROR.
                    IF AVAILABLE items_orden_entrega THEN DO:
                        FIND FIRST clausulas WHERE clausulas.id_clausula = 
                                                   items_orden_entrega.id_condicion_venta
                                             NO-LOCK NO-ERROR.
                        IF AVAILABLE clausulas THEN DO:
                            v_clausula = clausulas.descripcion.
                        END.
                    END.
                    v_contenedores = 0.
                    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK .
                        v_contenedores = v_contenedores + items_orden_entrega.contenedores.
                    END.

                    v_importe_coma = STRING(ROUND(v_importe_gasto,3)).
                    /*v_importe_coma = REPLACE(v_importe_coma,".",",").*/
                    
                    run p_reportes_9.p (input "orden_entrega_ped_fondos",
                                      input "Reporte de Pedidos de Fondos",
                                      input "orden_entrega.id_orden_entrega = " + string(v_oe),
                                      input v_importe_coma + ";" +
                                            v_clientes + ";" +
                                            v_productos + ";" +
                                            v_clausula + ";" +
                                            string(v_contenedores) + ";").                    
                end.
            when "Pedido fondos(Local)" then 
                do:
                    for each gastos_orden_entrega of orden_entrega no-lock.
                        if gastos_orden_entrega.id_gasto = 19 /* GASTOS BL */ /* OR
                           gastos_orden_entrega.id_gasto = 15 /* HANDLING */ */ then
                           v_importe_gasto = v_importe_gasto + gastos_orden_entrega.importe.
                    end.
                    v_clientes = "".
                    FOR EACH items_orden_entrega OF orden_entrega
                                                 BREAK BY items_orden_entrega.id_cliente.
                        IF LAST-OF(items_orden_entrega.id_cliente) THEN DO:
                            FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
                            IF AVAILABLE clientes THEN DO:
                                v_clientes = v_clientes + clientes.nombre.
                            END.
                        END.
                    END.
                    v_productos = "".
                    FOR EACH items_orden_entrega OF orden_entrega
                                                 BREAK BY items_orden_entrega.id_articulo.
                        IF LAST-OF(items_orden_entrega.id_articulo) THEN DO:
                            FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
                            IF AVAILABLE productos_terminados THEN DO:
                                v_productos = v_productos + productos_terminados.abreviatura.
                            END.
                        END.
                    END.
                    FIND FIRST items_orden_entrega OF orden_entrega NO-LOCK NO-ERROR.
                    IF AVAILABLE items_orden_entrega THEN DO:
                        FIND FIRST clausulas WHERE clausulas.id_clausula = 
                                                   items_orden_entrega.id_condicion_venta
                                             NO-LOCK NO-ERROR.
                        IF AVAILABLE clausulas THEN DO:
                            v_clausula = clausulas.descripcion.
                        END.
                    END.
                    v_contenedores = 0.
                    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK .
                        v_contenedores = v_contenedores + items_orden_entrega.contenedores.
                    END.

                    v_importe_coma = STRING(v_importe_gasto).
                    v_importe_coma = REPLACE(v_importe_coma,".",",").

                    run p_reportes_9.p (input "orden_entrega_ped_fondos_local",
                                      input "Reporte de Pedidos de Fondos",
                                      input "orden_entrega.id_orden_entrega = " + string(v_oe),
                                      input v_importe_coma + ";" +
                                            v_clientes + ";" +
                                            v_productos + ";" +
                                            v_clausula + ";" +
                                            string(v_contenedores) + ";").                    
                end.
            when "Pedido fondos(Int)" then 
                do:
                    for each gastos_orden_entrega of orden_entrega no-lock.
                        if gastos_orden_entrega.id_gasto <> 3 /* DUTY DDP */ and
                           gastos_orden_entrega.id_gasto <> 11 /* SEGUROS */ and 
                           gastos_orden_entrega.id_gasto <> 12 /* VARIOS */ and
                           gastos_orden_entrega.id_gasto <> 19 /* GASTOS BL */ /* and
                           gastos_orden_entrega.id_gasto <> 15 /* HANDLING */ */ and
                           gastos_orden_entrega.id_gasto <> 27 /* DCR */ then
                           v_importe_gasto = v_importe_gasto + gastos_orden_entrega.importe.
                    end.
                    v_clientes = "".
                    FOR EACH items_orden_entrega OF orden_entrega
                                                 BREAK BY items_orden_entrega.id_cliente.
                        IF LAST-OF(items_orden_entrega.id_cliente) THEN DO:
                            FIND FIRST clientes OF items_orden_entrega NO-LOCK NO-ERROR.
                            IF AVAILABLE clientes THEN DO:
                                v_clientes = v_clientes + clientes.nombre.
                            END.
                        END.
                    END.
                    v_productos = "".
                    FOR EACH items_orden_entrega OF orden_entrega
                                                 BREAK BY items_orden_entrega.id_articulo.
                        IF LAST-OF(items_orden_entrega.id_articulo) THEN DO:
                            FIND FIRST productos_terminados OF items_orden_entrega NO-LOCK NO-ERROR.
                            IF AVAILABLE productos_terminados THEN DO:
                                v_productos = v_productos + productos_terminados.abreviatura.
                            END.
                        END.
                    END.
                    FIND FIRST items_orden_entrega OF orden_entrega NO-LOCK NO-ERROR.
                    IF AVAILABLE items_orden_entrega THEN DO:
                        FIND FIRST clausulas WHERE clausulas.id_clausula = 
                                                   items_orden_entrega.id_condicion_venta
                                             NO-LOCK NO-ERROR.
                        IF AVAILABLE clausulas THEN DO:
                            v_clausula = clausulas.descripcion.
                        END.
                    END.
                    v_contenedores = 0.
                    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK .
                        v_contenedores = v_contenedores + items_orden_entrega.contenedores.
                    END.

                    v_importe_coma = STRING(v_importe_gasto).
                    v_importe_coma = REPLACE(v_importe_coma,".",",").

                    run p_reportes_9.p (input "orden_entrega_ped_fondos_int",
                                      input "Reporte de Pedidos de Fondos",
                                      input "orden_entrega.id_orden_entrega = " + string(v_oe),
                                      input v_importe_coma + ";" +
                                            v_clientes + ";" +
                                            v_productos + ";" +
                                            v_clausula + ";" +
                                            string(v_contenedores) + ";").                    
                end.
            when "Exportacion Excell" then DO:
                RUN w_exportacion_oe.w.                    
            END.
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

   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
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

