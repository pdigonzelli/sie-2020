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
&Scoped-Define ENABLED-OBJECTS BUTTON-16 semana BUTTON-17 BUTTON-1 RECT-5 ~
RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS semana 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_gastos_orden_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_orden_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_orden_entrega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_orden_entrega_1 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Actualizar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-16 
     LABEL "Enviar Mail" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-17 
     LABEL "Reporte" 
     SIZE 13.4 BY 1.14.

DEFINE VARIABLE semana AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Semana" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 147 BY 12.14.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 147 BY 6.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 3.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-16 AT ROW 1.48 COL 10
     semana AT ROW 3.62 COL 137 COLON-ALIGNED
     BUTTON-17 AT ROW 4.81 COL 131
     BUTTON-1 AT ROW 6.71 COL 129
     RECT-5 AT ROW 8.38 COL 1
     RECT-6 AT ROW 20.52 COL 1
     RECT-7 AT ROW 3.14 COL 127
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 147.4 BY 25.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Orden de Entrega"
         HEIGHT             = 25.86
         WIDTH              = 147.6
         MAX-HEIGHT         = 25.91
         MAX-WIDTH          = 147.6
         VIRTUAL-HEIGHT     = 25.91
         VIRTUAL-WIDTH      = 147.6
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Orden de Entrega */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Orden de Entrega */
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
  run recalcular-datos in h_v_orden_entrega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 W-Win
ON CHOOSE OF BUTTON-16 IN FRAME F-Main /* Enviar Mail */
DO:
    define var r as rowid.
    run get-rowid1 in h_b_orden_entrega (output r).
  run p_correo_aviso_contrato.p (input 2, input r, input "creado o modificado").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-17 W-Win
ON CHOOSE OF BUTTON-17 IN FRAME F-Main /* Reporte */
DO:
  
define var v_semana as integer.
define var v_filtro as char.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

v_semana = integer(semana:screen-value in frame F-Main).

if v_semana > 0 then
    do:
        run p_reporte_oe_lote_semana.p (input v_semana).
        
        /************************************************************************************************************/
        /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
        /************************************************************************************************************/
                            
        v_filtro = "".
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "reporte_oe_lote_semana",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Orden de Entregas Semanal",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
        /************************************************************************************************************/


        
        
    end.
else message "Por favor ingrese la semana que desea imprimir." view-as alert-box.    
                     
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
       RUN set-position IN h_cus-updsav ( 1.00 , 36.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.00 , 71.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 24.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.00 , 95.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_orden_entrega ).
       RUN set-position IN h_b_orden_entrega ( 2.91 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b_orden_entrega ( 5.48 , 118.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_orden_entrega ).
       RUN set-position IN h_v_orden_entrega ( 8.62 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.57 , 143.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_orden_entrega_1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_orden_entrega_1 ).
       RUN set-position IN h_v_orden_entrega_1 ( 18.38 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.00 , 141.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_gastos_orden_entrega.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_gastos_orden_entrega ).
       RUN set-position IN h_b_gastos_orden_entrega ( 20.76 , 54.00 ) NO-ERROR.
       RUN set-size IN h_b_gastos_orden_entrega ( 5.71 , 87.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-2 ).
       RUN set-position IN h_cus-updsav-2 ( 21.24 , 8.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 36.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_orden_entrega ).

       /* Links to SmartViewer h_v_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_b_orden_entrega , 'Record':U , h_v_orden_entrega ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_orden_entrega ).

       /* Links to SmartViewer h_v_orden_entrega_1. */
       RUN add-link IN adm-broker-hdl ( h_b_orden_entrega , 'record':U , h_v_orden_entrega_1 ).
       RUN add-link IN adm-broker-hdl ( h_v_orden_entrega , 'group-assign':U , h_v_orden_entrega_1 ).

       /* Links to csmartbrowser h_b_gastos_orden_entrega. */
       RUN add-link IN adm-broker-hdl ( h_b_orden_entrega , 'Record':U , h_b_gastos_orden_entrega ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_b_gastos_orden_entrega ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-updsav ,
             BUTTON-16:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-navico ,
             h_cus-updsav , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             h_cus-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_orden_entrega ,
             BUTTON-16:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v_orden_entrega ,
             BUTTON-1:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v_orden_entrega_1 ,
             h_v_orden_entrega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_gastos_orden_entrega ,
             h_v_orden_entrega_1 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-updsav-2 ,
             h_b_gastos_orden_entrega , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-fob-viewer W-Win 
PROCEDURE carga-fob-viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter v_orden_entrega as integer no-undo.
run carga-fob in h_v_orden_entrega (input v_orden_entrega).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-tipo-venta W-Win 
PROCEDURE carga-tipo-venta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_tipo_venta AS INTEGER.

RUN carga-tipo-venta IN h_v_orden_entrega_1 (INPUT p_tipo_venta).
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
define output parameter v_orden_entrega as rowid.

run get-rowid1 in h_b_orden_entrega (output v_orden_entrega).
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
  DISPLAY semana 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-16 semana BUTTON-17 BUTTON-1 RECT-5 RECT-6 RECT-7 
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


run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

find orden_entrega where rowid(orden_entrega) = r no-lock no-error.
if available orden_entrega then
    v_oe = orden_entrega.id_orden_entrega.
else
    v_oe = 0.

lista_reportes = "Orden Entrega,Estado OE,Pedido de fondos,Exporta Excell,Orden Entrega(IKS),Listado contenedores,Listado contenedores entre fecha".

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    do:
        run custom/support/cfun.w(input lista_reportes,output cresult).
        case cresult:
            when "Orden Entrega" then 
                do:
                    run p_reportes_9.p (input "orden_entrega_fax",
                                      input "Reporte de Orden de Entregas",
                                      input "orden_entrega.id_orden_entrega = " + string(v_oe),
                                      input "").                    
                end.
            when "Estado OE" then 
                do:
                    run p_reportes.p (input "estado_contratos_oe",
                                      input "Reporte de Estados de las Orden de Entregas",
                                      input "",
                                      input "").                    
                end.
           when "Pedido de fondos" then 
                do:
                    for each gastos_orden_entrega of orden_entrega no-lock.
                        if gastos_orden_entrega.id_gasto <> 3 and
                           gastos_orden_entrega.id_gasto <> 11 and 
                           gastos_orden_entrega.id_gasto <> 12 then
                           v_importe_gasto = v_importe_gasto + gastos_orden_entrega.importe.
                    end.
                    run p_reportes.p (input "orden_entrega_ped_fondos",
                                      input "Reporte de Pedidos de Fondos",
                                      input "orden_entrega.id_orden_entrega = " + string(v_oe),
                                      input string(v_importe_gasto) + ";").                    
                end.
           when "Exporta Excell" then 
                do:
                    run p_exporta_oe.p.                    
                end.
           when "Orden Entrega(IKS)" then 
                do:
                    run p_reportes.p (input "orden_entrega_fax_iks",
                                      input "Reporte de Orden de Entregas",
                                      input "orden_entrega.id_orden_entrega = " + string(v_oe),
                                      input "").                    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query-browser W-Win 
PROCEDURE open-query-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run actualiza-browser in h_b_orden_entrega.
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

run dispatch in h_b_gastos_orden_entrega ('open-query':U).

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

