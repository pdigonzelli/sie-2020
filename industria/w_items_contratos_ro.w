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

define input parameter p_id_contrato as char.
define input parameter p_id_tipo_contrato as integer.
define input parameter p_anio as integer.


/* Local Variable Definitions ---                                       */
define var alta as logical no-undo initial false.
define var r_item as rowid.

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
&Scoped-Define ENABLED-OBJECTS RECT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_consulta_item_contrato_item_ AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_items_contratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_r_gastos_items_contratos_aux AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_items_contratos_precio_base AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_items_contratos_principal AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_items_contrato_precios AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 229 BY 50
     BGCOLOR 7 FGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RECT-2 AT Y 0 X 315
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 146.8 BY 23.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 3
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Envios"
         HEIGHT             = 23.29
         WIDTH              = 146.8
         MAX-HEIGHT         = 23.95
         MAX-WIDTH          = 151.2
         VIRTUAL-HEIGHT     = 23.95
         VIRTUAL-WIDTH      = 151.2
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
ON END-ERROR OF W-Win /* Envios */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Envios */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualiza_browser W-Win 
PROCEDURE actualiza_browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

run dispatch in h_b_items_contratos ('open-query').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.24 , 64.80 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 24.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 87.80 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_items_contratos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_items_contratos ).
       RUN set-position IN h_b_items_contratos ( 3.62 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b_items_contratos ( 6.71 , 130.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Principal|Precios|OE' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 10.52 , 3.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 13.33 , 143.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_items_contratos. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_items_contratos ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_items_contratos_principal.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_items_contratos_principal ).
       RUN set-position IN h_v_items_contratos_principal ( 12.52 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.48 , 138.00 ) */

       /* Links to SmartViewer h_v_items_contratos_principal. */
       RUN add-link IN adm-broker-hdl ( h_b_items_contratos , 'Record':U , h_v_items_contratos_principal ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_r_gastos_items_contratos_aux.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_r_gastos_items_contratos_aux ).
       RUN set-position IN h_b_r_gastos_items_contratos_aux ( 16.24 , 5.40 ) NO-ERROR.
       RUN set-size IN h_b_r_gastos_items_contratos_aux ( 5.00 , 138.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_items_contratos_precio_base.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_items_contratos_precio_base ).
       RUN set-position IN h_v_items_contratos_precio_base ( 12.19 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.95 , 99.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_items_contrato_precios.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_items_contrato_precios ).
       RUN set-position IN h_v_items_contrato_precios ( 21.48 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.95 , 98.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to csmartbrowser h_b_r_gastos_items_contratos_aux. */
       RUN add-link IN adm-broker-hdl ( h_b_items_contratos , 'Record':U , h_b_r_gastos_items_contratos_aux ).

       /* Links to SmartViewer h_v_items_contratos_precio_base. */
       RUN add-link IN adm-broker-hdl ( h_b_items_contratos , 'Record':U , h_v_items_contratos_precio_base ).
       RUN add-link IN adm-broker-hdl ( h_v_items_contratos_principal , 'group-assign':U , h_v_items_contratos_precio_base ).

       /* Links to SmartViewer h_v_items_contrato_precios. */
       RUN add-link IN adm-broker-hdl ( h_b_items_contratos , 'record':U , h_v_items_contrato_precios ).
       RUN add-link IN adm-broker-hdl ( h_v_items_contratos_principal , 'group-assign':U , h_v_items_contrato_precios ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_consulta_item_contrato_item_oe.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_consulta_item_contrato_item_ ).
       RUN set-position IN h_b_consulta_item_contrato_item_ ( 14.10 , 14.00 ) NO-ERROR.
       RUN set-size IN h_b_consulta_item_contrato_item_ ( 6.71 , 104.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_consulta_item_contrato_item_. */
       RUN add-link IN adm-broker-hdl ( h_b_items_contratos , 'Record':U , h_b_consulta_item_contrato_item_ ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 3 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame_datos_contrato W-Win 
PROCEDURE dame_datos_contrato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter p1 as char.
define output parameter p2 as integer.
define output parameter p3 as integer.



p1 = p_id_contrato.
p2 = p_id_tipo_contrato.
p3 = p_anio.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame_rowid_item W-Win 
PROCEDURE dame_rowid_item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter pr as rowid.

run get-rowid1 in h_b_items_contratos (output pr).
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
  ENABLE RECT-2 
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

define var v_contrato as char.
define var v_tipo_contrato as integer.
define var v_anio as integer.
define var v_item as integer.

define var v_filtro as character.
define var v_fecha as character.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

find items_contratos where rowid(items_contratos) = r no-lock no-error.
if available items_contratos then
    do:
        v_contrato = items_contratos.id_contrato.
        v_tipo_contrato = items_contratos.id_tipo_contrato.
        v_anio = items_contratos.anio.
        v_item = items_contratos.item.
    end.
    
lista_reportes = "Contratos detallado".

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    do:
        run custom/support/cfun.w(input lista_reportes,output cresult).
        case cresult:
            when "Contratos detallado" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "items_contratos.id_contrato = '" + v_contrato +
                         "' and items_contratos.id_tipo_contrato = " + string(v_tipo_contrato) +
                          " and items_contratos.anio = " + string(v_anio) + 
                          " and items_contratos.item = " + string(v_item).
                    
                    message " Filtro " v_filtro view-as alert-box.
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "contratos_items_ddip",                    /* RB-REPORT-NAME */
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
                           "Reporte de Contratos",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pasa-rowid-cabecera W-Win 
PROCEDURE pasa-rowid-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter pr as rowid.
define var r as rowid.

run get-rowid1 in h_b_items_contratos (output pr).
r = pr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pasar-rowid-copy W-Win 
PROCEDURE pasar-rowid-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter p_r as rowid.
define output parameter bandera as integer.

bandera = 1.
p_r = r_item.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-add W-Win 
PROCEDURE post-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


find contratos where contratos.id_tipo_contrato = p_id_tipo_contrato
                 and contratos.id_contrato = p_id_contrato
                 and contratos.anio = p_anio
                 no-lock no-error.

if available contratos then
    do:
        if p_id_tipo_contrato <= 3 then
            do:
                /* SON TIPOS DE CONTRATOS VIEJOS */
                
                run habilito_producto_calidad in h_v_items_contratos_principal.
                
            end.
        else
            do:
                /* SON NUEVOS TIPOS DE CONTRATOS ESPECIALES */
                
                run deshabilito_producto_calidad in h_v_items_contratos_principal.
            end.
        find last items_contratos of contratos no-lock no-error.
        if available items_contratos then
            do:
                run carga_datos_iniciales in h_v_items_contratos_principal (input contratos.fecha, 
                                                                            input contratos.id_po_cliente[1],
                                                                            input items_contratos.item + 1).
            end.
        else
            do:
                run carga_datos_iniciales in h_v_items_contratos_principal (input contratos.fecha, 
                                                                            input contratos.id_po_cliente[1],
                                                                            input 1).
            end.
                                                                        
    end.
             
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

/* run recalculo_precio_de_venta. */
/*assign items_contratos.precio_venta = items_contratos.precio_origen.*/
alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-add W-Win 
PROCEDURE pre-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
define buffer it for items_contratos.

run get-rowid1 in h_b_items_contratos (output r_item).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalculo_precio_de_venta W-Win 
PROCEDURE recalculo_precio_de_venta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r as rowid.
define var v_importe as integer.

v_importe = 0.
run get-rowid1 in h_b_items_contratos (output r).

find items_contratos where rowid(items_contratos) = r no-lock no-error.
if available items_contratos then
    do:
        
        for each r_gastos_items_contrato of items_contratos no-lock.
        
            v_importe = v_importe + r_gastos_items_contrato.importe.
                        
        end.
        
        items_contrato.precio_venta      = items_contrato.precio_base_calculo + v_importe.
        items_contrato.precio_origen     = items_contrato.precio_venta * items_contrato.cotizacion_origen.

    end.

run actualizo_campos in h_v_items_contrato_precios.

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

