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

/*-- VARIABLES DE EXCEL --*/

    define var chExcelAplication as com-handle.
    define var chWorkbook        as com-handle.
    define var chWorkSheet       as com-handle.
    define var chchart           as com-handle.
    define var chWorkSheetRange  as com-handle.
  
    define var ifila  as integer.
    define var cfila  as character.
    define var crange as character.
    DEFINE VARIABLE v_fechaini AS DATE.

 /*-- FIN VARIABLES DE EXCEL --*/

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
&Scoped-Define ENABLED-OBJECTS BUTTON-15 BUTTON-31 RECT-4 RECT-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_items_muestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_muestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_protocolos_muestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cfolder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_datos_courier_tuc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_items_muestras AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_muestras AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-15 
     LABEL "Enviar Mail Produccion" 
     SIZE 25 BY 1.14.

DEFINE BUTTON BUTTON-31 
     LABEL "Excell" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-32 
     LABEL "Enviar Mail a Destinatario" 
     SIZE 11 BY 1.14.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 2.38.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 7.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-15 AT ROW 1.95 COL 94
     BUTTON-31 AT ROW 1.95 COL 124
     BUTTON-32 AT ROW 1.95 COL 140
     RECT-4 AT ROW 1 COL 1
     RECT-5 AT ROW 3.62 COL 85
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152 BY 24.33.


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
         TITLE              = "Muestras"
         HEIGHT             = 24.33
         WIDTH              = 152
         MAX-HEIGHT         = 24.52
         MAX-WIDTH          = 152.4
         VIRTUAL-HEIGHT     = 24.52
         VIRTUAL-WIDTH      = 152.4
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
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR BUTTON BUTTON-32 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-32:HIDDEN IN FRAME F-Main           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Muestras */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Muestras */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 W-Win
ON CHOOSE OF BUTTON-15 IN FRAME F-Main /* Enviar Mail Produccion */
DO:
DEFINE VAR r as rowid.
define var v_subject as char.
define var v_body as char.
define var v_usuarios as char.
DEFINE VAR v_lista    AS INTEGER.

RUN get-rowid1 in h_b_muestras (output r).

IF USERID('userdb') MATCHES "*facundoj*" OR 
   USERID('userdb') MATCHES "*computos*" THEN
   v_lista = 200.
ELSE
   v_lista = 5.

FOR EACH usuarios_listas WHERE usuarios_listas.id_lista = v_lista /* Comercial avisa a Produccion */
                         NO-LOCK.
   /* IF v_usuarios = "" THEN
       v_usuarios = v_usuarios + usuarios_listas.email.
    ELSE */
       v_usuarios = v_usuarios + "," + usuarios_listas.email.
END.

IF v_usuarios <> "" THEN v_usuarios = SUBSTRING(v_usuarios,2,LENGTH(v_usuarios) - 1).
    
FIND FIRST muestras WHERE ROWID(muestras) = r NO-LOCK NO-ERROR.
IF AVAILABLE muestras THEN DO:
    v_subject = "Muestra " + STRING(muestras.id_muestra,"9999") + "/" + STRING(muestras.anio_muestra,"99").
    v_body = "Se ha creado una nueva muestra".
                        
    IF v_usuarios <> "" THEN DO:
        RUN ..\industria\SendMail.p(INPUT "",             /* SIEMPRE TIENE QUE IR */
         /* RUN ..\desarrollo\serverdes\industria\SendMail.p (INPUT "", */ 
                       INPUT 2,                           /* PRIORIDAD */
                       INPUT v_subject,                   /* SUBJECT */
                       INPUT v_body,                      /* BODY     */
                       INPUT v_usuarios,                  /* DEST. SEP COMAS */
                       INPUT ""                           /* ARCHIVOS ATTACHED SEP POR COMAS */
                       ).
                           
    END.
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-31
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-31 W-Win
ON CHOOSE OF BUTTON-31 IN FRAME F-Main /* Excell */
DO:
  ASSIGN v_fechaini = DATE(01,01,YEAR(TODAY)).
  FORM SKIP(1)
       v_fechaini LABEL "Consulta Desde Fecha"
       SKIP(1)
       with frame ing centered SIDE-LABELS overlay
        title " Confirme Fecha Inicial "
        width 40 three-d view-as dialog-box.

  /********** Habilita radio buttons y botones 
  enable radio_i      with frame ing.
  enable b_aceptar    with frame ing.
  enable b_salir      with frame ing.
  **********/

  /********** Cierre de Windows **********/
  on window-close of frame ing do:
    apply "END-ERROR" to frame ing.
  end.
  on "END-ERROR" of frame ing do:
    hide frame ing.
    leave.
  end.

  display
      v_fechaini
      with frame ing.

  set v_fechaini
      with frame ing.

  RUN p_exportacion_excell_muestras.p (INPUT v_fechaini).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-32 W-Win
ON CHOOSE OF BUTTON-32 IN FRAME F-Main /* Enviar Mail a Destinatario */
DO:
DEFINE VAR r as rowid.
define var v_subject as char.
define var v_body as char.
define var v_usuarios as char.
DEFINE VAR v_protocolos AS CHAR.

/*
RUN get-rowid1 in h_b_muestras_tuc (output r).

FOR EACH usuarios_listas WHERE usuarios_listas.id_lista = 6 /* Produccion avisa a Bue*/
                         NO-LOCK.
    v_usuarios = v_usuarios + "," + usuarios_listas.email.            
END.
            
IF v_usuarios <> "" THEN v_usuarios = SUBSTRING(v_usuarios,2,LENGTH(v_usuarios) - 1).
    
FIND FIRST muestras WHERE ROWID(muestras) = r NO-LOCK NO-ERROR.
IF AVAILABLE muestras THEN DO:
    FOR EACH r_muestras_protocolos WHERE r_muestras_protocolos.id_muestra     = muestras.id_muestra
                                    AND r_muestras_protocolos.anio_muestra   = muestras.anio_muestra
                                    NO-LOCK.
        v_protocolos = v_protocolos +
                       STRING(r_muestras_protocolos.id_protocolo,"9999") + "/" + 
                       STRING(r_muestras_protocolos.anio,"99") + CHR(10) .
                        
                       
    END.
    v_subject = "Se vincularon los Protocolos de la Muestra " + 
                STRING(muestras.id_muestra,"9999") + "/" + 
                STRING(muestras.anio_muestra,"99").

    v_body = "Esto es una prueba!!!!!   Se han vinculado las siguientes protocolos:" + CHR(10) + v_protocolos.
                        
    IF v_usuarios <> "" THEN DO:
        RUN SendMail.p(INPUT "",                       /* SIEMPRE TIENE QUE IR */
                       INPUT 2,                                                   /* PRIORIDAD */
                       INPUT v_subject,                                           /* SUBJECT */
                       INPUT v_body,                                              /* BODY     */
                       INPUT v_usuarios,                                          /* DEST. SEP COMAS */
                       INPUT ""                                      /* ARCHIVOS ATTACHED SEP POR COMAS */
                      ).
                           
    END.
END.
*/
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
       RUN set-position IN h_cus-updsav ( 1.24 , 2.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.24 , 36.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 23.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 58.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 20.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_muestras.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_muestras ).
       RUN set-position IN h_b_muestras ( 3.62 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b_muestras ( 6.71 , 82.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_muestras.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_muestras ).
       RUN set-position IN h_v_muestras ( 3.86 , 87.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.71 , 65.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/template/cfolder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'PartesMuestra|Protocolos' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_cfolder ).
       RUN set-position IN h_cfolder ( 11.48 , 2.00 ) NO-ERROR.
       RUN set-size IN h_cfolder ( 13.81 , 151.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_muestras. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_muestras ).

       /* Links to SmartViewer h_v_muestras. */
       RUN add-link IN adm-broker-hdl ( h_b_muestras , 'Record':U , h_v_muestras ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_muestras ).

       /* Links to SmartFolder h_cfolder. */
       RUN add-link IN adm-broker-hdl ( h_cfolder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_items_muestras.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_items_muestras ).
       RUN set-position IN h_b_items_muestras ( 13.14 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b_items_muestras ( 4.52 , 77.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_datos_courier_tuc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_datos_courier_tuc ).
       RUN set-position IN h_v_datos_courier_tuc ( 13.14 , 84.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.43 , 63.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_cus-updsav-2 ).
       RUN set-position IN h_cus-updsav-2 ( 17.91 , 4.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_items_muestras.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_items_muestras ).
       RUN set-position IN h_v_items_muestras ( 19.81 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.05 , 144.00 ) */

       /* Links to csmartbrowser h_b_items_muestras. */
       RUN add-link IN adm-broker-hdl ( h_b_muestras , 'Record':U , h_b_items_muestras ).

       /* Links to SmartViewer h_v_datos_courier_tuc. */
       RUN add-link IN adm-broker-hdl ( h_b_items_muestras , 'Record':U , h_v_datos_courier_tuc ).

       /* Links to SmartViewer h_v_items_muestras. */
       RUN add-link IN adm-broker-hdl ( h_b_items_muestras , 'Record':U , h_v_items_muestras ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_v_items_muestras ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_protocolos_muestras.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_protocolos_muestras ).
       RUN set-position IN h_b_protocolos_muestras ( 14.33 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b_protocolos_muestras ( 8.05 , 146.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_protocolos_muestras. */
       RUN add-link IN adm-broker-hdl ( h_b_muestras , 'Record':U , h_b_protocolos_muestras ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame-rowid-muestras W-Win 
PROCEDURE dame-rowid-muestras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_r AS ROWID.
RUN get-rowid1 IN h_b_muestras (OUTPUT p_r).
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
  ENABLE BUTTON-15 BUTTON-31 RECT-4 RECT-5 
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

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_reportes,output cresult).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reporte-excell W-Win 
PROCEDURE reporte-excell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bb_solicitante FOR contactos_muestras.
DEFINE VAR v_protocolos AS CHAR.
DEFINE VAR v_lotes AS CHAR.
DEFINE BUFFER bb_couriers FOR couriers.

/**************************************GENERADOR DE EXCELL **********************************/
  
      create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 5.
    chWorkSheet:Columns("B"):ColumnWidth = 5.
    chWorkSheet:Columns("C"):ColumnWidth = 1.
    chWorkSheet:Columns("D"):ColumnWidth = 5.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 15.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 15.
    chWorkSheet:Columns("I"):ColumnWidth = 5.
    chWorkSheet:Columns("J"):ColumnWidth = 15.
    chWorkSheet:Columns("K"):ColumnWidth = 5.
    chWorkSheet:Columns("L"):ColumnWidth = 15.
    chWorkSheet:Columns("M"):ColumnWidth = 5.
    chWorkSheet:Columns("N"):ColumnWidth = 25.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00". */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:S3"):MergeCells = True.
  /* chWorkSheet:Range("B5:Q5"):MergeCells = True. */
  chWorkSheet:Range("N5:P5"):MergeCells = True. /* AGRUPAR LOS DATOS DE TUCUMAN */
  chWorkSheet:Range("Q5:S5"):MergeCells = True. /* AGRUPAR LOS DATOS DE BSAS */
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "PEDIDO DE MUESTRAS".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 22.
  chWorkSheet:Range("B3"):Font:colorindex = 1.

  chWorkSheet:Range("N5"):Value = "TUCUMAN".
  chWorkSheet:Range("N5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N5"):interior:colorindex = 31.
  chWorkSheet:Range("N5"):Font:colorindex = 1.

  chWorkSheet:Range("Q5"):Value = "BUENOS AIRES".
  chWorkSheet:Range("Q5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q5"):interior:colorindex = 53.
  chWorkSheet:Range("Q5"):Font:colorindex = 1.
   
  chWorkSheet:Range("B6"):Value = "#".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "A�O".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "FECHA".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "ENVIAR A".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "PRODUCTO".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "SOLICITADO POR".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "DIRECTO".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CARACTERISTICAS".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "ENVASE".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "CANTIDAD".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "PROTOCOLOS".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "LOTES".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "ENVIADO".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "COURIER".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "NRO GUIA".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "ENVIADO".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "COURIER".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S6"):Value = "NRO GUIA".
  chWorkSheet:Range("S6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH muestras WHERE muestras.fecha >= DATE("01/01/03")
                      AND muestras.fecha <= DATE("01/01/04")
                      BY muestras.fecha.
    FIND FIRST productos_terminados  OF muestras NO-LOCK NO-ERROR.
    FIND FIRST contactos_muestras    WHERE contactos_muestras.id_contacto = muestras.id_destinatario NO-LOCK NO-ERROR.
    FIND FIRST bb_solicitante        WHERE bb_solicitante.id_contacto = muestras.id_solicitante NO-LOCK NO-ERROR.
    
    FOR EACH items_muestras OF muestras
                            WHERE items_muestras.item_muestra > 0
                            NO-LOCK.
        v_protocolos = "".
        v_lotes = "".
        FIND FIRST envases_muestras OF items_muestras NO-LOCK NO-ERROR.
        FIND FIRST couriers         WHERE couriers.id_courier = items_muestras.id_courier_tuc NO-LOCK NO-ERROR.
        FIND FIRST bb_couriers      WHERE bb_couriers.id_courier = items_muestras.id_courier_bue NO-LOCK NO-ERROR.
        FOR EACH r_muestras_protocolos WHERE r_muestras_protocolos.id_muestra   = items_muestras.id_muestra
                                         AND r_muestras_protocolos.anio_muestra = items_muestras.anio_muestra
                                         AND r_muestras_protocolos.item_muestra = items_muestras.item_muestra
                                        NO-LOCK.
            v_protocolos = v_protocolos + STRING(r_muestras_protocolos.id_protocolo,"9999") + "/" +
                                          STRING(r_muestras_protocolos.anio,"99") + " ".
            FOR EACH protocolos WHERE protocolos.id_protocolo = r_muestras_protocolos.id_protocolo
                                  AND protocolos.anio         = r_muestras_protocolos.anio
                                  AND protocolos.id_articulo  = r_muestras_protocolos.id_articulo
                                  NO-LOCK . 
                FIND FIRST tambores_industria WHERE tambores_industria.id_empresa = protocolos.id_empresa 
                                                AND tambores_industria.id_sucursal = protocolos.id_sucursal
                                                AND tambores_industria.id_tipotambor = protocolos.id_tipotambor
                                                AND tambores_industria.nromov = protocolos.nromov
                                                NO-LOCK NO-ERROR.
                IF AVAILABLE tambores_industria THEN DO:
                    v_lotes = v_lotes + STRING(tambores_industria.id_lote,"9999") + "/" + 
                                        STRING(SUBSTRING(STRING(tambores_industria.anio),3,2),"99") + " ".
                END.
            END.
        END.
        cfila  = STRING(ifila).
        cRange = "B" + cfila.
        chWorkSheet:Range(crange):VALUE = muestras.id_muestra.
        cRange = "C" + cfila.
        chWorkSheet:Range(crange):VALUE = muestras.anio_muestra.
        cRange = "D" + cfila.
        chWorkSheet:Range(crange):VALUE = muestras.fecha.
        cRange = "E" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE contactos_muestras THEN contactos_muestras.nombre ELSE "NONE".
        cRange = "F" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion_ingles ELSE "NONE".
        cRange = "G" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE bb_solicitante THEN bb_solicitante.nombre ELSE "NONE".
        cRange = "H" + cfila.
        chWorkSheet:Range(crange):VALUE = IF muestras.directo_cliente THEN "SI" ELSE "NO".
        cRange = "I" + cfila.
        chWorkSheet:Range(crange):VALUE = items_muestras.caracteristicas.
        cRange = "J" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE envases_muestras THEN envases_muestras.descripcion ELSE "NONE".
        cRange = "K" + cfila.
        chWorkSheet:Range(crange):VALUE = items_muestras.cantidad.
        cRange = "L" + cfila.
        chWorkSheet:Range(crange):VALUE = v_protocolos.
        cRange = "M" + cfila.
        chWorkSheet:Range(crange):value = v_lotes.
        cRange = "N" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.fecha_enviado_tuc.
        cRange = "O" + cfila.
        chWorkSheet:Range(crange):value = IF AVAILABLE couriers THEN couriers.descripcion ELSE "NONE".
        cRange = "P" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.nro_guia_tuc.
        cRange = "Q" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.fecha_enviado_bue.
        cRange = "R" + cfila.
        chWorkSheet:Range(crange):value = IF AVAILABLE bb_couriers THEN bb_couriers.descripcion ELSE "NONE".
        cRange = "S" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.nro_guia_bue.
        
        ifila = ifila + 1.
    END.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/
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
