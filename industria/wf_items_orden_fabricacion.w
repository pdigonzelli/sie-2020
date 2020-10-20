&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
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
/*define input parameter p1 as rowid.*/

/* Local Variable Definitions ---                                       */

define var del_nro as integer.
define var del_anio as integer.
define var del_id_cliente as integer.
define var del_id_articulo as integer.
define var del_id_envase as integer.
define var del_id_calidad as integer.
define var del_item as integer.

/* Local Variable Definitions ---                                       */
{custom/support/cabmcdvar.i}







/**********EMPIEZA-TEMP-TABLES*********/
&SCOPED-DEFINE TABLA-CABECERA items_orden_fabricacion
&SCOPED-DEFINE TABLA-DETALLE envios_items
{custom/support/temp-tables1.i &detalle={&TABLA-DETALLE}}
&IF DEFINED(TABLA-CABECERA) <> 0 &THEN
define buffer aux-cabecera for {&TABLA-CABECERA}.
&ENDIF
/**********TERMINA-TEMP-TABLES*********/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame

&Scoped-define ADM-CONTAINER FRAME

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-16 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cambio-detalle F-Frame-Win 
FUNCTION cambio-detalle RETURNS LOGICAL
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_envios_items AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_items_orden_fabricacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cfolder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_items_orden_fabricacion AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 148 BY 22.86.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RECT-4 AT ROW 1.24 COL 3
     RECT-16 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 148.4 BY 22.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 2
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 22.86
         WIDTH              = 148.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "visibles" F-Frame-Win _INLINE
/* Actions: ? custom/support/cusvis.p ? ? ? */
/* SmartFrame,uib,49268
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" F-Frame-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartFrame,uib,49268
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "tablas" F-Frame-Win _INLINE
/* Actions: ? custom/support/cabecera1.p ? ? ? */
/*items_orden_fabricacion,envios_items*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win _ADM-CREATE-OBJECTS
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
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.48 , 38.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 23.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.48 , 60.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 20.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_items_orden_fabricacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_items_orden_fabricacion ).
       RUN set-position IN h_b_items_orden_fabricacion ( 4.10 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.76 , 143.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/template/cfolder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'cabecera|detalle' + ',
                     FOLDER-TAB-TYPE = 2':U ,
             OUTPUT h_cfolder ).
       RUN set-position IN h_cfolder ( 11.57 , 3.20 ) NO-ERROR.
       RUN set-size IN h_cfolder ( 11.81 , 144.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_items_orden_fabricacion. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_items_orden_fabricacion ).

       /* Links to SmartFolder h_cfolder. */
       RUN add-link IN adm-broker-hdl ( h_cfolder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav ).
       RUN set-position IN h_cus-updsav ( 1.48 , 4.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_items_orden_fabricacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_items_orden_fabricacion ).
       RUN set-position IN h_v_items_orden_fabricacion ( 13.86 , 11.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.05 , 80.00 ) */

       /* Links to SmartViewer h_v_items_orden_fabricacion. */
       RUN add-link IN adm-broker-hdl ( h_b_items_orden_fabricacion , 'Record':U , h_v_items_orden_fabricacion ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_items_orden_fabricacion ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_envios_items.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_envios_items ).
       RUN set-position IN h_b_envios_items ( 14.52 , 44.80 ) NO-ERROR.
       /* Size in UIB:  ( 7.62 , 55.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav-2 ).
       RUN set-position IN h_cus-updsav-2 ( 1.48 , 4.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 35.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to csmartbrowser h_b_envios_items. */
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_b_envios_items ).
       RUN add-link IN adm-broker-hdl ( h_v_items_orden_fabricacion , 'Record':U , h_b_envios_items ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE borra-items F-Frame-Win 
PROCEDURE borra-items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(TABLA-ITEMS) <> 0 &THEN
{custom/support/borraitems.i}
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambio-fila F-Frame-Win 
PROCEDURE cambio-fila :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
define var r as rowid no-undo.

if valid-handle(h) and h = hcabecera  then
do:
    if cambio-detalle() then
        run proceso-cabecera-detalle.
    run genero-detalle.
end.    
&IF DEFINED (TABLA-CABECERA) <> 0 &THEN
    run get-rowid-cabecera(output r).
    find aux-cabecera where rowid(aux-cabecera) = r no-error.
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consulta F-Frame-Win 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame-rowid F-Frame-Win 
PROCEDURE dame-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter p1 as rowid.
define var r as rowid.
RUN get-rowid1 in h_b_items_orden_fabricacion (output r).
p1 = r.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita-viewer-paginas F-Frame-Win 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer F-Frame-Win 
PROCEDURE deshabilita_viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid-cabecera F-Frame-Win 
PROCEDURE devuelve-rowid-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.

run get-rowid in hcabecera ( output r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid-detalle F-Frame-Win 
PROCEDURE devuelve-rowid-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.

run get-rowid in hdetalle ( output r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win _DEFAULT-ENABLE
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
  ENABLE RECT-4 RECT-16 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genero-detalle F-Frame-Win 
PROCEDURE genero-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run genero-detalle-interno.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genero-detalle-interno F-Frame-Win 
PROCEDURE genero-detalle-interno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(TABLA-CABECERA) <> 0 &THEN
{custom/support/gendetalle.i &cabecera={&TABLA-CABECERA}
                &detalle={&TABLA-DETALLE}}
&ENDIF                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-deshabilitados-paleta F-Frame-Win 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid F-Frame-Win 
PROCEDURE get-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r as rowid no-undo.
run devuelve-rowid(output r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-cabecera F-Frame-Win 
PROCEDURE get-rowid-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.

r = ?.
if valid-handle(hcabecera) then
    run get-rowid1 in hcabecera ( output r ) no-error.
else
    r = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-detalle F-Frame-Win 
PROCEDURE get-rowid-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.
r = ?.
if valid-handle(hdetalle) then
    run get-rowid1 in hdetalle ( output r) no-error.
else
    r = ?.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE got-rowid F-Frame-Win 
PROCEDURE got-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter v1 as integer.
define output parameter v2 as integer.
define output parameter v3 as integer.
define var p3 as rowid.
define var hcontainer as handle.

run get-container (output hcontainer).

run paso-rowid-frame in hcontainer (output p3).

for each orden_fabricacion where rowid(orden_fabricacion) = p3.
    if available orden_fabricacion then
    do.
        v1 = orden_fabricacion.nro.
        /*message v1 view-as alert-box.*/
        v2 = orden_fabricacion.anio .
        /*message v2 view-as alert-box.*/
        v3 = orden_fabricacion.id_cliente.
        /*message v3 view-as alert-box.*/
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-relacion-viewer-pagina F-Frame-Win 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresion F-Frame-Win 
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

/*if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_reportes,output cresult).*/
    
/************************************************************************************************************/
/********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
/************************************************************************************************************/
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
define var v_filtro as character.
define var v1 as integer.
define var v2 as integer.
define var v3 as integer.

run got-rowid (output v1, output v2, output v3).
v_filtro = "orden_fabricacion.nro = " + string(v1) + 
           " and orden_fabricacion.anio = " + string(v2) +
           " and orden_fabricacion.id_cliente = " + string(v3).



RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.

      RUN  aderb\_printrb(
       "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
       "orden_fabricacion",                    /* RB-REPORT-NAME */
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
       "Orden de Fabricación",         /* RB-WINDOW-TITLE */
       yes,                           /* RB-DISPLAY-ERRORS */
       yes,                           /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "" /* RB-OTHER-PARAMETERS */
       ).   
       
os-delete value(RB-MEMO-FILE).


/************************************************************************************************************/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page F-Frame-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 /* Code placed here will execute PRIOR to standard behavior. */
  
  run get-attribute ('current-page').
  if return-value = "2" and valid-handle(hdetalle) and pagina-origen = "1" then
  do:
        run check-modified in hcabecera (input "check").
        run genero-detalle.
        run dispatch in hdetalle ('open-query').
  end.

  run get-attribute ('current-page').
  if return-value = "1" and valid-handle(hcabecera) then
  do:
    if cambio-detalle() then    
        run proceso-cabecera-detalle.
  end.
  run get-attribute ('current-page').
  pagina-origen=return-value.


  /* Dispatch standard ADM method.                             */
 
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit F-Frame-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/

define var v1 as integer.
define var v2 as integer.
define var v3 as integer.
define var diferencia as integer.
define var cantidad_envios as integer.
define var bandera as logical initial false.
define var r as rowid.
RUN dame-rowid (output r).
run got-rowid (output v1, output v2, output v3).

for each items_orden_fabricacion where items_orden_fabricacion.nro = v1 and 
                                       items_orden_fabricacion.anio = v2 and 
                                       items_orden_fabricacion.id_cliente = v3.
    
    cantidad_envios = 0.
    for each envios_items where envios_items.nro = items_orden_fabricacion.nro and
                            envios_items.anio = items_orden_fabricacion.anio and
                            envios_items.id_cliente = items_orden_fabricacion.id_cliente and
                            envios_items.id_articulo = items_orden_fabricacion.id_articulo and
                            envios_items.id_calidad = items_orden_fabricacion.id_calidad and
                            envios_items.id_envase = items_orden_fabricacion.id_envase.
                            
        cantidad_envios = cantidad_envios + envios_items.cantidad.                            
    end.                            

    if cantidad_envios <> items_orden_fabricacion.cantidad then 
    do:
        diferencia = items_orden_fabricacion.cantidad - cantidad_envios.
        find productos_terminados where productos_terminados.id_articulo = items_orden_fabricacion.id_articulo no-lock no-error.
        if available productos_terminados then
        do:
            message "No ha cargado completamente los envios del pedido de " productos_terminado.descripcion 
            ". Hay una diferencia de " diferencia " kilogramos."
             skip(1) "Desea salir de todas maneras..?" view-as alert-box 
            buttons yes-no-cancel UPDATE respuesta AS LOGICAL.
            if not respuesta then bandera = true.
        end.
     end.
     
end.
if not bandera then 
do:
    /*message "Esta por salir" view-as alert-box.*/
    {custom/support/localexit.i}.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide F-Frame-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize F-Frame-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

            /*run got-rowid.*/

  /* Code placed here will execute AFTER standard behavior.    */
  hide frame {&FRAME-NAME}.
  {custom/support/cabmcd.i}
  run deshabilita-viewer-paginas.
  run habilita-relacion-viewer-pagina.
  {custom/method/ctitulo.i}
  view frame {&FRAME-NAME}.
  run select-page(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy F-Frame-Win 
PROCEDURE post-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
{custom/support/casecd.i "post-copy-cabecera(input h)" 
                         "post-copy-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy-cabecera F-Frame-Win 
PROCEDURE post-copy-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.

alta-cabecera = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy-detalle F-Frame-Win 
PROCEDURE post-copy-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.

alta-detalle = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create F-Frame-Win 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
/*message "post-create".*/
{custom/support/casecd.i "post-create-cabecera(input h)" 
                         "post-create-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create-cabecera F-Frame-Win 
PROCEDURE post-create-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter  h as handle no-undo.
/*define var v1 as integer.
define var v2 as integer.
define var v3 as integer.
run got-rowid (output v1, output v2, output v3).

assign items_orden_fabricacion.nro = v1
       items_orden_fabricacion.anio = v2
       items_orden_fabricacion.id_cliente = v3.

message v1 v2 v3 view-as alert-box.*/
/*message "post-create-cabecera".*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create-detalle F-Frame-Win 
PROCEDURE post-create-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter  h as handle no-undo.
/*message "post-create-detalle".*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete F-Frame-Win 
PROCEDURE post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
{custom/support/casecd.i "post-delete-cabecera(input h)" 
                         "post-delete-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete-cabecera F-Frame-Win 
PROCEDURE post-delete-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
/*message "post-delete-cabecera" view-as alert-box.*/
/******************************************************************/
/*  BORRO CON LAS VARIABLES GUARDADAS EN EL PRE-DELETE-CABECERA*/

for each envios_items where nro = del_nro and
                            anio = del_anio and
                            item = del_item and
                            id_cliente = del_id_cliente and
                            id_articulo = del_id_articulo and
                            id_envase = del_id_envase and
                            id_calidad = del_id_calidad.

    delete envios_items.
                        
end.
/**********************************************************************/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete-detalle F-Frame-Win 
PROCEDURE post-delete-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.
/*message "post-delete-detalle" view-as alert-box.*/
flag-detalle = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update F-Frame-Win 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */
/*message "post-update".*/
{custom/support/casecd.i "post-update-cabecera(input r , input h)" 
                         "post-update-detalle(input r , input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update-cabecera F-Frame-Win 
PROCEDURE post-update-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.
/*message "post-update-cabecera".*/
if alta-cabecera then
    run enable-folder-page in hfolder ( input 2).
alta-cabecera = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update-detalle F-Frame-Win 
PROCEDURE post-update-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.
/*message "post-update-detalle".*/

flag-detalle = true.
alta-detalle = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy F-Frame-Win 
PROCEDURE pre-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
{custom/support/casecd.i "pre-copy-cabecera(input h)" 
                         "pre-copy-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy-cabecera F-Frame-Win 
PROCEDURE pre-copy-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta-cabecera = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy-detalle F-Frame-Win 
PROCEDURE pre-copy-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta-detalle = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create F-Frame-Win 
PROCEDURE pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
/*message "pre-create".*/
{custom/support/casecd.i "pre-create-cabecera(input h)" 
                         "pre-create-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create-cabecera F-Frame-Win 
PROCEDURE pre-create-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
/*message "pre-create-cabecera".*/
run disable-folder-page in hfolder (input 2).
alta-cabecera = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create-detalle F-Frame-Win 
PROCEDURE pre-create-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta-detalle = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete F-Frame-Win 
PROCEDURE pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */
{custom/support/casecd.i "pre-delete-cabecera(input r , input h)" 
                         "pre-delete-detalle(input r , input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete-cabecera F-Frame-Win 
PROCEDURE pre-delete-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.
/*message "pre-delete-cabecera" view-as alert-box.*/
/***************************************************/
/*ALMACENO EN VARIABLES LOS DATOS DEL CAMPO BORRADO*/
define var r1 as rowid.
RUN get-rowid1 IN h_b_items_orden_fabricacion (OUTPUT r1).

for each items_orden_fabricacion where rowid(items_orden_fabricacion) = r1.
    del_nro = items_orden_fabricacion.nro.
    del_anio = items_orden_fabricacion.anio.
    del_item = items_orden_fabricacion.item.
    del_id_cliente = items_orden_fabricacion.id_cliente.
    del_id_articulo = items_orden_fabricacion.id_articulo.
    del_id_envase = items_orden_fabricacion.id_envase.
    del_id_calidad = items_orden_fabricacion.id_calidad.
end.

/*message del_nro del_anio del_item del_id_cliente del_id_articulo del_id_envase del_id_calidad view-as alert-box.*/
/***************************************************/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete-detalle F-Frame-Win 
PROCEDURE pre-delete-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.
/*message "pre-delete-detalle" view-as alert-box.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update F-Frame-Win 
PROCEDURE pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */
/*message "pre-update".*/
{custom/support/casecd.i "pre-update-cabecera(input r , input h)" 
                         "pre-update-detalle(input r , input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update-cabecera F-Frame-Win 
PROCEDURE pre-update-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.
/*message "pre-update-cabecera".*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update-detalle F-Frame-Win 
PROCEDURE pre-update-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.
/*************************************************************************/
/*VERIFICO QUE NO SE CARGUE CANTIDADES MAYORES QUE LAS TOTALES***************/

define var v1 as integer.
define var v2 as integer.
define var v3 as integer.
DEFINE var r1 as rowid.
define var cantidad_envios as integer initial 0.
run got-rowid (output v1, output v2, output v3).

/**ESTO ES NUEVO**/  RUN dame-rowid (output r1).

/*CODIGO VIEJO   for each envios_items where envios_items.nro = v1 and
                            envios_items.anio = v2 and
                            envios_items.id_cliente = v3.
                            
    cantidad_envios = cantidad_envios + envios_items.cantidad.                            
end.                            


for each items_orden_fabricacion where items_orden_fabricacion.nro = v1 and
                                       items_orden_fabricacion.anio = v2 and
                                       items_orden_fabricacion.id_cliente = v3.
    
    if cantidad_envios > items_orden_fabricacion.cantidad then 
    do:
        message "La cantidad total del envio no puede ser superado por la suma de los envios parciales" view-as alert-box.
        undo, return "ADM-ERROR".
        for each envios_items where envios_items.nro = 0.
            delete envios_items.
        end.
     end.

end.*/



for each items_orden_fabricacion where ROWID(items_orden_fabricacion) = r1.
    
    for each envios_items where envios_items.nro = items_orden_fabricacion.nro and
                            envios_items.anio = items_orden_fabricacion.anio and
                            envios_items.id_cliente = items_orden_fabricacion.id_cliente and
                            envios_items.id_articulo = items_orden_fabricacion.id_articulo and
                            envios_items.id_calidad = items_orden_fabricacion.id_calidad and
                            envios_items.id_envase = items_orden_fabricacion.id_envase.
                            
        cantidad_envios = cantidad_envios + envios_items.cantidad.                            
    end.                            

    if cantidad_envios > items_orden_fabricacion.cantidad then 
    do:
        message "La cantidad total del envio no puede ser superado por la suma de los envios parciales" view-as alert-box.
        undo, return "ADM-ERROR".
        for each envios_items where envios_items.nro = 0 or envios_items.anio = 0 or envios_items.id_cliente = 0.
            delete envios_items.
        end.
     end.

end.




/********************************************************************************/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proceso-cabecera-detalle F-Frame-Win 
PROCEDURE proceso-cabecera-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run proceso-cabecera-detalle-interno.
flag-detalle = no.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proceso-cabecera-detalle-interno F-Frame-Win 
PROCEDURE proceso-cabecera-detalle-interno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(TABLA-CABECERA) <> 0 &THEN
{custom/support/deldetalle1.i &cabecera={&TABLA-CABECERA}
             &detalle={&TABLA-DETALLE}
             &items={&TABLA-ITEMS}
             &bloque-borrado=}
&ENDIF             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetea-registro F-Frame-Win 
PROCEDURE resetea-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
if alta-cabecera then
    run enable-folder-page in hfolder ( input 2).
alta-cabecera = false.
alta-detalle = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartFrame, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cambio-detalle F-Frame-Win 
FUNCTION cambio-detalle RETURNS LOGICAL
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  if flag-detalle then
    return true.

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


