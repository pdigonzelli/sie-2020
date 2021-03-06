&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
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

/* Local Variable Definitions ---                                       */
/* Local Variable Definitions ---                                       */
{custom/support/cabmcdvar.i}

/**********EMPIEZA-TEMP-TABLES*************/

&IF DEFINED(TABLA-CABECERA) <> 0 &THEN
    define buffer aux-cabecera for {&TABLA-CABECERA}.
&ENDIF
/**********TERMINA-TEMP-TABLES*************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame

&Scoped-define ADM-CONTAINER FRAME

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-16 RECT-4 b-detalle BUTTON-14 

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
DEFINE VARIABLE h_b_items_produccion_cascara_new AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_produccion_cascara_new AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_produccion_cascara_new AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-detalle 
     LABEL "&Detalle" 
     SIZE 20 BY 1.71.

DEFINE BUTTON BUTTON-14 
     LABEL "Imprimir Etiqueta" 
     SIZE 25 BY 1.33.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 111 BY 20.81.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     b-detalle AT ROW 1.76 COL 87
     BUTTON-14 AT ROW 11.38 COL 82
     RECT-16 AT ROW 1 COL 1
     RECT-4 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.6 BY 20.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 1
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
         HEIGHT             = 20.81
         WIDTH              = 111.2.
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
/*presupuestos,items_presupuestos*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartFrameCues" F-Frame-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartFrame,uib,49268
Destroy on next read */
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




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b-detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-detalle F-Frame-Win
ON CHOOSE OF b-detalle IN FRAME F-Main /* Detalle */
DO:
  case  b-detalle:label:
   when "&Detalle" Then
    do:
      b-detalle:label = "&Principal".
 /*     rec3:hidden = false.
      rec4:hidden = false.
      button-3:hidden = true.
      button-6:hidden = true.
      FILL1:visible = false.
      button-5:hidden = true.
      button-7:hidden = false.
      rec1:hidden = true.
      rect-17:hidden = true. */
      run select-page(2).
/*      run automatico in h_b_envasadores_fabrica1.*/
    end.
   otherwise 
     do:
      b-detalle:label = "&Detalle".
/*      rec3:hidden = true.
      rec4:hidden = true.
      rec1:hidden = false.
      button-3:hidden = false.
      button-7:hidden = true.
      button-6:hidden = false.
      FILL1:visible = TRUE.
      button-5:hidden = false.
      rect-17:hidden = false. */
      run select-page(1).
         
     end.  
   end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 F-Frame-Win
ON CHOOSE OF BUTTON-14 IN FRAME F-Main /* Imprimir Etiqueta */
DO:
        define var r as rowid.
        run get-rowid1 in h_b_produccion_cascara_new (output r).
        find produccion_cascara where rowid(produccion_cascara) = r no-lock no-error.
        if available produccion_cascara then
            do:
                for each items_produccion_cascara of produccion_cascara.
        output to c:\lpt1.  
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
        
        put control "^FO720,20^GB3,1200,3^FS".                
        put control "^FO725,520^A0R,45,70^FD" "S.A. SAN MIGUEL" "^FS".
        put control "^FO680,20^GB3,1200,3^FS".
        put control "^FO688,296^A0R,25,30^FD" "Agricola, Ganadera, Industrial, Comercial, Inmobiliaria,  y Financiera" "^FS".
        put control "^FO648,672^A0R,25,30^FD" "Administracion y Planta Industrial 1" "^FS".
        put control "^FO616,336^A0R,25,30^FD" "Lavalle 4001- C.C. 240(4000) S.M. de TUCUMAN - ARGENTINA." "^FS".
        put control "^FO584,184^A0R,25,30^FD" "Tel: (0381)4512600 - Fax: (0381)4512612 - R.N.P.A.23032189 - R.N.E.23000573" "^FS".
        put control "^FO578,20^GB3,1200,3^FS".
        put control "^FO510,32^A0R,50,60^FD" "COD. PRODUCTO" "^FS".
        put control "^FO510,710^A0R,50,50^FD" items_produccion_cascara.id_articulo "^FS".
        
        put control "^FO410,32^A0R,50,60^FD" "DESCRIPCION" "^FS".
        put control "^FO410,510^A0R,50,50^FD" "CASCARA DESHIDRATADA" "^FS".
        
        put control "^FO310,32^A0R,50,60^FD" "FECHA ELABORACION" "^FS".
        put control "^FO310,710^A0R,50,50^FD" items_produccion_cascara.fecha "^FS".
                
        put control "^FO210,32^A0R,50,60^FD" "NRO. PALLET" "^FS".
        put control "^FO210,735^A0R,50,50^FD" items_produccion_cascara.id_produccion "^FS".
        
        put control "^FO120,32^A0R,50,60^FD" "NRO. PLANTA INDUSTRIAL" "^FS".
        put control "^FO120,710^A0R,50,50^FD" "1" "^FS".
  
        put control "^FO10,200^A0R,^BY3^B3R,N,100,N,N^FD" produccion_cascara.id_etiqueta "^FS".
                
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close. 
                end. /* del primer for each  */
            end. /* del primer if available */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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
       RUN set-position IN h_cus-navico ( 1.71 , 37.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 23.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.71 , 59.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 20.00 ) NO-ERROR.

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
       RUN set-position IN h_cus-updsav ( 1.71 , 3.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_produccion_cascara_new.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_produccion_cascara_new ).
       RUN set-position IN h_b_produccion_cascara_new ( 4.05 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.71 , 103.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_produccion_cascara_new.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_produccion_cascara_new ).
       RUN set-position IN h_v_produccion_cascara_new ( 13.19 , 15.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.05 , 76.00 ) */

       /* Links to csmartbrowser h_b_produccion_cascara_new. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_produccion_cascara_new ).

       /* Links to SmartViewer h_v_produccion_cascara_new. */
       RUN add-link IN adm-broker-hdl ( h_b_produccion_cascara_new , 'Record':U , h_v_produccion_cascara_new ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_produccion_cascara_new ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav-2 ).
       RUN set-position IN h_cus-updsav-2 ( 1.71 , 3.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_items_produccion_cascara_new.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_items_produccion_cascara_new ).
       RUN set-position IN h_b_items_produccion_cascara_new ( 4.91 , 25.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.71 , 56.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to csmartbrowser h_b_items_produccion_cascara_new. */
       RUN add-link IN adm-broker-hdl ( h_b_produccion_cascara_new , 'record':U , h_b_items_produccion_cascara_new ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_b_items_produccion_cascara_new ).

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
run get-rowid1 in h_b_produccion_cascara_new (output p1).
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
  ENABLE RECT-16 RECT-4 b-detalle BUTTON-14 
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

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_reportes,output cresult).
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
{custom/support/localexit.i}
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


