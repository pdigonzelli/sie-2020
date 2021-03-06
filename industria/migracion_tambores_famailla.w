&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/* Local Variable Definitions ---                                       */

define var sucursal_local as integer.
define var sucursal_actual as integer.
define var i as integer initial 0.
define var j as integer initial 0.
define var v_resultado as logical initial true.

define var alta as logical no-undo initial false.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 sucursal BUTTON-1 lista lista_mod ~
BUTTON-2 BUTTON-3 registros 
&Scoped-Define DISPLAYED-OBJECTS sucursal lista lista_mod registros 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Conectar a Bases de Datos" 
     SIZE 36 BY 1.43.

DEFINE BUTTON BUTTON-2 
     LABEL "Migraci�n de datos" 
     SIZE 36 BY 1.43.

DEFINE BUTTON BUTTON-3 
     LABEL "Migraci�n de Remitos" 
     SIZE 36 BY 1.43.

DEFINE VARIABLE registros AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Registros Importados" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sucursal AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Sucursal (95-Famailla , 96-Lavalle)" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 7.14.

DEFINE VARIABLE lista AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.48 NO-UNDO.

DEFINE VARIABLE lista_mod AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.48 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     sucursal AT ROW 1.48 COL 42 COLON-ALIGNED
     BUTTON-1 AT ROW 3.38 COL 5
     lista AT ROW 4.57 COL 51 NO-LABEL
     lista_mod AT ROW 4.57 COL 87 NO-LABEL
     BUTTON-2 AT ROW 5.05 COL 5
     BUTTON-3 AT ROW 6.71 COL 5
     registros AT ROW 8.38 COL 25 COLON-ALIGNED
     RECT-1 AT ROW 2.91 COL 3
     "Productos Importados:" VIEW-AS TEXT
          SIZE 34 BY .95 AT ROW 3.62 COL 51
     "Registros Actualizados:" VIEW-AS TEXT
          SIZE 34 BY .95 AT ROW 3.62 COL 87
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 122.4 BY 9.62.


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
         TITLE              = "Migraci�n de datos de Famailla"
         HEIGHT             = 9.71
         WIDTH              = 122.4
         MAX-HEIGHT         = 23.71
         MAX-WIDTH          = 122.4
         VIRTUAL-HEIGHT     = 23.71
         VIRTUAL-WIDTH      = 122.4
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



/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Migraci�n de datos de Famailla */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Migraci�n de datos de Famailla */
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
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Conectar a Bases de Datos */
DO:
  if not connected("industria") then
      do:
        connect -db industria -ld industria -N tcp -H tucuman1 -S industria no-error.
        if connected("industria") then message "Se conecto a la base Industria de Lavalle" view-as alert-box.
                                  else message "NO se conecto a la base Industria de Lavalle" view-as alert-box.
      end.
  else  message "Ya esta conectado a la base de Industria de Lavalle." view-as alert-box.  
  if not connected("famindust") then
      do:
        connect -db indust -ld famindust -N tcp -H famailla -S faindustsm no-error. 
        if connected("famindust") then message "Se conecto a la base Industria de Famailla" view-as alert-box.
                                  else message "NO se conecto a la base Industria de Famailla" view-as alert-box.
      end.
  else  message "Ya esta conectado a la base de Industria de Famailla." view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Migraci�n de datos */
DO:
if connected("famindust") and connected("industria") then 
    do:
        sucursal_actual = integer(sucursal:screen-value in frame F-Main). 
    
        if sucursal_actual = 96 then /* ESTOY PARADO EN LAVALLE */
            do:
               MESSAGE "Estoy en Lavalle y estoy trayendo datos de Famailla" view-as alert-box.
               sucursal_local = 95.  /* TENGO QUE TRAER LOS DATOS DE FAMAILLA */              
               run migracion_hacia_lavalle.
                
            end. 
        else                          
            do: 
                if sucursal_actual = 95 then /* ESTOY EN FAMAILLA */
                    do:                    
                        MESSAGE "Estoy en Famailla y estoy trayendo datos de Lavalle" view-as alert-box.
                        sucursal_local = 96.  /* TENGO QUE TRAER LOS DATOS DE LAVALLE */   
                        run migracion_hacia_famailla.
                    end.
                else message "Por favor ingrese la sucursal de Famailla o Lavalle (95 - 96)" view-as alert-box.
                
                
            end. 
    end. 
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
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
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 94.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             sucursal:HANDLE IN FRAME F-Main , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
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
  DISPLAY sucursal lista lista_mod registros 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-1 sucursal BUTTON-1 lista lista_mod BUTTON-2 BUTTON-3 registros 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Migracion W-Win 
PROCEDURE Migracion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE migracion_hacia_famailla W-Win 
PROCEDURE migracion_hacia_famailla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

                /* LOTES DE JUGO */
                for each general.lotes_jugo no-lock where general.lotes_jugo.id_sucursal = sucursal_local
                                                        and general.lotes_jugo.fecha > date("01/01/01"):
                    
                    
                    find first famindust.lotes_jugo where 
                       famindust.lotes_jugo.id_empresa      = general.lotes_jugo.id_empresa and
                       famindust.lotes_jugo.id_sucursal     = general.lotes_jugo.id_sucursal and
                       famindust.lotes_jugo.id_tipotambor   = general.lotes_jugo.id_tipotambor and
                       famindust.lotes_jugo.nromov          = general.lotes_jugo.nromov no-error.
                    
                    if available famindust.lotes_jugo then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
                        do:
                        
                        end.
                    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
                        do:
                           create famindust.lotes_jugo.
                           buffer-copy general.lotes_jugo to famindust.lotes_jugo.
                           i = i + 1. 
                           registros:screen-value in frame F-Main = string(i).
                           
                           /* AHORA CREO LOS TAMBORES DE ESE LOTE */
                           for each general.tambores_industria of general.lotes_jugo no-lock.
                                create famindust.tambores_industria.
                                buffer-copy general.tambores_industria to famindust.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* ESTOY CREANDO LAS COMPOSICIONES DE LOS LOTES */
                           for each general.composicion_lote where 
                              general.composicion_lote.id_empresa = general.lotes_jugo.id_empresa and
                              general.composicion_lote.id_sucursal = general.lotes_jugo.id_sucursal and
                              general.composicion_lote.id_tipotambor = general.lotes_jugo.id_tipotambor and
                              general.composicion_lote.nromov = general.lotes_jugo.nromov  no-lock.
                                create famindust.composicion_lote.
                                buffer-copy general.composicion_lote to famindust.composicion_lote.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* VOY A GRABAR LAS INSPECCIONES DE LOS LOTES */
                           for each general.inspecciones_lote of general.lotes_jugo no-lock.
                                create famindust.inspecciones_lote.
                                buffer-copy general.inspecciones_lote to famindust.inspecciones_lote.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* AHORA LE TOCA TRAER LOS SOBRANTE DE LOTE */
                           for each general.sobrante of general.lotes_jugo no-lock.
                                create famindust.sobrante.
                                buffer-copy general.sobrante to famindust.sobrante.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                                /* TAMBIEN LOS TAMBORES DEL SOBRANTE */
                                for each general.tambores_industria where 
                                    general.tambores_industria.id_empresa = general.sobrante.id_empresa and
                                    general.tambores_industria.id_sucursal = general.sobrante.id_sucursal and
                                    general.tambores_industria.id_tipotambor = general.sobrante.id_tipotambor_sobrante and
                                    general.tambores_industria.nromov = general.sobrante.nromov_sobrante no-lock.
                                
                                     create famindust.tambores_industria.
                                     buffer-copy general.tambores_industria to famindust.tambores_industria.
                                     i = i + 1. 
                                     registros:screen-value in frame F-Main = string(i).
                                end.
                                    
                           /* PERO NO NOS OLVIDEMOS DE LOS ARRASTRE DE LOTE */
                           for each general.arrastre_lote of general.lotes_jugo no-lock.
                                create famindust.arrastre_lote.
                                buffer-copy general.arrastre_lote to famindust.arrastre_lote.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                                /* TAMBIEN LOS TAMBORES DEL ARRASTRE */
                                for each general.tambores_industria where 
                                    general.tambores_industria.id_empresa = general.arrastre_lote.id_empresa and
                                    general.tambores_industria.id_sucursal = general.arrastre_lote.id_sucursal and
                                    general.tambores_industria.id_tipotambor = general.arrastre_lote.id_tipotambor_arrastre and
                                    general.tambores_industria.nromov = general.arrastre_lote.nromov_arrastre no-lock.
                                
                                     create famindust.tambores_industria.
                                     buffer-copy general.tambores_industria to famindust.tambores_industria.
                                     i = i + 1. 
                                     registros:screen-value in frame F-Main = string(i).
                                
                                end.
                        end.
                end.
                
                lista:INSERT("Lotes de Jugo - " + string(i),1).
                
                i = 0.
                /*
                /* LOTES DE ACEITE */
                for each general.lotes_aceite no-lock where general.lotes_aceite.id_sucursal = sucursal_local:
                    
                    find first famindust.lotes_aceite where 
                       famindust.lotes_aceite.id_empresa      = general.lotes_aceite.id_empresa and
                       famindust.lotes_aceite.id_sucursal     = general.lotes_aceite.id_sucursal and
                       famindust.lotes_aceite.id_tipotambor   = general.lotes_aceite.id_tipotambor and
                       famindust.lotes_aceite.nromov          = general.lotes_aceite.nromov no-error.
                    
                    if available famindust.lotes_aceite then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
                        do:
                        
                        end.
                    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
                        do:
                           create famindust.lotes_aceite.
                           buffer-copy general.lotes_aceite to famindust.lotes_aceite.
                           i = i + 1. 
                           registros:screen-value in frame F-Main = string(i). 
                           
                           /* AHORA CREO LOS TAMBORES DE ESE LOTE DE ACEITE*/
                           for each general.tambores_industria of general.lotes_aceite no-lock.
                                create famindust.tambores_industria.
                                buffer-copy general.tambores_industria to famindust.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* ESTOY CREANDO LAS COMPOSICIONES DE LOS LOTES DE ACEITE */
                           for each general.composicion_lote_aceite where 
                              general.composicion_lote_aceite.id_empresa      = general.lotes_aceite.id_empresa and
                              general.composicion_lote_aceite.id_sucursal     = general.lotes_aceite.id_sucursal and
                              general.composicion_lote_aceite.id_tipotambor   = general.lotes_aceite.id_tipotambor and
                              general.composicion_lote_aceite.nromov          = general.lotes_aceite.nromov  no-lock.
                                create famindust.composicion_lote_aceite.
                                buffer-copy general.composicion_lote_aceite to famindust.composicion_lote_aceite.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* AHORA LE TOCA TRAER LOS SOBRANTE DE LOTE */
                           for each general.sobrante_lotes_aceite of general.lotes_aceite no-lock.
                                create famindust.sobrante_lotes_aceite.
                                buffer-copy general.sobrante_lotes_aceite to famindust.sobrante_lotes_aceite.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                                /* TAMBIEN LOS TAMBORES DEL SOBRANTE */
                                for each general.tambores_industria where 
                                    general.tambores_industria.id_empresa     = general.sobrante_lotes_aceite.id_empresa and
                                    general.tambores_industria.id_sucursal    = general.sobrante_lotes_aceite.id_sucursal and
                                    general.tambores_industria.id_tipotambor  = general.sobrante_lotes_aceite.id_tipotambor_sobrante and
                                    general.tambores_industria.nromov         = general.sobrante_lotes_aceite.nromov_sobrante no-lock.
                                
                                     create famindust.tambores_industria.
                                     buffer-copy general.tambores_industria to famindust.tambores_industria.
                                     i = i + 1. 
                                     registros:screen-value in frame F-Main = string(i).
                                
                                end.
                           
                        end.
                end.
                
                lista:INSERT("Lotes de Aceite - " + string(i),2).
                
                i = 0.
                /* PRODUCCION DE JUGOS */
                for each general.produccion_jugo no-lock where general.produccion_jugo.id_sucursal = sucursal_local:
                    
                    find first famindust.produccion_jugo where 
                       famindust.produccion_jugo.id_empresa      = general.produccion_jugo.id_empresa and
                       famindust.produccion_jugo.id_sucursal     = general.produccion_jugo.id_sucursal and
                       famindust.produccion_jugo.id_tipotambor   = general.produccion_jugo.id_tipotambor and
                       famindust.produccion_jugo.nromov          = general.produccion_jugo.nromov no-error.
                    
                    if available famindust.produccion_jugo then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
                        do:
                        
                        end.
                    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
                        do:
                           create famindust.produccion_jugo.
                           buffer-copy general.produccion_jugo to famindust.produccion_jugo.
                           i = i + 1. 
                           registros:screen-value in frame F-Main = string(i). 
                           
                           /* AHORA CREO LOS TAMBORES DE ESA PRODUCCION DE JUGO*/
                           for each general.tambores_industria of general.produccion_jugo no-lock.
                                create famindust.tambores_industria.
                                buffer-copy general.tambores_industria to famindust.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                        end.
                end.
                
                lista:INSERT("Producci�n de Jugo - " + string(i),3).
                
                i = 0.
                
                /* PRODUCCION DE ACEITE */
                for each general.tambores_industria where general.tambores_industria.id_tipotambor = 2 and
                                                            general.tambores_industria.id_sucursal = sucursal_local no-lock.
                    find first famindust.tambores_industria where 
                        famindust.tambores_industria.id_empresa      = general.tambores_industria.id_empresa and
                        famindust.tambores_industria.id_sucursal     = general.tambores_industria.id_sucursal and
                        famindust.tambores_industria.id_tipotambor   = general.tambores_industria.id_tipotambor and
                        famindust.tambores_industria.nromov          = general.tambores_industria.nromov and
                        famindust.tambores_industria.id_tambor       = general.tambores_industria.id_tambor no-error.    
                    
                    if available famindust.tambores_industria then
                        do:
                        
                        end.
                    else
                        do: /* NO EXISTE ENTONCES LO MIGRO */
                            create famindust.tambores_industria.
                            buffer-copy general.tambores_industria to famindust.tambores_industria.
                            i = i + 1. 
                            registros:screen-value in frame F-Main = string(i).
                        end.
                end.
                
                lista:INSERT("Producci�n de Aceite - " + string(i),4).
                
                i = 0.
                
                /* PRODUCTOS DE TERCEROS */
                for each general.productos_terceros no-lock where general.productos_terceros.id_sucursal = sucursal_local:
                    
                    find first famindust.productos_terceros where 
                       famindust.productos_terceros.id_empresa      = general.productos_terceros.id_empresa and
                       famindust.productos_terceros.id_sucursal     = general.productos_terceros.id_sucursal and
                       famindust.productos_terceros.id_proveedor    = general.productos_terceros.id_proveedor and
                       famindust.productos_terceros.id_articulo     = general.productos_terceros.id_articulo and
                       famindust.productos_terceros.id_tipotambor   = general.productos_terceros.id_tipotambor and
                       famindust.productos_terceros.nromov          = general.productos_terceros.nromov no-error.
                    
                    if available famindust.productos_terceros then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
                        do:
                        
                        end.
                    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
                        do:
                           create famindust.productos_terceros.
                           buffer-copy general.productos_terceros to famindust.productos_terceros.
                           i = i + 1. 
                           registros:screen-value in frame F-Main = string(i). 
                           
                           /* AHORA CREO LOS TAMBORES DE ESA PRODUCCION DE JUGO*/
                           for each general.tambores_industria of general.productos_terceros no-lock.
                                create famindust.tambores_industria.
                                buffer-copy general.tambores_industria to famindust.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                        end.
                end.
                
                lista:INSERT("Productos de Terceros - " + string(i),5).
                lista:insert("                     ",6).
                */
                
                lista:INSERT("Se termino el proceso de ",7).
                lista:INSERT("    Importacion!!!!!",8).
                lista:bgcolor = 8.
                disable lista.
                
                message "Se termin� la importaci�n de datos de Lavalle" view-as alert-box.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE migracion_hacia_lavalle W-Win 
PROCEDURE migracion_hacia_lavalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* LOTES DE JUGO */
                for each famindust.lotes_jugo no-lock where famindust.lotes_jugo.id_sucursal = sucursal_local
                                                        and famindust.lotes_jugo.fecha > date("01/01/01"):
                    
                    find first general.lotes_jugo where 
                       general.lotes_jugo.id_empresa      = famindust.lotes_jugo.id_empresa and
                       general.lotes_jugo.id_sucursal     = famindust.lotes_jugo.id_sucursal and
                       general.lotes_jugo.id_tipotambor   = famindust.lotes_jugo.id_tipotambor and
                       general.lotes_jugo.nromov          = famindust.lotes_jugo.nromov no-error.
                    
                    if available general.lotes_jugo then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
                        do:
                            /*
                            buffer-compare famindust.lotes_jugo to general.lotes_jugo save result in v_resultado.
                            if not v_resultado then
                                do:
                                    buffer-copy famindust.lotes_jugo to general.lotes_jugo.
                                    j = j + 1.
                                end.
                           /* AHORA REBIZO LOS TAMBORES DE ESE LOTE */
                           for each famindust.tambores_industria of famindust.lotes_jugo no-lock.
                                create general.tambores_industria.
                                buffer-copy famindust.tambores_industria to general.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.     
                           
                           */
                        end.
                    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
                        do:
                           create general.lotes_jugo.
                           buffer-copy famindust.lotes_jugo to general.lotes_jugo.
                           i = i + 1. 
                           registros:screen-value in frame F-Main = string(i).
                           
                           /* AHORA CREO LOS TAMBORES DE ESE LOTE */
                           for each famindust.tambores_industria of famindust.lotes_jugo no-lock.
                                create general.tambores_industria.
                                buffer-copy famindust.tambores_industria to general.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* ESTOY CREANDO LAS COMPOSICIONES DE LOS LOTES */
                           for each famindust.composicion_lote where 
                              famindust.composicion_lote.id_empresa = famindust.lotes_jugo.id_empresa and
                              famindust.composicion_lote.id_sucursal = famindust.lotes_jugo.id_sucursal and
                              famindust.composicion_lote.id_tipotambor = famindust.lotes_jugo.id_tipotambor and
                              famindust.composicion_lote.nromov = famindust.lotes_jugo.nromov  no-lock.
                                create general.composicion_lote.
                                buffer-copy famindust.composicion_lote to general.composicion_lote.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* VOY A GRABAR LAS INSPECCIONES DE LOS LOTES */
                           for each famindust.inspecciones_lote of famindust.lotes_jugo no-lock.
                                create general.inspecciones_lote.
                                buffer-copy famindust.inspecciones_lote to general.inspecciones_lote.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* AHORA LE TOCA TRAER LOS SOBRANTE DE LOTE */
                           for each famindust.sobrante of famindust.lotes_jugo no-lock.
                                create general.sobrante.
                                buffer-copy famindust.sobrante to general.sobrante.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                                /* TAMBIEN LOS TAMBORES DEL SOBRANTE */
                                for each famindust.tambores_industria where 
                                    famindust.tambores_industria.id_empresa = famindust.sobrante.id_empresa and
                                    famindust.tambores_industria.id_sucursal = famindust.sobrante.id_sucursal and
                                    famindust.tambores_industria.id_tipotambor = famindust.sobrante.id_tipotambor_sobrante and
                                    famindust.tambores_industria.nromov = famindust.sobrante.nromov_sobrante no-lock.
                                
                                     create general.tambores_industria.
                                     buffer-copy famindust.tambores_industria to general.tambores_industria.
                                     i = i + 1. 
                                     registros:screen-value in frame F-Main = string(i).
                                end.
                                    
                           /* PERO NO NOS OLVIDEMOS DE LOS ARRASTRE DE LOTE */
                           for each famindust.arrastre_lote of famindust.lotes_jugo no-lock.
                                create general.arrastre_lote.
                                buffer-copy famindust.arrastre_lote to general.arrastre_lote.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                                /* TAMBIEN LOS TAMBORES DEL ARRASTRE */
                                for each famindust.tambores_industria where 
                                    famindust.tambores_industria.id_empresa = famindust.arrastre_lote.id_empresa and
                                    famindust.tambores_industria.id_sucursal = famindust.arrastre_lote.id_sucursal and
                                    famindust.tambores_industria.id_tipotambor = famindust.arrastre_lote.id_tipotambor_arrastre and
                                    famindust.tambores_industria.nromov = famindust.arrastre_lote.nromov_arrastre no-lock.
                                
                                     create general.tambores_industria.
                                     buffer-copy famindust.tambores_industria to general.tambores_industria.
                                     i = i + 1. 
                                     registros:screen-value in frame F-Main = string(i).
                                
                                end.
                        end.
                end.
                
                lista:INSERT("Lotes de Jugo - " + string(i),1).
                /*
                i = 0.
                
                /* LOTES DE ACEITE */
                for each famindust.lotes_aceite no-lock where famindust.lotes_aceite.id_sucursal = sucursal_local:
                    
                    find first general.lotes_aceite where 
                       general.lotes_aceite.id_empresa      = famindust.lotes_aceite.id_empresa and
                       general.lotes_aceite.id_sucursal     = famindust.lotes_aceite.id_sucursal and
                       general.lotes_aceite.id_tipotambor   = famindust.lotes_aceite.id_tipotambor and
                       general.lotes_aceite.nromov          = famindust.lotes_aceite.nromov no-error.
                    
                    if available general.lotes_aceite then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
                        do:
                        
                        end.
                    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
                        do:
                           create general.lotes_aceite.
                           buffer-copy famindust.lotes_aceite to general.lotes_aceite.
                           i = i + 1. 
                           registros:screen-value in frame F-Main = string(i). 
                           
                           /* AHORA CREO LOS TAMBORES DE ESE LOTE DE ACEITE*/
                           for each famindust.tambores_industria of famindust.lotes_aceite no-lock.
                                create general.tambores_industria.
                                buffer-copy famindust.tambores_industria to general.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* ESTOY CREANDO LAS COMPOSICIONES DE LOS LOTES DE ACEITE */
                           for each famindust.composicion_lote_aceite where 
                              famindust.composicion_lote_aceite.id_empresa      = famindust.lotes_aceite.id_empresa and
                              famindust.composicion_lote_aceite.id_sucursal     = famindust.lotes_aceite.id_sucursal and
                              famindust.composicion_lote_aceite.id_tipotambor   = famindust.lotes_aceite.id_tipotambor and
                              famindust.composicion_lote_aceite.nromov          = famindust.lotes_aceite.nromov  no-lock.
                                create general.composicion_lote_aceite.
                                buffer-copy famindust.composicion_lote_aceite to general.composicion_lote_aceite.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                           /* AHORA LE TOCA TRAER LOS SOBRANTE DE LOTE */
                           for each famindust.sobrante_lotes_aceite of famindust.lotes_aceite no-lock.
                                create general.sobrante_lotes_aceite.
                                buffer-copy famindust.sobrante_lotes_aceite to general.sobrante_lotes_aceite.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                                /* TAMBIEN LOS TAMBORES DEL SOBRANTE */
                                for each famindust.tambores_industria where 
                                    famindust.tambores_industria.id_empresa     = famindust.sobrante_lotes_aceite.id_empresa and
                                    famindust.tambores_industria.id_sucursal    = famindust.sobrante_lotes_aceite.id_sucursal and
                                    famindust.tambores_industria.id_tipotambor  = famindust.sobrante_lotes_aceite.id_tipotambor_sobrante and
                                    famindust.tambores_industria.nromov         = famindust.sobrante_lotes_aceite.nromov_sobrante no-lock.
                                
                                     create general.tambores_industria.
                                     buffer-copy famindust.tambores_industria to general.tambores_industria.
                                     i = i + 1. 
                                     registros:screen-value in frame F-Main = string(i).
                                
                                end.
                           
                        end.
                end.
                
                lista:INSERT("Lotes de Aceite - " + string(i),2).
                
                i = 0.
                /* PRODUCCION DE JUGOS */
                for each famindust.produccion_jugo no-lock where famindust.produccion_jugo.id_sucursal = sucursal_local:
                    
                    find first general.produccion_jugo where 
                       general.produccion_jugo.id_empresa      = famindust.produccion_jugo.id_empresa and
                       general.produccion_jugo.id_sucursal     = famindust.produccion_jugo.id_sucursal and
                       general.produccion_jugo.id_tipotambor   = famindust.produccion_jugo.id_tipotambor and
                       general.produccion_jugo.nromov          = famindust.produccion_jugo.nromov no-error.
                    
                    if available general.produccion_jugo then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
                        do:
                        
                        end.
                    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
                        do:
                           create general.produccion_jugo.
                           buffer-copy famindust.produccion_jugo to general.produccion_jugo.
                           i = i + 1. 
                           registros:screen-value in frame F-Main = string(i). 
                           
                           /* AHORA CREO LOS TAMBORES DE ESA PRODUCCION DE JUGO*/
                           for each famindust.tambores_industria of famindust.produccion_jugo no-lock.
                                create general.tambores_industria.
                                buffer-copy famindust.tambores_industria to general.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                        end.
                end.
                
                lista:INSERT("Producci�n de Jugo - " + string(i),3).
                
                i = 0.
                
                /* PRODUCCION DE ACEITE */
                for each famindust.tambores_industria where famindust.tambores_industria.id_tipotambor = 2 and
                                                            famindust.tambores_industria.id_sucursal = sucursal_local no-lock.
                    find first general.tambores_industria where 
                        general.tambores_industria.id_empresa      = famindust.tambores_industria.id_empresa and
                        general.tambores_industria.id_sucursal     = famindust.tambores_industria.id_sucursal and
                        general.tambores_industria.id_tipotambor   = famindust.tambores_industria.id_tipotambor and
                        general.tambores_industria.nromov          = famindust.tambores_industria.nromov and
                        general.tambores_industria.id_tambor       = famindust.tambores_industria.id_tambor no-error.    
                    
                    if available general.tambores_industria then
                        do:
                        
                        end.
                    else
                        do: /* NO EXISTE ENTONCES LO MIGRO */
                            create general.tambores_industria.
                            buffer-copy famindust.tambores_industria to general.tambores_industria.
                            i = i + 1. 
                            registros:screen-value in frame F-Main = string(i).
                        end.
                end.
                
                lista:INSERT("Producci�n de Aceite - " + string(i),4).
                
                i = 0.
                
                /* PRODUCTOS DE TERCEROS */
                for each famindust.productos_terceros no-lock where famindust.productos_terceros.id_sucursal = sucursal_local:
                    
                    find first general.productos_terceros where 
                       general.productos_terceros.id_empresa      = famindust.productos_terceros.id_empresa and
                       general.productos_terceros.id_sucursal     = famindust.productos_terceros.id_sucursal and
                       general.productos_terceros.id_proveedor    = famindust.productos_terceros.id_proveedor and
                       general.productos_terceros.id_articulo     = famindust.productos_terceros.id_articulo and
                       general.productos_terceros.id_tipotambor   = famindust.productos_terceros.id_tipotambor and
                       general.productos_terceros.nromov          = famindust.productos_terceros.nromov no-error.
                    
                    if available general.productos_terceros then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
                        do:
                        
                        end.
                    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
                        do:
                           create general.productos_terceros.
                           buffer-copy famindust.productos_terceros to general.productos_terceros.
                           i = i + 1. 
                           registros:screen-value in frame F-Main = string(i). 
                           
                           /* AHORA CREO LOS TAMBORES DE ESA PRODUCCION DE JUGO*/
                           for each famindust.tambores_industria of famindust.productos_terceros no-lock.
                                create general.tambores_industria.
                                buffer-copy famindust.tambores_industria to general.tambores_industria.
                                i = i + 1. 
                                registros:screen-value in frame F-Main = string(i).
                           end.
                           
                        end.
                end.
                
                lista:INSERT("Productos de Terceros - " + string(i),5).
                lista:insert("                     ",6).
                */
                
                lista:INSERT("Se termino el proceso de ",7).
                lista:INSERT("    Importacion!!!!!",8).
                lista:bgcolor = 8.
                disable lista.
                
                message "Se termin� la importaci�n de datos de Famailla" view-as alert-box.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
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


