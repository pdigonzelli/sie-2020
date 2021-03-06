&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-4 FILL-IN-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_lotes_jugo_consulta AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_seleccion_locaciones AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_tambores_sobrante AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Movimiento de tambores" 
     SIZE 30 BY 1.43.

DEFINE BUTTON BUTTON-2 
     LABEL "Movimiento de un tambor" 
     SIZE 30 BY 1.43.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "LOTES DE JUGO" 
     VIEW-AS FILL-IN 
     SIZE 149.8 BY .95
     BGCOLOR 2 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "TAMBORES DE SOBRANTE" 
     VIEW-AS FILL-IN 
     SIZE 152.6 BY .95
     BGCOLOR 6 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "LOCACIONES" 
     VIEW-AS FILL-IN 
     SIZE 83.2 BY .95
     BGCOLOR 1 FGCOLOR 15 FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-1 AT ROW 1.24 COL 1.2 COLON-ALIGNED NO-LABEL
     FILL-IN-4 AT ROW 10.14 COL 2.2 NO-LABEL
     FILL-IN-5 AT ROW 18.62 COL 2.4 NO-LABEL
     BUTTON-1 AT ROW 19.1 COL 89
     BUTTON-2 AT ROW 20.76 COL 89
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 154.8 BY 24.43.


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
         TITLE              = "Sobrante"
         HEIGHT             = 24.43
         WIDTH              = 155
         MAX-HEIGHT         = 25.29
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 25.29
         VIRTUAL-WIDTH      = 160
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
ASSIGN 
       BUTTON-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
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
ON END-ERROR OF W-Win /* Sobrante */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Sobrante */
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
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Movimiento de tambores */
DO:
  define var r as rowid.
  define var r2 as rowid.
  define var hcon as handle.
  define var v_emp like locaciones.id_empresa.
  define var v_suc like locaciones.id_sucursal.
  define var v_loc like locaciones.id_locacion.

  
  run get-rowid1 in h_b_seleccion_locaciones (output r2).
  find locaciones where rowid(locaciones) = r2 no-lock no-error.
  if available locaciones then
    do:
        v_emp = locaciones.id_empresa.
        v_suc = locaciones.id_sucursal.
        v_loc = locaciones.id_locacion.
    end.
    
  run get-rowid1 in h_b_lotes_jugo_consulta (output r).
  find lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
  if available lotes_jugo then
    do:
        find sobrante of lotes_jugo no-lock no-error.
        if available sobrante then
            do:
                for each tambores_industria where tambores_industria.id_empresa     = sobrante.id_empresa
                                              and tambores_industria.id_sucursal    = sobrante.id_sucursal
                                              and tambores_industria.id_tipotambor  = sobrante.id_tipotambor_sobrante
                                              and tambores_industria.nromov         = sobrante.nromov_sobrante.

                        {i_mov_manuales.i}
                        
                end.                                              
            end.
    end.
  
run dispatch in h_b_tambores_sobrante ('open-query':U).  
message "Movimiento realizado" view-as alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Movimiento de un tambor */
DO:
  define var r as rowid.
  define var r2 as rowid.
  define var hcon as handle.
  define var v_emp like locaciones.id_empresa.
  define var v_suc like locaciones.id_sucursal.
  define var v_loc like locaciones.id_locacion.
     
  run get-rowid1 in h_b_seleccion_locaciones (output r2).
  find locaciones where rowid(locaciones) = r2 no-lock no-error.
  if available locaciones then
    do:
        v_emp = locaciones.id_empresa.
        v_suc = locaciones.id_sucursal.
        v_loc = locaciones.id_locacion.
    end.
    
  run tercera_tabla in h_b_tambores_sobrante (output r).
  
  find tambores_industria where rowid(tambores_industria) = r no-error.
  if available tambores_industria then
    do:
        {i_mov_manuales.i} 
    end.                                              


run dispatch in h_b_tambores_sobrante ('open-query':U).    
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
             INPUT  '../industria/b_lotes_jugo_consulta.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_lotes_jugo_consulta ).
       RUN set-position IN h_b_lotes_jugo_consulta ( 2.52 , 3.40 ) NO-ERROR.
       /* Size in UIB:  ( 7.38 , 150.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_tambores_sobrante.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_tambores_sobrante ).
       RUN set-position IN h_b_tambores_sobrante ( 11.19 , 1.60 ) NO-ERROR.
       /* Size in UIB:  ( 6.95 , 153.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 18.81 , 126.20 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_seleccion_locaciones.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_seleccion_locaciones ).
       RUN set-position IN h_b_seleccion_locaciones ( 19.67 , 2.40 ) NO-ERROR.
       /* Size in UIB:  ( 5.48 , 83.00 ) */

       /* Links to csmartbrowser h_b_tambores_sobrante. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo_consulta , 'Record':U , h_b_tambores_sobrante ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_lotes_jugo_consulta ,
             FILL-IN-1:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_tambores_sobrante ,
             FILL-IN-4:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             FILL-IN-5:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_seleccion_locaciones ,
             BUTTON-1:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  DISPLAY FILL-IN-1 FILL-IN-4 FILL-IN-5 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-1 BUTTON-2 
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


