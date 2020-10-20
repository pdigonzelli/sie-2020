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

define input parameter p_empresa as integer.
define input parameter p_sucursal as integer.
define input parameter p_locacion as integer.
define input parameter p_posicion as integer.

/* Local Variable Definitions ---                                       */

define temp-table tt_movi 
    field id_empresa as integer
    field id_sucursal as integer
    field id_movimiento as integer
    field id_tipo_movimiento as integer
    field id_tipo_mov_existente as integer.

define temp-table tt_etiqueta
    field etiqueta as char format "x(20)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-3 BUTTON-6 BUTTON-5 BUTTON-4 etiqueta 
&Scoped-Define DISPLAYED-OBJECTS etiqueta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_etiqueta_inventario AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-3 
     LABEL "Aceptar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "adeicon\exit-au":U
     LABEL "Button 4" 
     SIZE 11 BY 2.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "adeicon\repti":U
     LABEL "Button 5" 
     SIZE 11 BY 2.

DEFINE BUTTON BUTTON-6 
     LABEL "Limpiar" 
     SIZE 18 BY 1.91.

DEFINE VARIABLE etiqueta AS CHARACTER FORMAT "X(20)":U 
     LABEL "Etiqueta:" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-3 AT ROW 1.67 COL 53
     BUTTON-6 AT ROW 11.19 COL 22
     BUTTON-5 AT ROW 11.29 COL 52
     BUTTON-4 AT ROW 11.29 COL 64
     etiqueta AT ROW 1.76 COL 21 COLON-ALIGNED
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.95.


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
         TITLE              = "Etiquetas"
         HEIGHT             = 12.95
         WIDTH              = 80
         MAX-HEIGHT         = 27.67
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 27.67
         VIRTUAL-WIDTH      = 160
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   Custom                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Etiquetas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON ENTRY OF W-Win /* Etiquetas */
DO:
  apply 'entry' to etiqueta in frame F-main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Etiquetas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main W-Win
ON ENTRY OF FRAME F-Main
DO:
  apply 'entry' to etiqueta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Aceptar */
DO:
    define var v_etiqueta as char.
    define var emp as integer.
    define var suc as integer.
    define var lot as integer.
    define var tam as integer.
    define var tip as integer.
    define var art as integer.
    define var ind as integer.
    v_etiqueta = "".
    
    if LENGTH(etiqueta:screen-value, "CHARACTER") <= 7 then  
       do:  /************************************************/
            /*ACA ENTRA CUANDO SE INGRESA A MANO LA ETIQUETA*/
            /************************************************/
            v_etiqueta = etiqueta:screen-value.
            /* message v_etiqueta. */
       end.
    else 
        do:
            if LENGTH(etiqueta:screen-value, "CHARACTER") < 11 then 
                do:
                    /***************************************/
                    /*ACA ENTRA CUANDO SE LEE CON LA LECTORA LAS ETIQUETAS NUEVAS CON SOLO EL ID_ETIQUETA */
                    /***************************************/
                    v_etiqueta = substr(etiqueta:screen-value,4,7).
                    /* message v_etiqueta. */
                end.
            else 
                do:
                    /**************************************/
                    /*ACA SE ENTRA CUANDO SE LEE UNA ETIQUETA VIEJA CON EMP SUC LOT TAM ETC ETC*/
                    /**************************************/
                    tip = integer(substr(etiqueta:screen-value,2,2)).
                    if tip = 2 then
                        do:                                     /*ETIQUETAS DE PRODUCCION DE ACEITE*/
                            emp = integer(substr(etiqueta:screen-value,4,1)).
                            suc = integer(substr(etiqueta:screen-value,5,3)).
                            art = integer(substr(etiqueta:screen-value,8,3)).
                            tam = integer(substr(etiqueta:screen-value,11,5)).
                            /* message emp suc art tam tip view-as alert-box. */
                    
                            find tambores_industria where id_empresa = emp 
                                                      and id_sucursal = suc
                                                      and id_articulo = art
                                                      and id_tambor = tam
                                                      and id_tipotambor = tip no-lock no-error.

                            if available tambores_industria then v_etiqueta = string(tambores_industria.id_etiqueta).
                            /* message v_etiqueta. */
                        end.
                    else
                        do:
                            if tip <> 9 then
                              do:                        /*ETIQUETAS DE PRODUCTOS DE TERCEROS*/
                                emp = integer(substr(etiqueta:screen-value,4,1)).
                                suc = integer(substr(etiqueta:screen-value,5,3)).
                                lot = integer(substr(etiqueta:screen-value,8,5)).
                                tam = integer(substr(etiqueta:screen-value,13,3)).
                                /* message emp suc lot tam tip view-as alert-box. */
                        
                                find tambores_industria where id_empresa = emp 
                                                          and id_sucursal = suc
                                                          and id_lote = lot
                                                          and id_tambor = tam
                                                          and id_tipotambor = tip no-lock no-error.

                                if available tambores_industria then v_etiqueta = string(tambores_industria.id_etiqueta).
                                /* message v_etiqueta.     */
                              end.
                            else
                              do:                                    /*TODAS LAS DEMAS ETIQUETAS*/
                                emp = integer(substr(etiqueta:screen-value,4,1)).
                                suc = integer(substr(etiqueta:screen-value,5,3)).
                                ind = integer(substr(etiqueta:screen-value,8,7)).
                                /* message emp suc ind tip view-as alert-box. */
                        
                                find tambores_industria where id_empresa = emp 
                                                          and id_sucursal = suc
                                                          and indice_tambor = ind
                                                          and id_tipotambor = tip no-lock no-error.

                                if available tambores_industria then v_etiqueta = string(tambores_industria.id_etiqueta).
                                /* message v_etiqueta. */
                              end.
                              
                        end.
                            
                    
                end.
       end.
    
      
    
    run cargar_etiqueta in h_b_etiqueta_inventario (input v_etiqueta).
    RUN dispatch IN h_b_etiqueta_inventario ('open-query':U).
    etiqueta:screen-value = "".
    etiqueta:sensitive = false.
    etiqueta:sensitive = true.
    apply 'entry' to etiqueta.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Button 4 */
DO:
  apply ("window-close") to w-win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Button 5 */
DO:

/************************************************************************************************************/
/********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
/************************************************************************************************************/
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
define var v_filtro as character.

v_filtro = "locaciones.id_empresa = " + string(p_empresa) + " and " + 
           "locaciones.id_sucursal = " + string(p_sucursal) + " and " + 
           "locaciones.id_locacion = " + string(p_locacion) .



      RUN  aderb\_printrb(
       "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
       "registro_locaciones",                    /* RB-REPORT-NAME */
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
       "Registro de Locaciones",         /* RB-WINDOW-TITLE */
       yes,                           /* RB-DISPLAY-ERRORS */
       yes,                           /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "" /* RB-OTHER-PARAMETERS */
       ).   
       
/************************************************************************************************************/
    
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 W-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Limpiar */
DO:
  for each tambores_industria where tambores_industria.id_empresa_ubicacion = p_empresa
                                and tambores_industria.id_sucursal_ubicacion = p_sucursal
                                and tambores_industria.id_locacion_ubicacion = p_locacion
                                and tambores_industria.id_posicion_ubicacion = p_posicion.

       assign tambores_industria.id_empresa_ubicacion = 1
              tambores_industria.id_sucursal_ubicacion = 96
              tambores_industria.id_locacion_ubicacion = 99
              tambores_industria.id_posicion_ubicacion = 1.
   end.                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL etiqueta W-Win
ON RETURN OF etiqueta IN FRAME F-Main /* Etiqueta: */
DO:
  apply 'choose' to button-3.
  
END.

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
             INPUT  '../industria/b_etiqueta_inventario.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_etiqueta_inventario ).
       RUN set-position IN h_b_etiqueta_inventario ( 3.67 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.71 , 66.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_etiqueta_inventario ,
             BUTTON-3:HANDLE IN FRAME F-Main , 'AFTER':U ).
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar W-Win 
PROCEDURE cargar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar_etiqueta W-Win 
PROCEDURE cargar_etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter eti as char.

create tt_etiqueta.
assign tt_etiqueta.etiqueta = eti.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame-movimiento W-Win 
PROCEDURE dame-movimiento :
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
  DISPLAY etiqueta 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-3 BUTTON-6 BUTTON-5 BUTTON-4 etiqueta 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-parametros W-Win 
PROCEDURE get-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter emp as integer.
define output parameter suc as integer.
define output parameter loc as integer.
define output parameter pos as integer.

emp = p_empresa.
suc = p_sucursal.
loc = p_locacion.
pos = p_posicion.

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

  /* Code placed here will execute AFTER standard behavior.    */

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


