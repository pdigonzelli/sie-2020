&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEFINE VAR r AS ROWID.
/* Local Variable Definitions ---                                       */

DEFINE OUTPUT PARAMETER v_id_programa_despacho AS INTEGER.
DEFINE OUTPUT PARAMETER v_id_cliente AS INTEGER.
DEFINE OUTPUT PARAMETER v_cliente AS CHAR.
DEFINE OUTPUT PARAMETER v_id_articulo AS INTEGER.
DEFINE OUTPUT PARAMETER v_articulo AS CHAR.
DEFINE OUTPUT PARAMETER v_id_marca AS INTEGER.
DEFINE OUTPUT PARAMETER v_marca AS CHAR.
DEFINE OUTPUT PARAMETER v_id_calidad AS INTEGER.
DEFINE OUTPUT PARAMETER v_calidad AS CHAR.
DEFINE OUTPUT PARAMETER v_id_envase AS INTEGER.
DEFINE OUTPUT PARAMETER v_envase AS CHAR.
DEFINE OUTPUT PARAMETER v_pallets AS DECIMAL.
DEFINE OUTPUT PARAMETER v_cajas_por_pallets AS DECIMAL.
DEFINE OUTPUT PARAMETER v_kilos_por_cajas AS DECIMAL.
DEFINE OUTPUT PARAMETER v_importe_final AS DECIMAL.
DEFINE OUTPUT PARAMETER v_id_moneda AS INTEGER.
DEFINE OUTPUT PARAMETER v_id_tipo_unidad_venta AS INTEGER.

/**********EMPIEZA CON PARAMETROS*********/
&SCOPED-DEFINE CON-PARAMETROS YES 
/**********TERMINA CON PARAMETROS*********/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS aceptar salir RECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_items_programa_despacho_oq AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_programa_despacho AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON aceptar 
     IMAGE-UP FILE "custom/imagen/check":U
     IMAGE-INSENSITIVE FILE "custom/imagen/check":U NO-CONVERT-3D-COLORS
     LABEL "&Aceptar" 
     SIZE 10 BY 1 TOOLTIP "Aceptar tarea".

DEFINE BUTTON salir 
     IMAGE-UP FILE "custom/imagen/cross":U
     IMAGE-INSENSITIVE FILE "custom/imagen/cross":U NO-CONVERT-3D-COLORS
     LABEL "&Salir" 
     SIZE 10 BY 1 TOOLTIP "Abandonar Tarea".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     aceptar AT ROW 16.95 COL 116
     salir AT ROW 16.95 COL 128
     RECT-1 AT ROW 16.71 COL 113
     SPACE(10.99) SKIP(0.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Consultas - Gessi - Grupo Sauken".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/contenedor.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" D-Dialog _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Consultas - Gessi - Grupo Sauken */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL aceptar D-Dialog
ON CHOOSE OF aceptar IN FRAME D-Dialog /* Aceptar */
DO:
  define var rresult as rowid no-undo.  
  run devuelve-datos-programa IN h_b_items_programa_despacho_oq (OUTPUT v_id_programa_despacho,
                                                                 OUTPUT v_id_cliente ,
                                                                 OUTPUT v_cliente ,
                                                                 OUTPUT v_id_articulo ,
                                                                 OUTPUT v_articulo ,
                                                                 OUTPUT v_id_marca ,
                                                                 OUTPUT v_marca ,
                                                                 OUTPUT v_id_calidad ,
                                                                 OUTPUT v_calidad ,
                                                                 OUTPUT v_id_envase ,
                                                                 OUTPUT v_envase ,
                                                                 OUTPUT v_pallets ,
                                                                 OUTPUT v_cajas_por_pallets ,
                                                                 OUTPUT v_kilos_por_cajas,
                                                                 OUTPUT v_importe_final ,
                                                                 OUTPUT v_id_moneda ,
                                                                 OUTPUT v_id_tipo_unidad_venta ).


  APPLY "END-ERROR":U TO frame d-dialog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL salir D-Dialog
ON CHOOSE OF salir IN FRAME D-Dialog /* Salir */
DO:
  APPLY "END-ERROR":U TO frame d-dialog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualiza_items D-Dialog 
PROCEDURE actualiza_items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN dispatch IN h_b_items_programa_despacho_oq ('open-query':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-busca-rowid D-Dialog 
PROCEDURE adm-busca-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run devuelve-valor (output r).
apply "close" to this-procedure.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
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
             INPUT  '../industria/b_programa_despacho.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_programa_despacho ).
       RUN set-position IN h_b_programa_despacho ( 1.24 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b_programa_despacho ( 6.71 , 102.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_items_programa_despacho_oq.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_items_programa_despacho_oq ).
       RUN set-position IN h_b_items_programa_despacho_oq ( 8.38 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b_items_programa_despacho_oq ( 6.71 , 144.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_programa_despacho ,
             aceptar:HANDLE , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_items_programa_despacho_oq ,
             h_b_programa_despacho , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros D-Dialog 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame-rowid-programa D-Dialog 
PROCEDURE dame-rowid-programa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER r AS ROWID.
RUN get-rowid1 IN h_b_programa_despacho (OUTPUT r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-valor D-Dialog 
PROCEDURE devuelve-valor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter cresult as rowid no-undo.
define var c as character no-undo.
define var h as handle no-undo.
define var r as rowid no-undo.

run get-link-handle in adm-broker-hdl ( input this-procedure , input 'CONSULTA-TARGET' , output c).
h=widget-handle(c).

if valid-handle(h) then
do:
    run get-attribute in h (input 'TYPE':U).
    if return-value = 'csmartbrowser' then
    do:
        run devuelve-rowid in h (output r).
        cresult = r.
    end.
    else
        cresult = ?.    
end.        

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  ENABLE aceptar salir RECT-1 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry D-Dialog 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects D-Dialog 
PROCEDURE local-create-objects :
define var cresult as character no-undo.
define var h as handle no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Links to csmartbrowser h_b-table. */
  run get-link-handle in adm-broker-hdl ( input this-procedure,
                                          input 'Consulta-TARGET',
                                          output cresult).
  h=widget-handle(cresult).
  if valid-handle(h) then
    return.  
  
  run get-link-handle in adm-broker-hdl ( input this-procedure,
                                          input 'CONTAINER-TARGET',
                                          output cresult).
  h=widget-handle(cresult).
  if valid-handle(h) then
  do:                                        
    run get-attribute in h ('TYPE').
    if return-value = "csmartbrowser" then
    do:
        run add-link in adm-broker-hdl (input this-procedure, 'Consulta', h).
    end.
  end.  
    &IF DEFINED(CON-PARAMETROS) &THEN
        run asigna-parametros.
        run contenedor.
        run notify ('open-query').
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

