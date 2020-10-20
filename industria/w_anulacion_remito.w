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

DEFINE INPUT PARAMETER p_sucursal AS INTEGER.
DEFINE INPUT PARAMETER p_tipo_comprobante AS INTEGER.

/* Local Variable Definitions ---                                       */
define var alta as logical no-undo initial false.
DEFINE VAR hProg AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-1 RECT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_anulacion_remitos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Anular Remito" 
     SIZE 41 BY 1.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 230 BY 50
     BGCOLOR 7 FGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 1.48 COL 71
     RECT-2 AT Y 0 X 15
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 131.6 BY 19.14.


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
         TITLE              = "Procesamiento de Remitos"
         HEIGHT             = 17.76
         WIDTH              = 131.6
         MAX-HEIGHT         = 19.14
         MAX-WIDTH          = 131.6
         VIRTUAL-HEIGHT     = 19.14
         VIRTUAL-WIDTH      = 131.6
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
ON END-ERROR OF W-Win /* Procesamiento de Remitos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Procesamiento de Remitos */
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
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Anular Remito */
DO:
  DEFINE VARIABLE r_remito AS ROWID      NO-UNDO.

  RUN get-rowid1 IN h_b_anulacion_remitos (OUTPUT r_remito).

  FIND FIRST remitos WHERE ROWID(remitos) = r_remito NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    FOR EACH items_factura OF remitos.
      IF items_factura.id_articulo <> 54 THEN
        RUN anulacionRemitoTambores.
      ELSE
        RUN anulacionRemitoCascara.
    END.
  END.
  
  
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
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.24 , 5.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 24.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 28.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_anulacion_remitos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_anulacion_remitos ).
       RUN set-position IN h_b_anulacion_remitos ( 3.86 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b_anulacion_remitos ( 12.86 , 126.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_anulacion_remitos. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_anulacion_remitos ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-navico ,
             BUTTON-1:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             h_cus-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b_anulacion_remitos ,
             BUTTON-1:HANDLE IN FRAME F-Main , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulacionRemitoCascara W-Win 
PROCEDURE anulacionRemitoCascara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE r_remito AS ROWID      NO-UNDO.

  
  DO TRANSACTION ON ERROR UNDO , LEAVE:
    RUN get-rowid1 IN h_b_anulacion_remitos (OUTPUT r_remito).
    FIND FIRST remitos WHERE ROWID(remitos) = r_remito NO-ERROR.
    IF AVAILABLE remitos THEN DO:
      FOR EACH items_factura OF remitos.
        /*CASCARA*/
        DEFINE VARIABLE vlAnul AS LOGICAL    NO-UNDO.
        FIND FIRST r_lote_cascara_remito WHERE r_lote_cascara_remito.nro_remito         = ITEMs_factura.nro
                                           AND r_lote_cascara_remito.id_sucursal_remito = ITEMs_factura.id_sucursal
                                           AND r_lote_cascara_remito.id_tipo_movsto     = ITEMs_factura.id_tipo_movsto
                                           AND r_lote_cascara_remito.ITEM_factura       = ITEMs_factura.ITEM
                                         NO-LOCK NO-ERROR.
        IF AVAILABLE r_lote_cascara_remito THEN DO:
          vlAnul = DYNAMIC-FUNCTION('anulacionRemitoCascara' IN hProg, r_lote_cascara_remito.nro_remito,
                                                                       r_lote_cascara_remito.id_sucursal_remito,
                                                                       r_lote_cascara_remito.id_tipo_movsto, 
                                                                       r_lote_cascara_remito.ITEM_factura, 
                                                                       r_lote_cascara_remito.id_empresa, 
                                                                       r_lote_cascara_remito.id_sucursal,
                                                                       r_lote_cascara_remito.id_tipotambor, 
                                                                       r_lote_cascara_remito.nromov).
          IF NOT vlAnul THEN 
            RETURN "error en anulacion de remito de cascara.".
        END.
        /*fin CASCARA*/
      END.
      /*aqui va la actualizacion al remito cambi a estado anulado*/
      FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = remitos.id_orden_entrega 
                                       AND items_orden_entrega.item_oe          = remitos.item_oe
                                     NO-ERROR.
      IF AVAILABLE items_orden_entrega THEN 
        ASSIGN items_orden_entrega.id_estado = 1.
            
      ASSIGN remitos.estado    = FALSE
             remitos.c_fecha   = TODAY
             remitos.c_usuario = USERID("userdb")
             remitos.c_hora    = STRING(TIME,"hh:mm:ss").
      MESSAGE "Se a anulado satisfactoriamente el remito " remitos.nro_comp VIEW-AS ALERT-BOX.
      RUN dispatch IN h_b_anulacion_remitos ('open-query').
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulacionRemitoTambores W-Win 
PROCEDURE anulacionRemitoTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR r_remito AS ROWID.
  DEFINE VAR v_nro_comp AS INTEGER.
  DEFINE VAR v_punto_venta AS INTEGER.
  DEFINE VAR v_nro_comprobante AS CHAR.
  DEFINE VAR v_sucursal_ubicacion AS INTEGER.
  DEFINE VAR v_nrocopia AS INTEGER.
  DEFINE VAR wpanta AS LOGICAL.
  DEFINE VAR v_respuesta AS LOGICAL.
  DEFINE VAR v_tipo_remito AS LOGICAL.
  DEFINE VAR v_suc_destino AS INTEGER.
  

  RUN get-rowid1 IN h_b_anulacion_remitos (OUTPUT r_remito).
  DO TRANSACTION ON ERROR UNDO , LEAVE:
  FIND FIRST remitos WHERE ROWID(remitos) = r_remito NO-ERROR.
  IF AVAILABLE remitos THEN DO:
      IF remitos.estado <> NO THEN DO:
        IF (remitos.id_tipocomp_compra <> 0 OR
            remitos.id_operacion_compra <> 0 OR
            remitos.nromov_compra <> 0) THEN DO:
            MESSAGE "NO SE PUEDE ANULAR, POR ESTAR YA FACTURADO O CONTROLADO.. !!!" VIEW-AS ALERT-BOX.
            RETURN "ADM-ERROR".
        END.
        ELSE DO:

            v_sucursal_ubicacion = remitos.id_sucursal.
            
            FOR EACH items_factura OF remitos.
                FIND FIRST tipostambor OF items_factura NO-LOCK NO-ERROR.
                IF tipostambor.tabla <> "tambores_industria" THEN DO:
                    FIND FIRST tambores_industria WHERE tambores_industria.id_sucursal_remito = items_factura.id_sucursal
                                                    AND tambores_industria.id_tipo_movsto     = items_factura.id_tipo_movsto
                                                    AND tambores_industria.nro_remito         = items_factura.nro
                                                    AND tambores_industria.ITEM_factura       = items_factura.ITEM
                                                    NO-LOCK NO-ERROR.
                    IF AVAILABLE tambores_industria THEN DO:
                        v_suc_destino = tambores_industria.id_sucursal_ubicacion.
                    END.
                    ELSE DO:
                        v_suc_destino = 85.
                    END.

                    RUN despachoRemitos IN hProg (INPUT tambores_industria.id_empresa,
                                                  INPUT tambores_industria.id_sucursal,
                                                  INPUT tambores_industria.id_tipotambor,
                                                  INPUT tambores_industria.nromov,
                                                  INPUT v_suc_destino,         /* SUC ORIGEN */
                                                  INPUT v_sucursal_ubicacion,  /* SUC DESTINO */
                                                  INPUT items_factura.fecha,
                                                  INPUT items_factura.cantidad,
                                                  INPUT items_factura.desde_lote,
                                                  INPUT items_factura.hasta_lote,
                                                  INPUT 4
                                                  ).
                    
                    /*
                    RUN ../industria/y_gstkrem_actualizado.p (INPUT remitos.id_sucursal,
                                                              INPUT remitos.id_tipo_movsto,
                                                              INPUT remitos.nro,
                                                              INPUT items_factura.item,
                                                              INPUT v_sucursal_ubicacion, /* SUC DESTINO */
                                                              INPUT v_suc_destino, /* SUC ORIGEN */
                                                              INPUT 4) "items_factura". */
                    IF RETURN-VALUE <> "" THEN DO:
                        MESSAGE "Error en el Procesamiento de Remitos" VIEW-AS ALERT-BOX.
                        RETURN "ADM-ERROR".
                    END.
                    FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_remito = items_factura.id_sucursal
                                                  AND tambores_industria.id_tipo_movsto     = items_factura.id_tipo_movsto
                                                  AND tambores_industria.nro_remito         = items_factura.nro
                                                  AND tambores_industria.ITEM_factura       = items_factura.ITEM
                                                  .
                        ASSIGN tambores_industria.id_sucursal_ubicacion = v_sucursal_ubicacion
                               tambores_industria.id_sucursal_remito    = 0
                               tambores_industria.id_tipo_movsto        = 0
                               tambores_industria.nro_remito            = 0
                               tambores_industria.ITEM_factura          = 0.
                    END.
                END.
                ELSE DO:
                    FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_remito = items_factura.id_sucursal
                                                  AND tambores_industria.id_tipo_movsto     = items_factura.id_tipo_movsto
                                                  AND tambores_industria.nro_remito         = items_factura.nro
                                                  AND tambores_industria.ITEM_factura       = items_factura.ITEM
                                                  .
                        IF tipostambor.tabla = "tambores_industria" THEN DO:
                            run ../industria/y_gstkrprod.p (input tambores_industria.id_empresa,
                                                            input tambores_industria.id_sucursal,
                                                            input tambores_industria.id_tipotambor,
                                                            input tambores_industria.nromov,
                                                            input tambores_industria.id_tambor,
                                                            input tambores_industria.id_tambor,
                                                            INPUT remitos.id_sucursal,
                                                            input tambores_industria.id_sucursal_ubicacion,
                                                            input 4).
        
                            IF RETURN-VALUE <> "" THEN DO:
                                MESSAGE "Error en el Procesamiento de Remitos" VIEW-AS ALERT-BOX.
                                /*RETURN "ADM-ERROR".*/
                            END.
                        END.
                         ASSIGN tambores_industria.id_sucursal_ubicacion = v_sucursal_ubicacion
                                tambores_industria.id_sucursal_remito    = 0
                                tambores_industria.id_tipo_movsto        = 0
                                tambores_industria.nro_remito            = 0
                                tambores_industria.ITEM_factura          = 0.
                    END.
                END.
            END.
    
            FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = remitos.id_orden_entrega 
                                             AND items_orden_entrega.item_oe          = remitos.item_oe
                                            NO-ERROR.
            IF AVAILABLE items_orden_entrega THEN 
                ASSIGN items_orden_entrega.id_estado = 1.
            
           ASSIGN remitos.estado    = FALSE
                  remitos.c_fecha   = TODAY
                  remitos.c_usuario = USERID("userdb")
                  remitos.c_hora    = STRING(TIME,"hh:mm:ss").
           MESSAGE "Se a anulado satisfactoriamente el remito " remitos.nro_comp VIEW-AS ALERT-BOX.
           RUN dispatch IN h_b_anulacion_remitos ('open-query').
        END.
      END.
      ELSE DO:
          MESSAGE "No se puede anular el remito " remitos.nro_comp " porque ya fue anulado por "
                   remitos.c_usuario " el dia " STRING(remitos.c_fecha) VIEW-AS ALERT-BOX.
      END.
  END.
 END.


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
  ENABLE BUTTON-1 RECT-2 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-sucursal W-Win 
PROCEDURE get-sucursal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER p_s AS INTEGER.
DEFINE OUTPUT PARAMETER p_tipo AS INTEGER.

p_s     = p_sucursal.
p_tipo  = p_tipo_comprobante.

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
    RUN libLotesUbicacion.p PERSISTENT SET hProg.
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

