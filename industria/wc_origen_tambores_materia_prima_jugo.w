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
define input parameter r as rowid.

/* Local Variable Definitions ---                                       */
define var p_id_empresa as integer.
define var p_id_sucursal as integer.
define var p_id_tipotambor as integer.
define var p_nromov as integer.
define var articulo as integer.
define var fecha as date.

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
&Scoped-Define ENABLED-OBJECTS cantidad restantes BUTTON-1 BUTTON-2 RECT-3 ~
RECT-4 
&Scoped-Define DISPLAYED-OBJECTS cantidad restantes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bc_origen_tambores_materia_pri AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "adeicon\check":U
     LABEL "Button 1" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "adeicon\cross":U
     LABEL "Button 2" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cantidad AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Tambores" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE restantes AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Restantes" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 146 BY 16.1.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cantidad AT ROW 18.62 COL 54 COLON-ALIGNED
     restantes AT ROW 18.62 COL 77 COLON-ALIGNED
     BUTTON-1 AT ROW 18.71 COL 113
     BUTTON-2 AT ROW 18.71 COL 131
     RECT-3 AT ROW 1.95 COL 3
     RECT-4 AT ROW 18.24 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157.2 BY 19.43.


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
         TITLE              = "Selecci�n de Tambores de Producci�n"
         HEIGHT             = 19.43
         WIDTH              = 150.2
         MAX-HEIGHT         = 19.43
         MAX-WIDTH          = 157.2
         VIRTUAL-HEIGHT     = 19.43
         VIRTUAL-WIDTH      = 157.2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

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
ON END-ERROR OF W-Win /* Selecci�n de Tambores de Producci�n */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Selecci�n de Tambores de Producci�n */
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
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  define var elegidos as char.
  define var i as integer no-undo.
  define var v_tambores as integer no-undo.
  define var vr as rowid.
  
  FIND FIRST lotes_jugo where rowid(lotes_jugo) = r no-error.
  IF AVAILABLE lotes_jugo THEN DO:
      p_id_empresa       = lotes_jugo.id_empresa.
      p_id_sucursal      = lotes_jugo.id_sucursal.
      p_id_tipotambor    = lotes_jugo.id_tipotambor.
      p_nromov           = lotes_jugo.nromov.
  END.
  ELSE DO:
      FIND FIRST produccion_jugo WHERE ROWID(produccion_jugo) = r NO-ERROR.
      IF AVAILABLE produccion_jugo THEN DO:
          p_id_empresa       = produccion_jugo.id_empresa.
          p_id_sucursal      = produccion_jugo.id_sucursal.
          p_id_tipotambor    = produccion_jugo.id_tipotambor.
          p_nromov           = produccion_jugo.nromov.
      END.
      ELSE DO:
          FIND FIRST cargas WHERE ROWID(cargas) = r NO-ERROR.
          IF AVAILABLE cargas THEN DO:
              p_id_empresa = cargas.id_empresa.
              p_id_sucursal = cargas.id_sucursal.
              p_id_tipotambor = cargas.id_tipotambor.
              p_nromov = cargas.nromov.
          END.
      END.
  END.
      
  v_tambores = integer(cantidad:screen-value in frame F-Main).
  run get-rowid1 in h_bc_origen_tambores_materia_pri (output vr).
  
  MESSAGE "Eligio " v_tambores " tambores de produccion" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
                                                         UPDATE choice AS LOGICAL.
  CASE choice:
    WHEN TRUE THEN /* Yes */ DO:
        FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = vr NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_jugo THEN DO:
            DO i = 1 TO v_tambores:
                FIND FIRST tambores_industria WHERE tambores_industria.id_empresa        = lotes_jugo.id_empresa
                                                AND tambores_industria.id_sucursal       = lotes_jugo.id_sucursal
                                                AND tambores_industria.id_tipotambor     = lotes_jugo.id_tipotambor
                                                AND tambores_industria.nromov            = lotes_jugo.nromov
                                                AND tambores_industria.id_locacion_ubicacion = 4
                                                AND tambores_industria.id_sucursal_ubicacion = p_id_sucursal
                                                AND tambores_industria.id_articulo       > 900
                                                NO-ERROR.
                IF AVAILABLE tambores_industria THEN DO:
                    ASSIGN tambores_industria.id_empresa_destino = p_id_empresa
                           tambores_industria.id_sucursal_destino= p_id_sucursal
                           tambores_industria.id_tipotambor_destino = p_id_tipotambor
                           tambores_industria.nromov_destino = p_nromov
                           tambores_industria.id_locacion_ubicacion = 10.

                    RUN y_gstkcre.p (input tambores_industria.id_empresa,
                                     input tambores_industria.id_sucursal,
                                     input tambores_industria.id_tipotambor,
                                     input tambores_industria.nromov,
                                     INPUT tambores_industria.id_tambor,
                                     INPUT tambores_industria.id_tambor,
                                     input 16) "tambores_industria".
            
                    IF return-value <> "" then do:
                        MESSAGE "Error en el procesamiento de movimientos de stock" view-as alert-box.
                        RETURN "ADM-ERROR".
                    END.

                END.
                ELSE  MESSAGE "Se ha producido un error en la eleccion del tambor de origen. Avisar a sistemas."
                        VIEW-AS ALERT-BOX.  
                
                APPLY "window-close" TO h_bc_origen_tambores_materia_pri. 
            END.
        END.
    END.      
  END CASE.
    
  APPLY "WINDOW-CLOSE":U TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
  r=?.
  APPLY "END-ERROR":U TO frame {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualizo-campos W-Win 
PROCEDURE actualizo-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var p_r as rowid.
define var v_quedan as integer.
define var v_fecha as date.
define var v_suc as integer.
DEFINE VAR v_art AS INTEGER.

run get-rowid1 in h_bc_origen_tambores_materia_pri (output p_r).
run get-articulo (output v_fecha,
                  output v_suc,
                  OUTPUT v_art).
find lotes_jugo where rowid(lotes_jugo) = p_r no-lock no-error.
if available lotes_jugo then do:
        for each tambores_industria where tambores_industria.id_empresa             = lotes_jugo.id_empresa
                                      and tambores_industria.id_sucursal            = lotes_jugo.id_sucursal
                                      and tambores_industria.id_tipotambor          = lotes_jugo.id_tipotambor
                                      and tambores_industria.nromov                 = lotes_jugo.nromov
                                      AND tambores_industria.id_articulo            > 900
                                      and tambores_industria.id_sucursal_ubicacion  = v_suc
                                      and tambores_industria.id_locacion_ubicacion  = 4 no-lock.
            v_quedan = v_quedan + 1.
        end.
        restantes:screen-value in frame F-Main = string(v_quedan).
    end.
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
             INPUT  '../industria/bc_origen_tambores_materia_prima_jugo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_bc_origen_tambores_materia_pri ).
       RUN set-position IN h_bc_origen_tambores_materia_pri ( 2.43 , 6.00 ) NO-ERROR.
       RUN set-size IN h_bc_origen_tambores_materia_pri ( 15.14 , 140.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_bc_origen_tambores_materia_pri ,
             cantidad:HANDLE IN FRAME F-Main , 'BEFORE':U ).
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
  DISPLAY cantidad restantes 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE cantidad restantes BUTTON-1 BUTTON-2 RECT-3 RECT-4 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-articulo W-Win 
PROCEDURE get-articulo :
define output parameter fech as date.
define output parameter sucursal as integer.
define output parameter articulo as integer.


find lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
if available lotes_jugo then
    do:
        fech = lotes_jugo.fecha.  
        sucursal = lotes_jugo.id_sucursal.      
        articulo = lotes_jugo.id_articulo.
    end.
else
    do:
        find produccion_jugo where rowid(produccion_jugo) = r no-lock no-error.
        if available produccion_jugo then
            do:
                fech = produccion_jugo.fecha.  
                sucursal = produccion_jugo.id_sucursal.
                articulo = produccion_jugo.id_articulo.
            end.
        else
            do:
                find cargas where rowid(cargas) = r no-lock no-error.
                if available cargas then
                    do:
                        fech = cargas.fecha.  
                        sucursal = cargas.id_sucursal.
                        articulo = cargas.id_articulo.
                    end.
            end.
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

