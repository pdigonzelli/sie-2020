&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
define var alta as logical no-undo initial false.
DEFINE TEMP-TABLE tt_stock LIKE repstockr RCODE-INFORMATION.

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
&Scoped-Define ENABLED-OBJECTS fi-tipomov fi-sucursal fi-articulo fi-lote ~
fi-ano fi-desde fi-hasta fi-detallado fi-ceros fi-auditoria BUTTON-35 ~
BUTTON-40 BUTTON-36 nombre_articulo RECT-4 
&Scoped-Define DISPLAYED-OBJECTS fi-tipomov fi-sucursal fi-articulo fi-lote ~
fi-ano fi-desde fi-hasta fi-detallado fi-ceros fi-auditoria fi-descripcion ~
fi-nomsuc nombre_articulo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-35 
     LABEL "Reporte" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-36 
     LABEL "Salir" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-40 
     LABEL "Excel" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE fi-ano AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Ingrese Año" NO-UNDO.

DEFINE VARIABLE fi-articulo AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 TOOLTIP "Ingrese Cod. Articulo" NO-UNDO.

DEFINE VARIABLE fi-ceros AS LOGICAL FORMAT "Si/No":U INITIAL NO 
     LABEL "Visualiza Stock en 0" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Visualiza Stock en Cero?" NO-UNDO.

DEFINE VARIABLE fi-descripcion AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .62
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Movimientos Desde" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Ingrese desde cuando quiere analizar los movimientos" NO-UNDO.

DEFINE VARIABLE fi-detallado AS LOGICAL FORMAT "Si/No":U INITIAL NO 
     LABEL "Detalla Movimientos" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Detalla Movimientos de Stock" NO-UNDO.

DEFINE VARIABLE fi-hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Stock a" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Ingrese la Fecha Tope de Control de Stock" NO-UNDO.

DEFINE VARIABLE fi-lote AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Ingrese Lote" NO-UNDO.

DEFINE VARIABLE fi-nomsuc AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 45 BY .62
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-sucursal AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "Ingrese Cod. de Sucursal" NO-UNDO.

DEFINE VARIABLE fi-tipomov AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Tipo de Movimiento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Ingrese el Tipo de Movimiento" NO-UNDO.

DEFINE VARIABLE nombre_articulo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 50 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 14.76.

DEFINE VARIABLE fi-auditoria AS LOGICAL INITIAL no 
     LABEL "Consistencia de Movimientos" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 TOOLTIP "Marque para Controlar Errores" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-tipomov AT ROW 3.14 COL 22 COLON-ALIGNED
     fi-sucursal AT ROW 4.57 COL 22 COLON-ALIGNED
     fi-articulo AT ROW 6 COL 22 COLON-ALIGNED
     fi-lote AT ROW 7.19 COL 22 COLON-ALIGNED
     fi-ano AT ROW 7.19 COL 45 COLON-ALIGNED
     fi-desde AT ROW 8.86 COL 22 COLON-ALIGNED
     fi-hasta AT ROW 8.86 COL 48 COLON-ALIGNED
     fi-detallado AT ROW 10.29 COL 22 COLON-ALIGNED
     fi-ceros AT ROW 10.29 COL 58 COLON-ALIGNED
     fi-auditoria AT ROW 13.62 COL 7
     BUTTON-35 AT ROW 13.62 COL 41
     BUTTON-40 AT ROW 13.62 COL 54
     BUTTON-36 AT ROW 13.62 COL 65
     fi-descripcion AT ROW 3.14 COL 37 COLON-ALIGNED NO-LABEL
     fi-nomsuc AT ROW 4.57 COL 32 COLON-ALIGNED NO-LABEL
     nombre_articulo AT ROW 6 COL 32 COLON-ALIGNED NO-LABEL
     RECT-4 AT ROW 1 COL 2
     "Reporte de Stock a Fecha (Nuevo Sistema)" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.71 COL 6
          FGCOLOR 1 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87 BY 18.86.


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
         TITLE              = "REPORTE DE MOVIMIENTOS DE STOCK"
         HEIGHT             = 15
         WIDTH              = 87.6
         MAX-HEIGHT         = 21.24
         MAX-WIDTH          = 107
         VIRTUAL-HEIGHT     = 21.24
         VIRTUAL-WIDTH      = 107
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
/* SETTINGS FOR FILL-IN fi-descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nomsuc IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* REPORTE DE MOVIMIENTOS DE STOCK */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* REPORTE DE MOVIMIENTOS DE STOCK */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-35 W-Win
ON CHOOSE OF BUTTON-35 IN FRAME F-Main /* Reporte */
DO:
  define vari v_general as character.
  define vari vceros as logical.
  define vari vdetallado as logical.

  for each repstockr. delete repstockr. end.
  for each repstock. delete repstock. end.

  if fi-ceros:screen-value = "Si" then vceros = true.
                                  else vceros = false.
  if fi-detallado:screen-value = "Si" then vdetallado = true.
                                  else vdetallado = false.
  

  run y_proceso_stock_f.p (input fi-tipomov:screen-value,
                           input fi-sucursal:screen-value,
                           input fi-articulo:screen-value,
                           input fi-lote:screen-value,
                           input fi-ano:screen-value,
                           input fi-desde:screen-value,
                           input fi-hasta:screen-value,
                           input vdetallado,
                           input vceros).
                           
    v_general = string(fi-desde:screen-value) + ";" + 
                string(fi-hasta:screen-value) + ";" +
                string(fi-tipomov:screen-value) + ";" +
                string(fi-descripcion:screen-value) + ";" +
                string(fi-sucursal:screen-value) + ";" +
                string(fi-nomsuc:screen-value).
                
    if fi-auditoria:screen-value = "Yes" then do:
      for each repstockr where repstockr.cant_tmb = repstockr.vcontrol. 
        delete repstockr. 
      end.
      vdetallado = false.
    end.
    if vdetallado then
        run p_reportes.p (input "stock_fecha_detallado", 
                          input "STOCK A FECHA...",
                          input "",
                          input v_general).                            
    else
        run p_reportes.p (input "stock_fecha_resumido", 
                          input "STOCK A FECHA...",
                          input "",
                          input v_general).                            
                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-36
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-36 W-Win
ON CHOOSE OF BUTTON-36 IN FRAME F-Main /* Salir */
DO:
  apply "window-close" to current-window.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-40
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-40 W-Win
ON CHOOSE OF BUTTON-40 IN FRAME F-Main /* Excel */
DO:
  define vari v_general as character.
  define vari vceros as logical.
  define vari vdetallado as logical.

  for each repstockr. delete repstockr. end.
  for each repstock. delete repstock. end.

  if fi-ceros:screen-value = "Si" then vceros = true.
                                  else vceros = false.
  if fi-detallado:screen-value = "Si" then vdetallado = true.
                                  else vdetallado = false.
  

  run y_proceso_stock_f.p (input fi-tipomov:screen-value,
                           input fi-sucursal:screen-value,
                           input fi-articulo:screen-value,
                           input fi-lote:screen-value,
                           input fi-ano:screen-value,
                           input fi-desde:screen-value,
                           input fi-hasta:screen-value,
                           input vdetallado,
                           input vceros).
                           
    v_general = string(fi-desde:screen-value) + ";" + 
                string(fi-hasta:screen-value) + ";" +
                string(fi-tipomov:screen-value) + ";" +
                string(fi-descripcion:screen-value) + ";" +
                string(fi-sucursal:screen-value) + ";" +
                string(fi-nomsuc:screen-value).
                
    if fi-auditoria:screen-value = "Yes" then do:
      for each repstockr where repstockr.cant_tmb = repstockr.vcontrol. 
        delete repstockr. 
      end.
      vdetallado = false.
    end.
    /*
    if vdetallado then
        run p_reportes.p (input "stock_fecha_detallado", 
                          input "STOCK A FECHA...",
                          input "",
                          input v_general).                            
    else
        run p_reportes.p (input "stock_fecha_resumido", 
                          input "STOCK A FECHA...",
                          input "",
                          input v_general).                            
    */
    FOR EACH repstockr.
        CREATE tt_stock.
        BUFFER-COPY repstockr TO tt_stock.
    END.
    
    RUN generateExcel.p (INPUT TABLE tt_stock,
                     INPUT " Stock a Fecha",
                     INPUT " Nada" ,
                     INPUT 7,
                     INPUT 8,
                     INPUT "Century Gothic",
                     INPUT 7).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-articulo W-Win
ON LEAVE OF fi-articulo IN FRAME F-Main /* Articulo */
DO:
  find productos_terminados where productos_terminados.id_articulo =
                                  integer(fi-articulo:screen-value in frame F-Main)  no-lock no-error.
  if available productos_terminados then
    nombre_articulo:screen-value in frame F-Main = productos_terminados.descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-articulo W-Win
ON MOUSE-SELECT-DBLCLICK OF fi-articulo IN FRAME F-Main /* Articulo */
DO:
  define var r as rowid.
  
  run wc_articulos.w (output r).
  find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
  if available productos_terminados then
    fi-articulo:screen-value in frame F-Main = string(productos_terminados.id_articulo).
    nombre_articulo:screen-value in frame F-Main = productos_terminados.descripcion.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sucursal W-Win
ON LEAVE OF fi-sucursal IN FRAME F-Main /* Sucursal */
DO:
  find sucursales where sucursales.id_sucursal =  INTEGER(fi-sucursal:screen-value) no-lock no-error.
  if available sucursales then do:
     fi-sucursal:screen-value = string(sucursales.id_sucursal).
     fi-nomsuc:screen-value = sucursales.nombre.
  end.
  else do:
     fi-sucursal:screen-value = string(0).
     fi-nomsuc:screen-value = "TODOS".
  end. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sucursal W-Win
ON MOUSE-SELECT-DBLCLICK OF fi-sucursal IN FRAME F-Main /* Sucursal */
DO:
  define var r as rowid.
  
  run wc_sucursales.w (output r).
  find sucursales where rowid(sucursales) = r no-lock no-error.
  if available sucursales then do:
     fi-sucursal:screen-value = string(sucursales.id_sucursal).
     fi-nomsuc:screen-value = sucursales.nombre.
  end.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipomov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipomov W-Win
ON LEAVE OF fi-tipomov IN FRAME F-Main /* Tipo de Movimiento */
DO:
  find tipos_movimientos where 
    tipos_movimientos.id_tipo_movimiento = integer(fi-tipomov:screen-value)
    no-lock no-error.
  if available tipos_movimientos then do:
     fi-tipomov:screen-value = string(tipos_movimientos.id_tipo_movimiento).
     fi-descripcion:screen-value = tipos_movimientos.descripcion.
  end.
  else do:
     fi-tipomov:screen-value = string(0).
     fi-descripcion:screen-value = "TODOS".
  end.
    
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipomov W-Win
ON MOUSE-SELECT-DBLCLICK OF fi-tipomov IN FRAME F-Main /* Tipo de Movimiento */
DO:
  define var r as rowid.
  
  run c_tipo_movimiento.w (output r).
  find tipos_movimientos where rowid(tipos_movimientos) = r no-lock no-error.
  if available tipos_movimientos then do:
     fi-tipomov:screen-value = string(tipos_movimientos.id_tipo_movimiento).
     fi-descripcion:screen-value = tipos_movimientos.descripcion.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

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
  DISPLAY fi-tipomov fi-sucursal fi-articulo fi-lote fi-ano fi-desde fi-hasta 
          fi-detallado fi-ceros fi-auditoria fi-descripcion fi-nomsuc 
          nombre_articulo 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fi-tipomov fi-sucursal fi-articulo fi-lote fi-ano fi-desde fi-hasta 
         fi-detallado fi-ceros fi-auditoria BUTTON-35 BUTTON-40 BUTTON-36 
         nombre_articulo RECT-4 
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

   fi-tipomov:load-mouse-pointer ('glove') in frame {&frame-name}.
   fi-articulo:load-mouse-pointer ('glove') in frame {&frame-name}.
   fi-sucursal:load-mouse-pointer ('glove') in frame {&frame-name}.
   fi-desde = date("01/05/03").
   fi-hasta = today.

   displa fi-desde fi-hasta with frame {&frame-name}.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proceso_stock_f W-Win 
PROCEDURE proceso_stock_f :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

