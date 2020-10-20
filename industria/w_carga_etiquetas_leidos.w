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

DEFINE INPUT PARAMETER p_tipo_movimiento AS INTEGER.
DEFINE INPUT PARAMETER p_tipo_mov_existente AS INTEGER.

/* Local Variable Definitions ---                                       */
DEFINE VAR v_movi AS INTEGER.
DEFINE VAR VAR-DATO AS CHAR.

DEFINE VAR v_item AS INTEGER.
v_item = 1.

define temp-table tt_etiqueta 
    FIELD tipo_movimiento       AS INTEGER
    FIELD tipo_mov_existente    AS INTEGER
    FIELD id_sucursal           as integer
    field id_etiqueta           as integer
    field id_lote               as integer
    field id_tambor             as integer
    FIELD id_movimiento         AS INTEGER
    FIELD sucursal              AS CHAR FORMAT "x(20)"
    FIELD locacion              AS CHAR FORMAT "x(10)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_etiqueta

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt_etiqueta.id_sucursal tt_etiqueta.id_etiqueta tt_etiqueta.id_lote tt_etiqueta.id_tambor tt_etiqueta.sucursal tt_etiqueta.locacion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt_etiqueta NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt_etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt_etiqueta


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-8 BROWSE-1 BUTTON-7 etiqueta BUTTON-8 
&Scoped-Define DISPLAYED-OBJECTS etiqueta_seleccionada etiqueta ~
total_tambores 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-1 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-1 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-7 
     LABEL "Asignación de O.F. a un tambor seleccionado" 
     SIZE 47 BY 1.19.

DEFINE BUTTON BUTTON-8 
     LABEL "Borrar" 
     SIZE 29 BY .95.

DEFINE VARIABLE etiqueta AS INTEGER FORMAT "9999999":U INITIAL 0 
     LABEL "Etiqueta:" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE etiqueta_seleccionada AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 14 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE total_tambores AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 13 BY .95
     BGCOLOR 10 FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt_etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt_etiqueta.id_sucursal        COLUMN-LABEL "Sucursal"     FORMAT ">99"
      tt_etiqueta.id_etiqueta        COLUMN-LABEL "Etiqueta"     FORMAT "9999999"
      tt_etiqueta.id_lote            COLUMN-LABEL "Lote"         FORMAT ">999"
      tt_etiqueta.id_tambor          COLUMN-LABEL "Tambor"       FORMAT "99"
      tt_etiqueta.sucursal           COLUMN-LABEL "Sucursal Destino"
      tt_etiqueta.locacion           COLUMN-LABEL "Sucursal Locacion"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 125 BY 19.19
         BGCOLOR 10 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-1 AT ROW 2 COL 1
     BUTTON-7 AT ROW 21.24 COL 2
     etiqueta_seleccionada AT ROW 21.24 COL 48 COLON-ALIGNED NO-LABEL
     etiqueta AT ROW 21.24 COL 79 COLON-ALIGNED
     total_tambores AT ROW 21.24 COL 106 COLON-ALIGNED
     BUTTON-8 AT ROW 22.67 COL 2
     "LECTOR 5" VIEW-AS TEXT
          SIZE 16.8 BY .76 AT ROW 1.14 COL 59.6
          BGCOLOR 10 FONT 0
     RECT-8 AT ROW 1 COL 59
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126 BY 23.19.


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
         TITLE              = "LECTURA DE PALLETS"
         HEIGHT             = 23.05
         WIDTH              = 125.4
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
         FONT               = 8
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
                                                                        */
/* BROWSE-TAB BROWSE-1 RECT-8 F-Main */
/* SETTINGS FOR FILL-IN etiqueta_seleccionada IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN total_tambores IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_etiqueta NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-1 ASSIGN
       FRAME        = FRAME F-Main:HANDLE
       ROW          = 22.19
       COLUMN       = 65
       HEIGHT       = 1.33
       WIDTH        = 5.6
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME        = FRAME F-Main:HANDLE
       ROW          = 22.19
       COLUMN       = 118
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.

PROCEDURE adm-create-controls:
      CtrlFrame-1:NAME = "CtrlFrame-1":U .
/* CtrlFrame-1 OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {648A5600-2C6E-101B-82B6-000000000014} type: MSComm */
      CtrlFrame-1:MOVE-AFTER(total_tambores:HANDLE IN FRAME F-Main).
      CtrlFrame:MOVE-AFTER(CtrlFrame-1).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* LECTURA DE PALLETS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* LECTURA DE PALLETS */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON MOUSE-SELECT-CLICK OF BROWSE-1 IN FRAME F-Main
DO:
  etiqueta_seleccionada:SCREEN-VALUE IN FRAME F-Main = tt_etiqueta.id_etiqueta:SCREEN-VALUE IN BROWSE Browse-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 W-Win
ON CHOOSE OF BUTTON-7 IN FRAME F-Main /* Asignación de O.F. a un tambor seleccionado */
DO:
  DEFINE VAR r AS ROWID.
  DEFINE VAR h_con AS HANDLE.
  DEFINE VAR v_etiqueta AS INTEGER.

  v_etiqueta = INTEGER(etiqueta_seleccionada:SCREEN-VALUE IN FRAME F-Main).
  RUN wc_items_contratos.w (OUTPUT r).
  
  IF NOT r = ? THEN
  DO:
  
      FIND items_contratos WHERE ROWID(items_contratos) = r NO-LOCK NO-ERROR.
      IF AVAILABLE items_contratos THEN
      DO:
         
         FIND tambores_industria WHERE tambores_industria.id_etiqueta = v_etiqueta NO-ERROR.
         IF AVAILABLE tambores_industria THEN
         DO:
            ASSIGN tambores_industria.id_contrato_of = items_contratos.id_contrato
                   tambores_industria.id_tipocontrato_of = items_contratos.id_tipo_contrato
                   tambores_industria.anio_of = items_contratos.anio
                   tambores_industria.item_of = items_contratos.ITEM.
            MESSAGE "El tambor " v_etiqueta " se ha actualizado satisfactoriamente." VIEW-AS ALERT-BOX.   
         END.
         ELSE MESSAGE "No se encontró tambor con la etiqueta " v_etiqueta VIEW-AS ALERT-BOX.
      END.
      ELSE MESSAGE "No se seleccionó nunguna fecha de envio de O.F." VIEW-AS ALERT-BOX.

  END.
  ELSE MESSAGE "La selección es nula, por favor avise a Sistemas" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 W-Win
ON CHOOSE OF BUTTON-8 IN FRAME F-Main /* Borrar */
DO:
    DEFINE VAR v_etiqueta AS INTEGER.
    v_etiqueta = INTEGER(etiqueta_seleccionada:SCREEN-VALUE IN FRAME F-Main).
    MESSAGE "Está seguro que desea quitar el tambor " v_etiqueta " de la lista de despacho?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE choice AS LOGICAL.

      CASE choice:
         WHEN TRUE THEN /* Yes */
          DO:
            FIND tambores_transporte WHERE tambores_transporte.id_etiqueta = v_etiqueta NO-ERROR.
            IF AVAILABLE tambores_transporte THEN
                DELETE tambores_transporte.
            ELSE MESSAGE "No existe el tambor con el numero " v_etiqueta VIEW-AS ALERT-BOX INFORMATION.

          END.
         WHEN FALSE THEN /* No */
          DO:
             MESSAGE "Eliminación cancelada."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                    RETURN NO-APPLY.
          END.

         OTHERWISE /* Cancel */
             STOP.
         END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame W-Win
PROCEDURE CtrlFrame.MSComm.OnComm .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  define var i as integer no-undo.
  define var j as integer no-undo.
  define var c as character no-undo.
  DEFINE VAR C1 AS CHARACTER NO-UNDO.
  
  i = chCtrlFrame:MSComm:inBufferCount.
   
  IF chCtrlFrame:MSComm:CommEvent = 2 THEN
  DO:
    C  = chCtrlFrame:MSComm:Input .
    DO j = 1 To Length(C):
        VAR-DATO  = VAR-DATO + ENTRY(J , C).
        IF ENTRY(J , C) = 'Z'  then
        DO:
            /* MESSAGE substr(VAR-DATO,4,7) VIEW-AS ALERT-BOX. */
            RUN carga_tambor_movimiento(INPUT substr(VAR-DATO,4,7)).
            
            VAR-DATO = ''.
            chCtrlFrame:MSComm:InBufferCount = 0.
        END.        
    END.    
  END. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-1 W-Win
PROCEDURE CtrlFrame-1.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
{&open-query-browse-1}
/* {&open-query-browse-2} */

run totales.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL etiqueta W-Win
ON RETURN OF etiqueta IN FRAME F-Main /* Etiqueta: */
DO:
  run carga_tambor_movimiento (input integer(etiqueta:SCREEN-VALUE)).
  etiqueta:SCREEN-VALUE = "".
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga_tambor_movimiento W-Win 
PROCEDURE carga_tambor_movimiento :
DEFINE INPUT PARAMETER eti AS CHAR.
DEFINE var hcon as HANDLE.

find tipos_mov_existentes where tipos_mov_existentes.id_tipo_movimiento = p_tipo_movimiento and
                                tipos_mov_existentes.id_tipo_mov_existente = p_tipo_mov_existente no-lock no-error.
if available tipos_mov_existentes then
    do:   
      find tambores_industria where tambores_industria.id_etiqueta = integer(eti) no-error.
      if available tambores_industria then
       do:
             if tambores_industria.id_empresa_ubicacion <> tipos_mov_existentes.id_empresa_origen or
                tambores_industria.id_sucursal_ubicacion <> tipos_mov_existentes.id_sucursal_origen or
                tambores_industria.id_locacion_ubicacion <> tipos_mov_existentes.id_locacion_origen or
                tambores_industria.id_posicion_ubicacion <> tipos_mov_existentes.id_posicion_origen then
                message "Este tambor " eti " no estaba registrado estar en esta locación o posición" view-as alert-box.
                       
             assign tambores_industria.id_empresa_ubicacion = tipos_mov_existentes.id_empresa_destino
                    tambores_industria.id_sucursal_ubicacion = tipos_mov_existentes.id_sucursal_destino
                    tambores_industria.id_locacion_ubicacion = tipos_mov_existentes.id_locacion_destino
                    tambores_industria.id_posicion_ubicacion = tipos_mov_existentes.id_posicion_destino.
             
           /*  MESSAGE tipos_mov_existentes.id_empresa_destino tipos_mov_existentes.id_sucursal_destino tipos_mov_existentes.id_locacion_destino
                     tipos_mov_existentes.id_posicion_destino v_movi p_tipo_movimiento p_tipo_mov_existente 
                     " Ahora viene el item " v_item VIEW-AS ALERT-BOX.                  */
             /* FIND sucursales WHERE sucursales.id_sucursal = tambores_industria.id_sucursal_destino NO-ERROR NO-LOCK.
             FIND locaciones WHERE locaciones.id_locacion = tambores_industria.id_locacion_destino NO-ERROR NO-LOCK.
             create tt_etiqueta.
             assign  tt_etiqueta.id_sucursal            = tambores_industria.id_sucursal
                     tt_etiqueta.tipo_movimiento        = p_tipo_movimiento
                     tt_etiqueta.tipo_mov_existente     = p_tipo_mov_existente
                     tt_etiqueta.etiqueta               = eti
                     tt_etiqueta.id_lote                = tambores_industria.id_lote
                     tt_etiqueta.id_tambor              = tambores_industria.id_tambor
                     tt_etiqueta.id_movimiento          = v_movi
                     tt_etiqueta.sucursal               = sucursales.nombre
                     tt_etiqueta.locacion               = locaciones.descripcion.   

            create tt_movi.
            assign tt_movi.id_empresa = 1
                   tt_movi.id_sucursal = 95                    
                   tt_movi.id_movimiento = v_movi
                   tt_movi.id_tipo_movimiento = p1
                   tt_movi.id_tipo_mov_existente = p2. */
        
            if v_item = 1 then
             do:
                create movimientos_industria.
                assign movimientos_industria.id_empresa = 1
                       movimientos_industria.id_sucursal = 96
                       movimientos_industria.id_tipo_movimiento = p_tipo_movimiento
                       movimientos_industria.id_tipo_mov_existente = p_tipo_mov_existente
                       movimientos_industria.id_movimiento = v_movi
                       movimientos_industria.fecha = today
                       movimientos_industria.c_usuario = userid("userdb")
                       movimientos_industria.c_fecha = today
                       movimientos_industria.c_hora = string(time,"HH:MM:SS").
            end.
    
   
            create items_movimientos_industria.
            assign items_movimientos_industria.id_empresa = 1
                   items_movimientos_industria.id_sucursal = 96
                   items_movimientos_industria.id_tipo_movimiento = p_tipo_movimiento
                   items_movimientos_industria.id_tipo_mov_existente = p_tipo_mov_existente
                   items_movimientos_industria.id_movimiento = v_movi
                   items_movimientos_industria.fecha = today
                   items_movimientos_general.items_movimiento = v_item
                   items_movimientos_industria.id_empresa_origen = tipos_mov_existentes.id_empresa_origen
                   items_movimientos_industria.id_sucursal_origen = tipos_mov_existentes.id_sucursal_origen
                   items_movimientos_industria.id_locacion_origen = tipos_mov_existentes.id_locacion_origen
                   items_movimientos_industria.id_posicion_origen = tipos_mov_existentes.id_posicion_origen
                   items_movimientos_industria.id_empresa_destino = tipos_mov_existentes.id_empresa_destino
                   items_movimientos_industria.id_sucursal_destino = tipos_mov_existentes.id_sucursal_destino
                   items_movimientos_industria.id_locacion_destino = tipos_mov_existentes.id_locacion_destino
                   items_movimientos_industria.id_posicion_destino = tipos_mov_existentes.id_posicion_destino
                   items_movimientos_industria.id_etiqueta = integer(eti)
                   items_movimientos_industria.c_usuario = userid("userdb")
                   items_movimientos_industria.c_fecha = today
                   items_movimientos_industria.c_hora = string(time,"HH:MM:SS").       
            v_item = v_item + 1.
            
          end.
          else message "No se encontro la etiqueta " eti view-as alert-box.  
    end.

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load W-Win _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w_carga_etiquetas_leidos.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-1 = CtrlFrame-1:COM-HANDLE
    UIB_S = chCtrlFrame-1:LoadControls( OCXFile, "CtrlFrame-1":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "w_carga_etiquetas_leidos.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY etiqueta_seleccionada etiqueta total_tambores 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-8 BROWSE-1 BUTTON-7 etiqueta BUTTON-8 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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
   chCtrlFrame:MSComm:PortOpen = False.
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

        v_movi = next-value(movimientos_industria).

        chCtrlFrame:MSComm:CommPort = 1.
        chCtrlFrame:MSComm:Settings = "9600,N,8,1".
        chCtrlFrame:MSComm:InputLen = 1.
        chCtrlFrame:MSComm:RThreshold = 1. 
        chCtrlFrame:MSComm:PortOpen = True.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view W-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN TOTALES. 
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt_etiqueta"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totales W-Win 
PROCEDURE totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR v_TOTAL AS INTEGER.

FOR EACH tt_etiqueta:
    DELETE tt_etiqueta.
END.

v_total = 0.

FOR EACH items_movimientos_industria WHERE items_movimientos_industria.id_empresa = 1
                           AND items_movimientos_industria.id_sucursal = 96
                           AND items_movimientos_industria.id_tipo_movimiento = p_tipo_movimiento
                           AND items_movimientos_industria.id_tipo_mov_existente = p_tipo_mov_existente
                           AND items_movimientos_industria.id_movimiento = v_movi NO-LOCK.
    
    FIND tambores_industria where tambores_industria.id_etiqueta = 
                                  items_movimientos_industria.id_etiqueta NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN
    DO:
       FIND sucursales WHERE sucursales.id_sucursal = tambores_industria.id_sucursal_ubicacion NO-LOCK NO-ERROR.
       FIND locaciones WHERE locaciones.id_locacion = tambores_industria.id_locacion_ubicacion NO-LOCK NO-ERROR.
       create tt_etiqueta.
       assign  tt_etiqueta.id_sucursal            = tambores_industria.id_sucursal
               tt_etiqueta.tipo_movimiento        = p_tipo_movimiento
               tt_etiqueta.tipo_mov_existente     = p_tipo_mov_existente
               tt_etiqueta.id_etiqueta            = tambores_industria.id_etiqueta
               tt_etiqueta.id_lote                = tambores_industria.id_lote
               tt_etiqueta.id_tambor              = tambores_industria.id_tambor
               tt_etiqueta.id_movimiento          = v_movi
               tt_etiqueta.sucursal               = sucursales.nombre
               tt_etiqueta.locacion               = locaciones.descripcion. 
    END.
    v_total = v_total + 1.    
END.

total_tambores:SCREEN-VALUE IN FRAME F-Main = STRING(v_total).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


