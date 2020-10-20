&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RADIO-SET-2 B-generar B-borrar FI-empresa ~
T-empresa FI-cuadrilla T-cuadrilla Fi-fecha Fi-referencia 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-2 FI-empresa nombre-empresa ~
T-empresa FI-cuadrilla nombre-cuadrilla T-cuadrilla Fi-fecha Fi-referencia 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-borrar 
     LABEL "Borrar" 
     SIZE 16 BY 1.19.

DEFINE BUTTON B-generar 
     LABEL "&Generar" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-cuadrilla AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Cuadrilla" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-empresa AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Fi-fecha AS DATE FORMAT "99/99/99":U INITIAL ? 
     LABEL "Fecha de registro" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.19 NO-UNDO.

DEFINE VARIABLE Fi-referencia AS DATE FORMAT "99/99/99":U INITIAL ? 
     LABEL "Fecha de Referencia" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-cuadrilla AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1.19 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1.19 NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Lluvia", 1,
"Ausente sin aviso", 2
     SIZE 25 BY 1.91 NO-UNDO.

DEFINE VARIABLE T-cuadrilla AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-empresa AS LOGICAL INITIAL no 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RADIO-SET-2 AT ROW 1 COL 14 NO-LABEL WIDGET-ID 14
     B-generar AT ROW 1.24 COL 54 WIDGET-ID 6
     B-borrar AT ROW 1.24 COL 71 WIDGET-ID 28
     FI-empresa AT ROW 3.14 COL 12 COLON-ALIGNED WIDGET-ID 8
     nombre-empresa AT ROW 3.14 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     T-empresa AT ROW 3.43 COL 79 WIDGET-ID 22
     FI-cuadrilla AT ROW 4.57 COL 12 COLON-ALIGNED WIDGET-ID 10
     nombre-cuadrilla AT ROW 4.57 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     T-cuadrilla AT ROW 4.81 COL 79 WIDGET-ID 24
     Fi-fecha AT ROW 6.48 COL 23 COLON-ALIGNED WIDGET-ID 2
     Fi-referencia AT ROW 6.48 COL 64 COLON-ALIGNED WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93.2 BY 8.81 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Generacion de Causas de ausencia en Planillas de cosecha"
         HEIGHT             = 8.81
         WIDTH              = 93.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN nombre-cuadrilla IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-empresa IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Generacion de Causas de ausencia en Planillas de cosecha */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Generacion de Causas de ausencia en Planillas de cosecha */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-borrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-borrar wWin
ON CHOOSE OF B-borrar IN FRAME fMain /* Borrar */
DO:
    DEF VAR v_registro AS DATE.

    IF integer(radio-set-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 1 THEN
    DO:
        MESSAGE "Proceso de borrado s¢lo habilitado para " SKIP
                "generaci¢n de ausente con aviso" VIEW-AS ALERT-BOX WARNING.
        RETURN.
                 
    END.

    v_registro = DATE(fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

    IF v_registro < DATE("01/08/18") OR v_registro > date("15/08/18") THEN
    DO:
            MESSAGE "Proceso fuera de rango de prueba" VIEW-AS ALERT-BOX WARNING.
            RETURN.
        
    END.



    run  pborragenaus.p (INPUT INTEGER(radio-set-2:SCREEN-VALUE),
                    INPUT INTEGER(fi-empresa:SCREEN-VALUE), 
                    INPUT(fi-cuadrilla:SCREEN-VALUE),
                    INPUT date(fi-fecha:screen-value),
                    INPUT DATE(fi-referencia:SCREEN-VALUE)). 
    
    message 'Finaliza Proceso de borrado' view-as alert-box information.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-generar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-generar wWin
ON CHOOSE OF B-generar IN FRAME fMain /* Generar */
DO:
  define var ireg as integer no-undo.

  DEF VAR v_registro AS DATE.

  v_registro = DATE(fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  IF WEEKDAY(v_registro) = 7 THEN
  DO:
      MESSAGE "Proceso no habilitado para d¡a domingo" VIEW-AS ALERT-BOX WARNING.
      RETURN.
  END.
 
   IF v_registro < DATE("01/08/18") OR v_registro > date("15/08/18") THEN
    DO:
            MESSAGE "Proceso fuera de rango de prueba" VIEW-AS ALERT-BOX WARNING.
            RETURN.
        
    END.

    run  pgenaus.p (INPUT INTEGER(radio-set-2:SCREEN-VALUE),
                    INPUT INTEGER(fi-empresa:SCREEN-VALUE), 
                    INPUT(fi-cuadrilla:SCREEN-VALUE),
                    INPUT date(fi-fecha:screen-value),
                    INPUT DATE(fi-referencia:SCREEN-VALUE)). 
    
    message 'Finaliza Proceso de generaci¢n' view-as alert-box information.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-cuadrilla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-cuadrilla wWin
ON MOUSE-SELECT-DBLCLICK OF FI-cuadrilla IN FRAME fMain /* Cuadrilla */
DO:
    DEFINE VAR xFieldResult AS CHARACTER.

RUN adm2/support/gConsultas.w (INPUT "bliqcuadrillas.w",
                             INPUT "dliqcuadrillas.w",
                             INPUT "id_cuadrilla",
                             INPUT "liq_cuadrillas.id_empresa_cosechera = " + fi-empresa:SCREEN-VALUE, 
                             OUTPUT xfieldResult).
IF xFieldResult <> "" AND xFieldResult <> ? THEN
DO:
   fi-cuadrilla:SCREEN-VALUE = xfieldResult. 
   RUN descriptivos.
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-empresa wWin
ON MOUSE-SELECT-DBLCLICK OF FI-empresa IN FRAME fMain /* Empresa */
DO:
    DEFINE VAR xFieldResult AS CHARACTER.

  RUN adm2/support/gConsultas.w (INPUT "bliqempresas.w",
                               INPUT "dliqempresas.w",
                               INPUT "id_empresa_liq",
                               INPUT "", 
                               OUTPUT xfieldResult).
  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:
     fi-empresa:SCREEN-VALUE = xfieldResult. 
     RUN descriptivos.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fi-fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-fecha wWin
ON VALUE-CHANGED OF Fi-fecha IN FRAME fMain /* Fecha de registro */
DO:
  DEF VAR v_registro AS DATE.
  DEF VAR v_referencia AS DATE.

  v_registro = DATE(fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
  IF WEEKDAY(v_registro) = 1  THEN v_referencia = v_registro - 2.
                              ELSE v_referencia = v_registro - 1.


  fi-referencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(v_referencia).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-cuadrilla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-cuadrilla wWin
ON VALUE-CHANGED OF T-cuadrilla IN FRAME fMain /* Todas */
DO:
    if t-cuadrilla:screen-value = 'yes' Then
  fi-cuadrilla:sensitive = false.
Else
  fi-cuadrilla:sensitive = true.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-empresa wWin
ON VALUE-CHANGED OF T-empresa IN FRAME fMain /* Todas */
DO:
    if t-empresa:screen-value = 'yes' Then
    fi-empresa:sensitive = false.
Else
    fi-empresa:sensitive = true.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descriptivos wWin 
PROCEDURE descriptivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST liq_empresas WHERE liq_empresas.id_empresa_liq = integer(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
IF AVAILABLE liq_empresas THEN
    nombre-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = liq_empresas.descripcion.
  ELSE
   nombre-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

   FIND FIRST liq_cuadrillas WHERE liq_cuadrillas.id_empresa_cosechera = integer(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
       liq_cuadrillas.id_cuadrilla = integer(fi-cuadrilla:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
       NO-LOCK NO-ERROR.
      IF AVAILABLE liq_cuadrillas THEN
          nombre-cuadrilla:SCREEN-VALUE IN FRAME {&FRAME-NAME} = liq_cuadrillas.nombre.
        ELSE
         nombre-cuadrilla:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY RADIO-SET-2 FI-empresa nombre-empresa T-empresa FI-cuadrilla 
          nombre-cuadrilla T-cuadrilla Fi-fecha Fi-referencia 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RADIO-SET-2 B-generar B-borrar FI-empresa T-empresa FI-cuadrilla 
         T-cuadrilla Fi-fecha Fi-referencia 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v_registro AS DATE.
DEF VAR v_referencia AS DATE.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */


  fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).

  v_registro = DATE(fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  IF WEEKDAY(v_registro) = 1  THEN v_referencia = v_registro - 2.
                              ELSE v_referencia = v_registro - 1.


  fi-referencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(v_referencia).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

