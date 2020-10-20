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

DEF VAR ultimos-dias AS INTEGER EXTENT 12 INITIAL [31,28,31,30,31,30,31,31,30,31,30,31].
DEF VAR dias-semana AS CHARACTER EXTENT 7 INITIAL ["DOMINGO", "LUNES","MARTES","MIERCOLES","JUEVES","VIERNES","SABADO"].


{src/adm2/widgetprto.i}

{s_varsis.i}

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
&Scoped-Define ENABLED-OBJECTS Btn_OK fi-empresa Btn_Cancel FI-periodo ~
FI-mes FI-quincena RADIO-SET-1 B-gestion B-compensado B-excel 
&Scoped-Define DISPLAYED-OBJECTS nombre-empresa fi-empresa FI-periodo ~
FI-mes FI-quincena RADIO-SET-1 FI-desde-fecha FI-hasta-fecha Fi-desde-1 ~
Fi-hasta-1 FI-total-1 Fi-desde-2 Fi-hasta-2 FI-total-2 Fi-desde-3 ~
Fi-hasta-3 FI-total-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-compensado 
     LABEL "Resumen Compensaci¢n" 
     SIZE 25 BY 1.43.

DEFINE BUTTON B-excel 
     LABEL "Excel Compensaci¢n" 
     SIZE 22 BY 1.19.

DEFINE BUTTON B-gestion 
     LABEL "Resumen Gesti¢n" 
     SIZE 22 BY 1.43.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Generar" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE Fi-desde-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19 NO-UNDO.

DEFINE VARIABLE Fi-desde-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19 NO-UNDO.

DEFINE VARIABLE Fi-desde-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-desde-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emp Contratista" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE Fi-hasta-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19 NO-UNDO.

DEFINE VARIABLE Fi-hasta-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19 NO-UNDO.

DEFINE VARIABLE Fi-hasta-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-hasta-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-mes AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-periodo AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FI-quincena AS CHARACTER FORMAT "X(256)":U 
     LABEL "Quincena" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-total-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.24 NO-UNDO.

DEFINE VARIABLE FI-total-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.24 NO-UNDO.

DEFINE VARIABLE FI-total-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.24 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Generar", 1,
"Borrar", 2
     SIZE 16 BY 1.91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.48 COL 82 WIDGET-ID 4
     nombre-empresa AT ROW 2.43 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fi-empresa AT ROW 2.48 COL 20 COLON-ALIGNED WIDGET-ID 48
     Btn_Cancel AT ROW 2.67 COL 82 WIDGET-ID 2
     FI-periodo AT ROW 4.1 COL 20 COLON-ALIGNED WIDGET-ID 66
     FI-mes AT ROW 4.1 COL 39.6 COLON-ALIGNED WIDGET-ID 68
     FI-quincena AT ROW 4.1 COL 58.6 COLON-ALIGNED WIDGET-ID 70
     RADIO-SET-1 AT ROW 5.52 COL 82 NO-LABEL WIDGET-ID 22
     FI-desde-fecha AT ROW 5.95 COL 20 COLON-ALIGNED WIDGET-ID 6
     FI-hasta-fecha AT ROW 6 COL 57 COLON-ALIGNED WIDGET-ID 10
     Fi-desde-1 AT ROW 8.14 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     Fi-hasta-1 AT ROW 8.14 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     FI-total-1 AT ROW 8.14 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     Fi-desde-2 AT ROW 9.57 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     Fi-hasta-2 AT ROW 9.57 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     FI-total-2 AT ROW 9.57 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     Fi-desde-3 AT ROW 11 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     Fi-hasta-3 AT ROW 11 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     FI-total-3 AT ROW 11 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     B-gestion AT ROW 13.14 COL 11 WIDGET-ID 74
     B-compensado AT ROW 13.14 COL 38 WIDGET-ID 76
     B-excel AT ROW 13.14 COL 68 WIDGET-ID 102
     "  Filtros" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 32
          BGCOLOR 1 FGCOLOR 15 
     "Intervalo 1" VIEW-AS TEXT
          SIZE 11 BY 1.19 AT ROW 8.14 COL 8 WIDGET-ID 78
     "Intervalo 2" VIEW-AS TEXT
          SIZE 11 BY 1.19 AT ROW 9.57 COL 8 WIDGET-ID 80
     "Intervalo 3" VIEW-AS TEXT
          SIZE 11 BY 1.19 AT ROW 11 COL 8 WIDGET-ID 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.6 BY 14.95 WIDGET-ID 100.


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
         TITLE              = "Proceso de Compensaci¢n de Hs"
         HEIGHT             = 14.95
         WIDTH              = 98.6
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
/* SETTINGS FOR FILL-IN Fi-desde-1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fi-desde-2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fi-desde-3 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-desde-fecha IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fi-hasta-1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fi-hasta-2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fi-hasta-3 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-hasta-fecha IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-total-1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-total-2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-total-3 IN FRAME fMain
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
ON END-ERROR OF wWin /* Proceso de Compensaci¢n de Hs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Proceso de Compensaci¢n de Hs */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-compensado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-compensado wWin
ON CHOOSE OF B-compensado IN FRAME fMain /* Resumen Compensaci¢n */
DO:
  RUN imprime-compensado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-excel wWin
ON CHOOSE OF B-excel IN FRAME fMain /* Excel Compensaci¢n */
DO:
  RUN p_excel_compensa.p (INPUT INTEGER(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                          INPUT DATE(fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                          INPUT DATE(fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-gestion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-gestion wWin
ON CHOOSE OF B-gestion IN FRAME fMain /* Resumen Gesti¢n */
DO:
  RUN imprime-gestion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* Generar */
DO:
  DEF VAR v_empresa AS INTEGER.
  DEF VAR v_desde AS DATE.
  DEF VAR v_hasta AS DATE.


  DEF VAR v_desde-1 AS DATE.
  DEF VAR v_hasta-1 AS DATE.
  DEF VAR v1 AS INTEGER.
  DEF VAR v_desde-2 AS DATE.
  DEF VAR v_hasta-2 AS DATE.
  DEF VAR v2 AS INTEGER.
  DEF VAR v_desde-3 AS DATE.
  DEF VAR v_hasta-3 AS DATE.
  DEF VAR v3 AS INTEGER.

  v_empresa = INTEGER(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_desde =  DATE(fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_hasta =  DATE(fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  
  
  v_desde-1 = DATE(fi-desde-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_hasta-1 = DATE(fi-hasta-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v1 = INTEGER(fi-total-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_desde-2 = DATE(fi-desde-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_hasta-2 = DATE(fi-hasta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v2 = INTEGER(fi-total-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_desde-3 = DATE(fi-desde-3:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_hasta-3 = DATE(fi-hasta-3:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v3 = INTEGER(fi-total-3:SCREEN-VALUE IN FRAME {&FRAME-NAME}).


  /*MESSAGE "No tiene permisos para ejecutar este proceso" VIEW-AS ALERT-BOX WARNING.*/

  FOR EACH resumen_gestion :
       DELETE resumen_gestion.
  END.

  FOR EACH resumen_compensado :
       DELETE resumen_compensado.
  END.




  RUN p_gencompensa01.p (INPUT v_empresa,
                       INPUT v_desde-1,
                       INPUT v_hasta-1,
                       INPUT v1).

  RUN p_gencompensa01.p (INPUT v_empresa,
                       INPUT v_desde-2,
                       INPUT v_hasta-2,
                       INPUT v2).

  IF v3 > 0 THEN
  DO:
      RUN p_gencompensa01.p (INPUT v_empresa,
                           INPUT v_desde-3,
                           INPUT v_hasta-3,
                           INPUT v3).
  END.

  MESSAGE "Proceso generado" VIEW-AS ALERT-BOX.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresa wWin
ON LEAVE OF fi-empresa IN FRAME fMain /* Emp Contratista */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresa wWin
ON MOUSE-SELECT-DBLCLICK OF fi-empresa IN FRAME fMain /* Emp Contratista */
DO:
     DEFINE VAR hfield AS HANDLE NO-UNDO.
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


&Scoped-define SELF-NAME FI-mes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-mes wWin
ON LEAVE OF FI-mes IN FRAME fMain /* Mes */
DO:
  RUN cargar-fechas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-quincena
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-quincena wWin
ON LEAVE OF FI-quincena IN FRAME fMain /* Quincena */
DO:
  RUN cargar-fechas.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar-fechas wWin 
PROCEDURE cargar-fechas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR vdf1 AS DATE.
    DEF VAR vhf1 AS DATE.
    DEF VAR v1 AS INTEGER.
    DEF VAR vdf2 AS DATE.
    DEF VAR vhf2 AS DATE.
    DEF VAR v2 AS INTEGER.
    DEF VAR vdf3 AS DATE.
    DEF VAR vhf3 AS DATE.
    DEF VAR v3 AS INTEGER.

    
    IF INTEGER(fi-quincena:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 1 THEN
    DO:
        fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "01/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
        fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "15/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
    END.


    IF INTEGER(fi-quincena:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 2 THEN
    DO:
        fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "16/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
        fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ultimos-dias[INTEGER(fi-mes:SCREEN-VALUE)]) + "/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
    END.


    RUN p-rangos-grupos.p (INPUT DATE(fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                           INPUT DATE(fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                           OUTPUT vdf1, OUTPUT vhf1, OUTPUT v1,
                           OUTPUT vdf2, OUTPUT vhf2, OUTPUT v2,
                           OUTPUT vdf3, OUTPUT vhf3, OUTPUT v3).

    fi-desde-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vdf1).
    fi-hasta-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vhf1).
    fi-total-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(v1).
    
    fi-desde-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vdf2).
    fi-hasta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vhf2).
    fi-total-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(v2).
    
    
    IF v3 > 0 THEN
    DO:
        fi-desde-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vdf3).
        fi-hasta-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vhf3).
        fi-total-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(v3).
    END.
  ELSE
  DO:
      fi-desde-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
      fi-hasta-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
      fi-total-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.

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
  DISPLAY nombre-empresa fi-empresa FI-periodo FI-mes FI-quincena RADIO-SET-1 
          FI-desde-fecha FI-hasta-fecha Fi-desde-1 Fi-hasta-1 FI-total-1 
          Fi-desde-2 Fi-hasta-2 FI-total-2 Fi-desde-3 Fi-hasta-3 FI-total-3 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK fi-empresa Btn_Cancel FI-periodo FI-mes FI-quincena RADIO-SET-1 
         B-gestion B-compensado B-excel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime-compensado wWin 
PROCEDURE imprime-compensado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  define var v_filtro as character initial "".
  define var v_general as character.
  DEF VAR v_desde AS CHARACTER.
  DEF VAR v_hasta AS CHARACTER.
  DEF VAR v-total AS INTEGER.

  v_desde = fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  v_hasta = fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

  v-total = INTEGER(fi-total-1:SCREEN-VALUE) * 8 + 
            INTEGER(fi-total-2:SCREEN-VALUE) * 8 +
            INTEGER(fi-total-3:SCREEN-VALUE) * 8.


        
        v_filtro = "".
        RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.
        
        /* vlc_dir_fuentes = "z:\sistemas\sami\sistemas\". */
         RUN  aderb\_prntrb2(
               (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
               "resumen_compensado", /* RB-REPORT-NAME */
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
               "Nomenclador de tareas",         /* RB-WINDOW-TITLE */
               yes,                           /* RB-DISPLAY-ERRORS */
               yes,                           /* RB-DISPLAY-STATUS */
               no,                              /* RB-NO-WAIT */
               "v_general = " + v_desde + "|" + v_hasta + "|" + STRING(v-total) + "|"  /* RB-OTHER-PARAMETERS */,
               ""
               ).   
        
        os-delete value(RB-MEMO-FILE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime-gestion wWin 
PROCEDURE imprime-gestion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  define var v_filtro as character initial "".
  define var v_general as character.
  DEF VAR v_desde AS CHARACTER.
  DEF VAR v_hasta AS CHARACTER.
  DEF VAR v-total AS INTEGER.

  v_desde = fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  v_hasta = fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  v-total = INTEGER(fi-total-1:SCREEN-VALUE) * 8 + 
            INTEGER(fi-total-2:SCREEN-VALUE) * 8 +
            INTEGER(fi-total-3:SCREEN-VALUE) * 8.

        
        v_filtro = "".
        RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.
        
         /* vlc_dir_fuentes = "z:\sistemas\sami\sistemas\".*/ 
         RUN  aderb\_prntrb2(
               (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
               "resumen_gestion", /* RB-REPORT-NAME */
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
               "Reporte Gestion",         /* RB-WINDOW-TITLE */
               yes,                           /* RB-DISPLAY-ERRORS */
               yes,                           /* RB-DISPLAY-STATUS */
               no,                              /* RB-NO-WAIT */
               "v_general = " + v_desde + "|" + v_hasta + "|" + string(v-total) + "|" /* RB-OTHER-PARAMETERS */,
               ""
               ).   
        
        os-delete value(RB-MEMO-FILE).

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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  fi-empresa:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
  
  fi-periodo:SCREEN-VALUE in frame {&FRAME-NAME} = STRING(YEAR(TODAY),"9999").
  fi-mes:SCREEN-VALUE in frame {&FRAME-NAME} = STRING(MONTH(TODAY),"99").

   IF DAY(TODAY) < 16 THEN 
       fi-quincena:SCREEN-VALUE in frame {&FRAME-NAME} = "1".
    ELSE 
        fi-quincena:SCREEN-VALUE in frame {&FRAME-NAME} = "2".
  RUN cargar-fechas.  



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

