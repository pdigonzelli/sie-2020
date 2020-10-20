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
{s_inicio.i}

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
&Scoped-Define ENABLED-OBJECTS B-imprimir fi-periodo RADIO-SET-1 T-excel ~
T-excel-1 T-excel-2 T-excel-3 FI-producto T-productos T-excel-4 FI-desde ~
FI-hasta 
&Scoped-Define DISPLAYED-OBJECTS fi-periodo RADIO-SET-1 T-excel T-excel-1 ~
T-excel-2 T-excel-3 FI-producto T-productos T-excel-4 FI-desde FI-hasta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-imprimir 
     LABEL "Imprimir" 
     SIZE 16 BY 1.43.

DEFINE VARIABLE FI-desde AS DATE FORMAT "99/99/99":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-hasta AS DATE FORMAT "99/99/99":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-periodo AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE FI-producto AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.43 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Fincas Propias", 1,
"Fincas Arrendadas", 2,
"Todas", 3
     SIZE 26 BY 3.33 NO-UNDO.

DEFINE VARIABLE T-excel AS LOGICAL INITIAL no 
     LABEL "Inventario Plantaci¢n" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE T-excel-1 AS LOGICAL INITIAL no 
     LABEL "An lisis de Campa¤a" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE T-excel-2 AS LOGICAL INITIAL no 
     LABEL "Hist¢rico An lisis de Campa¤a" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE T-excel-3 AS LOGICAL INITIAL no 
     LABEL "Sami Late" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE T-excel-4 AS LOGICAL INITIAL no 
     LABEL "Grafico" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-productos AS LOGICAL INITIAL yes 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-imprimir AT ROW 1.95 COL 52 WIDGET-ID 2
     fi-periodo AT ROW 2.43 COL 16 COLON-ALIGNED WIDGET-ID 4
     RADIO-SET-1 AT ROW 3.86 COL 18 NO-LABEL WIDGET-ID 6
     T-excel AT ROW 4.33 COL 47 WIDGET-ID 20
     T-excel-1 AT ROW 5.29 COL 47 WIDGET-ID 28
     T-excel-2 AT ROW 6.24 COL 47 WIDGET-ID 22
     T-excel-3 AT ROW 7.14 COL 47 WIDGET-ID 24
     FI-producto AT ROW 7.91 COL 16 COLON-ALIGNED WIDGET-ID 10
     T-productos AT ROW 7.91 COL 27 WIDGET-ID 12
     T-excel-4 AT ROW 8.1 COL 47 WIDGET-ID 26
     FI-desde AT ROW 11.48 COL 14 COLON-ALIGNED WIDGET-ID 16
     FI-hasta AT ROW 11.48 COL 40 COLON-ALIGNED WIDGET-ID 18
     " Datos de Cosecha" VIEW-AS TEXT
          SIZE 56 BY 1.19 AT ROW 9.81 COL 6 WIDGET-ID 14
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.6 BY 12.29 WIDGET-ID 100.


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
         TITLE              = "Analisis de Campa¤a"
         HEIGHT             = 12.29
         WIDTH              = 84.6
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Analisis de Campa¤a */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Analisis de Campa¤a */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-imprimir wWin
ON CHOOSE OF B-imprimir IN FRAME fMain /* Imprimir */
DO:
  DEF VAR v-filtro AS CHARACTER.
  DEF VAR v-parametros AS CHARACTER.
  DEF VAR v_desde AS DATE.
  DEF VAR v_hasta AS DATE.
  DEF VAR v_tipo AS INTEGER.
  DEF VAR v_producto AS INTEGER.
  DEF VAR v_tipo_finca AS INTEGER.


  v_desde = DATE(fi-desde:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_hasta = DATE(fi-hasta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_tipo = INTEGER(radio-set-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  v_producto = integer(fi-producto:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  CASE v_tipo:
      WHEN 1 THEN
          v_tipo_finca = 1.
      WHEN 2 THEN
          v_tipo_finca = 2.
      WHEN 3 THEN
          v_tipo_finca = 0.
  END CASE.

  IF logical(t-excel:SCREEN-VALUE) = NO AND logical(t-excel-1:SCREEN-VALUE) = NO AND 
     logical(t-excel-2:SCREEN-VALUE) = NO AND logical(t-excel-3:SCREEN-VALUE) = NO  THEN
  DO:
          v-filtro = "inv_lotes_plantacion.anio = " + fi-periodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}. 
        
          CASE v_tipo:
              WHEN 1 THEN
                  v-filtro =  v-filtro + " AND origenes.id_tipo_origen = 1".
              WHEN 2 THEN
                  v-filtro =  v-filtro + " AND origenes.id_tipo_origen = 3".
              WHEN 3 THEN
                  v-filtro =  v-filtro + " AND (origenes.id_tipo_origen = 1 or origenes.id_tipo_origen = 3)".
          END CASE.
        
          IF t-productos:SCREEN-VALUE = "NO" THEN
              v-filtro = v-filtro + " and inv_lotes_plantacion.id_articulo = " + fi-producto:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
        
        
          FOR EACH rb_cosecha_lote:
              DELETE rb_cosecha_lote.
          END.
        
          FOR EACH inv_lotes_plantacion WHERE anio = INTEGER(fi-periodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK,
               EACH CONTROL_finca_lote WHERE CONTROL_finca_lotes.fecha >= v_desde AND
                CONTROL_finca_lotes.fecha <= v_hasta and
                control_finca_lotes.id_proveedor = inv_lotes_plantacion.id_proveedor and
                control_finca_lotes.id_origen = inv_lotes_plantacion.id_origen AND
                control_finca_lotes.id_lote = inv_lotes_plantacion.id_lote NO-LOCK USE-INDEX fecha_producto:
              FIND FIRST rb_cosecha_lote WHERE
                  rb_cosecha_lote.id_articulo = CONTROL_finca_lote.id_articulo AND
                  rb_cosecha_lote.id_proveedor = CONTROL_finca_lote.id_proveedor AND
                  rb_cosecha_lote.id_origen = CONTROL_finca_lote.id_origen AND
                  rb_cosecha_lote.id_lote = CONTROL_finca_lote.id_lote NO-ERROR.
              IF NOT AVAILABLE rb_cosecha_lote THEN
              DO:
                  CREATE rb_cosecha_lote.
                  ASSIGN rb_cosecha_lote.id_articulo = CONTROL_finca_lote.id_articulo
                         rb_cosecha_lote.id_proveedor = CONTROL_finca_lote.id_proveedor
                         rb_cosecha_lote.id_origen = CONTROL_finca_lote.id_origen
                         rb_cosecha_lote.id_lote = CONTROL_finca_lote.id_lote.
              END.
              ASSIGN rb_cosecha_lote.cant_bins_tijera = rb_cosecha_lote.cant_bins_tijera + (CONTROL_finca_lotes.cant_bins_B_tijera + CONTROL_finca_lotes.cant_bins_V_tijera)
                     rb_cosecha_lote.cant_bins_mano = rb_cosecha_lote.cant_bins_mano + (CONTROL_finca_lotes.cant_bins_B_mano + CONTROL_finca_lotes.cant_bins_V_mano +
                                                                                        CONTROL_finca_lotes.bolsas_20). 
        
          END.
        
          FOR EACH inv_lotes_plantacion WHERE anio = INTEGER(fi-periodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK,
               EACH liq_CONTROL_finca_lote WHERE liq_CONTROL_finca_lotes.fecha >= v_desde AND
                liq_CONTROL_finca_lotes.fecha <= v_hasta and
                liq_control_finca_lotes.id_proveedor = inv_lotes_plantacion.id_proveedor and
                liq_control_finca_lotes.id_origen = inv_lotes_plantacion.id_origen AND
                liq_control_finca_lotes.id_lote = inv_lotes_plantacion.id_lote NO-LOCK USE-INDEX fecha_producto:
              FIND FIRST rb_cosecha_lote WHERE
                  rb_cosecha_lote.id_articulo = liq_CONTROL_finca_lote.id_articulo AND
                  rb_cosecha_lote.id_proveedor = liq_CONTROL_finca_lote.id_proveedor AND
                  rb_cosecha_lote.id_origen = liq_CONTROL_finca_lote.id_origen AND
                  rb_cosecha_lote.id_lote = liq_CONTROL_finca_lote.id_lote NO-ERROR.
              IF NOT AVAILABLE rb_cosecha_lote THEN
              DO:
                  CREATE rb_cosecha_lote.
                  ASSIGN rb_cosecha_lote.id_articulo = liq_CONTROL_finca_lote.id_articulo
                         rb_cosecha_lote.id_proveedor = liq_CONTROL_finca_lote.id_proveedor
                         rb_cosecha_lote.id_origen = liq_CONTROL_finca_lote.id_origen
                         rb_cosecha_lote.id_lote = liq_CONTROL_finca_lote.id_lote.
              END.
              ASSIGN rb_cosecha_lote.cant_bins_tijera = rb_cosecha_lote.cant_bins_tijera + (liq_CONTROL_finca_lotes.cant_bins_B_tijera + liq_CONTROL_finca_lotes.cant_bins_V_tijera)
                     rb_cosecha_lote.cant_bins_mano = rb_cosecha_lote.cant_bins_mano + (liq_CONTROL_finca_lotes.cant_bins_B_mano + liq_CONTROL_finca_lotes.cant_bins_V_mano +
                                                                                        liq_CONTROL_finca_lotes.bolsas_20). 
        
          END.
        
        
          v-parametros = "v_general = " + fi-desde:SCREEN-VALUE + ";" + fi-hasta:SCREEN-VALUE + ";".
        
          RUN  aderb\_prntrb2(
        (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "cosecha~\cosecha.prl", /* RB-REPORT-LIBRARY */
         "analisis01",                    /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         v-filtro,                              /* RB-FILTER */
         "",                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         ?,                              /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
          1,                              /* RB-NUMBER-COPIES  - zero */                  
          0,                              /* RB-BEGIN-PAGE - zero */
          0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Analisis de Campa¤a",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         v-parametros, /* RB-OTHER-PARAMETERS */
         "status.out"
         ).   
    END.
    ELSE
    DO:
        IF logical(t-excel:SCREEN-VALUE) = YES THEN
        RUN p_exporta_analisis.p (INPUT INTEGER(fi-periodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                                  INPUT DATE(fi-desde:SCREEN-VALUE),
                                  INPUT DATE(fi-hasta:SCREEN-VALUE)).
      
        IF logical(t-excel-1:SCREEN-VALUE) = YES THEN
        RUN p_exporta_analisis-1.p (INPUT INTEGER(fi-periodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                                      INPUT DATE(fi-desde:SCREEN-VALUE),
                                      INPUT DATE(fi-hasta:SCREEN-VALUE)).

        IF logical(t-excel-2:SCREEN-VALUE) = YES THEN
        RUN p_exporta_analisis-2.p (INPUT INTEGER(fi-periodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                                      INPUT DATE(fi-desde:SCREEN-VALUE),
                                      INPUT DATE(fi-hasta:SCREEN-VALUE),
                                      INPUT v_tipo_finca,
                                      INPUT v_producto).

        IF logical(t-excel-3:SCREEN-VALUE) = YES THEN
        RUN p_exporta_samilate.p (INPUT INTEGER(fi-periodo:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                                      INPUT DATE(fi-desde:SCREEN-VALUE),
                                      INPUT DATE(fi-hasta:SCREEN-VALUE)).

    END.
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
  DISPLAY fi-periodo RADIO-SET-1 T-excel T-excel-1 T-excel-2 T-excel-3 
          FI-producto T-productos T-excel-4 FI-desde FI-hasta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE B-imprimir fi-periodo RADIO-SET-1 T-excel T-excel-1 T-excel-2 
         T-excel-3 FI-producto T-productos T-excel-4 FI-desde FI-hasta 
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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  fi-periodo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(YEAR(TODAY),"9999").
  fi-desde:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING("01/" + string(MONTH(TODAY),"99") + "/" + STRING(YEAR(TODAY),"9999")) .
  fi-hasta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

