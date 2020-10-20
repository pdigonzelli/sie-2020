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
&Scoped-Define ENABLED-OBJECTS RADIO-SET-1 Fi-tipo-liq fi-empresa ~
FI-sucursal Fi-sector FI-desde-fecha FI-hasta-fecha Btn_OK Btn_Cancel ~
T-todos T-fincas FI-impresora B-cambiar 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 Fi-tipo-liq fi-empresa ~
nombre-empresa FI-sucursal nombre-sucursal Fi-sector nombre-sector ~
FI-desde-fecha FI-hasta-fecha FI-legajo T-todos FI-id_origen ~
FI-id_proveedor nombre-finca T-fincas FI-impresora 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-cambiar 
     LABEL "Cambiar" 
     SIZE 15 BY 1.19.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Imprimir" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-desde-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emp Contratista" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE FI-hasta-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-id_origen AS CHARACTER FORMAT "X(256)":U 
     LABEL "Finca" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .95 NO-UNDO.

DEFINE VARIABLE FI-id_proveedor AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE FI-impresora AS CHARACTER FORMAT "X(256)":U 
     LABEL "Impresora" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-legajo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Legajo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE Fi-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE FI-sucursal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE Fi-tipo-liq AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Liq" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-finca AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sucursal AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY .95 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "RHPRO", 1,
"ABACUS", 2
     SIZE 19 BY 1.91 NO-UNDO.

DEFINE VARIABLE T-fincas AS LOGICAL INITIAL yes 
     LABEL "Todas" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE T-todos AS LOGICAL INITIAL yes 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RADIO-SET-1 AT ROW 5.76 COL 89 NO-LABEL WIDGET-ID 48
     Fi-tipo-liq AT ROW 9.57 COL 73 COLON-ALIGNED WIDGET-ID 46
     fi-empresa AT ROW 2.67 COL 20 COLON-ALIGNED WIDGET-ID 10
     nombre-empresa AT ROW 2.67 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     FI-sucursal AT ROW 3.86 COL 20 COLON-ALIGNED WIDGET-ID 24
     nombre-sucursal AT ROW 3.86 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     Fi-sector AT ROW 5.05 COL 20 COLON-ALIGNED WIDGET-ID 22
     nombre-sector AT ROW 5.05 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     FI-desde-fecha AT ROW 6.24 COL 20 COLON-ALIGNED WIDGET-ID 8
     FI-hasta-fecha AT ROW 7.67 COL 20 COLON-ALIGNED WIDGET-ID 12
     Btn_OK AT ROW 1.24 COL 91 WIDGET-ID 6
     Btn_Cancel AT ROW 2.43 COL 91 WIDGET-ID 4
     FI-legajo AT ROW 9.57 COL 21 COLON-ALIGNED WIDGET-ID 20
     T-todos AT ROW 9.57 COL 39 WIDGET-ID 36
     FI-id_origen AT ROW 11 COL 21 COLON-ALIGNED WIDGET-ID 14
     FI-id_proveedor AT ROW 11 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     nombre-finca AT ROW 11 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     T-fincas AT ROW 11 COL 93 WIDGET-ID 34
     FI-impresora AT ROW 12.67 COL 21 COLON-ALIGNED WIDGET-ID 18
     B-cambiar AT ROW 12.67 COL 86 WIDGET-ID 2
     "  Filtros" VIEW-AS TEXT
          SIZE 81 BY .71 AT ROW 1.48 COL 3 WIDGET-ID 38
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.8 BY 14.24 WIDGET-ID 100.


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
         TITLE              = "Resumen Tarja Personal x Legajo"
         HEIGHT             = 14.24
         WIDTH              = 120.8
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN FI-id_origen IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-id_proveedor IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-legajo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-empresa IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-finca IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sector IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sucursal IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Resumen Tarja Personal x Legajo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Resumen Tarja Personal x Legajo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-cambiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-cambiar wWin
ON CHOOSE OF B-cambiar IN FRAME fMain /* Cambiar */
DO:
  system-dialog printer-setup.
  fi-impresora:screen-value in frame {&frame-name} = session:printer-name.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel wWin
ON CHOOSE OF Btn_Cancel IN FRAME fMain /* Cancel */
DO:
    APPLY "window-close" TO CURRENT-WINDOW.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* Imprimir */
DO:
 DEF VAR v_sector AS INTEGER.
 DEF VAR v_sucursal AS INTEGER.
 DEF VAR v_fecha_desde AS DATE.
 DEF VAR v_fecha_hasta AS DATE.
 DEF VAR v_empresa AS INTEGER.
 DEF VAR v_finca AS LOGICAL.
 DEF VAR v_tipo AS CHARACTER.
 DEF VAR v_impresora AS CHARACTER.
 DEF VAR v_legajo AS INTEGER.

 v_sector = INTEGER(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_sucursal = INTEGER(fi-sucursal:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_fecha_desde = DATE(fi-desde-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_fecha_hasta = DATE(fi-hasta-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_empresa = INTEGER(fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_finca = LOGICAL(t-fincas:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
 v_tipo = fi-tipo-liq:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
 v_impresora = fi-impresora:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
 v_legajo = INTEGER(fi-legajo:SCREEN-VALUE IN FRAME {&FRAME-NAME}).


 /* Actualizo cant_jornal y cant_horas */

for each liq_items_control_tareas where 
      liq_items_control_tareas.id_empresa = v_empresa and  
      liq_items_control_tareas.id_sucursal = v_sucursal and 
      liq_items_control_tareas.fecha >= v_fecha_desde and 
      liq_items_control_tareas.fecha <= v_fecha_hasta and
      liq_items_control_tareas.id_sector = v_sector, first liq_legajos where liq_legajos.id_empresa_liq =
      liq_items_control_tareas.id_empresa and liq_legajos.legajo = liq_items_control_tareas.legajo AND
      (IF v_legajo <> 0 THEN liq_items_control_tareas.legajo = v_legajo ELSE TRUE) AND
      (IF v_tipo <> "" THEN liq_legajos.tipo_liquidacion = v_tipo ELSE TRUE)  NO-LOCK
      by liq_items_control_tareas.id_empresa by liq_items_control_tareas.legajo
      by liq_items_control_tareas.nombre by liq_items_control_tareas.fecha :
      
      IF cant_hs_norm <> 0 OR cant_hs_compensa <> 0 THEN
          IF cant_horas = 0 THEN ASSIGN liq_items_control_tareas.cant_horas = liq_items_control_tareas.cant_hs_norm + liq_items_control_tareas.cant_hs_compensa.  
      
      IF cant_jornal_norm <> 0 THEN
          IF cant_jornal = 0 THEN ASSIGN liq_items_control_tareas.cant_jornal = liq_items_control_tareas.cant_jornal_norm. 
        
  END.




CASE integer(radio-set-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
    WHEN 1 THEN
        case integer(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
          WHEN 5 THEN
          DO:
            IF v_legajo = 0 THEN
            run p_liq_genera_agricola_rh.p ( INPUT v_empresa, INPUT v_sector,
                                          INPUT v_sucursal,
                                          INPUT v_fecha_desde,
                                          INPUT v_fecha_hasta,
                                          INPUT v_finca,
                                          INPUT v_tipo,
                                          INPUT v_impresora).
            ELSE
             run p_liq_genera_agricola_legajo_rh.p ( INPUT v_empresa, INPUT v_sector,
                                              INPUT v_sucursal,
                                              INPUT v_fecha_desde,
                                              INPUT v_fecha_hasta,
                                              INPUT v_finca,
                                              INPUT v_tipo,
                                              INPUT v_impresora,
                                              INPUT v_legajo).

          END.
          WHEN 6 THEN
          DO:
            IF v_legajo = 0 THEN
              run p_liq_genera_cosecha_rh.p ( INPUT v_empresa, INPUT v_sector,
                                            INPUT v_sucursal,
                                            INPUT v_fecha_desde,
                                            INPUT v_fecha_hasta,
                                            INPUT v_finca,
                                            INPUT v_tipo,
                                            INPUT v_impresora).
            ELSE
                run p_liq_genera_cosecha_legajo_rh.p ( INPUT v_empresa, INPUT v_sector,
                                              INPUT v_sucursal,
                                              INPUT v_fecha_desde,
                                              INPUT v_fecha_hasta,
                                              INPUT v_finca,
                                              INPUT v_tipo,
                                              INPUT v_impresora,
                                              INPUT v_legajo).

          END.
          OTHERWISE
            DO:
              IF v_legajo = 0 THEN
                 run p_liq_genera_agricola_rh.p ( INPUT v_empresa, INPUT v_sector,
                                            INPUT v_sucursal,
                                            INPUT v_fecha_desde,
                                            INPUT v_fecha_hasta,
                                            INPUT v_finca,
                                            INPUT v_tipo,
                                            INPUT v_impresora).
              ELSE
                 run p_liq_genera_agricola_legajo_rh.p ( INPUT v_empresa, INPUT v_sector,
                                         INPUT v_sucursal,
                                         INPUT v_fecha_desde,
                                         INPUT v_fecha_hasta,
                                         INPUT v_finca,
                                         INPUT v_tipo,
                                         INPUT v_impresora,
                                         INPUT v_legajo).


            END.

        end case.    
    WHEN 2 THEN
        case integer(fi-sector:SCREEN-VALUE IN FRAME {&FRAME-NAME}):
          WHEN 5 THEN
          DO:
            IF v_legajo = 0 THEN
            run p_liq_genera_agricola.p ( INPUT v_empresa, INPUT v_sector,
                                          INPUT v_sucursal,
                                          INPUT v_fecha_desde,
                                          INPUT v_fecha_hasta,
                                          INPUT v_finca,
                                          INPUT v_tipo,
                                          INPUT v_impresora).
            ELSE
             run p_liq_genera_agricola_legajo.p ( INPUT v_empresa, INPUT v_sector,
                                              INPUT v_sucursal,
                                              INPUT v_fecha_desde,
                                              INPUT v_fecha_hasta,
                                              INPUT v_finca,
                                              INPUT v_tipo,
                                              INPUT v_impresora,
                                              INPUT v_legajo).

          END.
          WHEN 6 THEN
          DO:
            IF v_legajo = 0 THEN
              run p_liq_genera_cosecha.p ( INPUT v_empresa, INPUT v_sector,
                                            INPUT v_sucursal,
                                            INPUT v_fecha_desde,
                                            INPUT v_fecha_hasta,
                                            INPUT v_finca,
                                            INPUT v_tipo,
                                            INPUT v_impresora).
            ELSE
                run p_liq_genera_cosecha_legajo.p ( INPUT v_empresa, INPUT v_sector,
                                              INPUT v_sucursal,
                                              INPUT v_fecha_desde,
                                              INPUT v_fecha_hasta,
                                              INPUT v_finca,
                                              INPUT v_tipo,
                                              INPUT v_impresora,
                                              INPUT v_legajo).

          END.
          OTHERWISE
            DO:
              IF v_legajo = 0 THEN
                 run p_liq_genera_agricola.p ( INPUT v_empresa, INPUT v_sector,
                                            INPUT v_sucursal,
                                            INPUT v_fecha_desde,
                                            INPUT v_fecha_hasta,
                                            INPUT v_finca,
                                            INPUT v_tipo,
                                            INPUT v_impresora).
              ELSE
                 run p_liq_genera_agricola_legajo.p ( INPUT v_empresa, INPUT v_sector,
                                         INPUT v_sucursal,
                                         INPUT v_fecha_desde,
                                         INPUT v_fecha_hasta,
                                         INPUT v_finca,
                                         INPUT v_tipo,
                                         INPUT v_impresora,
                                         INPUT v_legajo).


            END.

        end case.    
END CASE.

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


&Scoped-define SELF-NAME FI-id_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-id_origen wWin
ON MOUSE-SELECT-DBLCLICK OF FI-id_origen IN FRAME fMain /* Finca */
DO:
 define var r as rowid no-undo.
 run c_fincas_propias.w (output r).
 find origenes where rowid(origenes) = r no-lock no-error.
 if available origenes then 
 do:
   fi-id_proveedor:screen-value = string(origenes.id_proveedor).
   fi-id_origen:screen-value = string(origenes.id_origen).
   nombre-finca:screen-value = origenes.descripcion.
 end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fi-sector
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON GO OF Fi-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON LEAVE OF Fi-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON MOUSE-SELECT-DBLCLICK OF Fi-sector IN FRAME fMain /* Sector */
DO:
    
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.

    
    IF INTEGER(fi-empresa:SCREEN-VALUE) = 0 THEN
      DO:
          MESSAGE "Debe elegir una empresa " VIEW-AS ALERT-BOX WARNING.
          RETURN.
      END.


    RUN adm2/support/gConsultas.w (INPUT "bliqsectores.w",
                                   INPUT "dliqsectores.w",
                                   INPUT "id_sector_liq",
                                   INPUT "liq_sectores.id_empresa_liq = " + fi-empresa:SCREEN-VALUE, 
                                   OUTPUT xfieldResult).
    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:
         fi-sector:SCREEN-VALUE = xfieldResult. 
         RUN descriptivos.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fi-sector wWin
ON U1 OF Fi-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sucursal wWin
ON LEAVE OF FI-sucursal IN FRAME fMain /* Sucursal */
DO:
  RUN descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sucursal wWin
ON MOUSE-SELECT-DBLCLICK OF FI-sucursal IN FRAME fMain /* Sucursal */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
    DEFINE VAR xFieldResult AS CHARACTER.
    RUN adm2/support/gConsultas.w (INPUT "bsucursales.w",
                                   INPUT "dsucursales.w",
                                   INPUT "id_sucursal",
                                   INPUT "sucursales.id_tipo_sucursal = 19" ,
                                   OUTPUT xfieldResult).

    IF xFieldResult <> "" AND xFieldResult <> ? THEN
    DO:
           fi-sucursal:SCREEN-VALUE = xfieldResult.     
           RUN descriptivos.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-fincas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-fincas wWin
ON VALUE-CHANGED OF T-fincas IN FRAME fMain /* Todas */
DO:
   if t-fincas:screen-value = 'yes' Then
    do:
    fi-id_origen:screen-value = "".
    nombre-finca:screen-value = "".
    fi-id_origen:sensitive = false.
    fi-id_proveedor:screen-value = "".
    end.
   Else
    fi-id_origen:sensitive = true.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-todos wWin
ON VALUE-CHANGED OF T-todos IN FRAME fMain /* Todos */
DO:
  if t-todos:screen-value = 'yes' Then
    fi-legajo:sensitive = false.
   Else
    fi-legajo:sensitive = true.
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
 find first liq_empresas where liq_empresas.id_empresa_liq = INTEGER(fi-empresa:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
 if available liq_empresas Then
      nombre-empresa:screen-value = liq_empresas.descripcion.
   ELSE
     nombre-empresa:screen-value = "".

 find first sucursales where sucursales.id_sucursal = INTEGER(fi-sucursal:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
 if available sucursales Then
          nombre-sucursal:screen-value = sucursales.nombre.
       ELSE
         nombre-empresa:screen-value = "".


find first liq_sectores where 
    liq_sectores.id_empresa_liq = integer(fi-empresa:screen-value in frame {&FRAME-NAME} ) AND
    liq_sectores.id_sector_liq = integer(fi-sector:screen-value in frame {&FRAME-NAME} )  no-lock no-error .
if available liq_sectores then 
    nombre-sector:screen-value   = string(liq_sectores.descripcion).
    else
    nombre-sector:screen-value  = ''.


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
  DISPLAY RADIO-SET-1 Fi-tipo-liq fi-empresa nombre-empresa FI-sucursal 
          nombre-sucursal Fi-sector nombre-sector FI-desde-fecha FI-hasta-fecha 
          FI-legajo T-todos FI-id_origen FI-id_proveedor nombre-finca T-fincas 
          FI-impresora 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RADIO-SET-1 Fi-tipo-liq fi-empresa FI-sucursal Fi-sector 
         FI-desde-fecha FI-hasta-fecha Btn_OK Btn_Cancel T-todos T-fincas 
         FI-impresora B-cambiar 
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

  fi-sector:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
fi-empresa:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
fi-sucursal:load-mouse-pointer("glove") in frame {&FRAME-NAME}.

fi-id_origen:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
fi-desde-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
fi-hasta-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
fi-impresora:screen-value in frame {&frame-name} = session:printer-name.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

