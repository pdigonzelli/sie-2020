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
&Scoped-Define ENABLED-OBJECTS Btn_generar Fi-sector Btn_Cancel ~
FI-desde-fecha RADIO-SET-1 FI-hasta-fecha FI-salida FI-feriado FI-empresa 
&Scoped-Define DISPLAYED-OBJECTS Fi-sector nombre-sector FI-desde-fecha ~
RADIO-SET-1 FI-hasta-fecha FI-salida FI-feriado FI-empresa nombre-empresa 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_generar AUTO-GO 
     LABEL "&Generar" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-desde-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-empresa AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FI-feriado AS DATE FORMAT "99/99/9999":U 
     LABEL "Feriado" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-hasta-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-salida AS CHARACTER FORMAT "X(256)":U INITIAL "z:~\temp~\repgral01.txt" 
     LABEL "Archivo de salida" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .95 NO-UNDO.

DEFINE VARIABLE Fi-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "General x Persona", 1,
"General x Persona x Fecha", 2,
"Detalle x persona x tarea x finca", 3,
"Excel Feriado", 4,
"Resumen Mensual Feriado", 5,
"General x Persona x Fecha x Tarea", 6
     SIZE 38 BY 6.91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_generar AT ROW 1.48 COL 93 WIDGET-ID 4
     Fi-sector AT ROW 2.67 COL 21 COLON-ALIGNED WIDGET-ID 16
     nombre-sector AT ROW 2.67 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     Btn_Cancel AT ROW 2.91 COL 93 WIDGET-ID 2
     FI-desde-fecha AT ROW 4.57 COL 21 COLON-ALIGNED WIDGET-ID 6
     RADIO-SET-1 AT ROW 5.52 COL 67 NO-LABEL WIDGET-ID 20
     FI-hasta-fecha AT ROW 5.76 COL 21 COLON-ALIGNED WIDGET-ID 12
     FI-salida AT ROW 9.1 COL 21 COLON-ALIGNED WIDGET-ID 14
     FI-feriado AT ROW 12.91 COL 22 COLON-ALIGNED WIDGET-ID 10
     FI-empresa AT ROW 14.1 COL 22 COLON-ALIGNED WIDGET-ID 8
     nombre-empresa AT ROW 14.1 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     "  Filtros" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 28
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.4 BY 17 WIDGET-ID 100.


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
         TITLE              = "Resumen de Tareas Tarjas - Excel"
         HEIGHT             = 17
         WIDTH              = 110.4
         MAX-HEIGHT         = 35.86
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 35.86
         VIRTUAL-WIDTH      = 256
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
/* SETTINGS FOR FILL-IN nombre-empresa IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombre-sector IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Resumen de Tareas Tarjas - Excel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Resumen de Tareas Tarjas - Excel */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_generar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_generar wWin
ON CHOOSE OF Btn_generar IN FRAME fMain /* Generar */
DO:

case radio-set-1:screen-value :
   when "1" Then /* General x Persona */
       do:
          case integer(fi-sector:screen-value):
           when 1 or when 5 or when 6 or when 7 Then
           run genera_res_3eros_gral.p (input integer(fi-sector:screen-value),
                                          input date(fi-desde-fecha:screen-value),
                                          input date(fi-hasta-fecha:screen-value),
                                          input fi-salida:screen-value). 
       
           when 2 Then
           run genera_res_3eros_cos_gral.p (input integer(fi-sector:screen-value),
                                                  input date(fi-desde-fecha:screen-value),
                                                  input date(fi-hasta-fecha:screen-value),
                                                  input fi-salida:screen-value). 
          end.                                        
      end.

   when "2" Then /* General x Persona x Fecha */
      do:
           run genera_res_gral_fecha.p (input integer(fi-sector:screen-value),
                                                  input date(fi-desde-fecha:screen-value),
                                                  input date(fi-hasta-fecha:screen-value),
                                                  input fi-salida:screen-value). 
      
      end.
   
   when "3" Then  /* Detalle x persona x tarea x finca */
       do:
          case integer(fi-sector:screen-value):
           when 1 or when 5 or when 6 or when 7 Then
           run genera_resumen_3eros.p (input integer(fi-sector:screen-value),
                                          input date(fi-desde-fecha:screen-value),
                                          input date(fi-hasta-fecha:screen-value),
                                          input fi-salida:screen-value). 
       
           when 2 Then
           run genera_resumen_3eros_cos.p (input integer(fi-sector:screen-value),
                                                  input date(fi-desde-fecha:screen-value),
                                                  input date(fi-hasta-fecha:screen-value),
                                                  input fi-salida:screen-value). 
          end.                                        
      end.
      
   when "4" Then  /* Excel Feriado */
     do:
         if (date(fi-feriado:screen-value) >= date(fi-desde-fecha:screen-value) and
            date(fi-feriado:screen-value) <= date(fi-hasta-fecha:screen-value)) or
            date(fi-feriado:screen-value) = ? Then
             do:
                run p_dias_jornales_personas.p (input integer(fi-empresa:screen-value),
                                                input integer(fi-sector:screen-value),
                                                input date(fi-desde-fecha:screen-value),
                                                input date(fi-hasta-fecha:screen-value),
                                                input date(fi-feriado:screen-value)). 
             end.
           Else
             do:
                message "Feriado fuera de rango" skip
                        "No puede ejecutar esta opcion " view-as alert-box.
                return.     
             end.
                                                  
     end.   

   when "5" Then /* Resumen Excel Feriado */
     do:
         if (date(fi-feriado:screen-value) >= date(fi-desde-fecha:screen-value) and
            date(fi-feriado:screen-value) <= date(fi-hasta-fecha:screen-value)) or
            date(fi-feriado:screen-value) = ? Then
             do:
                run p_dias_jornales_pers-resumen.p (input integer(fi-empresa:screen-value),
                                                input integer(fi-sector:screen-value),
                                                input date(fi-desde-fecha:screen-value),
                                                input date(fi-hasta-fecha:screen-value),
                                                input date(fi-feriado:screen-value)). 
             end.
           Else
             do:
                message "Feriado fuera de rango" skip
                        "No puede ejecutar esta opcion " view-as alert-box.
                return.     
             end.
                                                  
     end.   

   when "6" Then /* General x Persona x Fecha x Tarea*/
      do:
          if integer(fi-empresa:screen-value) = 0 Then
          do:
             message "Debe ingresar un codigo de empresa" view-as alert-box.
             return.
          end.
          case integer(fi-sector:screen-value):
           when 1 or when 5 or when 6 or when 7 Then
           run genera_personas_tareas.p (input integer(fi-sector:screen-value),
                                         input integer(fi-empresa:screen-value),
                                          input date(fi-desde-fecha:screen-value),
                                          input date(fi-hasta-fecha:screen-value),
                                          input fi-salida:screen-value). 
           when 2 Then
                do:
           run genera_personas_tareas_cos.p (input integer(fi-sector:screen-value),
                                         input integer(fi-empresa:screen-value),
                                          input date(fi-desde-fecha:screen-value),
                                          input date(fi-hasta-fecha:screen-value),
                                          input fi-salida:screen-value). 
                                           
            end.
          end case.                                        
      
      end.


end. 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-empresa wWin
ON MOUSE-SELECT-DBLCLICK OF FI-empresa IN FRAME fMain /* Empresa */
DO:
    DEFINE VAR hfield AS HANDLE NO-UNDO.
  DEFINE VAR xFieldResult AS CHARACTER.
  RUN adm2/support/gConsultas.w (INPUT "brprovactiv.w",
                                 INPUT "drprovactiv.w",
                                 INPUT "id_proveedor",
                                 INPUT "r_prov_activ.id_actividad = 6" ,
                                 OUTPUT xfieldResult).

  IF xFieldResult <> "" AND xFieldResult <> ? THEN
  DO:

         fi-empresa:SCREEN-VALUE = xfieldResult.     
         RUN descriptivos.
  END.

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
    RUN adm2/support/gConsultas.w (INPUT "bsectoresagricola.w",
                                   INPUT "dsectoresagricola.w",
                                   INPUT "id_sector",
                                   INPUT "" ,
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


&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 wWin
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME fMain
DO:
  case integer(radio-set-1:screen-value):
     when 1 Then
      fi-salida:screen-value = "z:\temp\repgral01.txt".
     when 2 Then
      fi-salida:screen-value = "z:\temp\repfecha01.txt".
     when 3 Then
      fi-salida:screen-value = "z:\temp\repdeta01.txt".
     otherwise
      fi-salida:screen-value = "z:\temp\archivo.txt".
  end.
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
 find first proveedores where proveedores.id_proveedor = INTEGER(fi-empresa:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
if available proveedores Then
     nombre-empresa:screen-value = proveedores.nombre.
  ELSE
    nombre-empresa:screen-value = "".

 
find first sectores_agricolas where sectores_agricolas.id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME} )  no-lock no-error .
if available sectores_agricolas then 
    nombre-sector:screen-value   = string(sectores_agricolas.descripcion).
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
  DISPLAY Fi-sector nombre-sector FI-desde-fecha RADIO-SET-1 FI-hasta-fecha 
          FI-salida FI-feriado FI-empresa nombre-empresa 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_generar Fi-sector Btn_Cancel FI-desde-fecha RADIO-SET-1 
         FI-hasta-fecha FI-salida FI-feriado FI-empresa 
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
fi-desde-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
fi-hasta-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

