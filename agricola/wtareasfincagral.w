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
&Scoped-Define ENABLED-OBJECTS Btn_OK Fi-sector Btn_Cancel FI-desde-fecha ~
RADIO-SET-1 T-excel FI-hasta-fecha 
&Scoped-Define DISPLAYED-OBJECTS Fi-sector nombre-sector FI-desde-fecha ~
RADIO-SET-1 T-excel FI-hasta-fecha 

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

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Imprimir" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-desde-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-hasta-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE Fi-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .95 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "En Jornales y Horas", 1,
"En Jornales", 2
     SIZE 26 BY 2.24 NO-UNDO.

DEFINE VARIABLE T-excel AS LOGICAL INITIAL no 
     LABEL "Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.48 COL 88 WIDGET-ID 4
     Fi-sector AT ROW 2.67 COL 21 COLON-ALIGNED WIDGET-ID 10
     nombre-sector AT ROW 2.67 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     Btn_Cancel AT ROW 2.67 COL 88 WIDGET-ID 2
     FI-desde-fecha AT ROW 3.86 COL 21 COLON-ALIGNED WIDGET-ID 6
     RADIO-SET-1 AT ROW 4.1 COL 55 NO-LABEL WIDGET-ID 14
     T-excel AT ROW 4.57 COL 87 WIDGET-ID 18
     FI-hasta-fecha AT ROW 5.05 COL 21 COLON-ALIGNED WIDGET-ID 8
     "  Filtros" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 20
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.8 BY 7.81 WIDGET-ID 100.


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
         TITLE              = "Resumen de tareas x finca (Propias y de 3eros)"
         HEIGHT             = 7.81
         WIDTH              = 107
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
/* SETTINGS FOR FILL-IN nombre-sector IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Resumen de tareas x finca (Propias y de 3eros) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Resumen de tareas x finca (Propias y de 3eros) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* Imprimir */
DO:
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  define var v_filtro as character initial "".
  define var v_cargo like personal_finca.id_cargo.
  define var v_reporte like agricola.rb_resumen_agricola.id_reporte. 
  define var v_sector as integer.
  
  


v_reporte = RANDOM(1,1000000).
v_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}).

for each agricola.rb_resumen_agricola where id_reporte = v_reporte :
   delete agricola.rb_resumen_agricola VALIDATE(TRUE,"").
end.

for each control_tareas where control_tareas.id_sector = integer(fi-sector:screen-value in frame {&FRAME-NAME}) and 
    control_tareas.fecha >= date(fi-desde-fecha:screen-value in frame {&FRAME-NAME}) and
    control_tareas.fecha <= date(fi-hasta-fecha:screen-value in frame {&FRAME-NAME}) no-lock:
   
    for each items_control_tareas of control_tareas 
      where items_control_tareas.id_tarea <> 0 no-lock:
      find first personal_finca where personal_finca.id_empresa_cosechera =
          items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
          if available personal_finca Then
              v_cargo = personal_finca.id_cargo.
            Else
              v_cargo = 0. 
    
       find first agricola.rb_resumen_agricola where 
                  rb_resumen_agricola.id_reporte = v_reporte and
                  rb_resumen_agricola.id_empresa = items_control_tareas.id_empresa and
                  rb_resumen_agricola.id_proveedor = items_control_tareas.id_proveedor and
                  rb_resumen_agricola.id_origen = items_control_tareas.id_origen and
                  rb_resumen_agricola.id_tarea = items_control_tareas.id_tarea 
                  no-error.
       if not available rb_resumen_agricola Then
          do:
             find first origenes of control_tareas no-lock no-error.
             create rb_resumen_agricola.
             assign 
                  rb_resumen_agricola.id_reporte = v_reporte 
                  rb_resumen_agricola.id_empresa = items_control_tareas.id_empresa 
                  rb_resumen_agricola.id_tarea = items_control_tareas.id_tarea 
                  rb_resumen_agricola.id_proveedor = items_control_tareas.id_proveedor 
                  rb_resumen_agricola.id_origen = items_control_tareas.id_origen
                  rb_resumen_agricola.id_sector = items_control_tareas.id_sector 
                  rb_resumen_agricola.id_zona = (if available origenes Then origenes.id_zona Else 0)
                  rb_resumen_agricola.c_fecha = today.
          end.
         case v_sector:
         when 1 Then
         do:
         /* Jornal Peon */
         if (v_cargo <> 42 AND v_cargo <> 32 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.jornal_peon = rb_resumen_agricola.jornal_peon + items_control_tareas.cant_jornal.            

         /* Jornal Trac */
         if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
           rb_resumen_agricola.jornal_trac = rb_resumen_agricola.jornal_trac + items_control_tareas.cant_jornal.            

         /* Jornal Ayudante Capataz */
         if (v_cargo = 32 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.jornal_ayud = rb_resumen_agricola.jornal_ayud + items_control_tareas.cant_jornal.            

          if items_control_tareas.id_unidad_liquidacion = 11 Then
             rb_resumen_agricola.jornal_ayud = rb_resumen_agricola.jornal_ayud + items_control_tareas.cantidad.          


         /* Horas Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.horas_peon = rb_resumen_agricola.horas_peon + items_control_tareas.cant_horas.            

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           rb_resumen_agricola.horas_trac = rb_resumen_agricola.horas_trac + items_control_tareas.cant_horas.            

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           rb_resumen_agricola.horas_trac_cont = rb_resumen_agricola.horas_trac_cont + items_control_tareas.cant_horas.            

         /* Horas Pulverizacion */ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0)) Then
           rb_resumen_agricola.horas_pulv = rb_resumen_agricola.horas_pulv + items_control_tareas.cantidad.            

         /* Horas Pulv Trac*/ 
          if (items_control_tareas.id_unidad_liquidacion = 7 and 
             (items_control_tareas.id_tarea < 107 or  items_control_tareas.id_tarea  > 118) and 
             (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0)) Then
           rb_resumen_agricola.horas_pulv_trac = rb_resumen_agricola.horas_pulv_trac + items_control_tareas.cantidad.            
           
         /* Licencias */
         
         if (items_control_tareas.id_tarea >= 107 and items_control_tareas.id_tarea <= 116) or items_control_tareas.id_tarea = 118 Then
           rb_resumen_agricola.lic = rb_resumen_agricola.lic + items_control_tareas.cantidad.            
         
         /* LLuvia */  
         if items_control_tareas.id_tarea = 117 Then
           rb_resumen_agricola.lluvia = rb_resumen_agricola.lluvia + items_control_tareas.cantidad.            
           
         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.destajo = rb_resumen_agricola.destajo + items_control_tareas.cantidad.            
         
         
         /* Viaje Colectivo */  
         if items_control_tareas.id_unidad_liquidacion = 12 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viaje = rb_resumen_agricola.viaje + items_control_tareas.cantidad.            
         
         end.
         
       when 2 Then
         do:   

         /* Jornal Peon */
         if (v_cargo <> 42 and v_cargo <> 32 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.jornal_peon = rb_resumen_agricola.jornal_peon + items_control_tareas.cant_jornal.            
             
         /* Jornal Trac */
         if (v_cargo = 42 or items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) Then
           rb_resumen_agricola.jornal_trac = rb_resumen_agricola.jornal_trac + items_control_tareas.cant_jornal.            

         /* Jornal Ayudante Capataz */
         if (v_cargo = 32 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.jornal_ayud = rb_resumen_agricola.jornal_ayud + items_control_tareas.cant_jornal.            

          if items_control_tareas.id_unidad_liquidacion = 11 Then
             rb_resumen_agricola.jornal_ayud = rb_resumen_agricola.jornal_ayud + items_control_tareas.cantidad.          


         /* Horas Peon */
         if (v_cargo <> 42 and items_control_tareas.nro_tractor = 0 and items_control_tareas.nro_maquina = 0) Then
           rb_resumen_agricola.horas_peon = rb_resumen_agricola.horas_peon + items_control_tareas.cant_horas.            

         /* Horas Trac */
         if (v_cargo = 42 or (items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0) and 
            items_control_tareas.nro_tractor <> 99999) Then
           rb_resumen_agricola.horas_trac = rb_resumen_agricola.horas_trac + items_control_tareas.cant_horas.            

         /* Trac Cont*/
         if (items_control_tareas.nro_tractor = 99999) Then
           rb_resumen_agricola.horas_trac_cont = rb_resumen_agricola.horas_trac_cont + items_control_tareas.cant_horas.            

         /* Destajo */
         
         if items_control_tareas.id_unidad_liquidacion = 4 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.destajo = rb_resumen_agricola.destajo + items_control_tareas.cantidad.            

         /* Hs.Tractoelev */
         
         if items_control_tareas.id_unidad_liquidacion = 17 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.horas_tractoelev = rb_resumen_agricola.horas_tractoelev + items_control_tareas.cantidad.            

         /* Plus x Bins */
         
         if items_control_tareas.id_unidad_liquidacion = 16 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.plus_x_bins = rb_resumen_agricola.plus_x_bins + items_control_tareas.cantidad.            
        
         /* Viatico Peon */
         
         if items_control_tareas.id_unidad_liquidacion = 8 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viatico_peon = rb_resumen_agricola.viatico_peon + items_control_tareas.cantidad.            

         /* Viatico Ayud. */
         
         if items_control_tareas.id_unidad_liquidacion = 9 and (items_control_tareas.id_tarea < 107 or items_control_tareas.id_tarea  > 118) Then
           rb_resumen_agricola.viatico_ayud = rb_resumen_agricola.viatico_ayud + items_control_tareas.cantidad.            

         end.
        end. 


    end.
end.


if t-excel:screen-value in frame {&frame-name} = "no" Then
  do:
        v_filtro = "rb_resumen_agricola.id_reporte = " + string(v_reporte).
        
        
        RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.
        
        case radio-set-1:screen-value:
         when  "1" Then 
         case v_sector :
           when 1 Then
            RUN  aderb\_prntrb2(
               (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
               "resumen_finca_gral-01", /* RB-REPORT-NAME */
               "",                             /* RB-DB-CONNECTION */
               "O",                             /* RB-INCLUDE-RECORDS */
               v_filtro,                              /* RB-FILTER */
               RB-MEMO-FILE,                              /* RB-MEMO-FILE */
               "?",                             /* RB-PRINT-DESTINATION */
               "",                              /* RB-PRINTER-NAME */
               "",                              /* RB-PRINTER-PORT */
               "",                              /* RB-OUTPUT-FILE */
                1,                              /* RB-NUMBER-COPIES  - zero */                  
                0,                              /* RB-BEGIN-PAGE - zero */
                0,                              /* RB-END-PAGE - zero */
               no,                              /* RB-TEST-PATTERN */
               "Resumen Propias y de Terceros",         /* RB-WINDOW-TITLE */
               yes,                           /* RB-DISPLAY-ERRORS */
               yes,                           /* RB-DISPLAY-STATUS */
               no,                              /* RB-NO-WAIT */
               "v_general = " + fi-desde-fecha:screen-value + ";" + fi-hasta-fecha:screen-value 
               /* RB-OTHER-PARAMETERS */,
               ""
               ).   
            when 2 Then
             RUN  aderb\_prntrb2(
               (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
               "resumen_finca_gral-01-cos", /* RB-REPORT-NAME */
               "",                             /* RB-DB-CONNECTION */
               "O",                             /* RB-INCLUDE-RECORDS */
               v_filtro,                              /* RB-FILTER */
               RB-MEMO-FILE,                              /* RB-MEMO-FILE */
               "?",                             /* RB-PRINT-DESTINATION */
               "",                              /* RB-PRINTER-NAME */
               "",                              /* RB-PRINTER-PORT */
               "",                              /* RB-OUTPUT-FILE */
                1,                              /* RB-NUMBER-COPIES  - zero */                  
                0,                              /* RB-BEGIN-PAGE - zero */
                0,                              /* RB-END-PAGE - zero */
               no,                              /* RB-TEST-PATTERN */
               "Resumen Propias y de Terceros",         /* RB-WINDOW-TITLE */
               yes,                           /* RB-DISPLAY-ERRORS */
               yes,                           /* RB-DISPLAY-STATUS */
               no,                              /* RB-NO-WAIT */
               "v_general = " + fi-desde-fecha:screen-value + ";" + fi-hasta-fecha:screen-value 
               /* RB-OTHER-PARAMETERS */,
               ""
               ).   
          end.
         when  "2" Then 
         RUN  aderb\_prntrb2(
               (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
               "resumen_finca_gral-02", /* RB-REPORT-NAME */
               "",                             /* RB-DB-CONNECTION */
               "O",                             /* RB-INCLUDE-RECORDS */
               v_filtro,                              /* RB-FILTER */
               RB-MEMO-FILE,                              /* RB-MEMO-FILE */
               "?",                             /* RB-PRINT-DESTINATION */
               "",                              /* RB-PRINTER-NAME */
               "",                              /* RB-PRINTER-PORT */
               "",                              /* RB-OUTPUT-FILE */
                1,                              /* RB-NUMBER-COPIES  - zero */                  
                0,                              /* RB-BEGIN-PAGE - zero */
                0,                              /* RB-END-PAGE - zero */
               no,                              /* RB-TEST-PATTERN */
               "Resumen Propias y de Terceros",         /* RB-WINDOW-TITLE */
               yes,                           /* RB-DISPLAY-ERRORS */
               yes,                           /* RB-DISPLAY-STATUS */
               no,                              /* RB-NO-WAIT */
               "v_general = " + fi-desde-fecha:screen-value + ";" + fi-hasta-fecha:screen-value 
               /* RB-OTHER-PARAMETERS */,
               ""
               ).   
        
        end.     
        os-delete value(RB-MEMO-FILE).
end.
Else
do:
  run p_genera_excel_gral.p (input v_reporte).
end.
        
        
for each rb_resumen_agricola where id_reporte = v_reporte :
   delete rb_resumen_agricola.
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
  DISPLAY Fi-sector nombre-sector FI-desde-fecha RADIO-SET-1 T-excel 
          FI-hasta-fecha 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK Fi-sector Btn_Cancel FI-desde-fecha RADIO-SET-1 T-excel 
         FI-hasta-fecha 
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
fi-desde-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
fi-hasta-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

