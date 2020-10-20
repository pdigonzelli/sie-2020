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

DEF VAR ultimos-dias AS INTEGER EXTENT 12 INITIAL [31,28,31,30,31,30,31,31,30,31,30,31].
DEF VAR dias-semana AS CHARACTER EXTENT 7 INITIAL ["DOMINGO", "LUNES","MARTES","MIERCOLES","JUEVES","VIERNES","SABADO"].

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
&Scoped-Define ENABLED-OBJECTS Btn_OK FI-periodo FI-mes FI-quincena ~
Btn_Cancel RADIO-SET-3 FI-fecha FI-semana fi-empresa FI-sucursal FI-sector ~
FI-id_origen FI-id_proveedor FI-zona FI-grupo RADIO-SET-1 FI-copias ~
FI-nro_planilla FI-impresora B-cambiar 
&Scoped-Define DISPLAYED-OBJECTS FI-periodo FI-mes FI-quincena RADIO-SET-3 ~
FI-desde-1 FI-hasta-1 FI-desde-2 FI-hasta-2 FI-fecha FI-diasemana FI-semana ~
fi-empresa nombre-empresa FI-sucursal nombre-sucursal FI-sector ~
nombre-sector FI-id_origen FI-id_proveedor nombre-finca FI-zona FI-grupo ~
RADIO-SET-1 FI-copias FI-nro_planilla FI-impresora 

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
     LABEL "Generar" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-copias AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Nro copias" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-desde-1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FI-desde-2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FI-diasemana AS CHARACTER FORMAT "X(256)":U 
     LABEL "D¡a" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Emp Contratista" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE FI-fecha AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fecha Planilla" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE FI-grupo AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Grupo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .95 NO-UNDO.

DEFINE VARIABLE FI-hasta-1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE FI-hasta-2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE FI-id_origen AS CHARACTER FORMAT "X(256)":U 
     LABEL "Finca" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE FI-id_proveedor AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Productor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FI-impresora AS CHARACTER FORMAT "X(256)":U 
     LABEL "Impresora" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1.19 NO-UNDO.

DEFINE VARIABLE FI-mes AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-nro_planilla AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nro Planilla" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE FI-periodo AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FI-quincena AS CHARACTER FORMAT "X(256)":U 
     LABEL "Quincena" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-sector AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE FI-semana AS CHARACTER FORMAT "X(256)":U 
     LABEL "Semana" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-sucursal AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE FI-zona AS CHARACTER FORMAT "X(256)":U 
     LABEL "Zona" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE nombre-finca AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE nombre-sucursal AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Carga Anterior", 1,
"Archivo Personal", 2,
"Nro Planilla", 3,
"Planilla Sin Legajos",4
     SIZE 25 BY 3.81 NO-UNDO.

DEFINE VARIABLE RADIO-SET-3 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "PLANILLA 1", 1,
"PLANILLA 2", 2,
"PLANILLA 3", 3
     SIZE 27 BY 3
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.48 COL 85 WIDGET-ID 12
     FI-periodo AT ROW 1.71 COL 32 COLON-ALIGNED WIDGET-ID 66
     FI-mes AT ROW 1.71 COL 52 COLON-ALIGNED WIDGET-ID 68
     FI-quincena AT ROW 1.71 COL 71 COLON-ALIGNED WIDGET-ID 70
     Btn_Cancel AT ROW 2.67 COL 85 WIDGET-ID 10
     RADIO-SET-3 AT ROW 4.1 COL 8 NO-LABEL WIDGET-ID 4
     FI-desde-1 AT ROW 5.29 COL 53 COLON-ALIGNED WIDGET-ID 52
     FI-hasta-1 AT ROW 5.29 COL 88 COLON-ALIGNED WIDGET-ID 58
     FI-desde-2 AT ROW 7.91 COL 53 COLON-ALIGNED WIDGET-ID 62
     FI-hasta-2 AT ROW 7.91 COL 88 COLON-ALIGNED WIDGET-ID 64
     FI-fecha AT ROW 9.33 COL 15 COLON-ALIGNED WIDGET-ID 18
     FI-diasemana AT ROW 9.33 COL 37 COLON-ALIGNED WIDGET-ID 72
     FI-semana AT ROW 9.33 COL 71 COLON-ALIGNED WIDGET-ID 74
     fi-empresa AT ROW 12.19 COL 16 COLON-ALIGNED WIDGET-ID 16
     nombre-empresa AT ROW 12.19 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     FI-sucursal AT ROW 13.14 COL 16 COLON-ALIGNED WIDGET-ID 32
     nombre-sucursal AT ROW 13.14 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     FI-sector AT ROW 14.1 COL 16 COLON-ALIGNED WIDGET-ID 30
     nombre-sector AT ROW 14.1 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     FI-id_origen AT ROW 15.76 COL 15 COLON-ALIGNED WIDGET-ID 22
     FI-id_proveedor AT ROW 15.76 COL 38 COLON-ALIGNED WIDGET-ID 24
     nombre-finca AT ROW 15.76 COL 52 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     FI-zona AT ROW 17.19 COL 15 COLON-ALIGNED WIDGET-ID 34
     FI-grupo AT ROW 18.38 COL 16 COLON-ALIGNED WIDGET-ID 20
     RADIO-SET-1 AT ROW 20.1 COL 17.8 NO-LABEL WIDGET-ID 42
     FI-copias AT ROW 20.52 COL 79 COLON-ALIGNED WIDGET-ID 14
     FI-nro_planilla AT ROW 24.52 COL 15 COLON-ALIGNED WIDGET-ID 28
     FI-impresora AT ROW 25.76 COL 15 COLON-ALIGNED WIDGET-ID 26
     B-cambiar AT ROW 25.76 COL 82 WIDGET-ID 8
     "1era Semana" VIEW-AS TEXT
          SIZE 64 BY .71 AT ROW 4.1 COL 40 WIDGET-ID 56
          BGCOLOR 1 FGCOLOR 15 
     "2da Semana" VIEW-AS TEXT
          SIZE 64 BY .71 AT ROW 6.71 COL 40 WIDGET-ID 60
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.8 BY 27.19 WIDGET-ID 100.


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
         TITLE              = "Planilla - Tarja Personal x Empresa x Finca"
         HEIGHT             = 27.19
         WIDTH              = 106.8
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
/* SETTINGS FOR FILL-IN FI-desde-1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-desde-2 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-diasemana IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-hasta-1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-hasta-2 IN FRAME fMain
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
ON END-ERROR OF wWin /* Planilla - Tarja Personal x Empresa x Finca */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Planilla - Tarja Personal x Empresa x Finca */
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
ON CHOOSE OF Btn_OK IN FRAME fMain /* Generar */
DO:
  define var v_nro as INTEGER NO-UNDO.
  DEFINE VAR v_tipo AS INTEGER NO-UNDO.
  DEFINE VAR v_empresa AS INTEGER NO-UNDO.
  DEFINE VAR v_sucursal AS INTEGER NO-UNDO.
  DEFINE VAR v_sector AS INTEGER NO-UNDO.
  DEFINE VAR v_fecha AS DATE NO-UNDO.
  DEFINE VAR v_proveedor AS INTEGER NO-UNDO.
  DEFINE VAR v_origen AS INTEGER NO-UNDO.
  DEFINE VAR v_grupo AS INTEGER NO-UNDO.
  DEFINE VAR v_nroplanilla AS INTEGER NO-UNDO.
  DEFINE VAR v_desde_fecha AS DATE NO-UNDO.
  DEF VAR v_hasta_fecha AS DATE NO-UNDO.



v_tipo = INTEGER(radio-set-3:SCREEN-VALUE).
v_nro = integer(fi-copias:screen-value).
v_empresa = integer(fi-empresa:screen-value).
v_sucursal = integer(fi-sucursal:screen-value).
v_sector = integer(fi-sector:screen-value).
v_fecha = date(fi-fecha:screen-value).
v_proveedor = integer(fi-id_proveedor:screen-value).
v_origen = integer(fi-id_origen:screen-value).
v_grupo = integer(fi-grupo:screen-value).

IF INTEGER(fi-semana:SCREEN-VALUE) = 1 THEN 
do:
     v_desde_fecha = DATE(fi-desde-1:SCREEN-VALUE).
     v_hasta_fecha = DATE(fi-hasta-1:SCREEN-VALUE).
END.
ELSE
do:
     v_desde_fecha = DATE(fi-desde-2:SCREEN-VALUE).
     v_hasta_fecha = DATE(fi-hasta-2:SCREEN-VALUE).
END.



case radio-set-1:screen-value : /* Carga anterior */
   when "1" Then
    do:
         CASE radio-set-3:screen-value:
             WHEN "1" OR WHEN "3" THEN
             DO:
               RUN p_genera_planilla.p (INPUT v_tipo, INPUT v_empresa, INPUT v_sucursal,INPUT v_sector, 
                                        INPUT v_fecha, INPUT v_proveedor, INPUT v_origen, INPUT v_grupo, 
                                        INPUT v_desde_fecha, INPUT v_hasta_fecha,
                                        OUTPUT v_nroplanilla). 
    
               IF v_nroplanilla <> 0 THEN
                  RUN imprime-planilla ( INPUT v_empresa,
                                         INPUT v_sucursal,
                                         INPUT v_sector,
                                         INPUT v_tipo,
                                         INPUT v_nroplanilla).
             END.
             WHEN "2" THEN
             DO:
               RUN p_genera_planilla_extras.p (INPUT v_tipo, INPUT v_empresa, INPUT v_sucursal,INPUT v_sector, 
                                        
                                        INPUT v_fecha, INPUT v_proveedor, INPUT v_origen, INPUT v_grupo, 
                                        INPUT v_desde_fecha, INPUT v_hasta_fecha,
                                        OUTPUT v_nroplanilla).
              IF v_nroplanilla <> 0 THEN
              DO:
               run wcontroltareasextras.w (
                                     INPUT v_empresa,
                                     input v_sucursal,
                                     input v_sector, 
                                     INPUT v_tipo,
                                     INPUT v_nroplanilla,
                                     INPUT v_fecha).
              END.
             END.
         END CASE.
   end. 
    WHEN "2" THEN /* Archivo Personal */
    DO:
        CASE radio-set-3:screen-value:
            WHEN "1" OR WHEN "3" THEN
            DO:
              RUN p_genera_planilla_archivopers.p (INPUT v_tipo, INPUT v_empresa, INPUT v_sucursal,INPUT v_sector, 
                                                   INPUT v_fecha, INPUT v_proveedor, INPUT v_origen, INPUT v_grupo, 
                                                   INPUT v_desde_fecha, INPUT v_hasta_fecha,
                                                   OUTPUT v_nroplanilla). 

              IF v_nroplanilla <> 0 THEN
                 RUN imprime-planilla ( INPUT v_empresa,
                                        INPUT v_sucursal,
                                        INPUT v_sector,
                                        INPUT v_tipo,
                                        INPUT v_nroplanilla).
            END.
            WHEN "2" THEN
            DO:
              RUN p_genera_planilla_extras.p (INPUT v_tipo, INPUT v_empresa, INPUT v_sucursal,INPUT v_sector, 
                                       INPUT v_fecha, INPUT v_proveedor, INPUT v_origen, INPUT v_grupo, 
                                       INPUT v_desde_fecha, INPUT v_hasta_fecha,
                                       OUTPUT v_nroplanilla).
             IF v_nroplanilla <> 0 THEN
             DO:
              run wcontroltareasextras.w (
                                    INPUT v_empresa,
                                    input v_sucursal,
                                    input v_sector, 
                                    INPUT v_tipo,
                                    INPUT v_nroplanilla,
                                    INPUT v_fecha).
             END.
            END.
        END CASE.
    END.

    WHEN "4" THEN /* Planilla sin Legajos */
    DO:
        CASE radio-set-3:screen-value:
            WHEN "1" OR WHEN "3" THEN
            DO:
              RUN p_genera_planilla_vacia.p (INPUT v_tipo, INPUT v_empresa, INPUT v_sucursal,INPUT v_sector, 
                                                   INPUT v_fecha, INPUT v_proveedor, INPUT v_origen, INPUT v_grupo, 
                                                   INPUT v_desde_fecha, INPUT v_hasta_fecha,
                                                   OUTPUT v_nroplanilla). 

            END.
        END CASE.
    END.
END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-empresa wWin
ON MOUSE-SELECT-DBLCLICK OF fi-empresa IN FRAME fMain /* Emp Contratista */
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


&Scoped-define SELF-NAME FI-fecha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-fecha wWin
ON LEAVE OF FI-fecha IN FRAME fMain /* Fecha Planilla */
DO:
  RUN cargar-dia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-id_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-id_origen wWin
ON MOUSE-SELECT-DBLCLICK OF FI-id_origen IN FRAME fMain /* Finca */
DO:
 define var r as rowid no-undo.
 
 run wselfinca.w (output r).
 find FIRST origenes where rowid(origenes) = r no-lock no-error.
 if available origenes then 
 do:
   fi-id_proveedor:screen-value = string(origenes.id_proveedor).
   fi-id_origen:screen-value = string(origenes.id_origen).
   nombre-finca:screen-value = origenes.descripcion.
 end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-mes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-mes wWin
ON LEAVE OF FI-mes IN FRAME fMain /* Mes */
DO:
  RUN cargar-semanas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-quincena
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-quincena wWin
ON LEAVE OF FI-quincena IN FRAME fMain /* Quincena */
DO:
  RUN cargar-semanas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-sector
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON GO OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON LEAVE OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON MOUSE-SELECT-DBLCLICK OF FI-sector IN FRAME fMain /* Sector */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-sector wWin
ON U1 OF FI-sector IN FRAME fMain /* Sector */
DO:
  run descriptivos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-sucursal
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar-dia wWin 
PROCEDURE cargar-dia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v_dia AS INTEGER NO-UNDO.
DEF VAR v_nombre_dia AS CHARACTER NO-UNDO.

v_dia = WEEKDAY(date(fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
v_nombre_dia = dias-semana[v_dia].
fi-diasemana:SCREEN-VALUE = v_nombre_dia.

IF DATE(fi-fecha:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <= DATE(fi-hasta-1:SCREEN-VALUE) THEN fi-semana:SCREEN-VALUE = "1".
   ELSE fi-semana:SCREEN-VALUE = "2".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar-semanas wWin 
PROCEDURE cargar-semanas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
IF INTEGER(fi-quincena:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 1 THEN
DO:
    fi-desde-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "01/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
    fi-hasta-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "07/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
    fi-desde-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "08/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
    fi-hasta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "15/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
END.
ELSE
DO:
    fi-desde-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "16/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
    fi-hasta-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "22/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
    fi-desde-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "23/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
    fi-hasta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ultimos-dias[INTEGER(fi-mes:SCREEN-VALUE)]) + "/" + fi-mes:SCREEN-VALUE + "/" + fi-periodo:SCREEN-VALUE.
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
 find first proveedores where proveedores.id_proveedor = INTEGER(fi-empresa:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
 if available proveedores Then
      nombre-empresa:screen-value = proveedores.nombre.
   ELSE
     nombre-empresa:screen-value = "".

 find first sucursales where sucursales.id_sucursal = INTEGER(fi-sucursal:screen-value in frame {&FRAME-NAME} ) no-lock no-error.
 if available sucursales Then
          nombre-sucursal:screen-value = sucursales.nombre.
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
  DISPLAY FI-periodo FI-mes FI-quincena RADIO-SET-3 FI-desde-1 FI-hasta-1 
          FI-desde-2 FI-hasta-2 FI-fecha FI-diasemana FI-semana fi-empresa 
          nombre-empresa FI-sucursal nombre-sucursal FI-sector nombre-sector 
          FI-id_origen FI-id_proveedor nombre-finca FI-zona FI-grupo RADIO-SET-1 
          FI-copias FI-nro_planilla FI-impresora 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK FI-periodo FI-mes FI-quincena Btn_Cancel RADIO-SET-3 FI-fecha 
         FI-semana fi-empresa FI-sucursal FI-sector FI-id_origen 
         FI-id_proveedor FI-zona FI-grupo RADIO-SET-1 FI-copias FI-nro_planilla 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprime-planilla wWin 
PROCEDURE imprime-planilla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p_empresa AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_sucursal AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_sector AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_tipo AS INTEGER NO-UNDO.
  DEF INPUT PARAMETER p_nro AS INTEGER NO-UNDO.
  
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "" NO-UNDO.
  DEFINE VAR v_filtro AS CHARACTER NO-UNDO.

            
            

            RB-MEMO-FILE  = vlc_dir_spool + string(next-value(next-spool),"99999999").
            
            
            v_filtro = "control_tareas.id_empresa = " + STRING(p_empresa) +
                       " and control_tareas.id_sucursal = " + STRING(p_sucursal) + 
                       " and control_tareas.id_sector = " + STRING(p_sector) + 
                       " and control_tareas.id_tipo_planilla = " + STRING(p_tipo) + 
                       " and control_tareas.nro_planilla = " + STRING(p_nro).
            
            RUN  aderb\_prntrb2(
                 (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
                 "planilla_personal-01",                    /* RB-REPORT-NAME */
                 "",                             /* RB-DB-CONNECTION */
                 "O",                             /* RB-INCLUDE-RECORDS */
                 v_filtro,                              /* RB-FILTER */
                 RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                 "?",                             /* RB-PRINT-DESTINATION */
                 "?",  /* RB-PRINTER-NAME */
                 "",                              /* RB-PRINTER-PORT */
                 "",                              /* RB-OUTPUT-FILE */
                  0,                              /* RB-NUMBER-COPIES  - zero */                  
                  0,                              /* RB-BEGIN-PAGE - zero */
                  0,                              /* RB-END-PAGE - zero */
                 no,                              /* RB-TEST-PATTERN */
                 "Planilla Tarja Personal",         /* RB-WINDOW-TITLE */
                 yes,                           /* RB-DISPLAY-ERRORS */
                 yes,                           /* RB-DISPLAY-STATUS */
                 no,                              /* RB-NO-WAIT */
                 "", /* RB-OTHER-PARAMETERS */
                 ""
                 ).   

            OS-DELETE VALUE(RB-MEMO-FILE). 




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
fi-sucursal:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
fi-id_origen:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
fi-fecha:screen-value in frame {&FRAME-NAME} = string(today,"99/99/9999").
fi-sector:load-mouse-pointer("glove") in frame {&FRAME-NAME}.
fi-impresora:screen-value in frame {&frame-name} = session:printer-name.

fi-periodo:SCREEN-VALUE in frame {&FRAME-NAME} = STRING(YEAR(TODAY),"9999").
fi-mes:SCREEN-VALUE in frame {&FRAME-NAME} = STRING(MONTH(TODAY),"99").

IF DAY(TODAY) < 16 THEN fi-quincena:SCREEN-VALUE in frame {&FRAME-NAME} = "1".
                   ELSE fi-quincena:SCREEN-VALUE in frame {&FRAME-NAME} = "2".

RUN cargar-semanas.
RUN cargar-dia.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

