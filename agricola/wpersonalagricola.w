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
&Scoped-Define ENABLED-OBJECTS Btn_OK FI-empresa Btn_Cancel FI-desde_fecha ~
FI-hasta_fecha FI-sector FI-carpeta 
&Scoped-Define DISPLAYED-OBJECTS FI-empresa nombre-empresa FI-desde_fecha ~
FI-hasta_fecha FI-sector nombre-sector FI-carpeta 

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
     SIZE 16 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Genera Excel" 
     SIZE 16 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-carpeta AS CHARACTER FORMAT "X(256)":U INITIAL "z:~\temp~\" 
     LABEL "Carpeta" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE FI-desde_fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde Fecha" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .95 NO-UNDO.

DEFINE VARIABLE FI-hasta_fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta Fecha" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-sector AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Sector" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE nombre-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE nombre-sector AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_OK AT ROW 1.48 COL 93 WIDGET-ID 4
     FI-empresa AT ROW 2.43 COL 21.2 COLON-ALIGNED WIDGET-ID 8
     nombre-empresa AT ROW 2.43 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     Btn_Cancel AT ROW 2.67 COL 93 WIDGET-ID 2
     FI-desde_fecha AT ROW 3.62 COL 21 COLON-ALIGNED WIDGET-ID 6
     FI-hasta_fecha AT ROW 4.81 COL 21 COLON-ALIGNED WIDGET-ID 10
     FI-sector AT ROW 6 COL 21 COLON-ALIGNED WIDGET-ID 12
     nombre-sector AT ROW 6 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     FI-carpeta AT ROW 7.43 COL 21 COLON-ALIGNED WIDGET-ID 24
     "  Filtros" VIEW-AS TEXT
          SIZE 70 BY .71 AT ROW 1.48 COL 4 WIDGET-ID 18
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.4 BY 7.91 WIDGET-ID 100.


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
         TITLE              = "Detalle de Personal x Sector"
         HEIGHT             = 7.91
         WIDTH              = 111.4
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" wWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
ON END-ERROR OF wWin /* Detalle de Personal x Sector */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Detalle de Personal x Sector */
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
ON CHOOSE OF Btn_OK IN FRAME fMain /* Genera Excel */
DO:
  run p_genera_pers_agricola.p (input integer(fi-empresa:screen-value in frame {&frame-name}),
                                input date(fi-desde_fecha:screen-value in frame {&frame-name}),
                                input date(fi-hasta_fecha:screen-value in frame {&frame-name}),
                                input integer(fi-sector:screen-value in frame {&frame-name}),
                                INPUT fi-carpeta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

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
  DISPLAY FI-empresa nombre-empresa FI-desde_fecha FI-hasta_fecha FI-sector 
          nombre-sector FI-carpeta 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE Btn_OK FI-empresa Btn_Cancel FI-desde_fecha FI-hasta_fecha FI-sector 
         FI-carpeta 
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
fi-desde_fecha:screen-value in frame {&frame-name} = "01/01/" + string(year(today),"9999").
fi-hasta_fecha:screen-value in frame {&frame-name} = string(today,"99/99/9999").
run descriptivos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

