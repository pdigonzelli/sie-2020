&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
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
define var del_empresa as integer.
define var del_sucursal as integer.
define var del_lote as integer.
define var deshabilita as logical initial false.
/* Local Variable Definitions ---                                       */
{custom/support/cabmcdvar.i}

/**********EMPIEZA-TEMP-TABLES*************/

&IF DEFINED(TABLA-CABECERA) <> 0 &THEN
    define buffer aux-cabecera for {&TABLA-CABECERA}.
&ENDIF
/**********TERMINA-TEMP-TABLES*************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS b-detalle FILL-IN-1 boton_ar ar BUTTON-8 ~
BUTTON-10 BUTTON-11 BUTTON-1 Boton1 BUTTON-7 BUTTON-9 REC1 REC3 REC4 ~
RECT-16 RECT-17 RECT-18 RECT-19 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-2 ar FILL1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cambio-detalle F-Frame-Win 
FUNCTION cambio-detalle RETURNS LOGICAL
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_composicion_lote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_inspecciones_lote1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_lotes_jugo_full AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_origen_tambores_lote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q_arrastre_lote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q_sobrante AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_arrastre_lote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_lotes_jugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_sobrante AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-detalle 
     LABEL "&Detalle" 
     SIZE 22 BY 1.67.

DEFINE BUTTON Boton1 
     LABEL "Características Jugo Clarificado" 
     SIZE 34 BY 1.67.

DEFINE BUTTON boton_ar 
     LABEL "AR-" 
     SIZE 7 BY 1.05.

DEFINE BUTTON BUTTON-1  NO-CONVERT-3D-COLORS
     LABEL "Impresión de Etiquetas" 
     SIZE 24 BY 1.24.

DEFINE BUTTON BUTTON-10 
     LABEL "P y G" 
     SIZE 9 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-11 
     LABEL "WILD" 
     SIZE 9 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-3 
     LABEL "Producción" 
     SIZE 16 BY 1.43.

DEFINE BUTTON BUTTON-5 
     LABEL "Sobrante" 
     SIZE 15 BY 1.43.

DEFINE BUTTON BUTTON-6 
     LABEL "Quitar Tambor" 
     SIZE 17 BY 1.43.

DEFINE BUTTON BUTTON-7 
     LABEL "Impresión de Etiquetas" 
     SIZE 24 BY 1.24.

DEFINE BUTTON BUTTON-8 
     LABEL "Comunes" 
     SIZE 11 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-9 
     LABEL "Impresión de Etiquetas" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE ar AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-2 AS CHARACTER FORMAT "X(256)":U INITIAL "INSPECCIONES - ANALISIS PARCIALES" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1.05
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Impresión de Etiquetas" 
     VIEW-AS FILL-IN 
     SIZE 51 BY .95
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL1 AS CHARACTER FORMAT "X(256)":U INITIAL "COMPOSICION DEL LOTE - ORIGENES DE JUGO" 
     VIEW-AS FILL-IN 
     SIZE 150 BY 1.05
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE REC1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 151 BY 9.05.

DEFINE RECTANGLE REC3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 151 BY 8.38.

DEFINE RECTANGLE REC4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 8.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 152 BY 26.19.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 152 BY 9.05.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 1 BY 7.43.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 8.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 2.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     b-detalle AT ROW 1.38 COL 108.8
     FILL-IN-1 AT ROW 3.38 COL 96 COLON-ALIGNED NO-LABEL
     FILL-2 AT ROW 4.1 COL 2 NO-LABEL
     boton_ar AT ROW 4.33 COL 130
     ar AT ROW 4.33 COL 136 COLON-ALIGNED NO-LABEL
     BUTTON-8 AT ROW 4.43 COL 98
     BUTTON-10 AT ROW 4.43 COL 110
     BUTTON-11 AT ROW 4.43 COL 120
     BUTTON-1 AT ROW 12.52 COL 49
     FILL1 AT ROW 18.14 COL 2 NO-LABEL
     Boton1 AT ROW 19.33 COL 81
     BUTTON-6 AT ROW 19.38 COL 4
     BUTTON-5 AT ROW 19.38 COL 21
     BUTTON-3 AT ROW 19.38 COL 36
     BUTTON-7 AT ROW 21.38 COL 49
     BUTTON-9 AT ROW 21.43 COL 128
     REC1 AT ROW 9 COL 1
     REC3 AT ROW 10.52 COL 1
     REC4 AT ROW 19.14 COL 2
     RECT-16 AT ROW 1 COL 1
     RECT-17 AT ROW 18.14 COL 1
     RECT-18 AT ROW 11.14 COL 76
     RECT-19 AT ROW 19.1 COL 76
     RECT-4 AT ROW 1.24 COL 2
     "ARRASTRE" VIEW-AS TEXT
          SIZE 44 BY .86 AT ROW 21.48 COL 81
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "TAMBORES" VIEW-AS TEXT
          SIZE 43 BY .95 AT ROW 12.62 COL 5
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "SOBRANTE" VIEW-AS TEXT
          SIZE 43 BY .86 AT ROW 21.48 COL 5
          BGCOLOR 1 FGCOLOR 15 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152.2 BY 26.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 1
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 26.33
         WIDTH              = 152.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "visibles" F-Frame-Win _INLINE
/* Actions: ? custom/support/cusvis.p ? ? ? */
/* SmartFrame,uib,49268
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" F-Frame-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartFrame,uib,49268
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "tablas" F-Frame-Win _INLINE
/* Actions: ? custom/support/cabecera1.p ? ? ? */
/*cabecera,detalle*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
/* SETTINGS FOR BUTTON BUTTON-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL1 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b-detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-detalle F-Frame-Win
ON CHOOSE OF b-detalle IN FRAME F-Main /* Detalle */
DO:
   case  b-detalle:label:
   when "&Detalle" Then
    do:
      b-detalle:label = "&Principal".
      rec3:hidden = false.
      rec4:hidden = false.
      button-3:hidden = true.
      button-6:hidden = true.
      FILL1:visible = false.
      button-5:hidden = true.
      button-7:hidden = false.
      rec1:hidden = true.
      rect-17:hidden = true. 
      run select-page(2).
/*      run automatico in h_b_envasadores_fabrica1.*/
    end.
   otherwise 
     do:
      b-detalle:label = "&Detalle".
      rec3:hidden = true.
      rec4:hidden = true.
      rec1:hidden = false.
      button-3:hidden = false.
      button-7:hidden = true.
      button-6:hidden = false.
      FILL1:visible = TRUE.
      button-5:hidden = false.
      rect-17:hidden = false.
      run select-page(1).
         
     end.  
   end. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Boton1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Boton1 F-Frame-Win
ON CHOOSE OF Boton1 IN FRAME F-Main /* Características Jugo Clarificado */
DO:
define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo_full (output r).
  run w_datos_jugo_clarificado.w (input r).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME boton_ar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boton_ar F-Frame-Win
ON CHOOSE OF boton_ar IN FRAME F-Main /* AR- */
DO:
define var hcontainer as handle.
define var r as rowid.
define var f as integer.
RUN get-rowid1 in h_b_lotes_jugo_full (output r).

find lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
if available lotes_jugo then
do:
        for each tambores_industria of lotes_jugo no-lock.
                output to c:\lpt1.  
                put control "^XA".
                put control "^LH0,0".
                put control "^PQ" 1 "^FS".
        
                put control "^FO15,50^GB760,1120,3^FS".        
                put control "^FO50,60^A0R,560,280^FD" "AR" "^FS".
                put control "^FO50,410^A0R,560,120^FD" "-" "^FS".
                put control "^FO50,570^A0R,560,280^FD" ar:screen-value in frame F-Main "^FS".
                put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
                put control "^XZ".
                output close.
                
                do f = 1 to 30000:
                
                end.
        end.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 F-Frame-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Impresión de Etiquetas */
DO:
  define var anio as char.
  define var r as rowid.

  run get-rowid1 in h_b_composicion_lote (output r).
  find composicion_lote where rowid(composicion_lote) = r no-lock no-error.
  if available composicion_lote then
    do:
        anio = SUBSTRING(STRING(composicion_lote.fecha),7,2,"CHARACTER").
       
        run zebra_lotes_jugo.p (input r).
    end.
  else message "Existe un error. Debe ingresar los tambores producidos.".


   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 F-Frame-Win
ON CHOOSE OF BUTTON-10 IN FRAME F-Main /* P y G */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo_full (output r).


run zebra_inspecciones_lote_pyg.p (input r). 


/* define var gall1 as decimal.
define var gall2 as decimal.
 find lotes_jugo where rowid(lotes_jugo) = r  no-lock no-error.
if available lotes_jugo then
do:  
    find last inspecciones_lote of lotes_jugo no-lock no-error.
    if available inspecciones_lote then
     do:
        message inspecciones_lote.litros.
        find last brix where brix.brix <= inspecciones_lote.bx_correg no-lock no-error.
        
        if available brix then 
        do:
            gall1 = 260 / brix.pe.
            gall2 = gall1 / 3.785.
            message gall1 " final " round(gall2,2) " truncate--> " truncate(gall2,2).
        end.
        
        /* run zebra_inspecciones_lote.p (input rowid(inspecciones_lote)). */
     end.
    else message "Existe un error en las inspecciones".

end. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 F-Frame-Win
ON CHOOSE OF BUTTON-11 IN FRAME F-Main /* WILD */
DO:
define var hcontainer as handle.
define var p_wn as char.
define var r as rowid.


p_wn = ar:screen-value in frame F-Main.

RUN get-rowid1 in h_b_lotes_jugo_full (output r).

run zebra_inspecciones_lote_wild.p (input r, input p_wn).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 F-Frame-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Producción */
DO:
  define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo_full (output r).
  run wc_origen_tambores_produccion_jugo.w (input r).
  run dispatch in h_b_origen_tambores_lote ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 F-Frame-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Sobrante */
DO:
    define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo_full (output r).
  run wc_origen_tambores_sobrante_lote.w (input r).
  run dispatch in h_b_origen_tambores_lote ('open-query':U).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 F-Frame-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Quitar Tambor */
DO:
run borra-row in h_b_origen_tambores_lote.
run dispatch in h_b_origen_tambores_lote ('open_query':U).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 F-Frame-Win
ON CHOOSE OF BUTTON-7 IN FRAME F-Main /* Impresión de Etiquetas */
DO:
define var hcontainer as handle.
define var r as rowid.
define var entro as logical initial false.

RUN get-rowid1 in h_b_lotes_jugo_full (output r).
run zebra_lotes_jugo_sobrante.p (input r).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 F-Frame-Win
ON CHOOSE OF BUTTON-8 IN FRAME F-Main /* Comunes */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo_full (output r).

run zebra_inspecciones_lote.p (input r). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 F-Frame-Win
ON CHOOSE OF BUTTON-9 IN FRAME F-Main /* Impresión de Etiquetas */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo_full (output r).
run zebra_lotes_jugo_arrastre.p (input r).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.33 , 37.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 23.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.33 , 59.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 20.00 ) NO-ERROR.

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_lotes_jugo_full.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_lotes_jugo_full ).
       RUN set-position IN h_b_lotes_jugo_full ( 3.38 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b_lotes_jugo_full ( 5.62 , 150.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_lotes_jugo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_lotes_jugo ).
       RUN set-position IN h_v_lotes_jugo ( 9.33 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.67 , 149.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_origen_tambores_lote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_origen_tambores_lote ).
       RUN set-position IN h_b_origen_tambores_lote ( 21.00 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b_origen_tambores_lote ( 5.62 , 149.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_lotes_jugo_full. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_lotes_jugo_full ).

       /* Links to SmartViewer h_v_lotes_jugo. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo_full , 'Record':U , h_v_lotes_jugo ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_inspecciones_lote1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_inspecciones_lote1 ).
       RUN set-position IN h_b_inspecciones_lote1 ( 5.48 , 1.00 ) NO-ERROR.
       RUN set-size IN h_b_inspecciones_lote1 ( 4.76 , 146.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_composicion_lote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_b_composicion_lote ).
       /* Position in AB:  ( 13.62 , 10.00 ) */
       /* Size in UIB:  ( 4.76 , 51.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_sobrante.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = No':U ,
             OUTPUT h_v_sobrante ).
       RUN set-position IN h_v_sobrante ( 22.81 , 5.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.14 , 69.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_arrastre_lote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_arrastre_lote ).
       RUN set-position IN h_v_arrastre_lote ( 22.91 , 81.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.00 , 60.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/q_sobrante.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q_sobrante ).
       RUN set-position IN h_q_sobrante ( 19.19 , 56.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/q_arrastre_lote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q_arrastre_lote ).
       RUN set-position IN h_q_arrastre_lote ( 23.86 , 119.20 ) NO-ERROR.
       /* Size in UIB:  ( 2.05 , 11.60 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to csmartbrowser h_b_inspecciones_lote1. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo_full , 'record':U , h_b_inspecciones_lote1 ).

       /* Links to  h_b_composicion_lote. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo_full , 'record':U , h_b_composicion_lote ).

       /* Links to SmartViewer h_v_sobrante. */
       RUN add-link IN adm-broker-hdl ( h_q_sobrante , 'Record':U , h_v_sobrante ).

       /* Links to SmartViewer h_v_arrastre_lote. */
       RUN add-link IN adm-broker-hdl ( h_q_arrastre_lote , 'Record':U , h_v_arrastre_lote ).

       /* Links to SmartQuery h_q_sobrante. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo_full , 'record':U , h_q_sobrante ).

       /* Links to SmartQuery h_q_arrastre_lote. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo_full , 'record':U , h_q_arrastre_lote ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE borra-items F-Frame-Win 
PROCEDURE borra-items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(TABLA-ITEMS) <> 0 &THEN
{custom/support/borraitems.i}
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambio-fila F-Frame-Win 
PROCEDURE cambio-fila :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
define var r as rowid no-undo.
define var r1 as rowid.
if valid-handle(h) and h = hcabecera  then
do:
    if cambio-detalle() then
        run proceso-cabecera-detalle.
    run genero-detalle.
    run get-rowid1 in h_b_lotes_jugo_full (output r1).
    find lotes_jugo where rowid(lotes_jugo) = r1 no-error.
    if available lotes_jugo then
        do:
            if lotes_jugo.id_articulo = 21 then run habilitar_boton. else run deshabilitar_boton.
        end.

end.    
&IF DEFINED (TABLA-CABECERA) <> 0 &THEN
    run get-rowid-cabecera(output r).
    find aux-cabecera where rowid(aux-cabecera) = r no-error.
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancel-record F-Frame-Win 
PROCEDURE cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
message "cancel-record de la ventana" view-as alert-box.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consulta F-Frame-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dame-rowid-composicion F-Frame-Win 
PROCEDURE dame-rowid-composicion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid.
run get-rowid1 in h_b_composicion_lote (output r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita-viewer-paginas F-Frame-Win 
PROCEDURE deshabilita-viewer-paginas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var ch as character no-undo.
define var h as handle no-undo.
define var nro-paginas as integer no-undo.
define var i as integer no-undo.

run get-link-handle in adm-broker-hdl ( input this-procedure ,
                                    input "PAGE-SOURCE" ,
                                    OUTPUT ch). 
if ch <> "" then
do:
    h=widget-handle(ch).
    run nro-paginas in h ( output nro-paginas).
    do i = 1 to nro-paginas :
        run select-page(i).
        run deshabilita_viewer.
    end.
end.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilitar_boton F-Frame-Win 
PROCEDURE deshabilitar_boton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Boton1:visible in frame {&FRAME-NAME} = false.
run dispatch in h_b_origen_tambores_lote ('open_query':U).
run dispatch in h_b_origen_tambores_lote ('open-query':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer F-Frame-Win 
PROCEDURE deshabilita_viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid-cabecera F-Frame-Win 
PROCEDURE devuelve-rowid-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.

run get-rowid in hcabecera ( output r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid-detalle F-Frame-Win 
PROCEDURE devuelve-rowid-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.

run get-rowid in hdetalle ( output r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-1 FILL-2 ar FILL1 
      WITH FRAME F-Main.
  ENABLE b-detalle FILL-IN-1 boton_ar ar BUTTON-8 BUTTON-10 BUTTON-11 BUTTON-1 
         Boton1 BUTTON-7 BUTTON-9 REC1 REC3 REC4 RECT-16 RECT-17 RECT-18 
         RECT-19 RECT-4 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genero-detalle F-Frame-Win 
PROCEDURE genero-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run genero-detalle-interno.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genero-detalle-interno F-Frame-Win 
PROCEDURE genero-detalle-interno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(TABLA-CABECERA) <> 0 &THEN
{custom/support/gendetalle.i &cabecera={&TABLA-CABECERA}
                &detalle={&TABLA-DETALLE}}
&ENDIF                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-deshabilitados-paleta F-Frame-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid F-Frame-Win 
PROCEDURE get-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r as rowid no-undo.
run devuelve-rowid(output r).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-cabecera F-Frame-Win 
PROCEDURE get-rowid-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.

r = ?.
if valid-handle(hcabecera) then
    run get-rowid1 in hcabecera ( output r ) no-error.
else
    r = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-detalle F-Frame-Win 
PROCEDURE get-rowid-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.
r = ?.
if valid-handle(hdetalle) then
    run get-rowid1 in hdetalle ( output r) no-error.
else
    r = ?.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE got-rowid F-Frame-Win 
PROCEDURE got-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter v1 as integer.
define output parameter v2 as integer.
define output parameter v3 as integer.
define var r as rowid.

run get-rowid1 in h_b_lotes_jugo_full (output r).

for each lotes_jugo where rowid(lotes_jugo) = r.
    v1 = lotes_jugo.id_empresa.
    v2 = lotes_jugo.id_sucursal.
    v3 = lotes_jugo.id_lote.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-relacion-viewer-pagina F-Frame-Win 
PROCEDURE habilita-relacion-viewer-pagina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var ch as character no-undo.
define var h as handle no-undo.
define var nro-paginas as integer no-undo.
define var i as integer no-undo.

run get-link-handle in adm-broker-hdl ( input this-procedure ,
                                    input "PAGE-SOURCE" ,
                                    OUTPUT ch). 
if ch <> "" then
do:
    h=widget-handle(ch).
    run nro-paginas in h ( output nro-paginas).
    do i = 1 to nro-paginas :
        run select-page(i).
        run habilitar_relacion_viewer.
    end.
end.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilitar_boton F-Frame-Win 
PROCEDURE habilitar_boton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Boton1:visible in frame {&FRAME-NAME} = true.

run dispatch in h_b_origen_tambores_lote ('open_query':U).
run dispatch in h_b_origen_tambores_lote ('open-query':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresion F-Frame-Win 
PROCEDURE impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.
define var string-memo as char.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

/******************************************************************/
/***********OBTENGO LAS VARIABLES DEL REGISTRO ACTUAL**************/
define var v1 as integer.
define var v2 as integer.
define var v3 as integer.

run got-rowid (output v1, output v2, output v3).

/******************************************************************/


/***********************************************************************************************************/
/*************************CARGO LOS ENVASADORES EN EL MEMO**************************************************/
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".



OUTPUT TO C:\TEMP\LOTES_JUGO.TXT.


/******************************MEMO PARA LA FECHA***************************************************/
put "NEWMEMO FECHA_COMP:" skip "~{".
for each composicion_lote where composicion_lote.id_empresa = v1 and
                                composicion_lote.id_sucursal = v2 and
                                composicion_lote.id_lote = v3 no-lock:
    
    put composicion_lote.fecha "~~n" skip.   
    
end.
put "~}" skip(1).

/****************************MEMO PARA LA COMPOSICION***********************************************/
put "NEWMEMO COMPOSICION:" skip "~{".

for each composicion_lote where composicion_lote.id_empresa = v1 and
                                composicion_lote.id_sucursal = v2 and
                                composicion_lote.id_lote = v3 no-lock:
    
    put composicion_lote.cantidad_tambores " tambores, desde el " composicion_lote.numeracion_desde " hasta "
        composicion_lote.numeracion_hasta "." "~~n" skip.   
end.
put "~}" skip(1).


/*****************************MEMO PARA ENVALADORES**************************************************/
put "NEWMEMO ENVALADORES:" skip "~{".
for each composicion_lote where composicion_lote.id_empresa = v1 and
                                composicion_lote.id_sucursal = v2 and
                                composicion_lote.id_lote = v3 no-lock:
    
    for each envasadores_fabrica where envasadores_fabrica.id_empresa = composicion_lote.id_empresa and
                                       envasadores_fabrica.id_sucursal = composicion_lote.id_sucursal and
                                       envasadores_fabrica.id_lote = composicion_lote.id_lote and
                                       envasadores_fabrica.fecha = composicion_lote.fecha no-lock: 
        
        find first legajo where legajo.lega-12 = envasadores_fabrica.id_legajo no-lock no-error.
        if available legajo then
        do:
            put legajo.nomb-12 "~~n" skip.       
        end.
    end.
end.
put "~}" skip(1).

/****************************MEMO PARA EL ORIGEN DEL JUGO***********************************************/
put "NEWMEMO ORIGEN_JUGO:" skip "~{".
for each envasado_lote where envasado_lote.id_empresa = v1 and
                             envasado_lote.id_sucursal = v2 and
                             envasado_lote.id_lote = v3 no-lock:
   find tipos_origen where tipos_origen.id_tipo_origen = envasado_lote.id_tipo_origen no-lock no-error.
   if available tipos_origen then
    do:
        if tipos_origen.id_tipo_origen = 1 then
            put "Número de " tipos_origen.descripcion envasado_lote.id_origen "/00," " cantidad " 
                envasado_lote.cantidad "~~n" skip.
        else
            put tipos_origen.descripcion " del lote " envasado_lote.id_origen "/00," " cantidad " 
                envasado_lote.cantidad "~~n" skip.
    end.
    
END.
put "~}".
OUTPUT CLOSE.

/**********************************************************************************************************/



/************************************************************************************************************/
/********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
/************************************************************************************************************/
/*message "esta comentado lo del report builder".*/

define var v_filtro as character.
define var filtro as char.



filtro = "lotes_jugo.id_empresa = " + string(v1) + 
           " and lotes_jugo.id_sucursal = " + string(v2) +
           " and lotes_jugo.id_lote = " + string(v3).

/* message "filtro " filtro view-as alert-box.*/

v_filtro = filtro.

find lotes_jugo where lotes_jugo.id_empresa = v1 and
                      lotes_jugo.id_sucursal = v2 and
                      lotes_jugo.id_lote = v3 no-lock no-error.

if lotes_jugo.id_articulo = 20 or lotes_jugo.id_articulo = 52 then
do:

      RUN  aderb\_printrb(
       "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
       "lotes_jugo",                    /* RB-REPORT-NAME */
       "",                             /* RB-DB-CONNECTION */
       "O",                             /* RB-INCLUDE-RECORDS */
       v_filtro,                              /* RB-FILTER */
       "c:\temp\lotes_jugo.txt",                              /* RB-MEMO-FILE */
       "D",                             /* RB-PRINT-DESTINATION */
       "?",                              /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        1,                              /* RB-NUMBER-COPIES  - zero */                  
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "Lotes de Jugo",         /* RB-WINDOW-TITLE */
       yes,                           /* RB-DISPLAY-ERRORS */
       yes,                           /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "" /* RB-OTHER-PARAMETERS */
       ).   
end.
else
if lotes_jugo.id_articulo = 21 or lotes_jugo.id_articulo = 53 then
do:

/*RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000)) + '.TXT'.*/

      RUN  aderb\_printrb(
       "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
       "lotes_jugo_clarificado",                    /* RB-REPORT-NAME */
       "",                             /* RB-DB-CONNECTION */
       "O",                             /* RB-INCLUDE-RECORDS */
       v_filtro,                              /* RB-FILTER */
       "c:\temp\lotes_jugo.txt",                              /* RB-MEMO-FILE */
       "D",                             /* RB-PRINT-DESTINATION */
       "?",                              /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        1,                              /* RB-NUMBER-COPIES  - zero */                  
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "Lotes de Jugo",         /* RB-WINDOW-TITLE */
       yes,                           /* RB-DISPLAY-ERRORS */
       yes,                           /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "" /* RB-OTHER-PARAMETERS */
       ).
end.
else message "Esta usando un producto no valido" view-as alert-box.

       
os-delete value("c:\temp\lotes_jugo.txt").

end.
/************************************************************************************************************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page F-Frame-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 /* Code placed here will execute PRIOR to standard behavior. */
  
  run get-attribute ('current-page').
  if return-value = "2" and valid-handle(hdetalle) and pagina-origen = "1" then
  do:
        /*run check-modified in hcabecera (input "check").*/
        run genero-detalle.
        run dispatch in hdetalle ('open-query').
  end.

  run get-attribute ('current-page').
  if return-value = "1" and valid-handle(hcabecera) then
  do:
    if cambio-detalle() then    
        run proceso-cabecera-detalle.
  end.
  run get-attribute ('current-page').
  pagina-origen=return-value.


  /* Dispatch standard ADM method.                             */
 
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit F-Frame-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
{custom/support/localexit.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize F-Frame-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  hide frame {&FRAME-NAME}.
  {custom/support/cabmcd.i}
  run deshabilita-viewer-paginas.
      rec3:hidden = true.
      rec4:hidden = true.
      rect-19:hidden = true.
      button-7:hidden = true.
      rec1:hidden = false.
/***********************************************************************/
    define var r1 as rowid.
    run get-rowid1 in h_b_lotes_jugo_full (output r1).
    find lotes_jugo where rowid(lotes_jugo) = r1 no-error.
    if available lotes_jugo then
        do:
            if lotes_jugo.id_articulo = 53 then run habilitar_boton. else run deshabilitar_boton.
        end.

/*MESSAGE "CAMBIO FILA" lotes_jugo.id_articulo "yyyy".*/
/*********************************************************************/
  run habilita-relacion-viewer-pagina.
  {custom/method/ctitulo.i}
  view frame {&FRAME-NAME}.
  run select-page(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available F-Frame-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE muestra-objetos F-Frame-Win 
PROCEDURE muestra-objetos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if deshabilita then
    do:
        run dispatch in h_b_origen_tambores_lote ('enable':U).
        b-detalle:sensitive in frame {&FRAME-NAME} = true.
        boton1:sensitive in frame {&FRAME-NAME} = true.
        deshabilita = false.
        button-5:sensitive in frame {&FRAME-NAME} = true.
        button-6:sensitive in frame {&FRAME-NAME} = true.
        button-3:sensitive in frame {&FRAME-NAME} = true.
        
    end.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oculta-objetos F-Frame-Win 
PROCEDURE oculta-objetos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run dispatch in h_b_origen_tambores_lote ('disable':U).
b-detalle:sensitive in frame {&FRAME-NAME} = false.
boton1:sensitive in frame {&FRAME-NAME} = false.
button-5:sensitive in frame {&FRAME-NAME} = false.
button-6:sensitive in frame {&FRAME-NAME} = false.
button-3:sensitive in frame {&FRAME-NAME} = false.

deshabilita = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy F-Frame-Win 
PROCEDURE post-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
{custom/support/casecd.i "post-copy-cabecera(input h)" 
                         "post-copy-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy-cabecera F-Frame-Win 
PROCEDURE post-copy-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.

alta-cabecera = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy-detalle F-Frame-Win 
PROCEDURE post-copy-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.

alta-detalle = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create F-Frame-Win 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

run muestra-objetos.

{custom/support/casecd.i "post-create-cabecera(input h)" 
                         "post-create-detalle(input h)"}




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create-cabecera F-Frame-Win 
PROCEDURE post-create-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter  h as handle no-undo.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create-detalle F-Frame-Win 
PROCEDURE post-create-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter  h as handle no-undo.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete F-Frame-Win 
PROCEDURE post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
{custom/support/casecd.i "post-delete-cabecera(input h)" 
                         "post-delete-detalle(input h)"}




run get-attribute ('current-page').

/* message "Entro al postdelete con valor " return-value view-as alert-box. */
  if return-value = "2" then
  do:
        /*message "Estaria bien" view-as alert-box.*/
  end.

  run get-attribute ('current-page').
  if return-value = "1" then
  do:

/*
/*    message "OJO que estoy por borrar todas las tablas relacionadas!!!!!!" view-as alert-box.*/
/*******************************************************************************/
/********************BORRO TODAS LAS TABLAS ASOCIADAS***************************/
for each inspecciones_lote where inspecciones_lote.id_empresa = del_empresa and
                             inspecciones_lote.id_sucursal = del_sucursal and
                             inspecciones_lote.id_lote = del_lote.

    delete inspecciones_lote.
end.                                 

for each sobrante where sobrante.id_empresa = del_empresa and
                    sobrante.id_sucursal = del_sucursal and
                    sobrante.id_lote = del_lote.

    delete sobrante.
end.
for each tambores_sobrante where tambores_aobrante.id_empresa = del_empresa and
                                 tambores_aobrante.id_sucursal = del_sucursal and
                                 tambores_aobrante.id_lote = del_lote.
    delete tambores_sobrante.
end.                                 


for each jugo_clarificado. where jugo_clarificado.id_empresa = del_empresa and
                                 jugo_clarificado.id_sucursal = del_sucursal and
                                 jugo_clarificado.id_lote = del_lote.
      delete jugo_clarificado.
end.

for each composicion_lote where composicion_lote.id_empresa = del_empresa and
                                composicion_lote.id_sucursal = del_sucursal and
                                composicion_lote.id_lote = del_lote.
            delete composicion_lote.
end.

for each envasadores_fabrica where envasadores_fabrica.id_empresa = del_empresa and
                               envasadores_fabrica.id_sucursal = del_sucursal and
                               envasadores_fabrica.id_lote = del_lote.

    delete envasadores_fabrica.
end.

for each tambores_lote where tambores_lote.id_empresa = del_empresa and
                             tambores_lote.id_sucursal = del_sucursal and
                             tambores_lote.id_lote = del_lote.

    delete tambores_lote.
end.

for each arrastre_lote where arrastre_lote.id_empresa = del_empresa and
                             arrastre_lote.id_sucursal = del_sucursal and
                             arrastre_lote.id_lote = del_lote.
            delete arrastre_lote.
end.

for each tambores_arrastre_lote where tambores_arrastre_lote.id_empresa = del_empresa and
                             tambores_arrastre_lote.id_sucursal = del_sucursal and
                             tambores_arrastre_lote.id_lote = del_lote.

    delete tambores_arrastre_lote.
end.

/***********************************************************************************/
*/
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete-cabecera F-Frame-Win 
PROCEDURE post-delete-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

/*for each inspecciones_lote where inspecciones_lote.id_empresa = del_empresa and
                             inspecciones_lote.id_sucursal = del_sucursal and
                             inspecciones_lote.id_lote = del_lote.

    delete inspecciones_lote.
end.                                 

for each sobrante where sobrante.id_empresa = del_empresa and
                    sobrante.id_sucursal = del_sucursal and
                    sobrante.id_lote = del_lote.

    delete sobrante.
end.


for each envasadores_fabrica where envasadores_fabrica.id_empresa = del_empresa and
                               envasadores_fabrica.id_sucursal = del_sucursal and
                               envasadores_fabrica.id_lote = del_lote.

    delete envasadores_fabrica.
end.


message "holaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" view-as alert-box.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete-detalle F-Frame-Win 
PROCEDURE post-delete-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle.

flag-detalle = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update F-Frame-Win 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

{custom/support/casecd.i "post-update-cabecera(input r , input h)" 
                         "post-update-detalle(input r , input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update-cabecera F-Frame-Win 
PROCEDURE post-update-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.

if alta-cabecera then
    run enable-folder-page in hfolder ( input 2).
alta-cabecera = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update-detalle F-Frame-Win 
PROCEDURE post-update-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.

flag-detalle = true.
alta-detalle = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy F-Frame-Win 
PROCEDURE pre-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
{custom/support/casecd.i "pre-copy-cabecera(input h)" 
                         "pre-copy-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy-cabecera F-Frame-Win 
PROCEDURE pre-copy-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta-cabecera = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy-detalle F-Frame-Win 
PROCEDURE pre-copy-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta-detalle = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create F-Frame-Win 
PROCEDURE pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
{custom/support/casecd.i "pre-create-cabecera(input h)" 
                         "pre-create-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create-cabecera F-Frame-Win 
PROCEDURE pre-create-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

run disable-folder-page in hfolder (input 2).
alta-cabecera = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create-detalle F-Frame-Win 
PROCEDURE pre-create-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta-detalle = true.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete F-Frame-Win 
PROCEDURE pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */
{custom/support/casecd.i "pre-delete-cabecera(input r , input h)" 
                         "pre-delete-detalle(input r , input h)"}





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete-cabecera F-Frame-Win 
PROCEDURE pre-delete-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
define var r as rowid.
message "pre delete cabecera".
return "AdM-ERROR".
/******************************************************************************/
/*************************ALMACENO LOS ID DEL REGISTRO A BORRAR****************/
run get-rowid1 in h_b_lotes_jugo (output r).
for each lotes_jugo where rowid(lotes_jugo) = r.

        del_empresa = lotes_jugo.id_empresa.
        del_sucursal = lotes_jugo.id_sucursal.
        del_lote = lotes_jugo.id_lote.
    
end.

/************ VERIFICO ANTES DE BORRAR SI TIENE SOBRANTES USADOS **************/
find sobrante where sobrante.id_empresa = del_empresa and
                    sobrante.id_sucursal = del_sucursal and
                    sobrante.id_lote = del_lote .
if available sobrante then
    do:
        find first envasado_lote where envasado_lote.id_empresa_origen = sobrante.id_empresa and
                                 envasado_lote.id_sucursal_origen = sobrante.id_sucursal and
                                 envasado_lote.id_origen = sobrante.id_lote.

        if available envasado_lote then 
            do:
                message "No puede borrar el lote " del_lote " porque el sobrante del mismo esta siendo utilizado por el lote" envasado_lote.id_lote view-as alert-box.
                return "ADM-ERROR".            
            end. 
    
    end.
*/

/*define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.
run get-rowid1 in h_b_lotes_jugo (output r).
for each lotes_jugo where rowid(lotes_jugo) = r.
    del_empresa = lotes_jugo.id_empresa.
    del_sucursal = lotes_jugo.id_sucursal.
    del_lote = lotes_jugo.id_lote.
end.
message "holaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" view-as alert-box.

message del_empresa del_sucursal del_lote view-as alert-box.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete-detalle F-Frame-Win 
PROCEDURE pre-delete-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update F-Frame-Win 
PROCEDURE pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

{custom/support/casecd.i "pre-update-cabecera(input r , input h)" 
                         "pre-update-detalle(input r , input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update-cabecera F-Frame-Win 
PROCEDURE pre-update-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update-detalle F-Frame-Win 
PROCEDURE pre-update-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proceso-cabecera-detalle F-Frame-Win 
PROCEDURE proceso-cabecera-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run proceso-cabecera-detalle-interno.
flag-detalle = no.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proceso-cabecera-detalle-interno F-Frame-Win 
PROCEDURE proceso-cabecera-detalle-interno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(TABLA-CABECERA) <> 0 &THEN
{custom/support/deldetalle1.i &cabecera={&TABLA-CABECERA}
             &detalle={&TABLA-DETALLE}
             &items={&TABLA-ITEMS}
             &bloque-borrado=}
&ENDIF             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetea-registro F-Frame-Win 
PROCEDURE resetea-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

run muestra-objetos.

if alta-cabecera then
   do: 
    run enable-folder-page in hfolder ( input 2).    
   end. 
alta-cabecera = false.
alta-detalle = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartFrame, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cambio-detalle F-Frame-Win 
FUNCTION cambio-detalle RETURNS LOGICAL
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  if flag-detalle then
    return true.

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

