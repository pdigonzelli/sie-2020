&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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

/* Local Variable Definitions ---                                       */
{custom/support/cabmcdvar.i}










/**********EMPIEZA-TEMP-TABLES*********/
&SCOPED-DEFINE TABLA-CABECERA lotes_aceite
&SCOPED-DEFINE TABLA-DETALLE composicion_lote_aceite
{custom/support/temp-tables1.i &detalle={&TABLA-DETALLE}}
&IF DEFINED(TABLA-CABECERA) <> 0 &THEN
define buffer aux-cabecera for {&TABLA-CABECERA}.
&ENDIF
/**********TERMINA-TEMP-TABLES*********/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS b-detalle FILL-IN-5 date_manufacture ~
BUTTON-10 BUTTON-34 FILL-IN-2 BUTTON-12 BUTTON-31 desde hasta BUTTON-11 ~
BUTTON-6 BUTTON-5 BUTTON-3 BUTTON-30 BUTTON-32 BUTTON-33 RECT-25 RECT-26 ~
RECT-4 RECT-5 RECT-6 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-3 FILL-IN-5 date_manufacture ~
FILL-IN-2 FILL-IN-1 desde hasta FILL-IN-4 FILL-IN-9 perdida 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cambio-detalle W-Win 
FUNCTION cambio-detalle RETURNS LOGICAL
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_lotes AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_new_composicion_lote_aceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_new_lotes_aceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_new_origen_tambores_lote_ace AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_new_sobrante_lote_aceite AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_lotes_ind AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_new_lotes_aceite AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-detalle 
     LABEL "&Detalle" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-10 
     LABEL "Imprimir Etiquetas" 
     SIZE 35 BY 1.14.

DEFINE BUTTON BUTTON-11 
     LABEL "Impresi�n de Etiquetas" 
     SIZE 27 BY 1.14.

DEFINE BUTTON BUTTON-12 
     LABEL "Imprimir" 
     SIZE 10 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Producci�n" 
     SIZE 13 BY 1.43.

DEFINE BUTTON BUTTON-30 
     LABEL "Impresion" 
     SIZE 11 BY 1.43.

DEFINE BUTTON BUTTON-31 
     LABEL "" 
     SIZE 4 BY 1.14.

DEFINE BUTTON BUTTON-32 
     LABEL "Pot - Terpeno" 
     SIZE 16 BY 1.43.

DEFINE BUTTON BUTTON-33 
     LABEL "Lotes Aceite" 
     SIZE 14 BY 1.43.

DEFINE BUTTON BUTTON-34 
     LABEL "Corregir Tara" 
     SIZE 35 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "Sobrante y Borra" 
     SIZE 18 BY 1.43.

DEFINE BUTTON BUTTON-6 
     LABEL "Quitar Tambor" 
     SIZE 16 BY 1.43.

DEFINE VARIABLE date_manufacture AS DATE FORMAT "99/99/99":U 
     LABEL "Date of Manufacture" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE desde AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Etapas en la Produccion de los Lotes de Aceite" 
     VIEW-AS FILL-IN 
     SIZE 88 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Exportaci�n No Coca Cola" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .86
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "FORMACION DE LOS TAMBORES DEL LOTE" 
     VIEW-AS FILL-IN 
     SIZE 106 BY .86
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "SOBRANTE DE LOTE" 
     VIEW-AS FILL-IN 
     SIZE 143 BY .86
     BGCOLOR 6 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Exportaci�n Coca Cola" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .86
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS DECIMAL FORMAT "->>>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE hasta AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE perdida AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 9 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 151 BY 10.24.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 7.86.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 2.38.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61.6 BY 5.24.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 153 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     b-detalle AT ROW 1.48 COL 111
     FILL-IN-3 AT ROW 3.62 COL 2 NO-LABEL
     FILL-IN-5 AT ROW 4.1 COL 113 COLON-ALIGNED NO-LABEL
     date_manufacture AT ROW 5.05 COL 134 COLON-ALIGNED
     BUTTON-10 AT ROW 6.24 COL 115
     BUTTON-34 AT ROW 7.43 COL 115
     FILL-IN-2 AT ROW 8.95 COL 113 COLON-ALIGNED NO-LABEL
     FILL-IN-1 AT ROW 9.33 COL 62 COLON-ALIGNED NO-LABEL
     BUTTON-12 AT ROW 10.29 COL 138
     BUTTON-31 AT ROW 10.29 COL 148
     desde AT ROW 10.38 COL 116 COLON-ALIGNED
     hasta AT ROW 10.38 COL 129 COLON-ALIGNED
     FILL-IN-4 AT ROW 12.67 COL 4 NO-LABEL
     BUTTON-11 AT ROW 13.86 COL 42
     BUTTON-6 AT ROW 16.71 COL 3
     BUTTON-5 AT ROW 16.71 COL 19
     BUTTON-3 AT ROW 16.71 COL 37
     BUTTON-30 AT ROW 16.71 COL 50
     BUTTON-32 AT ROW 16.71 COL 61
     BUTTON-33 AT ROW 16.71 COL 77
     FILL-IN-9 AT ROW 16.95 COL 104 COLON-ALIGNED NO-LABEL
     perdida AT ROW 16.95 COL 134 COLON-ALIGNED NO-LABEL
     RECT-25 AT ROW 12.19 COL 2
     RECT-26 AT ROW 3.62 COL 109
     RECT-4 AT ROW 1 COL 1
     RECT-5 AT ROW 3.62 COL 92
     RECT-6 AT ROW 8.86 COL 1
     "Perdida:" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 16.95 COL 128
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 153 BY 23.43.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Lotes de Aceite"
         HEIGHT             = 23.43
         WIDTH              = 153
         MAX-HEIGHT         = 24.33
         MAX-WIDTH          = 156.2
         VIRTUAL-HEIGHT     = 24.33
         VIRTUAL-WIDTH      = 156.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "visibles" W-Win _INLINE
/* Actions: ? custom/support/cusvis.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" W-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "tablas" W-Win _INLINE
/* Actions: ? custom/support/cabecera1.p ? ? ? */
/*lotes_aceite,composicion_lote_aceite*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN perdida IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Lotes de Aceite */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Lotes de Aceite */
DO:
   /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  run get-attribute ('current-page').
  if return-value <> "1" then do:
    message "No puede abandonar desde esta p�gina".
    run select-page(1).
  end.
  else
  do: 
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY. 
  end.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-detalle W-Win
ON CHOOSE OF b-detalle IN FRAME F-Main /* Detalle */
DO:
  case  b-detalle:label:
   when "&Detalle" Then
    do:
      b-detalle:label    = "&Principal".
      rect-5:hidden      = true.
      rect-6:hidden      = true. 
      rect-25:hidden      = false.
      rect-26:hidden      = false. 
      button-3:hidden    = true.
      button-6:hidden    = true.
      button-5:hidden    = true.
      button-10:hidden    = false.
      button-12:hidden    = false.
      fill-in-1:hidden   = true.
      fill-in-2:hidden   = false.
      fill-in-3:hidden   = false.
      fill-in-4:hidden   = false.
      fill-in-5:hidden   = false.
      fill-in-9:hidden   = true.
      date_manufacture:hidden   = false.
      desde:hidden   = false.
      hasta:hidden   = false.
      
      /*verificar existencia de registro de composicion_lote - by facundo 22/11/2004*/
      DEFINE VAR r AS ROWID.
  
      RUN get-rowid1 IN h_b_new_lotes_aceite (OUTPUT r).
      FIND FIRST lotes_aceite WHERE ROWID(lotes_aceite) = r NO-LOCK NO-ERROR.
      IF AVAILABLE lotes_aceite THEN
        RUN pLotesAceiteSinComposicion.p (lotes_aceite.nromov).
      
      run select-page(2).
    end.
   otherwise 
     do:
      b-detalle:label    = "&Detalle".
      rect-5:hidden      = false.
      rect-6:hidden      = false.
      rect-25:hidden      = true.
      rect-26:hidden      = true. 
      button-3:hidden    = false.
      button-6:hidden    = false.
      button-5:hidden    = false.
      button-10:hidden    = true.
      button-12:hidden    = true.
      fill-in-1:hidden   = false.
      fill-in-2:hidden   = true.
      fill-in-3:hidden   = true.
      fill-in-4:hidden   = true.
      fill-in-5:hidden   = true.
      fill-in-9:hidden   = false.
      date_manufacture:hidden   = true.
      desde:hidden   = true.
      hasta:hidden   = true.
      run select-page(1).         
     end.  
   end. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 W-Win
ON CHOOSE OF BUTTON-10 IN FRAME F-Main /* Imprimir Etiquetas */
DO:
  define var anio as char.
  define var r as rowid.
  
  
  run get-rowid1 in h_b_new_lotes_aceite (output r).
  find lotes_aceite where rowid(lotes_aceite) = r no-lock no-error.
  if available lotes_aceite then
    do:
        run zebra_lotes_aceite_coca.p (input r, input date(date_manufacture:screen-value in frame {&FRAME-NAME})).
        RUN mailing.
    end.
  else message "Existe un error. Debe ingresar los tambores producidos.".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 W-Win
ON CHOOSE OF BUTTON-11 IN FRAME F-Main /* Impresi�n de Etiquetas */
DO:

  define var hcontainer as handle.
  define var r as rowid.
  define var entro as logical initial false.
  
  RUN get-rowid1 in h_b_new_sobrante_lote_aceite (output r).
  run zebra_lotes_aceite_sobrante.p (input r).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 W-Win
ON CHOOSE OF BUTTON-12 IN FRAME F-Main /* Imprimir */
DO:
  define var anio as char.
  define var r as rowid.
  define var a1 as integer.
  define var a2 as integer.
  a1 = integer(desde:screen-value in frame F-Main).
  a2 = integer(hasta:screen-value in frame F-Main).
  RUN get-rowid1 in h_b_new_lotes_aceite (output r).
  run zebra_lotes_aceite_no_coca.p (input r, input a1, input a2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Producci�n */
DO:
  define var r as rowid.
    
  run get-rowid1 in h_b_new_lotes_aceite (output r).
  RUN wc_origen_tambores_prod_aceite_opt.w (INPUT r).
  /* run wc_origen_tambores_produccion_aceite.w (input r). */
  run dispatch in h_b_new_origen_tambores_lote_ace ('open-query':U).
  run actualiza-kilos.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-30 W-Win
ON CHOOSE OF BUTTON-30 IN FRAME F-Main /* Impresion */
DO:
  define var r as rowid.
  define var v_query as char.
  
  run get-rowid1 in h_b_new_lotes_aceite (output r).
  find first lotes_aceite where rowid(lotes_aceite) = r no-lock no-error.
  if available lotes_aceite then do:
        v_query = " tambores_industria.id_empresa_destino = " + string(lotes_aceite.id_empresa) +
                  " and tambores_industria.id_sucursal_destino = " + string(lotes_aceite.id_sucursal) +
                  " and tambores_industria.id_tipotambor_destino = " + string(lotes_aceite.id_tipotambor) +
                  " and tambores_industria.nromov_destino = " + string(lotes_aceite.nromov).
       
       
                 
       run p_reportes.p (input "tam_origen_de_lotes",
                         input "Origen de aceite de los Lotes",
                         input v_query,
                         input "").             
  end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-31
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-31 W-Win
ON CHOOSE OF BUTTON-31 IN FRAME F-Main
DO:
    define var anio as char.
  define var r as rowid.
  define var a1 as integer.
  define var a2 as integer.
  a1 = integer(desde:screen-value in frame F-Main).
  a2 = integer(hasta:screen-value in frame F-Main).
  RUN get-rowid1 in h_b_new_lotes_aceite (output r).
  
  run w_sel_suc_impresion.w (input r /* ROWID LOTE ACEITE */,
                             input a1 /* DESDE LOTE */, 
                             input a2 /* HASTA TAMBOR */ ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-32 W-Win
ON CHOOSE OF BUTTON-32 IN FRAME F-Main /* Pot - Terpeno */
DO:
  define var r as rowid.
    
  run get-rowid1 in h_b_new_lotes_aceite (output r).
  run wc_origen_tambores_pot_terpeno.w (input r).
  run dispatch in h_b_new_origen_tambores_lote_ace ('open-query':U).
  run actualiza-kilos.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-33 W-Win
ON CHOOSE OF BUTTON-33 IN FRAME F-Main /* Lotes Aceite */
DO:
  define var r as rowid.
    
  run get-rowid1 in h_b_new_lotes_aceite (output r).
  run wc_origen_tambores_lotes_aceite.w (input r).
  run dispatch in h_b_new_origen_tambores_lote_ace ('open-query':U).
  run actualiza-kilos.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-34
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-34 W-Win
ON CHOOSE OF BUTTON-34 IN FRAME F-Main /* Corregir Tara */
DO:
  define var r as rowid.
  
  RUN get-rowid1 in h_b_new_composicion_lote_aceite (output r).
  run wTaraTambores.w (input r).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Sobrante y Borra */
DO:
  define var r as rowid.
  run get-rowid1 in h_b_new_lotes_aceite (output r).
  run wc_origen_tambores_sobrante_lote_aceite.w (input r).
  run dispatch in h_b_new_origen_tambores_lote_ace ('open-query':U).
  run actualiza-kilos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 W-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Quitar Tambor */
DO:

run borra-row in h_b_new_origen_tambores_lote_ace.
run dispatch in h_b_new_origen_tambores_lote_ace ('open-query':U).
run actualiza-kilos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/method/winitialize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualiza-kilos W-Win 
PROCEDURE actualiza-kilos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 define var v_kilos_origen as decimal.
 DEFINE VAR v_kilos_lotes AS DECIMAL.
 DEFINE VAR r AS ROWID.

 run kilos_total in h_b_new_origen_tambores_lote_ace (output v_kilos_origen).
  
 fill-in-9:screen-value in frame F-Main = string(v_kilos_origen).

 RUN GET-rowid1 IN h_b_new_lotes_aceite (OUTPUT r).
 FIND FIRST lotes_aceite WHERE ROWID(lotes_aceite) = r NO-LOCK NO-ERROR.
 FOR EACH tambores_industria OF lotes_aceite NO-LOCK.
     v_kilos_lotes = v_kilos_lotes + tambores_industria.kilos_tambor.
 END.

 IF v_kilos_lotes > 0 THEN perdida:SCREEN-VALUE IN FRAME F-Main = STRING(v_kilos_origen - v_kilos_lotes).
 ELSE perdida:SCREEN-VALUE IN FRAME F-Main = "0".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
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
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 58.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 20.00 ) NO-ERROR.

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_lotes_ind.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_lotes_ind ).
       RUN set-position IN h_v_lotes_ind ( 3.81 , 93.20 ) NO-ERROR.
       /* Size in UIB:  ( 4.81 , 59.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_cus-updsav-3 ).
       RUN set-position IN h_cus-updsav-3 ( 1.24 , 2.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-3 ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico-2 ).
       RUN set-position IN h_cus-navico-2 ( 1.24 , 36.00 ) NO-ERROR.
       RUN set-size IN h_cus-navico-2 ( 1.91 , 23.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_lotes.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_lotes ).
       RUN set-position IN h_b_lotes ( 3.62 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b_lotes ( 5.00 , 88.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav ).
       RUN set-position IN h_cus-updsav ( 9.10 , 4.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_new_lotes_aceite.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_new_lotes_aceite ).
       RUN set-position IN h_b_new_lotes_aceite ( 11.24 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b_new_lotes_aceite ( 5.00 , 89.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_new_lotes_aceite.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_new_lotes_aceite ).
       RUN set-position IN h_v_new_lotes_aceite ( 11.48 , 91.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.81 , 62.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_new_origen_tambores_lote_aceite.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_new_origen_tambores_lote_ace ).
       RUN set-position IN h_b_new_origen_tambores_lote_ace ( 18.38 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b_new_origen_tambores_lote_ace ( 5.62 , 149.00 ) NO-ERROR.

       /* Links to SmartViewer h_v_lotes_ind. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes , 'Record':U , h_v_lotes_ind ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-3 , 'TableIO':U , h_v_lotes_ind ).

       /* Links to csmartbrowser h_b_lotes. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico-2 , 'Navigation':U , h_b_lotes ).

       /* Links to csmartbrowser h_b_new_lotes_aceite. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes , 'record':U , h_b_new_lotes_aceite ).

       /* Links to SmartViewer h_v_new_lotes_aceite. */
       RUN add-link IN adm-broker-hdl ( h_b_new_lotes_aceite , 'record':U , h_v_new_lotes_aceite ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_new_lotes_aceite ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav-2 ).
       RUN set-position IN h_cus-updsav-2 ( 1.24 , 4.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_new_composicion_lote_aceite.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_b_new_composicion_lote_aceite ).
       RUN set-position IN h_b_new_composicion_lote_aceite ( 4.57 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b_new_composicion_lote_aceite ( 6.71 , 105.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-4 ).
       RUN set-position IN h_cus-updsav-4 ( 13.62 , 5.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-4 ( 1.91 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_new_sobrante_lote_aceite.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_new_sobrante_lote_aceite ).
       RUN set-position IN h_b_new_sobrante_lote_aceite ( 15.52 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b_new_sobrante_lote_aceite ( 6.71 , 145.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to csmartbrowser h_b_new_composicion_lote_aceite. */
       RUN add-link IN adm-broker-hdl ( h_b_new_lotes_aceite , 'record':U , h_b_new_composicion_lote_aceite ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_b_new_composicion_lote_aceite ).

       /* Links to csmartbrowser h_b_new_sobrante_lote_aceite. */
       RUN add-link IN adm-broker-hdl ( h_b_new_lotes_aceite , 'record':U , h_b_new_sobrante_lote_aceite ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-4 , 'TableIO':U , h_b_new_sobrante_lote_aceite ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros W-Win 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE borra-items W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambio-fila W-Win 
PROCEDURE cambio-fila :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
define var r as rowid no-undo.

if valid-handle(h) and h = hcabecera  then
do:
    if cambio-detalle() then
        run proceso-cabecera-detalle.
    run genero-detalle.
end.    
&IF DEFINED (TABLA-CABECERA) <> 0 &THEN
    run get-rowid-cabecera(output r) no-error.
    find aux-cabecera where rowid(aux-cabecera) = r no-error.
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar-origen W-Win 
PROCEDURE cargar-origen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

run dispatch in h_b_new_origen_tambores_lote_ace ('open-query':U).
run actualiza-kilos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consulta W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita-viewer-paginas W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer W-Win 
PROCEDURE deshabilita_viewer :
define var lista as character no-undo.
define var i as integer no-undo.
define var h as handle no-undo.
define var lista_campos as character no-undo initial "id_sucursal,fi-sucursales-nombre,anio,id_lote,Fecha,citral,descripcion,id_articulo,fecha,citral,id_envase,Peso_neto,tanque,fi-envases_prod-descripcion,fi-productos_terminados-descrip".
    run get-link-handle in adm-broker-hdl (input this-procedure,"CONTAINER-TARGET",
                                       output lista).
    do i=1 to num-entries(lista):
        h=widget-handle(entry(i,lista)).
        run get-attribute in h ('TYPE').
        if return-value = "smartviewer" then 
            run deshabilita_campos in h (input lista_campos).
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid-cabecera W-Win 
PROCEDURE devuelve-rowid-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.

run get-rowid in hcabecera ( output r) no-error.
message "Estoy en devuelve-rowid-cabecera" view-as alert-box.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid-detalle W-Win 
PROCEDURE devuelve-rowid-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.

run get-rowid in hdetalle ( output r) no-error.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-botones W-Win 
PROCEDURE disable-botones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
button-3:sensitive in frame F-Main = false.
button-5:sensitive in frame F-Main = false.
button-6:sensitive in frame F-Main = false.
button-32:sensitive in frame F-Main = false.
button-33:sensitive in frame F-Main = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-botones W-Win 
PROCEDURE enable-botones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
button-3:sensitive in frame F-Main = true.
button-5:sensitive in frame F-Main = true.
button-6:sensitive in frame F-Main = true.
button-32:sensitive in frame F-Main = TRUE.
button-33:sensitive in frame F-Main = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-3 FILL-IN-5 date_manufacture FILL-IN-2 FILL-IN-1 desde hasta 
          FILL-IN-4 FILL-IN-9 perdida 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE b-detalle FILL-IN-5 date_manufacture BUTTON-10 BUTTON-34 FILL-IN-2 
         BUTTON-12 BUTTON-31 desde hasta BUTTON-11 BUTTON-6 BUTTON-5 BUTTON-3 
         BUTTON-30 BUTTON-32 BUTTON-33 RECT-25 RECT-26 RECT-4 RECT-5 RECT-6 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genero-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genero-detalle-interno W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-deshabilitados-paleta W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-cabecera W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-lotes_aceite W-Win 
PROCEDURE get-rowid-lotes_aceite :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter pr as rowid.
run get-rowid1 in h_b_new_lotes_aceite (output pr) no-error.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-lotes_ind W-Win 
PROCEDURE get-rowid-lotes_ind :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter pr as rowid.

run get-rowid1 in h_b_lotes (output pr).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-relacion-viewer-pagina W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresion W-Win 
PROCEDURE impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_reportes,output cresult).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 /* Code placed here will execute PRIOR to standard behavior. */
  
  run get-attribute ('current-page').
  if return-value = "2" and valid-handle(hdetalle) and pagina-origen = "1" then
  do:
        run check-modified in hcabecera (input "check").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
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
  run deshabilita-viewer-paginas.
  run habilita-relacion-viewer-pagina.
  {custom/method/ctitulo.i}
  {custom/support/cabmcd.i}
  run select-page(1).
  /*
  run ocultar-objetos-pag2. */
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailing W-Win 
PROCEDURE mailing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cUsuarios AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSubject  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBody     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cArt      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEnv      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCant     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE r         AS ROWID    NO-UNDO.
  
  /*mandar mail a deposito con datos del lote*/
  
  RUN get-rowid1 IN h_b_new_lotes_aceite (OUTPUT r).

  FIND FIRST lotes_aceite WHERE ROWID(lotes_aceite) = r NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_aceite THEN DO:
    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = lotes_aceite.id_articulo NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF lotes_aceite  NO-LOCK NO-ERROR.
    FOR EACH tambores_industria OF lotes_aceite NO-LOCK.
      iCant = iCant + 1.
    END.

    IF lotes_aceite.id_sucursal = 95 THEN 
      cUsuarios = "depositof@sa-sanmiguel.com,mmuroni@sa-sanmiguel.com".

    IF lotes_aceite.id_sucursal = 96 THEN 
      cUsuarios = "jchaile@sa-sanmiguel.com".

    cUsuarios = cUsuarios + ",rvelez@sa-sanmiguel.com,diegomf@sa-sanmiguel.com,depositoc@sa-sanmiguel.com,rocaran@sa-sanmiguel.com".

    cLot = STRING(lotes_aceite.id_lote) + "/" + STRING(lotes_aceite.anio).
    cArt = IF AVAILABLE productos_terminados THEN trim(productos_terminados.descripcion) ELSE "nada".
    cEnv = IF AVAILABLE envases_prod THEN trim(envases_prod.descripcion) ELSE "nada".

    cSubject  = "Aviso de creacion de lote para registro de salida de insumos del lote " + cLot.
    cBody     = "Lote:     " + cLot + CHR(10) + 
                "Tipo:     " + cArt + CHR(10) + 
                "Envase:   " + cEnv + CHR(10) + 
                "Cantidad: " + STRING(iCant) + " tambores".
                          
    RUN SendMail.p(INPUT "",           /* SIEMPRE TIENE QUE IR */
                   INPUT 2,            /* PRIORIDAD */
                   INPUT cSubject,     /* SUBJECT */
                   INPUT cBody,        /* BODY     */
                   INPUT cUsuarios,    /* DEST. SEP COMAS */
                   INPUT ""            /* ARCHIVOS ATTACHED SEP POR COMAS */).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ocultar-objetos-pag2 W-Win 
PROCEDURE ocultar-objetos-pag2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
rect-25:hidden in frame F-Main           = false.
rect-26:hidden in frame F-Main           = false.
button-10:hidden in frame F-Main         = false.
button-12:hidden in frame F-Main         = false.
button-31:hidden in frame F-Main         = false.
fill-in-2:hidden in frame F-Main         = false.
fill-in-3:hidden in frame F-Main     = false.
fill-in-4:hidden in frame F-Main        = false.
fill-in-5:hidden in frame F-Main      = false.
date_manufacture:hidden in frame F-Main  = false.
desde:hidden in frame F-Main             = false.
hasta:hidden in frame F-Main             = false.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy-cabecera W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create W-Win 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
{custom/support/casecd.i "post-create-cabecera(input h)" 
                         "post-create-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create-cabecera W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete W-Win 
PROCEDURE post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
{custom/support/casecd.i "post-delete-cabecera(input h)" 
                         "post-delete-detalle(input h)"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete-cabecera W-Win 
PROCEDURE post-delete-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update-cabecera W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy-cabecera W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create-cabecera W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete-cabecera W-Win 
PROCEDURE pre-delete-cabecera :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update-cabecera W-Win 
PROCEDURE pre-update-cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid no-undo.
define input parameter h as handle no-undo.

message "Entro a Pre-Update-Cabecera" view-as alert-box.
run pre-update in h_v_new_lotes_aceite.
/*
define var r1 as rowid.

RUN get-rowid-lotes_aceite (output r1).
find lotes_aceite where rowid(lotes_aceite) = r1 no-lock no-error.

if available lotes_aceite then
    do:
        if not lotes_aceite.activo then do:
            message "No se puede modificar un lote viejo" view-as alert-box.
            /* return "ADM-ERROR". */
            RUN notify ('reset-record':U).
            RUN notify ('cancel-record':U).
        end.
    end.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proceso-cabecera-detalle W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proceso-cabecera-detalle-interno W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetea-registro W-Win 
PROCEDURE resetea-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
if alta-cabecera then
    run enable-folder-page in hfolder ( input 2).
alta-cabecera = false.
alta-detalle = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cambio-detalle W-Win 
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
