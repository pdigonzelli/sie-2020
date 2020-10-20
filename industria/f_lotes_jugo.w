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
define var del_tipotambor as integer.
define var del_nromov as integer.
define var del_fecha as date.
define var del_articulo as integer.
define var del_calidad as integer.

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
&Scoped-Define ENABLED-OBJECTS b-detalle FILL-IN-1 BUTTON-8 BUTTON-29 ~
BUTTON-28 BUTTON-33 BUTTON-43 BUTTON-31 ar BUTTON-32 BUTTON-1 BUTTON-9 ~
BUTTON-30 radTipoTambor BUTTON-7 BUTTON-42 BUTTON-5 BUTTON-6 BUTTON-3 ~
BUTTON-12 BUTTON-13 REC1 REC3 RECT-17 RECT-18 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS FILL-2 FILL-IN-1 ar FILL1 radTipoTambor 

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
DEFINE VARIABLE h_b_arrastre_lotes_jugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_composicion_lote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_inspecciones_lote1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_lotes_jugo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_origen_tambores_lote AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b_sobrante AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-updsav-6 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_lotes_jugo AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-detalle 
     LABEL "&Detalle" 
     SIZE 22 BY 1.05.

DEFINE BUTTON Boton1 
     IMAGE-UP FILE "src/adm2/image/etiqueta.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Caracter�sticas Jugo Clarificado" 
     SIZE 5.2 BY 1.19 TOOLTIP "Caracteristicas del Jugo Claro".

DEFINE BUTTON BUTTON-1  NO-CONVERT-3D-COLORS
     LABEL "Impresi�n de Etiquetas" 
     SIZE 24 BY 1.24.

DEFINE BUTTON BUTTON-12 
     LABEL "Lotes P/MP" 
     SIZE 14 BY 1.43.

DEFINE BUTTON BUTTON-13 
     LABEL "P.Terceros" 
     SIZE 14 BY 1.43.

DEFINE BUTTON BUTTON-28 
     LABEL "en Libras" 
     SIZE 10 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-29 
     LABEL "Rango" 
     SIZE 8 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-3 
     LABEL "Producci�n" 
     SIZE 13 BY 1.43.

DEFINE BUTTON BUTTON-30 
     IMAGE-UP FILE "src/adm2/image/004.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Impresion" 
     SIZE 5.2 BY 1.19 TOOLTIP "Impresion".

DEFINE BUTTON BUTTON-31 
     LABEL "Rgo/Adit" 
     SIZE 8 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-32 
     LABEL "Sin Nombre" 
     SIZE 13 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-33 
     LABEL "PepsiChile" 
     SIZE 11 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-42 
     IMAGE-UP FILE "src/adm2/image/buscar.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 42" 
     SIZE 5.2 BY 1.19 TOOLTIP "Buscar Tambores".

DEFINE BUTTON BUTTON-43 
     LABEL "Com. c/ad" 
     SIZE 11 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-5 
     LABEL "Sobrante y Arrastre" 
     SIZE 20 BY 1.43.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "src/adm2/image/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Quitar Tambor" 
     SIZE 5.2 BY 1.19 TOOLTIP "Quitar Tambor".

DEFINE BUTTON BUTTON-7 
     LABEL "Impresi�n de Etiquetas" 
     SIZE 24 BY 1.24.

DEFINE BUTTON BUTTON-8 
     LABEL "Com s/ad" 
     SIZE 11 BY 1.05
     BGCOLOR 1 FGCOLOR 15 .

DEFINE BUTTON BUTTON-9 
     LABEL "Impresi�n de Etiquetas" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE ar AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-2 AS CHARACTER FORMAT "X(256)":U INITIAL "INSPECCIONES - ANALISIS PARCIALES" 
     VIEW-AS FILL-IN 
     SIZE 125 BY 1.05
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Impresi�n de Etiquetas" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .95
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL1 AS CHARACTER FORMAT "X(256)":U INITIAL "COMPOSICION DEL LOTE - ORIGENES DE JUGO" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.05
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE radTipoTambor AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Produccion", 1,
"Sobrante", 4,
"Arrastre", 5,
"Lotes P/MP", 77,
"P. Terceros", 9
     SIZE 91 BY .86 NO-UNDO.

DEFINE RECTANGLE REC1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 152 BY 9.05.

DEFINE RECTANGLE REC3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 151 BY 8.38.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 152 BY 1.43.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 1 BY 7.43.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 2.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     b-detalle AT ROW 1.24 COL 109
     FILL-2 AT ROW 3.38 COL 2 NO-LABEL
     FILL-IN-1 AT ROW 3.38 COL 126 COLON-ALIGNED NO-LABEL
     BUTTON-8 AT ROW 4.57 COL 129.2
     BUTTON-29 AT ROW 4.57 COL 142.2
     BUTTON-28 AT ROW 5.76 COL 129.2
     BUTTON-33 AT ROW 5.76 COL 139.2
     BUTTON-43 AT ROW 6.95 COL 129
     BUTTON-31 AT ROW 6.95 COL 142.2
     ar AT ROW 8.14 COL 136.2 COLON-ALIGNED NO-LABEL
     BUTTON-32 AT ROW 9.33 COL 129.2
     BUTTON-1 AT ROW 11.24 COL 48
     BUTTON-9 AT ROW 11.67 COL 126
     Boton1 AT ROW 19.29 COL 145.6
     FILL1 AT ROW 19.33 COL 2 NO-LABEL
     BUTTON-30 AT ROW 19.29 COL 140.2
     radTipoTambor AT ROW 19.43 COL 37 NO-LABEL
     BUTTON-7 AT ROW 23.38 COL 6
     BUTTON-42 AT ROW 19.29 COL 128.4
     BUTTON-5 AT ROW 26.24 COL 89
     BUTTON-6 AT ROW 19.29 COL 134.4
     BUTTON-3 AT ROW 26.24 COL 109
     BUTTON-12 AT ROW 26.24 COL 123
     BUTTON-13 AT ROW 26.24 COL 138
     REC1 AT ROW 10.05 COL 1
     REC3 AT ROW 10.52 COL 1
     RECT-17 AT ROW 19.19 COL 1
     RECT-18 AT ROW 11 COL 73
     RECT-4 AT ROW 1.19 COL 1.4
     "TAMBORES" VIEW-AS TEXT
          SIZE 43 BY .95 AT ROW 10.76 COL 3
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "ARRASTRE" VIEW-AS TEXT
          SIZE 44 BY .86 AT ROW 10.76 COL 76
          BGCOLOR 1 FGCOLOR 15 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152.2 BY 26.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 2
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
         HEIGHT             = 26.91
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
/* SETTINGS FOR BUTTON Boton1 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Boton1:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       BUTTON-32:HIDDEN IN FRAME F-Main           = TRUE.

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
      /*rec4:hidden = false.*/
      button-3:hidden = true.
      button-6:hidden = true.
      fill1:SCREEN-VALUE IN FRAME F-Main = "SOBRANTE".
      button-5:hidden = true.
      button-7:hidden = false.
      rec1:hidden = true.
      rect-17:hidden = true.
      button-28:hidden = FALSE.
      run select-page(2).
/*      run automatico in h_b_envasadores_fabrica1.*/
    end.
   otherwise 
     do:
      b-detalle:label = "&Detalle".
      rec3:hidden = true.
      /*rec4:hidden = true.*/
      rec1:hidden = false.
      button-3:hidden = false.
      button-7:hidden = true.
      button-6:hidden = false.
      fill1:SCREEN-VALUE IN FRAME F-Main = "COMPOSICION LOTE".
      button-5:hidden = false.
      rect-17:hidden = false.
      button-28:hidden = TRUE.
      run select-page(1).
         
     end.  
   end. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Boton1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Boton1 F-Frame-Win
ON CHOOSE OF Boton1 IN FRAME F-Main /* Caracter�sticas Jugo Clarificado */
DO:
define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo (output r).
  run w_datos_jugo_clarificado.w (input r).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 F-Frame-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Impresi�n de Etiquetas */
DO:
  define var anio as char.
  define var r as rowid.

  DEFINE VARIABLE cUsuarios AS CHARACTER  NO-UNDO.

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


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 F-Frame-Win
ON CHOOSE OF BUTTON-12 IN FRAME F-Main /* Lotes P/MP */
DO:
  define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo (output r).
  run wc_origen_tambores_materia_prima_jugo.w (input r).
  run dispatch in h_b_origen_tambores_lote ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 F-Frame-Win
ON CHOOSE OF BUTTON-13 IN FRAME F-Main /* P.Terceros */
DO:
  define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo (output r).
  run wc_origen_tambores_prod_terceros_jugo.w (input r).
  run dispatch in h_b_origen_tambores_lote ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-28 F-Frame-Win
ON CHOOSE OF BUTTON-28 IN FRAME F-Main /* en Libras */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo (output r).

run zebra_inspecciones_lote_libra.p (input r). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-29 F-Frame-Win
ON CHOOSE OF BUTTON-29 IN FRAME F-Main /* Rango */
DO:
define var hcontainer as handle.
define var r as rowid.
define var v1 as integer.
define var v2 as integer.
DEFINE VAR v_suc AS INTEGER.
RUN get-rowid1 in h_b_lotes_jugo (output r).

run w_rango_etiquetas.w (output v1,
                         output v2,
                         OUTPUT v_suc).
                         
run zebra_inspecciones_lote_rango.p (input r, input v1, input v2, INPUT v_suc). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 F-Frame-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Producci�n */
DO:
  define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo (output r).
  run wc_origen_tambores_produccion_jugo.w (input r).
  run dispatch in h_b_origen_tambores_lote ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-30 F-Frame-Win
ON CHOOSE OF BUTTON-30 IN FRAME F-Main /* Impresion */
DO:
  define var r as rowid.
  define var v_query as char.
  
  run get-rowid1 in h_b_lotes_jugo (output r).
  find first lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
  if available lotes_jugo then do:
        v_query = " tambores_industria.id_empresa_destino = " + string(lotes_jugo.id_empresa) +
                  " and tambores_industria.id_sucursal_destino = " + string(lotes_jugo.id_sucursal) +
                  " and tambores_industria.id_tipotambor_destino = " + string(lotes_jugo.id_tipotambor) +
                  " and tambores_industria.nromov_destino = " + string(lotes_jugo.nromov).
       
       
                 
       run p_reportes.p (input "tam_origen_de_lotes",
                         input "Origen de jugo de los Lotes",
                         input v_query,
                         input "").             
  end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-31
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-31 F-Frame-Win
ON CHOOSE OF BUTTON-31 IN FRAME F-Main /* Rgo/Adit */
DO:
define var hcontainer as handle.
define var p_wn as char.
define var r as rowid.
define var v1 as integer.
define var v2 as integer.
DEFINE VAR v_suc AS INTEGER.


p_wn = ar:screen-value in frame F-Main.

RUN get-rowid1 in h_b_lotes_jugo (output r).

run w_rango_etiquetas.w (output v1,
                         output v2,
                         OUTPUT v_suc).
                         
run zebra_inspecciones_lote_rango_aditivos.p (input r, input v1, input v2, INPUT v_suc).  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-32 F-Frame-Win
ON CHOOSE OF BUTTON-32 IN FRAME F-Main /* Sin Nombre */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo (output r).

run zebra_inspecciones_lote_sin_nombre.p (input r). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-33 F-Frame-Win
ON CHOOSE OF BUTTON-33 IN FRAME F-Main /* PepsiChile */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo (output r).

run zebra_inspecciones_lote_pepsi_chile.p (input r). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-42
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-42 F-Frame-Win
ON CHOOSE OF BUTTON-42 IN FRAME F-Main /* Button 42 */
DO:
  DEFINE VARIABLE r     AS ROWID      NO-UNDO.
  DEFINE VARIABLE iTipo AS INTEGER    NO-UNDO.
  
  RUN get-rowid1 in h_b_lotes_jugo (output r).
  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
   iTipo = INTEGER(radTipoTambor:SCREEN-VALUE).
  
    RUN wqOrigenTambores.w (lotes_jugo.id_sucursal, iTipo, r, "jugo").
    RUN dispatch IN h_b_origen_tambores_lote ('open-query':U).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-43
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-43 F-Frame-Win
ON CHOOSE OF BUTTON-43 IN FRAME F-Main /* Com. c/ad */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo (output r).

run zebra_inspecciones_lote_con_aditivos.p (input r). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 F-Frame-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Sobrante y Arrastre */
DO:
    define var r as rowid.
  run get-rowid1 in h_b_lotes_jugo (output r).
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
/* run dispatch in h_b_origen_tambores_lote ('open-query':U). */
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 F-Frame-Win
ON CHOOSE OF BUTTON-7 IN FRAME F-Main /* Impresi�n de Etiquetas */
DO:
define var hcontainer as handle.
define var r as rowid.
define var entro as logical initial false.

RUN get-rowid1 in h_b_lotes_jugo (output r).
run zebra_lotes_jugo_sobrante.p (input r).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 F-Frame-Win
ON CHOOSE OF BUTTON-8 IN FRAME F-Main /* Com s/ad */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo (output r).

run zebra_inspecciones_lote.p (input r). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 F-Frame-Win
ON CHOOSE OF BUTTON-9 IN FRAME F-Main /* Impresi�n de Etiquetas */
DO:
define var hcontainer as handle.
define var r as rowid.
RUN get-rowid1 in h_b_lotes_jugo (output r).
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
       RUN set-position IN h_cus-navico ( 1.29 , 36.40 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 23.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.29 , 58.40 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 20.00 ) NO-ERROR.

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-updsav ).
       RUN set-position IN h_cus-updsav ( 1.29 , 2.40 ) NO-ERROR.
       RUN set-size IN h_cus-updsav ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_lotes_jugo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_lotes_jugo ).
       RUN set-position IN h_b_lotes_jugo ( 3.38 , 1.00 ) NO-ERROR.
       RUN set-size IN h_b_lotes_jugo ( 6.33 , 150.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_lotes_jugo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_lotes_jugo ).
       RUN set-position IN h_v_lotes_jugo ( 10.29 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.67 , 149.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_origen_tambores_lote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_origen_tambores_lote ).
       RUN set-position IN h_b_origen_tambores_lote ( 20.76 , 1.00 ) NO-ERROR.
       RUN set-size IN h_b_origen_tambores_lote ( 7.14 , 152.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_lotes_jugo. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_lotes_jugo ).

       /* Links to SmartViewer h_v_lotes_jugo. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo , 'Record':U , h_v_lotes_jugo ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav , 'TableIO':U , h_v_lotes_jugo ).

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
       RUN set-position IN h_cus-updsav-2 ( 1.29 , 3.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-2 ( 1.91 , 35.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_inspecciones_lote1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_inspecciones_lote1 ).
       RUN set-position IN h_b_inspecciones_lote1 ( 4.57 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b_inspecciones_lote1 ( 4.76 , 125.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-5 ).
       RUN set-position IN h_cus-updsav-5 ( 11.71 , 5.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-5 ( 2.00 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-6 ).
       RUN set-position IN h_cus-updsav-6 ( 11.71 , 75.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-6 ( 2.00 , 36.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_composicion_lote.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = No':U ,
             OUTPUT h_b_composicion_lote ).
       RUN set-position IN h_b_composicion_lote ( 13.62 , 5.00 ) NO-ERROR.
       RUN set-size IN h_b_composicion_lote ( 4.76 , 57.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_arrastre_lotes_jugo.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_arrastre_lotes_jugo ).
       RUN set-position IN h_b_arrastre_lotes_jugo ( 13.62 , 75.00 ) NO-ERROR.
       RUN set-size IN h_b_arrastre_lotes_jugo ( 4.76 , 76.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/b_sobrante.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_sobrante ).
       RUN set-position IN h_b_sobrante ( 19.33 , 37.00 ) NO-ERROR.
       RUN set-size IN h_b_sobrante ( 8.33 , 115.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_cus-updsav-3 ).
       RUN set-position IN h_cus-updsav-3 ( 20.76 , 1.00 ) NO-ERROR.
       RUN set-size IN h_cus-updsav-3 ( 2.00 , 36.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to csmartbrowser h_b_inspecciones_lote1. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo , 'record':U , h_b_inspecciones_lote1 ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-2 , 'TableIO':U , h_b_inspecciones_lote1 ).

       /* Links to csmartbrowser h_b_composicion_lote. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo , 'record':U , h_b_composicion_lote ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-5 , 'TableIO':U , h_b_composicion_lote ).

       /* Links to csmartbrowser h_b_arrastre_lotes_jugo. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo , 'Record':U , h_b_arrastre_lotes_jugo ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-6 , 'TableIO':U , h_b_arrastre_lotes_jugo ).

       /* Links to csmartbrowser h_b_sobrante. */
       RUN add-link IN adm-broker-hdl ( h_b_lotes_jugo , 'Record':U , h_b_sobrante ).
       RUN add-link IN adm-broker-hdl ( h_cus-updsav-3 , 'TableIO':U , h_b_sobrante ).

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
    run get-rowid1 in h_b_lotes_jugo (output r1).
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
  DISPLAY FILL-2 FILL-IN-1 ar FILL1 radTipoTambor 
      WITH FRAME F-Main.
  ENABLE b-detalle FILL-IN-1 BUTTON-8 BUTTON-29 BUTTON-28 BUTTON-33 BUTTON-43 
         BUTTON-31 ar BUTTON-32 BUTTON-1 BUTTON-9 BUTTON-30 radTipoTambor 
         BUTTON-7 BUTTON-42 BUTTON-5 BUTTON-6 BUTTON-3 BUTTON-12 BUTTON-13 REC1 
         REC3 RECT-17 RECT-18 RECT-4 
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

run get-rowid1 in h_b_lotes_jugo (output r).

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
            put "N�mero de " tipos_origen.descripcion envasado_lote.id_origen "/00," " cantidad " 
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
      /*rec4:hidden = true.*/
      /*rect-19:hidden = true.*/
      button-7:hidden = true.
      rec1:hidden = false.
/***********************************************************************/
    define var r1 as rowid.
    run get-rowid1 in h_b_lotes_jugo (output r1).
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

  button-5:HIDDEN IN FRAME F-Main  = TRUE.
  button-3:HIDDEN IN FRAME F-Main  = TRUE.
  button-12:HIDDEN IN FRAME F-Main = TRUE.
  button-13:HIDDEN IN FRAME F-Main = TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailing F-Frame-Win 
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
  
  RUN get-rowid1 IN h_b_lotes_jugo (OUTPUT r).

  FIND FIRST lotes_jugo WHERE ROWID(lotes_jugo) = r NO-LOCK NO-ERROR.
  IF AVAILABLE lotes_jugo THEN DO:
    FIND FIRST productos_terminados WHERE productos_terminados.id_articulo = lotes_jugo.id_articulo NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF lotes_jugo  NO-LOCK NO-ERROR.
    FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
    FOR EACH tambores_industria OF lotes_jugo NO-LOCK.
      iCant = iCant + 1.
    END.

    IF lotes_jugo.id_sucursal = 95 THEN 
      cUsuarios = "depositof@sa-sanmiguel.com,mmuroni@sa-sanmiguel.com".

    IF lotes_jugo.id_sucursal = 96 THEN 
      cUsuarios = "jchaile@sa-sanmiguel.com".

    cUsuarios = cUsuarios + ",facundoj@sa-sanmiguel.com".

    cLot = STRING(lotes_jugo.id_lote) + "/" + STRING(lotes_jugo.anio).
    cArt = IF AVAILABLE productos_terminados THEN trim(productos_terminados.descripcion) ELSE "nada".
    cCal = IF AVAILABLE calidades THEN trim(calidades.descripcion) ELSE "nada".
    cEnv = IF AVAILABLE envases_prod THEN trim(envases_prod.descripcion) ELSE "nada".

    cUsuarios = "facundoj@sa-sanmiguel.com".
    cSubject  = "Aviso de creacion de lote para registro de salida de insumos del lote " + cLot.
    cBody     = "Lote:     " + cLot + CHR(10) + 
                "Tipo:     " + cArt + CHR(10) + 
                "Calidad:  " + cCal + CHR(10) + 
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

        del_empresa     = lotes_jugo.id_empresa.
        del_sucursal    = lotes_jugo.id_sucursal.
        del_tipotambor  = lotes_jugo.id_tipotambor.
        del_nromov      = lotes_jugo.nromov.
        del_lote        = lotes_jugo.id_lote.
        del_fecha       = lotes_jugo.fecha.
        del_articulo    = lotes_jugo.id_articulo.
        del_calidad     = lotes_jugo.id_calidad.
        
    
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
