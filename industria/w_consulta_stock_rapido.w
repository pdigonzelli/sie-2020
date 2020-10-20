&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
define var alta as logical no-undo initial false.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-7 RECT-9 RECT-6 RECT-4 RECT-8 ~
sucursal articulo calidad BUTTON-20 BUTTON-24 BUTTON-23 BUTTON-21 BUTTON-27 ~
BUTTON-15 BUTTON-13 BUTTON-14 BUTTON-19 BUTTON-17 BUTTON-16 BUTTON-18 ~
nombre_articulo nombre_calidad 
&Scoped-Define DISPLAYED-OBJECTS sucursal articulo calidad FILL-IN-10 ~
FILL-IN-7 FILL-IN-9 FILL-IN-8 nombre_articulo nombre_calidad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-13 
     LABEL "Stock desde Lavalle" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-14 
     LABEL "Stock desde Famailla (Citrix)" 
     SIZE 30 BY 1.14.

DEFINE BUTTON BUTTON-15 
     LABEL "Stock desde Lavalle" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-16 
     LABEL "Stock desde Lavalle" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-17 
     LABEL "Stock desde Lavalle" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BUTTON-18 
     LABEL "Stock desde Famailla (Citrix)" 
     SIZE 30 BY 1.14.

DEFINE BUTTON BUTTON-19 
     LABEL "Reporte" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-20 
     LABEL "Listado desde Lavalle" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-21 
     LABEL "Listado desde Famailla" 
     SIZE 24 BY 1.14.

DEFINE BUTTON BUTTON-23 
     LABEL "Stock x OF desde Famailla" 
     SIZE 27 BY 1.14.

DEFINE BUTTON BUTTON-24 
     LABEL "Stock x OF desde Lavalle" 
     SIZE 27 BY 1.14.

DEFINE BUTTON BUTTON-27 
     LABEL "Stock x OF desde Famailla" 
     SIZE 27 BY 1.14.

DEFINE VARIABLE articulo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE calidad AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Calidad" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Listados Stock Completos Ordenados" 
     VIEW-AS FILL-IN 
     SIZE 57 BY 1
     BGCOLOR 6 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "Listados Stock Materia Prima de Jugos" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Listados Stock Materia Prima de Aceites" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Listado x Citral x Kilos" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 2 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE nombre_articulo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 50 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE nombre_calidad AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 46 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE sucursal AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Plantas(95 Famailla - 96 Lavalle)" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 115 BY 50
     BGCOLOR 7 FGCOLOR 7 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY 5.48.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 3.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 3.57.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 3.57.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     sucursal AT ROW 1.71 COL 36 COLON-ALIGNED
     articulo AT ROW 2.91 COL 11 COLON-ALIGNED
     calidad AT ROW 4.1 COL 11 COLON-ALIGNED
     FILL-IN-10 AT ROW 7.67 COL 3 COLON-ALIGNED NO-LABEL
     BUTTON-20 AT ROW 9.1 COL 5
     BUTTON-24 AT ROW 9.1 COL 34
     BUTTON-23 AT ROW 10.29 COL 94
     BUTTON-21 AT ROW 10.52 COL 5
     BUTTON-27 AT ROW 10.52 COL 34
     FILL-IN-7 AT ROW 12.91 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 12.91 COL 72 COLON-ALIGNED NO-LABEL
     BUTTON-15 AT ROW 14.33 COL 6
     BUTTON-13 AT ROW 14.33 COL 6
     BUTTON-14 AT ROW 14.33 COL 37
     BUTTON-19 AT ROW 14.33 COL 75
     FILL-IN-8 AT ROW 16.71 COL 2 COLON-ALIGNED NO-LABEL
     BUTTON-17 AT ROW 18.14 COL 6
     BUTTON-16 AT ROW 18.14 COL 6
     BUTTON-18 AT ROW 18.14 COL 37
     nombre_articulo AT ROW 2.91 COL 19 COLON-ALIGNED NO-LABEL
     nombre_calidad AT ROW 4.1 COL 23 COLON-ALIGNED NO-LABEL
     RECT-2 AT Y 340 X 500
     RECT-7 AT ROW 16.24 COL 2
     RECT-9 AT ROW 7.19 COL 2
     RECT-6 AT ROW 12.43 COL 2
     RECT-4 AT ROW 1.48 COL 2
     RECT-8 AT ROW 12.43 COL 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126.8 BY 19.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Reporte Stock"
         HEIGHT             = 19.19
         WIDTH              = 126.8
         MAX-HEIGHT         = 19.19
         MAX-WIDTH          = 126.8
         VIRTUAL-HEIGHT     = 19.19
         VIRTUAL-WIDTH      = 126.8
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
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" W-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Reporte Stock */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Reporte Stock */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME articulo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL articulo W-Win
ON LEAVE OF articulo IN FRAME F-Main /* Articulo */
DO:
  find productos_terminados where productos_terminados.id_articulo =
                                  integer(articulo:screen-value in frame F-Main)  no-lock no-error.
  if available productos_terminados then
    nombre_articulo:screen-value in frame F-Main = productos_terminados.descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL articulo W-Win
ON MOUSE-SELECT-DBLCLICK OF articulo IN FRAME F-Main /* Articulo */
DO:
  define var r as rowid.
  
  run wc_articulos.w (output r).
  find productos_terminados where rowid(productos_terminados) = r no-lock no-error.
  if available productos_terminados then
    articulo:screen-value in frame F-Main = string(productos_terminados.id_articulo).
    nombre_articulo:screen-value in frame F-Main = productos_terminados.descripcion.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 W-Win
ON CHOOSE OF BUTTON-13 IN FRAME F-Main /* Stock desde Lavalle */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_tambores_mat_prima.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 W-Win
ON CHOOSE OF BUTTON-14 IN FRAME F-Main /* Stock desde Famailla (Citrix) */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_tambores_mat_prima.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_citrix",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 W-Win
ON CHOOSE OF BUTTON-15 IN FRAME F-Main /* Stock desde Lavalle */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_tambores_mat_prima.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 W-Win
ON CHOOSE OF BUTTON-16 IN FRAME F-Main /* Stock desde Lavalle */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  for each stock_tambores.
    delete stock_tambores.
  end.
  
  run p_calculo_stock_mat_prima_aceite.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-17 W-Win
ON CHOOSE OF BUTTON-17 IN FRAME F-Main /* Stock desde Lavalle */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  for each stock_tambores.
    delete stock_tambores.
  end.
  
  run p_calculo_stock_mat_prima_aceite.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 W-Win
ON CHOOSE OF BUTTON-18 IN FRAME F-Main /* Stock desde Famailla (Citrix) */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  
  for each stock_tambores.
    delete stock_tambores.
  end.
  
  run p_calculo_stock_mat_prima_aceite.p (input integer(sucursal:screen-value in frame F-Main)).
  
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_citrix",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 W-Win
ON CHOOSE OF BUTTON-19 IN FRAME F-Main /* Reporte */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 

  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          v_filtro = "tambores_industria.id_tipotambor = 2 and " + 
                     " tambores_industria.id_locacion_ubicacion = 4 and " + 
                     " tambores_industria.id_sucursal = " + sucursal:screen-value in frame F-Main .                    
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "prod_aceite_citral",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-20 W-Win
ON CHOOSE OF BUTTON-20 IN FRAME F-Main /* Listado desde Lavalle */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_nuevo_tambores.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_ordenado",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-21 W-Win
ON CHOOSE OF BUTTON-21 IN FRAME F-Main /* Listado desde Famailla */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_nuevo_tambores.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_citrix_ord",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-23 W-Win
ON CHOOSE OF BUTTON-23 IN FRAME F-Main /* Stock x OF desde Famailla */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_nuevo_tambores.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "rep_stock_agrupado_of_citrix_ord",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-24 W-Win
ON CHOOSE OF BUTTON-24 IN FRAME F-Main /* Stock x OF desde Lavalle */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_nuevo_tambores.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "rep_stock_agrupado_of_ord",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-27 W-Win
ON CHOOSE OF BUTTON-27 IN FRAME F-Main /* Stock x OF desde Famailla */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
if integer(sucursal:screen-value in frame F-Main) = 95 or 
   integer(sucursal:screen-value in frame F-Main) = 96 then
  do: 
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  
  run w_calculo_stock_nuevo_tambores.p (input integer(sucursal:screen-value in frame F-Main)).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "stock_tambores.id_articulo = " + string(v_articulo) +
                           " and stock_tambores.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
             
                 
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "stock_agrup_of_citrix_ord_total",                    /* RB-REPORT-NAME */
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
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/
end.
else
    message "Por favor ingrese la codigo de sucursal(95/96) de la planta que desea consultar." view-as 
            alert-box.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME calidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL calidad W-Win
ON LEAVE OF calidad IN FRAME F-Main /* Calidad */
DO:
  find calidades where calidades.id_calidad =
                       integer(calidad:screen-value in frame F-Main)  no-lock no-error.
  if available calidades then
    nombre_calidad:screen-value in frame F-Main = calidades.descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL calidad W-Win
ON MOUSE-SELECT-DBLCLICK OF calidad IN FRAME F-Main /* Calidad */
DO:
  define var r as rowid.
  
  run wc_calidades.w (output r).
  find calidades where rowid(calidades) = r no-lock no-error.
  if available calidades then
    calidad:screen-value in frame F-Main = string(calidades.id_calidad).
    nombre_calidad:screen-value in frame F-Main = calidades.descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/method/winitialize.i}
{custom/method/ctitulo.i}
run deshabilita_viewer.
run habilitar_relacion_viewer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
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
       RUN set-position IN h_cus-misc ( 17.43 , 102.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             FILL-IN-8:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer W-Win 
PROCEDURE deshabilita_viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
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
  DISPLAY sucursal articulo calidad FILL-IN-10 FILL-IN-7 FILL-IN-9 FILL-IN-8 
          nombre_articulo nombre_calidad 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-2 RECT-7 RECT-9 RECT-6 RECT-4 RECT-8 sucursal articulo calidad 
         BUTTON-20 BUTTON-24 BUTTON-23 BUTTON-21 BUTTON-27 BUTTON-15 BUTTON-13 
         BUTTON-14 BUTTON-19 BUTTON-17 BUTTON-16 BUTTON-18 nombre_articulo 
         nombre_calidad 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy W-Win 
PROCEDURE post-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.
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

alta = false.
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
alta = true.

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
alta = true.

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

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
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


