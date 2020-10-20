&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fecha_desde fecha_hasta lugdes articulo ~
calidad suc_remito BUTTON-3 BUTTON-5 BUTTON-22 BUTTON-33 nombre_lugdes ~
nombre_articulo nombre_calidad RECT-2 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS fecha_desde fecha_hasta lugdes articulo ~
calidad suc_remito nombre_lugdes nombre_articulo nombre_calidad 

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
DEFINE BUTTON BUTTON-22 
     LABEL "Reporte por OF" 
     SIZE 31 BY .95.

DEFINE BUTTON BUTTON-3 
     LABEL "Reporte Despacho Standard" 
     SIZE 29 BY .95.

DEFINE BUTTON BUTTON-33 
     LABEL "Reporte por Cliente" 
     SIZE 31 BY .95.

DEFINE BUTTON BUTTON-4 
     LABEL "Reporte Despacho Quimicos" 
     SIZE 29 BY .95.

DEFINE BUTTON BUTTON-5 
     LABEL "Reporte por OE" 
     SIZE 31 BY .95.

DEFINE VARIABLE articulo AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE calidad AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Calidad" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fecha_desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fecha_hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "hasta" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lugdes AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Lug.Descarga" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE nombre_articulo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 50 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE nombre_calidad AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 46 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE nombre_lugdes AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 43 BY .95
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE suc_remito AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 115 BY 50
     BGCOLOR 7 FGCOLOR 7 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 11.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fecha_desde AT ROW 2.91 COL 9 COLON-ALIGNED
     fecha_hasta AT ROW 2.91 COL 35 COLON-ALIGNED
     lugdes AT ROW 4.1 COL 18 COLON-ALIGNED
     articulo AT ROW 5.29 COL 11 COLON-ALIGNED
     calidad AT ROW 6.48 COL 11 COLON-ALIGNED
     suc_remito AT ROW 7.67 COL 11 COLON-ALIGNED
     BUTTON-3 AT ROW 9.1 COL 4
     BUTTON-5 AT ROW 9.1 COL 39
     BUTTON-22 AT ROW 10.05 COL 39
     BUTTON-33 AT ROW 11 COL 39
     BUTTON-4 AT ROW 14.33 COL 10
     nombre_lugdes AT ROW 4.1 COL 26 COLON-ALIGNED NO-LABEL
     nombre_articulo AT ROW 5.29 COL 19 COLON-ALIGNED NO-LABEL
     nombre_calidad AT ROW 6.48 COL 23 COLON-ALIGNED NO-LABEL
     RECT-2 AT Y 270 X 225
     RECT-4 AT ROW 1.48 COL 2
     "Reporte de Despacho de Lotes:" VIEW-AS TEXT
          SIZE 56 BY .71 AT ROW 1.71 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.4 BY 15.86.


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
         TITLE              = "Reporte de Despachos"
         HEIGHT             = 15.86
         WIDTH              = 72
         MAX-HEIGHT         = 21.24
         MAX-WIDTH          = 107
         VIRTUAL-HEIGHT     = 21.24
         VIRTUAL-WIDTH      = 107
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
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-4:HIDDEN IN FRAME F-Main           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Reporte de Despachos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Reporte de Despachos */
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


&Scoped-define SELF-NAME BUTTON-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-22 W-Win
ON CHOOSE OF BUTTON-22 IN FRAME F-Main /* Reporte por OF */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  define var v_lugdes as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  DEFINE VAR r AS ROWID.
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  v_lugdes = integer(lugdes:screen-value in frame F-Main).
  
  RUN wc_contratos.w (OUTPUT r).

  run w_desp_industria_x_of.p (input r).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo) +
                           " and despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
            
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_despachos_quimicos",                    /* RB-REPORT-NAME */
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
         "Reporte de Despachos",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/

  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Reporte Despacho Standard */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  define var v_lugdes as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  v_lugdes = integer(lugdes:screen-value in frame F-Main).
  
  run w_calculo_despachos_industria.p (input v_fecha_desde, input v_fecha_hasta, input v_lugdes).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo) +
                           " and despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
            
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_despachos_rapido",                    /* RB-REPORT-NAME */
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
         "Reporte de Despachos",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/

  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-33 W-Win
ON CHOOSE OF BUTTON-33 IN FRAME F-Main /* Reporte por Cliente */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  define var v_lugdes as integer.
  DEFINE VAR v_sucursal AS INTEGER.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  DEFINE VAR r AS ROWID.
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  v_lugdes = integer(lugdes:screen-value in frame F-Main).
  v_sucursal = INTEGER(suc_remito:SCREEN-VALUE IN FRAME F-Main).
  
  RUN wc_clientes.w (OUTPUT r).
  
  FIND FIRST clientes WHERE ROWID(cliente) = r NO-LOCK NO-ERROR.
  IF AVAILABLE cliente THEN DO:
    run w_desp_industria_x_cliente.p (INPUT clientes.id_cliente,
                                      INPUT v_fecha_desde,
                                      INPUT v_fecha_hasta,
                                      INPUT v_sucursal).

    /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
 /*       
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo) +
                           " and despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
   */         
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_despachos_quimicos",                    /* RB-REPORT-NAME */
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
         "Reporte de Despachos",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/

  END.
  
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Reporte Despacho Quimicos */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  define var v_lugdes as integer.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  v_lugdes = integer(lugdes:screen-value in frame F-Main).
  
  run w_calculo_despachos_industria.p (input v_fecha_desde, input v_fecha_hasta, input v_lugdes).
  
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo) +
                           " and despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
            
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_despachos_quimicos",                    /* RB-REPORT-NAME */
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
         "Reporte de Despachos",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/

  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Reporte por OE */
DO:
  define var v_filtro as char.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  define var v_lugdes as integer.
  DEFINE VAR v_sucursal AS INTEGER.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  
  
  v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
  v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
  v_articulo = integer(articulo:screen-value in frame F-Main).
  v_calidad = integer(calidad:screen-value in frame F-Main).
  v_lugdes = integer(lugdes:screen-value in frame F-Main).
  v_sucursal = INTEGER(suc_remito:SCREEN-VALUE IN FRAME F-Main).
  
  IF v_articulo = 54 THEN 
    RUN pDespachosCascara.p (INPUT v_fecha_desde, 
                             INPUT v_fecha_hasta, 
                             INPUT v_lugdes,
                             INPUT v_sucursal).
  ELSE
    RUN w_calculo_despachos_definitivos_industria.p (INPUT v_fecha_desde, 
                                                     INPUT v_fecha_hasta, 
                                                     INPUT v_lugdes,
                                                     INPUT v_sucursal).
    
  /************************************************************************************************************/
  /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /************************************************************************************************************/
        
          if v_articulo <> 0 and v_calidad = 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo).
            end. 
          
          if v_articulo = 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo <> 0 and v_calidad <> 0 then
            do:
                v_filtro = "despachos_industria.id_articulo = " + string(v_articulo) +
                           " and despachos_industria.id_calidad = " + string(v_calidad).       
            end.
          
          if v_articulo = 0 and v_calidad = 0 then
            do:
                v_filtro = "".       
            end.
            
                        
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_despachos_quimicos",                    /* RB-REPORT-NAME */
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
         "Reporte de Despachos",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
 
  /************************************************************************************************************/

  

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


&Scoped-define SELF-NAME lugdes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lugdes W-Win
ON LEAVE OF lugdes IN FRAME F-Main /* Lug.Descarga */
DO:
  find lugar_descarga where lugar_descarga.id_lugdes =
                                  integer(lugdes:screen-value in frame F-Main)  no-lock no-error.
  if available lugar_descarga then
    nombre_lugdes:screen-value in frame F-Main = lugar_descarga.descripcion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lugdes W-Win
ON MOUSE-SELECT-DBLCLICK OF lugdes IN FRAME F-Main /* Lug.Descarga */
DO:
  define var r as rowid.
  
  run wc_lugar_descarga.w (output r).
  find lugar_descarga where rowid(lugar_descarga) = r no-lock no-error.
  if available lugar_descarga then
    lugdes:screen-value in frame F-Main = string(lugar_descarga.id_lugdes).
    nombre_lugdes:screen-value in frame F-Main = lugar_descarga.descripcion.
  
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
       RUN set-position IN h_cus-misc ( 14.10 , 47.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             BUTTON-33:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

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
  DISPLAY fecha_desde fecha_hasta lugdes articulo calidad suc_remito 
          nombre_lugdes nombre_articulo nombre_calidad 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fecha_desde fecha_hasta lugdes articulo calidad suc_remito BUTTON-3 
         BUTTON-5 BUTTON-22 BUTTON-33 nombre_lugdes nombre_articulo 
         nombre_calidad RECT-2 RECT-4 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reporte_despacho W-Win 
PROCEDURE reporte_despacho :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var v_fecha_desde as date.
define var v_fecha_hasta as date.
define var v_articulo as integer.
define var v_calidad as integer.
define var v_filtro as char.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".



v_articulo = integer(articulo:screen-value in frame F-Main).
v_calidad = integer(calidad:screen-value in frame F-Main).

if fecha_desde:screen-value in frame F-Main <> "" and 
   fecha_hasta:screen-value in frame F-Main = "" then
    do:
        v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
        v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
        v_filtro = "remitos.fecha = date('" + string(v_fecha_desde) + "')".
    end.

if fecha_desde:screen-value in frame F-Main <> "" and 
   fecha_hasta:screen-value in frame F-Main <> "" then
    do:
        v_fecha_desde = date(fecha_desde:screen-value in frame F-Main).
        v_fecha_hasta = date(fecha_hasta:screen-value in frame F-Main).
        v_filtro = "remitos.fecha >= date('" + string(v_fecha_desde) + "') and remitos.fecha <= date('" + string(v_fecha_hasta) + "')".
    end.
               
if v_articulo > 0 then
    v_filtro = v_filtro + " and tambores_industria.id_articulo = " + string(v_articulo).
    
if v_calidad > 0 then
    v_filtro = v_filtro + " and tambores_industria.id_calidad = " + string(v_calidad).    
             
         RUN  aderb\_prntrb2(
         "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_despachos",                    /* RB-REPORT-NAME */
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
         "Reporte de Despachos",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).   
    
      
  /************************************************************************************************************/
  





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

