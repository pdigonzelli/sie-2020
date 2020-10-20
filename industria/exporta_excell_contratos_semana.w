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
/*-- VARIABLES DE EXCEL --*/

    define var chExcelAplication as com-handle.
    define var chWorkbook        as com-handle.
    define var chWorkSheet       as com-handle.
    define var chchart           as com-handle.
    define var chWorkSheetRange  as com-handle.
  
    define var ifila  as integer.
    define var cfila  as character.
    define var crange as character.

 /*-- FIN VARIABLES DE EXCEL --*/

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
&Scoped-Define ENABLED-OBJECTS BUTTON-5 BUTTON-19 BUTTON-16 BUTTON-17 ~
BUTTON-18 BUTTON-20 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 

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
DEFINE BUTTON BUTTON-16 
     LABEL "Contrato (Articulo - Calidad- Precio)" 
     SIZE 37 BY 1.14.

DEFINE BUTTON BUTTON-17 
     LABEL "Reporte Comercial Completo" 
     SIZE 37 BY 1.14.

DEFINE BUTTON BUTTON-18 
     LABEL "Reporte Prog. de la Produccion" 
     SIZE 37 BY 1.14.

DEFINE BUTTON BUTTON-19 
     LABEL "Reporte de Shipping Status" 
     SIZE 28 BY 1.19.

DEFINE BUTTON BUTTON-20 
     LABEL "Presupuesto" 
     SIZE 37 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "Contrato por Semana" 
     SIZE 37 BY 1.14.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Programa de Exportación a Excell" 
     VIEW-AS FILL-IN 
     SIZE 65 BY 1
     BGCOLOR 4 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 115 BY 50
     BGCOLOR 7 FGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-1 AT ROW 1.48 COL 4 NO-LABEL
     BUTTON-5 AT ROW 3.62 COL 7
     BUTTON-19 AT ROW 3.62 COL 44
     BUTTON-16 AT ROW 4.81 COL 7
     BUTTON-17 AT ROW 6 COL 7
     BUTTON-18 AT ROW 7.19 COL 7
     BUTTON-20 AT ROW 8.38 COL 7
     RECT-2 AT Y 240 X 280
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 19.14.


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
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 14.1
         WIDTH              = 80.4
         MAX-HEIGHT         = 19.14
         MAX-WIDTH          = 107
         VIRTUAL-HEIGHT     = 19.14
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
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 W-Win
ON CHOOSE OF BUTTON-16 IN FRAME F-Main /* Contrato (Articulo - Calidad- Precio) */
DO:

define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.
    
run wc_sel_rango_semana.w (output v_semana_desde,
                           output v_anio_desde,
                           output v_semana_hasta,
                           output v_anio_hasta).
                           
  
 /*-- CONFIGURACION INICIAL --*/

    create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 5.
    chWorkSheet:Columns("B"):ColumnWidth = 5.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 5.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 25.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 25.
    chWorkSheet:Columns("I"):ColumnWidth = 25.
    chWorkSheet:Columns("J"):ColumnWidth = 15.
    chWorkSheet:Columns("K"):ColumnWidth = 5.
    chWorkSheet:Columns("L"):ColumnWidth = 9.
    chWorkSheet:Columns("M"):ColumnWidth = 25.
    chWorkSheet:Columns("N"):ColumnWidth = 7.
    chWorkSheet:Columns("O"):ColumnWidth = 25.
    chWorkSheet:Columns("P"):ColumnWidth = 25.
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "REPORTE POR SEMANA".
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  */
  
  chWorkSheet:Range("C6"):Value = "SEMANA EMBARQUE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "COD.".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "CALIDAD".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "COD.".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "PRODUCTO".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CLIENTE".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "CONTRATO".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "PARTE".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "PRECIO VTA.".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "DESTINO".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "CANTIDAD".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "SHIPPING CONFIRMATION".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "CLIENTE FINAL".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).

  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 9.
  if (v_semana_desde >= 0 and v_semana_desde < 53) and
     (v_semana_hasta >= 0 and v_semana_hasta < 53) then
    do:
        if v_anio_desde = v_anio_hasta then
            do:
               for each items_contratos where items_contratos.semana_entrega >= v_semana_desde and 
                                              items_contratos.semana_entrega <= v_semana_hasta and 
                                              items_contratos.anio_semana_entrega = v_anio_desde no-lock
                                              by items_contratos.anio_semana_entrega
                                              by items_contratos.semana_entrega.
                                              
                    find contratos of items_contratos no-lock no-error.
                    find clientes of contratos no-lock no-error.
                    find calidades of items_contratos no-lock no-error.
                    find destinos where destinos.id_destino = items_contratos.id_destino no-lock no-error.    
                    find productos_terminados of items_contratos no-lock no-error.
                    
                    {p_exportacion_por_semana.i}.
                        
               end.
            end.
        else
            do:
               for each items_contratos where (items_contratos.semana_entrega >= v_semana_desde and 
                                               items_contratos.semana_entrega <= 52 and 
                                               items_contratos.anio_semana_entrega = v_anio_desde) 
                                           or (items_contratos.semana_entrega <= v_semana_hasta and 
                                               items_contratos.anio_semana_entrega = v_anio_hasta) no-lock
                                              by items_contratos.anio_semana_entrega
                                              by items_contratos.semana_entrega.
                    find contratos of items_contratos no-lock no-error.
                    find clientes of contratos no-lock no-error.
                    find calidades of items_contratos no-lock no-error.
                    find destinos where destinos.id_destino = items_contratos.id_destino no-lock no-error.
                    find productos_terminados of items_contratos no-lock no-error.
                        
                    {p_exportacion_por_semana.i}.    
               end.
            end.    
    end.
else
    do:
        for each items_contratos no-lock
            by items_contratos.anio_semana_entrega
            by items_contratos.semana_entrega.
            find contratos of items_contratos no-lock no-error.
            find clientes of contratos no-lock no-error.
            find calidades of items_contratos no-lock no-error.
            find destinos where destinos.id_destino = items_contratos.id_destino no-lock no-error.
            find productos_terminados of items_contratos no-lock no-error.
            
              {p_exportacion_por_semana.i}.       
              
        end.
    end.
  
  
  
  
                       
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-17 W-Win
ON CHOOSE OF BUTTON-17 IN FRAME F-Main /* Reporte Comercial Completo */
DO:

define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.
/*    
run wc_sel_rango_semana.w (output v_semana_desde,
                           output v_anio_desde,
                           output v_semana_hasta,
                           output v_anio_hasta).
*/                           
RUN p_calculo_re_cial_completo1.p (INPUT 0, DATE("01/01/2002")).
 /*-- CONFIGURACION INICIAL --*/

    create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 5.
    chWorkSheet:Columns("B"):ColumnWidth = 15.
    chWorkSheet:Columns("C"):ColumnWidth = 15.
    chWorkSheet:Columns("D"):ColumnWidth = 15.
    chWorkSheet:Columns("E"):ColumnWidth = 15.
    chWorkSheet:Columns("F"):ColumnWidth = 15.
    chWorkSheet:Columns("G"):ColumnWidth = 15.
    chWorkSheet:Columns("H"):ColumnWidth = 15.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    /* chWorkSheet:Columns("I"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("J"):ColumnWidth = 15.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 15.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 15.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    chWorkSheet:Columns("R"):ColumnWidth = 15.
    chWorkSheet:Columns("S"):ColumnWidth = 15.
    chWorkSheet:Columns("T"):ColumnWidth = 15.
    chWorkSheet:Columns("T"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("U"):ColumnWidth = 15.
    chWorkSheet:Columns("V"):ColumnWidth = 15.
    chWorkSheet:Columns("W"):ColumnWidth = 15.
    chWorkSheet:Columns("W"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("X"):ColumnWidth = 15.
    chWorkSheet:Columns("X"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("Y"):ColumnWidth = 15.
    chWorkSheet:Columns("Z"):ColumnWidth = 15.
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "REPORTE POR SEMANA".
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  */
  
  chWorkSheet:Range("A5:Z5"):HorizontalAlignment = 3.
  chWorkSheet:Range("B5:J5"):MergeCells = True.
  chWorkSheet:Range("K5:M5"):MergeCells = True.
  chWorkSheet:Range("N5:P5"):MergeCells = True.
  chWorkSheet:Range("Q5:X5"):MergeCells = True.

  chWorkSheet:Range("B5"):Value = "CONTRATO".
  chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B5"):interior:colorindex = 19.
  chWorkSheet:Range("B5"):Font:colorindex = 1.
  chWorkSheet:Range("C5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C5"):Font:colorindex = 2.
  chWorkSheet:Range("D5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D5"):Font:colorindex = 2.
  chWorkSheet:Range("E5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E5"):Font:colorindex = 2.
  chWorkSheet:Range("F5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F5"):Font:colorindex = 2.
  chWorkSheet:Range("G5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G5"):Font:colorindex = 2.
  chWorkSheet:Range("H5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H5"):Font:colorindex = 2.
  chWorkSheet:Range("I5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I5"):Font:colorindex = 1.
  chWorkSheet:Range("J5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J5"):Font:colorindex = 2.
  chWorkSheet:Range("K5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K5"):interior:colorindex = 31.
  chWorkSheet:Range("K5"):Font:colorindex = 2.
  chWorkSheet:Range("K5"):Value = "ORDEN EMBARQUE".
  chWorkSheet:Range("L5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L5"):Font:colorindex = 2.
  chWorkSheet:Range("M5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M5"):Font:colorindex = 1.
  chWorkSheet:Range("N5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N5"):interior:colorindex = 22.
  chWorkSheet:Range("N5"):Font:colorindex = 1.
  chWorkSheet:Range("N5"):Value = "PACKING LIST".
  chWorkSheet:Range("O5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O5"):Font:colorindex = 2.
  chWorkSheet:Range("P5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P5"):Font:colorindex = 2.
  chWorkSheet:Range("Q5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q5"):interior:colorindex = 21.
  chWorkSheet:Range("Q5"):Font:colorindex = 2.
  chWorkSheet:Range("Q5"):Value = "FACTURA".
  chWorkSheet:Range("R5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R5"):Font:colorindex = 2.
  chWorkSheet:Range("S5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S5"):Font:colorindex = 2.
  chWorkSheet:Range("T5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("T5"):Font:colorindex = 2.
  chWorkSheet:Range("U5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("U5"):Font:colorindex = 2.
  chWorkSheet:Range("V5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("V5"):Font:colorindex = 2.
  chWorkSheet:Range("W5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("W5"):Font:colorindex = 2.
  chWorkSheet:Range("X5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("X5"):Font:colorindex = 2.
  chWorkSheet:Range("Y5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Y5"):interior:colorindex = 19.
  chWorkSheet:Range("Y5"):Font:colorindex = 1.
  chWorkSheet:Range("Y5"):Value = "CONTRATO".
  chWorkSheet:Range("Z5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Z5"):Font:colorindex = 2.


  chWorkSheet:Range("B6"):Value = "AÑO".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "DESDE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "HASTA".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "CLIENTE".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "CLIENTE FINAL".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "CONTRATO".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "CALIDAD".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "CANTIDAD CONTRATO".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "PARTE".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "OE".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "SEMANA OE".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "ESTADO".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "PACKING LIST".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "CANTIDAD".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "LOTE".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "FACTURA".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "FECHA".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S6"):Value = "VENCIMIENTO".
  chWorkSheet:Range("S6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("T6"):Value = "VENTA".
  chWorkSheet:Range("T6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("U6"):Value = "MONEDA".
  chWorkSheet:Range("U6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("V6"):Value = "UNIDAD".
  chWorkSheet:Range("V6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("W6"):Value = "TOTAL ITEM".
  chWorkSheet:Range("W6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("X6"):Value = "TOTAL".
  chWorkSheet:Range("X6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Y6"):Value = "% COMISION".
  chWorkSheet:Range("Y6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Z6"):Value = "IMPORTE COMISION".
  chWorkSheet:Range("Z6"):BorderAround(1,2,1,1).
  

  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH re_cial_completo BY re_cial_completo.anio
                            BY re_cial_completo.semana_desde
                            BY re_cial_completo.semana_hasta.
    
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.anio.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.semana_desde.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.semana_hasta.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.cliente.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.cliente_final.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.id_contrato.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.calidad.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.cantidad_contratos.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.ITEM.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.id_orden_entrega.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.semana_oe.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.estado.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.nro_pack_list.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.cantidad_pl.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.lote.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.factura.
    cRange = "R" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.fecha_factura.
    cRange = "S" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.vto_factura.
    cRange = "T" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.precio_venta.
    cRange = "U" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.moneda_venta.
    cRange = "V" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.unidad_venta.
    cRange = "W" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.importe_item_factura.
    cRange = "X" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.importe_factura.
    cRange = "Y" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.porc_comision.
    cRange = "Z" + cfila.
    chWorkSheet:Range(crange):value = re_cial_completo.importe_comision.

    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 W-Win
ON CHOOSE OF BUTTON-18 IN FRAME F-Main /* Reporte Prog. de la Produccion */
DO:

define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.

RUN p_calculo_prog_produccion.p .
 /*-- CONFIGURACION INICIAL --*/

    create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 5.
    chWorkSheet:Columns("B"):ColumnWidth = 15.
    chWorkSheet:Columns("C"):ColumnWidth = 15.
    chWorkSheet:Columns("D"):ColumnWidth = 15.
    chWorkSheet:Columns("E"):ColumnWidth = 15.
    chWorkSheet:Columns("F"):ColumnWidth = 15.
    chWorkSheet:Columns("G"):ColumnWidth = 15.
    chWorkSheet:Columns("H"):ColumnWidth = 15.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    /* chWorkSheet:Columns("I"):NumberFormat = " ###,###,##0.00".*/
    chWorkSheet:Columns("J"):ColumnWidth = 15.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):ColumnWidth = 15.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 15.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    chWorkSheet:Columns("R"):ColumnWidth = 15.

 /*-- FIN CONFIGURACION INICIAL --*/
 

  chWorkSheet:Range("A5:Z5"):HorizontalAlignment = 3.
  chWorkSheet:Range("B5:J5"):MergeCells = True.
  chWorkSheet:Range("K5:N5"):MergeCells = True.
  chWorkSheet:Range("O5:P5"):MergeCells = True.
  chWorkSheet:Range("Q5:R5"):MergeCells = True.
  
  chWorkSheet:Range("B5"):Value = "CONTRATO".
  chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B5"):interior:colorindex = 19.
  chWorkSheet:Range("B5"):Font:colorindex = 1.
  chWorkSheet:Range("C5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C5"):Font:colorindex = 2.
  chWorkSheet:Range("D5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D5"):Font:colorindex = 2.
  chWorkSheet:Range("E5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E5"):Font:colorindex = 2.
  chWorkSheet:Range("F5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F5"):Font:colorindex = 2.
  chWorkSheet:Range("G5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G5"):Font:colorindex = 2.
  chWorkSheet:Range("H5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H5"):Font:colorindex = 2.
  chWorkSheet:Range("I5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I5"):Font:colorindex = 2.
  chWorkSheet:Range("J5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J5"):Font:colorindex = 2.
  chWorkSheet:Range("K5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K5"):interior:colorindex = 31.
  chWorkSheet:Range("K5"):Font:colorindex = 2.
  chWorkSheet:Range("K5"):Value = "PRODUCCION".
  chWorkSheet:Range("L5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L5"):Font:colorindex = 2.
  chWorkSheet:Range("M5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M5"):Font:colorindex = 1.
  chWorkSheet:Range("N5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N5"):interior:colorindex = 22.
  chWorkSheet:Range("O5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O5"):interior:colorindex = 22.
  chWorkSheet:Range("O5"):Font:colorindex = 1.
  chWorkSheet:Range("O5"):Value = "LOGISTICA".
  chWorkSheet:Range("P5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P5"):Font:colorindex = 2.
  chWorkSheet:Range("Q5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q5"):interior:colorindex = 32.
  chWorkSheet:Range("Q5"):Font:colorindex = 2.
  chWorkSheet:Range("Q5"):Value = "OF".
  chWorkSheet:Range("R5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R5"):Font:colorindex = 2.

  chWorkSheet:Range("B6"):Value = "Nro OF".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "Nro Contrato".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "Calidad".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "Total Kgs".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "Nro Parte".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "Kgs Parte".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "Año".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "Sem. desde".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "hasta".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "Lote asignado".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "Kgs asignados".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "Kgs acum asig".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "Kgs pendientes".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "OE".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "Semana".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "Lote asig. OF".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "Kgs asig. OF".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).

  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH prog_produccion BY prog_produccion.anio
                           BY prog_produccion.semana_desde
                           BY prog_produccion.semana_hasta.
    
    cfila  = string(ifila).
    
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.orden_fabricacion.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.id_contrato.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.calidad.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.TOTAL_kgs_contrato.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.ITEM.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.kgs_parte.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.anio.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.semana_desde.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.semana_hasta.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.lote.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.kgs_asignados.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.kgs_acum_asignados.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.kgs_pendientes.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.id_orden_entrega.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.semana_oe.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.lote_of.
    cRange = "R" + cfila.
    chWorkSheet:Range(crange):value = prog_produccion.kgs_asignados_of.

    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 W-Win
ON CHOOSE OF BUTTON-19 IN FRAME F-Main /* Reporte de Shipping Status */
DO:
  RUN w_reporte_suzzy.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-20 W-Win
ON CHOOSE OF BUTTON-20 IN FRAME F-Main /* Presupuesto */
DO:

define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.
                          
RUN p_calculo_presupuestos.p (INPUT 0, DATE("01/01/2002")).
 /*-- CONFIGURACION INICIAL --*/

    create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 5.
    chWorkSheet:Columns("B"):ColumnWidth = 15.
    chWorkSheet:Columns("C"):ColumnWidth = 15.
    chWorkSheet:Columns("D"):ColumnWidth = 15.
    chWorkSheet:Columns("E"):ColumnWidth = 15.
    chWorkSheet:Columns("F"):ColumnWidth = 15.
    chWorkSheet:Columns("G"):ColumnWidth = 15.
    chWorkSheet:Columns("H"):ColumnWidth = 15.
    chWorkSheet:Columns("I"):ColumnWidth = 15.
    chWorkSheet:Columns("J"):ColumnWidth = 15.
    chWorkSheet:Columns("K"):ColumnWidth = 15.
    chWorkSheet:Columns("K"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("L"):ColumnWidth = 15.
    chWorkSheet:Columns("L"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("M"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("N"):ColumnWidth = 15.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("R"):ColumnWidth = 15.
    chWorkSheet:Columns("R"):NumberFormat = " ###,###,##0.00".
    chWorkSheet:Columns("S"):ColumnWidth = 15.
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
  chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3.
  chWorkSheet:Range("B5:M5"):MergeCells = True.
  chWorkSheet:Range("N5:S5"):MergeCells = True.
  
  chWorkSheet:Range("B5"):Value = "CONTRATO".
  chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B5"):interior:colorindex = 19.
  chWorkSheet:Range("B5"):Font:colorindex = 1.
  chWorkSheet:Range("C5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C5"):Font:colorindex = 2.
  chWorkSheet:Range("D5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D5"):Font:colorindex = 2.
  chWorkSheet:Range("E5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E5"):Font:colorindex = 2.
  chWorkSheet:Range("F5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F5"):Font:colorindex = 2.
  chWorkSheet:Range("G5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G5"):Font:colorindex = 2.
  chWorkSheet:Range("H5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H5"):Font:colorindex = 2.
  chWorkSheet:Range("I5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I5"):Font:colorindex = 1.
  chWorkSheet:Range("J5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J5"):Font:colorindex = 2.
  chWorkSheet:Range("K5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K5"):Font:colorindex = 2.
  chWorkSheet:Range("L5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L5"):Font:colorindex = 2.
  chWorkSheet:Range("M5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M5"):Font:colorindex = 1.
  chWorkSheet:Range("N5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N5"):interior:colorindex = 22.
  chWorkSheet:Range("N5"):Font:colorindex = 1.
  chWorkSheet:Range("N5"):Value = "PACKING LIST".
  chWorkSheet:Range("O5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O5"):Font:colorindex = 2.
  chWorkSheet:Range("P5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P5"):Font:colorindex = 2.
  chWorkSheet:Range("Q5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q5"):Font:colorindex = 2.
  chWorkSheet:Range("R5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R5"):Font:colorindex = 2.
  chWorkSheet:Range("S5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S5"):Font:colorindex = 2.
  
  
  chWorkSheet:Range("B6"):Value = "CLIENTE".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "CLIENTE FINAL".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "CALIDAD".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "CONTRATO".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "PARTE".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "AÑO".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "DESDE".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "HASTA".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "CANTIDAD KILOS".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "PRECIO VENTA".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "PRECIO FOB".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "PRECIO COMERCIAL".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "OE".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "SEMANA OE".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "AÑO OE".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "PRECIO VENTA".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "PRECIO FOB".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S6"):Value = "CANTIDAD KILOS".
  chWorkSheet:Range("S6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH reporte_presupuestos BY reporte_presupuestos.anio
                                BY reporte_presupuestos.semana_desde
                                BY reporte_presupuestos.semana_hasta.
    
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.cliente.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.cliente_final.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.calidad.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.id_contrato.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.ITEM.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.anio.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.semana_desde.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.semana_hasta.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.cantidad_kilos_contrato.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.precio_vta_contrato.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.precio_fob_contrato.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.precio_com_contrato.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.id_orden_entrega.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.semana_oe.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.anio_envio.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.precio_vta_pl.
    cRange = "R" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.precio_fob_pl.
    cRange = "S" + cfila.
    chWorkSheet:Range(crange):value = reporte_presupuestos.cantidad_kilos_pl.
    
    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Contrato por Semana */
DO:

define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.
    
run wc_sel_rango_semana.w (output v_semana_desde,
                           output v_anio_desde,
                           output v_semana_hasta,
                           output v_anio_hasta).
                           
  
 /*-- CONFIGURACION INICIAL --*/

    create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 5.
    chWorkSheet:Columns("B"):ColumnWidth = 5.
    chWorkSheet:Columns("C"):ColumnWidth = 5.
    chWorkSheet:Columns("D"):ColumnWidth = 5.
    chWorkSheet:Columns("E"):ColumnWidth = 25.
    chWorkSheet:Columns("F"):ColumnWidth = 25.
    chWorkSheet:Columns("G"):ColumnWidth = 15.
    chWorkSheet:Columns("H"):ColumnWidth = 5.
    chWorkSheet:Columns("I"):ColumnWidth = 9.
    chWorkSheet:Columns("J"):ColumnWidth = 25.
    chWorkSheet:Columns("K"):ColumnWidth = 7.
    chWorkSheet:Columns("L"):ColumnWidth = 25.
    chWorkSheet:Columns("M"):ColumnWidth = 25.
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "REPORTE POR SEMANA".
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  */
  
  chWorkSheet:Range("C6"):Value = "SEMANA EMBARQUE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "CALIDAD".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "CLIENTE".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "CONTRATO".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "PARTE".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "PRECIO VTA.".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "DESTINO".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "CANTIDAD".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "SHIPPING CONFIRMATION".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "CLIENTE FINAL".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).

  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 9.
  if (v_semana_desde >= 0 and v_semana_desde < 53) and
     (v_semana_hasta >= 0 and v_semana_hasta < 53) then
    do:
        if v_anio_desde = v_anio_hasta then
            do:
               for each items_contratos where items_contratos.semana_entrega >= v_semana_desde and 
                                              items_contratos.semana_entrega <= v_semana_hasta and 
                                              items_contratos.anio_semana_entrega = v_anio_desde no-lock
                                              by items_contratos.anio_semana_entrega
                                              by items_contratos.semana_entrega.
                                              
                    find contratos of items_contratos no-lock no-error.
                    find clientes of contratos no-lock no-error.
                    find calidades of items_contratos no-lock no-error.
                    find destinos where destinos.id_destino = items_contratos.id_destino no-lock no-error.    
                    
                    {p_exportacion_por_semana.i}.
                        
               end.
            end.
        else
            do:
               for each items_contratos where (items_contratos.semana_entrega >= v_semana_desde and 
                                               items_contratos.semana_entrega <= 52 and 
                                               items_contratos.anio_semana_entrega = v_anio_desde) 
                                           or (items_contratos.semana_entrega <= v_semana_hasta and 
                                               items_contratos.anio_semana_entrega = v_anio_hasta) no-lock
                                              by items_contratos.anio_semana_entrega
                                              by items_contratos.semana_entrega.
                    find contratos of items_contratos no-lock no-error.
                    find clientes of contratos no-lock no-error.
                    find calidades of items_contratos no-lock no-error.
                    find destinos where destinos.id_destino = items_contratos.id_destino no-lock no-error.
                        
                    {p_exportacion_por_semana.i}.    
               end.
            end.    
    end.
else
    do:
        for each items_contratos no-lock
            by items_contratos.anio_semana_entrega
            by items_contratos.semana_entrega.
            find contratos of items_contratos no-lock no-error.
            find clientes of contratos no-lock no-error.
            find calidades of items_contratos no-lock no-error.
            find destinos where destinos.id_destino = items_contratos.id_destino no-lock no-error.
 
              {p_exportacion_por_semana.i}.       
              
        end.
    end.
  
  
  
  
                       
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
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
       RUN set-position IN h_cus-misc ( 12.67 , 58.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             BUTTON-20:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  DISPLAY FILL-IN-1 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-5 BUTTON-19 BUTTON-16 BUTTON-17 BUTTON-18 BUTTON-20 RECT-2 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportacion-por-semana W-Win 
PROCEDURE exportacion-por-semana :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*       
         FOR EACH contratos WHERE contratos.anio = 2002 and contratos.id_tipo_contrato < 100 NO-LOCK:
    find clientes of contratos no-lock no-error.
    for each items_contratos of contratos no-lock.
        find calidades of items_contratos no-lock no-error.
        find destinos where destinos.id_destino = items_contratos.id_destino no-lock no-error.
                cfila  = string(ifila).
                cRange = "A" + cfila.
                
               
                /*Formato Titulo*/
                chWorkSheet:range(crange):font:bold       = true.
                chWorkSheet:range(crange):font:size       = 10.
                chWorkSheet:range(crange):font:colorindex = 9.
    
                ifila = ifila + 1.
 
        /*>>> Datos */
            cfila  = string(ifila).
            cRange = "B" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.semana_entrega.
            cRange = "C" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.semana_entrega_hasta.
            cRange = "D" + cfila.
            chWorkSheet:Range(crange):value = general.items_contratos.anio_semana_entrega.
            if available calidades then
                do:
                    cRange = "E" + cfila.
                    chWorkSheet:Range(crange):value = calidades.descripcion.
                end.
            if available clientes then
                do:
                    cRange = "F" + cfila.
                    chWorkSheet:Range(crange):value = clientes.razon_social.
                end.
            cRange = "G" + cfila.
            chWorkSheet:Range(crange):value = contratos.id_contrato.
            cRange = "H" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.item.
            cRange = "I" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.precio_venta.
            if available destinos then
                do:
                    cRange = "J" + cfila.
                    chWorkSheet:Range(crange):value = destinos.descripcion.
                end.
            cRange = "K" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.cantidad.
    end.
  END.
*/   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rango-semana W-Win 
PROCEDURE rango-semana :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

