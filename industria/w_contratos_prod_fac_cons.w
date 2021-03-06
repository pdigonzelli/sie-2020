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
&Scoped-Define ENABLED-OBJECTS fecha cliente BUTTON-3 BUTTON-4 BUTTON-5 ~
RECT-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-3 fecha cliente texto_cliente 

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
DEFINE BUTTON BUTTON-3 
     LABEL "Imprimir Reporte" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-4 
     LABEL "Exportar Excell" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "Para Filtrar" 
     SIZE 21 BY 1.14.

DEFINE VARIABLE cliente AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fecha AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Fecha Inicio" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Reporte Gerencial de Industria" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE texto_cliente AS CHARACTER FORMAT "X(30)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .95
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 115 BY 50
     BGCOLOR 7 FGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-3 AT ROW 1.48 COL 3 COLON-ALIGNED NO-LABEL
     fecha AT ROW 3.62 COL 12 COLON-ALIGNED
     cliente AT ROW 4.81 COL 12 COLON-ALIGNED
     BUTTON-3 AT ROW 6.71 COL 7
     BUTTON-4 AT ROW 6.71 COL 30
     BUTTON-5 AT ROW 8.14 COL 30
     texto_cliente AT ROW 4.81 COL 25 COLON-ALIGNED NO-LABEL
     RECT-2 AT Y 125 X 265
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 9.67.


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
         TITLE              = "Reporte"
         HEIGHT             = 9.76
         WIDTH              = 80.4
         MAX-HEIGHT         = 19.62
         MAX-WIDTH          = 107
         VIRTUAL-HEIGHT     = 19.62
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
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN texto_cliente IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Reporte */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Reporte */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Imprimir Reporte */
DO:
  define var v_filtro as char.
  define var RB-MEMO-FILE as char.
  define var v_broker as integer.
  define var v_cliente as integer.
  
  v_cliente = integer(cliente:screen-value in frame f-Main).
  
  IF date(fecha:SCREEN-VALUE IN FRAME F-Main) = ? THEN MESSAGE "Debe ingresar una fecha valida." VIEW-AS ALERT-BOX.
  ELSE
  DO:
      RUN ..\industria\p_calculo_estado_general_contratos.p (INPUT v_cliente, 
                                                             INPUT DATE(fecha:SCREEN-VALUE IN FRAME F-Main)).
      run ..\industria\p_reportes_9.p (input "contratos_prod_fac_cons_4",
                                       input "CONTRATOS",
                                       input "",
                                       input "").
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Exportar Excell */
DO:
  define var v_filtro as char.
  define var RB-MEMO-FILE as char.
  define var v_broker as integer.
  define var v_cliente as integer.
  
  v_cliente = integer(cliente:screen-value in frame f-Main).
  
  IF date(fecha:SCREEN-VALUE IN FRAME F-Main) = ? THEN MESSAGE "Debe ingresar una fecha valida." VIEW-AS ALERT-BOX.
  ELSE
  DO:
      RUN ..\industria\p_calculo_estado_general_contratos.p (INPUT v_cliente, 
                                                             INPUT DATE(fecha:SCREEN-VALUE IN FRAME F-Main)).
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
    chWorkSheet:Columns("B"):ColumnWidth = 5. /* COLUMNA OE*/
    chWorkSheet:Columns("C"):ColumnWidth = 15. /* COLUMNA contrato*/
    chWorkSheet:Columns("D"):ColumnWidth = 5. /* COLUMNA parte*/
    chWorkSheet:Columns("E"):ColumnWidth = 5. /* COLUMNA semana*/
    chWorkSheet:Columns("F"):ColumnWidth = 5. /* COLUMNA a�o*/
    chWorkSheet:Columns("G"):ColumnWidth = 5. /* COLUMNA tam contrato*/
    chWorkSheet:Columns("H"):ColumnWidth = 5. /* COLUMNA tam prod*/
    chWorkSheet:Columns("I"):ColumnWidth = 15. /* COLUMNA kilos prod*/
    chWorkSheet:Columns("J"):ColumnWidth = 5. /* COLUMNA tam desp*/
    chWorkSheet:Columns("K"):ColumnWidth = 15. /* COLUMNA kilos desp*/
    chWorkSheet:Columns("L"):ColumnWidth = 5. /* COLUMNA tam stock*/
    chWorkSheet:Columns("M"):ColumnWidth = 15. /* COLUMNA kilos stock*/
    chWorkSheet:Columns("N"):ColumnWidth = 15. /* COLUMNA importe contrato*/
    chWorkSheet:Columns("O"):ColumnWidth = 15. /* COLUMNA importe factura*/
    chWorkSheet:Columns("P"):ColumnWidth = 15. /* COLUMNA por facturar*/
    chWorkSheet:Columns("Q"):ColumnWidth = 5. /* COLUMNA tam fac*/
    chWorkSheet:Columns("R"):ColumnWidth = 20. /* COLUMNA factura*/
    chWorkSheet:Columns("S"):ColumnWidth = 20. /* COLUMNA packing list*/
    chWorkSheet:Columns("T"):ColumnWidth = 15. /* COLUMNA importe cobrado*/
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "REPORTE POR SEMANA".
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  */
  
  
  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 9.
  cfila  = string(ifila).
  cRange = "A" + cfila.
                
               
     /*Formato Titulo*/
   chWorkSheet:range(crange):font:bold       = true.
   chWorkSheet:range(crange):font:size       = 10.
   chWorkSheet:range(crange):font:colorindex = 9.
    ifila = 6.
  FOR EACH contratos_prod_fac_cons BREAK BY contratos_prod_fac_cons.id_cliente
                                         BY contratos_prod_fac_cons.id_contrato
                                         BY contratos_prod_fac_cons.ITEM.
     
    IF FIRST-OF(contratos_prod_fac_cons.id_cliente) THEN
    DO:
          cfila  = string(ifila).
          cRange = "D" + cfila.
          chWorkSheet:Range(cRange):Value = contratos_prod_fac_cons.cliente.
          chWorkSheet:range(crange):font:bold       = true.
          chWorkSheet:range(crange):font:size       = 12.
          chWorkSheet:range(crange):font:colorindex = 9.    
          ifila = ifila + 2.
          
          cfila  = string(ifila).
          cRange = "B" + cfila.
          chWorkSheet:Range(cRange):Value = "OE".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "C" + cfila.
          chWorkSheet:Range(cRange):Value = "CONTRATO".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "D" + cfila.
          chWorkSheet:Range(cRange):Value = "PARTE".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "E" + cfila.
          chWorkSheet:Range(cRange):Value = "SEMANA".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "F" + cfila.
          chWorkSheet:Range(cRange):Value = "A�O".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "G" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM CONTRATO".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "H" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM PROD".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "I" + cfila.
          chWorkSheet:Range(cRange):Value = "KILOS PROD".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "J" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM DESP".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "K" + cfila.
          chWorkSheet:Range(cRange):Value = "KILOS DESP".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "L" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM STOCK".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "M" + cfila.
          chWorkSheet:Range(cRange):Value = "KILOS STOCK".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "N" + cfila.
          chWorkSheet:Range(cRange):Value = "IMPORTE CONTRATO".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "O" + cfila.
          chWorkSheet:Range(cRange):Value = "IMPORTE FAC".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "P" + cfila.
          chWorkSheet:Range(cRange):Value = "POR FACTURAR".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "Q" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM FAC".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "R" + cfila.
          chWorkSheet:Range(cRange):Value = "FACTURA".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "S" + cfila.
          chWorkSheet:Range(cRange):Value = "PACKING LIST".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "T" + cfila.
          chWorkSheet:Range(cRange):Value = "".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          ifila = ifila + 1.
    END.
        /*>>> Datos */
      cfila  = string(ifila).
      cRange = "B" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.id_orden_entrega.
      cRange = "C" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.id_contrato.
      cRange = "D" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.ITEM.
      cRange = "E" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.semana_envio_desde.
      cRange = "F" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.anio_envio.
      cRange = "G" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_contrato.
      cRange = "H" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_prod.
      cRange = "I" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.kilos_tambores_prod.
      cRange = "J" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_desp.
      cRange = "K" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.kilos_tambores_desp.
      cRange = "L" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_stock.
      cRange = "M" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.kilos_tambores_stock.
      cRange = "N" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.importe_contrato.
      cRange = "O" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.importe_factura.
      cRange = "P" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.importe_factura - contratos_prod_fac_cons.importe_cobranza.
      cRange = "Q" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_factura.
      cRange = "R" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.factura.
      cRange = "S" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.nro_pack_list.
      cRange = "T" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.importe_cobranza.
      
       ifila = ifila + 1.

       IF LAST-OF(contratos_prod_fac_cons.id_cliente) THEN
        DO:
          ifila = ifila + 1.
       END.
  END.
  


                       
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Para Filtrar */
DO:
  define var v_filtro as char.
  define var RB-MEMO-FILE as char.
  define var v_broker as integer.
  define var v_cliente as integer.
  
  v_cliente = integer(cliente:screen-value in frame f-Main).
  
  IF date(fecha:SCREEN-VALUE IN FRAME F-Main) = ? THEN MESSAGE "Debe ingresar una fecha valida." VIEW-AS ALERT-BOX.
  ELSE
  DO:
      RUN ..\industria\p_calculo_estado_general_contratos.p (INPUT v_cliente, 
                                                             INPUT DATE(fecha:SCREEN-VALUE IN FRAME F-Main)).
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
    chWorkSheet:Columns("B"):ColumnWidth = 10. /* COLUMNA COD CLIENTE*/
    chWorkSheet:Columns("C"):ColumnWidth = 25. /* COLUMNA CLIENTE*/
    chWorkSheet:Columns("D"):ColumnWidth = 5. /* COLUMNA OE*/
    chWorkSheet:Columns("E"):ColumnWidth = 15. /* COLUMNA contrato*/
    chWorkSheet:Columns("F"):ColumnWidth = 5. /* COLUMNA parte*/
    chWorkSheet:Columns("G"):ColumnWidth = 5. /* COLUMNA semana*/
    chWorkSheet:Columns("H"):ColumnWidth = 5. /* COLUMNA a�o*/
    chWorkSheet:Columns("I"):ColumnWidth = 5. /* COLUMNA tam contrato*/
    chWorkSheet:Columns("J"):ColumnWidth = 5. /* COLUMNA tam prod*/
    chWorkSheet:Columns("K"):ColumnWidth = 15. /* COLUMNA kilos prod*/
    chWorkSheet:Columns("L"):ColumnWidth = 5. /* COLUMNA tam desp*/
    chWorkSheet:Columns("M"):ColumnWidth = 15. /* COLUMNA kilos desp*/
    chWorkSheet:Columns("N"):ColumnWidth = 5. /* COLUMNA tam stock*/
    chWorkSheet:Columns("O"):ColumnWidth = 15. /* COLUMNA kilos stock*/
    chWorkSheet:Columns("P"):ColumnWidth = 15. /* COLUMNA importe contrato*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15. /* COLUMNA importe factura*/
    chWorkSheet:Columns("R"):ColumnWidth = 15. /* COLUMNA por facturar*/
    chWorkSheet:Columns("S"):ColumnWidth = 5. /* COLUMNA tam fac*/
    chWorkSheet:Columns("T"):ColumnWidth = 20. /* COLUMNA factura*/
    chWorkSheet:Columns("U"):ColumnWidth = 20. /* COLUMNA packing list*/
    chWorkSheet:Columns("V"):ColumnWidth = 15. /* COLUMNA importe cobrado*/
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "REPORTE POR SEMANA".
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  */
  
  
  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 3.
  cfila  = string(ifila).
  cRange = "A" + cfila.
                
               
     /*Formato Titulo*/
   chWorkSheet:range(crange):font:bold       = true.
   chWorkSheet:range(crange):font:size       = 10.
   chWorkSheet:range(crange):font:colorindex = 9.
   chWorkSheet:Range(cRange):Value = "REPORTE GERENCIAL DE INDUSTRIA".
          
          ifila = 6.
          cfila  = string(ifila).
          cRange = "B" + cfila.
          chWorkSheet:Range(cRange):Value = "COD CLIENTE".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "C" + cfila.
          chWorkSheet:Range(cRange):Value = "CLIENTE".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "D" + cfila.
          chWorkSheet:Range(cRange):Value = "OE".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "E" + cfila.
          chWorkSheet:Range(cRange):Value = "CONTRATO".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "F" + cfila.
          chWorkSheet:Range(cRange):Value = "PARTE".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "G" + cfila.
          chWorkSheet:Range(cRange):Value = "SEMANA".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "H" + cfila.
          chWorkSheet:Range(cRange):Value = "A�O".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "I" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM CONTRATO".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "J" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM PROD".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "K" + cfila.
          chWorkSheet:Range(cRange):Value = "KILOS PROD".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "L" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM DESP".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "M" + cfila.
          chWorkSheet:Range(cRange):Value = "KILOS DESP".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "N" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM STOCK".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "O" + cfila.
          chWorkSheet:Range(cRange):Value = "KILOS STOCK".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "P" + cfila.
          chWorkSheet:Range(cRange):Value = "IMPORTE CONTRATO".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "Q" + cfila.
          chWorkSheet:Range(cRange):Value = "IMPORTE FAC".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "R" + cfila.
          chWorkSheet:Range(cRange):Value = "POR FACTURAR".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "S" + cfila.
          chWorkSheet:Range(cRange):Value = "TAM FAC".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "T" + cfila.
          chWorkSheet:Range(cRange):Value = "FACTURA".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "U" + cfila.
          chWorkSheet:Range(cRange):Value = "PACKING LIST".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          cRange = "V" + cfila.
          chWorkSheet:Range(cRange):Value = "".
          chWorkSheet:Range(cRange):BorderAround(1,2,1,1).
          chWorkSheet:Range(cRange):interior:colorindex = 9.
          chWorkSheet:Range(cRange):Font:colorindex = 2.
          ifila = ifila + 1.

  FOR EACH contratos_prod_fac_cons BREAK BY contratos_prod_fac_cons.id_cliente
                                         BY contratos_prod_fac_cons.id_contrato
                                         BY contratos_prod_fac_cons.ITEM.
          
        /*>>> Datos */
      cfila  = string(ifila).
      cRange = "B" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.id_cliente.
      cRange = "C" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.cliente.
      cRange = "D" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.id_orden_entrega.
      cRange = "E" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.id_contrato.
      cRange = "F" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.ITEM.
      cRange = "G" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.semana_envio_desde.
      cRange = "H" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.anio_envio.
      cRange = "I" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_contrato.
      cRange = "J" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_prod.
      cRange = "K" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.kilos_tambores_prod.
      cRange = "L" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_desp.
      cRange = "M" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.kilos_tambores_desp.
      cRange = "N" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_stock.
      cRange = "O" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.kilos_tambores_stock.
      cRange = "P" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.importe_contrato.
      cRange = "Q" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.importe_factura.
      cRange = "R" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.importe_factura - contratos_prod_fac_cons.importe_cobranza.
      cRange = "S" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.tambores_factura.
      cRange = "T" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.factura.
      cRange = "U" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.nro_pack_list.
      cRange = "V" + cfila.
      chWorkSheet:Range(crange):value = contratos_prod_fac_cons.importe_cobranza.
      
       ifila = ifila + 1.
       
  END.
  


                       
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cliente W-Win
ON LEAVE OF cliente IN FRAME F-Main /* Cliente */
DO:
  find clientes where clientes.id_cliente = 
                      integer(cliente:screen-value in frame F-Main) no-lock no-error.
  if available clientes then
    do:
        texto_cliente:screen-value in frame F-Main   = clientes.nombre.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cliente W-Win
ON MOUSE-SELECT-DBLCLICK OF cliente IN FRAME F-Main /* Cliente */
DO:
  define var r as rowid.
  run ..\industria\wc_clientes.w (output r).
  find clientes where rowid(clientes) = r no-lock no-error.
  if available clientes then
    do:
        cliente:screen-value in frame F-Main         = string(clientes.id_cliente).
        texto_cliente:screen-value in frame F-Main   = clientes.nombre.
    end.
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
       RUN set-position IN h_cus-misc ( 7.19 , 55.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             BUTTON-4:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  DISPLAY FILL-IN-3 fecha cliente texto_cliente 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fecha cliente BUTTON-3 BUTTON-4 BUTTON-5 RECT-2 
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

