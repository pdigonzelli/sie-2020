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
&Scoped-Define ENABLED-OBJECTS broker cliente BUTTON-3 BUTTON-4 BUTTON-5 ~
RECT-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-3 broker cliente texto_broker ~
texto_cliente 

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

DEFINE VARIABLE broker AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Broker" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE cliente AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Reporte OE, Packing, Contratos, Lotes" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE texto_broker AS CHARACTER FORMAT "X(30)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .95
     BGCOLOR 8 FGCOLOR 1  NO-UNDO.

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
     broker AT ROW 2.91 COL 12 COLON-ALIGNED
     cliente AT ROW 4.1 COL 12 COLON-ALIGNED
     BUTTON-3 AT ROW 6 COL 7
     BUTTON-4 AT ROW 6 COL 30
     BUTTON-5 AT ROW 7.43 COL 30
     texto_broker AT ROW 2.91 COL 25 COLON-ALIGNED NO-LABEL
     texto_cliente AT ROW 4.1 COL 25 COLON-ALIGNED NO-LABEL
     RECT-2 AT Y 110 X 265
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
/* SETTINGS FOR FILL-IN texto_broker IN FRAME F-Main
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


&Scoped-define SELF-NAME broker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL broker W-Win
ON LEAVE OF broker IN FRAME F-Main /* Broker */
DO:
  find contactos_industria where contactos_industria.id_contacto = 
                                 integer(broker:screen-value in frame F-Main) no-lock no-error.
  if available contactos_industria then
    do:
        texto_broker:screen-value in frame F-Main   = contactos_industria.nombre.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL broker W-Win
ON MOUSE-SELECT-DBLCLICK OF broker IN FRAME F-Main /* Broker */
DO:
  define var r as rowid.
  run ..\industria\wc_contactos_industria.w (output r).
  find contactos_industria where rowid(contactos_industria) = r no-lock no-error.
  if available contactos_industria then
    do:
        broker:screen-value in frame F-Main         = string(contactos_industria.id_contacto).
        texto_broker:screen-value in frame F-Main   = contactos_industria.nombre.
    end.
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
  
  v_broker = integer(broker:screen-value in frame f-Main).
  v_cliente = integer(cliente:screen-value in frame f-Main).
  
  FOR EACH oe_packing_contratos_lotes.
      DELETE oe_packing_contratos_lotes.
  END.
  FOR EACH item_oe_packing_contrato_lote.
      DELETE item_oe_packing_contrato_lote.
  END.

  RUN ..\industria\p_calculo_oe_packing_contratos_lotes.p (INPUT v_cliente, INPUT v_broker).

  RUN  aderb\_prntrb2(
  "..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
  "estado_oe_suzzy_new",                    /* RB-REPORT-NAME */
  "",                             /* RB-DB-CONNECTION */
  "O",                             /* RB-INCLUDE-RECORDS */
  "",                              /* RB-FILTER */
  RB-MEMO-FILE,                              /* RB-MEMO-FILE */
  "D",                             /* RB-PRINT-DESTINATION */
  "?",                              /* RB-PRINTER-NAME */
  "",                              /* RB-PRINTER-PORT */
  "",                              /* RB-OUTPUT-FILE */
  1,                              /* RB-NUMBER-COPIES  - zero */                  
  0,                              /* RB-BEGIN-PAGE - zero */
  0,                              /* RB-END-PAGE - zero */
  no,                              /* RB-TEST-PATTERN */
  "Reporte de Orden de Entregas",         /* RB-WINDOW-TITLE */
  yes,                           /* RB-DISPLAY-ERRORS */
  yes,                           /* RB-DISPLAY-STATUS */
  no,                              /* RB-NO-WAIT */
  "" /* RB-OTHER-PARAMETERS */,
  ""
  ).   
                           
/************************************************************************************************************/

/*
  /***********************************************************************************************************/
  /*******************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
  /***********************************************************************************************************/
                    
  v_filtro = "".
    
  if v_broker <> 0 and v_cliente = 0 then 
    v_filtro = "contratos.id_broker = " + string(v_broker).
  
  if v_broker <> 0 and v_cliente <> 0 then 
    v_filtro = "contratos.id_broker = " + string(v_broker) + 
               " and contratos.id_cliente = " + string(v_cliente).
  
  if v_broker = 0 and v_cliente = 0 then 
    v_filtro = "".
  
  if v_broker = 0 and v_cliente <> 0 then 
    v_filtro = "contratos.id_cliente = " + string(v_cliente).
   
   
                     
                          RUN  aderb\_prntrb2(
                           "..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                           "estado_oe_suzzy",                    /* RB-REPORT-NAME */
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
                           "Reporte de Orden de Entregas",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
/************************************************************************************************************/
*/
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
  
  v_broker = integer(broker:screen-value in frame f-Main).
  v_cliente = integer(cliente:screen-value in frame f-Main).
  
  FOR EACH oe_packing_contratos_lotes.
      DELETE oe_packing_contratos_lotes.
  END.
  FOR EACH item_oe_packing_contrato_lote.
      DELETE item_oe_packing_contrato_lote.
  END.

  RUN ..\industria\p_calculo_oe_packing_contratos_lotes.p (INPUT v_cliente, INPUT v_broker).

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
    chWorkSheet:Columns("C"):ColumnWidth = 25. /* COLUMNA cliente*/
    chWorkSheet:Columns("D"):ColumnWidth = 15. /* COLUMNA cliente final*/
    chWorkSheet:Columns("E"):ColumnWidth = 15. /* COLUMNA product*/
    chWorkSheet:Columns("F"):ColumnWidth = 15. /* COLUMNA contract*/
    chWorkSheet:Columns("G"):ColumnWidth = 5. /* COLUMNA part*/
    chWorkSheet:Columns("H"):ColumnWidth = 5. /* COLUMNA drums*/
    chWorkSheet:Columns("I"):ColumnWidth = 9. /* COLUMNA etd*/
    chWorkSheet:Columns("J"):ColumnWidth = 9. /* COLUMNA eta*/
    chWorkSheet:Columns("K"):ColumnWidth = 25. /* COLUMNA cia*/
    chWorkSheet:Columns("L"):ColumnWidth = 25. /* COLUMNA vapor*/
    chWorkSheet:Columns("M"):ColumnWidth = 25. /* COLUMNA DESTINATION*/
    chWorkSheet:Columns("N"):ColumnWidth = 15. /* COLUMNA CONDITION*/
    chWorkSheet:Columns("O"):ColumnWidth = 15. /* COLUMNA CONTAINER*/
    chWorkSheet:Columns("P"):ColumnWidth = 10. /* COLUMNA BATCH*/
    chWorkSheet:Columns("Q"):ColumnWidth = 17. /* COLUMNA DOCUMNENT SETTINGS*/
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "REPORTE POR SEMANA".
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  */
  
  chWorkSheet:Range("B6"):Value = "OE".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B6"):interior:colorindex = 9.
  chWorkSheet:Range("B6"):Font:colorindex = 2.
  chWorkSheet:Range("C6"):Value = "CUSTOMER".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):interior:colorindex = 9.
  chWorkSheet:Range("C6"):Font:colorindex = 2.
  chWorkSheet:Range("D6"):Value = "FINAL CUSTOMER".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):interior:colorindex = 9.
  chWorkSheet:Range("D6"):Font:colorindex = 2.
  chWorkSheet:Range("E6"):Value = "PRODUCT".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):interior:colorindex = 9.
  chWorkSheet:Range("E6"):Font:colorindex = 2.
  chWorkSheet:Range("F6"):Value = "CONTRACT".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):interior:colorindex = 9.
  chWorkSheet:Range("F6"):Font:colorindex = 2.
  chWorkSheet:Range("G6"):Value = "PART".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):interior:colorindex = 9.
  chWorkSheet:Range("G6"):Font:colorindex = 2.
  chWorkSheet:Range("H6"):Value = "DRUMS".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):interior:colorindex = 9.
  chWorkSheet:Range("H6"):Font:colorindex = 2.
  chWorkSheet:Range("I6"):Value = "ETD".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):interior:colorindex = 9.
  chWorkSheet:Range("I6"):Font:colorindex = 2.
  chWorkSheet:Range("J6"):Value = "ETA".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):interior:colorindex = 9.
  chWorkSheet:Range("J6"):Font:colorindex = 2.
  chWorkSheet:Range("K6"):Value = "CIA".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):interior:colorindex = 9.
  chWorkSheet:Range("K6"):Font:colorindex = 2.
  chWorkSheet:Range("L6"):Value = "VAPOR".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):interior:colorindex = 9.
  chWorkSheet:Range("L6"):Font:colorindex = 2.
  chWorkSheet:Range("M6"):Value = "DESTINATION".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):interior:colorindex = 9.
  chWorkSheet:Range("M6"):Font:colorindex = 2.
  chWorkSheet:Range("N6"):Value = "CONDITION".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):interior:colorindex = 9.
  chWorkSheet:Range("N6"):Font:colorindex = 2.
  chWorkSheet:Range("O6"):Value = "CONTAINER".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):interior:colorindex = 9.
  chWorkSheet:Range("O6"):Font:colorindex = 2.
  chWorkSheet:Range("P6"):Value = "BATCH".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):interior:colorindex = 9.
  chWorkSheet:Range("P6"):Font:colorindex = 2.
  chWorkSheet:Range("Q6"):Value = "DOCUMENT SETTINGS".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):interior:colorindex = 9.
  chWorkSheet:Range("Q6"):Font:colorindex = 2.

  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 9.
  cfila  = string(ifila).
  cRange = "A" + cfila.
                
               
     /*Formato Titulo*/
   chWorkSheet:range(crange):font:bold       = true.
   chWorkSheet:range(crange):font:size       = 10.
   chWorkSheet:range(crange):font:colorindex = 9.
    ifila = 10.
  FOR EACH oe_packing_contratos_lotes BY oe_packing_contratos_lotes.etd.
     
    
     
 
        /*>>> Datos */
      cfila  = string(ifila).
      cRange = "B" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.id_orden_entrega.
      cRange = "C" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.cliente.
      cRange = "D" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.cliente_final.
      cRange = "E" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.articulo.
      cRange = "F" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.id_contrato.
      cRange = "G" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.ITEM.
      cRange = "H" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.tambores.
      cRange = "I" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.etd.
      cRange = "J" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.eta.
      cRange = "K" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.agencia.
      cRange = "L" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.vapor.
      cRange = "M" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.destino.
      cRange = "N" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.estado_oe.


      FOR EACH item_oe_packing_contrato_lote NO-LOCK WHERE item_oe_packing_contrato_lote.id_orden_entrega = 
                                                   oe_packing_contratos_lotes.id_orden_entrega.
        cfila  = string(ifila).
      
        cRange = "O" + cfila.
        chWorkSheet:Range(crange):value = item_oe_packing_contrato_lote.nro_contenedor.
        cRange = "P" + cfila.
        chWorkSheet:Range(crange):value = item_oe_packing_contrato_lote.nro_lote.
        ifila = ifila + 1.
      END.

      cRange = "Q" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.documentacion.
      
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
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Para Filtrar */
DO:
  define var v_filtro as char.
  define var RB-MEMO-FILE as char.
  define var v_broker as integer.
  define var v_cliente as integer.
  
  v_broker = integer(broker:screen-value in frame f-Main).
  v_cliente = integer(cliente:screen-value in frame f-Main).
  
  FOR EACH oe_packing_contratos_lotes.
      DELETE oe_packing_contratos_lotes.
  END.
  FOR EACH item_oe_packing_contrato_lote.
      DELETE item_oe_packing_contrato_lote.
  END.

  RUN ..\industria\p_calculo_oe_packing_contratos_lotes.p (INPUT v_cliente, INPUT v_broker).

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
    chWorkSheet:Columns("C"):ColumnWidth = 7. /* COLUMNA Item OE*/
    chWorkSheet:Columns("D"):ColumnWidth = 25. /* COLUMNA cliente*/
    chWorkSheet:Columns("E"):ColumnWidth = 15. /* COLUMNA cliente final*/
    chWorkSheet:Columns("F"):ColumnWidth = 15. /* COLUMNA product*/
    chWorkSheet:Columns("G"):ColumnWidth = 15. /* COLUMNA contract*/
    chWorkSheet:Columns("H"):ColumnWidth = 5. /* COLUMNA part*/
    chWorkSheet:Columns("I"):ColumnWidth = 5. /* COLUMNA drums*/
    chWorkSheet:Columns("J"):ColumnWidth = 5. /* COLUMNA semana despacho*/
    chWorkSheet:Columns("K"):ColumnWidth = 9. /* COLUMNA etd*/
    chWorkSheet:Columns("L"):ColumnWidth = 9. /* COLUMNA eta*/
    chWorkSheet:Columns("M"):ColumnWidth = 25. /* COLUMNA cia*/
    chWorkSheet:Columns("N"):ColumnWidth = 25. /* COLUMNA vapor*/
    chWorkSheet:Columns("O"):ColumnWidth = 25. /* COLUMNA DESTINATION*/
    chWorkSheet:Columns("P"):ColumnWidth = 15. /* COLUMNA CONDITION*/
    chWorkSheet:Columns("Q"):ColumnWidth = 15. /* COLUMNA CONTAINER*/
    chWorkSheet:Columns("R"):ColumnWidth = 15. /* COLUMNA SHIPPED DRUMS */
    chWorkSheet:Columns("S"):ColumnWidth = 10. /* COLUMNA BATCH*/
    chWorkSheet:Columns("T"):ColumnWidth = 17. /* COLUMNA DOCUMNENT SETTINGS*/
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "REPORTE POR SEMANA".
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  */
  
  chWorkSheet:Range("B6:C6"):MergeCells = True.
  chWorkSheet:Range("D6:J6"):MergeCells = True.
  chWorkSheet:Range("K6:T6"):MergeCells = True.

  chWorkSheet:Range("B6"):Value = "P/L".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B6"):interior:colorindex = 19.
  chWorkSheet:Range("B6"):Font:colorindex = 1.
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):interior:colorindex = 19.
  chWorkSheet:Range("C6"):Font:colorindex = 1.
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):interior:colorindex = 31.
  chWorkSheet:Range("D6"):Font:colorindex = 2.
  chWorkSheet:Range("D6"):Value = "CONTRACTS".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):interior:colorindex = 31.
  chWorkSheet:Range("E6"):Font:colorindex = 2.
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):interior:colorindex = 31.
  chWorkSheet:Range("F6"):Font:colorindex = 2.
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):interior:colorindex = 31.
  chWorkSheet:Range("G6"):Font:colorindex = 2.
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):interior:colorindex = 31.
  chWorkSheet:Range("H6"):Font:colorindex = 2.
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):interior:colorindex = 31.
  chWorkSheet:Range("I6"):Font:colorindex = 2.
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):interior:colorindex = 31.
  chWorkSheet:Range("J6"):Font:colorindex = 2.
  chWorkSheet:Range("K6"):Value = "P/L".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):interior:colorindex = 19.
  chWorkSheet:Range("K6"):Font:colorindex = 1.
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):interior:colorindex = 19.
  chWorkSheet:Range("L6"):Font:colorindex = 2.
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):interior:colorindex = 19.
  chWorkSheet:Range("M6"):Font:colorindex = 2.
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):interior:colorindex = 19.
  chWorkSheet:Range("N6"):Font:colorindex = 2.
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):interior:colorindex = 19.
  chWorkSheet:Range("O6"):Font:colorindex = 1.
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):interior:colorindex = 19.
  chWorkSheet:Range("P6"):Font:colorindex = 2.
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):interior:colorindex = 19.
  chWorkSheet:Range("Q6"):Font:colorindex = 2.
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):interior:colorindex = 19.
  chWorkSheet:Range("R6"):Font:colorindex = 2.
  chWorkSheet:Range("S6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S6"):interior:colorindex = 19.
  chWorkSheet:Range("S6"):Font:colorindex = 2.
  chWorkSheet:Range("T6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("T6"):interior:colorindex = 19.
  chWorkSheet:Range("T6"):Font:colorindex = 2.



  chWorkSheet:Range("B7"):Value = "OE".
  chWorkSheet:Range("B7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B7"):interior:colorindex = 9.
  chWorkSheet:Range("B7"):Font:colorindex = 2.
  chWorkSheet:Range("C7"):Value = "OE PART".
  chWorkSheet:Range("C7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C7"):interior:colorindex = 9.
  chWorkSheet:Range("C7"):Font:colorindex = 2.
  chWorkSheet:Range("D7"):Value = "CUSTOMER".
  chWorkSheet:Range("D7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D7"):interior:colorindex = 9.
  chWorkSheet:Range("D7"):Font:colorindex = 2.
  chWorkSheet:Range("E7"):Value = "FINAL CUSTOMER".
  chWorkSheet:Range("E7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E7"):interior:colorindex = 9.
  chWorkSheet:Range("E7"):Font:colorindex = 2.
  chWorkSheet:Range("F7"):Value = "PRODUCT".
  chWorkSheet:Range("F7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F7"):interior:colorindex = 9.
  chWorkSheet:Range("F7"):Font:colorindex = 2.
  chWorkSheet:Range("G7"):Value = "CONTRACT".
  chWorkSheet:Range("G7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G7"):interior:colorindex = 9.
  chWorkSheet:Range("G7"):Font:colorindex = 2.
  chWorkSheet:Range("H7"):Value = "PART".
  chWorkSheet:Range("H7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H7"):interior:colorindex = 9.
  chWorkSheet:Range("H7"):Font:colorindex = 2.
  chWorkSheet:Range("I7"):Value = "DRUMS".
  chWorkSheet:Range("I7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I7"):interior:colorindex = 9.
  chWorkSheet:Range("I7"):Font:colorindex = 2.
  chWorkSheet:Range("J7"):Value = "WEEK".
  chWorkSheet:Range("J7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J7"):interior:colorindex = 9.
  chWorkSheet:Range("J7"):Font:colorindex = 2.
  chWorkSheet:Range("K7"):Value = "ETD".
  chWorkSheet:Range("K7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K7"):interior:colorindex = 9.
  chWorkSheet:Range("K7"):Font:colorindex = 2.
  chWorkSheet:Range("L7"):Value = "ETA".
  chWorkSheet:Range("L7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L7"):interior:colorindex = 9.
  chWorkSheet:Range("L7"):Font:colorindex = 2.
  chWorkSheet:Range("M7"):Value = "CIA".
  chWorkSheet:Range("M7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M7"):interior:colorindex = 9.
  chWorkSheet:Range("M7"):Font:colorindex = 2.
  chWorkSheet:Range("N7"):Value = "VAPOR".
  chWorkSheet:Range("N7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N7"):interior:colorindex = 9.
  chWorkSheet:Range("N7"):Font:colorindex = 2.
  chWorkSheet:Range("O7"):Value = "DESTINATION".
  chWorkSheet:Range("O7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O7"):interior:colorindex = 9.
  chWorkSheet:Range("O7"):Font:colorindex = 2.
  chWorkSheet:Range("P7"):Value = "CONDITION".
  chWorkSheet:Range("P7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P7"):interior:colorindex = 9.
  chWorkSheet:Range("P7"):Font:colorindex = 2.
  chWorkSheet:Range("Q7"):Value = "CONTAINER".
  chWorkSheet:Range("Q7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q7"):interior:colorindex = 9.
  chWorkSheet:Range("Q7"):Font:colorindex = 2.
  chWorkSheet:Range("R7"):Value = "SHIPPED DRUMS".
  chWorkSheet:Range("R7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R7"):interior:colorindex = 9.
  chWorkSheet:Range("R7"):Font:colorindex = 2.
  chWorkSheet:Range("S7"):Value = "BATCH".
  chWorkSheet:Range("S7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S7"):interior:colorindex = 9.
  chWorkSheet:Range("S7"):Font:colorindex = 2.
  chWorkSheet:Range("T7"):Value = "DOCUMENT SETTINGS".
  chWorkSheet:Range("T7"):BorderAround(1,2,1,1).
  chWorkSheet:Range("T7"):interior:colorindex = 9.
  chWorkSheet:Range("T7"):Font:colorindex = 2.

  /*-- FIN TITULOS GENERALES Y PARTICULARES --*/


  
  
/*------------------------------------- PROCESO PRINCIPAL ---------------------------------------*/
/*  ifila = 9.
  cfila  = string(ifila).
  cRange = "A" + cfila.
                
               
     /*Formato Titulo*/
   chWorkSheet:range(crange):font:bold       = true.
   chWorkSheet:range(crange):font:size       = 10.
   chWorkSheet:range(crange):font:colorindex = 9.
*/
  ifila = 8.
  FOR EACH oe_packing_contratos_lotes BY oe_packing_contratos_lotes.etd.
     
    
     
 
  /*>>> Datos */
  /*  cfila  = string(ifila).*/
      FIND FIRST item_oe_packing_contrato_lote WHERE item_oe_packing_contrato_lote.id_orden_entrega = 
                                                     oe_packing_contratos_lotes.id_orden_entrega NO-LOCK NO-ERROR.
      IF NOT AVAILABLE item_oe_packing_contrato_lote THEN
      DO:
          cfila  = string(ifila).
          cRange = "B" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.id_orden_entrega.
          cRange = "C" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.item_oe.
          cRange = "D" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.cliente.
          cRange = "E" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.cliente_final.
          cRange = "F" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.articulo.
          cRange = "G" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.id_contrato.
          cRange = "H" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.ITEM.
          cRange = "I" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.tambores.
          cRange = "J" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.semana_desde.
          cRange = "K" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.etd.
          cRange = "L" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.eta.
          cRange = "M" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.agencia.
          cRange = "N" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.vapor.
          cRange = "O" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.destino.
          cRange = "P" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.estado_oe.
          ifila = ifila + 1.
      END.

      FOR EACH item_oe_packing_contrato_lote NO-LOCK WHERE item_oe_packing_contrato_lote.id_orden_entrega = 
                                                           oe_packing_contratos_lotes.id_orden_entrega.
        cfila  = string(ifila).
      
      cRange = "B" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.id_orden_entrega.
          cRange = "C" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.item_oe.
          cRange = "D" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.cliente.
          cRange = "E" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.cliente_final.
          cRange = "F" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.articulo.
          cRange = "G" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.id_contrato.
          cRange = "H" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.ITEM.
          cRange = "I" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.tambores.
          cRange = "J" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.semana_desde.
          cRange = "K" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.etd.
          cRange = "L" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.eta.
          cRange = "M" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.agencia.
          cRange = "N" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.vapor.
          cRange = "O" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.destino.
          cRange = "P" + cfila.
          chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.estado_oe.

      cRange = "Q" + cfila.
      chWorkSheet:Range(crange):value = item_oe_packing_contrato_lote.nro_contenedor.
      cRange = "R" + cfila.
      chWorkSheet:Range(crange):value = item_oe_packing_contrato_lote.tambores_despachados.
      cRange = "S" + cfila.
      chWorkSheet:Range(crange):value = item_oe_packing_contrato_lote.nro_lote.
       
      cRange = "T" + cfila.
      chWorkSheet:Range(crange):value = oe_packing_contratos_lotes.documentacion.

        ifila = ifila + 1.
      END.

      
      
      /* ifila = ifila + 1.*/
  END.
  


                       
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.

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
       RUN set-position IN h_cus-misc ( 6.48 , 55.00 ) NO-ERROR.
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
  DISPLAY FILL-IN-3 broker cliente texto_broker texto_cliente 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE broker cliente BUTTON-3 BUTTON-4 BUTTON-5 RECT-2 
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

