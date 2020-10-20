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

define buffer bstock for stock_historico_tambores.
define vari cnt_tmb as integer.
define vari vdebe  as integer.
define vari vhaber as integer.
define vari vsaldo as integer.
define vari vsaldo_t as integer.
define vari v_cantidad          as decimal.
define vari v_cantidad_400      as decimal.
define vari v_cantidad_t        as decimal.
define vari v_cantidad_400_t    as decimal.
define vari vnomcalidad         like calidades.descripcion. 

define vari vdes like bstock.id_suc_des.
define vari vori like bstock.id_suc_origen.
define vari lissuc as character initial
"73,CHESTE,74,PHOENI,75,USCOLD,76,MIDFLO,
 77,ALLENT,78,PREFER,79,GREGOR,80,LANTER,
 81,ALLTMP,83,VANBON,84,EXT.BV,85,DESPAC,
 86,HIWA  ,87,METAN ,88,COOLQU,89,SWIFF ,
 91,FACCLI,95,FAMAIL,96,LAVALL,960,PRDLAV,950,PRDFAM".
define vari vnomori as character format "x(6)".
define vari vnomdes as character format "x(6)".
define vari vnomaux as character format "x(6)".
define temp-table auxi
    field id_empresa  like bstock.id_empresa
    field id_sucursal like bstock.id_sucursal
    field id_tipotambor like bstock.id_tipotambor
    field nromov      like bstock.nromov
    field id_articulo like bstock.id_articulo
    field id_lote     like bstock.id_lote
    field anio        like bstock.anio
    field fecha       like bstock.fecha
    field cant_tmb    as integer
    field c_usuario   like bstock.c_usuario
    field cantidad    as decimal format ">>>>,>>>,>>9"
    field cantidad_400 as decimal
    field nomcalidad     like calidades.descripcion
    index ind is unique id_articulo id_lote anio.
    
define vari varticulo like bstock.id_articulo.
define vari vid_lote  like bstock.id_lote.
define vari vanio     like bstock.anio.
define vari vfecha    as date.
define vari vsucursal like bstock.id_sucursal.
define vari vlote like bstock.id_lote.
define vari vano like bstock.anio.
define vari vnomsuco like sucursales.nombre.
define vari vdetalle  as logical format "Si/No".




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
&Scoped-Define ENABLED-OBJECTS sucursal articulo lote anio fecha_hasta ~
detallado BUTTON-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS sucursal articulo lote anio fecha_hasta ~
detallado 

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
DEFINE BUTTON BUTTON-1 
     LABEL "Button 1" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE anio AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE articulo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE detallado AS LOGICAL FORMAT "SI/NO":U INITIAL NO 
     LABEL "Detallado (Si/No)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fecha_hasta AS DATE FORMAT "99/99/99":U INITIAL ? 
     LABEL "Stock a" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lote AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE sucursal AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 115 BY 50
     BGCOLOR 7 FGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     sucursal AT ROW 3.86 COL 9 COLON-ALIGNED
     articulo AT ROW 5.05 COL 9 COLON-ALIGNED
     lote AT ROW 6.24 COL 9 COLON-ALIGNED
     anio AT ROW 6.24 COL 26 COLON-ALIGNED
     fecha_hasta AT ROW 7.43 COL 9 COLON-ALIGNED
     detallado AT ROW 8.62 COL 16.6 COLON-ALIGNED
     BUTTON-1 AT ROW 8.62 COL 47
     RECT-2 AT Y 0 X 285
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
    /**************************************GENERADOR DE EXCELL **********************************/
  
      create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 15.
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
    chWorkSheet:Columns("L"):ColumnWidth = 15.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 15.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "REPORTE DE STOCK".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 22.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  
  
  chWorkSheet:Range("B6"):Value = "SERIAL".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "LOTE".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "AÑO".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "FECHA".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "MOVSTK".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "SUC".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "ORIGEN".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "SUC".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "DESTINO".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "INGRESO".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "EGRESO".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "STOCK".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "ACUMULADO".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "KG NOMINAL".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "KG 400".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "CALIDAD".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "USUARIO".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  /************ variable de fila del excell */
    
  
varticulo = INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main).
vsucursal = INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main).
vlote     = INTEGER(lote:SCREEN-VALUE IN FRAME F-Main).
vano     = INTEGER(anio:SCREEN-VALUE IN FRAME F-Main).
vfecha   = DATE(fecha_hasta:SCREEN-VALUE IN FRAME F-Main).
vdetalle = IF detallado:SCREEN-VALUE IN FRAME F-Main = "SI" THEN TRUE ELSE FALSE.

  FIND productos_terminados WHERE productos_terminados.id_articulo = varticulo
                            no-lock no-error.
  find sucursales where sucursales.id_sucursal = vsucursal 
       no-lock no-error. 
  assign vnomsuco = sucursales.nombre.

  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "FICHA DE STOCK".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 22.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.

  for each productos_terminados where productos_terminados.id_tipo_articulo = 2 
                                  AND if varticulo = 0 then true 
                                      else productos_terminados.id_articulo = varticulo
                                no-lock.
   assign varticulo = productos_terminados.id_articulo.    
   assign vsaldo = 0 v_cantidad_t = 0 v_cantidad_400_t = 0.
   for each auxi. delete auxi. end.

   for each bstock use-index serial_lote
          where bstock.id_articulo    = varticulo and
                bstock.id_suc_origen  = vsucursal and
                bstock.fecha          <= vfecha and
                if vlote <> 0 then bstock.id_lote = vlote and
                                   bstock.anio    = vano
                              else true
                no-lock
                break by bstock.id_articulo
                      by bstock.id_lote
                      by bstock.anio:
  
          assign cnt_tmb = bstock.tambor_hasta - bstock.tambor_desde + 1.
          assign v_cantidad = 0 v_cantidad_400 = 0 vnomcalidad = "" .

          run determina_datos (input bstock.id_empresa,
                               input bstock.id_sucursal,
                               input bstock.id_tipotambor,
                               input bstock.nromov,
                               input bstock.tambor_desde,
                               input bstock.tambor_hasta,
                               input cnt_tmb).

          find auxi where auxi.id_articulo = bstock.id_articulo and
                          auxi.id_lote     = bstock.id_lote and
                          auxi.anio        = bstock.anio
                          no-error.
          if not available auxi then create auxi.
          assign 
                 auxi.id_empresa   = bstock.id_empresa
                 auxi.id_sucursal  = bstock.id_sucursal
                 auxi.id_tipotambor = bstock.id_tipotambor
                 auxi.nromov       = bstock.nromov
                 auxi.id_articulo  = bstock.id_articulo 
                 auxi.id_lote      = bstock.id_lote 
                 auxi.anio         = bstock.anio
                 auxi.fecha        = bstock.fecha
                 auxi.c_usuario    = bstock.c_usuario
                 auxi.nomcalidad   = vnomcalidad.
          if bstock.signo = "+" then 
                  assign auxi.cant_tmb     = auxi.cant_tmb + cnt_tmb
                         v_cantidad_t      = v_cantidad_t + v_cantidad
                         v_cantidad_400_t  = v_cantidad_400_t + v_cantidad_400.
          if bstock.signo = "-" then 
                  assign auxi.cant_tmb     = auxi.cant_tmb - cnt_tmb
                         v_cantidad_t      = v_cantidad_t - v_cantidad
                         v_cantidad_400_t  = v_cantidad_400_t - v_cantidad_400.
          if bstock.signo = "B" then 
                  assign auxi.cant_tmb     = cnt_tmb
                         v_cantidad_t      = v_cantidad
                         v_cantidad_400_t  = v_cantidad_400.



          if vdetalle then do:               
             assign vdebe = 0 vhaber = 0.
             if bstock.signo = "+" then assign vdebe  = cnt_tmb
                                               vsaldo = vsaldo + cnt_tmb
                                               vsaldo_t = vsaldo_t + cnt_tmb.
             if bstock.signo = "-" then assign vhaber  = cnt_tmb
                                               vsaldo = vsaldo - cnt_tmb
                                               vsaldo_t = vsaldo_t - cnt_tmb.
             if bstock.signo = "B" then assign vdebe  = cnt_tmb
                                               vsaldo = cnt_tmb
                                               vsaldo_t = cnt_tmb.

             find tipos_movimientos of bstock no-lock no-error.                  
             /* Determina Abraviatura de Sucursales */
             if lookup(string(bstock.id_suc_origen),lissuc) <> 0 then 
              assign vnomori =
              entry(lookup(string(bstock.id_suc_origen),lissuc) + 1,lissuc).
             else do:
                 find sucursales where 
                      sucursales.id_sucursal = bstock.id_suc_origen
                      no-lock no-error.
                 assign vnomori = sucursales.nombre.               
             end.           
             if lookup(string(bstock.id_suc_des),lissuc) <> 0 then
              assign vnomdes =
              entry(lookup(string(bstock.id_suc_des),lissuc) + 1,lissuc).
             else do:
                 find sucursales where 
                      sucursales.id_sucursal = bstock.id_suc_des
                      no-lock no-error.
                 assign vnomdes = sucursales.nombre.               
             end.           
             if bstock.signo = "+" then assign vori = bstock.id_suc_des
                                               vdes = bstock.id_suc_origen
                                               vnomaux = vnomdes
                                               vnomdes = vnomori
                                               vnomori = vnomaux.
             if bstock.signo = "-" then assign vdes = bstock.id_suc_des
                                               vori = bstock.id_suc_origen.
             if bstock.signo = "B" then assign vdes = bstock.id_suc_origen
                                               vori = bstock.id_suc_origen
                                               vnomdes = vnomori.

             
             /* -------------- */                                              
             
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = bstock.id_serial.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = bstock.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = bstock.anio.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = bstock.fecha.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = tipos_movimiento.alta.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = vori.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = vnomori.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = vdes.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = vnomdes.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = vdebe.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = vhaber.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = vsaldo_t.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = v_cantidad.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = v_cantidad_400.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = vnomcalidad.
    cRange = "R" + cfila.
    chWorkSheet:Range(crange):value = auxi.c_usuario.
    
    ifila = ifila + 1.

             if last-of(bstock.anio) then do:
                  cfila  = string(ifila).
                  cRange = "M" + cfila.
                  chWorkSheet:Range(crange):value = "Total Lote".
                  cRange = "N" + cfila.
                  chWorkSheet:Range(crange):value = vsaldo.
                  assign vsaldo_t = 0.
                  ifila = ifila + 1.
             end.

          end.            


   end.
  
   ifila = ifila + 3.
   cfila  = string(ifila).
   cRange = "A" + cfila.
   chWorkSheet:Range(crange):value = "RESUMEN DE STOCK " + string(auxi.id_articulo) + " - " +
              upper(productos_terminados.descripcion) + " -----> Suc: " +
              vnomsuco.
   ifila = ifila + 1.

  for each auxi /* where auxi.cant_tmb <> 0 */ .
      assign v_cantidad = 0 v_cantidad_400 = 0 .
      run determina_datos (input auxi.id_empresa,
                           input auxi.id_sucursal,
                           input auxi.id_tipotambor,
                           input auxi.nromov,
                           input 1,
                           input 9999999,
                           input auxi.cant_tmb).
      assign auxi.cantidad = v_cantidad
             auxi.cantidad_400 = v_cantidad_400.                                

    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = auxi.id_articulo.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = auxi.id_lote.
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = auxi.anio.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = auxi.fecha.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = auxi.cant_tmb.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = auxi.cantidad.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = auxi.cantidad_400.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = auxi.nomcalidad.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = vnomdes.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = vdebe.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = vhaber.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = vsaldo_t.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = v_cantidad.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = v_cantidad_400.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = vnomcalidad.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = auxi.c_usuario.
    
    ifila = ifila + 1.
  end.
  end.
  


/*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/

  

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
       RUN set-position IN h_cus-misc ( 1.24 , 59.00 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_cus-misc ,
             sucursal:HANDLE IN FRAME F-Main , 'BEFORE':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE datos_extra W-Win 
PROCEDURE datos_extra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter vcalidad  like tambores_industria.id_calidad.
  define input parameter venvase   like tambores_industria.id_envase.
  define input parameter varticulo like tambores_industria.id_articulo.
  define input parameter  vcant as integer.
  define input parameter vkgtmb    like tambores_industria.kilos_tambor.


  define vari v_kilos_envase      like r_productos_calidad_envase.kilos.
  define vari v_coef              as decimal.
  define vari v_pesoref           as decimal.

  FIND r_productos_calidad_envase where
          r_productos_calidad_envase.id_articulo = varticulo and
          r_productos_calidad_envase.id_calidad = vcalidad and
          r_productos_calidad_envase.id_envase = venvase
          NO-LOCK NO-ERROR.

  IF AVAILABLE r_productos_calidad_envase 
  THEN v_kilos_envase = r_productos_calidad_envase.kilos.
  ELSE v_kilos_envase = 250.

  if vkgtmb = 0 then assign v_cantidad = vcant * v_kilos_envase.
                else assign v_cantidad = vkgtmb.

  IF varticulo = 52 OR
     varticulo = 53 THEN DO:
     FIND r_productos_calidad WHERE 
      r_productos_calidad.id_articulo = varticulo  AND        r_productos_calidad.id_calidad  = vcalidad 
     NO-LOCK NO-ERROR.  
     IF AVAILABLE r_productos_calidad 
      THEN v_coef = r_productos_calidad.coeficiente.
      ELSE v_coef = 1.

  END.
  ELSE v_coef = 1.

  IF v_coef >= 1.25 THEN v_pesoref = 260.
                    ELSE v_pesoref = 250.

  if vkgtmb = 0 then assign v_cantidad_400 = vcant * v_kilos_envase * v_coef.
                else assign v_cantidad_400 = vkgtmb * v_coef.

  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE determina_datos W-Win 
PROCEDURE determina_datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter vempresa    like bstock.id_empresa.
  define input parameter vsucursal   like bstock.id_sucursal.
  define input parameter vtipotambor like bstock.id_tipotambor.
  define input parameter vnromov     like bstock.nromov.
  define input parameter vdesde      as integer.
  define input parameter vhasta      as integer.
  define input parameter vcant       as integer.

  find movimientos_tambores where 
          movimientos_tambores.id_empresa = vempresa and
          movimientos_tambores.id_sucursal = vsucursal and
          movimientos_tambores.id_tipotambor = vtipotambor and
          movimientos_tambores.nromov_mov = vnromov
          no-lock no-error.
  if available movimientos_tambores 
      then assign vnromov = movimientos_tambores.nromov.        
  for each tambores_industria where
                  tambores_industria.id_empresa    = vempresa and
                  tambores_industria.id_sucursal   = vsucursal and
                  tambores_industria.id_tipotambor = vtipotambor and
                  tambores_industria.nromov        = vnromov and
                  tambores_industria.id_tambor    >= vdesde and
                  tambores_industria.id_tambor    <= vhasta 
                  no-lock.
                  accum tambores_industria.kilos_tambor (total).

  /*                display tambores_industria.id_lote tambores_industria.id_tambor
                  kilos_tambor
                  accum total tambores_industria.kilos_tambor. */

  end.                

  find first tambores_industria where
              tambores_industria.id_empresa    = vempresa and
              tambores_industria.id_sucursal   = vsucursal and
              tambores_industria.id_tipotambor = vtipotambor and
              tambores_industria.nromov        = vnromov
              no-lock no-error.
          if not available tambores_industria then do:
  /*            message "Atencion Inconsistencia" view-as alert-box.
              next. */
          end.
          else do:
              find calidades where 
                   calidades.id_calidad = tambores_industria.id_calidad
                   no-lock no-error.
              assign vnomcalidad = calidades.descripcion 
                                      when available calidades. 


              run datos_extra (input tambores_industria.id_calidad,
                               input tambores_industria.id_envase,
                               input tambores_industria.id_articulo,
                               input vcant,
                         input (accum total tambores_industria.kilos_tambor)).
          end.
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
  DISPLAY sucursal articulo lote anio fecha_hasta detallado 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE sucursal articulo lote anio fecha_hasta detallado BUTTON-1 RECT-2 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rep_excell W-Win 
PROCEDURE rep_excell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/**************************************GENERADOR DE EXCELL **********************************/
  
      create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1). 
  

    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 15.
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
    chWorkSheet:Columns("L"):ColumnWidth = 15.
    chWorkSheet:Columns("M"):ColumnWidth = 15.
    chWorkSheet:Columns("N"):ColumnWidth = 15.
    chWorkSheet:Columns("O"):ColumnWidth = 15.
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:Q3"):MergeCells = True.
  chWorkSheet:Range("B5:Q5"):MergeCells = True.
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "REPORTE DE STOCK".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 22.
  chWorkSheet:Range("B3"):Font:colorindex = 1.
    FIND FIRST comercial.sucursales WHERE comercial.sucursales.id_sucursal = INTEGER(sucursal:screen-value in frame F-Main)
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE comercial.sucursales THEN DO:
        chWorkSheet:Range("B5"):Value = "PLANTA " + comercial.sucursales.abreviatura.
        chWorkSheet:Range("B5"):BorderAround(1,2,1,1).
        chWorkSheet:Range("B5"):interior:colorindex = 19.
        chWorkSheet:Range("B5"):Font:colorindex = 1.
    END.
  
  /*
  chWorkSheet:Range("B6"):Value = "LOTE".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "AÑO".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "ENVASE".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "ARTICULO".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "CALIDAD".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "OF".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "CONTRATO".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "CLIENTE".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):Value = "TAMBORES".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("P6"):Value = "KILOS".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "CONV. 400".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  */
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH stock_tambores BY stock_tambores.orden_reporte
                          BY stock_tambores.id_lote.
                         /* BY stock_tambores.id_envase
                          BY stock_tambores.id_articulo
                          BY stock_tambores.id_calidad. */
    
    cfila  = string(ifila).
    cRange = "B" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_lote.
    cRange = "C" + cfila.
    chWorkSheet:Range(crange):value = "/".
    cRange = "D" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.anio_lote.
    cRange = "E" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_envase.
    cRange = "F" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.envase.
    cRange = "G" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_articulo.
    cRange = "H" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.articulo.
    cRange = "I" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_calidad.
    cRange = "J" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.calidad.
    cRange = "K" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.orden_fabricacion.
    cRange = "L" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_contrato.
    cRange = "M" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.id_cliente.
    cRange = "N" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.cliente.
    cRange = "O" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.tambores.
    cRange = "P" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.kilos.
    cRange = "Q" + cfila.
    chWorkSheet:Range(crange):value = stock_tambores.kilos_400.
    
    ifila = ifila + 1.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/


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

