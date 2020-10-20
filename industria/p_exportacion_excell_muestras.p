/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*-- VARIABLES DE EXCEL --*/
    DEFINE INPUT PARAMETER ip_fechaini AS DATE.

    define var chExcelAplication as com-handle.
    define var chWorkbook        as com-handle.
    define var chWorkSheet       as com-handle.
    define var chchart           as com-handle.
    define var chWorkSheetRange  as com-handle.
  
    define var ifila  as integer.
    define var cfila  as character.
    define var crange as character.

 /*-- FIN VARIABLES DE EXCEL --*/

DEFINE BUFFER bb_solicitante FOR contactos_muestras.
DEFINE VAR v_protocolos AS CHAR.
DEFINE VAR v_lotes AS CHAR.
DEFINE BUFFER bb_couriers FOR couriers.

/*** SI NO SE ESPECIFICÓ UNA FECHA LE DAREMOS EL PRIMER DÍA DEL AÑO EN CURSO ***/
IF ip_fechaini = ? THEN
  ASSIGN ip_fechaini = DATE(01,01,YEAR(TODAY)).

/**************************************GENERADOR DE EXCELL **********************************/
  
    CREATE "Excel.Application" chExcelAplication.
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
    chWorkSheet:Columns("C"):ColumnWidth = 1.
    chWorkSheet:Columns("D"):ColumnWidth = 5.
    chWorkSheet:Columns("E"):ColumnWidth = 5.
    chWorkSheet:Columns("F"):ColumnWidth = 15.
    chWorkSheet:Columns("G"):ColumnWidth = 5.
    chWorkSheet:Columns("H"):ColumnWidth = 15.
    chWorkSheet:Columns("I"):ColumnWidth = 5.
    chWorkSheet:Columns("J"):ColumnWidth = 15.
    chWorkSheet:Columns("K"):ColumnWidth = 5.
    chWorkSheet:Columns("L"):ColumnWidth = 15.
    chWorkSheet:Columns("M"):ColumnWidth = 5.
    chWorkSheet:Columns("N"):ColumnWidth = 25.
    chWorkSheet:Columns("O"):ColumnWidth = 20.
    chWorkSheet:Columns("P"):ColumnWidth = 15.
    chWorkSheet:Columns("Q"):ColumnWidth = 15.
    chWorkSheet:Columns("T"):ColumnWidth = 15.
    chWorkSheet:Columns("U"):ColumnWidth = 15.
    chWorkSheet:Columns("V"):ColumnWidth = 15.
    /*chWorkSheet:Columns("Q"):NumberFormat = " ###,###,##0.00". */
    
 /*-- FIN CONFIGURACION INICIAL --*/
 
 /* chWorkSheet:Range("A5:S5"):HorizontalAlignment = 3. /*CENTERED*/ */
  chWorkSheet:Range("B3:S3"):MergeCells = True.
  /* chWorkSheet:Range("B5:Q5"):MergeCells = True. */
  chWorkSheet:Range("N5:P5"):MergeCells = True. /* AGRUPAR LOS DATOS DE TUCUMAN */
  chWorkSheet:Range("Q5:S5"):MergeCells = True. /* AGRUPAR LOS DATOS DE BSAS */
  
  /* TITULO DE LA PLANILLA */

  chWorkSheet:Range("B3"):Value = "PEDIDO DE MUESTRAS".
  chWorkSheet:Range("B3"):BorderAround(1,2,1,1).
  chWorkSheet:Range("B3"):interior:colorindex = 22.
  chWorkSheet:Range("B3"):Font:colorindex = 1.

  chWorkSheet:Range("N5"):Value = "TUCUMAN".
  chWorkSheet:Range("N5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N5"):interior:colorindex = 31.
  chWorkSheet:Range("N5"):Font:colorindex = 1.

  chWorkSheet:Range("Q5"):Value = "BUENOS AIRES".
  chWorkSheet:Range("Q5"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q5"):interior:colorindex = 53.
  chWorkSheet:Range("Q5"):Font:colorindex = 1.
   
  chWorkSheet:Range("B6"):Value = "#".
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("C6"):Value = "AÑO".
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("D6"):Value = "FECHA".
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("E6"):Value = "PRIORIDAD".
  chWorkSheet:Range("E6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("F6"):Value = "ENVIAR A".
  chWorkSheet:Range("F6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("G6"):Value = "PRODUCTO".
  chWorkSheet:Range("G6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("H6"):Value = "SOLICITADO POR".
  chWorkSheet:Range("H6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("I6"):Value = "DIRECTO".
  chWorkSheet:Range("I6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("J6"):Value = "CARACTERISTICAS".
  chWorkSheet:Range("J6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("K6"):Value = "ENVASE".
  chWorkSheet:Range("K6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("L6"):Value = "CANTIDAD".
  chWorkSheet:Range("L6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("M6"):Value = "PROTOCOLOS".
  chWorkSheet:Range("M6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "LOTES".
  chWorkSheet:Range("N6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("N6"):Value = "LOTES".
  chWorkSheet:Range("O6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("O6"):VALUE = "LOTES SAP".
  chWorkSheet:Range("P6"):Value = "ENVIADO".
  chWorkSheet:Range("P6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("Q6"):Value = "COURIER".
  chWorkSheet:Range("Q6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("R6"):Value = "NRO GUIA".
  chWorkSheet:Range("R6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("S6"):Value = "ENVIADO".
  chWorkSheet:Range("S6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("T6"):Value = "COURIER".
  chWorkSheet:Range("T6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("U6"):Value = "NRO GUIA".
  chWorkSheet:Range("U6"):BorderAround(1,2,1,1).
  chWorkSheet:Range("V6"):Value = "ESTADO L.ENVIADO".
  chWorkSheet:Range("V6"):BorderAround(1,2,1,1).
  
    
  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------*/
  ifila = 7.
  FOR EACH muestras WHERE muestras.fecha >= ip_fechaini
                      /*-- DATE("01/01/08")  --*/
                      AND muestras.fecha <= TODAY
                      /*-- DATE("01/01/10") este código reemplazdo el 29-06-10 --*/
                      BY muestras.fecha.
    FIND FIRST prioridades OF muestras NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados  OF muestras NO-LOCK NO-ERROR.
    FIND FIRST contactos_muestras    WHERE contactos_muestras.id_contacto = muestras.id_destinatario NO-LOCK NO-ERROR.
    FIND FIRST bb_solicitante        WHERE bb_solicitante.id_contacto = muestras.id_solicitante NO-LOCK NO-ERROR.
    
    FOR EACH items_muestras OF muestras
                            WHERE items_muestras.item_muestra > 0
                            NO-LOCK.
        v_protocolos = "".
        v_lotes = "".
        FIND FIRST envases_muestras OF items_muestras NO-LOCK NO-ERROR.
        FIND FIRST couriers         WHERE couriers.id_courier = items_muestras.id_courier_tuc NO-LOCK NO-ERROR.
        FIND FIRST bb_couriers      WHERE bb_couriers.id_courier = items_muestras.id_courier_bue NO-LOCK NO-ERROR.
        FOR EACH r_muestras_protocolos WHERE r_muestras_protocolos.id_muestra   = items_muestras.id_muestra
                                         AND r_muestras_protocolos.anio_muestra = items_muestras.anio_muestra
                                         AND r_muestras_protocolos.item_muestra = items_muestras.item_muestra
                                        NO-LOCK.
            v_protocolos = v_protocolos + STRING(r_muestras_protocolos.id_protocolo,"9999") + "/" +
                                          STRING(r_muestras_protocolos.anio,"99") + " ".
            FOR EACH protocolos WHERE protocolos.id_protocolo = r_muestras_protocolos.id_protocolo
                                  AND protocolos.anio         = r_muestras_protocolos.anio
                                  AND protocolos.id_articulo  = r_muestras_protocolos.id_articulo
                                  NO-LOCK . 
                FIND FIRST tambores_industria WHERE tambores_industria.id_empresa = protocolos.id_empresa 
                                                AND tambores_industria.id_sucursal = protocolos.id_sucursal
                                                AND tambores_industria.id_tipotambor = protocolos.id_tipotambor
                                                AND tambores_industria.nromov = protocolos.nromov
                                                NO-LOCK NO-ERROR.
                IF AVAILABLE tambores_industria THEN DO:
                    v_lotes = v_lotes + STRING(tambores_industria.id_lote,"9999") + "/" + 
                                        STRING(SUBSTRING(STRING(tambores_industria.anio),3,2),"99") + " ".
                END.
            END.
        END.

        FIND estados_muestra WHERE estados_muestra.id_estado = items_muestras.id_estado_lote_enviado NO-LOCK NO-ERROR.
        cfila  = STRING(ifila).
        cRange = "B" + cfila.
        chWorkSheet:Range(crange):VALUE = muestras.id_muestra.
        cRange = "C" + cfila.
        chWorkSheet:Range(crange):VALUE = muestras.anio_muestra.
        cRange = "D" + cfila.
        chWorkSheet:Range(crange):VALUE = muestras.fecha.

        cRange = "E" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE prioridades THEN prioridades.descripcion ELSE "NONE".

        cRange = "F" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE contactos_muestras THEN contactos_muestras.nombre ELSE "NONE".
        cRange = "G" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion_ingles ELSE "NONE".
        cRange = "H" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE bb_solicitante THEN bb_solicitante.nombre ELSE "NONE".
        cRange = "I" + cfila.
        chWorkSheet:Range(crange):VALUE = IF muestras.directo_cliente THEN "SI" ELSE "NO".
        cRange = "J" + cfila.
        chWorkSheet:Range(crange):VALUE = items_muestras.caracteristicas.
        cRange = "K" + cfila.
        chWorkSheet:Range(crange):VALUE = IF AVAILABLE envases_muestras THEN envases_muestras.descripcion ELSE "NONE".
        cRange = "L" + cfila.
        chWorkSheet:Range(crange):VALUE = items_muestras.cantidad.
        cRange = "M" + cfila.
        chWorkSheet:Range(crange):VALUE = v_protocolos.
        cRange = "N" + cfila.
        chWorkSheet:Range(crange):value = v_lotes.
        cRange = "O" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.lote_sap.
        cRange = "P" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.fecha_enviado_tuc.
        cRange = "Q" + cfila.
        chWorkSheet:Range(crange):value = IF AVAILABLE couriers THEN couriers.descripcion ELSE "NONE".
        cRange = "R" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.nro_guia_tuc.
        cRange = "S" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.fecha_enviado_bue.
        cRange = "T" + cfila.
        chWorkSheet:Range(crange):value = IF AVAILABLE bb_couriers THEN bb_couriers.descripcion ELSE "NONE".
        cRange = "U" + cfila.
        chWorkSheet:Range(crange):value = items_muestras.nro_guia_bue.
        cRange = "V" + cfila.
        chWorkSheet:Range(crange):value = IF AVAILABLE estados_muestra THEN estados_muestra.descripcion ELSE "Sin info estado".
        
        ifila = ifila + 1.
    END.
  END.
  
  /*----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  
    
    /*****************************************************************************************/
