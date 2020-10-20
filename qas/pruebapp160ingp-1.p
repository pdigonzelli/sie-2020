
/*------------------------------------------------------------------------
    File        : pp159ing.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Feb 20 20:20:51 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER NO-UNDO.

  
  FUNCTION WLOG RETURNS LOGICAL
    ( INPUT ARCHIVO AS CHARACTER, INPUT TEXTO AS CHARACTER)  FORWARD.
  
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.
  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.
  DEFINE VAR par AS INTEGER INITIAL 0.
  DEFINE VARIABLE   cFecha          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cParametros     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   hPalletSap      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   xcentro         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   iEstado         AS INTEGER NO-UNDO.
  DEFINE VARIABLE   cPalletSap      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xunidProd       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   XMERCADO        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   XPACKING        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   XTURNO          AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE   ICAN            AS INTEGER  NO-UNDO.
  DEFINE VARIABLE   ARCHIVO         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   ARCHIVOL        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   ARCHIVO1        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   OPERACION       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   CUENTA          AS INTEGER NO-UNDO.
  DEFINE VARIABLE   DIR             AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   RES             AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   XALMACENMTS     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   CONTADOR        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE   ICANT           AS INTEGER NO-UNDO.
  DEFINE VARIABLE   ILOTE           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   XOMTS           AS CHARACTER NO-UNDO.



    DEFINE TEMP-TABLE TTCAJAS
        FIELD ID_EMPRESA LIKE cajas.id_empresa
        FIELD ID_PUNTO_EMISOR LIKE cajas.id_punto_emisor
        FIELD ID_ORDEN LIKE cajas.id_orden
        FIELD ITEM LIKE CAJAS.ITEM
        FIELD ID_PACKING LIKE cajas.id_packing
        FIELD CODIGO_TRAZABILIDAD LIKE cajas.codigo_trazabilidad
        FIELD LETRA_COLOR LIKE cajas.letra_color
        FIELD CALIBRE LIKE CAJAS.CALIBRE  
        FIELD OMTS AS CHARACTER
        FIELD ID_COLOR LIKE colores.ID_COLOR
        FIELD BULTOS AS INTEGER
        FIELD POSICION AS INTEGER
        FIELD BULTOS_TOTALES AS INTEGER.


    DEFINE TEMP-TABLE TTCAJAS1 LIKE CAJAS
        FIELD POSICION AS INTEGER.


FUNCTION CALIBRE RETURNS CHARACTER (CCALIBRE AS CHARACTER):
    DEFINE VAR CAL AS CHARACTER NO-UNDO.
    IF INDEX(CCALIBRE, '/') <> 0 THEN
         CAL = SUBSTRING(CCALIBRE,1,INDEX(CCALIBRE, '/') - 1).
    ELSE 
         CAL = SUBSTRING(CCALIBRE,1,LENGTH(CCALIBRE) - 1).
    RETURN CAL.    
    
END.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



{ttpallet160.i}


FILE-INFO:FILE-NAME = ".".
DIR = FILE-INFO:FULL-PATHNAME.
ARCHIVOL = SESSION:TEMP-DIRECTORY + 'Q160L' +  STRING(ISUCTRABAJO) + STRING(IPALLET).

DO:
  FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                     pallets.id_pallet = iPallet NO-ERROR.

  IF NOT AVAILABLE PALLETS THEN DO: 
          cStatus = 'Pallet inexistente'. 
          UNDO , RETURN ERROR cstatus. 
  END.    

    
  IF AVAILABLE pallets  THEN DO:

    cPallet = STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').
    cfecha = STRING(YEAR(TODAY),'9999') + string(MONTH(TODAY),'99') + STRING(DAY(TODAY),'99').
    
    WLOG(ARCHIVOL, 'PALLET ' + STRING(PALLETS.ID_PALLET)).  
    FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
    FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.

    IF NOT AVAILABLE pedidos_packing OR NOT AVAILABLE items_pedidos_packing  THEN 
    DO:
          cstatus = 'Pedido de produccion inexistente'.
          UNDO , RETURN ERROR cstatus.
    END.

 /*   RUN PREPARA-CAJAS. */
        
    cPallet = STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').
    cfecha = STRING(YEAR(TODAY),'9999') + string(MONTH(TODAY),'99') + STRING(DAY(TODAY),'99').

    FIND tipo_pallets OF PALLETS.

    IF pallets.id_packing = 1 THEN XPACKING = 'PG'. ELSE IF pallets.id_packing = 2 THEN XPACKING = 'PC'. ELSE XPACKING = 'PF'.
    IF SUBSTRING(STRING(pallets.id_turno_packing),2,1) = '1' THEN XTURNO = 'TM'. 
        ELSE IF SUBSTRING(STRING(pallets.id_turno_packing),2,1) = '2' THEN XTURNO = 'TT'.
        ELSE XTURNO = 'TN'. 

    CREATE TTPALLET.
    ASSIGN TTPALLET.Operacion = 0.


    FIND calidades OF pallets NO-LOCK NO-ERROR.
    FIND tipo_pallets OF pallets NO-LOCK NO-ERROR.
    FIND tipo_esquineros OF pallets NO-LOCK NO-ERROR.

    FIND categorias_packing OF pallets NO-LOCK NO-ERROR.
    FIND caracteristicas OF pallets NO-LOCK NO-ERROR.
    FIND marcas_prod OF PALLETS NO-LOCK NO-ERROR.
    FIND productos_terminados OF ITEMS_PEDIDOS_PACKING NO-LOCK NO-ERROR.
    FIND variedades OF items_pedidos_packing NO-LOCK NO-ERROR.
    FIND FIRST   lotes_plantacion WHERE lotes_plantacion.codigo_trazabilidad = pallets.codigo_trazabilidad NO-LOCK NO-ERROR.
    FIND FIRST   lote        OF lotes_plantacion  NO-LOCK NO-ERROR.
    FIND FIRST   origenes    OF lote NO-LOCK NO-ERROR.       
    IF pallets.union_europea THEN XMERCADO = 'UE'.
    ELSE IF pallets.china THEN XMERCADO = 'CHINA'.
    ELSE XMERCADO = 'NOUE'.

    FIND fechas_produccion WHERE fechas_produccion.fecha = pallets.fecha_prod NO-LOCK NO-ERROR.
    FIND tipos_procesos WHERE tipos_procesos.id_tipo_proceso = TRIM(pallets.tipo_proceso) NO-LOCK NO-ERROR.
    FIND packing OF pallets NO-LOCK NO-ERROR.
    
    CASE PALLETS.ID_SUC_TRABAJO:
        WHEN 98 THEN ASSIGN XALMACEN = '1108' XALMACENMTS = '1110'. 
        WHEN 97 THEN ASSIGN XALMACEN = '1208' XALMACENMTS = '1202'. 
    END CASE.
        
    CUENTA = 0.
    FOR EACH items_pallets OF pallets NO-LOCK.
        WLOG(ARCHIVOL,('ITEM_PALLET ' + ' ' + items_pallets.codigo_trazabilidad + ' ' + STRING(items_pallets.bultos))).
        FIND colores OF items_pallets NO-LOCK NO-ERROR.
        FIND CALIDADES OF items_pallets NO-LOCK NO-ERROR.

    
            FIND last volcado_packing where volcado_packing.codigo_trazabilidad = items_pallets.codigo_trazabilidad NO-LOCK.
            FIND LAST items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                    items_stock.id_tipo_movsto = 70 AND
                                    items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK.        
          
        
            
        XUNIDPROD = "UP-" + trim(origenes.zona_up)+ "-" + string(origenes.id_finca_senasa,"9999") + '-' + string(lote.id_lote_senasa,'999') /* +  '  NC.' */.   

        ICAN = ITEMS_PALLETS.BULTOS.
        FIND FIRST   lotes_plantacion WHERE lotes_plantacion.codigo_trazabilidad = items_pallets.codigo_trazabilidad NO-LOCK NO-ERROR.
        FIND FIRST   lote        OF lotes_plantacion  NO-LOCK NO-ERROR.
        FIND FIRST   origenes    OF lote NO-LOCK NO-ERROR.       
       
          

        CUENTA = CUENTA + 1.      
        CREATE TTPOSICIONPALLET.
        ASSIGN 
               TTPOSICIONPALLET.ID_PACKING = items_pallets.ID_PACKING
               TTPOSICIONPALLET.posicion = CUENTA
               TTPOSICIONPALLET.Ccalibre = CALIBRE(items_pallets.calibre)
               TTPOSICIONPALLET.Ccalidad = SUBSTRING(calidades.ID_CALIDAD_SAP,2)
               TTPOSICIONPALLET.Ccolor = colores.LETRA_COLOR
               TTPOSICIONPALLET.Cmarca = marcas_prod.id_marca_sap
               TTPOSICIONPALLET.esp_fitosanitaria = productos_terminados.descripcion
               TTPOSICIONPALLET.fecha_produccion = STRING(DAY(fechas_produccion.FECHA),'99') +  STRING(MONTH(fechas_produccion.FECHA),'99') + STRING(YEAR(fechas_produccion.FECHA),'9999')
               TTPOSICIONPALLET.finca_quinta = 'Tucuman'/*origenes.id_origen_sap*/
               TTPOSICIONPALLET.liote_cuadro = '0'/*lotes_plantacion.descripcion*/
               TTPOSICIONPALLET.material_sap = items_pedidos_packing.material_sap
               TTPOSICIONPALLET.nro_renspa = origenes.renspa
               TTPOSICIONPALLET.nro_up_puc_spa = XUNIDPROD  
               TTPOSICIONPALLET.turno_produccion = XTURNO
               TTPOSICIONPALLET.UM = 'CJ'
               TTPOSICIONPALLET.TIPO_MERCADO = XMERCADO  
               TTPOSICIONPALLET.orden_mto = STRING(pedidos_packing.id_orden)
               TTPOSICIONPALLET.planta_empaque = XPACKING
               TTPOSICIONPALLET.trazabilidad = lotes_plantacion.codigo_trazabilidad
               TTPOSICIONPALLET.var_fitosanitaria = variedades.descripcion
               TTPOSICIONPALLET.orden_mts = '00001'/*ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&')*/
               TTPOSICIONPALLET.cantidad =  STRING(ICAN)
               TTPOSICIONPALLET.ggn = STRING(pallets.gln) 
               TTPOSICIONPALLET.Almacen = XALMACEN
               TTPOSICIONPALLET.AlmacenMTS = XALMACENMTS
               TTPOSICIONPALLET.lote_sap = items_pallets.ubicacion
               TTPOSICIONPALLET.porcentaje_mrl = '0'.
        IF AVAILABLE calidades THEN                           
               TTPOSICIONPALLET.tipo_packaging = SUBSTRING(calidades.ID_CALIDAD_SAP,1,1).
               TTPOSICIONPALLET.CENTRO_COSTO = 'xxxx'.
    END.
    cStatus = 'OK'.
 
    FIND FIRST TTPALLET.
    DEFINE VARIABLE MARCH AS LONGCHAR NO-UNDO.

    
    RUN GENERA-XML( INPUT TABLE TTPALLET , INPUT TABLE TTPOSICIONPALLET , OUTPUT MARCH).
    
    MARCH = '<ns0:MT_Request xmlns:ns0="http://sanmiguel.com/PP160ING">
  <Operacion>0</Operacion>
  <!-- The following element may occur 0 or more times. -->
  <T_Posiciones>
    <Orden_Mto>0</Orden_Mto>
    <Material_Sap>0</Material_Sap>
    <Cantidad>0</Cantidad>
    <UM>0</UM>
    <Lote_Sap>0</Lote_Sap>
    <Almacen>0</Almacen>
    <Orden_Mts>0</Orden_Mts>
    <Almacen_Mts>0</Almacen_Mts>
    <Tipo_Mercado>0</Tipo_Mercado>
    <Calidad>0</Calidad>
    <Calibre>0</Calibre>
    <Tipo_Packaging>0</Tipo_Packaging>
    <Color>0</Color>
    <Porcentaje_Mrl>0</Porcentaje_Mrl>
    <Marca>0</Marca>
    <Planta_Empaque>0</Planta_Empaque>
    <Fecha_Produccion>0</Fecha_Produccion>
    <Turno_Produccion>0</Turno_Produccion>
    <Nro_Up_Puc_Spa>0</Nro_Up_Puc_Spa>
    <Nro_Renspa>0</Nro_Renspa>
    <Ggn>0</Ggn>
    <Finca_Quinta>0</Finca_Quinta>
    <Lote_Cuadro>0</Lote_Cuadro>
    <Trazabilidad>0</Trazabilidad>
    <Esp_Fitosanitaria>0</Esp_Fitosanitaria>
    <Var_Fitosanitaria>0</Var_Fitosanitaria>
    <Centro_Costo>0</Centro_Costo>
  </T_Posiciones>
</ns0:MT_Request>'.

    DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.
    DEFINE VARIABLE hSI_OS_PP160ING AS HANDLE NO-UNDO.
    
    CREATE SERVER hWebService.
    
    
    hWebService:CONNECT("-WSDL 'http://smazpiappdes.sanmiguel.local:50200/dir/wsdl?p=sa/511c8634ef4231619160b4f40327f2ea'
                         -WSDLUserid PIAPPLPID
                         -WSDLPassword SanMigu3l2015
                         -SOAPEndpointUserid PIAPPLPID 
                         -SOAPEndpointPassword SanMigu3l2015
                         -Port HTTP_Port").
    
    RUN SI_OS_PP160ING SET hSI_OS_PP160ING ON hWebService.
    
    DEFINE VARIABLE MT_Request AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE MT_Response AS LONGCHAR NO-UNDO.
    
    MT_Request = MARCH.
    
    RUN SI_OS_PP160ING IN hSI_OS_PP160ING(INPUT MT_Request, OUTPUT MT_Response).
        /*
        OPERACION = 'pp160ing.py'.
        CPARAMETROS =  '"' + ARCHIVO + ',' + ARCHIVO1 + '"'.
    
        RUN OPERACION.P (INPUT operacion , INPUT CPARAMETROS, OUTPUT  RES) NO-ERROR.  
        IF ERROR-STATUS:ERROR THEN
        DO:
            WLOG(ARCHIVOL, ('ERROR DE RETURN-VALUE: ' + RETURN-VALUE)).
            RETURN ERROR RETURN-VALUE.   
        END.
        */    
        WLOG(ARCHIVOL, ('RES ' + STRING(PALLETS.ID_PALLET) + ' '  + RES)).
        
     END.
 
END.
CATCH EX  AS Progress.Lang.Error :
    CSTATUS = EX:GETMESSAGE(1).
    UNDO , THROW EX.
END CATCH.



PROCEDURE GENERA-XML.
    DEFINE INPUT PARAMETER TABLE FOR TTPALLET.
    DEFINE INPUT PARAMETER TABLE FOR TTPOSICIONPALLET. 
    DEFINE OUTPUT PARAMETER MARCHIVO AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRow AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
    DEFINE VARIABLE hField AS HANDLE NO-UNDO.
    DEFINE VARIABLE hText AS HANDLE NO-UNDO.
    DEFINE VARIABLE ix AS INTEGER NO-UNDO.
    
    /* ********************  Preprocessor Definitions  ******************** */
    
    
    /* ***************************  Main Block  *************************** */
    
    CREATE X-DOCUMENT hDoc.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hRow.
    CREATE X-NODEREF hField.
    CREATE X-NODEREF hText.
    
    hDoc:CREATE-NODE(hRoot,"MT_Request","ELEMENT").
    hDoc:APPEND-CHILD(hRoot).
 
 /*
 <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:pp1="http://sanmiguel.com/PP160ING">
   <soapenv:Header/>
   <soapenv:Body>
      <pp1:MT_Request>
         <Operacion>1</Operacion>
         <!--Zero or more repetitions:-->
         <T_Posiciones>
            <Orden_Mto>1</Orden_Mto>
            <Material_Sap>1</Material_Sap>
            <Cantidad>1</Cantidad>
            <UM>1</UM>
            <Lote_Sap>1</Lote_Sap>
            <Almacen>1</Almacen>
            <Orden_Mts>1</Orden_Mts>
            <Almacen_Mts>1</Almacen_Mts>
            <Tipo_Mercado>1</Tipo_Mercado>
            <Calidad>1</Calidad>
            <Calibre>1</Calibre>
            <Tipo_Packaging>1</Tipo_Packaging>
            <Color>1</Color>
            <Porcentaje_Mrl>1</Porcentaje_Mrl>
            <Marca>1</Marca>
            <Planta_Empaque>1</Planta_Empaque>
            <Fecha_Produccion>1</Fecha_Produccion>
            <Turno_Produccion>1</Turno_Produccion>
            <Nro_Up_Puc_Spa>1</Nro_Up_Puc_Spa>
            <Nro_Renspa>1</Nro_Renspa>
            <Ggn>1</Ggn>
            <Finca_Quinta>1</Finca_Quinta>
            <Lote_Cuadro>1</Lote_Cuadro>
            <Trazabilidad>1</Trazabilidad>
            <Esp_Fitosanitaria>1</Esp_Fitosanitaria>
            <Var_Fitosanitaria>1</Var_Fitosanitaria>
            <Centro_Costo>1</Centro_Costo>
         </T_Posiciones>
      </pp1:MT_Request>
   </soapenv:Body>
</soapenv:Envelope>
 
 */   
    FOR EACH TTPALLET.
        hDoc:CREATE-NODE(hField,"Operacion","ELEMENT"). /* create  a row node */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        hField:APPEND-CHILD(hText).
        hRoot:APPEND-CHILD(hField).
        hText:NODE-VALUE = STRING(TTPALLET.OPERACION).
        FOR EACH TTPOSICIONPALLET.
            hDoc:CREATE-NODE(hRow,"T_Posiciones","ELEMENT"). /* create  a row node */
            hDoc:CREATE-NODE(hField,"Nodo","ELEMENT"). /* create  a row node */
            hDoc:CREATE-NODE(hText, "", "TEXT").
            hField:APPEND-CHILD(hText).
            hRow:APPEND-CHILD(hField).
            hRoot:APPEND-CHILD(hRow). /* put the row in the tree */    
        END.
    END.  
    
    hDoc:ENCODING= "UTF-8".
    hDoc:SAVE("LONGCHAR", MARCHIVO).
    
    DELETE OBJECT hDoc.
    DELETE OBJECT hRoot.
    DELETE OBJECT hRow.
    DELETE OBJECT hField.
    DELETE OBJECT hText.
END.


PROCEDURE PREPARA-CAJAS.
    DEFINE VAR VOMTS AS CHARACTER NO-UNDO.
    DEFINE VAR IPOSICION AS INTEGER NO-UNDO.
    DEFINE VAR BTOTALES AS INTEGER NO-UNDO.
    DEFINE VAR BTOTALES1 AS INTEGER NO-UNDO.
    DEFINE VAR BDIF AS INTEGER NO-UNDO.
    
    MESSAGE 'PREPARA-CAJAS'.
    FOR EACH cajas OF PALLETS WHERE cajas.id_testigo = 0 NO-LOCK:
        
        FIND volcado_packing OF CAJAS NO-LOCK NO-ERROR.
        IF  NOT AVAILABLE volcado_packing THEN DO:
            IPOSICION = IPOSICION + 1.
            FIND FIRST colores WHERE colores.letra_color = cajas.letra_color NO-LOCK NO-ERROR.
            CREATE TTCAJAS.
            BUFFER-COPY CAJAS TO TTCAJAS.
            IF AVAILABLE colores THEN
                ASSIGN TTCAJAS.ID_COLOR =  colores.ID_COLOR.
            ELSE TTCAJAS.ID_COLOR = 3.
            TTCAJAS.OMTS = VOMTS.
            TTCAJAS.POSICION = IPOSICION.
            TTCAJAS.BULTOS = 0.
            MESSAGE 'CAJAS OMTS VOLCADO' VOMTS.
            NEXT.
        END.
        FIND items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK NO-ERROR.
        IF NOT AVAILABLE items_stock THEN MESSAGE 'ERROR PARTIDA' volcado_packing.nro_partida volcado_packing.NRO_PARTIDA_SERIAL.         
        VOMTS = ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&') NO-ERROR.
        IF ERROR-STATUS:ERROR THEN VOMTS = items_stock.orden_entrega_sap.
        MESSAGE 'OMTS' VOMTS.
        IF VOMTS = '' THEN MESSAGE 'SIN OMTS' VIEW-AS ALERT-BOX.     
        FIND FIRST TTCAJAS WHERE TTCAJAS.ID_EMPRESA = cajas.id_empresa AND
                                 TTCAJAS.ID_PUNTO_EMISOR = cajas.id_punto_emisor AND
                                 TTCAJAS.ID_ORDEN = cajas.id_orden AND
                                 TTCAJAS.ITEM   = CAJAS.ITEM AND
                                 TTCAJAS.ID_PACKING = CAJAS.ID_PACKING AND
                                 TTCAJAS.CODIGO_TRAZABILIDAD = cajas.codigo_trazabilidad AND
                                 TTCAJAS.LETRA_COLOR = cajas.letra_color AND
                                 TTCAJAS.CALIBRE = cajas.calibre AND
                                 TTCAJAS.OMTS = VOMTS NO-ERROR.
        IF NOT AVAILABLE TTCAJAS THEN
        DO:
            IPOSICION = IPOSICION + 1.
            FIND FIRST colores WHERE colores.letra_color = cajas.letra_color NO-LOCK NO-ERROR.
            CREATE TTCAJAS.
            BUFFER-COPY CAJAS TO TTCAJAS.
            IF AVAILABLE colores THEN
                ASSIGN TTCAJAS.ID_COLOR =  colores.ID_COLOR.
            ELSE TTCAJAS.ID_COLOR = 3.
            TTCAJAS.OMTS = VOMTS.
            TTCAJAS.POSICION = IPOSICION.
            TTCAJAS.BULTOS = 0.
            MESSAGE 'CAJAS OMTS ' VOMTS.
        END.                             
        TTCAJAS.BULTOS = TTCAJAS.BULTOS + 1.
        CREATE TTCAJAS1.
        BUFFER-COPY CAJAS TO TTCAJAS1.
        TTCAJAS1.POSICION = IPOSICION.
    END.
    
    MESSAGE 'PREPARA CAJAS1'.
    FOR EACH items_pallets OF PALLETS NO-LOCK.
        ASSIGN BTOTALES = 0 BTOTALES1 = 0.
        FOR EACH TTCAJAS WHERE     
                                    items_PALLETS.ID_EMPRESA = TTCAJAS.ID_EMPRESA AND
                                    items_pallets.id_punto_emisor = TTCAJAS.ID_PUNTO_EMISOR AND
                                    items_pallets.id_orden = TTCAJAS.ID_ORDEN AND
                                    items_pallets.ITEM = TTCAJAS.ITEM AND
                                    items_pallets.id_packing = TTCAJAS.ID_PACKING AND
                                    items_pallets.codigo_trazabilidad = TTCAJAS.CODIGO_TRAZABILIDAD AND
                                    items_pallets.id_color = TTCAJAS.ID_COLOR AND
                                    items_pallets.calibre = TTCAJAS.CALIBRE.
            BTOTALES = BTOTALES + TTCAJAS.BULTOS.
            MESSAGE 'TTCAJAS' TTCAJAS.BULTOS.
        END.
        BDIF = items_pallets.bultos - BTOTALES.
        MESSAGE 'BDIF' BDIF.
        IF BDIF <> 0 THEN
        DO:
            FOR EACH TTCAJAS WHERE  
                                        items_PALLETS.ID_EMPRESA = TTCAJAS.ID_EMPRESA AND
                                        items_pallets.id_punto_emisor = TTCAJAS.ID_PUNTO_EMISOR AND
                                        items_pallets.id_orden = TTCAJAS.ID_ORDEN AND
                                        items_pallets.ITEM = TTCAJAS.ITEM AND
                                        items_pallets.id_packing = TTCAJAS.ID_PACKING AND
                                        items_pallets.codigo_trazabilidad = TTCAJAS.CODIGO_TRAZABILIDAD AND
                                        items_pallets.id_color = TTCAJAS.ID_COLOR AND
                                        items_pallets.calibre = TTCAJAS.CALIBRE.
                TTCAJAS.BULTOS = TTCAJAS.BULTOS + ROUND( ( BDIF * TTCAJAS.BULTOS / BTOTALES ),0).
                BTOTALES1 = BTOTALES1 + TTCAJAS.BULTOS.
                MESSAGE 'TTCAJAS1' TTCAJAS.BULTOS.
            END.
            BDIF = items_pallets.bultos - BTOTALES1.
            MESSAGE 'BDIF1' BDIF.
            IF BDIF <> 0 THEN
            FOR LAST TTCAJAS WHERE 
                                        items_PALLETS.ID_EMPRESA = TTCAJAS.ID_EMPRESA AND
                                        items_pallets.id_punto_emisor = TTCAJAS.ID_PUNTO_EMISOR AND
                                        items_pallets.id_orden = TTCAJAS.ID_ORDEN AND
                                        items_pallets.ITEM = TTCAJAS.ITEM AND
                                        items_pallets.id_packing = TTCAJAS.ID_PACKING AND
                                        items_pallets.codigo_trazabilidad = TTCAJAS.CODIGO_TRAZABILIDAD AND
                                        items_pallets.id_color = TTCAJAS.ID_COLOR AND
                                        items_pallets.calibre = TTCAJAS.CALIBRE.
                TTCAJAS.BULTOS = TTCAJAS.BULTOS + BDIF.
                WLOG(ARCHIVOL,('ULTIMA ' + STRING(TTCAJAS.BULTOS))).
            END.            
            
        END.
    END.
    WLOG(ARCHIVOL, 'SALIO CAJAS').
END PROCEDURE.

PROCEDURE ULTIMAMTS.
     DEFINE OUTPUT PARAMETER VOMTS AS CHARACTER NO-UNDO.
     
    FOR EACH CAJAS OF PALLETS NO-LOCK , FIRST volcado_packing OF CAJAS NO-LOCK,
            EACH items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial BY
                                items_stock.orden_entrega_sap. 
           /* ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&'). */
        VOMTS = ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&') NO-ERROR.
        IF ERROR-STATUS:ERROR THEN VOMTS = items_stock.orden_entrega_sap.
    END.
    IF VOMTS = '' THEN
    DO:
        FIND last volcado_packing where volcado_packing.codigo_trazabilidad = items_pallets.codigo_trazabilidad NO-LOCK.
        FIND LAST items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK NO-ERROR.
        IF AVAILABLE items_stock THEN VOMTS = items_stock.orden_entrega_sap.                 
    END.
    WLOG(ARCHIVOL, 'SALIO ULTIMAMTS').
END.

FUNCTION WLOG RETURNS LOGICAL (INPUT ARCHIVO AS CHARACTER, INPUT TEXTO AS CHARACTER):
    OUTPUT TO VALUE(ARCHIVO) APPEND.
    EXPORT DELIMITER "|" STRING(NOW)  TEXTO.
    OUTPUT CLOSE.
    RETURN TRUE.
END FUNCTION. 
