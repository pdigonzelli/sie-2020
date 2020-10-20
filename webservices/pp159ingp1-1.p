
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
  DEFINE OUTPUT PARAMETER OPALLET AS CHARACTER NO-UNDO.

  
  
  
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
  DEFINE VARIABLE   ARCHIVO1        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   OPERACION       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   CUENTA          AS INTEGER NO-UNDO.
  DEFINE VARIABLE   DIR             AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   RES             AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   XALMACENMTS     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   CONTADOR        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE   IPOS            AS INTEGER NO-UNDO.
  DEFINE VARIABLE   ILOTE           AS INTEGER NO-UNDO.



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

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



{ttpallet.i}


FILE-INFO:FILE-NAME = ".".
DIR = FILE-INFO:FULL-PATHNAME.


DO:
  FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                     pallets.id_pallet = iPallet NO-ERROR.

  IF NOT AVAILABLE PALLETS THEN DO: 
          cStatus = 'Pallet inexistente'. 
          UNDO , RETURN ERROR cstatus. 
  END.    

    
  IF AVAILABLE pallets  THEN DO:
      
    FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
    FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.

    IF NOT AVAILABLE pedidos_packing OR NOT AVAILABLE items_pedidos_packing  THEN 
    DO:
          cstatus = 'Pedido de produccion inexistente'.
          UNDO , RETURN ERROR cstatus.
    END.


    DEFINE VAR VOMTS AS CHARACTER NO-UNDO.
    DEFINE VAR IPOSICION AS INTEGER NO-UNDO.
    
    MESSAGE 'CAJAS-X'.
    FOR EACH cajas OF PALLETS WHERE cajas.id_testigo = 0 NO-LOCK:
        MESSAGE 'CAJAS' cajas.id_caja .
        FIND volcado_packing OF CAJAS NO-LOCK.
        FIND items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK.        
        VOMTS = ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&').
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
            MESSAGE 'CAJAS POSICION ' IPOSICION.
        END.                             
        TTCAJAS.BULTOS = TTCAJAS.BULTOS + 1.
        MESSAGE TTCAJAS.BULTOS.
        CREATE TTCAJAS1.
        BUFFER-COPY CAJAS TO TTCAJAS1.
        TTCAJAS1.POSICION = IPOSICION.
    END.
    
    DEFINE VAR BTOTALES AS INTEGER NO-UNDO.
    FOR EACH items_pallets OF PALLETS NO-LOCK.
        FOR EACH TTCAJAS WHERE      items_pallets OF PALLETS WHERE 
                                    items_PALLETS.ID_EMPRESA = TTCAJAS.ID_EMPRESA AND
                                    items_pallets.id_punto_emisor = TTCAJAS.ID_PUNTO_EMISOR AND
                                    items_pallets.id_orden = TTCAJAS.ID_ORDEN AND
                                    items_pallets.ITEM = TTCAJAS.ITEM AND
                                    items_pallets.id_packing = TTCAJAS.ID_PACKING AND
                                    items_pallets.codigo_trazabilidad = TTCAJAS.CODIGO_TRAZABILIDAD AND
                                    items_pallets.id_color = TTCAJAS.ID_COLOR AND
                                    items_pallets.calibre = TTCAJAS.CALIBRE.
            BTOTALES = BTOTALES + TTCAJAS.BULTOS.
        END.
        BDIF = items_pallets.bultos - BTOTALES.
        IF BDIF <> 0 THEN
        DO:
            FOR EACH TTCAJAS WHERE      items_pallets OF PALLETS WHERE 
                                        items_PALLETS.ID_EMPRESA = TTCAJAS.ID_EMPRESA AND
                                        items_pallets.id_punto_emisor = TTCAJAS.ID_PUNTO_EMISOR AND
                                        items_pallets.id_orden = TTCAJAS.ID_ORDEN AND
                                        items_pallets.ITEM = TTCAJAS.ITEM AND
                                        items_pallets.id_packing = TTCAJAS.ID_PACKING AND
                                        items_pallets.codigo_trazabilidad = TTCAJAS.CODIGO_TRAZABILIDAD AND
                                        items_pallets.id_color = TTCAJAS.ID_COLOR AND
                                        items_pallets.calibre = TTCAJAS.CALIBRE.
                TTCAJAS.BULTOS = TTCAJAS.BULTOS + ROUNDED( ( BDIF * TTCAJAS.BULTOS / BTOTALES ),0).
                BTOTALES1 = BTOTALES1 + TTCAJAS.BULTOS.
            END.
            BDIF = items_pallets.bultos - BTOTALES1.
            IF BDIF <> 0 THEN
            FOR LAST TTCAJAS WHERE      items_pallets OF PALLETS WHERE 
                                        items_PALLETS.ID_EMPRESA = TTCAJAS.ID_EMPRESA AND
                                        items_pallets.id_punto_emisor = TTCAJAS.ID_PUNTO_EMISOR AND
                                        items_pallets.id_orden = TTCAJAS.ID_ORDEN AND
                                        items_pallets.ITEM = TTCAJAS.ITEM AND
                                        items_pallets.id_packing = TTCAJAS.ID_PACKING AND
                                        items_pallets.codigo_trazabilidad = TTCAJAS.CODIGO_TRAZABILIDAD AND
                                        items_pallets.id_color = TTCAJAS.ID_COLOR AND
                                        items_pallets.calibre = TTCAJAS.CALIBRE.
                TTCAJAS.BULTOS = TTCAJAS.BULTOS + BDIF.
            END.            
        END.
    END.
        

    cPallet = STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').
    cfecha = STRING(YEAR(TODAY),'9999') + string(MONTH(TODAY),'99') + STRING(DAY(TODAY),'99').

    FIND tipo_pallets OF PALLETS.

    IF pallets.id_packing = 1 THEN XPACKING = 'PG'. ELSE IF pallets.id_packing = 2 THEN XPACKING = 'PC'. ELSE XPACKING = 'PF'.
    IF SUBSTRING(STRING(pallets.id_turno_packing),2,1) = '1' THEN XTURNO = 'TM'. 
        ELSE IF SUBSTRING(STRING(pallets.id_turno_packing),2,1) = '2' THEN XTURNO = 'TT'.
        ELSE XTURNO = 'TN'. 

    CREATE TTPALLET.
    ASSIGN TTPALLET.pallet_id = CPALLET
           TTPALLET.tipo_pallet = tipo_pallets.id_tipo_pallet_sap
           TTPALLET.CONTRAMARCA = if pallets.contramarca = '' then 'ZZ' else pallets.contramarca
           TTPALLET.ppecb = if pallets.pallet_senasa = '' then '1000' else pallets.pallet_senasa
           TTPALLET.sucursal = 'A100'
           TTPALLET.material_embalaje = '500990'
           TTPALLET.organizacion = 'ORG1'.


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
/*    FIND FIRST TTCAJAS NO-LOCK NO-ERROR.
    IF NOT AVAILABLE TTCAJAS THEN
    DO: */
    FOR EACH items_pallets OF pallets NO-LOCK.
        MESSAGE 'ITEM_PALLET ' items_pallets.codigo_trazabilidad items_pallets.bultos.
        FIND colores OF items_pallets NO-LOCK NO-ERROR.
        FIND CALIDADES OF items_pallets NO-LOCK NO-ERROR.

        FIND FIRST TTCAJAS WHERE
            TTCAJAS.ID_EMPRESA = items_pallets.id_empresa AND
            TTCAJAS.id_punto_emisor = items_pallets.id_punto_emisor AND
            TTCAJAS.ID_ORDEN = items_pallets.id_orden AND
            TTCAJAS.ITEM = items_pallets.ITEM AND
            TTCAJAS.ID_PACKING = items_pallets.id_packing AND
            TTCAJAS.CODIGO_TRAZABILIDAD = items_pallets.codigo_trazabilidad AND
            TTCAJAS.CALIBRE = items_pallets.calibre AND
            TTCAJAS.ID_COLOR = items_pallets.id_color NO-LOCK NO-ERROR.
        

        XUNIDPROD = "UP-" + trim(origenes.zona_up)+ "-" + string(origenes.id_finca_senasa,"9999") + '-' + string(lote.id_lote_senasa,'999') /* +  '  NC.' */.   

        ICAN = ITEMS_PALLETS.BULTOS.
        FIND FIRST   lotes_plantacion WHERE lotes_plantacion.codigo_trazabilidad = items_pallets.codigo_trazabilidad NO-LOCK NO-ERROR.
        FIND FIRST   lote        OF lotes_plantacion  NO-LOCK NO-ERROR.
        FIND FIRST   origenes    OF lote NO-LOCK NO-ERROR.       
          
        CUENTA = CUENTA + 1.      
        CREATE TTPOSICIONPALLET.
        ASSIGN TTPOSICIONPALLET.posicion = CUENTA
               TTPOSICIONPALLET.Ccalibre = items_pallets.calibre
               TTPOSICIONPALLET.Ccalidad = calidades.ID_CALIDAD_SAP
               TTPOSICIONPALLET.Ccolor = colores.LETRA_COLOR
               TTPOSICIONPALLET.Cmarca = marcas_prod.id_marca_sap
               TTPOSICIONPALLET.esp_fitosanitaria = productos_terminados.descripcion
               TTPOSICIONPALLET.fecha_produccion = STRING(DAY(fechas_produccion.FECHA),'99') +  STRING(MONTH(fechas_produccion.FECHA),'99') + STRING(YEAR(fechas_produccion.FECHA),'9999')
               TTPOSICIONPALLET.finca_quinta = origenes.id_origen_sap
               TTPOSICIONPALLET.liote_cuadro = lotes_plantacion.descripcion
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
               TTPOSICIONPALLET.orden_mts = /*ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&')*/ TTCAJAS.OMTS  
               TTPOSICIONPALLET.cantidad =  STRING(ICAN)
               TTPOSICIONPALLET.ggn = STRING(pallets.gln) 
               TTPOSICIONPALLET.Almacen = XALMACEN
               TTPOSICIONPALLET.AlmacenMTS = XALMACENMTS
               TTPOSICIONPALLET.lote_sap = ''
               TTPOSICIONPALLET.porcentaje_mrl = '0'.
        IF AVAILABLE calidades THEN                           
               TTPOSICIONPALLET.tipo_packaging = SUBSTRING(calidades.ID_CALIDAD_SAP,1,1).
     /*   END.*/
/*  END.
        ELSE
        DO:
            MESSAGE 'ITEMS PALLETS1 ' items_pallets.codigo_trazabilidad.
            FOR EACH TTCAJAS WHERE
                TTCAJAS.ID_EMPRESA = items_pallets.id_empresa AND
                TTCAJAS.id_punto_emisor = items_pallets.id_punto_emisor AND
                TTCAJAS.ID_ORDEN = items_pallets.id_orden AND
                TTCAJAS.ITEM = items_pallets.ITEM AND
                TTCAJAS.ID_PACKING = items_pallets.id_packing AND
                TTCAJAS.CODIGO_TRAZABILIDAD = items_pallets.codigo_trazabilidad AND
                TTCAJAS.CALIBRE = items_pallets.calibre AND
                TTCAJAS.ID_COLOR = items_pallets.ID_COLOR:
        
                FIND FIRST   lotes_plantacion WHERE lotes_plantacion.codigo_trazabilidad = items_pallets.codigo_trazabilidad NO-LOCK NO-ERROR.
                FIND FIRST   lote        OF lotes_plantacion  NO-LOCK NO-ERROR.
                FIND FIRST   origenes    OF lote NO-LOCK NO-ERROR.       
        
                MESSAGE 'LOTES PLANTACION ' LOTES_PLANTACION.codigo_trazabilidad.


                XUNIDPROD = "UP-" + trim(origenes.zona_up)+ "-" + string(origenes.id_finca_senasa,"9999") + '-' + string(lote.id_lote_senasa,'999') /* +  '  NC.' */.   

                CREATE TTPOSICIONPALLET.
                ASSIGN TTPOSICIONPALLET.posicion = TTCAJAS.POSICION
                       TTPOSICIONPALLET.Ccalibre = items_pallets.calibre
                       TTPOSICIONPALLET.Ccalidad = calidades.ID_CALIDAD_SAP
                       TTPOSICIONPALLET.Ccolor = TTCAJAS.LETRA_COLOR
                       TTPOSICIONPALLET.Cmarca = marcas_prod.id_marca_sap
                       TTPOSICIONPALLET.esp_fitosanitaria = productos_terminados.descripcion
                       TTPOSICIONPALLET.fecha_produccion = STRING(DAY(fechas_produccion.FECHA),'99') +  STRING(MONTH(fechas_produccion.FECHA),'99') + STRING(YEAR(fechas_produccion.FECHA),'9999') 
                       TTPOSICIONPALLET.finca_quinta = origenes.id_origen_sap
                       TTPOSICIONPALLET.liote_cuadro = lotes_plantacion.descripcion
                       TTPOSICIONPALLET.material_sap = items_pedidos_packing.material_sap
                       TTPOSICIONPALLET.nro_renspa = origenes.renspa
                       TTPOSICIONPALLET.nro_up_puc_spa = XUNIDPROD  
                       TTPOSICIONPALLET.turno_produccion = XTURNO
                       TTPOSICIONPALLET.UM = 'CJ'
                       TTPOSICIONPALLET.TIPO_MERCADO = XMERCADO  
                       TTPOSICIONPALLET.orden_mto = STRING(TTCAJAS.id_orden)
                       TTPOSICIONPALLET.planta_empaque = XPACKING
                       TTPOSICIONPALLET.trazabilidad = items_pallets.CODIGO_TRAZABILIDAD
                       TTPOSICIONPALLET.var_fitosanitaria = variedades.descripcion
                       TTPOSICIONPALLET.orden_mts = TTCAJAS.OMTS 
                       TTPOSICIONPALLET.cantidad =  STRING(TTCAJAS.BULTOS)
                       TTPOSICIONPALLET.ggn = STRING(pallets.gln)
                       TTPOSICIONPALLET.Almacen = XALMACEN
                       TTPOSICIONPALLET.AlmacenMTS = XALMACENMTS
                       TTPOSICIONPALLET.lote_sap = ''
                       TTPOSICIONPALLET.porcentaje_mrl = '0'.
                IF AVAILABLE calidades THEN                           
                       TTPOSICIONPALLET.tipo_packaging = SUBSTRING(calidades.ID_CALIDAD_SAP,1,1). 
            END.
        END. */
    END.
    cStatus = 'OK'.
  
  END.
  FIND FIRST TTPALLET.



  ARCHIVO  = SESSION:TEMP-DIRECTORY + 'P' + STRING(TTPALLET.pallet_id) + '.XML'.
  ARCHIVO1 = SESSION:TEMP-DIRECTORY + 'P' + STRING(TTPALLET.pallet_id) + '.RES'.

  RUN EXPORTAXML.P( ARCHIVO , INPUT TABLE TTPALLET , INPUT TABLE TTPOSICIONPALLET).

  OPERACION = 'pp159ing.py'.
  CPARAMETROS =  '"' + ARCHIVO + ',' + ARCHIVO1 + '"'.

  RUN OPERACION.P (INPUT operacion , INPUT CPARAMETROS, OUTPUT  RES) NO-ERROR.  
  /* MESSAGE RETURN-VALUE RES . */
  IF ERROR-STATUS:ERROR THEN
    RETURN ERROR RETURN-VALUE.   

  IF NUM-ENTRIES(RES,'|') < 2 THEN 
    RETURN ERROR 'RESPUESTA INSUFICIENTE'.

  OPALLET = ENTRY(2,ENTRY(1,RES,'|'),':').  
   
    
  DO CONTADOR = 2 TO NUM-ENTRIES(RES,'|'):
      IPOS = INTEGER(ENTRY(1,ENTRY(CONTADOR,RES,'|'),':')).
      ILOTE = INTEGER(ENTRY(2,ENTRY(CONTADOR,RES,'|'),':')) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.
      
      FIND FIRST TTPOSICIONPALLET WHERE TTPOSICIONPALLET.POSICION = IPOS NO-LOCK.
      
    FOR EACH  TTCAJAS1 WHERE
        TTCAJAS1.ID_EMPRESA = TTPOSICIONPALLET.id_empresa AND
        TTCAJAS1.id_punto_emisor = TTPOSICIONPALLET.id_punto_emisor AND
        TTCAJAS1.ID_ORDEN = TTPOSICIONPALLET.id_orden AND
        TTCAJAS1.ITEM = TTPOSICIONPALLET.ITEM AND
        TTCAJAS1.ID_PACKING = TTPOSICIONPALLET.id_packing AND
        TTCAJAS1.CODIGO_TRAZABILIDAD = TTPOSICIONPALLET.codigo_trazabilidad AND
        TTCAJAS1.CALIBRE = TTPOSICIONPALLET.calibre AND
        TTCAJAS1.ID_COLOR = TTPOSICIONPALLET.id_color.      
        ASSIGN TTCAJAS1.LOTE_SAP = STRING(ILOTE).
    END.
  END.  
  
  
  FOR EACH TTCAJAS1 .
    FIND CAJAS WHERE CAJAS.ID_SUC_TRABAJO = TTCAJAS1.ID_SUC_TRABAJO AND
                     cajas.id_caja = TTCAJAS1.ID_CAJA  NO-ERROR.
    CAJAS.LOTE_SAP = TTCAJAS1.LOTE_SAP.
  END.

/*
  DEFINE VARIABLE text-memptr AS MEMPTR NO-UNDO.
  DEFINE VARIABLE TEXTO AS CHARACTER NO-UNDO.
  DEFINE VAR CONTADOR AS INTEGER NO-UNDO.
  DEFINE VAR IPOS AS INTEGER NO-UNDO.
  DEFINE VAR ILOTE AS INTEGER NO-UNDO.
  
  COPY-LOB FROM FILE RES TO text-memptr.
  TEXTO = GET-STRING(texT-memptr,1).
    
  OPALLET = ENTRY(2,ENTRY(1,TEXTO,'|'),':').  

  DO CONTADOR = 2 TO NUM-ENTRIES(TEXTO,'|'):
      IPOS = INTEGER(ENTRY(1,ENTRY(CONTADOR,TEXTO,'|'),':')).
      ILOTE = INTEGER(ENTRY(2,ENTRY(CONTADOR,TEXTO,'|'),':')).
      FOR EACH TTCAJAS WHERE TTCAJAS.POSICION = IPOS.
        ASSIGN TTCAJAS.LOTE_SAP = STRING(ILOTE).
      END.
  END.  
  
  
  FOR EACH TTCAJAS .
    FIND CAJAS WHERE CAJAS.ID_SUC_TRABAJO = TTCAJAS.ID_SUC_TRABAJO AND
                     cajas.id_caja = TTCAJAS.ID_CAJA  NO-ERROR.
    CAJAS.LOTE_SAP = TTCAJAS.LOTE_SAP.
  END.

*/
END.
CATCH EX  AS Progress.Lang.Error :
    CSTATUS = EX:GETMESSAGE(1).
    UNDO , THROW EX.
END CATCH.
