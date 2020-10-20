
/*------------------------------------------------------------------------
    File        : verpallet.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Mar 23 20:04:22 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


FUNCTION CALIBRE RETURNS CHARACTER (CCALIBRE AS CHARACTER):
    DEFINE VAR CAL AS CHARACTER NO-UNDO.
    IF INDEX(CCALIBRE, '/') <> 0 THEN
         CAL = SUBSTRING(CCALIBRE,1,INDEX(CCALIBRE, '/') - 1).
    ELSE 
         CAL = SUBSTRING(CCALIBRE,1,LENGTH(CCALIBRE) - 1).
    RETURN CAL.    
    
END.


DEFINE VAR VOMTS AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

CURRENT-WINDOW:WIDTH = 120.

FIND FIRST PEDIDOS_PACKING WHERE ID_EMPRESA = 1 AND ID_PUNTO_EMISOR = 1 AND ID_ORDEN = 10000304.
DISP pedidos_packing WITH 2 COLUMNS.
FOR EACH items_pedidos_packing OF pedidos_packing no-lock.
    DISP items_pedidos_packing.ITEM WITH 2 COLUMNS.
END.

/*
FOR LAST PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET = 16002336 NO-LOCK.
    DISPLAY pallets.id_pallet_sap FORMAT 'X(30)'.
/*    FOR EACH items_pallets OF PALLETS.
        DISP    items_pallets.id_pallet  
                items_pallets.item_pallet 
                items_pallets.ubicacion FORMAT 'X(20)' 
                items_pallets.bultos 
                items_pallets.calibre
                items_pallets.codigo_trazabilidad
                items_pallets.id_caract
                items_pallets.id_packing
                items_pallets.id_color
                items_pallets.id_variedad
                CALIBRE(items_pallets.calibre) WITH WIDTH 100.
    END.
    FIND FIRST CAJAS OF PALLETS WHERE cajas.id_testigo = 0.
    DISP PALLETS.ID_PALLET CAJAS.ID_CAJA. */
     FOR EACH CAJAS OF PALLETS. /* NO-LOCK*/
/*     
      cajas.id_orden        = pedidos_packing.id_orden
      cajas.item            = ITEMs_pedidos_packing.item
      cajas.id_empresa      = pedidos_packing.id_empresa
      cajas.id_punto_emisor = pedidos_packing.id_punto_emisor
      cajas.id_packing      = cajoneras.id_packing
      cajas.id_embalador    = embaladores.id_embalador
      cajas.nro_volcado     = volcado_packing.nro_volcado
      cajas.id_turno        = turnos_packing.id_turno_packing
      cajas.fecha           = TODAY
      cajas.fecha_operativa = fop
      cajas.leida           = FALSE
      cajas.creacion        = NOW
      cajas.trazabilidad    = X_trazabil
      cajas.uniprod         = X_unidprod
      cajas.codigo_trazabilidad =  items_stock.codigo_trazabilidad
      cajas.letra_color     = IF cajoneras.color_fruta = 1 THEN 'A' ELSE 'R'
      cajas.id_tipo_proceso = volcado_packing.id_tipo_proceso
      cajas.empapelado      = IF AVAILABLE calidades THEN calidades.empapelado ELSE FALSE
      cajas.id_envase       = items_pedidos_packing.id_envase
      cajas.peso_nominal    = IF AVAILABLE r_envases_prod THEN r_envases_prod.kilos_nominal ELSE 0 
      cajas.id_caract       = items_pedidos_packing.id_caract
      cajas.id_calidad      = items_pedidos_packing.id_calidad
      cajas.id_categoria    = items_pedidos_packing.id_categoria
      cajas.id_envase       = items_pedidos_packing.id_envase
      cajas.id_marca        = items_pedidos_packing.id_marca
      cajas.UNION_europea   = pedidos_packing.UNION_europea
      cajas.contramarca     = pedidos_packing.contramarca
      cajas.china           = pedidos_packing.china
      cajas.calibre         = items_pedidos_packing.calibre
      cajas.bultos          = items_pedidos_packing.bultos 
      cajas.id_articulo     = items_pedidos_packing.id_articulo
      cajas.id_tipo_pallet  = items_pedidos_packing.id_tipo_pallet 
      cajas.id_tipo_esquinero = items_pedidos_packing.id_tipo_esquinero 
      cajas.id_variedad       = items_pedidos_packing.id_variedad.
  */   
     
      
     
      DISP CAJAS.ID_PROCESO_RESUMEN  WITH 2 COLUMNS. 
     /*, FIRST volcado_packing OF CAJAS NO-LOCK,
            EACH items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial BY
                                items_stock.ORDEN_ENTREGA_SAP. */ 
           /* ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&'). */
/*        VOMTS = ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&') NO-ERROR.
        disp items_stock.id_variedad items_stock.orden_entrega_sap format 'x(40)'.       
        IF ERROR-STATUS:ERROR THEN VOMTS = items_stock.orden_entrega_sap. */
    END.
    
    DISPLAY VOMTS FORMAT 'X(30)'.
        
END.
*/