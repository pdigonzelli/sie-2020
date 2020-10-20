
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

FOR LAST PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET = 17000259.
    DISPLAY pallets.id_pallet_sap FORMAT 'X(30)'.
    update  pallets.id_pallet_sap.
    FOR EACH items_pallets OF PALLETS.
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
    FIND FIRST CAJAS OF PALLETS WHERE cajas.id_testigo <> 0 no-error.
    DISP PALLETS.ID_PALLET CAJAS.ID_CAJA.
     FOR EACH CAJAS OF PALLETS NO-LOCK , FIRST volcado_packing OF CAJAS NO-LOCK,
            EACH items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial BY
                                items_stock.ORDEN_ENTREGA_SAP. 
           /* ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&'). */
        VOMTS = ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&') NO-ERROR.
        disp items_stock.id_variedad items_stock.orden_entrega_sap format 'x(40)'.       
        IF ERROR-STATUS:ERROR THEN VOMTS = items_stock.orden_entrega_sap.
    END.
    
    DISPLAY VOMTS FORMAT 'X(30)'.
        
END.
