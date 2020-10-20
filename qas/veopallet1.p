
/*------------------------------------------------------------------------
    File        : veopallet1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Mon Mar 21 23:22:52 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR LAST volcado_packing WHERE volcado_packing.codigo_trazabilidad = '05Q01' NO-LOCK.
    FOR EACH proceso_volcado OF volcado_packing.
        FIND items_stock WHERE  items_stock.id_sucursal = proceso_volcado.id_sucursal AND
             items_stock.id_tipo_movsto = proceso_volcado.id_tipo_movsto AND
             items_stock.NRO = proceso_volcado.NRO NO-LOCK.
        DISP items_stock with 2 columns.
    END.  
END.      
/*        
        
        
        
        
        
        FIND items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK.
*/