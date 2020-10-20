FOR LAST volcado_packing WHERE FECHA = TODAY  NO-LOCK.
        DISPLAY VOLCADO_PACKING.
        FOR  LAST items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND            
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK.
            DISP ITEMS_STOCK.ORDEN_ENTREGA_SAP ITEMS_STOCK.DOCUMENTO_SAP.                        
        END.
END.
