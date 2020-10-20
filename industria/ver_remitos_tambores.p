/*FOR EACH remitos WHERE nro_comprobante = "006900006164" :
    DISPLAY nro estado fecha.
    UPDATE remitos.estado.
    /*FOR EACH items_factura OF remitos NO-LOCK:
        DISPLAY  nro_lote desde_lote hasta_lote.
    END.*/
END.*/

FOR EACH tambores_industria WHERE anio = 2005 AND id_lote = 309 and id_articulo = 53 AND
    id_tambor >= 265 :
    
    DISPLAY id_tambor id_sucursal_ubicacion id_sucursal_remito id_tipo_movsto nro_remito.
    
    /*ASSIGN id_sucursal_remito = 0
           id_tipo_movsto = 0
           nro_remito = 0.*/
END.

