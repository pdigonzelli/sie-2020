FOR EACH items_factura WHERE nro_lote = "0345/03"
                         AND id_articulo = 53
                         /*AND c_fecha = DATE("01/09/2003")*/.
    DISP items_factura.bultos .
    FIND FIRST remitos OF items_factura.
    DISP remitos.nro_comp remitos.mercado remitos.id_cliente.
    FIND FIRST clientes OF remitos.
    DISP clientes.razon_social.
END.
