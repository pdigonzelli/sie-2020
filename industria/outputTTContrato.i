  MESSAGE 'Exportando .....' VIEW-AS ALERT-BOX.
 OUTPUT TO v:\temp\datos.txt.

  FOR EACH tt-contrato BY tt-contrato.anio
                       BY tt-contrato.semana_desde
                       BY tt-contrato.semana_hasta.

    EXPORT DELIMITER ";"
        tt-contrato.id_contrato
        tt-contrato.fecha_contrato
        tt-contrato.condicion_venta
        tt-contrato.ITEM
        tt-contrato.cantidad_contratos
        "region"
        tt-contrato.id_cliente
        tt-contrato.cliente
        "region cliente final"
        tt-contrato.id_cliente_final
        tt-contrato.cliente_final
        tt-contrato.id_articulo
        tt-contrato.articulo
        tt-contrato.id_calidad
        tt-contrato.calidad
        "Destino"
        tt-contrato.id_vapor
        tt-contrato.vapor
        "fecha salida"
        "fecha llegada"
        tt-contrato.orden_entrega
        "Nro Packing List"
        "Cantidad pl"
        tt-contrato.lote
        "Nro Factura"
        "Fecha Factura"
        "Vto Factura"
        "cantidad factura"
        "unidad de facturacion"
        "condicion de venta de factura"
        "fob unitario factura"
        "Importe Item Factura"
        "moneda origen"
        string(tt-contrato.gastos)
        "gasto para discriminar"
        /*
        IF NOT tt-contrato.consignacion THEN
            tt-contrato.factura
        ELSE ""
        IF NOT tt-contrato.consignacion THEN
        string(tt-contrato.fecha_factura)
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        string(tt-contrato.vto_factura)
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        "cantidad factura"
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        "unidad de facturacion"
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        "condicion de venta de factura"
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        "fob unitario factura"
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        string(tt-contrato.importe_item_factura)
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        "moneda origen"
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        string(tt-contrato.gastos)
            ELSE ""
        IF NOT tt-contrato.consignacion THEN
        "gasto para discriminar"
            ELSE "" 
        */
        .

  END.
  OUTPUT CLOSE.

  MESSAGE 'Termino .....' VIEW-AS ALERT-BOX.

