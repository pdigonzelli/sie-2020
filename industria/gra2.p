for each items_factura where substring(nro_lote,1,4) = "0005"
                         and substring(nro_lote,6,2) = "02".
                         
find remitos of items_factura no-error.
if available remitos then disp nro_comprobante remitos.fecha remitos.estado hasta_lote. 

end                        
