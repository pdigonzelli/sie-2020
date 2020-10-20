/*OUTPUT TO PRINTER.*/
  
FOR EACH remitos NO-LOCK WHERE fecha >= DATE("04/01/05") AND id_sucursal <> 95, 
    EACH items_factura NO-LOCK OF remitos WHERE nro_lote = "0569/04" AND id_articulo = 51 ,
    FIRST sucursales OF remitos NO-LOCK.
  DISP remitos.id_sucursal 
        sucursales.nombre
        remitos.nro remitos.estado items_factura.cantidad id_articulo WITH 2 COLUMNS.
  
END.
