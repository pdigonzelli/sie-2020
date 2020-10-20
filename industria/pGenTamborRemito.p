
CURRENT-WINDOW:WIDTH = 150.
  
FOR EACH items_factura NO-LOCK WHERE items_factura.id_tipo_movsto           = 123. /*primero para remitos automaticos*/                                 
    
  FOR EACH stock_historico NO-LOCK WHERE stock_historico.fecha                 >= DATE("01/01/2005")
                                     AND items_factura.id_sucursal              = stock_historico.id_suc_origen
                                     AND stock_historico.id_tipo_movimiento     = 3 
                                     AND stock_historico.signo                  = "-" 
                                     AND items_factura.id_articulo              = stock_historico.id_articulo
                                     AND STRING(items_factura.nro_lote, "x(7)") = STRING(stock_historico.id_lote, "9999") + "/" + SUBSTRING(STRING(stock_historico.anio, "9999"), 3, 2)
                                     AND stock_historico.nromov                 = 54744.
    DISP stock_historico.nromov 
         stock_historico.id_lote
         stock_historic.anio
         stock_historico.tambor_desde
         stock_historico.tambor_hasta
         items_factura.ITEM
         items_factura.desde_lote
         items_factura.hasta_lote
         WITH WIDTH 150.

  END.

END.
