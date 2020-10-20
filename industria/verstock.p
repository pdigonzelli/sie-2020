CURRENT-WINDOW:WIDTH = 200.

FOR EACH stock_historico NO-LOCK WHERE id_tipo_movimiento = 3 :
  FOR EACH remitos NO-LOCK WHERE remitos.fecha = stock_historico.fecha ,
    EACH items_factura NO-LOCK OF remitos WHERE items_factura.nro_lote = ( SUBSTRING ( STRING(stock_historico.id_lote, "9999" )   , 1 , 3) + "/" +
                                                                           SUBSTRING ( STRING(stock_historico.id_lote, "9999" )   , 1 , 3 ) + "/" + 
                                                                           SUBSTRING(STRING(stock_historico.id_lote, "9999") , 4, 1  ) + "/" + 
                                                                           SUBSTRING(STRING(stock_historico.anio ), 3, 2 ) ) 
                                          AND items_factura.id_articulo = stock_historico.id_articulo:


      
      DISPLAY remitos.nromov.
  END.

  
END.


/*| tipos_movimiento OF stock_historico NO-LOCK.
  DISP id_lote anio tambor_desde tambor_hasta descripcion 
     id_suc_origen id_suc_des fecha WITH WIDTH 200.


*/


/*

WHERE items_factura.nro_lote = ( SUBSTRING ( STRING(stock_historico.id_lote, "9999" )   , 1 , 3)
                                       SUBSTRING ( STRING(stock_historico.id_lote, "9999" )   , 1 , 3 ) + "/" + 
                                       SUBSTRING(STRING(stock_historico.id_lote, "9999") , 4, 1  ) + "/" + 
                                       SUBSTRING(STRING(stock_historico.anio ), 3, 2 ) )) :
  
  
*/
