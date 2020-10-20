DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE VARIABLE t AS INTEGER    NO-UNDO.
DEFINE VARIABLE j AS INTEGER    NO-UNDO.

ETIME(TRUE).

CURRENT-WINDOW:WIDTH = 150.

FOR EACH stock_historico NO-LOCK WHERE stock_historico.id_tipo_movimiento = 3 
                                   AND stock_historico.signo              = "-" 
                                   AND stock_historico.fecha             >= DATE("01/01/2004").

  FOR EACH items_factura NO-LOCK WHERE items_factura.id_sucursal              = stock_historico.id_suc_origen                             
                                   AND items_factura.id_tipo_movsto           = 122 
                                   AND items_factura.id_articulo              = stock_historico.id_articulo
                                   AND items_factura.fecha                    = stock_historico.fecha
                                   AND items_factura.id_tipotambor            = stock_historico.id_tipotambor
                                   AND STRING(items_factura.nro_lote, "x(7)") = STRING(stock_historico.id_lote, "9999") + "/" + SUBSTRING(STRING(stock_historico.anio, "9999"), 3, 2).
    
    /*DISP items_factura.nro items_factura.ITEM items_factura.desde_lote items_factura.hasta_lote stock_historico.tambor_desde stock_historico.tambor_hasta WITH WIDTH 150.*/    
    DO i = items_factura.desde_lote TO items_factura.hasta_lote:              

      FIND FIRST r_tambor_remito WHERE r_tambor_remito.id_empresa         = stock_historico.id_empresa
                                   AND r_tambor_remito.id_sucursal        = stock_historico.id_sucursal
                                   AND r_tambor_remito.id_tipotambor      = stock_historico.id_tipotambor
                                   AND r_tambor_remito.nromov             = stock_historico.nromov
                                   AND r_tambor_remito.id_tambor          = i
                                   AND r_tambor_remito.id_sucursal_remito = items_factura.id_sucursal
                                   AND r_tambor_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                                   AND r_tambor_remito.nro_remito         = items_factura.nro
                                   AND r_tambor_remito.ITEM_factura       = items_factura.ITEM
                                NO-ERROR.
      IF NOT AVAILABLE r_tambor_remito THEN DO:          
        /*grabo la relacion*/
        
        CREATE r_tambor_remito.
        ASSIGN r_tambor_remito.id_empresa         = stock_historico.id_empresa
               r_tambor_remito.id_sucursal        = stock_historico.id_sucursal
               r_tambor_remito.id_tipotambor      = stock_historico.id_tipotambor
               r_tambor_remito.nromov             = stock_historico.nromov
               r_tambor_remito.id_tambor          = i
               r_tambor_remito.id_sucursal_remito = items_factura.id_sucursal
               r_tambor_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
               r_tambor_remito.nro_remito         = items_factura.nro
               r_tambor_remito.ITEM_factura       = items_factura.ITEM
               r_tambor_remito.fecha              = stock_historico.fecha
               r_tambor_remito.c_usuario          = "facundo"
               r_tambor_remito.c_fecha            = TODAY
               r_tambor_remito.c_hora             = STRING(TIME, "HH:MM:SS").

      END.
    END.    
  END.

END.


t = ETIME / 1000.
DISP t.


/*
                                   AND items_factura.nro_lote       = (SUBSTRING ( STRING(stock_historico.id_lote, "9999" )   , 1 , 3 ) +  
                                                                       SUBSTRING(STRING(stock_historico.id_lote, "9999") , 4, 1  ) + "/" + 
                                                                       SUBSTRING(STRING(stock_historico.anio ), 3, 2 ) ) .

*/
