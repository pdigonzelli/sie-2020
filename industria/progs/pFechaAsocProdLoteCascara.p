CURRENT-WINDOW:WIDTH = 150.
FOR EACH produccion_cascara
    WHERE produccion_cascara.fecha >= DATE('01/01/2007')
      AND produccion_cascara.fecha <= DATE('23/08/2007').

  FOR EACH stock_historico_tambores
      WHERE stock_historico_tambores.id_empresa         = produccion_cascara.id_empresa
        AND stock_historico_tambores.id_sucursal        = produccion_cascara.id_sucursal
        AND stock_historico_tambores.id_tipotambor      = produccion_cascara.id_tipotambor
        AND stock_historico_tambores.nromov             = produccion_cascara.id_produccion
        AND stock_historico_tambores.id_suc_ori         = produccion_cascara.id_sucursal
        AND stock_historico_tambores.id_tipo_movimiento = 35 
      NO-LOCK.
    
    FOR FIRST r_produccion_cascara_lote
        WHERE r_produccion_cascara_lote.id_sucursal_prod  = produccion_cascara.id_sucursal
          AND r_produccion_cascara_lote.id_produccion     = produccion_cascara.id_produccion
          AND r_produccion_cascara_lote.cantidad          = stock_historico_tambores.tambor_hasta
        .
        
      r_produccion_cascara_lote.c_fecha = stock_historico_tambores.fecha.
    END.
  
    /*  
    DISP stock_historico_tambores.id_lote
         stock_historico_tambores.anio
         stock_historico_tambores.tambor_desde
         stock_historico_tambores.tambor_hasta
         stock_historico_tambores.fecha
         stock_historico_tambores.id_suc_ori
         stock_historico_tambores.id_suc_des

         r_produccion_cascara_lote.cantidad
         r_produccion_cascara_lote.nromov_lote
         r_produccion_cascara_lote.c_fecha
         WITH WIDTH 150
         .*/
  END.

END.

 

