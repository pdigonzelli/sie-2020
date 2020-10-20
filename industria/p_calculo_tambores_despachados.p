define var v_tambores as integer.

for each lotes_despachados_contrato.
    delete lotes_despachados_contrato.
end.

for each stock_tambores where stock_tambores.id_contrato <> ""
                        break by stock_tambores.id_contrato
                              by stock_tambores.id_tipo_contrato
                              by stock_tambores.anio
                              by stock_tambores.item.
                              
    if last-of(stock_tambores.item) then
      do:
         
         v_tambores = 0.
         for each tambores_industria no-lock where tambores_industria.id_contrato_of     = stock_tambores.id_contrato
                                       and tambores_industria.id_tipocontrato_of = stock_tambores.id_tipo_contrato
                                       and tambores_industria.anio_of            = stock_tambores.anio
                                       and tambores_industria.item_of            = stock_tambores.item
                                       and tambores_industria.nro_remito         <> 0
                                       and tambores_industria.id_sucursal_remito <> 0.
                                       
            v_tambores = v_tambores + 1.
         end.

         create lotes_despachados_contrato.
         assign lotes_despachados_contrato.id_tipo_contrato = stock_tambores.id_tipo_contrato
                lotes_despachados_contrato.id_contrato      = stock_tambores.id_contrato
                lotes_despachados_contrato.anio             = stock_tambores.anio
                lotes_despachados_contrato.item             = stock_tambores.item
                lotes_despachados_contrato.cantidad         = v_tambores.

      end.
    
end.
