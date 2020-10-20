for each tambores_industria where id_lote = 5
                              and id_tipotambor = 3
                              and id_articulo = 953
                              and year(fecha) = 2002
                              break by nromov_destino.
                              
if last-of(nromov_destino) then
do:
disp id_lote id_tambor fecha.                            
find lotes_jugo where lotes_jugo.id_sucursal = tambores_industria.id_sucursal_destino
                  and lotes_jugo.id_tipotambor = tambores_industria.id_tipotambor_destino
                  and lotes_jugo.nromov = tambores_industria.nromov_destino.
                  
                  disp lotes_jugo.id_lote
                       lotes_jugo.fecha.
                       
end.                       
end.
