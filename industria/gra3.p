for each tambores_industria where id_lote = 5
                              and id_tipotambor = 3
                              and id_articulo = 953
                              and year(fecha) = 2002
                              break by nromov_destino.
                              
if last-of(nromov_destino) then
disp id_lote id_tambor fecha id_sucursal_destino nromov_destino. 
