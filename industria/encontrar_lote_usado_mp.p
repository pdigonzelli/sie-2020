define var i as integer.
for each tambores_industria where id_lote = 856
                              and id_tipotambor = 3
                              and year(fecha) = 2001
                              break by id_sucursal_destino
                                    by id_tipotambor_destino
                                    by nromov_destino.
    i = i + 1.
    IF LAST-OF(nromov_destino) then do:
        find lotes_jugo where lotes_jugo.id_sucursal = tambores_industria.id_sucursal_destino
                          and lotes_jugo.id_tipotambor = tambores_industria.id_tipotambor_destino
                          and lotes_jugo.nromov = tambores_industria.nromov_destino 
                          no-lock no-error.
        if available lotes_jugo then disp lotes_jugo.id_lote tambores_industria.id_tambor lotes_jugo.fecha i.

        
        find produccion_jugo where produccion_jugo.id_sucursal = tambores_industria.id_sucursal_destino
                          and produccion_jugo.id_tipotambor = tambores_industria.id_tipotambor_destino
                          and produccion_jugo.nromov = tambores_industria.nromov_destino 
                          no-lock no-error.
        if available produccion_jugo then disp produccion_jugo.id_produccion tambores_industria.id_tambor 
                                               produccion_jugo.fecha i.
                
        i = 0.
    end.                              
end.
