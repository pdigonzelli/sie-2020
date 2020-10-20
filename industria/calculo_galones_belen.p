define var gall as decimal.
define var tam as integer initial 0.
define var total_gall as decimal.

for each lotes_jugo where id_lote = 45
                      and year(fecha) = 2001 
                      and id_articulo = 52 no-lock.
                      
    find last inspecciones_lote of lotes_jugo no-lock. 
    for each tambores_industria of lotes_jugo.
        disp id_tambor kilos_tambor galones_tambor.
        tam = tam + 1.
    end.
    
    find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
    message brix.brix round(inspecciones_lote.bx_correg,1) view-as alert-box.
    if available brix then 
        do:
           gall = (peso_neto / brix.pe) / 3.785.
        end.
        
     total_gall = round(gall,2) * tam.
     
     
     message "GAlones totales " total_gall " , galon por tambor " round(gall,2) " , brix " round(inspecciones_lote.bx_correg,1) ", tambores " tam  view-as alert-box.

    
end.                      
