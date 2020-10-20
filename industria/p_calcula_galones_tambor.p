define input parameter  p_id_etiqueta  AS INTEGER.
define output parameter p_gall as decimal.



FIND tambores_industria WHERE tambores_industria.id_etiqueta = p_id_etiqueta NO-LOCK NO-ERROR.                                          
                
    find lotes_jugo of tambores_industria no-lock no-error.
    if available lotes_jugo then
    do:
        find last inspecciones_lote of lotes_jugo no-lock no-error.
        if available inspecciones_lote then
        do:
           find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
           if available brix then 
           do:
                p_gall = round((tambores_industria.kilos_tambor / brix.pe) / 3.785 , 2).
           end.        
        end.
    end.                  
