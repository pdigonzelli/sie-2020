define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define output parameter p_kilos as decimal.
define output parameter p_kilos_brutos as decimal.
define output parameter p_cantidad_tambores as decimal.
define output parameter p_gall as decimal.
define output parameter p_gall_brix as decimal.

define var p_peso as decimal.
define var v_kilos_temp as decimal initial 0.
define var v_gall_tambor_temp as decimal initial 0.
define var v_gall_brix_temp as decimal initial 0.
define var v_total_tambores_lote as integer initial 0.
define var i as integer initial 0.

for each tambores_industria where tambores_industria.id_orden_entrega = p_id_orden_entrega
                                        break by tambores_industria.id_lote.
                                          
                
                v_kilos_temp = v_kilos_temp + tambores_industria.kilos_tambor.
                p_kilos_brutos = p_kilos_brutos + tambores_industria.kilos_tambor + tambores_industria.tara.
                p_peso = tambores_industria.kilos_tambor.
                p_cantidad_tambores = p_cantidad_tambores + 1.
                v_total_tambores_lote = v_total_tambores_lote + 1.
                
                if last-of(tambores_industria.id_lote) then
                    do:
                       p_kilos = p_kilos + v_kilos_temp.
                                              
                       find lotes_jugo of tambores_industria no-lock no-error.
                       if available lotes_jugo then
                           do:
                              find last inspecciones_lote of lotes_jugo no-lock no-error.
                              if available inspecciones_lote then
                                do:
                                   find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
                                   if available brix then 
                                    do:
                                       v_gall_tambor_temp   = round((p_peso / brix.pe) / 3.785 , 2).
                                       p_gall               = p_gall + (v_total_tambores_lote * v_gall_tambor_temp).
                                       v_gall_brix_temp     = v_gall_brix_temp + inspecciones_lote.bx_correg.
                                       i = i + 1.                                       
                                    end.
                                end.        
                           end.
                       v_kilos_temp = 0.
                       v_total_tambores_lote = 0.
                    end.                  
            end.
if i > 0 and v_gall_tambor_temp > 0 then 
    do:
        p_gall_brix     = v_gall_brix_temp / i.
    end.
