define input parameter p_rowid_contrato as rowid.
define var v_cantidad as integer.
define var v_lotes as character.
define var v_filtro as character.
define var v_contrato as char.
define var v_oe as char.
define var v_item as char.

for each lotes_items_contrato.
    delete lotes_items_contrato.
end.

find contratos where rowid(contratos) = p_rowid_contrato no-lock no-error.
if available contratos then
    do:
        v_contrato = contratos.id_contrato.
        for each items_contratos of contratos no-lock. 
            for each items_orden_entrega of items_contratos no-lock.
                
                if v_oe = "" then 
                    v_oe = string(items_orden_entrega.id_orden_entrega) + "-" + 
                           STRING(items_orden_entrega.ITEM_oe).
                else 
                    v_oe = v_oe + " " + string(items_orden_entrega.id_orden_entrega) + "-" + 
                                        STRING(items_orden_entrega.ITEM_oe).
                
                for each tambores_industria of items_orden_entrega no-lock break by id_lote.
                                    
                    v_cantidad = v_cantidad + 1.
                    if last-of(id_lote) then
                        do:
                            if v_lotes = "" then
                                do:
                                    v_lotes = string(tambores_industria.id_lote) + "/" + 
                                              SUBSTRING(STRING(tambores_industria.anio),3,2) + " (" +
                                              string(v_cantidad) + ")". 
                                     v_cantidad = 0.
                                 end.
                            else
                                do:
                                    v_lotes = v_lotes + " - " +
                                              string(tambores_industria.id_lote) + "/" + 
                                              SUBSTRING(STRING(tambores_industria.anio),3,2) + " (" +
                                              string(v_cantidad) + ")".
                                    v_cantidad = 0.
                                end.
                        end.
                end.
            end.
            
            create lotes_items_contrato.
            assign lotes_items_contrato.id_contrato         = items_contrato.id_contrato
                   lotes_items_contrato.id_tipo_contrato    = items_contrato.id_tipo_contrato
                   lotes_items_contrato.anio                = items_contrato.anio
                   lotes_items_contrato.item                = items_contrato.item
                   lotes_items_contrato.lotes               = v_lotes
                   lotes_items_contratos.orden_entregas     = v_oe.

             v_lotes = "".
             v_oe = "".
            
        end.
    end.
                   
    run p_reportes.p (input "contratos_items",
                      input "Reporte de Contratos",
                      input "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio),
                      input "").
                          
