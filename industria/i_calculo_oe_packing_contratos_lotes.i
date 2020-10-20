            find clientes of contratos no-lock no-error.
            find contactos_industria where contactos_industria.id_contacto =
                                           contratos.id_broker no-lock no-error.
            
            if (v_semana_desde > 0 and v_semana_desde < 53) and
               (v_semana_hasta > 0 and v_semana_hasta < 53) then
                do:
                    if v_anio_desde = v_anio_hasta then
                        do:
                            for each items_contratos of contratos 
                                                     where items_contratos.semana_entrega >= v_semana_desde
                                                       and items_contratos.semana_entrega <= v_semana_hasta
                                                       and items_contratos.anio_semana_entrega = v_anio_desde
                                                       AND estado
                                                      no-lock.
                                 {..\industria\i_cal_oe_pack_cont_lotes_item_con.i}
                                
                            end.
                        end.
                    else
                        do:
                           for each items_contratos of contratos 
                                                    where ((items_contratos.semana_entrega >= v_semana_desde
                                                        and items_contratos.anio_semana_entrega = v_anio_desde) 
                                                       or (items_contratos.semana_entrega <= v_semana_hasta
                                                        and items_contratos.anio_semana_entrega = v_anio_hasta))
                                                       AND estado
                                                       no-lock.
            
                                {..\industria\i_cal_oe_pack_cont_lotes_item_con.i}
                           end.
                        end.    
                end.
            else
                DO:
                    for each items_contratos of contratos
                                             WHERE estado.
                        {..\industria\i_cal_oe_pack_cont_lotes_item_con.i}   
                    END.
                END.
