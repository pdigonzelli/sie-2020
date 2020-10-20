   cfila  = string(ifila).
                cRange = "A" + cfila.
                
               
                /*Formato Titulo*/
                chWorkSheet:range(crange):font:bold       = true.
                chWorkSheet:range(crange):font:size       = 10.
                chWorkSheet:range(crange):font:colorindex = 9.
    
                ifila = ifila + 1.
 
        /*>>> Datos */
            cfila  = string(ifila).
            cRange = "B" + cfila.
            chWorkSheet:Range(crange):value = orden_entrega.id_orden_entrega.
            cRange = "C" + cfila.
            chWorkSheet:Range(crange):value = orden_entrega.semana_embarque.
            if available estados_oe then
                do:
                    cRange = "D" + cfila.
                    chWorkSheet:Range(crange):value = estados_oe.descripcion.
                end.
            cRange = "E" + cfila.
            chWorkSheet:Range(crange):value = orden_entrega.fecha_embarque.
            if available vapores then
                do:
                    cRange = "F" + cfila.
                    chWorkSheet:Range(crange):value = vapores.descripcion.
                end.
            if available destinos then
                do:
                    cRange = "G" + cfila.
                    chWorkSheet:Range(crange):value = destinos.descripcion.
                end.    
            if available cliente then
                do:
                    cRange = "H" + cfila.
                    chWorkSheet:Range(crange):value = clientes.razon_social.
                end.
            cRange = "I" + cfila.
            chWorkSheet:Range(crange):value = orden_entrega.cantidad.
            if available productos_terminados then
                do:
                    cRange = "J" + cfila.
                    chWorkSheet:Range(crange):value = productos_terminados.descripcion.
                end.
            if available items_contratos then
                do:
                    cRange = "K" + cfila.
                    chWorkSheet:Range(crange):value = items_contratos.id_contrato.
                    cRange = "L" + cfila.
                    chWorkSheet:Range(crange):value = items_contratos.item.
                end.
            if available clausulas then
                do:
                    cRange = "M" + cfila.
                    chWorkSheet:Range(crange):value = clausulas.descripcion.
                end.    
            for each gastos_orden_entrega of orden_entrega no-lock.
                if gastos_orden_entrega.id_gasto = 5 then
                    do:
                        cRange = "O" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 9 then
                    do:
                        cRange = "P" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 14 then
                    do:
                        cRange = "Q" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 15 then
                    do:
                        cRange = "R" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 16 then
                    do:
                        cRange = "S" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 17 then
                    do:
                        cRange = "T" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 18 then
                    do:
                        cRange = "U" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 19 then
                    do:
                        cRange = "V" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 20 then
                    do:
                        cRange = "W" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 12 then
                    do:
                        cRange = "X" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
                if gastos_orden_entrega.id_gasto = 11 then
                    do:
                        cRange = "Y" + cfila.
                        chWorkSheet:Range(crange):value = gastos_orden_entrega.importe.
                    end.
            end.
    
