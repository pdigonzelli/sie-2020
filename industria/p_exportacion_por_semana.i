   if available contratos then
   do:
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
            chWorkSheet:Range(crange):value = items_contratos.semana_entrega.
            cRange = "C" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.semana_entrega_hasta.
            cRange = "D" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.anio_semana_entrega.
            cRange = "E" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.id_calidad.
            if available calidades then
                do:
                    cRange = "F" + cfila.
                    chWorkSheet:Range(crange):value = calidades.descripcion.
                end.
            cRange = "G" + cfila.
            chWorkSheet:Range(crange):value = general.items_contratos.id_articulo.
            if available productos_terminados then
                do:
                    cRange = "H" + cfila.
                    chWorkSheet:Range(crange):value = productos_terminados.descripcion.
                end.
            if available clientes then
                do:
                    cRange = "I" + cfila.
                    chWorkSheet:Range(crange):value = clientes.razon_social.
                end.
            cRange = "J" + cfila.
            chWorkSheet:Range(crange):value = contratos.id_contrato.
            cRange = "K" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.item.
            cRange = "L" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.precio_venta.
            if available destinos then
                do:
                    cRange = "M" + cfila.
                    chWorkSheet:Range(crange):value = destinos.descripcion.
                end.
            cRange = "N" + cfila.
            chWorkSheet:Range(crange):value = items_contratos.cantidad.
            FIND contactos_industria WHERE contactos_industria.id_contacto = contratos.id_documentacion
                                        NO-LOCK NO-ERROR.
            IF AVAILABLE contactos_industria THEN
            DO:
               cRange = "O" + cfila.
               chWorkSheet:Range(crange):value = contactos_industria.nombre.
            END.
            FIND contactos_industria WHERE contactos_industria.id_contacto = contratos.id_cliente_destino
                                        NO-LOCK NO-ERROR.
            IF AVAILABLE contactos_industria THEN
            DO:
               cRange = "P" + cfila.
               chWorkSheet:Range(crange):value = contactos_industria.nombre.
            END.
    end.
