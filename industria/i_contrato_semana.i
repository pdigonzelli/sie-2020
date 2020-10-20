        find calidades of items_contrato no-lock no-error.
        find productos_terminados of items_contratos no-lock no-error.
        find contratos of items_contratos no-lock no-error.
        find clientes of contratos no-lock no-error.
        find clausulas of items_contratos no-lock no-error.
        find destinos of items_contratos no-lock no-error.
        FIND tipo_venta OF items_contratos NO-LOCK NO-ERROR.
        find first items_orden_entrega of items_contratos no-lock no-error.
        /* PRIMERO VEO SI HAY ALGUNA PARTE DE OE RELACIONADA A ESTA PARTE DE CONTRATO */ 
            if available items_orden_entrega then
                do: /* SI HAY ALGUNA, RECORRO TODAS LAS QUE HAYA, YA QUE PUEDEN SER
                        MAS DE UNA*/
                    FIND FIRST orden_entrega OF items_orden_entrega NO-LOCK NO-ERROR.
                    FIND FIRST agencias WHERE agencias.id_agencia = orden_entrega.id_agencia
                                         NO-LOCK NO-ERROR.
                    for each items_orden_entrega of items_contratos no-lock.
                        RUN genera-datos-reporte. /* ESTE PROCEDURE ESTA DEFINIDO EN EL PROGRAMA
                                                     p_contrato_semana_kilos.p O 
                                                     p_contrato_semana.p */
                    end.
               end.
            else
                do:
                    RUN genera-datos-reporte. /* ESTE PROCEDURE ESTA DEFINIDO EN EL PROGRAMA
                                                p_contrato_semana_kilos.p O 
                                                p_contrato_semana.p */
                end.



