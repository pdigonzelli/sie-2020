DEFINE VAR v_cantidad_tambores_industria AS INTEGER.
DEFINE VAR v_kilos_tambores_industria AS DECIMAL.

FOR EACH items_orden_entrega WHERE (YEAR(items_orden_entrega.fecha) = YEAR(TODAY)
                                 OR items_orden_entrega.fecha       >= (TODAY - 120))
                             NO-LOCK.
    v_cantidad_tambores_industria   = 0.
    v_kilos_tambores_industria      = 0.
    FIND FIRST tambores_industria WHERE tambores_industria.id_orden_entrega = items_orden_entrega.id_orden_entrega
                                    AND tambores_industria.ITEM_oe          = items_orden_entrega.ITEM_oe
                                  NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
        FOR EACH tambores_industria WHERE tambores_industria.id_orden_entrega = items_orden_entrega.id_orden_entrega
                                      AND tambores_industria.ITEM_oe          = items_orden_entrega.ITEM_oe
                                    NO-LOCK.
            
            v_cantidad_tambores_industria   = v_cantidad_tambores_industria + 1.
            v_kilos_tambores_industria      = v_kilos_tambores_industria + tambores_industria.kilos_tambor.
        END.
    END.
    ELSE DO:
        /* SI ENTRO ACA ES QUE NO HAY NI UN TAMBOR ASOCIADO A ESTA PARTE DE OE, POR ENDE SEGURO
                QUE ESTA PENDIENTE */
        FIND FIRST tambores_industria WHERE tambores_industria.id_orden_entrega = items_orden_entrega.id_orden_entrega
                                        AND tambores_industria.ITEM_oe          = items_orden_entrega.ITEM_oe
                                        AND tambores_industria.id_sucursal_remito <> 0
                                      NO-LOCK NO-ERROR.
        IF AVAILABLE tambores_industria THEN DO:
            /* SIGNIFICA QUE ENCONTRO AL MENOS UN TAMBOR DE ESA OE DESPACHADO */

        END.
        ELSE DO:
            /* SIGNIFICA QUE NO ENCONTRO NI UN SOLO TAMBOR CON LOS DATOS DEL REMITO, ES
                MUY PROBABLE QUE ESTE PENDIENTE, SALVO QUE SE HAYA HECHO UN REMITO MANUAL.
                EN TAL CASO SALDRA SOLITO LA SOLUCION O SIMPLEMENTE SE IGONORA CUANDO
                SE AVISA QUE NO ESTA PENDIENTE */
            FIND FIRST tambores_industria WHERE tambores_industria.id_orden_entrega = items_orden_entrega.id_orden_entrega
                                            AND tambores_industria.ITEM_oe          = items_orden_entrega.ITEM_oe
                                            AND tambores_industria.id_sucursal_ubicacion = 91
                                          NO-LOCK NO-ERROR.
            IF AVAILABLE tambores_industria THEN DO:
                /* SIGNIFICA QUE ENCONTRO AL MENOS UN TAMBOR QUE HA SIDO FACTURADO */
    
            END.
            ELSE DO:
                /* SIGNIFICA QUE NO HAY NI UN TAMBOR FACTURADO */
            END.
            
        END.
    END.
END.
