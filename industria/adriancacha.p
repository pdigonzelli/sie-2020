FOR EACH tambores_industria WHERE id_lote = 140
                              AND id_articulo = 52
                              AND anio = 2003
                              BREAK BY nro_remito.
    IF LAST-OF(nro_remito) THEN DO:
        FOR EACH remitos WHERE /* id_cliente = 2460
                           AND */(id_sucursal = 95 OR id_sucursal = 96)
                           AND mercado = 1
                           AND c_fecha >= DATE("01/09/2003")
                           AND remitos.nro = tambores_industria.nro_remito.
            FIND FIRST clientes OF remitos NO-LOCK.

            FOR EACH items_factura OF remitos.
                FOR EACH stock_historico_tambores WHERE stock_historico_tambores.nromov = tambores_industria.nromov
                                                      AND id_tipo_movimiento = 3           
                                                      AND stock_historico_tambores.datos_adicionales MATCHES "*" + STRING(remitos.nro) + "*".
                    DISP clientes.razon_social items_factura.desde_lote items_factura.hasta_lote.
                    DISP stock_historico_tambores.
                    MESSAGE "Esta Bien?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE respuesta AS LOGICAL.
                    IF respuesta THEN DO:
                        IF signo = "-" THEN DO:
                            stock_historico_tambores.id_suc_des = 85.
                        END.
                        ELSE DO:
                            stock_historico_tambores.id_suc_origen = 85.
                        END.
                    END.
                END.
            END.
        END. 
    END.
END.
