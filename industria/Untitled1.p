DEFINE VAR v_tam_des AS INTEGER.

FOR EACH items_contratos NO-LOCK WHERE id_contrato = "TW0150"
                                   AND ITEM = 3.
    FOR EACH orden_entrega OF items_contratos NO-LOCK.
        DISP orden_entrega.id_orden_entrega.
        FOR EACH packing_list WHERE packing_list.id_contrato      = items_contratos.id_contrato
                                AND packing_list.id_tipo_contrato = items_contratos.id_tipo_contrato
                                AND packing_list.anio             = items_contratos.anio
                                AND packing_list.item_contrato    = items_contratos.ITEM
                                NO-LOCK
                                BREAK BY packing_list.ITEM_contrato.
            MESSAGE "Packing List " packing_list.nro_pack_list VIEW-AS ALERT-BOX.
            FOR EACH items_packing_list WHERE items_packing_list.id_sucursal = packing_list.id_sucursal
                                          AND items_packing_list.id_packing_list = packing_list.id_packing_list
                                          AND integer(items_packing_list.nro_orden_embarque) = orden_entrega.id_orden_entrega
                                          NO-LOCK
                                          BREAK BY items_packing_list.nro_contenedor
                                                BY id_lote.
                MESSAGE "Cantidad P/L " items_packing_list.cantidad VIEW-AS ALERT-BOX.
                IF LAST-OF(id_lote) THEN
                    DO:
                        MESSAGE "Contenedor " items_packing_list.nro_contenedor
                                 " Lote " items_packing_list.nro_lote
                                 " tambores despachados " v_tam_des VIEW-AS ALERT-BOX.
                                v_tam_des = 0.
                    END.
            END.
        END.
    END.
END.
