DEFINE VAR v_tam_des AS INTEGER.

FOR EACH items_contratos NO-LOCK WHERE id_contrato = "TW0150"
                                   AND ITEM = 5.
    FOR EACH orden_entrega OF items_contratos NO-LOCK.
        DISP orden_entrega.id_orden_entrega.
        FOR EACH items_packing_list WHERE items_packing_list.nro_orden_embarque  = 
                                          orden_entrega.id_orden_entrega
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

                        FIND packing_list OF items_packing_list NO-LOCK NO-ERROR.
                            IF AVAILABLE packing_list THEN DO:
                                MESSAGE "Packing List " packing_list.nro_pack_list 
                                        " contrato " packing_list.id_contrato 
                                                     packing_list.ITEM VIEW-AS ALERT-BOX.                                
                        END.
                    END.

            
        END.
    END.
END.
