FOR EACH items_packing_list WHERE nro_permiso_embarque = "EC01002899R" 
                            BREAK BY nro_permiso_embarque.
    IF LAST-OF(nro_permiso_embarque) THEN DO:
        FIND FIRST packing_list OF items_packing_list NO-LOCK NO-ERROR.
        IF AVAILABLE packing_list THEN DO:
            DISP items_packing_list.nro_permiso_embarque packing_list.id_tipo_pack_list.
            FIND permisos_embarque WHERE permisos_embarque.id_permiso_embarque = 
                                         items_packing_list.nro_permiso_embarque NO-ERROR.
            IF AVAILABLE permisos_embarque THEN DO:
                DISP permisos_embarque.id_permiso_embarque.
                ASSIGN permisos_embarque.tipo_pe = packing_list.id_tipo_pack_list.
            END.
        END.
    END.

END.
