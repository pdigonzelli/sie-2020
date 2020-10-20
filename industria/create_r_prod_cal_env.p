FOR EACH items_venta WHERE id_punto_venta = 81 BREAK 
                                                BY items_venta.id_articulo
                                                BY items_venta.id_calidad
                                                BY items_venta.id_envase.
    IF LAST-OF(items_venta.id_envase) THEN DO:

    FIND FIRST r_productos_calidad_envase OF items_venta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE r_productos_calidad_envase THEN DO:
        FIND FIRST tambores_industria WHERE tambores_industria.id_articulo = items_venta.id_articulo
                                        AND tambores_industria.id_calidad = items_venta.id_calidad
                                        AND tambores_industria.id_envase = items_venta.id_envase
                                        NO-LOCK NO-ERROR.
        IF AVAILABLE tambores_industria THEN DO:
            CREATE r_productos_calidad_envase.
            ASSIGN r_productos_calidad_envase.id_articulo = tambores_industria.id_articulo
                   r_productos_calidad_envase.id_calidad = tambores_industria.id_calidad
                   r_productos_calidad_envase.id_envase = tambores_industria.id_envase
                   r_productos_calidad_envase.kilos = tambores_industria.kilos_tambor
                   r_productos_calidad_envase.kilos_nominal = tambores_industria.kilos_tambor
                   r_productos_calidad_envase.c_fecha = TODAY
                   r_productos_calidad_envase.c_usuario = "adrianca".

            DISP tambores_industria.kilos_tambor.
            
        END.
        IF NOT AVAILABLE tambores_industria THEN DO:
            CREATE r_productos_calidad_envase.
            ASSIGN r_productos_calidad_envase.id_articulo = items_venta.id_articulo
                   r_productos_calidad_envase.id_calidad = items_venta.id_calidad
                   r_productos_calidad_envase.id_envase = items_venta.id_envase
                   r_productos_calidad_envase.kilos = 0
                   r_productos_calidad_envase.kilos_nominal = 0
                   r_productos_calidad_envase.c_fecha = TODAY
                   r_productos_calidad_envase.c_usuario = "adrianca".

            DISP items_venta.id_articulo
                 items_venta.id_calidad
                 items_venta.id_envase.
        END.
        
    END.
    END.
END.
