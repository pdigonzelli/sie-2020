DEFINE VAR v_kilos AS INTEGER.
v_kilos = 0.

FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_locacion_ubicacion  = 4
                                      AND (tambores_industria.id_tipotambor          = 1
                                        OR tambores_industria.id_tipotambor          = 4
                                        OR tambores_industria.id_tipotambor          = 5)
                                      BREAK BY tambores_industria.id_sucursal_ubicacion
                                            BY tambores_industria.anio.
                                          
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
        
    IF LAST-OF(tambores_industria.anio) THEN DO:
        
        CREATE reporte_stock_semanal.
        ASSIGN reporte_stock_semanal.id_reporte     = 1
               reporte_stock_semanal.id_sucursal    = tambores_industria.id_sucursal_ubicacion
               reporte_stock_semanal.id_articulo    = tambores_industria.id_articulo
               reporte_stock_semanal.id_contrato    = ""
               reporte_stock_semanal.anio           = tambores_industria.anio
               reporte_stock_semanal.kilos          = v_kilos
               reporte_stock_semanal.kilos_400      = 0.
               
        v_kilos = 0.
    END.
END.
