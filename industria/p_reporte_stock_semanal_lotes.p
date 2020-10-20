DEFINE VAR v_kilos as DECIMAL.
DEFINE VAR v_kilos_400 as DECIMAL.

/******************** TAMBORES DE LOTE JUGO Y ACEITE CON OF **********************************/
FOR EACH tambores_industria NO-LOCK 
            WHERE tambores_industria.id_locacion_ubicacion  = 4
              AND (tambores_industria.id_tipotambor         = 3
                OR tambores_industria.id_tipotambor         = 6
                OR tambores_industria.id_tipotambor         = 7)
              AND tambores_industria.id_contrato      <> ""
              BREAK BY tambores_industria.id_sucursal_ubicacion
                    BY tambores_industria.id_articulo
                    BY tambores_industria.anio
                    BY tambores_industria.id_calidad.


    ACCUMULATE tambores_industria.kilos_tambor (total by tambores_industria.anio).
    ACCUMULATE tambores_industria.kilos_tambor (total by tambores_industria.id_calidad).
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

    IF LAST-OF(tambores_industria.id_calidad) THEN DO:
        FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = tambores_industria.id_articulo
                                         AND r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                         NO-LOCK NO-ERROR. 
        v_kilos_400 = v_kilos_400 + IF AVAILABLE r_productos_calidad THEN (ACCUM TOTAL BY tambores_industria.id_calidad tambores_industria.kilos_tambor) * r_productos_calidad.coeficiente
                                                                       ELSE 0.
    END.
    IF LAST-OF(tambores_industria.anio) THEN DO:
                                          
        CREATE reporte_stock_semanal.
        ASSIGN reporte_stock_semanal.id_reporte     = 1
               reporte_stock_semanal.id_sucursal    = tambores_industria.id_sucursal_ubicacion
               reporte_stock_semanal.id_articulo    = tambores_industria.id_articulo
               reporte_stock_semanal.id_contrato    = "CON CONTRATO"
               reporte_stock_semanal.anio           = tambores_industria.anio
               reporte_stock_semanal.kilos          = v_kilos
               reporte_stock_semanal.kilos_400      = v_kilos_400.
            
        v_kilos_400 = 0.
        v_kilos = 0.
    END.
END.

/******************** TAMBORES DE LOTE JUGO Y ACEITE SIN OF **********************************/
FOR EACH tambores_industria NO-LOCK 
            WHERE tambores_industria.id_locacion_ubicacion  = 4
              AND (tambores_industria.id_tipotambor         = 3
                OR tambores_industria.id_tipotambor         = 6
                OR tambores_industria.id_tipotambor         = 7)
              AND tambores_industria.id_contrato      = ""
              BREAK BY tambores_industria.id_sucursal_ubicacion
                    BY tambores_industria.id_articulo
                    BY tambores_industria.anio
                    BY tambores_industria.id_calidad
                    .


    ACCUMULATE tambores_industria.kilos_tambor (total by tambores_industria.anio).
    ACCUMULATE tambores_industria.kilos_tambor (total by tambores_industria.id_calidad).
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

    IF LAST-OF(tambores_industria.id_calidad) THEN DO:
        FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = tambores_industria.id_articulo
                                         AND r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                         NO-LOCK NO-ERROR. 
        v_kilos_400 = v_kilos_400 + IF AVAILABLE r_productos_calidad THEN (ACCUM TOTAL BY tambores_industria.id_calidad tambores_industria.kilos_tambor) * r_productos_calidad.coeficiente
                                                                       ELSE 0.
    END.
    IF LAST-OF(tambores_industria.anio) THEN DO:
                                          
        
        CREATE reporte_stock_semanal.
        ASSIGN reporte_stock_semanal.id_reporte     = 1
               reporte_stock_semanal.id_sucursal    = tambores_industria.id_sucursal_ubicacion
               reporte_stock_semanal.id_articulo    = tambores_industria.id_articulo
               reporte_stock_semanal.id_contrato    = ""
               reporte_stock_semanal.anio           = tambores_industria.anio
               reporte_stock_semanal.kilos          = v_kilos
               reporte_stock_semanal.kilos_400      = v_kilos_400.
            
        v_kilos_400 = 0.
        v_kilos     = 0.
    END.
END.
