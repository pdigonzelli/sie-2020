DEFINE INPUT PARAMETER v_sucursal AS INTEGER.
DEFINE INPUT PARAMETER v_articulo AS INTEGER.
DEFINE VAR v_tambores AS INTEGER.
DEFINE VAR v_kilos_total_lote AS DECIMAL.


FOR EACH tambores_industria WHERE tambores_industria.id_articulo            = v_articulo
                              AND tambores_industria.id_sucursal_ubicacion  = v_sucursal
                              AND tambores_industria.id_locacion_ubicacion  = 4
                              AND tambores_industria.id_tipotambor          = 2
                          NO-LOCK 
                            BREAK BY tambores_industria.nromov.

    v_tambores = v_tambores + 1.
    v_kilos_total_lote = v_kilos_total_lote + tambores_industria.kilos_tambor.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
        FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST sucursales OF tambores_industria NO-LOCK NO-ERROR.
        CREATE stock_tambores.
            ASSIGN stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = tambores_industria.id_sucursal
                   stock_tambores.sucursal              = IF AVAILABLE sucursales           THEN sucursales.abreviatura           ELSE "NONE"
                   stock_tambores.id_sucursal_ubicacion = v_sucursal 
                   stock_tambores.id_lote               = tambores_industria.id_lote 
                   stock_tambores.id_tipotambor         = tambores_industria.id_tipotambor
                   stock_tambores.anio_lote             = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2)) 
                   stock_tambores.id_envase             = tambores_industria.id_envase
                   stock_tambores.envase                = IF AVAILABLE envases_prod         THEN envases_prod.abreviatura         ELSE "NONE"
                   stock_tambores.id_articulo           = tambores_industria.id_articulo
                   stock_tambores.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
                   stock_tambores.calidad               = IF AVAILABLE calidades            THEN calidades.abreviatura            ELSE "NONE"
                   stock_tambores.id_calidad            = tambores_industria.id_calidad
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos_total_lote.
        v_tambores = 0.
        v_kilos_total_lote = 0.
    END.
END.
