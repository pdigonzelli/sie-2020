DEFINE INPUT PARAMETER v_sucursal AS INTEGER.
DEFINE INPUT PARAMETER v_articulo AS INTEGER.
DEFINE VAR v_tambores AS INTEGER.
DEFINE VAR v_kilos_total_lote AS DECIMAL.

/*
BREAK BY tambores_industria.id_lote
                               BY YEAR(tambores_industria.fecha)
                               BY tambores_industria.id_articulo
                               BY tambores_industria.id_calidad
                               BY tambores_industria.id_envase
                               BY tambores_industria.kilos_tambor.
*/


FOR EACH tambores_industria WHERE tambores_industria.id_articulo = v_articulo
                              AND tambores_industria.id_sucursal_ubicacion = v_sucursal
                              AND tambores_industria.id_locacion_ubicacion = 4
                            NO-LOCK
                            BREAK BY tambores_industria.nromov.
                         
    v_tambores = v_tambores + 1.
    v_kilos_total_lote = v_kilos_total_lote + tambores_industria.kilos_tambor.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
        FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST sucursales OF tambores_industria NO-LOCK NO-ERROR.

        CREATE produccion_industria.
        ASSIGN produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
               produccion_industria.id_empresa              = tambores_industria.id_empresa
               produccion_industria.id_sucursal             = tambores_industria.id_sucursal
               produccion_industria.fecha                   = tambores_industria.fecha
               produccion_industria.id_lote                 = tambores_industria.id_lote
               produccion_industria.anio_lote               = tambores_industria.anio
               produccion_industria.tambores                = v_tambores
               produccion_industria.kilos                   = v_kilos_total_lote
               produccion_industria.id_articulo             = tambores_industria.id_articulo
               produccion_industria.id_calidad              = tambores_industria.id_calidad
               produccion_industria.id_envase               = tambores_industria.id_envase
               produccion_industria.kilos_tambor            = tambores_industria.kilos_tambor
               produccion_industria.articulo                = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
               produccion_industria.calidad                 = IF AVAILABLE calidades            THEN calidades.descripcion            ELSE "NONE"
               produccion_industria.envase                  = IF AVAILABLE envases_prod         THEN envases_prod.descripcion         ELSE "NONE"
               produccion_industria.sucursal                = IF AVAILABLE sucursales           THEN sucursales.nombre                ELSE "NONE"
               produccion_industria.nromov                  = tambores_industria.nromov
               produccion_industria.origen_water            = tambores_industria.origen_water.


        v_tambores = 0.
        v_kilos_total_lote = 0.
    END.
END.
