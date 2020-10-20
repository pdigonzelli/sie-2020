                              FIND FIRST sucursales OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
    
    CREATE produccion_industria.
    ASSIGN produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
           produccion_industria.id_empresa              = tambores_industria.id_empresa
           produccion_industria.id_sucursal             = tambores_industria.id_sucursal
           produccion_industria.fecha                   = tambores_industria.fecha
           produccion_industria.id_lote                 = v_lote
           produccion_industria.anio_lote               = tambores_industria.anio
           produccion_industria.tambores                = v_tambores
           produccion_industria.kilos                   = tambores_industria.kilos_tambor
           produccion_industria.id_articulo             = tambores_industria.id_articulo
           produccion_industria.id_calidad              = tambores_industria.id_calidad
           produccion_industria.id_envase               = tambores_industria.id_envase
           produccion_industria.kilos_tambor            = tambores_industria.kilos_tambor
           produccion_industria.kilos_400               = tambores_industria.citral
           produccion_industria.articulo                = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
           produccion_industria.envase                  = IF AVAILABLE envases_prod         THEN envases_prod.descripcion         ELSE "NONE"
           produccion_industria.sucursal                = IF AVAILABLE sucursales           THEN sucursales.nombre           ELSE "NONE"
           produccion_industria.nromov                  = tambores_industria.nromov
           produccion_industria.origen_water            = tambores_industria.origen_water
           .
        
