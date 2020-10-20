            FIND FIRST envases_prod of tambores_industria no-lock no-error.
            FIND FIRST productos_terminados of tambores_industria no-lock no-error.
            FIND FIRST calidades of tambores_industria no-lock no-error.
            FIND FIRST r_productos_calidad where r_productos_calidad.id_articulo = tambores_industria.id_articulo
                                             and r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                             no-lock no-error. 
            
            IF NOT AVAILABLE productos_terminados THEN DO:
                MESSAGE "Hay tambores con el siguiente articulo cargado " tambores_industria.id_articulo
                         VIEW-AS alert-box.
            END.
                CREATE stock_tambores.
                ASSIGN stock_tambores.id_empresa     = tambores_industria.id_empresa
                       stock_tambores.id_sucursal    = tambores_industria.id_sucursal
                       stock_tambores.sucursal       = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NONE"
                       stock_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion 
                       stock_tambores.id_lote        = tambores_industria.id_lote 
                       stock_tambores.id_tipotambor  = tambores_industria.id_tipotambor
                       stock_tambores.anio_lote      = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2)) 
                       stock_tambores.fecha          = tambores_industria.fecha
                       stock_tambores.id_envase      = tambores_industria.id_envase
                       stock_tambores.envase         = IF AVAILABLE envases_prod THEN envases_prod.abreviatura ELSE "NONE"
                       stock_tambores.id_articulo    = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 0
                       stock_tambores.articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
                       stock_tambores.orden_reporte  = p_orden                                                              
                       stock_tambores.id_estado      = IF AVAILABLE estados_lotes THEN estados_lotes.id_estado_lote ELSE 0    
                       stock_tambores.estado         = IF AVAILABLE estados_lotes THEN estados_lotes.descripcion ELSE "NONE"
                       stock_tambores.calidad        = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "NONE"
                       stock_tambores.id_calidad     = IF AVAILABLE calidades THEN calidades.id_calidad ELSE 0
                       stock_tambores.tambores       = (accum count by tambores_industria.anio tambores_industria.id_lote)
                       stock_tambores.kilos          = (accum total by tambores_industria.anio tambores_industria.kilos_tambor)
                       stock_tambores.kilos_400      = IF AVAILABLE r_productos_calidad THEN 
                                                            (accum total by tambores_industria.anio tambores_industria.kilos_tambor) * r_productos_calidad.coeficiente
                                                                                        ELSE 0.
