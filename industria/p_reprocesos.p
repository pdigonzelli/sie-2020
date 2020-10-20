DEFINE INPUT PARAMETER v_fecha_desde AS DATE.
DEFINE INPUT PARAMETER v_fecha_hasta AS DATE.

DEFINE VAR v_tambores AS INTEGER.
DEFINE VAR v_kilos AS DECIMAL.

FOR EACH lotes_reproceso.
    DELETE lotes_reproceso.
END.

FOR EACH tambores_industria WHERE id_tipotambor             = 3
                              AND id_articulo               > 900
                              AND tambores_industria.fecha  >= v_fecha_desde
                              AND tambores_industria.fecha  <= v_fecha_hasta
                              NO-LOCK
                              BREAK BY id_lote
                                    BY id_articulo
                                    BY id_calidad.
    v_tambores  = v_tambores + 1.
    v_kilos     = v_kilos + tambores_industria.kilos_tambor.
    IF LAST-OF (id_calidad) THEN DO:
        
        FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa       = tambores_industria.id_empresa_destino
                                AND lotes_jugo.id_sucursal      = tambores_industria.id_sucursal_destino
                                AND lotes_jugo.id_tipotambor    = tambores_industria.id_tipotambor_destino
                                AND lotes_jugo.nromov           = tambores_industria.nromov_destino
                                NO-LOCK NO-ERROR.
        IF AVAILABLE lotes_jugo THEN DO:
            IF YEAR(lotes_jugo.fecha) = 2002 THEN DO:
                FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
                FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
                FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
                
                CREATE lotes_reproceso.
                ASSIGN lotes_reproceso.id_empresa            = tambores_industria.id_empresa
                       lotes_reproceso.id_sucursal           = tambores_industria.id_sucursal
                       lotes_reproceso.id_tipotambor         = tambores_industria.id_tipotambor
                       lotes_reproceso.id_lote               = tambores_industria.id_lote
                       lotes_reproceso.anio_lote             = YEAR(tambores_industria.fecha)
                       lotes_reproceso.fecha                 = tambores_industria.fecha
                       lotes_reproceso.id_envase             = tambores_industria.id_envase
                       lotes_reproceso.envase                = envases_prod.descripcion
                       lotes_reproceso.id_articulo           = tambores_industria.id_articulo
                       lotes_reproceso.articulo              = productos_terminados.descripcion
                       lotes_reproceso.id_calidad            = tambores_industria.id_calidad
                       lotes_reproceso.calidad               = calidades.descripcion
                       lotes_reproceso.tambores              = v_tambores
                       lotes_reproceso.kilos                 = v_kilos
                       lotes_reproceso.kilos_400             = v_kilos
                       lotes_reproceso.id_sucursal_ubicacion = tambores_industria.id_sucursal_ubicacion
                       lotes_reproceso.sucursal              = "".



                DISP tambores_industria.id_lote 
                     tambores_industria.fecha
                     v_tambores.
                DISP lotes_jugo.id_lote 
                     lotes_jugo.fecha.
            END.
            
        END.
        v_tambores  = 0.
        v_kilos     = 0.
    END.    
END.
