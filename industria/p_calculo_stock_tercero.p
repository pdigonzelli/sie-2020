define input parameter p_suc as integer.
define var v_tambores as integer.
define var v_kilos as integer.
define var h_con as handle.
define var v_total_tambores_of as integer.
define var v_total_kilos_of as decimal.
define var v_articulo as integer.
define var v_envase as integer.

/*********************** CALCULO LOS TAMBORES DE PRODUCTOS DE TERCERO *************************/
v_tambores = 0.
v_kilos = 0.
FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_sucursal_ubicacion  = p_suc 
                                      AND tambores_industria.id_locacion_ubicacion  = 4
                                      AND tambores_industria.id_tipotambor          = 9
                                      BREAK BY tambores_industria.estado
                                            BY tambores_industria.nromov.
    
    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
    
    IF LAST-OF(tambores_industria.nromov) THEN DO:
        
        FIND FIRST sucursales OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST proveedores OF tambores_industria NO-LOCK NO-ERROR.
                                                                
        CREATE stock_tambores.
        ASSIGN stock_tambores.id_empresa            = tambores_industria.id_empresa
               stock_tambores.id_sucursal           = tambores_industria.id_sucursal
               stock_tambores.sucursal              = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NONE"
               stock_tambores.id_sucursal_ubicacion = p_suc 
               stock_tambores.id_lote               = tambores_industria.id_lote 
               stock_tambores.id_tipotambor         = tambores_industria.id_tipotambor
               stock_tambores.anio_lote             = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2))
               stock_tambores.id_envase             = tambores_industria.id_envase
               stock_tambores.envase                = IF AVAILABLE envases_prod THEN envases_prod.abreviatura ELSE "NONE"
               stock_tambores.id_articulo           = tambores_industria.id_articulo
               stock_tambores.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
               stock_tambores.calidad               = IF tambores_industria.estado THEN "P.Ter.Propio" ELSE "P.Ter.Extern"
               stock_tambores.id_calidad            = tambores_industria.id_calidad
               stock_tambores.tambores              = v_tambores
               stock_tambores.kilos                 = v_kilos
               stock_tambores.id_cliente            = tambores_industria.id_proveedor
               stock_tambores.orden_reporte         = 170
               stock_tambores.cliente               = IF AVAILABLE proveedores THEN proveedores.nombre
                                                                               ELSE "SIN CLIENTE ASIGNADO".
        v_tambores = 0.
        v_kilos = 0.
    END.
END.
      
/***************************************************************************************************/
