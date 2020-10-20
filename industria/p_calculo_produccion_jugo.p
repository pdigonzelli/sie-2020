define input parameter p_suc as integer.
define var v_tambores as integer.
define var v_kilos as integer.
DEFINE VAR vKilos400 AS DECIMAL.
define var v_articulo as integer.
define var v_envase as integer.
define var v_total_tambores_of as integer.
define var v_total_kilos_of as decimal.

FIND FIRST comercial.sucursales where comercial.sucursales.id_sucursal = p_suc no-lock no-error.


/*********************** CALCULO LOS TAMBORES DE PRODUCCION DE JUGO   ******************************/
v_tambores = 0.
v_kilos = 0.

FOR EACH tambores_industria no-lock WHERE tambores_industria.id_sucursal_ubicacion  = p_suc
                                      AND tambores_industria.id_locacion_ubicacion  = 4
                                      AND tambores_industria.id_tipotambor          = 1
                                      BREAK BY tambores_industria.id_articulo
                                            BY tambores_industria.id_lote
                                            BY tambores_industria.anio
                                            BY tambores_industria.id_envase.
                                          
    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
        
    IF LAST-OF(tambores_industria.id_envase) THEN DO:
        
        FIND FIRST sucursales where sucursales.id_sucursal      = tambores_industria.id_sucursal no-lock no-error.
        FIND FIRST envases_prod where envases_prod.id_envase    = tambores_industria.id_envase no-lock no-error.
        FIND FIRST productos_terminados of tambores_industria no-lock no-error.
        FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = tambores_industria.id_articulo
                                         AND r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                         NO-LOCK NO-ERROR.
                
        CREATE stock_tambores.
        ASSIGN stock_tambores.id_empresa            = tambores_industria.id_empresa
               stock_tambores.id_sucursal           = tambores_industria.id_sucursal
               stock_tambores.sucursal              = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NONE"
               stock_tambores.id_sucursal_ubicacion = p_suc 
               stock_tambores.id_lote               = tambores_industria.id_lote 
               stock_tambores.id_tipotambor         = tambores_industria.id_tipotambor
               stock_tambores.anio_lote             = integer(substring(string(tambores_industria.anio),3,2)) 
               stock_tambores.id_envase             = tambores_industria.id_envase
               stock_tambores.envase                = IF AVAILABLE envases_prod THEN envases_prod.abreviatura ELSE "NONE"
               stock_tambores.id_articulo           = tambores_industria.id_articulo
               stock_tambores.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
               stock_tambores.id_calidad            = tambores_industria.id_calidad /*by facundo*/
               stock_tambores.calidad               = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "NONE"
               stock_tambores.tambores              = v_tambores
               stock_tambores.kilos                 = v_kilos
               stock_tambores.kilos_400             = IF AVAILABLE r_productos_calidad THEN (v_kilos * r_productos_calidad.coeficiente)
                                                                                       ELSE 0.
               
        IF (tambores_industria.id_articulo = 532 OR
            tambores_industria.id_articulo = 534 OR
            tambores_industria.id_articulo = 535 ) THEN ASSIGN stock_tambores.orden_reporte = 30.
                    
        IF (tambores_industria.id_articulo = 521 OR
            tambores_industria.id_articulo = 523 OR
            tambores_industria.id_articulo = 524 OR
            tambores_industria.id_articulo = 882) THEN ASSIGN stock_tambores.orden_reporte = 40.
                    
        IF (tambores_industria.id_articulo = 942) THEN ASSIGN stock_tambores.orden_reporte = 50.
                
        IF (tambores_industria.id_articulo = 42 OR
            tambores_industria.id_articulo = 43 ) THEN ASSIGN stock_tambores.orden_reporte = 60.
                                                               
        v_tambores = 0.
        v_kilos = 0.
    END.
END.
      
/***************************************************************************************************/


/*********************** CALCULO LOS TAMBORES DE SOBRANTE DE LOTES DE JUGO   ***********************/
v_tambores = 0.
v_kilos = 0.
    
FOR EACH tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion  = p_suc
                                      AND tambores_industria.id_locacion_ubicacion  = 4
                                      AND (tambores_industria.id_tipotambor         = 4
                                       OR tambores_industria.id_tipotambor          = 5)
                                     BREAK BY tambores_industria.nromov.
    
    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
    
    IF LAST-OF(tambores_industria.nromov) THEN DO:
        
        IF tambores_industria.id_tipotambor = 4 THEN DO: /*sobrante*/
            FIND FIRST sobrante WHERE sobrante.id_empresa               = tambores_industria.id_empresa
                                  AND sobrante.id_sucursal              = tambores_industria.id_sucursal
                                  AND sobrante.id_tipotambor_sobrante   = tambores_industria.id_tipotambor
                                  AND sobrante.nromov_sobrante          = tambores_industria.nromov
                                NO-LOCK NO-ERROR.
            IF AVAILABLE sobrante THEN
                FIND FIRST lotes_jugo of sobrante no-lock no-error.
        END.
        ELSE DO: /*arrastre*/
            FIND FIRST arrastre WHERE arrastre.id_empresa               = tambores_industria.id_empresa
                                  AND arrastre.id_sucursal              = tambores_industria.id_sucursal
                                  AND arrastre.id_tipotambor_arrastre   = tambores_industria.id_tipotambor
                                  AND arrastre.nromov_arrastre          = tambores_industria.nromov
                                NO-LOCK NO-ERROR.
            IF AVAILABLE arrastre THEN
                FIND FIRST lotes_jugo of arrastre no-lock no-error.
        END.
            
        FIND FIRST sucursales where sucursales.id_sucursal = tambores_industria.id_sucursal no-lock no-error.
        FIND FIRST envases_prod where envases_prod.id_envase = tambores_industria.id_envase no-lock no-error.
        FIND FIRST productos_terminados of lotes_jugo no-lock no-error.
        
        /* AGREGADO POR ADRIANCA EL 01/09/04 */
        FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = lotes_jugo.id_articulo
                                         AND r_productos_calidad.id_calidad = lotes_jugo.id_calidad
                                         NO-LOCK NO-ERROR.
        IF AVAILABLE r_productos_calidad THEN vKilos400 = (v_kilos * r_productos_calidad.coeficiente).
                                         ELSE vKilos400 = 0.

        /**888888888888888888*****************/
                /*
DEFINE VAR dbg AS LOGICAL.
dbg = DEBUGGER:INITIATE().
dbg = DEBUGGER:SET-BREAK().
*/
        
        CREATE stock_tambores.
        ASSIGN stock_tambores.id_empresa            = tambores_industria.id_empresa
               stock_tambores.id_sucursal           = tambores_industria.id_sucursal
               stock_tambores.sucursal              = IF AVAILABLE sucursales THEN sucursales.abreviatura ELSE "NONE"
               stock_tambores.id_sucursal_ubicacion = p_suc 
               stock_tambores.id_lote               = tambores_industria.id_lote 
               stock_tambores.id_tipotambor         = tambores_industria.id_tipotambor
               stock_tambores.anio_lote             = integer(substring(string(tambores_industria.anio),3,2)) 
               stock_tambores.id_envase             = tambores_industria.id_envase
               stock_tambores.envase                = IF AVAILABLE envases_prod THEN envases_prod.abreviatura ELSE "NONE"
               stock_tambores.id_articulo           = tambores_industria.id_articulo
               stock_tambores.articulo              = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
               stock_tambores.calidad               = IF tambores_industria.id_tipotambor = 4 THEN "SobranteJugoss" ELSE "ArrastreJugoss" 
               stock_tambores.id_calidad            = IF AVAILABLE lotes_jugo THEN lotes_jugo.id_calidad ELSE 999 /*by facundo*/
               stock_tambores.tambores              = v_tambores
               stock_tambores.kilos                 = v_kilos
               /*stock_tambores.kilos_400             = v_kilos * 1.25. by Adrianca */
               stock_tambores.kilos_400             = vKilos400. /* by Adrianca */

        IF tambores_industria.id_articulo = 53 THEN ASSIGN stock_tambores.orden_reporte = 30.
        IF tambores_industria.id_articulo = 52 THEN ASSIGN stock_tambores.orden_reporte = 40.
        IF (tambores_industria.id_articulo = 42 OR
            tambores_industria.id_articulo = 43) THEN ASSIGN stock_tambores.orden_reporte = 60.
        
        v_tambores = 0.
        v_kilos = 0.
        vKilos400 = 0.
    END.
END.
      
/***************************************************************************************************/
