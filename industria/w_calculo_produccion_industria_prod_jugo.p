define input parameter fecha_desde as date.
define input parameter fecha_hasta as date.
define input parameter p_sucursal as integer.
define input parameter p_articulo as integer.
define input parameter p_calidad as integer.

define var v_tambores as integer.
define var v_kilos as decimal.

for each produccion_industria.
    delete produccion_industria.
end.

/**********  PRODUCCIONES DE JUGO  **************************************************/
FOR EACH produccion_jugo WHERE (produccion_jugo.fecha >= fecha_desde)
                           AND (produccion_jugo.fecha <= fecha_hasta)
                           AND (IF p_sucursal > 0 THEN produccion_jugo.id_sucursal = p_sucursal ELSE TRUE)
                           AND (IF p_articulo > 0 THEN produccion_jugo.id_articulo = p_articulo ELSE TRUE)                     
                           NO-LOCK.
    
    v_tambores = 0.
    v_kilos = 0.
    FOR EACH tambores_industria OF produccion_jugo NO-LOCK.
        v_tambores = v_tambores + 1.
        v_kilos = v_kilos + tambores_industria.kilos_tambor.
    end.                      
    
    FIND FIRST tambores_industria OF produccion_jugo NO-LOCK NO-ERROR.   
    FIND FIRST productos_terminados OF produccion_jugo NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF lotes_jugo NO-LOCK NO-ERROR.
    FIND FIRST comercial.sucursales OF produccion_jugo NO-LOCK NO-ERROR.
    FIND FIRST r_productos_calidad WHERE r_productos_calidad.id_articulo = tambores_industria.id_articulo
                                     AND r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                     NO-LOCK NO-ERROR.
    
    CREATE produccion_industria.
    ASSIGN produccion_industria.id_empresa          = produccion_jugo.id_empresa
           produccion_industria.id_sucursal         = produccion_jugo.id_sucursal
           produccion_industria.fecha               = produccion_jugo.fecha
           produccion_industria.id_lote             = produccion_jugo.id_produccion
           produccion_industria.anio_lote           = INTEGER(SUBSTRING(STRING(produccion_jugo.anio),3,2))
           produccion_industria.tambores            = v_tambores
           produccion_industria.kilos               = v_kilos
           produccion_industria.kilos_tambor        = IF AVAILABLE tambores_industria THEN tambores_industria.kilos_tambor ELSE 0
           produccion_industria.id_articulo         = produccion_jugo.id_articulo
           produccion_industria.id_calidad          = IF AVAILABLE tambores_industria THEN tambores_industria.id_calidad ELSE 0
           produccion_industria.id_envase           = IF AVAILABLE tambores_industria THEN tambores_industria.id_envase ELSE 0
           produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
           produccion_industria.c_usuario           = produccion_jugo.c_usuario
           produccion_industria.c_fecha             = produccion_jugo.c_fecha
           produccion_industria.c_hora              = produccion_jugo.c_hora.

           IF AVAILABLE sucursales THEN       
               ASSIGN produccion_industria.sucursal            = sucursales.nombre.

           IF AVAILABLE productos_terminados THEN       
               ASSIGN produccion_industria.articulo            = productos_terminados.abreviatura.
           
           IF AVAILABLE envases_prod THEN
               ASSIGN produccion_industria.envase              = envases_prod.abreviatura. 
           
           IF AVAILABLE r_productos_calidad THEN
                    ASSIGN produccion_industria.kilos_400 = v_kilos * r_productos_calidad.coeficiente.

end.                      

/***************************************************************************************************/

/******************** PRODUCCION DE ACEITES **************************************************************/

FOR EACH tambores_industria WHERE tambores_industria.id_tipotambor = 2
                              AND (tambores_industria.fecha >= fecha_desde)
                              AND (tambores_industria.fecha <= fecha_hasta)
                              AND (IF p_sucursal > 0 THEN tambores_industria.id_sucursal = p_sucursal ELSE TRUE)
                              AND (IF p_articulo > 0 THEN tambores_industria.id_articulo = p_articulo ELSE TRUE)
                              NO-LOCK.
    
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST comercial.sucursales OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
        
    CREATE produccion_industria.
    ASSIGN produccion_industria.id_empresa          = tambores_industria.id_empresa
           produccion_industria.id_sucursal         = tambores_industria.id_sucursal
           produccion_industria.fecha               = tambores_industria.fecha
           produccion_industria.id_lote             = tambores_industria.id_tambor
           produccion_industria.anio_lote           = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2))
           produccion_industria.tambores            = 1
           produccion_industria.kilos               = tambores_industria.kilos_tambor
           produccion_industria.kilos_tambor        = tambores_industria.kilos_tambor
           produccion_industria.id_articulo         = tambores_industria.id_articulo
           produccion_industria.id_envase           = tambores_industria.id_envase
           produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
           produccion_industria.c_usuario           = tambores_industria.c_usuario
           produccion_industria.c_fecha             = tambores_industria.c_fecha
           produccion_industria.c_hora              = tambores_industria.c_hora. 
           
           IF AVAILABLE sucursales THEN       
               ASSIGN produccion_industria.sucursal            = sucursales.nombre.

           IF AVAILABLE productos_terminados THEN       
               ASSIGN produccion_industria.articulo            = productos_terminados.abreviatura.
           
           IF AVAILABLE envases_prod THEN
               ASSIGN produccion_industria.envase              = envases_prod.abreviatura. 

    

end.

/***************************************************************************************************/
