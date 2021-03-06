define input parameter fecha_desde as date.
define input parameter fecha_hasta as date.
define input parameter p_sucursal as integer.
define input parameter p_articulo as integer.
define input parameter p_calidad as integer.

define var v_tambores as integer.
define var v_kilos as decimal.
define var p_articulo_2 as integer.

for each produccion_industria.
    delete produccion_industria.
end.

if p_articulo = 52 or p_articulo = 53 then
    do:
        p_articulo_2 = p_articulo + 900.       
    end.

            
/**********  LOTES DE JUGO  **************************************************/
for each lotes_jugo where (lotes_jugo.fecha >= fecha_desde)
                      and (lotes_jugo.fecha <= fecha_hasta)
                      and (if p_sucursal > 0 then lotes_jugo.id_sucursal = p_sucursal else true)
                      and 
                      (if p_articulo > 0 then 
                        (if p_articulo = 52 or p_articulo = 53 then 
                                lotes_jugo.id_articulo = p_articulo or 
                                lotes_jugo.id_articulo = p_articulo_2 
                         else
                                  lotes_jugo.id_articulo = p_articulo)  
                      else true)                     
                      and (if p_calidad > 0 then lotes_jugo.id_calidad = p_calidad else true)
                      no-lock.
    
    v_tambores = 0.
    v_kilos = 0.
    for each tambores_industria of lotes_jugo no-lock.
        v_tambores = v_tambores + 1.
        v_kilos = v_kilos + tambores_industria.kilos_tambor.
    end.                      
    
    find last tambores_industria of lotes_jugo no-lock no-error.   
    find contratos where contratos.id_contrato      = tambores_industria.id_contrato_of
                     and contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                     and contratos.anio             = tambores_industria.anio_of no-lock no-error.
    
    find productos_terminados of lotes_jugo no-lock no-error.
    find calidades of lotes_jugo no-lock no-error.
    find envases_prod of lotes_jugo no-lock no-error.
    find comercial.sucursales of lotes_jugo no-lock no-error.
    find FIRST r_productos_calidad where r_productos_calidad.id_articulo = lotes_jugo.id_articulo
                                   and r_productos_calidad.id_calidad = lotes_jugo.id_calidad
                                   no-lock no-error.
    
    create produccion_industria.
    assign produccion_industria.id_empresa          = lotes_jugo.id_empresa
           produccion_industria.id_sucursal         = lotes_jugo.id_sucursal
           produccion_industria.fecha               = lotes_jugo.fecha
           produccion_industria.id_lote             = lotes_jugo.id_lote
           produccion_industria.anio_lote           = INTEGER(SUBSTRING(STRING(lotes_jugo.anio),3,2))
           produccion_industria.tambores            = v_tambores
           produccion_industria.kilos               = v_kilos
           produccion_industria.kilos_tambor        = lotes_jugo.peso_neto
           produccion_industria.id_articulo         = lotes_jugo.id_articulo
           produccion_industria.id_calidad          = lotes_jugo.id_calidad
           produccion_industria.id_envase           = lotes_jugo.id_envase
           produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
           produccion_industria.c_usuario           = lotes_jugo.c_usuario
           produccion_industria.c_fecha             = lotes_jugo.c_fecha
           produccion_industria.c_hora              = lotes_jugo.c_hora.

           if available sucursales then       
               assign produccion_industria.sucursal            = sucursales.nombre.

           if available productos_terminados then       
               assign produccion_industria.articulo            = productos_terminados.abreviatura.
           
           if available calidades then
               assign produccion_industria.calidad             = calidades.abreviatura.
        
           if available envases_prod then
               assign produccion_industria.envase              = envases_prod.abreviatura. 
           
           if available contratos then
                    assign produccion_general.orden_fabricacion = string(contratos.orden_fabricacion)
                           produccion_industria.id_contrato = contratos.id_contrato.

           if available r_productos_calidad then
                    assign produccion_industria.kilos_400 = v_kilos * r_productos_calidad.coeficiente.

end.                      

/***************************************************************************************************/

/******************** LOTES DE ACEITE **************************************************************/

for each lotes_aceite where (lotes_aceite.fecha >= fecha_desde)
                        and (lotes_aceite.fecha <= fecha_hasta)
                        and (if p_sucursal > 0 then lotes_aceite.id_sucursal = p_sucursal else true)
                        and (if p_articulo > 0 then lotes_aceite.id_articulo = p_articulo else true)
                        no-lock
                        BREAK BY lotes_aceite.nro_partida
                              BY lotes_aceite.fecha
     .
    IF LAST-OF(lotes_aceite.nro_partida) THEN DO:
    
    v_tambores = 0.
    v_kilos = 0.
    for each tambores_industria of lotes_aceite no-lock.
        v_tambores = v_tambores + 1.
        v_kilos = v_kilos + tambores_industria.kilos_tambor.
    end.                      
    
    find last tambores_industria of lotes_aceite no-lock no-error.   
    find contratos where contratos.id_contrato      = tambores_industria.id_contrato_of
                     and contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                     and contratos.anio             = tambores_industria.anio_of no-lock no-error.

    find productos_terminados of lotes_aceite no-lock no-error.
    find comercial.sucursales of lotes_aceite no-lock no-error.
    find envases_prod of lotes_aceite no-lock no-error.
        
    create produccion_industria.
    assign produccion_industria.id_empresa          = lotes_aceite.id_empresa
           produccion_industria.id_sucursal         = lotes_aceite.id_sucursal
           produccion_industria.fecha               = lotes_aceite.fecha
           produccion_industria.id_lote             = lotes_aceite.id_lote
           produccion_industria.anio_lote           = INTEGER(SUBSTRING(STRING(lotes_aceite.anio),3,2))
           produccion_industria.tambores            = v_tambores
           produccion_industria.kilos               = v_kilos
           produccion_industria.kilos_tambor        = lotes_aceite.peso_neto
           produccion_industria.id_articulo         = lotes_aceite.id_articulo
           produccion_industria.id_envase           = lotes_aceite.id_envase
           produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion)
           produccion_industria.c_usuario           = lotes_aceite.c_usuario
           produccion_industria.c_fecha             = lotes_aceite.c_fecha
           produccion_industria.c_hora              = lotes_aceite.c_hora. 
           
           if available sucursales then       
               assign produccion_industria.sucursal            = sucursales.nombre.

           if available productos_terminados then       
               assign produccion_industria.articulo            = productos_terminados.abreviatura.
           
           if available envases_prod then
               assign produccion_industria.envase              = envases_prod.abreviatura.
               
           if available contratos then
                    assign produccion_general.orden_fabricacion = string(contratos.orden_fabricacion)
                           produccion_industria.id_contrato = contratos.id_contrato.

    
    END.
end.

/***************************************************************************************************/
