define input parameter fecha_desde as date.
define input parameter fecha_hasta as date.
define input parameter p_sucursal as integer.
define input parameter p_articulo as integer.
define input parameter p_calidad as integer.

define var v_tambores as integer.
define var v_kilos as decimal.
define var p_articulo_2 as integer.
DEFINE VARIABLE dCitral AS DECIMAL    NO-UNDO.

for each produccion_industria.
    delete produccion_industria.
end.

/**********  LOTES DE JUGO 2 ALTERNATIVA POR TAMBORES  **************************************************/
v_tambores = 0.
v_kilos = 0.
for each tambores_industria where tambores_industria.id_tipotambor = 1
                              and (tambores_industria.fecha >= fecha_desde)
                              and (tambores_industria.fecha <= fecha_hasta)
                              and (if (p_sucursal > 0) then (tambores_industria.id_sucursal = p_sucursal) else true)
                              and (if (p_articulo > 0) then (tambores_industria.id_articulo = p_articulo) else true)                     
                           no-lock
                           break by year(tambores_industria.fecha)
                                 by tambores_industria.id_lote
                                 by tambores_industria.id_articulo
                                 by tambores_industria.id_envase.
    
    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
    dCitral = dCitral + tambores_industria.citral.
    if last-of(tambores_industria.id_envase) then do:
    
        FIND FIRST productos_terminados of tambores_industria no-lock no-error.
        FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
        FIND FIRST comercial.sucursales of tambores_industria no-lock no-error.
        FIND FIRST envases_prod of tambores_industria no-lock no-error.
        find FIRST r_productos_calidad where r_productos_calidad.id_articulo = tambores_industria.id_articulo
                                         and r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                   no-lock no-error.
        
        create produccion_industria.
        assign produccion_industria.id_empresa          = tambores_industria.id_empresa
               produccion_industria.id_sucursal         = tambores_industria.id_sucursal
               produccion_industria.id_tipotambor       = tambores_industria.id_tipotambor
               produccion_industria.nromov              = tambores_industria.nromov
               produccion_industria.fecha               = tambores_industria.fecha
               produccion_industria.id_lote             = tambores_industria.id_lote
               produccion_industria.anio_lote           = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2))
               produccion_industria.tambores            = v_tambores
               produccion_industria.kilos               = v_kilos
               produccion_industria.kilos_tambor        = v_kilos / v_tambores
               produccion_industria.citral              = dCitral
               produccion_industria.id_articulo         = tambores_industria.id_articulo
               produccion_industria.id_calidad          = tambores_industria.id_calidad
               produccion_industria.envase              = envases_prod.descripcion
               produccion_industria.id_envase           = tambores_industria.id_envase
               produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion).
    
               if available comercial.sucursales then       
                   assign produccion_industria.sucursal            = sucursales.nombre.
    
               if available productos_terminados then       
                   assign produccion_industria.articulo            = productos_terminados.abreviatura.

               if available calidades then       
                   assign produccion_industria.calidad            = calidades.abreviatura.
               
               if available contratos then
                        assign produccion_general.orden_fabricacion = string(contratos.orden_fabricacion)
                               produccion_industria.id_contrato = contratos.id_contrato.

               if available r_productos_calidad then
                    assign produccion_industria.kilos_400 = v_kilos * r_productos_calidad.coeficiente.

        v_tambores = 0.
        v_kilos = 0.
    end.
end.                      

/***************************************************************************************************/




/**********  LOTES DE ACEITE 2 ALTERNATIVA POR TAMBORES  **************************************************/

for each tambores_industria where tambores_industria.id_tipotambor = 2
                              and (tambores_industria.fecha >= fecha_desde)
                              and (tambores_industria.fecha <= fecha_hasta)
                              and (if (p_sucursal > 0) then (tambores_industria.id_sucursal = p_sucursal) else true)
                              and (if (p_articulo > 0) then (tambores_industria.id_articulo = p_articulo) else true)                     
                           no-lock.
    
   
        find productos_terminados of tambores_industria no-lock no-error.
        find comercial.sucursales of tambores_industria no-lock no-error.
        find envases_prod of tambores_industria no-lock no-error.
        
        create produccion_industria.
        assign produccion_industria.id_empresa          = tambores_industria.id_empresa
               produccion_industria.id_sucursal         = tambores_industria.id_sucursal
               produccion_industria.fecha               = tambores_industria.fecha
               produccion_industria.id_lote             = tambores_industria.id_tambor
               produccion_industria.anio_lote           = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2))
               produccion_industria.tambores            = 1
               produccion_industria.kilos               = tambores_industria.kilos_tambor
               produccion_industria.kilos_tambor        = tambores_industria.kilos_tambor
               produccion_industria.id_articulo         = tambores_industria.id_articulo
               produccion_industria.envase              = envases_prod.descripcion
               produccion_industria.id_envase           = tambores_industria.id_envase
               produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion).
    
               if available comercial.sucursales then       
                   assign produccion_industria.sucursal            = sucursales.nombre.
    
               if available productos_terminados then       
                   assign produccion_industria.articulo            = productos_terminados.abreviatura.
end.                      

/***************************************************************************************************/


/*
/******************** LOTES DE ACEITE **************************************************************/

for each lotes_aceite where lotes_aceite.fecha >= fecha_desde
                        and lotes_aceite.fecha <= fecha_hasta
                        and if p_sucursal > 0 then lotes_aceite.id_sucursal = p_sucursal else true
                        and if p_articulo > 0 then lotes_aceite.id_articulo = p_articulo else true
                        no-lock.
    
    v_tambores = 0.
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
           produccion_industria.anio_lote           = integer(substring(string(year(lotes_aceite.fecha)),3,2))
           produccion_industria.tambores            = v_tambores
           produccion_industria.kilos               = v_kilos
           produccion_industria.kilos_tambor        = lotes_aceite.peso_neto
           produccion_industria.id_articulo         = lotes_aceite.id_articulo
           produccion_industria.id_envase           = lotes_aceite.id_envase
           produccion_industria.id_produccion_industria = NEXT-VALUE(rep_produccion). 
           
           if available sucursales then       
               assign produccion_industria.sucursal            = sucursales.nombre.

           if available productos_terminados then       
               assign produccion_industria.articulo            = productos_terminados.abreviatura.
           
           if available envases_prod then
               assign produccion_industria.envase              = envases_prod.abreviatura.
               
           if available contratos then
                    assign produccion_general.orden_fabricacion = string(contratos.orden_fabricacion)
                           produccion_industria.id_contrato = contratos.id_contrato.

    

end.

/***************************************************************************************************/
*/
