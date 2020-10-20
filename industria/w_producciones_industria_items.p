define input parameter fecha_desde as date.
define input parameter fecha_hasta as date.
define input parameter p_sucursal as integer.
define input parameter p_articulo as integer.
define input parameter p_calidad as integer.

DEFINE VAR v_rep_produccion AS INTEGER.
define var v_tambores as integer.
define var v_kilos as decimal.
define var p_articulo_2 as integer.
DEFINE BUFFER bb_tambores FOR tambores_industria.

for each produccion_industria.
    delete produccion_industria.
end.
for each items_produccion_industria.
    delete items_produccion_industria.
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
    
    v_rep_produccion = NEXT-VALUE(rep_produccion).
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
           produccion_industria.id_produccion_industria = v_rep_produccion.

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


           v_tambores = 0.
        v_kilos = 0.
        FOR EACH bb_tambores WHERE bb_tambores.id_empresa_destino       = lotes_jugo.id_empresa
                               AND bb_tambores.id_sucursal_destino      = lotes_jugo.id_sucursal
                               AND bb_tambores.id_tipotambor_destino    = lotes_jugo.id_tipotambor
                               AND bb_tambores.nromov_destino           = lotes_jugo.nromov
                               NO-LOCK
                           BREAK BY  bb_tambores.id_tipotambor
                                 BY YEAR(bb_tambores.fecha)
                                 BY bb_tambores.id_lote
                                 BY bb_tambores.id_articulo
                                 BY bb_tambores.id_envase.

            v_tambores = v_tambores + 1.
            v_kilos = v_kilos + bb_tambores.kilos_tambor.

            IF LAST-OF(bb_tambores.id_envase) THEN DO:
                FIND FIRST tipostambor OF bb_tambores NO-LOCK NO-ERROR.
                find productos_terminados of bb_tambores no-lock no-error.
                find comercial.sucursales of bb_tambores no-lock no-error.
                find envases_prod of bb_tambores no-lock no-error.
                
                create items_produccion_industria.
                assign items_produccion_industria.id_produccion_industria = v_rep_produccion
                       items_produccion_industria.ITEM                    = bb_tambores.id_tipotambor + id_articulo + id_lote
                       items_produccion_industria.id_empresa              = bb_tambores.id_empresa
                       items_produccion_industria.id_sucursal             = bb_tambores.id_sucursal
                       items_produccion_industria.fecha                   = bb_tambores.fecha
                       items_produccion_industria.id_lote                 = bb_tambores.id_lote
                       items_produccion_industria.anio_lote               = INTEGER(SUBSTRING(STRING(bb_tambores.anio),3,2))
                       items_produccion_industria.tambores                = v_tambores
                       items_produccion_industria.kilos                   = v_kilos
                       items_produccion_industria.kilos_tambor            = v_kilos / v_tambores
                       items_produccion_industria.id_articulo             = bb_tambores.id_articulo
                       items_produccion_industria.envase                  = envases_prod.descripcion
                       items_produccion_industria.id_envase               = bb_tambores.id_envase
                       items_produccion_industria.tipotambor              = tipostambor.descripcion
                       items_produccion_industria.id_tipotambor           = bb_tambores.id_tipotambor.
            
                       if available comercial.sucursales then       
                           assign items_produccion_industria.sucursal            = sucursales.nombre.
            
                       if available productos_terminados then       
                           assign items_produccion_industria.articulo            = productos_terminados.abreviatura.
                       
                       if available contratos then
                                assign items_produccion_general.orden_fabricacion = string(contratos.orden_fabricacion)
                                       items_produccion_industria.id_contrato = contratos.id_contrato.
        
                v_tambores = 0.
                v_kilos = 0.
              
            END.
        END.
end.                      

/***************************************************************************************************/



/*
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
    
    if last-of(tambores_industria.id_envase) then do:
    
        find productos_terminados of tambores_industria no-lock no-error.
        find comercial.sucursales of tambores_industria no-lock no-error.
        find envases_prod of tambores_industria no-lock no-error.
        
        v_rep_produccion = NEXT-VALUE(rep_produccion).
        create produccion_industria.
        assign produccion_industria.id_empresa          = tambores_industria.id_empresa
               produccion_industria.id_sucursal         = tambores_industria.id_sucursal
               produccion_industria.fecha               = tambores_industria.fecha
               produccion_industria.id_lote             = tambores_industria.id_lote
               produccion_industria.anio_lote           = integer(substring(string(year(tambores_industria.fecha)),3,2))
               produccion_industria.tambores            = v_tambores
               produccion_industria.kilos               = v_kilos
               produccion_industria.kilos_tambor        = v_kilos / v_tambores
               produccion_industria.id_articulo         = tambores_industria.id_articulo
               produccion_industria.envase              = envases_prod.descripcion
               produccion_industria.id_envase           = tambores_industria.id_envase
               produccion_industria.id_produccion_industria = v_rep_produccion.
    
               if available comercial.sucursales then       
                   assign produccion_industria.sucursal            = sucursales.nombre.
    
               if available productos_terminados then       
                   assign produccion_industria.articulo            = productos_terminados.abreviatura.
               
               if available contratos then
                        assign produccion_general.orden_fabricacion = string(contratos.orden_fabricacion)
                               produccion_industria.id_contrato = contratos.id_contrato.

        v_tambores = 0.
        v_kilos = 0.
        FOR EACH bb_tambores WHERE bb_tambores.id_empresa_destino       = tambores_industria.id_empresa
                               AND bb_tambores.id_sucursal_destino      = tambores_industria.id_sucursal
                               AND bb_tambores.id_tipotambor_destino    = tambores_industria.id_tipotambor
                               AND bb_tambores.nromov_destino           = tambores_industria.nromov
                               NO-LOCK
                           BREAK BY  bb_tambores.id_tipotambor
                                 BY YEAR(bb_tambores.fecha)
                                 BY bb_tambores.id_lote
                                 BY bb_tambores.id_articulo
                                 BY bb_tambores.id_envase.

            v_tambores = v_tambores + 1.
            v_kilos = v_kilos + bb_tambores.kilos_tambor.

            IF LAST-OF(bb_tambores.id_tipotambor) THEN DO:
                FIND FIRST tipostambor OF bb_tambores NO-LOCK NO-ERROR.
                find productos_terminados of bb_tambores no-lock no-error.
                find comercial.sucursales of bb_tambores no-lock no-error.
                find envases_prod of bb_tambores no-lock no-error.
                
                create items_produccion_industria.
                assign items_produccion_industria.id_produccion_industria = v_rep_produccion
                       items_produccion_industria.ITEM                    = bb_tambores.id_tipotambor
                       items_produccion_industria.id_empresa              = bb_tambores.id_empresa
                       items_produccion_industria.id_sucursal             = bb_tambores.id_sucursal
                       items_produccion_industria.fecha                   = bb_tambores.fecha
                       items_produccion_industria.id_lote                 = bb_tambores.id_lote
                       items_produccion_industria.anio_lote               = integer(substring(string(year(bb_tambores.fecha)),3,2))
                       items_produccion_industria.tambores                = v_tambores
                       items_produccion_industria.kilos                   = v_kilos
                       items_produccion_industria.kilos_tambor            = v_kilos / v_tambores
                       items_produccion_industria.id_articulo             = bb_tambores.id_articulo
                       items_produccion_industria.envase                  = envases_prod.descripcion
                       items_produccion_industria.id_envase               = bb_tambores.id_envase
                       items_produccion_industria.tipotambor              = tipostambor.descripcion
                       items_produccion_industria.id_tipotambor           = bb_tambores.id_tipotambor.
            
                       if available comercial.sucursales then       
                           assign items_produccion_industria.sucursal            = sucursales.nombre.
            
                       if available productos_terminados then       
                           assign items_produccion_industria.articulo            = productos_terminados.abreviatura.
                       
                       if available contratos then
                                assign items_produccion_general.orden_fabricacion = string(contratos.orden_fabricacion)
                                       items_produccion_industria.id_contrato = contratos.id_contrato.
        
                v_tambores = 0.
                v_kilos = 0.
            END.
        END.
    end.
end.                      

/***************************************************************************************************/
*/


/*
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
               produccion_industria.anio_lote           = integer(substring(string(year(tambores_industria.fecha)),3,2))
               produccion_industria.tambores            = 1
               produccion_industria.kilos               = tambores_industria.kilos_tambor
               produccion_industria.kilos_tambor        = tambores_industria.kilos_tambor
               produccion_industria.id_articulo         = tambores_industria.id_articulo
               produccion_industria.envase              = envases_prod.descripcion
               produccion_industria.id_envase           = tambores_industria.id_envase.
    
               if available comercial.sucursales then       
                   assign produccion_industria.sucursal            = sucursales.nombre.
    
               if available productos_terminados then       
                   assign produccion_industria.articulo            = productos_terminados.abreviatura.
end.                      

/***************************************************************************************************/
*/

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
           produccion_industria.id_envase           = lotes_aceite.id_envase. 
           
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
