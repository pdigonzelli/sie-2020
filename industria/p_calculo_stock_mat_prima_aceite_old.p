define input parameter p_suc as integer.
define var v_tambores as integer.
define var v_kilos as integer.
define var h_con as handle.
define var v_total_tambores_of as integer.
define var v_total_kilos_of as decimal.
define var v_articulo as integer.
define var v_envase as integer.


/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DE NARANJA Y MANDARINA (CODIGO 44)  ***********/
v_articulo = 44.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find comercial.sucursales where comercial.sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Ac.Nar.Mand."
                   stock_tambores.calidad        = "Ac.Nar.Mand."
                   stock_tambores.id_calidad     = 608
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = v_kilos
                   stock_tambores.orden_reporte  = 90.
        end.  
/***************************************************************************************************/

/****** CALCULO LOS TAMBORES DE PRODUCCION DE WATER DE NARANJA (CODIGO 48)  ***********/
v_articulo = 48.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "Pro.WaterNar"
                   stock_tambores.calidad               = "Pro.WaterNar"
                   stock_tambores.id_calidad            = 611
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos
                   stock_tambores.orden_reporte         = 120. 
        end.  
/***************************************************************************************************/


/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DE GRADO 2 DE PRODUCCION (CODIGO 50)  ***********/
v_articulo = 50.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "AceiteGrado2"
                   stock_tambores.calidad               = "AceiteGrado2"
                   stock_tambores.id_calidad            = 602
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/

/*********************** CALCULO LOS TAMBORES DE POT DE FOLDEADO (CODIGO 59)  **************/
run p_prod_aceite_x_lote.p (INPUT p_suc,
                            INPUT 59).
/***************************************************************************************************/

/*********************** CALCULO LOS TAMBORES DE ACEITE ESENCIAL DE LIMA (CODIGO 73)  **************/

v_tambores = 0.
v_articulo = 73.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Aceite Lima"
                   stock_tambores.calidad        = "Aceite Lima"
                   stock_tambores.id_calidad     = 607
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = v_kilos
                   stock_tambores.orden_reporte  = 160.
        end.  
/***************************************************************************************************/



/************************* CALCULO LOS TAMBORES DE ACEITE DESTILADO (CODIGO 76)  **************/
v_kilos = 0.
v_tambores = 0.
v_articulo = 76.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
        
end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Aceite Dest."
                   stock_tambores.calidad        = "Aceite Dest."
                   stock_tambores.id_calidad     = 606
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = (v_tambores * 170.184)
                   stock_tambores.orden_reporte  = 180.
        end.  
/***************************************************************************************************/


/************************* CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DE NARANJA (CODIGO 411)  **************/

v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = 411.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
    
    

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = 411
                   stock_tambores.articulo       = "Prd.Ac.Nar."
                   stock_tambores.calidad        = "Prd.Ac.Nar."
                   stock_tambores.id_calidad     = 620
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = (v_kilos)
                   stock_tambores.orden_reporte  = 85.
        end.  
/***************************************************************************************************/


/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DE PROD GRADO 2 (CODIGO 501)  ***********/
v_articulo = 501.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "AcLimon Gr 2"
                   stock_tambores.calidad               = "AcLimon Gr 2"
                   stock_tambores.id_calidad            = 622
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/



/************************* CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE (CODIGO 511)  **************/

v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = 511.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.
    
    

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = 511
                   stock_tambores.articulo       = "Prod.Aceite"
                   stock_tambores.calidad        = "Prod.Aceite"
                   stock_tambores.id_calidad     = 602
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = (v_kilos)
                   stock_tambores.orden_reporte  = 200.
        end.  
/***************************************************************************************************/




/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DESCERADO EN FRIO (CODIGO 512)  ***********/
v_articulo = 512.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "Ac.Desc.Frio"
                   stock_tambores.calidad               = "Ac.Desc.Frio"
                   stock_tambores.id_calidad            = 615
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/

/*
/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DESCERADO EN FRIO (CODIGO 512)  ***********/
v_articulo = 512.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 6
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "Ac.Desc.Frio"
                   stock_tambores.calidad               = "Ac.Desc.Frio"
                   stock_tambores.id_calidad            = 615
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/
*/
/****** CALCULO LOS TAMBORES DE PRODUCCION DE BORRA LIMPIA (CODIGO 513)  ***********/
v_articulo = 513.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "Borra Limpia"
                   stock_tambores.calidad               = "Borra Limpia"
                   stock_tambores.id_calidad            = 616
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/

/****** CALCULO LOS TAMBORES DE PRODUCCION DE BORRA P/LIMPIAR (CODIGO 514)  ***********/
v_articulo = 514.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "Bo.P/Limpiar"
                   stock_tambores.calidad               = "Bo.P/Limpiar"
                   stock_tambores.id_calidad            = 617
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/



/****** CALCULO LOS TAMBORES DE PRODUCCION DE BORRA FILTRADA (CODIGO 516)  ***********/
v_articulo = 516.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "Bo. Filtrada"
                   stock_tambores.calidad               = "Bo. Filtrada"
                   stock_tambores.id_calidad            = 618
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/


/****** CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DESC A TEMP AMBIENTE (CODIGO 517)  ***********/
v_articulo = 517.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "AcDescTempAm"
                   stock_tambores.calidad               = "AcDescTempAm"
                   stock_tambores.id_calidad            = 621
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/


/****** CALCULO LOS TAMBORES DE PRODUCCION DE BORRA SOBRANTE FILTRADA DE BORRA (CODIGO 518)  ***********/
v_articulo = 518.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "Sob.Bo.Filt."
                   stock_tambores.calidad               = "Sob.Bo.Filt."
                   stock_tambores.id_calidad            = 619
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/

/****** CALCULO LOS TAMBORES DE BORRA DESCERADO EN FRIO (CODIGO 519)  ***********/
v_articulo = 519.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "BorraDesFrio"
                   stock_tambores.calidad               = "BorraDesFrio"
                   stock_tambores.id_calidad            = 627
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/

/****** CALCULO LOS TAMBORES DE BORRA DESCERADA A TEMPERATURA AMBIENTE (CODIGO 520)  ***********/
v_articulo = 520.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa            = 1
                   stock_tambores.id_sucursal           = p_suc
                   stock_tambores.sucursal              = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion = p_suc 
                   stock_tambores.id_lote               = 0 
                   stock_tambores.id_tipotambor         = 2
                   stock_tambores.anio_lote             = integer(year(today)) 
                   stock_tambores.id_envase             = 501
                   stock_tambores.envase                = envases_prod.abreviatura
                   stock_tambores.id_articulo           = v_articulo
                   stock_tambores.articulo              = "BorraDesTemA"
                   stock_tambores.calidad               = "BorraDesTemA"
                   stock_tambores.id_calidad            = 628
                   stock_tambores.tambores              = v_tambores
                   stock_tambores.kilos                 = v_kilos.
        end.  
/***************************************************************************************************/

/*********************** CALCULO LOS TAMBORES DE PRODUCCION DE OIL PHASE (CODIGO 571)  **************/
v_articulo = 571.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Prd.OilPhase"
                   stock_tambores.calidad        = "Prd.OilPhase"
                   stock_tambores.id_calidad     = 600
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = v_kilos
                   stock_tambores.orden_reporte  = 113.
        end.  
/***************************************************************************************************/

/************************* CALCULO LOS TAMBORES DE PRODUCCION DE WATER PHASE (CODIGO 581)  **************/
v_articulo = 581.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Prd.WaterPh"
                   stock_tambores.calidad        = "Prd.WaterPh"
                   stock_tambores.id_calidad     = 601
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = v_kilos
                   stock_tambores.orden_reporte  = 108.
        end.  
/***************************************************************************************************/

/************************* CALCULO LOS TAMBORES DE PRODUCCION DE WATER PHASE (CODIGO 582)  **************/
v_articulo = 582.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Water Conce"
                   stock_tambores.calidad        = "Water Conce"
                   stock_tambores.id_calidad     = 623
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = v_kilos.
        end.  
/***************************************************************************************************/


/*********************** CALCULO LOS TAMBORES DE TERPENO DE FOLDEADO (CODIGO 741)  **************/
run p_prod_aceite_x_lote.p (INPUT p_suc,
                            INPUT 741).
/***************************************************************************************************/

/*************** CALCULO LOS TAMBORES DE PRODUCCION DE TERPENO ALTO BETAMINENO (CODIGO 742)  ********/
v_articulo = 742.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Terp.A.Beta"
                   stock_tambores.calidad        = "Terp.A.Beta"
                   stock_tambores.id_calidad     = 624
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = v_kilos.
        end.  
/***************************************************************************************************/

/******************* CALCULO LOS TAMBORES DE PRODUCCION DE TERPENO BAJO CITRAL (CODIGO 743)  **************/
v_articulo = 743.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "Terp.B.Citr"
                   stock_tambores.calidad        = "Terp.B.Citr"
                   stock_tambores.id_calidad     = 625
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = v_kilos.
        end.  
/***************************************************************************************************/

/******************* CALCULO LOS TAMBORES DE PRODUCCION DE ACEITE DESTILADO (CODIGO 762)  **************/
v_articulo = 762.
v_tambores = 0.
v_kilos = 0.
for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 2
                              and tambores_industria.id_articulo = v_articulo.

    v_tambores = v_tambores + 1.
    v_kilos = v_kilos + tambores_industria.kilos_tambor.

end.
    if v_tambores >= 1 then            
        do:
            find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.
            find envases_prod where envases_prod.id_envase = 501 no-lock no-error.
                                                                
            create stock_tambores.
            assign stock_tambores.id_empresa     = 1
                   stock_tambores.id_sucursal    = p_suc
                   stock_tambores.sucursal       = sucursales.abreviatura
                   stock_tambores.id_sucursal_ubicacion    = p_suc 
                   stock_tambores.id_lote        = 0 
                   stock_tambores.id_tipotambor  = 2
                   stock_tambores.anio_lote      = integer(year(today)) 
                   stock_tambores.id_envase      = 501
                   stock_tambores.envase         = envases_prod.abreviatura
                   stock_tambores.id_articulo    = v_articulo
                   stock_tambores.articulo       = "AcDestilado"
                   stock_tambores.calidad        = "AceiteDesti"
                   stock_tambores.id_calidad     = 625
                   stock_tambores.tambores       = v_tambores
                   stock_tambores.kilos          = v_kilos.
        end.  
/***************************************************************************************************/
