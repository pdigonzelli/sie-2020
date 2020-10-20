define input parameter vpi_nrolote like tambores_industria.id_lote.
define input parameter vpi_aniolote as integer format "99".
define input parameter vpi_articulo like tambores_industria.id_articulo.
define input parameter vpi_sucursal
                    like tambores_industria.id_sucursal_ubicacion.
define input parameter vpi_mercado like remitos.mercado.
define input-output parameter v_ok_lote as logical.
define input-output parameter v_id_calidad
                                like tambores_industria.id_calidad.
define input-output parameter v_kilos_tambor
                                like tambores_industria.kilos_tambor.
define input-output parameter v_id_envase
                                like tambores_industria.id_envase.
define input-output parameter v_id_ordenentrega like remitos.id_orden_entrega.

release lotes_jugo.
release lotes_aceite.
release produccion_jugo.
release productos_terceros.
release sobrante.
release sobrante_lotes_aceite.
release tambores_industria.
find lotes_jugo where 
   lotes_jugo.id_lote     = vpi_nrolote and 
   substring(string(lotes_jugo.anio,"9999"),3,2) 
                          = string(vpi_aniolote,"99") and
   lotes_jugo.id_articulo = vpi_articulo
   no-lock no-error.
                 
if not available lotes_jugo then
do:
  find last lotes_aceite use-index lotes_aceite where 
     lotes_aceite.id_lote     = vpi_nrolote and 
     substring(string(lotes_aceite.anio,"9999"),3,2) 
                              = string(vpi_aniolote,"99") and
     lotes_aceite.id_articulo = vpi_articulo
     no-lock no-error.
  if not available lotes_aceite then
  do:
    find produccion_jugo where
       produccion_jugo.id_produccion = vpi_nrolote and 
       substring(string(year(produccion_jugo.fecha),"9999"),3,2)
                                     = string(vpi_aniolote,"99") and
       produccion_jugo.id_articulo   = 
                                       vpi_articulo
       no-lock no-error.
    if not available produccion_jugo then 
    do:
      find productos_terceros where
         productos_terceros.id_lote     = vpi_nrolote and
         substring(string(productos_terceros.anio,"9999"),3,2)
                                        = string(vpi_aniolote,"99") and
         productos_terceros.id_articulo = vpi_articulo
         no-lock no-error.                        
      if not available productos_terceros then
      do:
        find first sobrante where
           sobrante.id_lote     = vpi_nrolote and
           substring(string(year(sobrante.fecha),"9999"),3,2) 
                                = string(vpi_aniolote,"99") and
           sobrante.id_articulo = vpi_articulo
           no-lock no-error.
        if not available sobrante then 
        do:
          find first sobrante_lotes_aceite where
             sobrante_lotes_aceite.id_lote  = vpi_nrolote and
             substring(string(year(sobrante_lotes_aceite.fecha),"9999"),3,2) 
                                            = string(vpi_aniolote,"99") and
             sobrante_lotes_aceite.id_articulo 
                                            = vpi_articulo
             no-lock no-error.
          if not available sobrante_lotes_aceite then
          do:
            find first tambores_industria where
               tambores_industria.id_tipotambor = 5 and
               tambores_industria.id_lote       = vpi_nrolote and
               substring(string(year(tambores_industria.fecha),"9999"),3,2) 
                                                = string(
                                                    vpi_aniolote,"99") and
               tambores_industria.id_articulo   = 
                                        vpi_articulo and
               tambores_industria.id_sucursal_ubicacion <> 85 and
               tambores_industria.id_sucursal_ubicacion = vpi_sucursal and
               tambores_industria.id_locacion_ubicacion <> 10
               no-lock no-error.
            if not available tambores_industria then
               assign v_ok_lote = false.
          end. /*--- not available sobrante_lotes... ---*/
        end. /*--- not available sobrante ---*/
      end. /*--- not avaialble productos_terceros ---*/
    end. /*--- not available produccion_jugo ---*/
  end. /*--- not available lotes_aceite ---*/
end. /*--- not available lotes_jugo ---*/

assign v_id_calidad      = 0
       v_kilos_tambor    = 0
       v_id_envase       = 0
       v_id_ordenentrega = 0.

if available tambores_industria then
   assign v_id_calidad   = tambores_industria.id_calidad
          v_id_envase    = tambores_industria.id_envase
          v_kilos_tambor = tambores_industria.kilos_tambor.

if available lotes_jugo then
do:
  if lotes_jugo.control_calidad and
     lotes_jugo.microbiologia then 
     do:
       find first tambores_industria of lotes_jugo where
          tambores_industria.anio                  = lotes_jugo.anio and
          tambores_industria.id_sucursal_ubicacion <> 85 and
          tambores_industria.id_sucursal_ubicacion = vpi_sucursal
          no-lock no-error.
       assign v_id_calidad   = tambores_industria.id_calidad
              v_kilos_tambor = tambores_industria.kilos_tambor
              v_id_envase    = tambores_industria.id_envase.
       if v_id_ordenentrega = 0 and
          vpi_mercado       <> 0 then
          assign v_id_ordenentrega = lotes_jugo.id_orden_entrega.
     end.
  else 
     do:
       message 
        "ESTE LOTE NO ESTA APROBADO POR CONTROL DE CALIDAD O MICROBIOLOGIA !!!"
        view-as alert-box.
       assign v_ok_lote = false.
     end.
end.
            
            if available lotes_aceite then do:
              find first tambores_industria of lotes_aceite where
                tambores_industria.anio                  = 
                                                lotes_aceite.anio and
                tambores_industria.id_sucursal_ubicacion <> 85 and
                tambores_industria.id_sucursal_ubicacion = vpi_sucursal
                no-lock no-error.
              assign v_kilos_tambor = tambores_industria.kilos_tambor
                     v_id_calidad   = tambores_industria.id_calidad
                     v_id_envase    = tambores_industria.id_envase.
              if v_id_ordenentrega = 0 and
                 vpi_mercado       <> 0 then
                 assign v_id_ordenentrega = lotes_aceite.id_orden_entrega.
            end.
            
            if available produccion_jugo then do:
              find first tambores_industria of produccion_jugo where
                   tambores_industria.id_sucursal_ubicacion <> 85 and
                   tambores_industria.id_sucursal_ubicacion = vpi_sucursal and
                   tambores_industria.id_locacion_ubicacion <> 10
                   no-lock no-error.
              assign v_id_calidad = tambores_industria.id_calidad
                     v_id_envase  = tambores_industria.id_envase.
            end.
            
            if available productos_terceros then do:
              find first tambores_industria of productos_terceros where
                   tambores_industria.id_sucursal_ubicacion <> 85 and
                   tambores_industria.id_sucursal_ubicacion = vpi_sucursal
                   no-lock no-error.
              assign v_kilos_tambor = tambores_industria.kilos_tambor
                     v_id_calidad   = tambores_industria.id_calidad
                     v_id_envase    = tambores_industria.id_envase.
            end.
            if available sobrante then do:
              find first tambores_industria where
                   tambores_industria.id_empresa = sobrante.id_empresa and
                   tambores_industria.id_sucursal = sobrante.id_sucursal and
                   tambores_industria.id_tipotambor =
                                            sobrante.id_tipotambor_sobrante and
                   tambores_industria.nromov = sobrante.nromov_sobrante and
                   tambores_industria.id_sucursal_ubicacion <> 85 and
                   tambores_industria.id_sucursal_ubicacion = vpi_sucursal
                   no-lock no-error.
              assign v_kilos_tambor = tambores_industria.kilos_tambor
                     v_id_calidad   = tambores_industria.id_calidad
                     v_id_envase    = tambores_industria.id_envase.
            end.
            if available sobrante_lotes_aceite then do:
              find first tambores_industria where
                   tambores_industria.id_empresa    = 
                                sobrante_lotes_aceite.id_empresa and
                   tambores_industria.id_sucursal   = 
                                sobrante_lotes_aceite.id_sucursal and
                   tambores_industria.id_tipotambor = 
                                sobrante_lotes_aceite.id_tipotambor_sobrante and
                   tambores_industria.nromov        = 
                                sobrante_lotes_aceite.nromov_sobrante and
                   tambores_industria.id_sucursal_ubicacion <> 85 and
                   tambores_industria.id_sucursal_ubicacion = vpi_sucursal
                   no-lock no-error.
              if not available tambores_industria then
                 assign v_ok_lote = false.
              else
                 assign v_id_calidad   = tambores_industria.id_calidad
                        v_id_envase    = tambores_industria.id_envase
                        v_kilos_tambor = tambores_industria.kilos_tambor.
            end.
            
