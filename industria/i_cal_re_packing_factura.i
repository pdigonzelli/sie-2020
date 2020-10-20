v_importe_fac_fob = 0.
v_importe_fac_todos = 0.
v_cantidad_pl = 0.

FIND clausulas WHERE tt-contrato.id_condicion_venta = clausulas.id_clausula NO-LOCK NO-ERROR.
FIND destinos WHERE destinos.id_destino = packing_list.id_destino_grupo NO-LOCK NO-ERROR.

IF AVAILABLE items_venta THEN DO:
        /*
      /* DE KILOS A .............*/
      CASE items_venta.tipo_unidad:
          WHEN "T" THEN  /* Toneladas */
              v_cantidad = items_packing_list.kilos_brutos / 1000.
          WHEN "K" THEN  /* Kilos */
              v_cantidad = items_packing_list.kilos_brutos.
          WHEN "G" THEN DO:  /* Galones */
          END.
          WHEN "L" THEN DO: /* Libras */
          END.
      END CASE.
          */
            
      case items_venta.tipo_unidad:
        when "T" then /* TONELADAS SE MANEJA IGUAL QUE KILOS */
            do:
              ASSIGN
               re_cial_completo.importe_item_factura = items_venta.precio_origen  / 1000
               re_cial_completo.importe_item_fac_todos = (items_venta.precio_origen + items_venta.gastos) / 1000.
            end.
        when "K" then /* KILOS */
            do:
             ASSIGN
               re_cial_completo.importe_item_factura = items_venta.precio_origen 
               re_cial_completo.importe_item_fac_todos = (items_venta.precio_origen + items_venta.gastos).
            end.
        when "G" then /* GALONES */                    
            do:
             ASSIGN
               re_cial_completo.importe_item_factura = (items_venta.precio_origen  * 53.6) / (IF tt-contrato.coef >= 1.25 THEN 260 ELSE 250)
               re_cial_completo.importe_item_fac_todos = (items_venta.precio_origen + items_venta.gastos) * 53.6  / (IF tt-contrato.coef >= 1.25 THEN 260 ELSE 250) .
            end.
        when "L" then /* LIBRAS */
            do:
             ASSIGN
               re_cial_completo.importe_item_factura = items_venta.precio_origen  * 2.20462
               re_cial_completo.importe_item_fac_todos = (items_venta.precio_origen + items_venta.gastos) * 2.20462.
            end.
      END case.

    ASSIGN
        re_cial_completo.importe_item_factura   = re_cial_completo.importe_item_factura 
        * IF ( items_packing_list.id_articulo = 54 OR items_packing_list.id_articulo = 55 ) THEN
            items_packing_list.kilos_contenedor ELSE items_packing_list.kilos
        re_cial_completo.importe_item_fac_todos = re_cial_completo.importe_item_fac_todos 
        * IF ( items_packing_list.id_articulo = 54 OR items_packing_list.id_articulo = 55 ) THEN
                           items_packing_list.kilos_contenedor ELSE items_packing_list.kilos.
            
        /*
      ASSIGN
        re_cial_completo.importe_item_factura   = (items_venta.precio_origen * items_venta.cantidad)
        re_cial_completo.importe_item_fac_todos = ((items_venta.precio_origen + items_venta.gastos) * items_venta.cantidad).*/

         FIND FIRST subd_vtas OF items_venta NO-LOCK NO-ERROR.
         IF AVAILABLE subd_vtas THEN DO:
                  
                     RUN p_fob_fac.p (INPUT ROWID(subd_vtas), 
                                      OUTPUT v_importe_fac_fob,
                                      OUTPUT v_importe_fac_todos ).
                  
                  

                       RUN getCobranzasIndust.p (INPUT ROWID(subd_vtas), 
                                        OUTPUT v_fechas_cobranzas,
                                        OUTPUT v_cobranzas).
    
                       IF subd_vtas.id_tipo_venta = 3 THEN DO:
                           v_consignacion = TRUE.
                       END.
               FIND tipo_moneda WHERE tipo_moneda.id_moneda = subd_vtas.id_moneda_origen NO-LOCK NO-ERROR.
               
               ASSIGN
                  re_cial_completo.id_punto_venta     = subd_vtas.id_punto_venta
                  re_cial_completo.nromov             = subd_vtas.nromov
                  re_cial_completo.factura            = string(subd_vtas.id_punto_venta,"9999") + STRING(subd_vtas.nro_comp,"99999999")
                  re_cial_completo.fecha_factura      = subd_vtas.fecha_comp
                  re_cial_completo.vto_factura        = subd_vtas.vencimiento
                  re_cial_completo.importe_factura    = v_importe_fac_fob
                  re_cial_completo.importe_fac_todos  = v_importe_fac_todos
                  re_cial_completo.moneda_venta      = tipo_moneda.descripcion
                  re_cial_completo.unidad_venta       = items_venta.tipo_unidad.
               

          END.
          
  END. 
      
      v_precio_fob = v_importe_item_fac_fob / v_cantidad_contratos.
      
        
