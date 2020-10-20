IF INTEGER(items_orden_entrega.id_moneda_cambio:SCREEN-VALUE IN FRAME F-Main) > 0 AND
       INTEGER(items_orden_entrega.id_moneda_cambio:SCREEN-VALUE IN FRAME F-Main) <> 2 THEN DO:
        FIND LAST moneda_cotizacion WHERE moneda_cotizaciones.fecha <= TODAY
                                      AND moneda_cotizaciones.id_moneda_origen =
                                          INTEGER(items_orden_entrega.id_moneda_cambio:SCREEN-VALUE IN FRAME F-Main)
                                    NO-LOCK NO-ERROR.
        IF AVAILABLE moneda_cotizacion THEN DO:
            v_tipo_cambio = moneda_cotizaciones.tipo_cambio.
        END.
    END.
    ELSE
        v_tipo_cambio = 1.
    
    items_orden_entrega.tipo_cambio:SCREEN-VALUE IN FRAME F-Main = STRING(v_tipo_cambio).
    items_orden_entrega.importe_factura_dolar:SCREEN-VALUE IN FRAME F-Main = STRING(INTEGER(items_orden_entrega.TOTAL_factura:screen-value in frame F-Main) /
                                                                                    v_tipo_cambio).
    items_orden_entrega.importe_fob_dolar:SCREEN-VALUE IN FRAME F-Main = STRING(INTEGER(items_orden_entrega.fob_ton:screen-value in frame F-Main) /
                                                                                    v_tipo_cambio).
    IF v_comision > 0 THEN
        items_orden_entrega.importe_comision:screen-value in frame F-Main = string(v_comision).
    
    FIND LAST coeficientes_aduana NO-LOCK NO-ERROR.
    IF AVAILABLE coeficientes_aduana THEN DO:
        v_comision = DECIMAL(items_orden_entrega.importe_comision:screen-value in frame F-Main).
        v_coef = coeficientes_aduana.coeficiente.
        items_orden_entrega.coeficiente:screen-value in frame F-Main = string(v_coef).
 /**/   /*items_orden_entrega.coeficiente = v_coef.*/
    
        v_derecho_aduana = v_fob_ton * v_coef.
        items_orden_entrega.valor_aduana_derechos:screen-value in frame F-Main = string(v_derecho_aduana).
/**/    /*items_orden_entrega.valor_aduana_derechos = v_derecho_aduana.*/
    
        v_reintegro_aduana = v_fob_ton - v_comision.
        
        /*by facundo 06/10/2004*/
        IF v_reintegro_aduana > v_derecho_aduana THEN 
          v_reintegro_aduana = v_derecho_aduana.

        items_orden_entrega.valor_aduana_reintegro:screen-value in frame F-Main = string(v_reintegro_aduana).
/**/    /*items_orden_entrega.valor_aduana_reintegro = v_reintegro_aduana.*/
    
        v_derecho = v_derecho_aduana * 0.05.
        items_orden_entrega.importe_derechos_exportacion:screen-value in frame F-Main = string(v_derecho).
/**/    /*items_orden_entrega.importe_derechos_exportacion = v_derecho.*/
            
        FIND LAST porcentaje_reint_articulo WHERE porcentaje_reint_articulo.id_articulo = 
                                                  INTEGER(items_orden_entrega.id_articulo)
                                            NO-LOCK NO-ERROR.
        IF AVAILABLE porcentaje_reint_articulo THEN DO:
            v_reintegro = v_reintegro_aduana * (porcentaje_reint_articulo.porcentaje / 100).
            items_orden_entrega.importe_reintegro_fijo:screen-value in frame F-Main = string(v_reintegro).
/**/        /*items_orden_entrega.importe_reintegro_fijo = v_reintegro.*/
        END.

        /**derechos y reintegros  by facundo**/
        items_orden_entrega.coeficiente                   = DECIMAL(items_orden_entrega.coeficiente:SCREEN-VALUE IN FRAME F-Main).
        items_orden_entrega.valor_aduana_derechos         = DECIMAL(items_orden_entrega.valor_aduana_derechos:SCREEN-VALUE IN FRAME F-Main).
        items_orden_entrega.valor_aduana_reintegro        = DECIMAL(items_orden_entrega.valor_aduana_reintegro:SCREEN-VALUE IN FRAME F-Main).
        items_orden_entrega.importe_derechos_exportacion  = DECIMAL(items_orden_entrega.importe_derechos_exportacion:SCREEN-VALUE IN FRAME F-Main).
        items_orden_entrega.importe_reintegro_fijo        = DECIMAL(items_orden_entrega.importe_reintegro_fijo:SCREEN-VALUE IN FRAME F-Main).
        
    END.
