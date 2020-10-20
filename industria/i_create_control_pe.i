    CREATE control_permisos_embarque.
    ASSIGN control_permisos_embarque.id_permiso_embarque  = permisos_embarque.id_permiso_embarque
           control_permisos_embarque.id_aduana            = permisos_embarque.id_aduana
           control_permisos_embarque.anio                 = permisos_embarque.anio
           control_permisos_embarque.fecha_oficializacion = permisos_embarque.fecha_oficializacion
           control_permisos_embarque.semana_oficializacion = v_semana_of
           control_permisos_embarque.fecha_final          = permisos_embarque.fecha_oficializacion + 120
           control_permisos_embarque.semana_final         = v_semana_fin
           control_permisos_embarque.importe_pe           = permisos_embarque.importe.
    
    FOR EACH r_subd_ventas_embarque WHERE r_subd_ventas_embarque.id_aduana = permisos_embarque.id_aduana
                                      AND r_subd_ventas_embarque.anio = permisos_embarque.anio
                                      AND r_subd_ventas_embarque.nro_embarque = permisos_embarque.id_permiso_embarque.

        FOR EACH subd_vtas OF r_subd_ventas_embarque.
            /* v_importe_pe = subd_vtas.importe_origen. */
            FIND clientes OF subd_vtas NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                ASSIGN CONTROL_permisos_embarque.id_cliente = subd_vtas.id_cliente
                       CONTROL_permisos_embarque.cliente = clientes.nombre.
            END. 
            IF v_facturas = "" THEN
                v_facturas = string(subd_vtas.id_punto_venta,"9999") + "-" + 
                             STRING(subd_vtas.nro_comp,"99999999").
            ELSE
                v_facturas = v_facturas + " " + string(subd_vtas.id_punto_venta,"9999") + 
                                          "-" + STRING(subd_vtas.nro_comp,"99999999").
        END.

        
    END.
    ASSIGN control_permisos_embarque.facturas   = v_facturas.
    
    FOR EACH r_cobranzas_pe WHERE r_cobranzas_pe.id_aduana           = permisos_embarque.id_aduana
                              AND r_cobranzas_pe.anio                = permisos_embarque.anio
                              AND r_cobranzas_pe.id_permiso_embarque = permisos_embarque.id_permiso_embarque
                              NO-LOCK.
        v_cobrado = v_cobrado + r_cobranzas_pe.importe.
        IF r_cobranzas_pe.cobro_ingresado THEN v_ingresado = v_ingresado + r_cobranzas_pe.importe.
        
        v_semana_cob = semana(r_cobranzas_pe.fecha).
        v_semana_ing = semana(r_cobranzas_pe.fecha + 5).
        CREATE items_control_permisos_embarque.
        ASSIGN items_control_permisos_embarque.id_aduana             = permisos_embarque.id_aduana
               items_control_permisos_embarque.anio                  = permisos_embarque.anio
               items_control_permisos_embarque.id_permiso_embarque   = permisos_embarque.id_permiso_embarque
               items_control_permisos_embarque.fecha_cobrado         = r_cobranzas_pe.fecha
               items_control_permisos_embarque.fecha_ingresar        = r_cobranzas_pe.fecha + 5
               items_control_permisos_embarque.cobrado               = r_cobranzas_pe.importe
               items_control_permisos_embarque.semana_cobrado        = v_semana_cob
               items_control_permisos_embarque.semana_ingresar       = v_semana_ing.
    END.
    ASSIGN control_permisos_embarque.importe_cobrado    = v_cobrado
           control_permisos_embarque.importe_ingresado  = v_ingresado
           control_permisos_embarque.saldo              = permisos_embarque.importe - v_cobrado
           control_permisos_embarque.saldo_ingresar     = v_cobrado - v_ingresado.
    
    v_cobrado = 0.
    v_ingresado = 0.
   /* v_importe_pe = 0. */
    v_facturas = "".