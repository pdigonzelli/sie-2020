DEFINE VAR vapor AS CHAR.
DEFINE VAR v_despachante AS INTEGER.
DEFINE VAR v_mes AS INTEGER.
DEFINE VAR v_anio AS INTEGER.
DEFINE VAR v_factura AS CHARACTER.
DEFINE VAR v_bandera AS INTEGER INITIAL 0.

FOR EACH pe_pendientes.
    DELETE pe_pendientes.
END.

FOR EACH permisos_embarque  BY c_fecha DESC.
    
    FOR EACH r_subd_ventas_embarque WHERE r_subd_ventas_embarque.anio_permiso =
                                          permisos_embarque.anio
                                      AND r_subd_ventas_embarque.id_aduana =
                                          permisos_embarque.id_aduana
                                      AND r_subd_ventas_embarque.nro_embarque =
                                          permisos_embarque.id_permiso_embarque
                                      NO-LOCK
                                          .
        FOR EACH subd_vtas OF r_subd_ventas_embarque NO-LOCK.
            IF subd_vtas.id_punto_venta = 9999 THEN v_bandera = 1. 
            ELSE DO: 
                v_bandera = 0.
                IF subd_vtas.nro_proforma = 0 THEN DO:
                    v_factura = v_factura + string(subd_vtas.id_punto_venta,"9999") + 
                                            STRING(subd_vtas.nro_comp,"99999999") + " ".
                END.
                ELSE DO:
                    v_factura = v_factura + string(subd_vtas.id_punto_venta,"9999") + 
                                            STRING(subd_vtas.nro_proforma,"99999999") + " ".
                END.
            END.
            
            
        END.
    END.
    IF v_bandera = 0 THEN DO:
        FIND FIRST items_packing_list WHERE items_packing_list.id_aduana = 
                                            permisos_embarque.id_aduana
                                        AND items_packing_list.anio_permiso = 
                                            permisos_embarque.anio
                                        AND items_packing_list.nro_permiso_embarque = 
                                            permisos_embarque.id_permiso_embarque
                                        NO-LOCK NO-ERROR.
        
        IF AVAILABLE items_packing_list THEN DO:
            FIND FIRST packing_list OF items_packing_list NO-LOCK NO-ERROR.
            IF AVAILABLE packing_list THEN DO:
                FIND FIRST despachantes OF permisos_embarque NO-LOCK NO-ERROR.
                FIND FIRST vapores OF packing_list NO-LOCK NO-ERROR.
                IF AVAILABLE vapores THEN vapor = vapores.descripcion.
                                     ELSE vapor = "N/A".
                CREATE pe_pendientes.
                ASSIGN pe_pendientes.id_aduana            = permisos_embarque.id_aduana
                       pe_pendientes.anio                 = permisos_embarque.anio
                       pe_pendientes.id_permiso_embarque  = permisos_embarque.id_permiso_embarque
                       pe_pendientes.id_orden_entrega     = permisos_embarque.id_orden_entrega
                       pe_pendientes.ITEM_oe              = permisos_embarque.ITEM_oe
                       pe_pendientes.id_vapor             = packing_list.id_vapor
                       pe_pendientes.vapor                = vapor
                       pe_pendientes.fecha_salida         = packing_list.fecha_salida_vapor
                       pe_pendientes.factura              = v_factura
                       pe_pendientes.id_articulo          = items_packing_list.id_articulo.

                FIND FIRST productos_terminados OF items_packing_list NO-LOCK NO-ERROR.
                IF AVAILABLE productos_terminados THEN DO:
                    ASSIGN pe_pendientes.articulo         = productos_terminados.descripcion.
                END.
                
                IF AVAILABLE despachantes THEN DO:
                    ASSIGN  pe_pendientes.id_despachante       = permisos_embarque.id_despachante
                            pe_pendientes.despachante          = despachantes.descripcion.
                END.       
            END.
        END.
    END.
    v_factura = "".
    v_bandera = 0.
END.
