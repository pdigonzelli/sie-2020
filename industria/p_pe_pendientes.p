DEFINE VAR vapor AS CHAR.
DEFINE VAR v_despachante AS INTEGER.
DEFINE VAR v_mes AS INTEGER.
DEFINE VAR v_anio AS INTEGER.

FOR EACH pe_pendientes.
    DELETE pe_pendientes.
END.

RUN w_consulta_despachante.w (OUTPUT v_despachante,
                              OUTPUT v_mes,
                              OUTPUT v_anio).

FOR EACH permisos_embarque WHERE fecha_oficializacion = ?
                             AND (IF v_despachante > 0 THEN permisos_embarque.id_despachante = v_despachante
                                                       ELSE TRUE)
                             AND (IF v_mes > 0 AND v_anio > 0 THEN 
                                    MONTH(permisos_embarque.fecha) = v_mes AND
                                    YEAR(permisos_embarque.fecha) = v_anio
                                                       ELSE TRUE)
                              BY c_fecha DESC.
    
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
            FIND despachantes OF permisos_embarque NO-LOCK NO-ERROR.
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
                   pe_pendientes.fecha_salida         = packing_list.fecha_salida_vapor.
            
            IF AVAILABLE despachantes THEN DO:
                ASSIGN  pe_pendientes.id_despachante       = permisos_embarque.id_despachante
                        pe_pendientes.despachante          = despachantes.descripcion.
            END.       
        END.
    END.
END.
