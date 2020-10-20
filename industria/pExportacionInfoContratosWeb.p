DEFINE VAR vCliente AS INTEGER.
DEFINE VAR vContenedor AS CHAR FORMAT "x(100)".
DEFINE VAR vDocumentos AS INTEGER.
DEFINE VAR vIdDoc AS INTEGER.
DEFINE VAR vNroDoc AS INTEGER.
DEFINE VAR vLoteDisplay AS CHAR.
DEFINE VAR vFechaNorm AS CHAR.
DEFINE VAR vFechaEmba AS CHAR.
DEFINE VAR vFechaArri AS CHAR.
DEFINE VAR vMeses AS CHAR.
DEFINE VAR vLotes AS CHAR.
DEFINE VAR vMuestras AS CHAR.
DEFINE VAR vFechaEnvio AS CHAR.

DEFINE BUFFER bbItemsOe FOR items_orden_entrega.

vMeses = "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

RUN contratos.
RUN itemsContratos.
RUN docsItemsContratos.
RUN muestras.
RUN clientes.




PROCEDURE contratos:
    OUTPUT TO "\\samiweb\public\contratos.txt".
    FOR EACH contratos WHERE contratos.id_tipo_contrato = 1
                       NO-LOCK.
        FIND FIRST items_contratos OF contratos NO-LOCK NO-ERROR.
        FIND FIRST productos_terminados OF items_contratos NO-LOCK NO-ERROR.
        FIND FIRST clientes WHERE clientes.id_cliente = contratos.id_cliente NO-LOCK NO-ERROR.
        
        IF contratos.id_po_cliente[1] <> "" AND 
           contratos.id_po_cliente[1] <> "N/A" AND
           contratos.id_po_cliente[1] <> "NA" AND
           contratos.id_po_cliente[1] <> "-" AND
           contratos.id_po_cliente[1] <> "--" THEN vLoteDisplay = contratos.id_po_cliente[1].
                                               ELSE vLoteDisplay = "San Miguels contract number " + contratos.id_contrato.
    
        vFechaNorm = STRING(DAY(contratos.fecha),"99") + "-" + 
                     ENTRY(MONTH(contratos.fecha),vMeses) + "-" + 
                     STRING(SUBSTRING(STRING(YEAR(contratos.fecha)),3,2),"99").
    
        EXPORT DELIMITER ";" contratos.id_tipo_contrato 
                             contratos.id_contrato
                             contratos.anio
                             contratos.id_po_cliente[1]
                             vFechaNorm
                             vLoteDisplay
                             IF AVAILABLE items_contratos THEN STRING(items_contratos.id_articulo) ELSE "NONE"
                             IF AVAILABLE productos_terminados THEN productos_terminados.descripcion_ingles ELSE "NONE"
                             contratos.id_cliente
                             IF AVAILABLE clientes THEN clientes.nombre ELSE "NONE".
                             
    END.
    OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE itemsContratos:

    OUTPUT TO "\\samiweb\public\items_contratos.txt".
    FOR EACH items_contratos WHERE items_contratos.id_tipo_contrato = 1
                               /*AND items_contratos.id_contrato      = "SM0065"*/
                               /*AND items_contratos.id_cliente = vCliente*/
                                NO-LOCK.
        FIND FIRST contratos OF items_contratos NO-LOCK NO-ERROR.
        FIND FIRST productos_terminados OF items_contratos NO-LOCK NO-ERROR.
        FIND FIRST calidades OF items_contratos NO-LOCK NO-ERROR.
        FIND FIRST envases_prod OF items_contratos NO-LOCK NO-ERROR.
        FIND FIRST destinos WHERE destinos.id_destino = items_contratos.destino_final
                            NO-LOCK NO-ERROR.
        FIND FIRST items_orden_entrega OF items_contratos NO-LOCK NO-ERROR.
        FIND FIRST estados_oe OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST orden_entrega OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST vapores OF orden_entrega NO-LOCK NO-ERROR.
        vDocumentos = 0.
        vContenedor = "".
        FOR EACH bbItemsOe OF items_contratos NO-LOCK.
            FOR EACH packing_list WHERE packing_list.id_orden_entrega   = bbItemsOe.id_orden_entrega
                                    AND packing_list.item_oe            = bbItemsOe.ITEM_oe
                                    NO-LOCK.
                vDocumentos = vDocumentos + 1.
                FOR EACH items_packing_list OF packing_list NO-LOCK.
                    
                    vContenedor = vContenedor + items_packing_list.nro_contenedor + "/".
                    FOR EACH r_items_venta_pack_list WHERE r_items_venta_pack_list.id_sucursal     = items_packing_list.id_sucursal
                                                       AND r_items_venta_pack_list.id_packing_list = items_packing_list.id_packing_list
                                                       AND r_items_venta_pack_list.ITEM_pack       = items_packing_list.ITEM NO-LOCK.
                        FOR EACH items_venta WHERE items_venta.id_punto_venta = r_items_venta_pack_list.id_punto_venta
                                               AND items_venta.nromov         = r_items_venta_pack_list.nromov
                                               AND items_venta.ITEM           = r_items_venta_pack_list.ITEM NO-LOCK.
                            vDocumentos = vDocumentos + 1.
                        END.
                    END.
                END.
            END.
            FOR EACH tambores_industria WHERE tambores_industria.id_orden_entrega   = bbItemsOe.id_orden_entrega
                                          AND tambores_industria.ITEM_oe            = bbItemsOe.ITEM_oe
                                        BREAK BY tambores_industria.nromov.
               IF LAST-OF(tambores_industria.nromov) THEN DO:
                   vLotes = vLotes + " " + STRING(tambores_industria.id_lote) + "/" + SUBSTRING(STRING(tambores_industria.anio),3,2).
               END.
            END.
        END.
        
        IF AVAILABLE orden_entrega THEN DO:
            
            vFechaEmba = STRING(DAY(orden_entrega.fecha_embarque),"99") + "-" + 
                         ENTRY(MONTH(orden_entrega.fecha_embarque),vMeses) + "-" + 
                         STRING(SUBSTRING(STRING(YEAR(orden_entrega.fecha_embarque)),3,2),"99").
        
            vFechaArri = STRING(DAY(orden_entrega.fecha_arribo),"99") + "-" + 
                         ENTRY(MONTH(orden_entrega.fecha_arribo),vMeses) + "-" + 
                         STRING(SUBSTRING(STRING(YEAR(orden_entrega.fecha_arribo)),3,2),"99").
        
        END.
        ELSE DO:
            vFechaArri = "NONE".
            vFechaEmba = "NONE".
        END.
        
        EXPORT DELIMITER ";" items_contratos.id_tipo_contrato 
                             items_contratos.id_contrato
                             items_contratos.anio
                             items_contratos.ITEM
                             items_contratos.id_articulo
                             IF AVAILABLE productos_terminados THEN productos_terminados.descripcion_ingles ELSE "NONE"
                             items_contratos.id_calidad
                             IF AVAILABLE calidades THEN calidades.descripcion_ingles ELSE "NONE"
                             items_contratos.id_articulo_cliente[1]
                             "Articulo Cliente "
                             items_contratos.numero_release[1]
                             items_contratos.cantidad
                             items_contratos.id_envase
                             IF AVAILABLE envases_prod THEN envases_prod.descripcion_ingles ELSE "NONE"
                             items_contratos.destino_final
                             IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE"
                             vFechaEmba 
                             IF AVAILABLE orden_entrega THEN STRING(orden_entrega.id_vapor) ELSE "NONE"
                             IF AVAILABLE vapores THEN vapores.descripcion ELSE "NONE"
                             vContenedor
                             vLotes
                             vFechaArri
                             IF AVAILABLE items_orden_entrega THEN STRING(items_orden_entrega.id_estado) ELSE "NONE"
                             IF AVAILABLE estados_oe THEN estados_oe.descripcion ELSE "NONE"
                             "Color segun el estado"
                             vDocumentos
                             "Guia DHL" 
                             . 
        vLotes = "".
    END.
    
    OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE docsItemsContratos:

    OUTPUT TO "\\samiweb\public\docs_items_contratos.txt".
    vNroDoc = 1.
    FOR EACH items_contratos WHERE items_contratos.id_tipo_contrato = 1
                       NO-LOCK.
        vIdDoc = 1.
        FOR EACH tambores_industria WHERE tambores_industria.id_contrato_of     = items_contratos.id_contrato
                                      AND tambores_industria.id_tipocontrato_of = items_contratos.id_tipo_contrato
                                      AND tambores_industria.anio_of            = items_contratos.anio
                                      AND tambores_industria.item_of            = items_contratos.ITEM
                                    NO-LOCK
                                   BREAK BY tambores_industria.nromov.
            IF LAST-OF(tambores_industria.nromov) THEN DO:
                FOR EACH protocolos WHERE protocolos.id_empresa     = tambores_industria.id_empresa
                                      AND protocolos.id_sucursal    = tambores_industria.id_sucursal
                                      AND protocolos.id_tipotambor  = tambores_industria.id_tipotambor
                                      AND protocolos.nromov         = tambores_industria.nromov
                                    NO-LOCK.
                    FOR EACH r_muestras_protocolos WHERE r_muestras_protocolos.id_protocolo = protocolos.id_protocolo
                                                     AND r_muestras_protocolos.anio         = protocolos.anio
                                                     AND r_muestras_protocolos.id_articulo  = protocolos.id_articulo
                                                    NO-LOCK.
                        FOR EACH items_muestras WHERE items_muestras.id_muestra     = r_muestras_protocolos.id_muestra
                                                  AND items_muestras.anio           = r_muestras_protocolos.anio_muestra
                                                  AND items_muestras.ITEM           = r_muestras_protocolos.item_muestra
                                                NO-LOCK.
                            FIND FIRST muestras OF items_muestras NO-LOCK NO-ERROR.
                            IF AVAILABLE muestras THEN DO:
                                vFechaNorm = STRING(DAY(muestras.fecha),"99") + "-" + 
                                             ENTRY(MONTH(muestras.fecha),vMeses) + "-" + 
                                             STRING(SUBSTRING(STRING(YEAR(muestras.fecha)),3,2),"99").
                            END.
                            ELSE vFechaNorm = "NONE".
                            vMuestras = vMuestras + " " + "Sample ordered " + vFechaNorm.
                        END.
                    END.
                    EXPORT DELIMITER ";" vNroDoc
                                         "1"
                                         vIdDoc
                                         items_contratos.id_tipo_contrato
                                         items_contratos.id_contrato
                                         items_contratos.anio
                                         items_contratos.ITEM
                                         IF AVAILABLE items_muestras THEN STRING(items_muestras.id_muestra) ELSE "NONE"
                                         vMuestras
                                         protocolos.anio
                                         "nombre de archivo pdf"
                                         tambores_industria.id_lote
                                         tambores_industria.anio
                                         .
                    vIDDoc = vIdDoc + 1.
                    vNroDoc = vNroDoc + 1.
                    vMuestras = "".
                END.
            END.
        END.
        FOR EACH packing_list WHERE packing_list.id_tipo_contrato   = items_contratos.id_tipo_contrato
                                AND packing_list.id_contrato        = items_contratos.id_contrato
                                AND packing_list.anio               = items_contratos.anio
                                AND packing_list.ITEM_contrato      = items_contratos.ITEM
                                NO-LOCK.
            FIND FIRST items_packing_list OF packing_list NO-LOCK NO-ERROR.
    
            EXPORT DELIMITER ";" vNroDoc
                                 "2"
                                 vIdDoc
                                 items_contratos.id_tipo_contrato
                                 items_contratos.id_contrato
                                 items_contratos.anio
                                 items_contratos.ITEM
                                 packing_list.id_packing_list
                                 packing_list.id_packing_list
                                 packing_list.anio
                                 "nombre de archivo pdf"
                                 IF AVAILABLE items_packing_list THEN SUBSTRING(items_packing_list.nro_lote,1,4) ELSE "NONE"
                                 IF AVAILABLE items_packing_list THEN SUBSTRING(items_packing_list.nro_lote,6,2) ELSE "NONE"
                                 .
            vIDDoc = vIdDoc + 1.
            vNroDoc = vNroDoc + 1.
    
        END.
    END.
    
    OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE muestras:

    OUTPUT TO "\\samiweb\public\muestras.txt".
    
    FOR EACH items_muestras NO-LOCK.
        
        FIND FIRST muestras OF items_muestras NO-LOCK NO-ERROR.
        IF AVAILABLE muestras THEN DO:
                vFechaNorm = STRING(DAY(muestras.fecha),"99") + "-" + 
                             ENTRY(MONTH(muestras.fecha),vMeses) + "-" + 
                             STRING(SUBSTRING(STRING(YEAR(muestras.fecha)),3,2),"99").
        
        /*
        ELSE vFechaNorm = "NONE".*/
    
            IF items_muestras.fecha_enviado_bue <> ? THEN
                vFechaEnvio = STRING(DAY(items_muestras.fecha_enviado_bue),"99") + "-" + 
                              ENTRY(MONTH(items_muestras.fecha_enviado_bue),vMeses) + "-" + 
                              STRING(SUBSTRING(STRING(YEAR(items_muestras.fecha_enviado_bue)),3,2),"99").
            ELSE
                vFechaEnvio = STRING(DAY(items_muestras.fecha_enviado_tuc),"99") + "-" + 
                              ENTRY(MONTH(items_muestras.fecha_enviado_tuc),vMeses) + "-" + 
                              STRING(SUBSTRING(STRING(YEAR(items_muestras.fecha_enviado_tuc)),3,2),"99").
            
            IF items_muestras.id_courier_bue <> 0 THEN
                FIND FIRST courier WHERE courier.id_courier = items_muestras.id_courier_bue NO-LOCK NO-ERROR.
            ELSE 
                FIND FIRST courier WHERE courier.id_courier = items_muestras.id_courier_tuc NO-LOCK NO-ERROR.
    
            vMuestras = vMuestras + " " + "Sample ordered " + vFechaNorm.
    
           
            FIND FIRST clientes OF muestras NO-LOCK NO-ERROR.
            FIND FIRST productos_terminados OF muestras NO-LOCK NO-ERROR.
            FIND FIRST contactos_industria WHERE contactos_industria.id_contacto = muestras.id_destinatario
                                            NO-LOCK NO-ERROR.
           
            
            
            EXPORT DELIMITER ";" items_muestras.id_muestra
                                 vMuestras
                                 vFechaNorm
                                 IF AVAILABLE muestras THEN STRING(muestras.id_articulo) ELSE "NONE"
                                 IF AVAILABLE productos_terminados THEN productos_terminados.descripcion  ELSE "NONE"
                                 IF AVAILABLE muestras THEN string(muestras.id_destinatario) ELSE "NONE"
                                 IF AVAILABLE contactos_industria THEN contactos_industria.nombre  ELSE "NONE"
                                 IF AVAILABLE muestras THEN string(muestras.id_cliente) ELSE "NONE"
                                 IF AVAILABLE clientes THEN clientes.razon_social  ELSE "NONE"
                                 vFechaEnvio
                                 IF AVAILABLE courier THEN courier.id_courier ELSE 0
                                 IF AVAILABLE courier THEN courier.descripcion ELSE "NONE"
                                 items_muestras.nro_guia_bue
                                 .
            
            vMuestras = "".
        END.
    END.
    OUTPUT CLOSE.
END.

PROCEDURE clientes:
   
 OUTPUT TO \\samiweb\public\mos_clients.txt.
     FOR EACH clientes WHERE clientes.id_tipo_cliente = 1 OR
                             clientes.id_tipo_cliente = 2 OR
                             clientes.id_tipo_cliente = 3 NO-LOCK: 
         EXPORT DELIMITER ";" id_cliente razon_social RECID(clientes).
     END.
 OUTPUT CLOSE.
END.



