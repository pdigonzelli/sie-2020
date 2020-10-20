DEFINE INPUT PARAMETER p_anio AS INTEGER.
DEFINE INPUT PARAMETER p_semana as integer.


DEFINE var v_lotes as CHAR.
DEFINE var v_cantidad as integer.
DEFINE var v_anio as integer.
DEFINE var v_semana as integer.

FOR EACH oe_lote_semana.
    DELETE oe_lote_semana.    
END.


FOR EACH orden_entrega WHERE orden_entrega.semana_embarque = p_semana 
                         AND YEAR(orden_entrega.fecha_embarque) = p_anio
                         AND orden_entrega.id_tipo_orden_entrega = 1 NO-LOCK:
    
    RUN semana_anio.p (INPUT orden_entrega.fecha,
                       OUTPUT v_semana, OUTPUT v_anio).

    
    IF v_anio <> p_anio THEN NEXT.
         
    FIND FIRST vapores        of orden_entrega no-lock no-error.
    FIND FIRST destinos       of orden_entrega no-lock no-error.
    FIND FIRST lugar_descarga of orden_entrega no-lock no-error.
    FIND FIRST agencias       WHERE agencias.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.

    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK.
        
        find clientes               of items_orden_entrega no-lock no-error.
        find calidades              OF items_orden_entrega no-lock no-error.
        find productos_terminados   of items_orden_entrega no-lock no-error.
        find contratos              of items_orden_entrega no-lock no-error.
        find items_contratos        OF items_orden_entrega no-lock no-error.
        FIND FIRST clausulas        WHERE clausulas.id_clausula = 
                                          items_orden_entrega.id_condicion_venta
                                    NO-LOCK NO-ERROR.
    
        IF items_orden_entrega.id_articulo <> 54 THEN DO:
          for each tambores_industria OF items_orden_entrega 
                                      break by tambores_industria.id_lote.
              
              v_cantidad = v_cantidad + 1.
              IF LAST-OF(tambores_industria.id_lote) THEN DO:
                      IF v_lotes = "" THEN DO:
                          v_lotes = string(tambores_industria.id_lote) + "/" + 
                                    SUBSTRING(STRING(tambores_industria.anio),3,2) + " (" +
                                    string(v_cantidad) + ")". 
                          v_cantidad = 0.
                      END.
                      ELSE DO:
                          v_lotes = v_lotes + " - " +
                                    string(tambores_industria.id_lote) + "/" + 
                                    SUBSTRING(STRING(tambores_industria.anio),3,2) + " (" +
                                    string(v_cantidad) + ")".
                          v_cantidad = 0.
                      END.
              END.
          end.
        END.
        ELSE DO: /*******CASCARA********/
           FOR EACH r_lote_cascara_oe WHERE r_lote_cascara_oe.id_orden_entrega = items_orden_entrega.id_orden_entrega
                                        AND r_lote_cascara_oe.id_lote          = items_orden_entrega.ITEM_oe
                                      NO-LOCK.
             FIND FIRST lotes_cascara OF r_lote_cascara_oe NO-LOCK NO-ERROR.
             v_cantidad = r_lote_cascara_oe.cantidad.
             v_lotes = v_lotes + " " + STRING(lotes_cascara.id_lote) + "/" + SUBSTRING(STRING(lotes_cascara.anio), 3, 2).
             v_lotes = v_lotes + " (" + string(v_cantidad) + ") ".
             FIND FIRST r_lote_cascara_contrato WHERE r_lote_cascara_contrato.nromov = lotes_cascara.nromov NO-LOCK NO-ERROR.
             FIND FIRST contratos WHERE r_lote_cascara_contrato.id_contrato = contratos.id_contrato NO-LOCK NO-ERROR.
           END.
           
        END.
        create oe_lote_semana.
        assign oe_lote_semana.id_orden_entrega  = orden_entrega.id_orden_entrega
               oe_lote_semana.item_oe           = items_orden_entrega.ITEM_oe
               oe_lote_semana.semana            = orden_entrega.semana_embarque
               oe_lote_semana.fecha             = items_orden_entrega.fecha
               oe_lote_semana.id_vapor          = orden_entrega.id_vapor
               oe_lote_semana.id_destino        = orden_entrega.id_destino
               oe_lote_semana.id_cliente        = items_orden_entrega.id_cliente
               oe_lote_semana.id_contrato       = items_orden_entrega.id_contrato
               oe_lote_semana.id_tipo_contrato  = items_orden_entrega.id_tipo_contrato
               oe_lote_semana.anio              = items_orden_entrega.anio
               oe_lote_semana.item              = items_orden_entrega.item
               oe_lote_semana.id_calidad        = items_orden_entrega.id_calidad
               oe_lote_semana.id_articulo       = items_orden_entrega.id_articulo
               oe_lote_semana.id_lugar_descarga = orden_entrega.id_lugdes
               oe_lote_semana.lotes             = v_lotes
               oe_lote_semana.tambores          = items_orden_entrega.tambores_pedidos
               oe_lote_semana.vapor             = IF AVAILABLE vapores THEN vapores.descripcion ELSE "null"
               oe_lote_semana.destino           = IF AVAILABLE destinos THEN destinos.descripcion ELSE "null"
               oe_lote_semana.cliente           = IF AVAILABLE clientes THEN clientes.nombre ELSE "null"
               oe_lote_semana.calidad           = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "null"
               oe_lote_semana.articulo          = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "null"
               oe_lote_semana.lugar_descarga    = IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "null"
               oe_lote_semana.orden_fabricacion = IF AVAILABLE contratos THEN string(contratos.orden_fabricacion) ELSE "null"
               
               oe_lote_semana.kilos_netos       = items_orden_entrega.kgs_netos_tambores
               oe_lote_semana.kilos_brutos      = items_orden_entrega.kgs_brutos_tambores
               oe_lote_semana.id_clausula       = items_orden_entrega.id_condicion_venta
               oe_lote_semana.id_agencia        = orden_entrega.id_agencia
               oe_lote_semana.clausula          = IF AVAILABLE clausulas THEN clausulas.descripcion ELSE "null"
               oe_lote_semana.agencia           = IF AVAILABLE agencias THEN agencias.descripcion ELSE "null"
                                       
                   .

        IF AVAILABLE items_contratos THEN DO:
                ASSIGN oe_lote_semana.observaciones     = items_contratos.observaciones
                       oe_lote_semana.marca_tambores    = items_contratos.marca_tambores
                       oe_lote_semana.release_nro       = items_contratos.numero_release[1]
                       oe_lote_semana.po_cliente        = items_contratos.id_po_cliente[1]
                       oe_lote_semana.cod_prod_cliente  = items_contratos.id_articulo_cliente[1].
                
        END.
        ELSE DO:
                ASSIGN oe_lote_semana.observaciones  = "null"
                       oe_lote_semana.marca_tambores = "null".
                
        END.
        v_lotes = "".
        v_cantidad = 0.
    END.
end.
