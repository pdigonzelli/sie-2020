define var r as rowid no-undo.
define var gall as decimal no-undo.
define var v_gall_tambor as decimal no-undo.
define var v_kilos as decimal no-undo.
define var v_kilos_brutos as decimal no-undo.
define var v_peso as decimal no-undo.
define var v_precio_total as decimal no-undo.
define var v_cantidad_tambores as integer no-undo.
define var v_gall_brix like inspecciones_lote.bx_correg no-undo.
define var v_id_orden_entrega like orden_entrega.id_orden_entrega no-undo.
DEFINE VAR h_con AS HANDLE.

RUN get-container (OUTPUT h_con).

enable  general.orden_entrega.anio
        general.orden_entrega.item
        general.orden_entrega.id_tipo_contrato
        general.orden_entrega.semana_entrega with frame F-Main.
            
run wc_items_contratos.w(output r).
find items_contratos where rowid(items_contratos) = r no-lock no-error.
if available items_contratos then
    do:
        find contratos of items_contratos no-lock no-error.
        if available contratos then 
          do:
            general.orden_entrega.id_contrato:screen-value in frame F-Main      = string(contratos.id_contrato).
            general.orden_entrega.anio:screen-value in frame F-Main             = string(contratos.anio).
            v_anio                                                = contratos.anio.
            general.orden_entrega.item:screen-value in frame F-Main             = string(items_contratos.item).
            v_item                                                = items_contratos.item.
            general.orden_entrega.id_tipo_contrato:screen-value in frame F-Main = string(contratos.id_tipo_contrato).
            v_id_tipo_contrato                                    = contratos.id_tipo_contrato.
            general.orden_entrega.semana_entrega:screen-value in frame F-Main   = string(items_contratos.semana_entrega).
            v_semana_entrega                                      = items_contratos.semana_entrega.
            general.orden_entrega.id_destino:screen-value in frame F-Main       = string(general.items_contratos.destino_final).
            general.orden_entrega.id_condicion_venta:screen-value in frame F-Main    = string(items_contratos.id_clausula).
            v_id_clausula = items_contratos.id_clausula.
            general.orden_entrega.id_cliente:screen-value in frame F-Main       = string(contratos.id_cliente).
            general.orden_entrega.id_calidad:screen-value in frame F-Main       = string(items_contratos.id_calidad).
            general.orden_entrega.id_articulo:screen-value in frame F-Main      = string(items_contratos.id_articulo).
            general.orden_entrega.cantidad:screen-value in frame F-Main         = string(items_contratos.cantidad).
            general.orden_entrega.plazo:screen-value in frame F-Main         = string(contratos.plazo).
            general.orden_entrega.id_tipo_plazo:screen-value in frame F-Main         = string(contratos.id_tipo_plazo).
            general.orden_entrega.id_instrumento_pago:screen-value in frame F-Main         = string(contratos.id_instrumento_pago).
      /*      general.orden_entrega.id_tipo_venta:SCREEN-VALUE IN FRAME F-Main          = STRING(items_contratos.id_tipo_venta).
           */
             
            RUN carga-tipo-venta IN h_con (INPUT items_contratos.id_tipo_venta).
            
            v_id_orden_entrega = integer(orden_entrega.id_orden_entrega:screen-value in frame F-Main).
            run p_calcular-brix.p (input v_id_orden_entrega, 
                             output v_kilos,
                             output v_kilos_brutos,
                             output v_cantidad_tambores,
                             output gall,
                             output v_gall_brix).
                                         
            general.orden_entrega.grados_brix:screen-value in frame F-Main = string(v_gall_brix).

            general.orden_entrega.kgs_netos_tambores:screen-value in frame F-Main    = string(v_kilos).
            general.orden_entrega.kgs_brutos_tambores:screen-value in frame F-Main    = string(v_kilos_brutos).
            general.orden_entrega.total_galones:screen-value in frame F-Main         = string(gall).
            
            case items_contratos.id_tipo_unidad_venta:
                when 1 then /* TONELADAS */
                    do:
                        v_precio_total = ((v_kilos / 1000) * items_contrato.precio_origen).
                    end.
                when 2 then /* KILOS */
                    do:
                        v_precio_total = (v_kilos * items_contrato.precio_origen).                    
                    end.
                when 3 then /* GALONES */
                    do:
                        v_precio_total = (gall * items_contrato.precio_origen).
                    end.
                when 4 then /* LIBRAS */
                    do:
                        v_precio_total = ((v_kilos * 2.20462) * items_contrato.precio_origen).
                    end.
           end case. 
                   
           general.orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total).
            
          end.
        
    end.

disable general.orden_entrega.anio
        general.orden_entrega.item
        general.orden_entrega.id_tipo_contrato
        general.orden_entrega.semana_entrega with frame F-Main.
   
