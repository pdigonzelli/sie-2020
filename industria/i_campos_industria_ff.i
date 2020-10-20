DEFINE VAR h_con AS HANDLE.
DEFINE VAR v_r_oe AS ROWID.
define var r as rowid no-undo.

RUN get-container (OUTPUT h_con).
RUN get-rowid-oe IN h_con (OUTPUT v_r_oe).

FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = v_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    CASE orden_entrega.id_tipo_orden_entrega:
        WHEN 1 THEN DO: /* INDUSTRIA */
            general.items_orden_entrega.id_calidad:LABEL IN FRAME F-Main = "Calidad".
            general.items_orden_entrega.cantidad_pallets:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.cajas_x_pallets:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.precio_x_caja:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.total_cajas:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.id_marca:HIDDEN IN FRAME F-Main = TRUE.
            fi-marcas_prod-descripcion:HIDDEN in frame F-Main = TRUE.
            general.items_orden_entrega.id_tipo_pallet:HIDDEN IN FRAME F-Main = TRUE.
            fi-tipo_pallets-descripcion:HIDDEN in frame F-Main = TRUE.
            general.items_orden_entrega.id_programa_despacho:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.id_envase:HIDDEN IN FRAME F-Main = TRUE.
            fi-envases_prod-descripcion:HIDDEN in frame F-Main = TRUE.
            
            general.items_orden_entrega.cantidad_tambores:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.tambores_pedidos:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.grados_brix:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.id_contrato:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.id_tipo_contrato:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.anio:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.ITEM:HIDDEN IN FRAME F-Main = FALSE.
        END.
        WHEN 2 THEN DO: /* FRUTA FRESCA */
            general.items_orden_entrega.id_calidad:LABEL IN FRAME F-Main = "Variedad".
            general.items_orden_entrega.cantidad_tambores:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.tambores_pedidos:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.grados_brix:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.id_contrato:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.id_tipo_contrato:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.anio:HIDDEN IN FRAME F-Main = TRUE.
            general.items_orden_entrega.ITEM:HIDDEN IN FRAME F-Main = TRUE.
            
            general.items_orden_entrega.cantidad_pallets:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.cajas_x_pallets:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.precio_x_caja:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.total_cajas:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.id_marca:HIDDEN IN FRAME F-Main = FALSE.
            fi-marcas_prod-descripcion:HIDDEN in frame F-Main = FALSE.
            general.items_orden_entrega.id_tipo_pallet:HIDDEN IN FRAME F-Main = FALSE.
            fi-tipo_pallets-descripcion:HIDDEN in frame F-Main = FALSE.
            general.items_orden_entrega.id_programa_despacho:HIDDEN IN FRAME F-Main = FALSE.
            general.items_orden_entrega.id_envase:HIDDEN IN FRAME F-Main = FALSE.
            fi-envases_prod-descripcion:HIDDEN in frame F-Main = FALSE.
        END.
    END CASE.
END.
