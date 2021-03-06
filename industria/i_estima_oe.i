  
  DEFINE VAR r                   AS ROWID NO-UNDO.
  DEFINE VAR gall                AS DECIMAL NO-UNDO.
  DEFINE VAR v_kilos             AS DECIMAL NO-UNDO.
  DEFINE VAR v_kilos_brutos      AS DECIMAL NO-UNDO.
  DEFINE VAR v_precio_total      AS DECIMAL NO-UNDO.
  DEFINE VAR v_cantidad_tambores AS INTEGER NO-UNDO.
  DEFINE VAR v_gall_brix         AS DECIMAL.
  DEFINE VAR v_id_orden_entrega  AS INTEGER.
  DEFINE VAR v_item_oe           AS INTEGER.
  DEFINE VAR h_con               AS HANDLE.
  DEFINE VAR r_item_oe           AS ROWID.
  DEFINE VAR v_comision          AS DECIMAL.
  DEFINE VAR v_porc_comision     AS DECIMAL.


  RUN get-rowid1 IN h_b_items_orden_entrega (OUTPUT r_item_oe).

  /*posiciona en item_orden_entrega rowid que toma del browser h_b_items_orden_entrega*/
  FIND items_orden_entrega WHERE ROWID(items_orden_entrega) = r_item_oe NO-ERROR.
  /*busca la entrega conrrespondiente en items_contratos*/
  IF AVAILABLE items_orden_entrega THEN DO:
    FIND items_contratos WHERE items_contratos.id_contrato    = items_orden_entrega.id_contrato
                         AND items_contratos.id_tipo_contrato = items_orden_entrega.id_tipo_contrato
                         AND items_contratos.anio             = items_orden_entrega.anio
                         AND items_contratos.item             = items_orden_entrega.ITEM 
                         NO-LOCK NO-ERROR.
    IF AVAILABLE items_contratos THEN
      DO:
        FIND contratos OF items_contratos NO-LOCK NO-ERROR.
          IF AVAILABLE contratos THEN 
            DO:
              /* MODO AUTOMATICO, ES DECIR TOMA INFORMACION DE TAMBORES RELACIONADOS */
                v_id_orden_entrega = items_orden_entrega.id_orden_entrega.
                v_item_oe          = items_orden_entrega.ITEM_oe.
                /*<aqui es donde hay que reemplazar con valores calculados para la estimacion
                RUN p_calcular-brix_item_oe.p (INPUT v_id_orden_entrega,
                                               INPUT v_item_oe,
                                               OUTPUT v_kilos,
                                               OUTPUT v_kilos_brutos,
                                               OUTPUT v_cantidad_tambores,
                                               OUTPUT gall,
                                               OUTPUT v_gall_brix).
                */
                FIND FIRST r_productos_calidad_envase OF items_contratos NO-LOCK NO-ERROR.
                IF AVAILABLE r_productos_calidad_envase THEN DO:
                    v_cantidad_tambores = general.items_contratos.cantidad.
                    v_kilos = v_cantidad_tambores * r_productos_calidad_envase.kilos.
                    gall = v_cantidad_tambores * 53.6.
                    MESSAGE "Kilos " v_kilos " Tambores " v_cantidad_tambores " Galones " gall VIEW-AS ALERT-BOX. 
                END.
                IF v_cantidad_tambores <> 0 THEN   
                  /*
                  general.items_orden_entrega.cantidad_tambores:SCREEN-VALUE IN FRAME F-Main   = STRING(v_cantidad_tambores).
                  general.items_orden_entrega.kgs_netos_tambores:SCREEN-VALUE IN FRAME F-Main  = STRING(v_kilos).
                  general.items_orden_entrega.kgs_brutos_tambores:SCREEN-VALUE IN FRAME F-Main = STRING(v_kilos_brutos).
                  */
                  CASE items_contratos.id_tipo_unidad_venta_origen:
                    WHEN 1 THEN /* TONELADAS */
                      DO:
                        v_precio_total = ((v_kilos / 1000) * items_contrato.precio_origen).
                      END.
                    WHEN 2 THEN /* KILOS */
                      DO:
                        v_precio_total = (v_kilos * items_contrato.precio_origen).
                      END.
                    WHEN 3 then /* GALONES */                    
                      DO:
                        IF items_contrato.id_articulo = 52 OR items_contrato.id_articulo = 53 THEN
                          DO:       
                            
                          END.
                        v_precio_total = (gall * items_contrato.precio_origen).
                      END.
                    WHEN 4 THEN /* LIBRAS */
                      DO:
                        v_precio_total = ((v_kilos * 2.20462) * items_contrato.precio_origen).
                      END.
                  END CASE. 
                  /*
                  general.items_orden_entrega.total_factura:SCREEN-VALUE IN FRAME F-Main  = STRING(v_precio_total).                        
                  */
                  MESSAGE "Precio Total " v_precio_total VIEW-AS ALERT-BOX.
                  /******************** COMISIONES PERO CREO QUE NO ESTA SIENDO EL VALOR DEFINITIVO *********/
                  IF items_contratos.id_articulo = 52 OR items_contratos.id_articulo = 53 OR items_contratos.id_articulo = 71  THEN DO:
                    FIND FIRST orden_entrega OF items_orden_entrega NO-LOCK NO-ERROR.
                      IF AVAILABLE orden_entrega THEN DO:
                        FIND destinos WHERE destinos.id_destino = orden_entrega.id_destino NO-LOCK NO-ERROR.
                          IF AVAILABLE destinos THEN DO:
                            IF destinos.id_destino_grupo = 25 THEN DO:
                              /* ES UN DESTINO DENTRO DE USA */
                                FIND FIRST r_gastos_items_contrato OF items_contratos
                                                                   WHERE r_gastos_items_contrato.id_gasto = 10 /* GASTO DE COMISION */
                                                                   NO-LOCK NO-ERROR.
                                IF AVAILABLE r_gastos_items_contrato THEN DO:
                                  v_comision = r_gastos_items_contrato.importe * gall.
                                  
                                  MESSAGE "Comision "   v_comision VIEW-AS ALERT-BOX.
                                  
                                END.
                            END.
                          END.
                      END.
                  END.
                  ELSE DO:
                    FIND FIRST r_gastos_items_contrato OF items_contratos
                                                       WHERE r_gastos_items_contrato.id_gasto = 10 /* GASTO DE COMISION */
                                                       NO-LOCK NO-ERROR.
                    IF AVAILABLE r_gastos_items_contrato THEN DO:
                      v_comision = r_gastos_items_contrato.importe * v_kilos.
                      MESSAGE "Comision "   v_comision VIEW-AS ALERT-BOX.
                    END.
                  END.

                /***************** END COMISIONES  **********************************************************/
              END.
            ELSE MESSAGE "No se encontro el contratos " VIEW-AS ALERT-BOX.
         END.
       ELSE MESSAGE "No se encontro el item de contrato " VIEW-AS ALERT-BOX.
       /* SE CALCULA SIEMPRE LOS GASTOS SIN IMPORTAR EL MODO DE ACTUALIZACION*/
       /*
       RUN calculo-gastos (INPUT items_orden_entrega.id_orden_entrega, 
                          INPUT INTEGER(items_orden_entrega.ITEM_oe:SCREEN-VALUE IN FRAME F-Main),
                          INPUT integer(items_orden_entrega.id_condicion_venta:SCREEN-VALUE IN FRAME F-Main)).
      */                    
      MESSAGE "Se actualizaron los datos satisfactoriamente!" VIEW-AS ALERT-BOX.
  END.
