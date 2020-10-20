/********************************
 Purpose:   Crea tabla para reporte de presupuestos.


*********************************/

DEFINE TABLE rptPresupuestoOE 
    RCODE-INFORMATION
    FIELD id_orden_entrega              AS INTEGER                              COLUMN-LABEL "OE"
    FIELD anio_oe                       AS INTEGER                              COLUMN-LABEL "Año"
    FIELD semana                        AS INTEGER                              COLUMN-LABEL "Semana"
    FIELD id_despachante                AS INTEGER                              COLUMN-LABEL "Cod.Despachante"
    FIELD despachante                   AS CHAR                                 COLUMN-LABEL "Despachante"
    FIELD id_vapor                      LIKE vapores.id_vapor                   COLUMN-LABEL "Cod.Vapor"
    FIELD vapor                         AS CHARACT                              COLUMN-LABEL "Vapor"
    FIELD fecha_salida                  AS DATE                                 COLUMN-LABEL "Fecha Salida"
    FIELD fecha_arribo                  AS DATE                                 COLUMN-LABEL "Fecha Arribo"
    FIELD id_agencia                    AS INTEGER                              COLUMN-LABEL "Cod.Agencia"
    FIELD agencia                       AS CHAR                                 COLUMN-LABEL "Agencia"
    FIELD id_destino                    AS INTEGER                              COLUMN-LABEL "Cod.Destino"
    FIELD destino                       AS CHAR                                 COLUMN-LABEL "Destino"
    FIELD ITEM_oe                       AS INTEGER                              COLUMN-LABEL "Parte OE"
    FIELD id_articulo                   LIKE productos_terminados.id_articulo   COLUMN-LABEL "Cod.Producto"
    FIELD articulo                      LIKE productos_terminados.descripcion   COLUMN-LABEL "Producto"
    FIELD id_envase                     LIKE envases_prod.id_envase             COLUMN-LABEL "Cod.Envase"
    FIELD envase                        LIKE envases_prod.descripcion           COLUMN-LABEL "Envase"
    FIELD id_calidad                    LIKE calidades.id_calidad               COLUMN-LABEL "Cod.Calidad"
    FIELD calidad                       LIKE calidades.descripcion              COLUMN-LABEL "Calidad"
    FIELD id_cliente                    LIKE subd_vtas.id_cliente               COLUMN-LABEL "Cod.Cliente"
    FIELD razon_social                  LIKE clientes.razon_social              COLUMN-LABEL "Cliente"
    FIELD id_contrato                   AS CHARACT                              COLUMN-LABEL "Contrato"
    FIELD id_tipo_contrato              AS integer                              COLUMN-LABEL "T.Contrato"
    FIELD anio                          AS integer                              COLUMN-LABEL "Año"
    FIELD item                          AS integer                              COLUMN-LABEL "Parte"
    FIELD cantidad_tambores             AS integer                              COLUMN-LABEL "Tambores"
    FIELD kgs_netos_tambores            AS decimal                              COLUMN-LABEL "Kgs Netos"                DECIMALS 4
    FIELD kgs_brutos_tambores           AS decimal                              COLUMN-LABEL "Kgs Brutos"               DECIMALS 4
    FIELD id_condicion_venta            AS integer                              COLUMN-LABEL "C.Cond.Vta."
    FIELD condicion_venta               AS CHAR                                 COLUMN-LABEL "Cond. Venta"
    FIELD precio_x_galon                AS decimal                              COLUMN-LABEL "Precio x Galon u$s"       DECIMALS 4
    FIELD total_galones                 AS decimal                              COLUMN-LABEL "T. Galones"               DECIMALS 4
    FIELD total_factura                 AS decimal                              COLUMN-LABEL "Total factura"            DECIMALS 4
    FIELD contenedores                  AS decimal                              COLUMN-LABEL "Contenedores"             DECIMALS 2
    FIELD fob_ton                       AS decimal                              COLUMN-LABEL "Fob Total"                DECIMALS 4
    FIELD grados_brix                   AS decimal                              COLUMN-LABEL "Grados Brix"              DECIMALS 4
    FIELD id_tipo_contenedor            AS integer                              COLUMN-LABEL "Cod.Tipo Cont."
    FIELD tipo_contenedor               AS CHAR                                 COLUMN-LABEL "Tipo Cont."
    FIELD plazo                         AS integer                              COLUMN-LABEL "Plazo"
    FIELD id_tipo_plazo                 AS integer                              COLUMN-LABEL "Cod.TipoPlazo"
    FIELD tipo_plazo                    AS CHAR                                 COLUMN-LABEL "Tipo Plazo"
    FIELD id_instrumento_pago           AS integer                              COLUMN-LABEL "Cod.Inst.Pago"
    FIELD instrumento_pago              AS CHAR                                 COLUMN-LABEL "Instrumento Pago"
    FIELD fob_unitario                  AS decimal                              COLUMN-LABEL "Fob Unitario"
    FIELD id_tipo_venta                 AS integer                              COLUMN-LABEL "Cod.Tipo Venta"
    FIELD tipo_venta                    AS CHAR                                 COLUMN-LABEL "Tipo Venta"
    FIELD cantidad_pallets              AS integer                              COLUMN-LABEL "Cantidad Pallets"
    FIELD cajas_x_pallets               AS integer                              COLUMN-LABEL "Cajas por Pallets"
    FIELD id_marca                      AS integer                              COLUMN-LABEL "Cod.Marca"
    FIELD marca                         AS CHAR                                 COLUMN-LABEL "Marca"
    FIELD importe_comisiones            AS decimal                              COLUMN-LABEL "Comision"                 DECIMALS 4
    FIELD coeficiente                   AS decimal                              COLUMN-LABEL "Coeficiente"              DECIMALS 6
    FIELD valor_aduana_derechos         AS decimal                              COLUMN-LABEL "Valor derechos aduana"    DECIMALS 4
    FIELD valor_aduana_reintegro        AS decimal                              COLUMN-LABEL "Valor Reintegro"          DECIMALS 4
    FIELD importe_derechos_exportacion  AS decimal                              COLUMN-LABEL "Importe Derechos"         DECIMALS 4
    FIELD importe_reintegro_fijo        AS decimal                              COLUMN-LABEL "Importe Reintegro"        DECIMALS 4
    FIELD flete                         AS decimal                              COLUMN-LABEL "Flete"                    DECIMALS 6
    FIELD seguro                        AS decimal                              COLUMN-LABEL "Seguro"                   DECIMALS 6
    FIELD thc                           AS decimal                              COLUMN-LABEL "THC"                      DECIMALS 6
    FIELD entri                         AS decimal                              COLUMN-LABEL "Entry"                    DECIMALS 6
    FIELD varios                        AS decimal                              COLUMN-LABEL "Varios"                   DECIMALS 4
    FIELD toll                          AS decimal                              COLUMN-LABEL "Toll"                     DECIMALS 4
    FIELD handling                      AS decimal                              COLUMN-LABEL "Handling"                 DECIMALS 4
    FIELD inland                        AS decimal                              COLUMN-LABEL "InLand"                   DECIMALS 4
    FIELD bunker                        AS decimal                              COLUMN-LABEL "Bunker"                   DECIMALS 4
    FIELD bl                            AS decimal                              COLUMN-LABEL "BL"                       DECIMALS 4
    FIELD thcdestino                    AS decimal                              COLUMN-LABEL "THC Destino"              DECIMALS 4
    FIELD inbalancesurcharge            AS decimal                              COLUMN-LABEL "Inbalance Surcharge"      DECIMALS 4
    FIELD dcr                           AS decimal                              COLUMN-LABEL "DCR"                      DECIMALS 4
    FIELD ebaf                          AS decimal                              COLUMN-LABEL "EBAF"                     DECIMALS 4
    FIELD ams                           AS decimal                              COLUMN-LABEL "AMS"                      DECIMALS 4.
