DEF INPUT PARAMETER v_anio AS INTEGER.
DEF INPUT PARAMETER v_proveedor AS INTEGER.
DEF INPUT PARAMETER v_origen AS INTEGER.
DEF INPUT PARAMETER v_nro_orden AS INTEGER.
DEF OUTPUT PARAMETER v_total_pl-10 AS DECIMAL.
DEF OUTPUT PARAMETER v_tn_ha-10 AS DECIMAL.

DEF VAR v_fecha_desde AS DATE.
DEF VAR v_fecha_hasta AS DATE.
         
DEF BUFFER bcontrol FOR CONTROL_finca_lote.
DEF BUFFER bliqcontrol FOR liq_CONTROL_finca_lote.
DEF BUFFER binv-1 FOR inv_lotes_plantacion.
DEF BUFFER bdist FOR distancias.
DEF VAR v_tijera-10 AS DECIMAL.
DEF VAR v_mano-10 AS DECIMAL.
DEF VAR v_plantas_ha-10 AS DECIMAL.
DEF VAR v_ha_efec_plant-10 AS DECIMAL.
DEF VAR v_ha_efec-10 AS DECIMAL.
DEF VAR v_total_ha_efec_plant-10 AS DECIMAL.





v_fecha_desde = DATE("01/01/" + STRING(v_anio)).
v_fecha_hasta = DATE("31/12/" + STRING(v_anio)).

v_mano-10 = 0.
v_tijera-10 = 0.
v_total_pl-10 = 0.
v_plantas_ha-10 = 0.


FOR EACH binv-1 WHERE binv-1.anio = v_anio AND
           binv-1.id_proveedor = v_proveedor AND
           binv-1.id_origen = v_origen AND
           binv-1.nro_orden = v_nro_orden  NO-LOCK
BREAK   BY binv-1.id_proveedor 
         BY binv-1.id_origen 
         BY binv-1.nro_orden BY id_lote:

FIND FIRST bdist OF binv-1 NO-LOCK NO-ERROR.
IF AVAILABLE bdist THEN
     v_plantas_ha-10 = bdist.plantas_ha.

    IF LAST-OF(binv-1.id_lote) THEN
    DO:
        v_total_pl-10 = v_total_pl-10 + binv-1.total_plantado.


               /* Datos de cosecha */
               FOR EACH CONTROL_finca_lote WHERE 
                 CONTROL_finca_lotes.fecha >= v_fecha_desde AND
                 CONTROL_finca_lotes.fecha <= v_fecha_hasta and
                 control_finca_lotes.id_proveedor = binv-1.id_proveedor and
                 control_finca_lotes.id_origen = binv-1.id_origen AND
                 control_finca_lotes.id_lote = binv-1.id_lote NO-LOCK USE-INDEX fecha_producto:
                    ASSIGN v_tijera-10 = v_tijera-10 + (CONTROL_finca_lotes.cant_bins_B_tijera + CONTROL_finca_lotes.cant_bins_V_tijera)
                          v_mano-10 = v_mano-10 + (CONTROL_finca_lotes.cant_bins_B_mano + CONTROL_finca_lotes.cant_bins_V_mano).
                END.

                FOR EACH liq_CONTROL_finca_lote WHERE 
                  liq_CONTROL_finca_lotes.fecha >= v_fecha_desde AND
                  liq_CONTROL_finca_lotes.fecha <= v_fecha_hasta and
                  liq_control_finca_lotes.id_proveedor = binv-1.id_proveedor and
                  liq_control_finca_lotes.id_origen = binv-1.id_origen AND
                  liq_control_finca_lotes.id_lote = binv-1.id_lote NO-LOCK USE-INDEX fecha_producto:
                     ASSIGN v_tijera-10 = v_tijera-10 + (liq_CONTROL_finca_lotes.cant_bins_B_tijera + liq_CONTROL_finca_lotes.cant_bins_V_tijera)
                           v_mano-10 = v_mano-10 + (liq_CONTROL_finca_lotes.cant_bins_B_mano + liq_CONTROL_finca_lotes.cant_bins_V_mano).
                 END.
           

               v_ha_efec-10 = v_ha_efec-10 + (IF v_plantas_ha-10 <> 0 THEN ((binv-1.total_plantado + binv-1.fallas) / v_plantas_ha-10) ELSE 0).
               v_ha_efec_plant-10 = v_ha_efec_plant-10 + (IF v_plantas_ha-10 <> 0 THEN (binv-1.total_plantado) / v_plantas_ha-10 ELSE 0).
               v_total_ha_efec_plant-10 = v_total_ha_efec_plant-10 + v_ha_efec_plant-10.
           
           END.

           IF LAST-OF(binv-1.nro_orden) THEN
           DO:
            v_tn_ha-10 = (IF v_ha_efec_plant-10 <> 0 THEN ((v_tijera-10 + v_mano-10) * 0.4) / v_ha_efec_plant-10 ELSE 0).
           END.
END.
 
