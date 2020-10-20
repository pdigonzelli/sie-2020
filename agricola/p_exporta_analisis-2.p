DEFINE INPUT PARAMETER v_periodo as INTEGER.
DEFINE INPUT PARAMETER v_desde AS DATE.
DEFINE INPUT PARAMETER v_hasta AS DATE.
DEFINE INPUT PARAMETER v_tipo_finca AS INTEGER.
DEFINE INPUT PARAMETER v_producto AS INTEGER.

define VAR v_archivo as character.
define var v_reporte as integer.
DEF VAR v_origenes AS CHARACTER.
DEF VAR v_nomprod AS CHARACTER.
DEF VAR v_nomlote AS CHARACTER.
DEF VAR vtraza AS CHARACTER.
DEF VAR vtipofinca AS CHARACTER.
DEF VAR v_up AS CHARACTER.
DEF VAR v_idlote AS INTEGER.

DEF BUFFER b_prov FOR proveedores.
DEF VAR v_tijera AS DECIMAL.
DEF VAR v_mano AS DECIMAL.
DEF VAR v_ha_efec AS DECIMAL.
DEF VAR v_ha_efec_plant AS DECIMAL.
DEF VAR v_ha_efec_plant-0 AS DECIMAL.
DEF VAR v_total_ha_efec_plant AS DECIMAL.
DEF VAR v_total_plan-0 AS DECIMAL.
DEF VAR v_tn_ha AS DECIMAL.
DEF VAR v_tn_ha-0 AS DECIMAL.
DEF VAR v_tn_ha-1 AS DECIMAL.
DEF VAR v_tn_ha-2 AS DECIMAL.
DEF VAR v_tn_ha-3 AS DECIMAL.
DEF VAR v_tn_ha-4 AS DECIMAL.


DEF BUFFER binv FOR inv_lotes_plantacion.
DEF BUFFER bdist FOR distancias.
DEF BUFFER bcontrol FOR CONTROL_finca_lote.
DEF BUFFER bliqcontrol FOR liq_CONTROL_finca_lote.
DEF VAR v_desde-1 AS DATE.
DEF VAR v_hasta-1 AS DATE.
DEF VAR v_t_plantado AS DECIMAL.
DEF VAR v_anio AS INTEGER.
DEF VAR v_periodo_desde AS INTEGER.
DEF VAR v_cuenta AS INTEGER.
DEF VAR v_valor_tijera AS DECIMAL.
DEF VAR v_valor_mano AS DECIMAL.
DEF VAR v_plantas_ha AS DECIMAL.
DEF VAR v_fallas AS DECIMAL.
DEF VAR v_fecha_desde AS DATE.
DEF VAR v_fecha_hasta AS DATE.

DEF VAR v_item AS INTEGER.

v_periodo_desde = YEAR(v_desde).
v_archivo = "z:\temp\analisis.txt".

output to value(v_archivo).



PUT "ANALISIS DE CAMPA¥A - Hist¢rico".
PUT SKIP.
PUT "Per¡odo ".
PUT  v_periodo.
PUT SKIP.

put "Codigo Unico Lote" ";"
    "Tipo Finca" ";"
    "Productor" ";"
    "Finca" ";"
    "Lote" ";"
    "Total Plantado" ";"
    "Especie" ";"
    "Variedad" ";"
    "Pie" ";"
    "Distancia" ";"
    "Plantas x Ha" ";"
    "Tipo Plantacion" ";"
    "Anio Plant." ";"
    "Edad" ";"
    "Pl Ejerc" ";"
    "Pl Form" ";"
    "Pl 5-10" ";"
    "Pl 11-20" ";"
    "Pl + de 20" ";"
    "Fallas" ";"
    "Potencial Lote" ";"
    "Ha Efec" ";"
    "Ha Efec Plant Actual" ";"
    "Bines Tijera" ";"
    "Bines Mano" ";"
    "Tn/Ha" + string(v_periodo) FORMAT "x(15)" ";".
    
    DO v_anio = v_periodo_desde TO (v_periodo - 1):
        PUT  "Tn/Ha " + STRING(v_anio) FORMAT "x(15)" ";". 
    END.
 put skip.




v_t_plantado = 0.
v_item = 0.
for each inv_lotes_plantacion WHERE inv_lotes_plantacion.anio = v_periodo and
    (IF v_producto <> 0 THEN inv_lotes_plantacion.id_articulo = v_producto ELSE TRUE) 
    NO-LOCK,
    FIRST origenes OF inv_lotes_plantacion 
     WHERE (if v_tipo_finca <> 0 THEN origenes.id_tipo_origen = v_tipo_finca ELSE origenes.id_tipo_origen <> 2)  NO-LOCK
    break  BY id_tipo_origen 
    BY inv_lotes_plantacion.id_proveedor 
    BY inv_lotes_plantacion.id_origen 
    BY inv_lotes_plantacion.nro_orden 
    BY id_lote:
    
        if available origenes THEN v_origenes = origenes.descripcion.
                              ELSE v_origenes = "".
    
        find first b_prov where b_prov.id_proveedor = inv_lotes_plantacion.id_proveedor no-lock no-error.
        if available b_prov Then
           v_nomprod = b_prov.nombre.
          Else
           v_nomprod = "".
    
         v_item = v_item + 1.
        find first lotes_plantacion where 
         lotes_plantacion.id_proveedor = inv_lotes_plantacion.id_proveedor and
         lotes_plantacion.id_origen = inv_lotes_plantacion.id_origen and
         lotes_plantacion.id_lote = inv_lotes_plantacion.id_lote no-lock no-error.
        if available lotes_plantacion Then
          DO:
           v_idlote = lotes_plantacion.id_lote.
           v_nomlote = lotes_plantacion.descripcion.
          END.
          Else
           DO:
               v_idlote = ?.
               v_nomlote = "".
          END.
    
          IF inv_lotes_plantacion.id_proveedor = 1 THEN vtipofinca = "Propia".
                                                   ELSE vtipofinca = "".

           FIND FIRST productos_terminados OF inv_lotes_plantacion NO-LOCK NO-ERROR.
           FIND FIRST variedades OF inv_lotes_plantacion NO-LOCK NO-ERROR.
           FIND FIRST pies WHERE pies.id_empresa = 1 and
                                 pies.id_pie = inv_lotes_plantacion.id_pie  NO-LOCK NO-ERROR.


       
      IF LAST-OF(inv_lotes_plantacion.id_lote) THEN
           DO:
               
              FIND FIRST distancias OF inv_lotes_plantacion NO-LOCK NO-ERROR.
               IF AVAILABLE distancias THEN
               DO:
                   v_plantas_ha = distancias.plantas_ha.
                   FIND FIRST tipo_plantacion OF distancias  NO-LOCK NO-ERROR.
               END.
               v_t_plantado = v_t_plantado + inv_lotes_plantacion.total_plantado.
               

               v_fecha_desde = DATE("01/01/" + STRING(v_periodo)).
               IF v_periodo <> YEAR(TODAY) THEN
                    v_fecha_hasta = DATE("31/12/" + STRING(v_periodo)).
                 ELSE
                    v_fecha_hasta = v_hasta. 

               /* Datos de cosecha actual*/
               FOR EACH CONTROL_finca_lote WHERE 
                 CONTROL_finca_lotes.fecha >= v_fecha_desde AND
                 CONTROL_finca_lotes.fecha <= v_fecha_hasta and
                 control_finca_lotes.id_proveedor = inv_lotes_plantacion.id_proveedor and
                 control_finca_lotes.id_origen = inv_lotes_plantacion.id_origen AND
                 control_finca_lotes.id_lote = inv_lotes_plantacion.id_lote NO-LOCK USE-INDEX fecha_producto:
                    ASSIGN v_tijera = v_tijera + (CONTROL_finca_lotes.cant_bins_B_tijera + CONTROL_finca_lotes.cant_bins_V_tijera)
                          v_mano = v_mano + (CONTROL_finca_lotes.cant_bins_B_mano + CONTROL_finca_lotes.cant_bins_V_mano).
                END.

                FOR EACH liq_CONTROL_finca_lote WHERE 
                  liq_CONTROL_finca_lotes.fecha >= v_fecha_desde AND
                  liq_CONTROL_finca_lotes.fecha <= v_fecha_hasta and
                  liq_control_finca_lotes.id_proveedor = inv_lotes_plantacion.id_proveedor and
                  liq_control_finca_lotes.id_origen = inv_lotes_plantacion.id_origen AND
                  liq_control_finca_lotes.id_lote = inv_lotes_plantacion.id_lote NO-LOCK USE-INDEX fecha_producto:
                     ASSIGN v_tijera = v_tijera + (liq_CONTROL_finca_lotes.cant_bins_B_tijera + liq_CONTROL_finca_lotes.cant_bins_V_tijera)
                           v_mano = v_mano + (liq_CONTROL_finca_lotes.cant_bins_B_mano + liq_CONTROL_finca_lotes.cant_bins_V_mano).
                 END.
           
           
               v_ha_efec = v_ha_efec + (IF v_plantas_ha <> 0 THEN ((inv_lotes_plantacion.total_plantado + inv_lotes_plantacion.fallas) / v_plantas_ha) ELSE 0).
                v_ha_efec_plant = v_ha_efec_plant + (IF v_plantas_ha <> 0 THEN (inv_lotes_plantacion.total_plantado) / v_plantas_ha ELSE 0).
                v_total_ha_efec_plant = v_total_ha_efec_plant + v_ha_efec_plant.

          END.

           IF LAST-OF(inv_lotes_plantacion.nro_orden) THEN
           DO:
               v_tn_ha = (IF v_ha_efec_plant <> 0 THEN ((v_tijera + v_mano) * 0.4) / v_ha_efec_plant ELSE 0).

               v_cuenta = 0.
               v_total_plan-0.
               DO v_anio = v_periodo_desde TO (v_periodo - 1):
                  v_cuenta = v_cuenta + 1.
                  RUN p_dev_datos_cosecha.p (INPUT v_anio,
                                             INPUT inv_lotes_plantacion.id_proveedor,
                                             INPUT inv_lotes_plantacion.id_origen,
                                             INPUT inv_lotes_plantacion.nro_orden,
                                             OUTPUT v_total_plan-0,
                                             OUTPUT v_tn_ha-0).


                  CASE v_cuenta:
                      WHEN 1 THEN  v_tn_ha-1 = v_tn_ha-0.
                      WHEN 2 THEN  v_tn_ha-2 = v_tn_ha-0.
                      WHEN 3 THEN  v_tn_ha-3 = v_tn_ha-0.
                      WHEN 4 THEN  v_tn_ha-4 = v_tn_ha-0.
                  END CASE.
               END.



                export delimiter ";"
                     STRING(origenes.id_origen) + " | " + string(inv_lotes_plantacion.nro_orden)
                     vtipofinca
                     v_nomprod
                     v_origenes
                     v_nomlote
                     string(v_t_plantado,">>>>9.99")
                     (IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE " ")  
                     (IF AVAILABLE variedades THEN variedades.descripcion ELSE " ")  
                     (IF AVAILABLE pies THEN pies.descripcion ELSE " ")
                     (IF AVAILABLE distancias THEN distancias.descripcion ELSE " ")
                     (IF AVAILABLE distancias THEN string(distancias.plantas_ha,">>>>>>9.99") ELSE " ")
                     (IF AVAILABLE tipo_plantacion THEN tipo_plantacion.descripcion ELSE " ")
                     inv_lotes_plantacion.anio_plantacion
                     (v_periodo - inv_lotes_plantacion.anio_plantacion)
                     inv_lotes_plantacion.plantas_ejercicio
                     inv_lotes_plantacion.plantas_formacion
                     inv_lotes_plantacion.plantas_5-10
                     inv_lotes_plantacion.plantas_11-20
                     inv_lotes_plantacion.plantas_21
                     inv_lotes_plantacion.fallas
                     string((inv_lotes_plantacion.total_plantado + inv_lotes_plantacion.fallas),">>>>9.99")
                     STRING(v_ha_efec,">>>>9.99")
                     STRING(v_ha_efec_plant, ">>>>9.99")
                     v_tijera
                     v_mano
                     STRING(v_tn_ha, ">>>>9.99")
                     STRING(v_tn_ha-1, ">>>>9.99")
                     STRING(v_tn_ha-2, ">>>>9.99")
                     STRING(v_tn_ha-3, ">>>>9.99")
                     STRING(v_tn_ha-4, ">>>>9.99").

              put skip.
              v_tijera = 0.
              v_mano = 0.
              v_ha_efec = 0.
              v_ha_efec_plant = 0.
              v_total_ha_efec_plant = 0. 
              v_t_plantado = 0.

              v_valor_tijera = 0.
              v_valor_mano = 0.

           END.
 end.
output close.

run p_texto_a_excel.p (input "TEXT;" + v_archivo).
