DEFINE INPUT PARAMETER v_periodo as INTEGER.
DEFINE INPUT PARAMETER v_desde AS DATE.
DEFINE INPUT PARAMETER v_hasta AS DATE.

define VAR v_archivo as character.
define var v_reporte as integer.
DEF VAR v_origenes AS CHARACTER.
DEF VAR v_nomprod AS CHARACTER.
DEF VAR v_nomlote AS CHARACTER.
DEF VAR vtraza AS CHARACTER.
DEF VAR vtipofinca AS CHARACTER.
DEF VAR v_producto AS INTEGER.
DEF VAR v_up AS CHARACTER.
DEF VAR v_idlote AS INTEGER.

DEF BUFFER b_prov FOR proveedores.
DEF VAR v_tijera AS DECIMAL.
DEF VAR v_mano AS DECIMAL.
DEF VAR v_ha_efec_plant AS DECIMAL.
DEF VAR v_total_tijera AS DECIMAL.
DEF VAR v_total_mano AS DECIMAL.
DEF VAR v_total_ha_efec_plant AS DECIMAL.
DEF VAR v_tn_ha AS DECIMAL.
DEF VAR v_total_plantado AS DECIMAL.

v_archivo = "z:\temp\analisis.txt".

output to value(v_archivo).


PUT "ANALISIS INVENTARIO PLANTACION".
PUT SKIP.
PUT "Periodo ".
PUT  v_periodo.
PUT SKIP.

put "Tipo Finca" ";"
    "Productor" ";"
    "Finca" ";"
    "Cod." ";"
    "Lote" ";"
    "UP" ";"
    "CT" ";"
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
    "Ha Efec Plant" ";"
    "Bines Tijera" ";"
    "Bines Mano" ";"
    "Tn/Ha" ";".

put skip.





for each inv_lotes_plantacion WHERE inv_lotes_plantacion.anio = v_periodo AND
    inv_lotes_plantacion.id_proveedor = 1 NO-LOCK,
    FIRST origenes OF inv_lotes_plantacion no-lock,
    first lotes_plantacion where 
             lotes_plantacion.id_proveedor = inv_lotes_plantacion.id_proveedor and
             lotes_plantacion.id_origen = inv_lotes_plantacion.id_origen and
             lotes_plantacion.id_lote = inv_lotes_plantacion.id_lote no-lock
    break  BY id_tipo_origen 
    BY inv_lotes_plantacion.id_proveedor 
    BY inv_lotes_plantacion.id_origen 
    BY inv_lotes_plantacion.nro_orden BY lotes_plantacion.codigo_trazabilidad BY lotes_plantacion.id_lote:
    




        if available origenes Then
          DO:
            v_origenes = origenes.descripcion.
            v_up = STRING(origenes.id_finca_senasa,"999") + " - " .
          END.
          Else
          DO:
           v_origenes = "".
           v_up = "". 
          END.
    
        find first b_prov where b_prov.id_proveedor = inv_lotes_plantacion.id_proveedor no-lock no-error.
        if available b_prov Then
           v_nomprod = b_prov.nombre.
          Else
           v_nomprod = "".
    
    
           v_idlote = lotes_plantacion.id_lote.
           v_nomlote = lotes_plantacion.descripcion.
           v_up = v_up + STRING(lotes_plantacion.id_lote_senasa,"999").
           vtraza = "|" + lotes_plantacion.codigo_trazabilidad + "|".
    
          IF inv_lotes_plantacion.id_proveedor = 1 THEN vtipofinca = "Propia".
                                                   ELSE vtipofinca = "". 

           FIND FIRST productos_terminados OF inv_lotes_plantacion NO-LOCK NO-ERROR.
           FIND FIRST variedades OF inv_lotes_plantacion NO-LOCK NO-ERROR.
           FIND FIRST pies WHERE pies.id_empresa = 1 and
                                 pies.id_pie = inv_lotes_plantacion.id_pie  NO-LOCK NO-ERROR.
           FIND FIRST distancias OF inv_lotes_plantacion NO-LOCK NO-ERROR.
           IF AVAILABLE distancias THEN 
           DO:
               FIND FIRST tipo_plantacion OF distancias  NO-LOCK NO-ERROR.
           END.


           IF LAST-OF(lotes_plantacion.id_lote) THEN
           DO:
               v_total_plantado = v_total_plantado + inv_lotes_plantacion.total_plantado.

               /* Datos de cosecha actual*/
               FOR EACH CONTROL_finca_lote WHERE CONTROL_finca_lotes.fecha >= v_desde AND
                 CONTROL_finca_lotes.fecha <= v_hasta and
                 control_finca_lotes.id_proveedor = inv_lotes_plantacion.id_proveedor and
                 control_finca_lotes.id_origen = inv_lotes_plantacion.id_origen AND
                 control_finca_lotes.id_lote = inv_lotes_plantacion.id_lote NO-LOCK USE-INDEX fecha_producto:
                    ASSIGN v_tijera = v_tijera + (CONTROL_finca_lotes.cant_bins_B_tijera + CONTROL_finca_lotes.cant_bins_V_tijera)
                          v_mano = v_mano + (CONTROL_finca_lotes.cant_bins_B_mano + CONTROL_finca_lotes.cant_bins_V_mano).
                END.

                FOR EACH liq_CONTROL_finca_lote WHERE liq_CONTROL_finca_lotes.fecha >= v_desde AND
                  liq_CONTROL_finca_lotes.fecha <= v_hasta and
                  liq_control_finca_lotes.id_proveedor = inv_lotes_plantacion.id_proveedor and
                  liq_control_finca_lotes.id_origen = inv_lotes_plantacion.id_origen AND
                  liq_control_finca_lotes.id_lote = inv_lotes_plantacion.id_lote NO-LOCK USE-INDEX fecha_producto:
                     ASSIGN v_tijera = v_tijera + (liq_CONTROL_finca_lotes.cant_bins_B_tijera + liq_CONTROL_finca_lotes.cant_bins_V_tijera)
                           v_mano = v_mano + (liq_CONTROL_finca_lotes.cant_bins_B_mano + liq_CONTROL_finca_lotes.cant_bins_V_mano).
                 END.
                 
                 v_total_tijera = v_total_tijera + v_tijera.
                 v_total_mano = v_total_mano + v_mano.

                 v_ha_efec_plant = v_ha_efec_plant + (IF plantas_ha <> 0 THEN (inv_lotes_plantacion.total_plantado) / plantas_ha ELSE 0).
                 v_total_ha_efec_plant = v_total_ha_efec_plant + v_ha_efec_plant.


                v_tn_ha = (IF v_total_ha_efec_plant <> 0 THEN ((v_total_tijera + v_total_mano) * 0.4) / v_total_ha_efec_plant ELSE 0).


              export delimiter ";" 
                     vtipofinca
                     v_nomprod
                     v_origenes
                     v_idlote
                     v_nomlote
                     v_up 
                     vtraza
                     string(inv_lotes_plantacion.total_plantado,">>>>9.99")
                     (IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE " ")  
                     (IF AVAILABLE variedades THEN variedades.descripcion ELSE " ")  
                     (IF AVAILABLE pies THEN pies.descripcion ELSE " ")
                     (IF AVAILABLE distancias THEN distancias.descripcion ELSE " ")
                     (IF AVAILABLE distancias THEN string(distancias.plantas_ha,">>>>>>9.99") ELSE "")
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
                     (IF AVAILABLE distancias THEN STRING(IF plantas_ha <> 0 THEN ((inv_lotes_plantacion.total_plantado + inv_lotes_plantacion.fallas) / plantas_ha) ELSE 0,">>>>9.99") ELSE "")
                     STRING(v_ha_efec_plant, ">>>>9.99")
                     v_tijera
                     v_mano
                     STRING(v_tn_ha, ">>>>9.99").
                     
              put skip.
              v_tijera = 0.
              v_mano = 0.
              v_total_tijera = 0.
              v_total_mano = 0.
              v_ha_efec_plant = 0.
              v_total_ha_efec_plant = 0. 
              v_total_plantado = 0.



           END.
  end.
output close.

run p_texto_a_excel.p (input "TEXT;" + v_archivo).
