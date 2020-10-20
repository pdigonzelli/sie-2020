define input parameter p_sector like liq_tarjas.id_sector.
define input parameter p_empresa like liq_tarjas.id_empresa.
define input parameter p_tipoliq like liq_legajo.tipo_liquidacion.
define input parameter p_fecha_desde like liq_tarjas.fecha.
define input parameter p_fecha_hasta like liq_tarjas.fecha.
define input parameter p_archivo as character.
 

define var v_cargo as integer.
DEFINE BUFFER bconceptos FOR conceptos_abacus.
DEFINE BUFFER bliqsec FOR liq_sectores.
DEFINE VAR v_sa_agricola AS DECIMAL.
DEFINE VAR v_sa_cosecha AS DECIMAL.
DEFINE VAR v_sa_servicio AS DECIMAL.
DEFINE VAR v_sa_total AS DECIMAL.

DEFINE VAR v_sg_agricola AS DECIMAL.
DEFINE VAR v_sg_cosecha AS DECIMAL.
DEFINE VAR v_sg_servicio AS DECIMAL.
DEFINE VAR v_sg_total AS DECIMAL.



if p_archivo = "" Then p_archivo = "z:\temp\reporte01.txt".

find first liq_empresas where liq_empresas.id_empresa_liq = p_empresa no-lock no-error.
IF NOT AVAILABLE liq_empresas THEN RETURN.

output to value(p_archivo).
  put "Periodo ".
  put p_fecha_desde.
  put " - ".
  put p_fecha_hasta.
  put skip.
  PUT "Empresa ".
  PUT liq_empresas.descripcion.
  put skip.
  PUT "Sector ".
  PUT (IF p_sector = 0 THEN "Todos" ELSE STRING(p_sector)).
  PUT SKIP.
  PUT "Tipo Liq ".
  PUT (IF p_tipoliq = "" THEN "Todos" ELSE p_tipoliq).
  PUT SKIP.
  PUT "Cod.Abacus".
  PUT ";".
  PUT "Descripcion".
  PUT ";".
  PUT "SA-Agricola".
  PUT ";".
  PUT "SA-Cosecha".
  PUT ";".
  PUT "SA-Servicios".
  PUT ";".
  PUT "SA-Total".
  PUT ";".
  PUT "SG-Agricola".
  PUT ";".
  PUT "SG-Cosecha".
  PUT ";".
  PUT "SG-Servicios".
  PUT ";".
  PUT "SG-Total".

 put skip.
  
 FOR EACH conceptos_abacus WHERE id_tipo_certificacion = 10 NO-LOCK BY id_concepto:
     v_sa_agricola = 0.
     v_sa_cosecha = 0.
     v_sa_servicio = 0.
     
     FOR EACH liq_items_tarjas WHERE
          liq_items_tarjas.id_empresa = p_empresa and 
          liq_items_tarjas.fecha >= p_fecha_desde and
          liq_items_tarjas.fecha <= p_fecha_hasta AND
          liq_items_tarjas.id_tarea <> 0 and
          (liq_items_tarjas.id_codigo_abacus = conceptos_abacus.id_concepto OR
           liq_items_tarjas.id_codigo_abacus_diferencial = conceptos_abacus.id_concepto) no-lock,
          FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_tarjas.id_empresa and
                                  liq_legajos.legajo = liq_items_tarjas.legajo AND
          (IF p_tipoliq <> "" THEN liq_legajos.tipo_liquidacion = p_tipoliq ELSE TRUE) NO-LOCK:
          
          CASE liq_legajos.id_sector:
              WHEN 5 THEN v_sa_agricola = v_sa_agricola + liq_items_tarjas.cant_hs_norm.
              WHEN 6 THEN v_sa_cosecha = v_sa_cosecha + liq_items_tarjas.cant_hs_norm.
              WHEN 7 THEN v_sa_servicio = v_sa_servicio + liq_items_tarjas.cant_hs_norm.
          END CASE.
     END.

     v_sg_agricola = 0.
     v_sg_cosecha = 0.
     v_sg_servicio = 0.
     
     FOR EACH liq_items_tarjas WHERE
          liq_items_tarjas.id_empresa = p_empresa and 
          liq_items_tarjas.fecha >= p_fecha_desde and
          liq_items_tarjas.fecha <= p_fecha_hasta AND
          liq_items_tarjas.id_tarea <> 0 and
          (liq_items_tarjas.id_codigo_abacus = conceptos_abacus.id_concepto OR
           liq_items_tarjas.id_codigo_abacus_diferencial = conceptos_abacus.id_concepto) no-lock,
          FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_tarjas.id_empresa and
                                  liq_legajos.legajo = liq_items_tarjas.legajo AND
          (IF p_tipoliq <> "" THEN liq_legajos.tipo_liquidacion = p_tipoliq ELSE TRUE) NO-LOCK:
          
          CASE liq_items_tarjas.id_sector:
              WHEN 5 THEN v_sg_agricola = v_sg_agricola + liq_items_tarjas.cant_hs_norm.
              WHEN 6 THEN v_sg_cosecha = v_sg_cosecha + liq_items_tarjas.cant_hs_norm.
              WHEN 7 THEN v_sg_servicio = v_sg_servicio + liq_items_tarjas.cant_hs_norm.
          END CASE.
     END.




        export delimiter ";" 
             conceptos_abacus.id_concepto
             conceptos_abacus.descripcion
             v_sa_agricola
             v_sa_cosecha
             v_sa_servicio
            (v_sa_agricola + v_sa_cosecha + v_sa_servicio)
            v_sg_agricola
            v_sg_cosecha
            v_sg_servicio
            (v_sg_agricola + v_sg_cosecha + v_sg_servicio).

    END.

output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
