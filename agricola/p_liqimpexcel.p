DEFINE TEMP-TABLE t-personal
       field legajo like liq_items_control_tareas.legajo
       field nombre like liq_items_control_tareas.nombre
       field id_concepto like r_tareas_unidades.id_concepto
       field cantidad like liq_items_control_tareas.cantidad
       field id_centro_abacus like liq_legajos.id_centro_costo.

define temp-table t-resumen
       field id_centro_abacus like liq_legajos.id_centro_costo
       field id_concepto like r_tareas_unidades.id_concepto
       field cantidad like liq_items_control_tareas.cantidad.

DEFINE INPUT PARAMETER TABLE FOR t-personal.
DEFINE INPUT PARAMETER TABLE FOR t-resumen.

DEFINE INPUT PARAMETER v_id_empresa AS INTEGER.

DEFINE VAR p_archivo AS CHARACTER.

p_archivo = "z:\temp\archexcel.txt".

OUTPUT TO VALUE(p_archivo).
PUT "Cod CC;Centro Costo;Legajo;Nombre;Cod Concepto;Concepto;Cantidad:".
PUT SKIP.
for each t-personal no-lock  break by t-personal.id_centro_abacus by t-personal.legajo 
    by t-personal.nombre :
      
   find first liq_centros_costos WHERE
       liq_centros_costos.id_centro_costo = t-personal.id_centro_abacus no-lock no-error.
   
    find first conceptos_abacus of t-personal no-lock.       
   EXPORT DELIMITER ";"
          t-personal.id_centro_abacus 
          (IF AVAILABLE liq_centros_costos THEN liq_centros_costos.descripcion  ELSE " ")
           t-personal.legajo 
           t-personal.nombre
           t-personal.id_concepto 
           (IF AVAILABLE conceptos_abacus THEN conceptos_abacus.descripcion ELSE " ")
           t-personal.cantidad.
end.   
OUTPUT CLOSE.

run p_texto_a_excel.p (input "TEXT;" + p_archivo).


