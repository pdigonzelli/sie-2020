DEF INPUT PARAMETER p_id_empresa AS INTEGER.
DEF INPUT PARAMETER p_id_sector AS INTEGER.
DEF INPUT PARAMETER p_fecha_desde AS DATE. 
DEF INPUT PARAMETER p_fecha_hasta AS DATE. 
 
DEF VAR v_archivo AS CHARACTER.

v_archivo = "z:\temp\partediario.txt".

output to value(v_archivo).
  put "Fecha; Depto; Finca/Sector; Empresa; Turno; Legajo; Apellido y Nombre;Detalle".
  PUT SKIP.

for each liq_control_tareas no-lock where 
        liq_control_tareas.id_empresa = p_id_empresa and 
        liq_control_tareas.fecha >= p_fecha_desde and 
        liq_control_tareas.fecha <= p_fecha_hasta  AND
        liq_control_tareas.id_sector = p_id_sector ,
        each liq_items_control_tareas of liq_control_tareas no-lock,
        FIRST tareas WHERE tareas.id_tarea = liq_items_control_tareas.id_tarea
        AND tareas.id_grupo_tarea = 13 NO-LOCK BREAK
        by liq_items_control_tareas.id_empresa by liq_items_control_tareas.legajo 
        BY liq_items_control_tareas.fecha:

      IF LAST-OF(liq_items_control_tareas.fecha) THEN
      DO:

          FIND FIRST liq_legajos WHERE liq_legajos.id_empresa = liq_control_tareas.id_empresa AND
                                       liq_legajos.legajo = liq_items_control_tareas.legajo  NO-LOCK NO-ERROR.
          FIND FIRST liq_sectores OF liq_legajos NO-LOCK NO-ERROR. 
          
          FIND FIRST liq_centros_costos OF liq_legajos NO-LOCK NO-ERROR. 
          FIND FIRST liq_empresas OF liq_legajos  NO-LOCK NO-ERROR.
          
              PUT liq_items_control_tareas.fecha.
              PUT ";".
              IF AVAILABLE liq_sectores THEN PUT liq_sectores.descripcion. ELSE PUT "".
              PUT ";".
              IF AVAILABLE liq_centros_costos THEN PUT liq_centros_costos.descripcion. ELSE PUT "".
              PUT ";".
              IF AVAILABLE liq_empresas THEN PUT liq_empresas.descripcion. ELSE PUT "".
              PUT ";".
              PUT liq_items_control_tareas.id_turno.
              PUT ";".
              PUT liq_items_control_tareas.legajo.
              PUT ";".
              PUT liq_legajos.apellido_nombre.
              PUT ";".
              PUT tareas.descripcion.
              PUT SKIP.
          
      END.
END.
OUTPUT CLOSE.

run p_texto_a_excel.p (input "TEXT;" + v_archivo).
