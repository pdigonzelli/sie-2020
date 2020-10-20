define input parameter p_empresa like  liq_control_tareas.id_empresa.
DEFINE INPUT PARAMETER t-empresa AS LOGICAL.
define input parameter p_sector like  liq_control_tareas.id_sector.
DEFINE INPUT PARAMETER t-sector AS LOGICAL.
define input parameter p_sucursal like  liq_control_tareas.id_sucursal.
DEFINE INPUT PARAMETER t-sucursal AS LOGICAL.
define input parameter p_id_origen like  liq_control_tareas.id_origen.
define input parameter p_id_proveedor like  liq_control_tareas.id_proveedor.
DEFINE INPUT PARAMETER t-fincas AS LOGICAL.
define input parameter p_grupo like tareas.id_grupo_tarea.
DEFINE INPUT PARAMETER p_grupo_consulta LIKE tareas.id_grupo_consulta.
define input parameter p_fecha_desde like  liq_control_tareas.fecha.
define input parameter p_fecha_hasta like  liq_control_tareas.fecha.
define input parameter p_archivo as character.
 
define temp-table rb_resumen_3eros 
FIELD cantidad              AS DECIMAL     DECIMALS 2
FIELD cantidad_adicional    AS DECIMAL     DECIMALS 2 LABEL "Cant.Adic" COLUMN-LABEL "Cant.Adic"
FIELD cantidad_adicional-1  AS DECIMAL     DECIMALS 2 LABEL "Cant.Adic" COLUMN-LABEL "Cant.Adic"
FIELD cant_horas            AS DECIMAL     DECIMALS 2 FORMAT ">>9.99" LABEL "Horas" COLUMN-LABEL "Horas"
FIELD cant_hs_compensa      AS DECIMAL     DECIMALS 2 LABEL "Hs.Comp." COLUMN-LABEL "Hs.Comp."
FIELD cant_hs_extras        AS DECIMAL     DECIMALS 2 LABEL "Hs.Extras" COLUMN-LABEL "Hs.Extras"
FIELD cant_hs_norm          AS DECIMAL     DECIMALS 2 LABEL "Hs.Norm." COLUMN-LABEL "Hs.Norm."
FIELD cant_jornal           AS DECIMAL     DECIMALS 2 FORMAT ">>9.99" LABEL "Jornal" COLUMN-LABEL "Jornal"
FIELD cant_jornal_norm      AS DECIMAL     DECIMALS 2 LABEL "Jornal Norm" COLUMN-LABEL "Jornal Norm"
FIELD compensa_hs           AS LOGICAL     FORMAT "si/no" LABEL "Compensa Hs" COLUMN-LABEL "Compensa Hs"
FIELD dni_cuil              AS CHARACTER   FORMAT "X(12)" LABEL "CUIL" COLUMN-LABEL "CUIL"
FIELD fecha                 AS DATE        FORMAT "99/99/9999"
FIELD id_empresa            AS INTEGER     FORMAT ">>>,>>9" LABEL "Empresa" COLUMN-LABEL "Empresa"
FIELD id_grupo              AS INTEGER     FORMAT ">>9" LABEL "Grupo" COLUMN-LABEL "Grupo"
FIELD id_lote               AS INTEGER     FORMAT ">>>9" LABEL "Lote" COLUMN-LABEL "Lote"
FIELD id_origen             AS INTEGER     FORMAT ">,>>9" LABEL "Origen" COLUMN-LABEL "Origen"
FIELD id_proveedor          AS INTEGER     FORMAT ">>,>>9" LABEL "Cod. Proveedor" COLUMN-LABEL "Cod. Proveedor"
FIELD id_reserva            AS INTEGER     LABEL "Reserva" COLUMN-LABEL "Reserva"
FIELD id_sector             AS INTEGER     LABEL "Sector" COLUMN-LABEL "Sector"
FIELD id_sucursal           AS INTEGER     FORMAT ">>9" LABEL "Sucursal" COLUMN-LABEL "Suc."
FIELD id_tarea              AS INTEGER     FORMAT ">>>,>>9" LABEL "Tarea" COLUMN-LABEL "Tarea"
FIELD id_tipo_planilla      AS INTEGER     FORMAT ">>9" LABEL "Tipo Planilla" COLUMN-LABEL "Tipo Planilla"
FIELD id_unidad_adicional   AS INTEGER     FORMAT ">>9" LABEL "Unid Adic" COLUMN-LABEL "Unid Adic"
FIELD id_unidad_liquidacion AS INTEGER     FORMAT ">>>,>>9" LABEL "Unid. Liquidacion" COLUMN-LABEL "Unid. Liq."
FIELD legajo                AS INTEGER     FORMAT ">>>,>>9" LABEL "Legajo" COLUMN-LABEL "Legajo"
FIELD nombre                AS CHARACTER   FORMAT "x(30)" LABEL "Apellido y Nombres" COLUMN-LABEL "Apellido y Nombres"
FIELD nro_maquina           AS INTEGER     FORMAT ">,>>>,>>9" LABEL "Nro Maquina" COLUMN-LABEL "Nro Maquina"
FIELD nro_tractor           AS INTEGER     FORMAT ">>>,>>9" LABEL "Nro Tractor" COLUMN-LABEL "Nro Tractor"
FIELD tipo_turno            AS LOGICAL     FORMAT "D/N" INITIAL TRUE LABEL "Turno" COLUMN-LABEL "Turno"
FIELD total_horas           AS DECIMAL     DECIMALS 2 LABEL "Total Hs" COLUMN-LABEL "Total Hs"
FIELD cant_destajo LIKE liq_items_control_tareas.cantidad
FIELD id_turno LIKE liq_items_control_tareas.id_turno             
FIELD nro_planilla AS INTEGER
INDEX ind-1 id_empresa id_sector legajo dni_cuil id_tarea id_proveedor id_origen id_lote fecha nro_maquina nro_tractor id_turno.

DEF VAR v_cargo AS INTEGER.

for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   


for each  liq_control_tareas where 
    (IF t-empresa = NO THEN  liq_control_tareas.id_empresa = p_empresa ELSE TRUE) AND
    (IF t-sector = NO THEN  liq_control_tareas.id_sector = p_sector ELSE TRUE) AND
    (IF t-sucursal = NO THEN  liq_control_tareas.id_sucursal = p_sucursal ELSE TRUE) AND
    (IF t-fincas = NO THEN (liq_CONTROL_tareas.id_proveedor = p_id_proveedor AND
                             liq_control_tareas.id_origen = p_id_origen)  ELSE TRUE) AND
     liq_control_tareas.fecha >= p_fecha_desde and
     liq_control_tareas.fecha <= p_fecha_hasta no-lock,
    each liq_items_control_tareas of  liq_control_tareas where id_tarea <> 0 
        /*AND liq_items_control_tareas.cantidad_adicional <> 0*/ no-lock,
        FIRST tareas OF liq_items_control_tareas WHERE 
        (IF p_grupo <> 0 THEN tareas.id_grupo_tarea = p_grupo ELSE TRUE)  and
        (IF p_grupo_consulta <> 0 THEN tareas.id_grupo_consulta = p_grupo_consulta ELSE true) NO-LOCK BY liq_items_control_tareas.id_empresa BY liq_items_control_tareas.legajo:
        
        find first liq_legajos where liq_legajos.id_empresa_liq = liq_control_tareas.id_empresa and 
            liq_legajos.legajo = liq_items_control_tareas.legajo no-lock no-error.
            if available liq_legajos Then
                v_cargo = liq_legajos.id_cargo.
              Else
                v_cargo = 0. 
    
       find first rb_resumen_3eros where rb_resumen_3eros.id_empresa = liq_items_control_tareas.id_empresa and
                  rb_resumen_3eros.id_sector = liq_items_control_tareas.id_sector and
                  rb_resumen_3eros.legajo = liq_items_control_tareas.legajo and
                  /*rb_resumen_3eros.nombre = liq_items_control_tareas.nombre and*/
                  rb_resumen_3eros.dni_cuil = liq_items_control_tareas.dni_cuil and
                  rb_resumen_3eros.id_tarea = liq_items_control_tareas.id_tarea and
                  rb_resumen_3eros.id_proveedor = liq_items_control_tareas.id_proveedor and
                  rb_resumen_3eros.id_origen = liq_items_control_tareas.id_origen and
                  rb_resumen_3eros.id_lote = liq_items_control_tareas.id_lote and
                  rb_resumen_3eros.fecha = liq_items_control_tareas.fecha and
                  rb_resumen_3eros.nro_maquina = liq_items_control_tareas.nro_maquina and
                  rb_resumen_3eros.nro_tractor = liq_items_control_tareas.nro_tractor and
                  rb_resumen_3eros.id_turno = liq_items_control_tareas.id_turno NO-ERROR.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = liq_items_control_tareas.id_empresa 
                  rb_resumen_3eros.id_sector = liq_items_control_tareas.id_sector 
                  rb_resumen_3eros.legajo = liq_items_control_tareas.legajo 
                  rb_resumen_3eros.nombre = liq_items_control_tareas.nombre 
                  rb_resumen_3eros.dni_cuil = liq_items_control_tareas.dni_cuil 
                  rb_resumen_3eros.id_tarea = liq_items_control_tareas.id_tarea 
                  rb_resumen_3eros.id_proveedor = liq_items_control_tareas.id_proveedor 
                  rb_resumen_3eros.id_origen = liq_items_control_tareas.id_origen
                  rb_resumen_3eros.id_lote = liq_items_control_tareas.id_lote
                  rb_resumen_3eros.fecha = liq_items_control_tareas.fecha
                  rb_resumen_3eros.nro_maquina = liq_items_control_tareas.nro_maquina
                  rb_resumen_3eros.nro_tractor = liq_items_control_tareas.nro_tractor
                  rb_resumen_3eros.id_turno = liq_items_control_tareas.id_turno
                  rb_resumen_3eros.nro_planilla = liq_control_tareas.nro_planilla.
          end.
 
         /* Cant Adic 1 */
          ASSIGN
          rb_resumen_3eros.cantidad_adicional = rb_resumen_3eros.cantidad_adicional + liq_items_control_tareas.cantidad_adicional
          rb_resumen_3eros.cantidad_adicional-1 = rb_resumen_3eros.cantidad_adicional-1 + liq_items_control_tareas.cantidad_adicional-1.

          /*  Hs y Js */
          ASSIGN
          rb_resumen_3eros.cant_jornal = rb_resumen_3eros.cant_jornal + liq_items_control_tareas.cant_jornal
          rb_resumen_3eros.cant_horas = rb_resumen_3eros.cant_horas + liq_items_control_tareas.cant_horas
          rb_resumen_3eros.cant_hs_compensa = rb_resumen_3eros.cant_hs_compensa + liq_items_control_tareas.cant_hs_compensa
          rb_resumen_3eros.cant_hs_extra = rb_resumen_3eros.cant_hs_extra + liq_items_control_tareas.cant_hs_extra.


end.

if p_archivo = "" Then p_archivo = "z:\temp\reporte01.txt".

output to value(p_archivo).
  put "Periodo ".
  put p_fecha_desde.
  put " - ".
  put p_fecha_hasta.
  put skip.
  put "Cod.Emp;".
  put "Nombre Emp;".
  put "Sector;". 
  put "Nombre Sector;".
  put "Legajo;". 
  put "Nombre;". 
  put "DNI/CUIL;". 
  put "Cod.;". 
  put "Tarea;". 
  put "Desc.Tarea ;".
  PUT "Grupo Tarea;".
  PUT "Grupo Consulta;".
  put "FINCA;". 
  put "Lote;".
  put "Nombre Lote;".
  PUT "Trazabilidad;".
  PUT "Fecha;".
  put "Cant Adic-1;".
  PUT "Cant Adic-2;".
  PUT "Nro Maq;".
  PUT "Nro Tractor;".
  PUT "Js Norm;".
  PUT "Hs Norm;".
  PUT "Hs Comp;".
  PUT "Hs Extras;".
  PUT "Turno;".
  PUT "Nro Planilla:".

  put skip.
  
  for each rb_resumen_3eros:
      find first liq_empresas where liq_empresas.id_empresa_liq = rb_resumen_3eros.id_empresa no-lock no-error.
      find first liq_sectores where 
          liq_sectores.id_empresa_liq = rb_resumen_3eros.id_empresa AND
          liq_sectores.id_sector = rb_resumen_3eros.id_sector no-lock no-error.
     find first tareas of rb_resumen_3eros no-lock no-error.
     find first origenes of rb_resumen_3eros no-lock no-error.
     find first lotes_plantacion where lotes_plantacion.id_proveedor = rb_resumen_3eros.id_proveedor and
     lotes_plantacion.id_origen = rb_resumen_3eros.id_origen and
     lotes_plantacion.id_lote = rb_resumen_3eros.id_lote
      no-lock no-error.
     FIND FIRST grupos_tareas OF tareas NO-LOCK NO-ERROR.
     FIND FIRST grupo_consulta OF tareas NO-LOCK NO-ERROR.
     FIND FIRST turnos_tarjas OF rb_resumen_3eros NO-LOCK NO-ERROR.

               export delimiter ";" 
                rb_resumen_3eros.id_empresa
                 if available liq_empresas Then liq_empresas.descripcion Else " "
                 rb_resumen_3eros.id_sector
                 if available liq_sectores Then liq_sectores.descripcion Else " "
                rb_resumen_3eros.legajo
                rb_resumen_3eros.nombre
                rb_resumen_3eros.dni_cuil
                rb_resumen_3eros.id_tarea
                (IF AVAILABLE tareas THEN tareas.abreviatura ELSE " ") 
                (IF AVAILABLE tareas THEN tareas.descripcion ELSE " ")
                (IF AVAILABLE grupos_tareas THEN grupos_tareas.descripcion ELSE " ")
                (IF AVAILABLE grupo_consulta THEN grupo_consulta.descripcion ELSE " ")
                if available origenes Then origenes.descripcion Else " "
                rb_resumen_3eros.id_lote
                if available lotes_plantacion Then lotes_plantacion.descripcion Else " "
                IF AVAILABLE lotes_plantacion THEN ("'" + lotes_plantacion.codigo_trazabilidad + "'") ELSE " "
                rb_resumen_3eros.fecha
                rb_resumen_3eros.cantidad_adicional
                rb_resumen_3eros.cantidad_adicional-1
                rb_resumen_3eros.nro_maquina
                rb_resumen_3eros.nro_tractor
                rb_resumen_3eros.cant_jornal
                rb_resumen_3eros.cant_horas
                rb_resumen_3eros.cant_hs_compensa
                rb_resumen_3eros.cant_hs_extra
                (IF AVAILABLE turnos_tarjas THEN turnos_tarjas.descripcion ELSE " ")
                rb_resumen_3eros.nro_planilla .
  END.
output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
