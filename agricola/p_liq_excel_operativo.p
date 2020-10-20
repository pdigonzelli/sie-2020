define input parameter p_empresa like  liq_tarjas.id_empresa.
DEFINE INPUT PARAMETER t-empresa AS LOGICAL.
define input parameter p_sector like  liq_tarjas.id_sector.
DEFINE INPUT PARAMETER t-sector AS LOGICAL.
define input parameter p_sucursal like  liq_tarjas.id_sucursal.
DEFINE INPUT PARAMETER t-sucursal AS LOGICAL.
define input parameter p_id_origen like  liq_tarjas.id_origen.
define input parameter p_id_proveedor like  liq_tarjas.id_proveedor.
DEFINE INPUT PARAMETER t-fincas AS LOGICAL.
define input parameter p_grupo like liq_tareas.id_grupo_tarea.
DEFINE INPUT PARAMETER p_grupo_consulta LIKE liq_tareas.id_grupo_consulta.
define input parameter p_fecha_desde like  liq_tarjas.fecha.
define input parameter p_fecha_hasta like  liq_tarjas.fecha.
define input parameter p_archivo as character.
 
define temp-table rb_resumen_3eros 
FIELD cantidad              AS DECIMAL     DECIMALS 2
FIELD cantidad_adicional    AS DECIMAL     DECIMALS 2 LABEL "Cant.Adic" COLUMN-LABEL "Cant.Adic"
FIELD cantidad_adicional-1  AS DECIMAL     DECIMALS 2 LABEL "Cant.Adic" COLUMN-LABEL "Cant.Adic"
FIELD cant_hs_norm          AS DECIMAL     DECIMALS 2 LABEL "Hs.Norm." COLUMN-LABEL "Hs.Norm."
FIELD hs_acond_finca       AS DECIMAL     DECIMALS 2 FORMAT ">>9.99" LABEL "HAF" COLUMN-LABEL "HAF"
FIELD hs_plus_tareas_automatico  AS DECIMAL     DECIMALS 2 LABEL "HPTA" COLUMN-LABEL "HPTA"
FIELD hs_plus_tareas_trabajadas  AS DECIMAL     DECIMALS 2 LABEL "HPTT" COLUMN-LABEL "HPTT"
FIELD hs_adicionales_tareas_trabajadas           AS DECIMAL  DECIMALS 2 LABEL "HATT" COLUMN-LABEL "HATT"
FIELD TOTAL_horas              AS DECIMAL DECIMALS 2 LABEL "Total Hs"  COLUMN-LABEL "Total Hs"
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
FIELD id_codigo_abacus AS INTEGER     FORMAT ">>>,>>9" LABEL "Cod.Abacus" COLUMN-LABEL "Cod.Abacus"
FIELD legajo                AS INTEGER     FORMAT ">>>,>>9" LABEL "Legajo" COLUMN-LABEL "Legajo"
FIELD nombre                AS CHARACTER   FORMAT "x(30)" LABEL "Apellido y Nombres" COLUMN-LABEL "Apellido y Nombres"
FIELD nro_maquina           AS INTEGER     FORMAT ">,>>>,>>9" LABEL "Nro Maquina" COLUMN-LABEL "Nro Maquina"
FIELD nro_tractor           AS INTEGER     FORMAT ">>>,>>9" LABEL "Nro Tractor" COLUMN-LABEL "Nro Tractor"
FIELD tipo_turno            AS LOGICAL     FORMAT "D/N" INITIAL TRUE LABEL "Turno" COLUMN-LABEL "Turno"
FIELD id_turno LIKE liq_items_tarjas.id_turno             
FIELD nro_planilla AS INTEGER
FIELD desc-tipotarea LIKE tipo_tarea.descripcion
FIELD desc-categoria LIKE liq_categorias.descripcion
FIELD id_codigo_abacus_cantidad AS INTEGER
FIELD id_codigo_abacus_diferencial AS INTEGER
FIELD id_codigo_abacus_adicional AS INTEGER
FIELD id_sector_abacus AS INTEGER
INDEX ind-1 id_empresa id_sector legajo dni_cuil id_tarea id_proveedor id_origen id_lote fecha nro_maquina nro_tractor id_turno.

DEF VAR v_cargo AS INTEGER.
DEF VAR v_tipo_tarea LIKE tipo_tarea.descripcion.
DEF VAR v_categoria LIKE liq_categorias.descripcion.
DEF BUFFER bcon01 FOR liq_conceptos.
DEF BUFFER bcon02 FOR liq_conceptos.
DEF BUFFER bcon03 FOR liq_conceptos.
DEF BUFFER bcant-1 FOR tipo_cant_adicional.
DEF BUFFER bcant-2 FOR tipo_cant_adicional.

for each rb_resumen_3eros:
   delete rb_resumen_3eros.
end.   


for each  liq_tarjas where 
    (IF t-empresa = NO THEN  liq_tarjas.id_empresa = p_empresa ELSE TRUE) AND
    (IF t-sector = NO THEN  liq_tarjas.id_sector = p_sector ELSE TRUE) AND
    (IF t-sucursal = NO THEN  liq_tarjas.id_sucursal = p_sucursal ELSE TRUE) AND
    (IF t-fincas = NO THEN (liq_tarjas.id_proveedor = p_id_proveedor AND
                             liq_tarjas.id_origen = p_id_origen)  ELSE TRUE) AND
     liq_tarjas.fecha >= p_fecha_desde and
     liq_tarjas.fecha <= p_fecha_hasta no-lock,
    each liq_items_tarjas of  liq_tarjas where id_tarea <> 0 no-lock,
        FIRST liq_tareas OF liq_items_tarjas WHERE 
        (IF p_grupo <> 0 THEN liq_tareas.id_grupo_tarea = p_grupo ELSE TRUE)  and
        (IF p_grupo_consulta <> 0 THEN liq_tareas.id_grupo_consulta = p_grupo_consulta ELSE true) NO-LOCK BY liq_items_tarjas.id_empresa BY liq_items_tarjas.legajo:
        
        find first liq_legajos where liq_legajos.id_empresa_liq = liq_tarjas.id_empresa and 
            liq_legajos.legajo = liq_items_tarjas.legajo no-lock no-error.
            if available liq_legajos Then
            DO:
                v_cargo = liq_legajos.id_cargo.
                FIND FIRST liq_categorias WHERE liq_categorias.id_convenio = liq_legajos.id_convenio AND
                    liq_categorias.id_categoria = liq_legajos.id_categoria NO-LOCK NO-ERROR.
                IF AVAILABLE liq_categorias THEN v_categoria = liq_categorias.descripcion.
                                            ELSE v_categoria = "".
            END.
          Else
                v_cargo = 0. 

       FIND FIRST categorias_tareas OF liq_tareas NO-LOCK NO-ERROR.
       IF AVAILABLE categorias_tareas THEN v_tipo_tarea = categorias_tareas.descripcion.
                                ELSE v_tipo_tarea = "".

    
       find first rb_resumen_3eros where rb_resumen_3eros.id_empresa = liq_items_tarjas.id_empresa and
                  rb_resumen_3eros.id_sector = liq_items_tarjas.id_sector and
                  rb_resumen_3eros.legajo = liq_items_tarjas.legajo and
                  rb_resumen_3eros.dni_cuil = liq_items_tarjas.dni_cuil and
                  rb_resumen_3eros.id_tarea = liq_items_tarjas.id_tarea and
                  rb_resumen_3eros.id_proveedor = liq_items_tarjas.id_proveedor and
                  rb_resumen_3eros.id_origen = liq_items_tarjas.id_origen and
                  rb_resumen_3eros.id_lote = liq_items_tarjas.id_lote and
                  rb_resumen_3eros.fecha = liq_items_tarjas.fecha and
                  rb_resumen_3eros.nro_maquina = liq_items_tarjas.nro_maquina and
                  rb_resumen_3eros.nro_tractor = liq_items_tarjas.nro_tractor and
                  rb_resumen_3eros.id_turno = liq_items_tarjas.id_turno NO-ERROR.
       if not available rb_resumen_3eros Then
          do:
             create rb_resumen_3eros.
             assign rb_resumen_3eros.id_empresa = liq_items_tarjas.id_empresa 
                  rb_resumen_3eros.id_sector = liq_items_tarjas.id_sector 
                  rb_resumen_3eros.legajo = liq_items_tarjas.legajo 
                  rb_resumen_3eros.nombre = liq_items_tarjas.nombre 
                  rb_resumen_3eros.dni_cuil = liq_items_tarjas.dni_cuil
                  rb_resumen_3eros.desc-tipotarea = v_tipo_tarea
                  rb_resumen_3eros.desc-categoria = v_categoria
                  rb_resumen_3eros.id_tarea = liq_items_tarjas.id_tarea 
                  rb_resumen_3eros.id_proveedor = liq_items_tarjas.id_proveedor 
                  rb_resumen_3eros.id_origen = liq_items_tarjas.id_origen
                  rb_resumen_3eros.id_lote = liq_items_tarjas.id_lote
                  rb_resumen_3eros.fecha = liq_items_tarjas.fecha
                  rb_resumen_3eros.nro_maquina = liq_items_tarjas.nro_maquina
                  rb_resumen_3eros.nro_tractor = liq_items_tarjas.nro_tractor
                  rb_resumen_3eros.id_turno = liq_items_tarjas.id_turno
                  rb_resumen_3eros.id_codigo_abacus = liq_items_tarjas.id_codigo_abacus
                  rb_resumen_3eros.id_codigo_abacus_diferencial = liq_items_tarjas.id_codigo_abacus_diferencial
                  rb_resumen_3eros.id_codigo_abacus_adicional = liq_items_tarjas.id_codigo_abacus_adicional
                  rb_resumen_3eros.id_codigo_abacus_cantidad = liq_items_tarjas.id_codigo_abacus_cantidad
                  rb_resumen_3eros.nro_planilla = liq_tarjas.nro_planilla.
          end.
 
         /* Cant Adic */
          ASSIGN
          rb_resumen_3eros.cantidad = rb_resumen_3eros.cantidad + liq_items_tarjas.cantidad
          rb_resumen_3eros.cantidad_adicional = rb_resumen_3eros.cantidad_adicional + liq_items_tarjas.cantidad_adicional
          rb_resumen_3eros.cantidad_adicional-1 = rb_resumen_3eros.cantidad_adicional-1 + liq_items_tarjas.cantidad_adicional-1.

          /*  Hs */
          ASSIGN
          rb_resumen_3eros.cant_hs_norm = rb_resumen_3eros.cant_hs_norm + liq_items_tarjas.cant_hs_norm
          rb_resumen_3eros.hs_acond_finca = rb_resumen_3eros.hs_acond_finca + liq_items_tarjas.hs_acond_finca
          rb_resumen_3eros.hs_plus_tareas_automatico = rb_resumen_3eros.hs_plus_tareas_automatico + liq_items_tarjas.hs_plus_tareas_automatico
          rb_resumen_3eros.hs_plus_tareas_trabajadas = rb_resumen_3eros.hs_plus_tareas_trabajadas + liq_items_tarjas.hs_plus_tareas_trabajadas
          rb_resumen_3eros.hs_adicionales_tareas_trabajadas = rb_resumen_3eros.hs_adicionales_tareas_trabajadas + liq_items_tarjas.hs_adicionales_tareas_trabajadas
          rb_resumen_3eros.TOTAL_horas = rb_resumen_3eros.cant_hs_norm + rb_resumen_3eros.hs_acond_finca +
                                         rb_resumen_3eros.hs_plus_tareas_automatico + rb_resumen_3eros.hs_plus_tareas_trabajadas +
                                         rb_resumen_3eros.hs_adicionales_tareas_trabajadas.

end.

if p_archivo = "" Then p_archivo = "z:\temp\reporte01.txt".

output to value(p_archivo).
  put "Periodo ".
  put p_fecha_desde.
  put " - ".
  put p_fecha_hasta.
  put skip.
  PUT "Mes;".
  PUT "Quincena;".
  put "Sector Gestion;".
  PUT "Fecha;".
  put "FINCA;". 
  put "Lote;".
  put "Nombre Lote;".
  PUT "Grupo;".
  PUT "SubGrupo;".
  put "Cod.;". 
  put "Tarea ;".
  PUT "Tipo Tarea;".
  put "Cod.Emp;".
  put "Nombre Emp;".
  put "Legajo;". 
  put "Nombre;". 
  put "DNI/CUIL;". 
  PUT "Categoria;".
  put "Cant Adic-1;".
  PUT "Tipo 1;".
  PUT "Cant Adic-2;".
  PUT "Tipo 2;".
  PUT "Nro Maq;".
  PUT "Nro Tractor;".
  PUT "Hs Norm;".
  PUT "HAF;".
  PUT "HPTA;".
  PUT "HPTT;".
  PUT "HATT;".
  PUT "Total Hs;".
  PUT "Con.Rhpro;".
  PUT "Hs Dif;".
  PUT "Dif Rhpro;".
  PUT "Cantidad;".
  PUT "Cant Rhpro;".
  PUT "Hs Adic;".
  PUT "Adic Rhpro;".
  PUT "Turno;".
  PUT "Nro Planilla:".

  put skip.
  
  for each rb_resumen_3eros NO-LOCK BREAK BY rb_resumen_3eros.id_sector BY id_empresa_liq BY rb_resumen_3eros.legajo 
      BY rb_resumen_3eros.fecha BY rb_resumen_3eros.id_proveedor BY rb_resumen_3eros.id_origen 
      BY rb_resumen_3eros.id_lote BY rb_resumen_3eros.id_tarea:
      
      find first liq_empresas where liq_empresas.id_empresa_liq = rb_resumen_3eros.id_empresa no-lock no-error.
      find first liq_sectores where 
          liq_sectores.id_empresa_liq = 101 AND
          liq_sectores.id_sector = rb_resumen_3eros.id_sector no-lock no-error.
     find first liq_tareas of rb_resumen_3eros no-lock no-error.
     find first origenes of rb_resumen_3eros no-lock no-error.
     find first lotes_plantacion where lotes_plantacion.id_proveedor = rb_resumen_3eros.id_proveedor and
     lotes_plantacion.id_origen = rb_resumen_3eros.id_origen and
     lotes_plantacion.id_lote = rb_resumen_3eros.id_lote
      no-lock NO-ERROR.

     FIND FIRST liq_conceptos WHERE liq_conceptos.id_concepto = rb_resumen_3eros.id_codigo_abacus NO-LOCK NO-ERROR.
     FIND FIRST bcon01 WHERE bcon01.id_concepto = rb_resumen_3eros.id_codigo_abacus_diferencial NO-LOCK NO-ERROR.
     FIND FIRST bcon02 WHERE bcon02.id_concepto = rb_resumen_3eros.id_codigo_abacus_cantidad NO-LOCK NO-ERROR.
     FIND FIRST bcon03 WHERE bcon03.id_concepto = rb_resumen_3eros.id_codigo_abacus_adicional NO-LOCK NO-ERROR.

     FIND FIRST liq_grupos_tareas OF liq_tareas NO-LOCK NO-ERROR.
     FIND FIRST liq_grupos_consulta OF liq_tareas NO-LOCK NO-ERROR.
     FIND FIRST turnos_tarjas OF rb_resumen_3eros NO-LOCK NO-ERROR.
     FIND FIRST bcant-1 WHERE bcant-1.id_tipo_cantidad = liq_tareas.id_tipo_cant-1 NO-LOCK NO-ERROR.
     FIND FIRST bcant-2 WHERE bcant-2.id_tipo_cantidad = liq_tareas.id_tipo_cant-2 NO-LOCK NO-ERROR.

               export delimiter ";" 
                month(rb_resumen_3eros.fecha)
                (IF DAY(rb_resumen_3eros.fecha) <= 15 THEN "1" ELSE "2")
               if available liq_sectores Then liq_sectores.descripcion Else " "
                   rb_resumen_3eros.fecha
                   if available origenes Then origenes.descripcion Else " "
                   rb_resumen_3eros.id_lote
                   if available lotes_plantacion Then lotes_plantacion.descripcion Else " "
                       (IF AVAILABLE liq_grupos_tareas THEN liq_grupos_tareas.descripcion ELSE " ")
                       (IF AVAILABLE liq_grupos_consulta THEN liq_grupos_consulta.descripcion ELSE " ")
                       rb_resumen_3eros.id_tarea
                       (IF AVAILABLE liq_tareas THEN liq_tareas.descripcion ELSE " ")
                       rb_resumen_3eros.desc-tipotarea
                       rb_resumen_3eros.id_empresa
                 if available liq_empresas Then liq_empresas.descripcion Else " "
                rb_resumen_3eros.legajo
                rb_resumen_3eros.nombre
                rb_resumen_3eros.dni_cuil
                rb_resumen_3eros.desc-categoria
                rb_resumen_3eros.cantidad_adicional
                (IF AVAILABLE bcant-1 THEN bcant-1.descripcion ELSE " ")
                rb_resumen_3eros.cantidad_adicional-1
                (IF AVAILABLE bcant-2 THEN bcant-2.descripcion ELSE " ")
                rb_resumen_3eros.nro_maquina
                rb_resumen_3eros.nro_tractor
                rb_resumen_3eros.cant_hs_norm
                rb_resumen_3eros.hs_acond_finca
                rb_resumen_3eros.hs_plus_tareas_automatico
                rb_resumen_3eros.hs_plus_tareas_trabajadas
                rb_resumen_3eros.hs_adicionales_tareas_trabajadas
                rb_resumen_3eros.TOTAL_horas
                (IF AVAILABLE liq_conceptos THEN liq_conceptos.descripcion ELSE " ")
                (IF rb_resumen_3eros.id_codigo_abacus_diferencial <> 0 THEN rb_resumen_3eros.cant_hs_norm ELSE 0) 
                (IF AVAILABLE bcon01 THEN bcon01.descripcion ELSE " ")
                rb_resumen_3eros.cantidad
               (IF AVAILABLE bcon02 THEN bcon02.descripcion ELSE " ")
                rb_resumen_3eros.hs_adicionales
               (IF AVAILABLE bcon03 THEN bcon03.descripcion ELSE " ")
                (IF AVAILABLE turnos_tarjas THEN turnos_tarjas.descripcion ELSE " ")
                rb_resumen_3eros.nro_planilla .

  END.
output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
